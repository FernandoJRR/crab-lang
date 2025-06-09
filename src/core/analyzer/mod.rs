use chumsky::{input::ValueInput, prelude::*};

type Span = SimpleSpan;
type Spanned<T> = (T, SimpleSpan);

#[derive(Debug, Clone)]
pub enum NodeKind {
    Null,
    Int(u64),
    Float(f64),
    Bool(bool),
    String(String),
    Value(String),
    Neg,
    Mult,
    Div,
    Add,
    Sub,
    Eq,  //Equal
    Neq, //Not equal
    Geq, //Greater than or equal
    Leq, //Lesser than or equal
    Gth, //Lesser than
    Lth, //Lesser than
    Or,
    And,
    Not,

    Decl,
    Assign,
    Fn(String),
    FnCall(String),
    Params,
    Param(String),

    Type(Type),

    Insts,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Int,
    Float,
    Bool,
    String
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let printable = match *self {
            Self::Int => "int",
            Self::Float => "float",
            Self::Bool => "bool",
            Self::String => "string",
        };
        write!(f, "{}", printable)
    }
}

#[derive(Debug, Clone)]
pub struct Node {
    pub kind: NodeKind,
    pub span: Span,
    pub children: Option<Vec<Self>>,
}

pub struct NodeBuilder {
    kind: Option<NodeKind>,
    span: Option<Span>,
    children: Option<Vec<Node>>,
}

impl NodeBuilder {
    pub fn new() -> Self {
        NodeBuilder {
            kind: None,
            span: None,
            children: None,
        }
    }

    pub fn kind(mut self, kind: NodeKind) -> Self {
        self.kind = Some(kind);
        self
    }

    pub fn span(mut self, span: Span) -> Self {
        self.span = Some(span);
        self
    }

    pub fn children(mut self, children: Vec<Node>) -> Self {
        self.children = Some(children);
        self
    }

    pub fn build(self) -> Node {
        Node {
            kind: self.kind.expect("missing kind"),
            span: self.span.expect("missing span"),
            children: self.children,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
#[allow(dead_code)]
pub enum Token<'src> {
    Null,
    Int(u64),
    Float(f64),
    Bool(bool),
    String(&'src str),
    Var(&'src str),
    Op(&'src str),
    Ctrl(&'src str),
    Type(Type),
    Let,
    Fn,
}

fn lexer<'src>() -> impl Parser<'src, &'src str, Vec<Spanned<Token<'src>>>> {
    let null = just("null").map(|_: &str| Token::Null).padded();

    let string = just("\"")
        .ignore_then(none_of("\"").repeated().to_slice())
        .then_ignore(just("\""))
        .map(Token::String)
        .padded();

    let boolean = just("true")
        .or(just("false"))
        .map(|b| match b {
            "true" => Token::Bool(true),
            "false" => Token::Bool(false),
            _ => panic!(),
        })
        .padded();

    let var_type = just("int")
        .or(just("float"))
        .or(just("bool"))
        .map(|s| {
            Token::Type(match s {
                "int" => Type::Int,
                "float" => Type::Float,
                "bool" => Type::Bool,
                _ => panic!(),
            })
        })
        .padded();

    let integer = text::int(10)
        .map(|s: &str| Token::Int(s.parse().unwrap()))
        .padded();

    let float = text::int(10)
        .then(just('.').then(text::digits(10)))
        .to_slice()
        .map(|s: &str| Token::Float(s.parse().unwrap()))
        .padded();

    let op = one_of("+*-=/!><")
        .repeated()
        .at_least(1)
        .to_slice()
        .map(Token::Op);

    let ctrl = one_of("()[]{};,:").to_slice().map(Token::Ctrl);

    let var = text::ascii::ident().map(|var: &str| match var {
        "let" => Token::Let,
        "fn" => Token::Fn,
        _ => Token::Var(var),
    });

    let token = float
        .or(integer)
        .or(string)
        .or(op)
        .or(ctrl)
        .or(null)
        .or(boolean)
        .or(var_type)
        .or(var);

    let comment = just("//")
        .then(any().and_is(just('\n').not()).repeated())
        .padded();

    token
        .map_with(|tok, e| (tok, e.span()))
        .padded_by(comment.repeated())
        .padded()
        .recover_with(skip_then_retry_until(any().ignored(), end()))
        .repeated()
        .collect()
}

fn expr_parser<'src, I>() -> impl Parser<'src, I, Node> + Clone
where
    I: ValueInput<'src, Token = Token<'src>, Span = Span>,
{
    recursive(|expr| {
        let fn_call = select! {Token::Var(fn_n) => fn_n.to_string()}
            .map_with(|expr, e| (expr, e.span()))
            .then(
                expr.clone()
                    .separated_by(just(Token::Ctrl(",")))
                    .collect::<Vec<_>>()
                    .delimited_by(just(Token::Ctrl("(")), just(Token::Ctrl(")"))),
            )
            .map(
                |((fn_name, fn_span), params): ((String, SimpleSpan), Vec<Node>)| {
                    NodeBuilder::new()
                        .kind(NodeKind::FnCall(fn_name))
                        .span(fn_span)
                        .children(params)
                        .build()
                },
            );

        let val = select! {
            Token::Null => NodeKind::Null,
            Token::Int(n) => NodeKind::Int(n),
            Token::Float(n) => NodeKind::Float(n),
            Token::Var(n) => NodeKind::Value(n.to_string()),
            Token::Bool(b) => NodeKind::Bool(b),
            Token::String(s) => NodeKind::String(s.to_string()),
        }
        .map_with(|expr, e| NodeBuilder::new().kind(expr).span(e.span()).build());

        let atom = fn_call
            .or(val)
            .or(expr.delimited_by(just(Token::Ctrl("(")), just(Token::Ctrl(")"))));

        let op = |op_tok| just(op_tok).map_with(|op: Token<'_>, e| (op, e.span()));

        let unary_minus =
            op(Token::Op("-"))
                .repeated()
                .foldr(atom.clone(), |op: (Token, Span), rhs| {
                    NodeBuilder::new()
                        .kind(NodeKind::Neg)
                        .span(op.1.union(rhs.span))
                        .children(vec![rhs.clone()])
                        .build()
                });

        let not = op(Token::Op("!"))
            .repeated()
            .foldr(atom, |op: (Token, Span), rhs| {
                NodeBuilder::new()
                    .kind(NodeKind::Not)
                    .span(op.1.union(rhs.span))
                    .children(vec![rhs.clone()])
                    .build()
            });

        let unary = unary_minus.or(not);

        let product = unary.clone().foldl(
            choice((
                op(Token::Op("*")).map(|o| (NodeKind::Mult, o.1)),
                op(Token::Op("/")).map(|o| (NodeKind::Div, o.1)),
            ))
            .then(unary)
            .repeated(),
            |lhs, (op, rhs): ((NodeKind, SimpleSpan), Node)| {
                NodeBuilder::new()
                    .kind(op.0)
                    .span(lhs.span.union(rhs.span))
                    .children(vec![lhs, rhs])
                    .build()
            },
        );

        let sum = product.clone().foldl(
            choice((
                op(Token::Op("+")).map(|o| (NodeKind::Add, o.1)),
                op(Token::Op("-")).map(|o| (NodeKind::Sub, o.1)),
            ))
            .then(product)
            .repeated(),
            |lhs, (op, rhs): ((NodeKind, SimpleSpan), Node)| {
                NodeBuilder::new()
                    .kind(op.0)
                    .span(lhs.span.union(rhs.span))
                    .children(vec![lhs, rhs])
                    .build()
            },
        );

        let relational = sum.clone().foldl(
            choice((
                op(Token::Op("==")).map(|o| (NodeKind::Eq, o.1)),
                op(Token::Op("!=")).map(|o| (NodeKind::Neq, o.1)),
                op(Token::Op(">")).map(|o| (NodeKind::Gth, o.1)),
                op(Token::Op("<")).map(|o| (NodeKind::Lth, o.1)),
                op(Token::Op(">=")).map(|o| (NodeKind::Geq, o.1)),
                op(Token::Op("<=")).map(|o| (NodeKind::Leq, o.1)),
            ))
            .then(sum)
            .repeated(),
            |lhs, (op, rhs): ((NodeKind, SimpleSpan), Node)| {
                NodeBuilder::new()
                    .kind(op.0)
                    .span(lhs.span.union(rhs.span))
                    .children(vec![lhs, rhs])
                    .build()
            },
        );

        let logical = relational.clone().foldl(
            choice((
                op(Token::Op("||")).map(|o| (NodeKind::Or, o.1)),
                op(Token::Op("&&")).map(|o| (NodeKind::And, o.1)),
            ))
            .then(relational)
            .repeated(),
            |lhs, (op, rhs): ((NodeKind, SimpleSpan), Node)| {
                NodeBuilder::new()
                    .kind(op.0)
                    .span(lhs.span.union(rhs.span))
                    .children(vec![lhs, rhs])
                    .build()
            },
        );

        logical
    })
}

pub fn insts_parser<'src, I>() -> impl Parser<'src, I, Node> + Clone
where
    I: ValueInput<'src, Token = Token<'src>, Span = Span>,
{
    let expr = expr_parser();

    let r#let = just(Token::Let)
        .ignore_then(
            select! {Token::Var(s) => NodeKind::Value(s.to_string())}.map_with(|expr, e| Node {
                kind: expr,
                span: e.span(),
                children: None,
            }),
        )
        .then_ignore(just(Token::Op("=")))
        .then(expr.clone())
        .then_ignore(just(Token::Ctrl(";")))
        .map(|(name, rhs)| {
            NodeBuilder::new()
                .kind(NodeKind::Decl)
                .span(name.span)
                .children(vec![name, rhs])
                .build()
        });

    let assign = select! {Token::Var(s) => NodeKind::Value(s.to_string())}
        .map_with(|expr, e| Node {
            kind: expr,
            span: e.span(),
            children: None,
        })
        .then_ignore(just(Token::Op("=")))
        .then(expr.clone())
        .then_ignore(just(Token::Ctrl(";")))
        .map(|(name, rhs)| {
            NodeBuilder::new()
                .kind(NodeKind::Assign)
                .span(name.span)
                .children(vec![name, rhs])
                .build()
        });

    let inst = r#let.or(assign).or(expr);

    inst.repeated().collect::<Vec<_>>().map_with(|nodes, _| {
        let start_span = match nodes.first() {
            Some(first_inst) => first_inst.span,
            None => SimpleSpan {
                start: 0,
                end: 0,
                context: (),
            },
        };

        let end_span = match nodes.last() {
            Some(last_inst) => last_inst.span,
            None => SimpleSpan {
                start: 0,
                end: 0,
                context: (),
            },
        };

        NodeBuilder::new()
            .kind(NodeKind::Insts)
            .span(start_span.union(end_span))
            .children(nodes)
            .build()
    })
}

pub fn parser<'src, I>() -> impl Parser<'src, I, Node> + Clone
where
    I: ValueInput<'src, Token = Token<'src>, Span = Span>,
{
    let insts = insts_parser();

    let params = select! {Token::Var(s) => s}
        .map_with(|expr, e| (expr, e.span()))
        .then_ignore(just(Token::Ctrl(":")))
        .then(
            select! {Token::Type(t) => NodeKind::Type(t)}
                .map_with(|t, e| NodeBuilder::new().kind(t).span(e.span()).build()),
        )
        .map(|((var_name, var_span), var_type): ((_, SimpleSpan), _)| {
            NodeBuilder::new()
                .kind(NodeKind::Param(var_name.to_string()))
                .span(var_span.union(var_type.span))
                .children(vec![var_type])
                .build()
        })
        .separated_by(just(Token::Ctrl(",")))
        .collect::<Vec<_>>()
        .delimited_by(just(Token::Ctrl("(")), just(Token::Ctrl(")")))
        .map(|args| {
            let span = match (args.first(), args.last()) {
                (Some(start), Some(end)) => start.span.union(end.span),
                _ => SimpleSpan {
                    start: 0,
                    end: 0,
                    context: (),
                },
            };

            let mut builder = NodeBuilder::new().kind(NodeKind::Params).span(span);

            if !args.is_empty() {
                builder = builder.children(args);
            }

            builder.build()
        });

    let fn_return = just(Token::Op("->"))
        .ignore_then(select! {Token::Type(t) => NodeKind::Type(t)})
        .map_with(|t, e| NodeBuilder::new().kind(t).span(e.span()).build());

    let fn_body = insts.delimited_by(just(Token::Ctrl("{")), just(Token::Ctrl("}")));

    let fns = just(Token::Fn)
        .ignore_then(select! {Token::Var(s) => s})
        .then(params)
        .then(fn_return.or_not())
        .then(fn_body)
        .map_with(|(((fn_name, args), fn_return), fn_body), e| {
            NodeBuilder::new()
                .kind(NodeKind::Fn(fn_name.to_string()))
                .span(e.span())
                .children(match fn_return {
                    Some(fn_return_node) => vec![args, fn_return_node, fn_body],
                    None => vec![args, fn_body],
                })
                .build()
        });

    fns.repeated().collect::<Vec<_>>().map_with(|nodes, _| {
        let start_span = match nodes.first() {
            Some(first_inst) => first_inst.span,
            None => SimpleSpan {
                start: 0,
                end: 0,
                context: (),
            },
        };

        let end_span = match nodes.last() {
            Some(last_inst) => last_inst.span,
            None => SimpleSpan {
                start: 0,
                end: 0,
                context: (),
            },
        };

        NodeBuilder::new()
            .kind(NodeKind::Insts)
            .span(start_span.union(end_span))
            .children(nodes)
            .build()
    })
}

pub fn analyze(src: &str) -> (Option<Node>, Vec<EmptyErr>) {
    let (tokens, _err) = lexer().parse(src).into_output_errors();

    if let Some(tokens) = &tokens {
        let (ast, parse_errs) = parser()
            .parse(
                tokens
                    .as_slice()
                    .map((src.len()..src.len()).into(), |(t, s)| (t, s)),
            )
            .into_output_errors();
        (ast, parse_errs)
    } else {
        (None, [].to_vec())
    }
}

#[derive(Debug)]
pub enum InterpolationPart<'src> {
    Text(String),
    Variable(&'src str),
}

pub fn interpolation_parser<'src>() -> impl Parser<'src, &'src str, Vec<InterpolationPart<'src>>> {
    let text = any()
        .filter(|c| *c != '{' && *c != '}')
        .repeated()
        .at_least(1)
        .collect::<String>()
        .map(InterpolationPart::Text);

    let var = just('{')
        .ignore_then(text::ident())
        .then_ignore(just('}'))
        .map(InterpolationPart::Variable);

    text.or(var).repeated().collect()
}
