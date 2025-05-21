use chumsky::{input::ValueInput, prelude::*};

type Span = SimpleSpan;
type Spanned<T> = (T, SimpleSpan);

#[derive(Debug, Clone)]
pub enum NodeKind {
    Null,
    Int(u64),
    Float(f64),
    Value(String),
    Neg,
    Mult,
    Div,
    Add,
    Sub,
    Decl,
    FnCall(String),

    Insts,
}

#[derive(Debug, Clone)]
pub struct Node {
    pub kind: NodeKind,
    pub span: Span,
    pub children: Option<Vec<Self>>,
}

#[derive(Debug, Clone, PartialEq)]
#[allow(dead_code)]
pub enum Token<'src> {
    Null,
    Int(u64),
    Float(f64),
    Var(&'src str),
    Op(&'src str),
    Ctrl(&'src str),
    Let,
}

fn lexer<'src>() -> impl Parser<'src, &'src str, Vec<Spanned<Token<'src>>>> {
    let null = just("null").map(|_: &str| Token::Null).padded();

    let integer = text::int(10)
        .map(|s: &str| Token::Int(s.parse().unwrap()))
        .padded();

    let float = text::int(10)
        .then(just('.').then(text::digits(10)))
        .to_slice()
        .map(|s: &str| Token::Float(s.parse().unwrap()))
        .padded();

    let op = one_of("+*-/=")
        .repeated()
        .at_least(1)
        .to_slice()
        .map(Token::Op);

    let ctrl = one_of("()[]{};,")
        .repeated()
        .at_least(1)
        .to_slice()
        .map(Token::Ctrl);

    let var = text::ascii::ident().map(|var: &str| match var {
        "let" => Token::Let,
        _ => Token::Var(var),
    });

    let token = float.or(integer).or(op).or(ctrl).or(null).or(var);

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

pub fn parser<'src, I>() -> impl Parser<'src, I, Node> + Clone
where
    I: ValueInput<'src, Token = Token<'src>, Span = Span>,
{
    let expr = recursive(|expr| {
        let fn_call = select! {Token::Var(fn_n) => fn_n.to_string()}
            .map_with(|expr, e| (expr, e.span()))
            .then(
                expr.clone()
                    .delimited_by(just(Token::Ctrl("(")), just(Token::Ctrl(")"))),
            )
            .map(
                |((fn_name, fn_span), expr): ((String, SimpleSpan), Node)| Node {
                    kind: NodeKind::FnCall(fn_name),
                    span: fn_span.union(expr.span),
                    children: Some(vec![expr]),
                },
            );

        let val = select! {
            Token::Null => NodeKind::Null,
            Token::Int(n) => NodeKind::Int(n),
            Token::Float(n) => NodeKind::Float(n),
            Token::Var(n) => NodeKind::Value(n.to_string())
        }
        .map_with(|expr, e| Node {
            kind: expr,
            span: e.span(),
            children: None,
        });

        let atom = fn_call
            .or(val)
            .or(expr.delimited_by(just(Token::Ctrl("(")), just(Token::Ctrl(")"))));

        let op = |op_tok| just(op_tok).map_with(|op: Token<'_>, e| (op, e.span()));

        let unary = op(Token::Op("-"))
            .repeated()
            .foldr(atom, |op: (Token, Span), rhs| Node {
                kind: NodeKind::Neg,
                span: op.1.union(rhs.span),
                children: Some(vec![rhs.clone()]),
            });

        let product = unary.clone().foldl(
            choice((
                op(Token::Op("*")).map(|o| (NodeKind::Mult, o.1)),
                op(Token::Op("/")).map(|o| (NodeKind::Div, o.1)),
            ))
            .then(unary)
            .repeated(),
            |lhs, (op, rhs): ((NodeKind, SimpleSpan), Node)| Node {
                kind: op.0,
                span: lhs.span.union(rhs.span),
                children: Some(vec![lhs, rhs]),
            },
        );

        let sum = product.clone().foldl(
            choice((
                op(Token::Op("+")).map(|o| (NodeKind::Add, o.1)),
                op(Token::Op("-")).map(|o| (NodeKind::Sub, o.1)),
            ))
            .then(product)
            .repeated(),
            |lhs, (op, rhs): ((NodeKind, SimpleSpan), Node)| Node {
                kind: op.0,
                span: lhs.span.union(rhs.span),
                children: Some(vec![lhs, rhs]),
            },
        );

        sum
    });

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
        .map(|(name, rhs)| Node {
            kind: NodeKind::Decl,
            span: name.span,
            children: Some(vec![name, rhs]),
        });

    let inst = r#let.or(expr);

    inst.repeated().collect::<Vec<_>>().map(|nodes| Node {
        kind: NodeKind::Insts,
        span: SimpleSpan {
            start: 0,
            end: 0,
            context: (),
        },
        children: Some(nodes),
    })
}

pub fn analyze<'src>(src: &'src str) -> Option<Node> {
    let (tokens, _err) = lexer().parse(src).into_output_errors();
    println!("{:?}", tokens);

    if let Some(tokens) = &tokens {
        let (ast, parse_errs) = parser()
            .parse(
                tokens
                    .as_slice()
                    .map((src.len()..src.len()).into(), |(t, s)| (t, s)),
            )
            .into_output_errors();
        ast
    } else {
        None
    }
}
