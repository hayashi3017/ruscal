use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{alpha1, alphanumeric1, char, multispace0},
    combinator::recognize,
    error::ParseError,
    multi::{self, fold_many0, many0},
    number::complete::recognize_float,
    sequence::{delimited, pair},
    IResult, Parser,
};

#[derive(Debug, PartialEq, Clone)]
enum Token<'src> {
    Ident(&'src str),
    Number(f64),
}

#[derive(Debug, PartialEq, Clone)]
enum Expression<'src> {
    Value(Token<'src>),
    Add(Box<Expression<'src>>, Box<Expression<'src>>),
    Sub(Box<Expression<'src>>, Box<Expression<'src>>),
    Mul(Box<Expression<'src>>, Box<Expression<'src>>),
    Div(Box<Expression<'src>>, Box<Expression<'src>>),
}

fn main() {
    fn ex_eval<'src>(input: &'src str) -> Result<f64, nom::Err<nom::error::Error<&'src str>>> {
        expr(input).map(|(_, e)| eval(e))
    }
    let input = "123";
    println!("source: {:?}, parsed: {:?}", input, ex_eval(input));

    let input = "2 * pi";
    println!("source: {:?}, parsed: {:?}", input, ex_eval(input));

    let input = "(123 + 456 ) + pi";
    println!("source: {:?}, parsed: {:?}", input, ex_eval(input));

    let input = "10 - (100 + 1)";
    println!("source: {:?}, parsed: {:?}", input, ex_eval(input));

    let input = "(3 + 7) / (2 + 3)";
    println!("source: {:?}, parsed: {:?}", input, ex_eval(input));
}

fn eval(expr: Expression) -> f64 {
    match expr {
        Expression::Value(Token::Ident("pi")) => std::f64::consts::PI,
        Expression::Value(Token::Ident(id)) => panic!("Unknown name {:?}", id),
        Expression::Value(Token::Number(n)) => n,
        Expression::Add(lhs, rhs) => eval(*lhs) + eval(*rhs),
        Expression::Sub(lhs, rhs) => eval(*lhs) - eval(*rhs),
        Expression::Mul(lhs, rhs) => eval(*lhs) * eval(*rhs),
        Expression::Div(lhs, rhs) => eval(*lhs) / eval(*rhs),
    }
}

fn expr(i: &str) -> IResult<&str, Expression> {
    let (i, init) = term(i)?;

    fold_many0(
        pair(space_delimited(alt((char('+'), char('-')))), term),
        move || init.clone(),
        |acc, (op, val)| match op {
            '+' => Expression::Add(Box::new(acc), Box::new(val)),
            '-' => Expression::Sub(Box::new(acc), Box::new(val)),
            _ => panic!("Additive expression should have '+' or '-' operator"),
        },
    )(i)
}

fn term(i: &str) -> IResult<&str, Expression> {
    let (i, init) = factor(i)?;

    fold_many0(
        pair(space_delimited(alt((char('*'), char('/')))), factor),
        move || init.clone(),
        |acc, (op, val)| match op {
            '*' => Expression::Mul(Box::new(acc), Box::new(val)),
            '/' => Expression::Div(Box::new(acc), Box::new(val)),
            _ => panic!("Multiplicative expression should have '*' or '/' operator"),
        },
    )(i)
}

fn space_delimited<'src, O, E>(
    f: impl Parser<&'src str, O, E>,
) -> impl FnMut(&'src str) -> IResult<&'src str, O, E>
where
    E: ParseError<&'src str>,
{
    delimited(multispace0, f, multispace0)
}

fn factor(i: &str) -> IResult<&str, Expression> {
    alt((number, ident, parens))(i)
}

fn parens(i: &str) -> IResult<&str, Expression> {
    delimited(
        multispace0,
        delimited(tag("("), expr, tag(")")),
        multispace0,
    )(i)
}

fn ident(input: &str) -> IResult<&str, Expression> {
    let (r, res) = space_delimited(identifier)(input)?;
    Ok((r, Expression::Value(Token::Ident(res))))
}

fn identifier(input: &str) -> IResult<&str, &str> {
    recognize(pair(
        alt((alpha1, tag("_"))),
        many0(alt((alphanumeric1, tag("_")))),
    ))(input)
}

fn number(input: &str) -> IResult<&str, Expression> {
    let (r, v) = space_delimited(recognize_float)(input)?;
    Ok((
        r,
        Expression::Value(Token::Number(v.parse().map_err(|_| {
            nom::Err::Error(nom::error::Error {
                input,
                code: nom::error::ErrorKind::Digit,
            })
        })?)),
    ))
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_ident() {
        assert_eq!(
            ident("Adam"),
            Ok(("", Expression::Value(Token::Ident("Adam"))))
        );
    }

    #[test]
    fn test_number() {
        assert_eq!(
            number("123.45 "),
            Ok(("", Expression::Value(Token::Number(123.45))))
        );
    }
}
