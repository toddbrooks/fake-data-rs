use crate::*;

#[test]
pub fn can_lex_simple_example() -> Result<(), Box<dyn Error>> {
    let tokens = lex_file("./examples/simple.fk")?;
    let expected: &[Token] = &[
        Token::Keyword(
            Keyword::Type,
        ),
        Token::Word(
            "AdultAge".into(),
        ),
        Token::Word(
            "int".into(),
        ),
        Token::OpenParen,
        Token::Number(
            18,
        ),
        Token::RangeDecl,
        Token::Number(
            70,
        ),
        Token::CloseParen,
        Token::Keyword(
            Keyword::Type,
        ),
        Token::Word(
            "ChildAge".into(),
        ),
        Token::Word(
            "int".into(),
        ),
        Token::OpenParen,
        Token::Number(
            0,
        ),
        Token::RangeDecl,
        Token::Number(
            17,
        ),
        Token::CloseParen,
        Token::Keyword(
            Keyword::Table,
        ),
        Token::Word(
            "Parent".into(),
        ),
        Token::OpenBrace,
        Token::PrimaryKey,
        Token::Word(
            "id".into(),
        ),
        Token::Eq,
        Token::Word(
            "GUIDv4".into(),
        ),
        Token::Word(
            "first".into(),
        ),
        Token::Eq,
        Token::Word(
            "FirstName".into(),
        ),
        Token::Word(
            "last".into(),
        ),
        Token::Eq,
        Token::Word(
            "LastName".into(),
        ),
        Token::Word(
            "age".into(),
        ),
        Token::Eq,
        Token::Word(
            "AdultAge".into(),
        ),
        Token::Word(
            "country".into(),
        ),
        Token::Eq,
        Token::Word(
            "CountryISO".into(),
        ),
        Token::CloseBrace,
        Token::Keyword(
            Keyword::Table,
        ),
        Token::Word(
            "Child".into(),
        ),
        Token::OpenBrace,
        Token::PrimaryKey,
        Token::Word(
            "id".into(),
        ),
        Token::Eq,
        Token::Word(
            "GUIDv4".into(),
        ),
        Token::Word(
            "first".into(),
        ),
        Token::Eq,
        Token::Word(
            "FirstName".into(),
        ),
        Token::Word(
            "last".into(),
        ),
        Token::Eq,
        Token::Word(
            "LastName".into(),
        ),
        Token::Word(
            "age".into(),
        ),
        Token::Eq,
        Token::Word(
            "ChildAge".into(),
        ),
        Token::Word(
            "parent".into(),
        ),
        Token::Eq,
        Token::Word(
            "Parent".into(),
        ),
        Token::CloseBrace,
        Token::Word(
            "Parent".into(),
        ),
        Token::RatioDecl,
        Token::OpenParen,
        Token::Number(
            10,
        ),
        Token::CloseParen,
        Token::Word(
            "Parent".into(),
        ),
        Token::RatioDecl,
        Token::Word(
            "Child".into(),
        ),
        Token::OpenParen,
        Token::Number(
            0,
        ),
        Token::RangeDecl,
        Token::Number(
            3,
        ),
        Token::CloseParen,
    ];

    assert_eq!(&tokens[..], expected);

    Ok(())
}

#[test]
fn can_parse_simple_example() -> Result<(), Box<dyn Error>> {
    let tokens = lex_file("./examples/simple.fk")?;
    let schema = parse_tokens(tokens)?;

    dbg!(&schema);
    assert!(false);

    assert!(schema.contains_table("Parent").is_some());
    assert!(schema.contains_table("Child").is_some());
    assert!(schema.contains_typedef("AdultAge").is_some());
    assert!(schema.contains_typedef("ChildAge").is_some());

    Ok(())
}