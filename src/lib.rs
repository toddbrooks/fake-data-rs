use std::error::Error;
use std::io::Read;

#[derive(Debug, Clone, PartialEq)]
pub enum Keyword {
    Table,
    Type
}

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Keyword(Keyword),
    OpenBrace, CloseBrace,
    OpenParen, CloseParen,
    Number(u32),
    RangeDecl,
    RatioDecl,
    Eq,
    Word(String),
    PrimaryKey
}

#[derive(Debug, Clone)]
pub enum Specifier {
    Const(u32),
    Range(u32, u32)
}

#[derive(Debug, Clone)]
pub enum DataType {
    Int(Specifier),
    GUIDv4,
    FirstName,
    LastName,
    CountryISO,
    PhoneNo,
    Email,
    String(Specifier),
    ForeignKey(Box<Table>, Box<Property>)
}

impl DataType {
    pub fn parse_from_string(s: String, specifier: Option<Specifier>) -> Result<Self, Box<dyn Error>> {
        let specifier = specifier.unwrap_or(Specifier::Const(0));

        match s.as_str() {
            "int" => Ok(DataType::Int(specifier)),
            "GUIDv4" => Ok(DataType::GUIDv4),
            "FirstName" => Ok(DataType::FirstName),
            "LastName" => Ok(DataType::LastName),
            "CountryISO" => Ok(DataType::CountryISO),
            "PhoneNo" => Ok(DataType::PhoneNo),
            "Email" => Ok(DataType::Email),
            "string" => Ok(DataType::String(specifier)),
            _ => panic!("Unknown data type: {s}")
        }
    }
}

#[derive(Debug, Clone)]
pub struct TypeDef {
    pub name: String,
    pub data_type: DataType 
}

#[derive(Debug, Clone)]
pub struct Ratio {
    pub first: Table,
    pub second: Option<Table>,
    pub ratio: Specifier
}

#[derive(Debug, Clone)]
pub struct Table {
    pub name: String,
    pub primary_key: Property,
    pub properties: Vec<Property>,
}

#[derive(Debug, Clone)]
pub struct Property {
    pub name: String,
    pub data_type: DataType
}

#[derive(Debug, Clone)]
pub struct Schema {
    pub type_defs: Vec<TypeDef>,
    pub ratios: Vec<Ratio>,
    pub tables: Vec<Table>
}

impl Schema {
    pub fn contains_typedef(&self, name: &str) -> Option<&TypeDef> {
        self.type_defs
            .iter()
            .find(|td| td.name == name)
    }

    pub fn contains_table(&self, name: &str) -> Option<&Table> {
        self.tables
            .iter()
            .find(|t| t.name == name)
    }
}

fn load_file(path: &str) -> Result<String, std::io::Error> {
    let mut file = std::fs::File::open(path)?;
    let mut contents = String::new();

    file.read_to_string(&mut contents)?;

    Ok(contents)
}

pub fn lex_file(path: &str) -> Result<Vec<Token>, Box<dyn Error>> {
    let mut tokens = Vec::new();
    let contents = load_file(path)?;
    let mut commented = false;
    
    let mut current_word = String::new();
    let mut symbol_acc = String::new();
    let mut number_acc = String::new();

    for c in contents.chars() {
        if !c.is_ascii_alphanumeric() {
            if !current_word.is_empty() {
                match current_word.as_str() {
                    "table" => tokens.push(Token::Keyword(Keyword::Table)),
                    "type" => tokens.push(Token::Keyword(Keyword::Type)),
                    _ => tokens.push(Token::Word(current_word))
                }

                current_word = String::new();
            } else if !number_acc.is_empty() {
                tokens.push(Token::Number(number_acc.parse::<u32>().unwrap()));
                number_acc = String::new();
            }
        }

        match c {
            // Comment handling
            '\n' if commented => {
                commented = false;
            },
            _ if commented => continue,
            
            // Numbers
            c if c.is_ascii_digit() && current_word.is_empty() => {
                number_acc.push(c);
            }

            // Word handling
            c if c.is_ascii_alphanumeric() => {
                current_word.push(c);
            },

            // Symbol handling
            '#' => {
                commented = true;
            },
            '+' => tokens.push(Token::PrimaryKey),
            '{' => tokens.push(Token::OpenBrace),
            '}' => tokens.push(Token::CloseBrace),
            '(' => tokens.push(Token::OpenParen),
            ')' => tokens.push(Token::CloseParen),
            '=' => tokens.push(Token::Eq),
            '.' => {
                if symbol_acc == "." {
                    tokens.push(Token::RangeDecl);
                    symbol_acc = String::new();
                } else {
                    symbol_acc.push(c);
                }
            },
            '-' if symbol_acc.is_empty() => {
                symbol_acc.push(c);
            },
            '>' if symbol_acc == "-" => {
                tokens.push(Token::RatioDecl);
                symbol_acc = String::new();
            }

            c if !c.is_ascii_whitespace() => println!("WARNING: Unhandled char: {}", c),
            _ => { }
        }

        if symbol_acc.len() >= 2 {
            panic!("Syntax error: Invalid symbol group");
        }
    }

    Ok(tokens)
}

#[allow(unused_assignments)]
pub fn parse_tokens(tokens: Vec<Token>) -> Result<Schema, Box<dyn Error>> {
    let mut schema = Schema {
        type_defs: Vec::new(),
        ratios: Vec::new(),
        tables: Vec::new()
    };

    for mut idx in 0..tokens.len() {
        // From tokens[idx]...
        // Match TypeDef pattern, 
        if let [ Token::Keyword(Keyword::Type), Token::Word(name), Token::Word(data_type), .. ] = &tokens[idx..] {
            idx += 3;

            let specifier = if let [ Token::OpenParen, Token::Number(start), Token::CloseParen, .. ] = &tokens[idx..] {
                idx += 3;

                Some(Specifier::Const(*start))
            } else if let [ Token::OpenParen, Token::Number(start), Token::RangeDecl, Token::Number(end), Token::CloseParen, .. ] = &tokens[idx..] {
                idx += 5;
                
                Some(Specifier::Range(*start, *end))
            } else {
                None
            };
            
            let d = DataType::parse_from_string(data_type.clone(), specifier)?;
            
            let data_type = TypeDef {
                name: name.clone(),
                data_type: d
            };

            schema.type_defs.push(data_type);

            continue;
        }

        // Match Table pattern
        if let [ Token::Keyword(Keyword::Table), Token::Word(name), Token::OpenBrace, .. ] = &tokens[idx..] {
            idx += 3;

            // Match Property pattern
            let mut properties: Vec<Property> = Vec::new();
            let mut primary_key: Option<Property> = None;

            while let [ before, Token::Word(name), Token::Eq, Token::Word(data_type), .. ] = &tokens[idx..] {
                idx += 3;

                let specifier = if let [ Token::OpenParen, Token::Number(start), Token::CloseParen, .. ] = &tokens[idx..] {
                    idx += 3;

                    Some(Specifier::Const(*start))
                } else if let [ Token::OpenParen, Token::Number(start), Token::RangeDecl, Token::Number(end), Token::CloseParen, .. ] = &tokens[idx..] {
                    idx += 5;
                    
                    Some(Specifier::Range(*start, *end))
                } else {
                    None
                };

                let data_type = if let Some(data_type) = schema.contains_typedef(data_type) {
                    data_type.data_type.clone()
                } else if let Some(table) = schema.contains_table(data_type) {
                    // TODO: this should be a reference, not a clone
                    DataType::ForeignKey(Box::new(table.clone()), Box::new(table.primary_key.clone()))
                } else {
                    DataType::parse_from_string(data_type.clone(), specifier)?
                };
                    
                let property = Property {
                    name: name.clone(),
                    data_type
                };
                
                properties.push(property);

                if let Token::PrimaryKey = before {
                    primary_key = Some(properties.last().unwrap().clone());
                }
            }
            
            if let Some(primary_key) = primary_key {
                let table = Table {
                    name: name.clone(),
                    properties,
                    primary_key
                };

                schema.tables.push(table);
            } else {
                panic!("Table must have primary key");
            }

            continue;
        }
        
        // Match Ratio pattern
        if let [ Token::Word(first), Token::RatioDecl, .. ] = &tokens[idx..] {
            idx += 2;

            let first = schema.contains_table(first).expect("Ratio must have valid first table");

            if let [ Token::OpenParen, Token::Number(start), .. ] = &tokens[idx..] {
                idx += 2;

                let ratio = if let [ Token::CloseParen, .. ] = &tokens[idx..] {
                    idx += 1;
                    
                    Specifier::Const(*start)
                } else if let [ Token::RangeDecl, Token::Number(end), Token::CloseParen, .. ] = &tokens[idx..] {
                    idx += 3;

                    Specifier::Range(*start, *end)
                } else {
                    panic!("Invalid ratio specifier");
                };

                let ratio = Ratio {
                    first: first.clone(), 
                    second: None,
                    ratio
                };

                schema.ratios.push(ratio);
            } else if let [ Token::Word(second), Token::OpenParen, Token::Number(start), .. ] = &tokens[idx..] {
                idx += 3;

                let second = schema.contains_table(second).expect("Ratio must have valid second table");

                let ratio = if let [ Token::CloseParen, .. ] = &tokens[idx..] {
                    idx += 1;
                    
                    Specifier::Const(*start)
                } else if let [ Token::RangeDecl, Token::Number(end), Token::CloseParen, .. ] = &tokens[idx..] {
                    idx += 3;

                    Specifier::Range(*start, *end)
                } else {
                    panic!("Invalid ratio specifier");
                };

                let ratio = Ratio {
                    first: first.clone(), 
                    second: Some(second.clone()),
                    ratio
                };

                schema.ratios.push(ratio);
            } else {
                panic!("Invalid ratio syntax");
            }
        }
    }

    Ok(schema)
}

#[cfg(test)]
mod test;
