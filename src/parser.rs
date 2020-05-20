use crate::ast::*;
use crate::token::*;
use std::error::Error;
use std::fmt;

#[derive(Debug)]
pub struct ParseError<'a> {
    pub token: &'a str,
    pub index: usize,
    pub message: String,
}

impl<'a> fmt::Display for ParseError<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "failed to parse @ {} ('{}'): {}",
            self.index, self.token, self.message
        )
    }
}

impl<'a> Error for ParseError<'a> {}

struct Parser;

trait Parse<'a, T: Node<'a>> {
    fn parse(tokens: &mut &[TokenSpan<'a>]) -> Result<T, ParseError<'a>>;

    /// parse but destroy mutations to tokens if Err
    fn parse_reset(tokens: &mut &[TokenSpan<'a>]) -> Result<T, ParseError<'a>> {
        let mut new_tokens = *tokens;
        let result = Self::parse(&mut new_tokens)?;
        *tokens = new_tokens;
        Ok(result)
    }
}

pub fn parse<'a>(mut tokens: &[TokenSpan<'a>]) -> Result<SelectorsGroup<'a>, ParseError<'a>> {
    Parser::parse(&mut tokens)
}

impl<'a> Parse<'a, SelectorsGroup<'a>> for Parser {
    fn parse(tokens: &mut &[TokenSpan<'a>]) -> Result<SelectorsGroup<'a>, ParseError<'a>> {
        let mut selectors: Vec<Selector<'a>> = vec![Parser::parse(tokens)?];
        while let Some((_, Token::COMMA)) = tokens.get(0) {
            *tokens = &tokens[1..];
            while let Some((_, Token::S)) = tokens.get(0) {
                *tokens = &tokens[1..];
            }
            selectors.push(Parser::parse(tokens)?);
        }

        Ok(SelectorsGroup::<'a> { selectors })
    }
}

impl<'a> Parse<'a, Selector<'a>> for Parser {
    fn parse(tokens: &mut &[TokenSpan<'a>]) -> Result<Selector<'a>, ParseError<'a>> {
        let base: SelectorSequence<'a> = Parser::parse(tokens)?;
        let mut modifiers: Vec<(Combinator, SelectorSequence<'a>)> = vec![];
        loop {
            let combinator = match tokens.get(0) {
                Some((_, Token::PLUS)) => Combinator::Plus,
                Some((_, Token::GREATER)) => Combinator::Greater,
                Some((_, Token::TILDE)) => Combinator::Tilde,
                Some((_, Token::S)) => Combinator::None,
                _ => break,
            };
            *tokens = &tokens[1..];
            while let Some((_, Token::S)) = tokens.get(0) {
                *tokens = &tokens[1..];
            }
            let modifier: SelectorSequence<'a> = Parser::parse(tokens)?;
            modifiers.push((combinator, modifier));
        }

        Ok(Selector::<'a> { base, modifiers })
    }
}

impl<'a> Parse<'a, SelectorSequence<'a>> for Parser {
    fn parse(tokens: &mut &[TokenSpan<'a>]) -> Result<SelectorSequence<'a>, ParseError<'a>> {
        let type_selector: Option<TypeSelector<'a>> = Parser::parse_reset(tokens).ok();
        let mut attribute_selectors = vec![];
        let last_error;
        loop {
            match Parser::parse(tokens) {
                Ok(attribute_selector) => {
                    attribute_selectors.push(attribute_selector);
                }
                Err(e) => {
                    last_error = e;
                    break;
                }
            }
        }
        if type_selector.is_none() && attribute_selectors.len() == 0 {
            return Err(ParseError::<'a> {
                index: tokens.get(0).map(|x| x.0.start).unwrap_or(0),
                token: tokens.get(0).map(|x| x.0.value).unwrap_or_default(),
                message: format!("expected at least 1 attribute selector to accompany the type selector, 0 found\n{:?}", last_error),
            });
        }
        Ok(SelectorSequence::<'a> {
            type_selector,
            attribute_selectors,
        })
    }
}

impl<'a> Parse<'a, AttributeSelector<'a>> for Parser {
    fn parse(tokens: &mut &[TokenSpan<'a>]) -> Result<AttributeSelector<'a>, ParseError<'a>> {
        match tokens.get(0) {
            Some((_, Token::HASH(hash))) => {
                *tokens = &tokens[1..];
                Ok(AttributeSelector::Hash(hash))
            }
            Some((_, Token::DOT(class))) => {
                *tokens = &tokens[1..];
                Ok(AttributeSelector::Class(class))
            }
            Some((_, Token::LBRACK)) => Ok(AttributeSelector::Attribute(Parser::parse(tokens)?)),
            Some((_, Token::COLON)) => Ok(AttributeSelector::Psuedo(Parser::parse(tokens)?)),
            Some((_, Token::NOT)) => Ok(AttributeSelector::Negation(Parser::parse(tokens)?)),
            Some((span, _token)) => Err(ParseError::<'a> {
                index: span.start,
                token: span.value,
                message: "expected a '.', '#', '[', ':', or 'NOT', none found".to_string(),
            }),
            None => Err(ParseError::<'a> {
                index: 0,
                token: "",
                message: "expected a '.', '#', '[', ':', or 'NOT', EOF found".to_string(),
            }),
        }
    }
}

impl<'a> Parse<'a, Namespace<'a>> for Parser {
    fn parse(tokens: &mut &[TokenSpan<'a>]) -> Result<Namespace<'a>, ParseError<'a>> {
        let matched = match tokens.get(0) {
            Some((_, Token::IDENT(ident))) => {
                *tokens = &tokens[1..];
                Namespace::<'a>::Ident(ident)
            }
            Some((_, Token::STAR)) => {
                *tokens = &tokens[1..];
                Namespace::<'a>::All
            }
            Some((span, _token)) => {
                return Err(ParseError::<'a> {
                    index: span.start,
                    token: span.value,
                    message: "expected a '[', none found".to_string(),
                })
            }
            None => {
                return Err(ParseError::<'a> {
                    index: 0,
                    token: "",
                    message: "expected a ']', EOF found".to_string(),
                })
            }
        };
        match tokens.get(0) {
            Some((_, Token::PIPE)) => {
                *tokens = &tokens[1..];
            }
            Some((span, _token)) => {
                return Err(ParseError::<'a> {
                    index: span.start,
                    token: span.value,
                    message: "expected a '[', none found".to_string(),
                })
            }
            None => {
                return Err(ParseError::<'a> {
                    index: 0,
                    token: "",
                    message: "expected a ']', EOF found".to_string(),
                })
            }
        };

        Ok(matched)
    }
}

impl<'a> Parse<'a, TypeSelector<'a>> for Parser {
    fn parse(tokens: &mut &[TokenSpan<'a>]) -> Result<TypeSelector<'a>, ParseError<'a>> {
        let namespace: Option<Namespace<'a>> = Parser::parse_reset(tokens).ok();
        match tokens.get(0) {
            Some((_, Token::IDENT(ident))) => {
                *tokens = &tokens[1..];
                Ok(TypeSelector::<'a> {
                    namespace,
                    element_name: Some(ident),
                })
            }
            Some((_, Token::STAR)) => {
                *tokens = &tokens[1..];
                Ok(TypeSelector::<'a> {
                    namespace,
                    element_name: None,
                })
            }
            Some((span, _token)) => Err(ParseError::<'a> {
                index: span.start,
                token: span.value,
                message: "expected a '*' or ident, none found".to_string(),
            }),
            None => Err(ParseError::<'a> {
                index: 0,
                token: "",
                message: "expected a '*' or ident, EOF found".to_string(),
            }),
        }
    }
}

impl<'a> Parse<'a, Attribute<'a>> for Parser {
    fn parse(tokens: &mut &[TokenSpan<'a>]) -> Result<Attribute<'a>, ParseError<'a>> {
        match tokens.get(0) {
            Some((_, Token::LBRACK)) => {
                *tokens = &tokens[1..];
            }
            Some((span, _token)) => {
                return Err(ParseError::<'a> {
                    index: span.start,
                    token: span.value,
                    message: "expected a '[', none found".to_string(),
                })
            }
            None => {
                return Err(ParseError::<'a> {
                    index: 0,
                    token: "",
                    message: "expected a '[', EOF found".to_string(),
                })
            }
        }
        while let Some((_, Token::S)) = tokens.get(0) {
            *tokens = &tokens[1..];
        }
        let namespace: Option<Namespace<'a>> = Parser::parse_reset(tokens).ok();
        let name = match tokens.get(0) {
            Some((_, Token::IDENT(ident))) => {
                *tokens = &tokens[1..];
                Ok(ident)
            }
            Some((span, _token)) => Err(ParseError::<'a> {
                index: span.start,
                token: span.value,
                message: "expected an ident, none found".to_string(),
            }),
            None => Err(ParseError::<'a> {
                index: 0,
                token: "",
                message: "expected an ident, EOF found".to_string(),
            }),
        }?;
        while let Some((_, Token::S)) = tokens.get(0) {
            *tokens = &tokens[1..];
        }
        let matcher = match tokens.get(0) {
            Some((_, Token::PREFIXMATCH)) => {
                *tokens = &tokens[1..];
                Ok(Matcher::Prefix)
            }
            Some((_, Token::SUFFIXMATCH)) => {
                *tokens = &tokens[1..];
                Ok(Matcher::Suffix)
            }
            Some((_, Token::SUBSTRINGMATCH)) => {
                *tokens = &tokens[1..];
                Ok(Matcher::Substring)
            }
            Some((_, Token::EQ)) => {
                *tokens = &tokens[1..];
                Ok(Matcher::Equal)
            }
            Some((_, Token::INCLUDES)) => {
                *tokens = &tokens[1..];
                Ok(Matcher::Includes)
            }
            Some((_, Token::DASHMATCH)) => {
                *tokens = &tokens[1..];
                Ok(Matcher::Dash)
            }
            Some((_, Token::RBRACK)) => {
                *tokens = &tokens[1..];
                return Ok(Attribute::<'a> {
                    namespace,
                    name,
                    matcher: None,
                    value: None,
                });
            }
            Some((span, _token)) => Err(ParseError::<'a> {
                index: span.start,
                token: span.value,
                message: "expected a '^=', '$=', '*=', '=', '~=', or '|=', none found".to_string(),
            }),
            None => Err(ParseError::<'a> {
                index: 0,
                token: "",
                message: "expected a '^=', '$=', '*=', '=', '~=', or '|=', EOF found".to_string(),
            }),
        }?;
        while let Some((_, Token::S)) = tokens.get(0) {
            *tokens = &tokens[1..];
        }
        let value = match tokens.get(0) {
            Some((_, Token::IDENT(value))) | Some((_, Token::STRING(value))) => {
                *tokens = &tokens[1..];
                Some(*value)
            }
            Some((span, _token)) => {
                return Err(ParseError::<'a> {
                    index: span.start,
                    token: span.value,
                    message: "expected an ident or string, none found".to_string(),
                })
            }
            None => {
                return Err(ParseError::<'a> {
                    index: 0,
                    token: "",
                    message: "expected an ident or string, EOF found".to_string(),
                })
            }
        };
        while let Some((_, Token::S)) = tokens.get(0) {
            *tokens = &tokens[1..];
        }
        match tokens.get(0) {
            Some((_, Token::RBRACK)) => {
                *tokens = &tokens[1..];
            }
            Some((span, _token)) => {
                return Err(ParseError::<'a> {
                    index: span.start,
                    token: span.value,
                    message: "expected a ']', none found".to_string(),
                })
            }
            None => {
                return Err(ParseError::<'a> {
                    index: 0,
                    token: "",
                    message: "expected a ']', EOF found".to_string(),
                })
            }
        }
        Ok(Attribute::<'a> {
            namespace,
            name,
            matcher: Some(matcher),
            value,
        })
    }
}

impl<'a> Parse<'a, Psuedo<'a>> for Parser {
    fn parse(tokens: &mut &[TokenSpan<'a>]) -> Result<Psuedo<'a>, ParseError<'a>> {
        match tokens.get(0) {
            Some((_, Token::COLON)) => {
                *tokens = &tokens[1..];
            }
            Some((span, _token)) => {
                return Err(ParseError::<'a> {
                    index: span.start,
                    token: span.value,
                    message: "expected a ':', none found".to_string(),
                })
            }
            None => {
                return Err(ParseError::<'a> {
                    index: 0,
                    token: "",
                    message: "expected a ':', EOF found".to_string(),
                })
            }
        };
        let is_class_type = match tokens.get(0) {
            Some((_, Token::COLON)) => {
                *tokens = &tokens[1..];
                false
            }
            _ => true,
        };
        match tokens.get(0) {
            Some((_, Token::IDENT(ident))) => {
                *tokens = &tokens[1..];
                Ok(Psuedo::<'a> {
                    is_class_type,
                    name: ident,
                    arg: None,
                })
            }
            Some((_, Token::FUNCTION(ident))) => {
                *tokens = &tokens[1..];
                while let Some((_, Token::S)) = tokens.get(0) {
                    *tokens = &tokens[1..];
                }
                let arg: Expression<'a> = Parser::parse(tokens)?;
                match tokens.get(0) {
                    Some((_, Token::RPAREN)) => {
                        *tokens = &tokens[1..];
                    }
                    Some((span, _token)) => {
                        return Err(ParseError::<'a> {
                            index: span.start,
                            token: span.value,
                            message: "expected a ')', none found".to_string(),
                        })
                    }
                    None => {
                        return Err(ParseError::<'a> {
                            index: 0,
                            token: "",
                            message: "expected a ')', EOF found".to_string(),
                        })
                    }
                };
                Ok(Psuedo::<'a> {
                    is_class_type,
                    name: ident,
                    arg: Some(arg),
                })
            }
            Some((span, _token)) => {
                return Err(ParseError::<'a> {
                    index: span.start,
                    token: span.value,
                    message: "expected an ident or function, none found".to_string(),
                })
            }
            None => {
                return Err(ParseError::<'a> {
                    index: 0,
                    token: "",
                    message: "expected an ident or function, EOF found".to_string(),
                })
            }
        }
    }
}

impl<'a> Parse<'a, Expression<'a>> for Parser {
    fn parse(tokens: &mut &[TokenSpan<'a>]) -> Result<Expression<'a>, ParseError<'a>> {
        let mut items = vec![];
        loop {
            let item = Parser::parse(tokens);
            if item.is_err() {
                break;
            }
            while let Some((_, Token::S)) = tokens.get(0) {
                *tokens = &tokens[1..];
            }
            items.push(item.ok().unwrap());
        }
        Ok(Expression::<'a> { items })
    }
}

impl<'a> Parse<'a, ExpressionItem<'a>> for Parser {
    fn parse(tokens: &mut &[TokenSpan<'a>]) -> Result<ExpressionItem<'a>, ParseError<'a>> {
        match tokens.get(0) {
            Some((_, Token::PLUS)) => {
                *tokens = &tokens[1..];
                Ok(ExpressionItem::<'a>::Plus)
            }
            Some((_, Token::SUB)) => {
                *tokens = &tokens[1..];
                Ok(ExpressionItem::<'a>::Minus)
            }
            Some((_, Token::DIMENSION(dimension, unit))) => {
                *tokens = &tokens[1..];
                Ok(ExpressionItem::<'a>::Dimension(dimension, unit))
            }
            Some((_, Token::NUM(number))) => {
                *tokens = &tokens[1..];
                Ok(ExpressionItem::<'a>::Number(number))
            }
            Some((_, Token::STRING(string))) => {
                *tokens = &tokens[1..];
                Ok(ExpressionItem::<'a>::Str(string))
            }
            Some((_, Token::IDENT(ident))) => {
                *tokens = &tokens[1..];
                Ok(ExpressionItem::<'a>::Ident(ident))
            }
            Some((span, _token)) => {
                return Err(ParseError::<'a> {
                    index: span.start,
                    token: span.value,
                    message: "expected a '+', '-', dimension, number, string, or ident, none found"
                        .to_string(),
                })
            }
            None => {
                return Err(ParseError::<'a> {
                    index: 0,
                    token: "",
                    message: "expected a '+', '-', dimension, number, string, or ident, EOF found"
                        .to_string(),
                })
            }
        }
    }
}

impl<'a> Parse<'a, Negation<'a>> for Parser {
    fn parse(tokens: &mut &[TokenSpan<'a>]) -> Result<Negation<'a>, ParseError<'a>> {
        match tokens.get(0) {
            Some((_, Token::NOT)) => {
                *tokens = &tokens[1..];
            }
            Some((span, _token)) => {
                return Err(ParseError::<'a> {
                    index: span.start,
                    token: span.value,
                    message: "expected a ':not(', none found".to_string(),
                })
            }
            None => {
                return Err(ParseError::<'a> {
                    index: 0,
                    token: "",
                    message: "expected a ':not(', EOF found".to_string(),
                })
            }
        }
        while let Some((_, Token::S)) = tokens.get(0) {
            *tokens = &tokens[1..];
        }
        let result = match tokens.get(0) {
            Some((_, Token::IDENT(_))) | Some((_, Token::STAR)) => {
                Ok(Negation::TypeSelector(Parser::parse(tokens)?))
            }
            Some((_, Token::HASH(hash))) => {
                *tokens = &tokens[1..];
                Ok(Negation::Hash(hash))
            }
            Some((_, Token::DOT(class))) => {
                *tokens = &tokens[1..];
                Ok(Negation::Class(class))
            }
            Some((_, Token::LBRACK)) => Ok(Negation::Attribute(Parser::parse(tokens)?)),
            Some((_, Token::COLON)) => Ok(Negation::Psuedo(Parser::parse(tokens)?)),
            Some((span, _token)) => Err(ParseError::<'a> {
                index: span.start,
                token: span.value,
                message: "expected a '.', '#', '[', ident, '*', or ':', none found".to_string(),
            }),
            None => Err(ParseError::<'a> {
                index: 0,
                token: "",
                message: "expected a '.', '#', '[', ident, '*', or ':', EOF found".to_string(),
            }),
        }?;
        while let Some((_, Token::S)) = tokens.get(0) {
            *tokens = &tokens[1..];
        }
        match tokens.get(0) {
            Some((_, Token::RPAREN)) => {
                *tokens = &tokens[1..];
            }
            Some((span, _token)) => {
                return Err(ParseError::<'a> {
                    index: span.start,
                    token: span.value,
                    message: "expected a ')', none found".to_string(),
                })
            }
            None => {
                return Err(ParseError::<'a> {
                    index: 0,
                    token: "",
                    message: "expected a ')', EOF found".to_string(),
                })
            }
        }
        Ok(result)
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::token::Lexer;
    use crate::PASS_SELECTORS;

    #[test]
    fn pass_tests() {
        for test in PASS_SELECTORS.iter() {
            let lexed = Lexer::parse(test).unwrap();
            // println!("{:?}", lexed.tokens.iter().map(|x| &x.1).collect::<Vec<&Token>>());
            if let Err(e) = parse(&lexed.tokens[..]) {
                panic!("failed to lex {}: {:?}", test, e);
            }
        }
    }
}
