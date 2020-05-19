use regex::Regex;
use std::fmt;

/// https://www.w3.org/TR/2018/REC-selectors-3-20181106/#lex
#[derive(Debug, Clone)]
pub enum Token<'a> {
    S,
    INCLUDES,
    DASHMATCH,
    PREFIXMATCH,
    SUFFIXMATCH,
    SUBSTRINGMATCH,
    IDENT(&'a str),
    STRING(&'a str),
    FUNCTION(&'a str),
    NUM(&'a str),
    HASH(&'a str),
    PLUS,
    GREATER,
    COMMA,
    TILDE,
    NOT,
    ATKEYWORD(&'a str),
    INVALID(&'a str),
    PERCENTAGE(&'a str),
    DIMENSION(&'a str, &'a str),
    CDO,
    CDC,
    COMMENTS(&'a str),
    COLON, // non-standard to aid in parsing follows
    DOT(&'a str),
    STAR,
    PIPE,
    SUB,
    LBRACK,
    RBRACK,
    EQ,
    RPAREN,
}

#[derive(Debug, Clone)]
pub struct Span<'a> {
    pub start: usize,
    pub stop: usize,
    pub value: &'a str,
}

pub type TokenSpan<'a> = (Span<'a>, Token<'a>);

pub struct Lexer<'a> {
    pub src: &'a str,
    pub tokens: Vec<TokenSpan<'a>>,
}

#[derive(Debug, Clone)]
pub struct LexerError<'a> {
    pub span: Span<'a>,
    pub message: String,
}

impl<'a> fmt::Display for LexerError<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "failed to lex @ {}:{} ('{}'): {}",
            self.span.start, self.span.stop, self.span.value, self.message
        )
    }
}

lazy_static! {
    static ref S: Regex = Regex::new(r"(?i)^[ \t\r\n\f]+").unwrap();
    static ref IDENT: Regex = Regex::new(r"(?i)^[-]?(?:[_a-z]|[^\x00-\x7F]|\\[0-9a-f]{1,6}(\r\n|[ \n\r\t\f])?|\\[^\n\r\f0-9a-f])(?:[_a-z0-9-]|[^\x00-\x7F]|\\[0-9a-f]{1,6}(\r\n|[ \n\r\t\f])?|\\[^\n\r\f0-9a-f])*").unwrap();
    static ref STRING: Regex = Regex::new(r#"(?i)^(?:"([^\n\r\f\\"]|\\\n|\r\n|\r|\f|[^\x00-\x7F]|\\[0-9a-f]{1,6}(\r\n|[ \n\r\t\f])?|\\[^\n\r\f0-9a-f])*")|(?:'([^\n\r\f\\"]|\\\n|\r\n|\r|\f|[^\x00-\x7F]|\\[0-9a-f]{1,6}(\r\n|[ \n\r\t\f])?|\\[^\n\r\f0-9a-f])*')"#).unwrap();
    static ref NUMBER: Regex = Regex::new(r"(?i)^(?:[0-9]+|[0-9]*\.[0-9]+)").unwrap();
    static ref PLUS_GREATER_COMMA_TILDE: Regex = Regex::new(r"(?i)^[ \t\r\n\f]*(\+|>|,|~)").unwrap();
    static ref NOT: Regex = Regex::new(r#"(?i)^:(?:n|\\0{0,4}(4e|6e)(\r\n|[ \t\r\n\f])?|\\n)(?:o|\\0{0,4}(4f|6f)(\r\n|[ \t\r\n\f])?|\\o)(?:t|\\0{0,4}(54|74)(\r\n|[ \t\r\n\f])?|\\t)\("#).unwrap();
    static ref COMMENTS: Regex = Regex::new(r"(?i)^/\*[^*]*\*+([^/*][^*]*\*+)*/").unwrap();
}

fn match_ident<'a>(src: &'a str) -> Option<&'a str> {
    IDENT.find(src).map(|ident| {
        assert_eq!(ident.start(), 0);
        &src[..ident.end()]
    })
}

fn match_token_raw<'a>(src: &mut &'a str) -> Result<Token<'a>, String> {
    if src.starts_with("~=") {
        *src = &src[2..];
        Ok(Token::INCLUDES)
    } else if src.starts_with("|=") {
        *src = &src[2..];
        Ok(Token::DASHMATCH)
    } else if src.starts_with("^=") {
        *src = &src[2..];
        Ok(Token::PREFIXMATCH)
    } else if src.starts_with("^=") {
        *src = &src[2..];
        Ok(Token::PREFIXMATCH)
    } else if src.starts_with("$=") {
        *src = &src[2..];
        Ok(Token::SUFFIXMATCH)
    } else if src.starts_with("*=") {
        *src = &src[2..];
        Ok(Token::SUBSTRINGMATCH)
    } else if let Some(ident) = IDENT.find(src) {
        assert_eq!(ident.start(), 0);
        let span = &src[0..ident.end()];
        *src = &src[ident.end()..];
        if src.starts_with("(") {
            *src = &src[1..];
            Ok(Token::FUNCTION(span))
        } else {
            Ok(Token::IDENT(span))
        }
    } else if let Some(string) = STRING.captures(src) {
        let internal = string.get(1).unwrap();
        let string = string.get(0).unwrap();
        assert_eq!(string.start(), 0);
        let span = &src[internal.start()..internal.end()];
        *src = &src[string.end()..];
        Ok(Token::STRING(span))
    } else if let Some(num) = NUMBER.find(src) {
        assert_eq!(num.start(), 0);
        let span = &src[0..num.end()];
        *src = &src[num.end()..];
        if src.starts_with("%") {
            *src = &src[1..];
            Ok(Token::PERCENTAGE(span))
        } else if let Some(ident) = IDENT.find(src).map(|ident| {
            assert_eq!(ident.start(), 0);
            let span = &src[..ident.end()];
            *src = &src[ident.end()..];
            span
        }) {
            Ok(Token::DIMENSION(span, ident))
        } else {
            Ok(Token::NUM(span))
        }
    } else if src.starts_with("#") {
        *src = &src[1..];
        let ident = match_ident(*src);
        if ident.is_none() {
            return Err("expected ident after '#'".to_string());
        }
        let ident = ident.unwrap();
        *src = &src[ident.len()..];
        Ok(Token::HASH(ident))
    } else if let Some(result) = PLUS_GREATER_COMMA_TILDE.captures(src) {
        let item = result.get(1).unwrap();
        let result = result.get(0).unwrap();
        assert_eq!(result.start(), 0);
        *src = &src[result.end()..];
        match item.as_str() {
            "+" => Ok(Token::PLUS),
            ">" => Ok(Token::GREATER),
            "," => Ok(Token::COMMA),
            "~" => Ok(Token::TILDE),
            x => panic!("unexpected regex match group value: {}", x),
        }
    } else if let Some(not) = NOT.find(src) {
        assert_eq!(not.start(), 0);
        *src = &src[not.end()..];
        Ok(Token::NOT)
    } else if src.starts_with("@") {
        *src = &src[1..];
        let ident = match_ident(*src);
        if ident.is_none() {
            return Err("expected ident after '@'".to_string());
        }
        let ident = ident.unwrap();
        *src = &src[ident.len()..];
        Ok(Token::ATKEYWORD(ident))
    } else if let Some(s) = S.find(src) {
        assert_eq!(s.start(), 0);
        *src = &src[s.end()..];
        Ok(Token::S)
    } else if src.starts_with("<!--") {
        *src = &src[4..];
        Ok(Token::CDO)
    } else if src.starts_with("-->") {
        *src = &src[3..];
        Ok(Token::CDC)
    } else if src.starts_with(":") {
        *src = &src[1..];
        Ok(Token::COLON)
    } else if src.starts_with(".") {
        *src = &src[1..];
        let ident = match_ident(*src);
        if ident.is_none() {
            return Err("expected ident after '.'".to_string());
        }
        let ident = ident.unwrap();
        *src = &src[ident.len()..];
        Ok(Token::DOT(ident))
    } else if src.starts_with("*") {
        *src = &src[1..];
        Ok(Token::STAR)
    } else if src.starts_with("|") {
        *src = &src[1..];
        Ok(Token::PIPE)
    } else if src.starts_with("-") {
        *src = &src[1..];
        Ok(Token::SUB)
    } else if src.starts_with("[") {
        *src = &src[1..];
        Ok(Token::LBRACK)
    } else if src.starts_with("]") {
        *src = &src[1..];
        Ok(Token::RBRACK)
    } else if src.starts_with("=") {
        *src = &src[1..];
        Ok(Token::EQ)
    } else if src.starts_with(")") {
        *src = &src[1..];
        Ok(Token::RPAREN)
    } else if let Some(comments) = COMMENTS.find(src) {
        assert_eq!(comments.start(), 0);
        let span = &src[0..comments.end()];
        *src = &src[comments.end()..];
        Ok(Token::COMMENTS(span))
    } else if let Some(s) = S.find(src) {
        assert_eq!(s.start(), 0);
        *src = &src[s.end()..];
        Ok(Token::S)
    } else {
        if src.len() > 0 {
            *src = &src[1..];
        }
        Err("no valid token matching".to_string())
    }
}

fn match_token<'a>(index: usize, src: &mut &'a str) -> Result<TokenSpan<'a>, LexerError<'a>> {
    let mut new_src = *src;
    let result = match_token_raw(&mut new_src);
    let span = Span::<'a> {
        start: index,
        stop: index + (src.len() - new_src.len()),
        value: &src[0..(src.len() - new_src.len())],
    };
    match result {
        Ok(token) => {
            *src = new_src;
            Ok((span, token))
        }
        Err(message) => Err(LexerError::<'a> { span, message }),
    }
}

impl<'a> Lexer<'a> {
    pub fn parse(src: &'a str) -> Result<Lexer<'a>, LexerError<'a>> {
        let mut index: usize = 0;
        let mut new_src = src;
        let mut tokens: Vec<TokenSpan> = vec![];
        while new_src.len() > 0 {
            match match_token(index, &mut new_src) {
                Ok((span, token)) => {
                    index += span.stop - span.start;
                    tokens.push((span, token));
                }
                Err(e) => {
                    return Err(e);
                }
            }
        }
        Ok(Lexer::<'a> { src, tokens })
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::PASS_SELECTORS;

    #[test]
    fn pass_tests() {
        for test in PASS_SELECTORS.iter() {
            if let Err(e) = Lexer::parse(test) {
                panic!("failed to lex {}: {:?}", test, e);
            }
        }
    }
}
