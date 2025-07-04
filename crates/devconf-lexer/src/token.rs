use core::fmt;
use std::ops::Range;

#[derive(Debug, Clone, Copy)]
pub struct Span {
    pub from: usize,
    pub to: usize,
}

impl ariadne::Span for Span {
    type SourceId = ();

    fn source(&self) -> &Self::SourceId {
        &()
    }

    fn start(&self) -> usize {
        self.from
    }

    fn end(&self) -> usize {
        self.to
    }
}

pub trait IntoSpan<V> {
    fn into_span(self) -> V;
}

impl Span {
    pub const ZERO: Self = Self { from: 0, to: 0 };

    pub const fn char(offset: usize) -> Self {
        Self {
            from: offset,
            to: offset + 1,
        }
    }
}

impl From<Range<usize>> for Span {
    fn from(value: Range<usize>) -> Self {
        Self {
            from: value.start,
            to: value.end,
        }
    }
}

impl<T> IntoSpan<(T, Span)> for (T, Range<usize>) {
    fn into_span(self) -> (T, Span) {
        (self.0, Span::from(self.1))
    }
}

#[derive(Debug, Clone)]
pub struct SpannedToken {
    pub span: Span,
    pub token: Token,
}

impl SpannedToken {
    pub fn new(span: Span, token: Token) -> Self {
        Self { span, token }
    }

    pub fn split(&self) -> (Span, Token) {
        (self.span, self.token.clone())
    }
}

impl std::ops::Deref for SpannedToken {
    type Target = Token;

    fn deref(&self) -> &Self::Target {
        &self.token
    }
}

impl PartialEq for SpannedToken {
    fn eq(&self, other: &Self) -> bool {
        self.token == other.token
    }
}

impl PartialEq<Token> for SpannedToken {
    fn eq(&self, other: &Token) -> bool {
        self.token == *other
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Comment(String),          // comments
    Ident(String),            // key
    Punctuation(Punctuation), // punctuation
    Literal(Literal),         // values
    Keyword(Keyword),         // for meta-programming
}

impl Token {
    pub fn into_ident(&self) -> Option<String> {
        if let Self::Ident(ident) = self {
            Some(ident.clone())
        } else {
            None
        }
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Token::Comment(comment) => write!(f, "# {comment}"),
            Token::Ident(ident) => write!(f, "{ident}"),
            Token::Punctuation(punct) => write!(f, "{punct}"),
            Token::Literal(lit) => write!(f, "{lit}"),
            Token::Keyword(keyword) => write!(f, "{keyword}"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Punctuation {
    Comma,
    Semicolon,
    Colon,
    Dot,
    Dollar,

    Equals,   // =
    Eq,       // ==
    Ne,       // !=
    And,      // &&
    Or,       // ||
    Bang,     // !, used for negation
    Question, // ?

    Newline,
    Identation,
    OpenBrace,
    CloseBrace,
    OpenParen,
    CloseParen,
    OpenSquareBracket,
    CloseSquareBracket,
}

impl fmt::Display for Punctuation {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let symbol = match self {
            Punctuation::Comma => ",",
            Punctuation::Semicolon => ";",
            Punctuation::Colon => ":",
            Punctuation::Dot => ".",
            Punctuation::Dollar => "$",

            Punctuation::Equals => "=",
            Punctuation::Eq => "==",
            Punctuation::Ne => "!=",
            Punctuation::And => "&&",
            Punctuation::Or => "||",
            Punctuation::Bang => "!",
            Punctuation::Question => "?",

            Punctuation::Newline => "\n",
            Punctuation::Identation => "  ",
            Punctuation::OpenBrace => "{",
            Punctuation::CloseBrace => "}",
            Punctuation::OpenParen => "(",
            Punctuation::CloseParen => ")",
            Punctuation::OpenSquareBracket => "[",
            Punctuation::CloseSquareBracket => "]",
        };
        write!(f, "{symbol}")
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    String(String),         // "hello world" | 'hello world'
    UnquotedString(String), // hello
    Integer(i64),           // 42
    Float(f64),             // 3.14
    Boolean(bool),          // true or false
    Null,                   // null
}

impl fmt::Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Literal::String(s) => write!(f, "\"{s}\""),
            Literal::UnquotedString(s) => write!(f, "{s}"),
            Literal::Integer(i) => write!(f, "{i}"),
            Literal::Float(fl) => write!(f, "{fl}"),
            Literal::Boolean(b) => write!(f, "{b}"),
            Literal::Null => write!(f, "null"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Keyword {
    If,
    Else,
    Use,
    Template,
}

impl fmt::Display for Keyword {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let keyword = match self {
            Keyword::If => "@if",
            Keyword::Else => "@else",
            Keyword::Use => "@use",
            Keyword::Template => "@template",
        };
        write!(f, "{keyword}")
    }
}
