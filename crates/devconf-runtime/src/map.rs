use devconf_ast::AstScope;
use devconf_lexer::Lexer;

use crate::property::Table;

/// Inner parses a configuration string into a Table.
#[must_use]
pub fn parse(ast: AstScope) -> Table {
    Table::from_ast(ast)
}

/// Parses a configuration string into a Table.
///
/// # Panics
///
/// Panics if the lexer fails to parse the input string.
#[must_use]
pub fn parse_from_str(input: &str) -> Table {
    let scope = AstScope::from_tokens(input, Lexer::from_str(input).unwrap());
    parse(scope)
}
