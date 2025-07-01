use std::fs;

use devconf_ast::AstScope;
use devconf_lexer::Lexer;

fn main() {
    let path = fs::read_to_string("primitives.devconf")
        .unwrap_or_else(|err| panic!("Cannot read because of: {err}"));

    let tokens = Lexer::from_str(&path)
        .inspect(|tokens| println!("<tokens>: {tokens:#?}"))
        .inspect_err(|err| println!("<error>: {err}"));

    _ = AstScope::from_tokens(&path, tokens.unwrap())
        .0
        .iter()
        .inspect(|ast| println!("<ast>: {ast:#?}"))
}
