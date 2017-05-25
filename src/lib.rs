#[macro_use]
mod error;
mod lex;
mod token;

pub use error::{LexError, LexResult};
pub use lex::{relex_literal, relex_ident};
pub use token::{LToken, IToken, Lit, FloatSuffix, IntSuffix, StrStyle};
