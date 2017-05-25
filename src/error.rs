use std::error::Error;
use std::fmt::{self, Display};

macro_rules! fmt_err {
    ($lexer:expr, $($rest:expr),*) => {
        return {
            Err($crate::error::LexError {
                reason: format!($($rest),*),
            })
        }
    }
}

/// The result type used by `rustlex`.
pub type LexResult<R> = Result<R, LexError>;

/// An error triggered during lexing.
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct LexError {
    /// A human readable string describing the cause of the error
    pub reason: String,
}

impl Display for LexError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.reason)
    }
}

impl Error for LexError {
    fn description(&self) -> &str {
        &self.reason
    }
}
