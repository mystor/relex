#[derive(Debug, Clone, Eq, PartialEq)]
pub enum IToken {
    // Lifetime
    Lifetime(String),

    // Identifier-likes
    Ident(String),
    Underscore,

    // Keywords
    Abstract,
    Alignof,
    As,
    Become,
    Box,
    Break,
    Const,
    Continue,
    Crate,
    Do,
    Else,
    Enum,
    Extern,
    Final,
    Fn,
    For,
    If,
    Impl,
    In,
    Let,
    Loop,
    Macro,
    Match,
    Mod,
    Move,
    Mut,
    Offsetof,
    Override,
    Priv,
    Proc,
    Pub,
    Pure,
    Ref,
    Return,
    CapSelf,
    LowSelf,
    Sizeof,
    Static,
    Struct,
    Super,
    Trait,
    Type,
    Typeof,
    Unsafe,
    Unsized,
    Use,
    Virtual,
    Where,
    While,
    Yield,
    True,
    False,
}

/// A single parsed token.
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum LToken {
    // Doc comments
    InnerDoc(String),
    OuterDoc(String),

    // Literals
    Lit(Lit),
}

/// A rust literal value.
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Lit {
    Byte(u8),
    Char(char),
    Integer(u64, IntSuffix),
    Float(String, FloatSuffix),
    Str(String, StrStyle),
    ByteStr(Vec<u8>, StrStyle),
}

/// The value of the suffix on a floating point number (e.g. `10.4f32` would
/// have the suffix `FloatSuffix::F32`).
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum FloatSuffix {
    Unsuffixed,
    F32,
    F64,
}

/// The value of the suffix on a integer number (e.g. `10i32` would have the
/// suffix `IntSuffix::I32`).
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum IntSuffix {
    Unsuffixed,
    Isize,
    Usize,
    I8,
    U8,
    I16,
    U16,
    I32,
    U32,
    I64,
    U64,
}

/// The type of string we're looking at. Either "cooked" or r#"raw"# where the
/// `usize` is the number of `#` characters in the raw string.
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum StrStyle {
    Cooked,
    Raw(usize),
}
