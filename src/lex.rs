use std::char;

use token::{IntSuffix, FloatSuffix, Lit, LToken, IToken, StrStyle};
use error::LexResult;

/// An incremental lexer. Consumes the input string and lexes individual tokens
/// whenever `next` is called.
#[derive(Copy, Clone)]
pub struct Lexer<'a> {
    input: &'a str,
    idx: usize,
}

impl<'a> Lexer<'a> {
    /// Create a new lexer, given the input string
    pub fn new(input: &'a str) -> Lexer<'a> {
        Lexer {
            input: input,
            idx: 0,
        }
    }

    fn byte(&self, idx: usize) -> u8 {
        if idx + self.idx < self.input.len() {
            self.input.as_bytes()[idx + self.idx]
        } else {
            0
        }
    }

    fn opt_byte(&self, idx: usize) -> Option<u8> {
        if idx + self.idx < self.input.len() {
            Some(self.byte(idx))
        } else {
            None
        }
    }

    fn rest(&self) -> &'a str {
        &self.input[self.idx..]
    }

    fn next_char(&self) -> char {
        self.rest().chars().next().unwrap_or('\0')
    }

    fn opt_next_char(&self) -> Option<char> {
        self.rest().chars().next()
    }

    fn block_comment(&mut self) -> LexResult<&'a str> {
        assert!(self.rest().starts_with("/*"));

        let start = self.idx;
        let mut depth = 0;
        while self.rest().len() >= 2 {
            if self.byte(0) == b'/' && self.byte(1) == b'*' {
                depth += 1;
                self.idx += 2; // eat '*'
                continue;
            }

            if self.byte(0) == b'*' && self.byte(1) == b'/' {
                depth -= 1;
                self.idx += 2;
                if depth == 0 {
                    return Ok(&self.input[start..self.idx]);
                }
                continue;
            }

            self.idx += 1;
        }

        fmt_err!(self, "unexpected EOF while parsing block comment");
    }

    fn doc_comment(&mut self) -> LexResult<Option<LToken>> {
        let start = self.idx;
        if self.byte(0) != b'/' {
            return Ok(None);
        }

        let (block_byte, inner_byte) = (self.byte(1), self.byte(2));

        let text = if block_byte == b'/' {
            if let Some(len) = self.rest().find('\n') {
                self.idx += len;
            } else {
                self.idx = self.input.len();
            }

            &self.input[start..self.idx]
        } else if block_byte == b'*' {
            self.block_comment()?
        } else {
            return Ok(None);
        };

        Ok(Some(if inner_byte == block_byte {
            LToken::OuterDoc(text.into())
        } else if inner_byte == b'!' {
            LToken::InnerDoc(text.into())
        } else {
            unreachable!()
        }))
    }

    fn num(&mut self) -> LexResult<Option<LToken>> {
        let start = self.idx;

        let base = match (self.byte(0), self.byte(1)) {
            (b'0', b'x') => {
                self.idx += 2;
                16
            }
            (b'0', b'o') => {
                self.idx += 2;
                8
            }
            (b'0', b'b') => {
                self.idx += 2;
                2
            }
            (b'0'...b'9', _) => 10,
            _ => return Ok(None),
        };

        let mut value = 0u64;
        let mut has_dot = false;
        let mut has_exp = false;
        let mut int_overflow = false;
        loop {
            let b = self.byte(0);
            let digit = match b {
                b'e' | b'E' if base == 10 => {
                    self.idx += 1;
                    has_exp = true;
                    break;
                }
                b'0'...b'9' => (b - b'0') as u64,
                b'a'...b'f' if base > 10 => 10 + (b - b'a') as u64,
                b'A'...b'F' if base > 10 => 10 + (b - b'A') as u64,
                b'_' => {
                    self.idx += 1;
                    continue;
                }
                b'.' if base == 10 => {
                    self.idx += 1;
                    if has_dot {
                        fmt_err!(self, "unexpected 2nd . while parsing float literal");
                    }
                    has_dot = true;
                    continue;
                }
                _ => break,
            };

            if digit >= base {
                fmt_err!(self, "unexpected digit {:x} out of base range", digit);
            }
            if let Some(v) = value.checked_mul(base).and_then(|v| v.checked_add(digit)) {
                value = v
            } else {
                int_overflow = true;
                value = 0;
            }
            self.idx += 1;
        }

        if has_exp {
            let mut has_value = false;
            loop {
                match self.byte(0) {
                    b'+' | b'-' if !has_value => {
                        self.idx += 1;
                    }
                    b'0'...b'9' => {
                        self.idx += 1;
                        has_value = true;
                    }
                    b'_' => {
                        self.idx += 1;
                    }
                    _ => {
                        if !has_value {
                            fmt_err!(self,
                                     "unexpected end of float literal after \
                                        `E` character");
                        } else {
                            break;
                        }
                    }
                }
            }
        }

        let can_be_int = !has_dot && !has_exp;
        let can_be_float = base == 10;

        let node = match (self.byte(0), self.byte(1), self.byte(2), self.byte(3), self.byte(4)) {
            (b'f', b'3', b'2', ..) if can_be_float => {
                self.idx += 3;
                LToken::Lit(Lit::Float(self.input[start..self.idx-3].into(), FloatSuffix::F32))
            }
            (b'f', b'6', b'4', ..) if can_be_float => {
                self.idx += 3;
                LToken::Lit(Lit::Float(self.input[start..self.idx-3].into(), FloatSuffix::F64))
            }
            (b'u', b'8', ..) if can_be_int => {
                self.idx += 2;
                LToken::Lit(Lit::Integer(value, IntSuffix::U8))
            }
            (b'i', b'8', ..) if can_be_int => {
                self.idx += 2;
                LToken::Lit(Lit::Integer(value, IntSuffix::I8))
            }
            (b'u', b'1', b'6', ..) if can_be_int => {
                self.idx += 3;
                LToken::Lit(Lit::Integer(value, IntSuffix::U16))
            }
            (b'i', b'1', b'6', ..) if can_be_int => {
                self.idx += 3;
                LToken::Lit(Lit::Integer(value, IntSuffix::I16))
            }
            (b'u', b'3', b'2', ..) if can_be_int => {
                self.idx += 3;
                LToken::Lit(Lit::Integer(value, IntSuffix::U32))
            }
            (b'i', b'3', b'2', ..) if can_be_int => {
                self.idx += 3;
                LToken::Lit(Lit::Integer(value, IntSuffix::I32))
            }
            (b'u', b'6', b'4', ..) if can_be_int => {
                self.idx += 3;
                LToken::Lit(Lit::Integer(value, IntSuffix::U64))
            }
            (b'i', b'6', b'4', ..) if can_be_int => {
                self.idx += 3;
                LToken::Lit(Lit::Integer(value, IntSuffix::I64))
            }
            (b'u', b's', b'i', b'z', b'e') if can_be_int => {
                self.idx += 5;
                LToken::Lit(Lit::Integer(value, IntSuffix::Usize))
            }
            (b'i', b's', b'i', b'z', b'e') if can_be_int => {
                self.idx += 5;
                LToken::Lit(Lit::Integer(value, IntSuffix::Isize))
            }
            _ => {
                if can_be_int {
                    LToken::Lit(Lit::Integer(value, IntSuffix::Unsuffixed))
                } else if can_be_float {
                    LToken::Lit(Lit::Float(self.input[start..self.idx].into(),
                                           FloatSuffix::Unsuffixed))
                } else {
                    unreachable!()
                }
            }
        };

        if int_overflow {
            if let LToken::Lit(Lit::Integer(..)) = node {
                fmt_err!(self, "overflow while parsing integer literal");
            }
        }

        Ok(Some(node))
    }

    fn raw_str(&mut self) -> LexResult<(String, usize)> {
        let mut pounds = 0;
        while self.byte(0) == b'#' {
            pounds += 1;
            self.idx += 1;
        }
        if self.byte(0) != b'"' {
            fmt_err!(self,
                     "unexpected character while parsing raw string literal");
        }
        self.idx += 1;
        let inner_start = self.idx;

        loop {
            match self.byte(0) {
                b'"' => {
                    let end_start = self.idx;
                    self.idx += 1;
                    let mut cl_pounds = 0;
                    while cl_pounds < pounds {
                        if self.byte(0) != b'#' {
                            break;
                        }
                        cl_pounds += 1;
                        self.idx += 1;
                    }
                    if cl_pounds == pounds {
                        return Ok((self.input[inner_start..end_start].into(), pounds));
                    }
                    self.idx = end_start;
                }
                _ => {}
            }
            self.idx += 1;
        }
    }

    fn backslash_x(&mut self) -> LexResult<u8> {
        let mut ch = 0;
        let b0 = self.byte(0);
        let b1 = self.byte(1);
        ch += 0x10 *
              match b0 {
                  b'0'...b'9' => b0 - b'0',
                  b'a'...b'f' => 10 + (b0 - b'a'),
                  b'A'...b'F' => 10 + (b0 - b'A'),
                  _ => fmt_err!(self, "unexpected non-hex character after \\x"),
              };
        ch += 0x1 *
              match b1 {
                  b'0'...b'9' => b1 - b'0',
                  b'a'...b'f' => 10 + (b1 - b'a'),
                  b'A'...b'F' => 10 + (b1 - b'A'),
                  _ => fmt_err!(self, "unexpected non-hex character after \\x"),
              };
        self.idx += 2;
        Ok(ch)
    }

    fn backslash_u(&mut self) -> LexResult<char> {
        if self.byte(0) != b'{' {
            fmt_err!(self, "expected {{ after \\u");
        }
        self.idx += 1;
        let mut ch = 0;
        for _ in 0..6 {
            let b = self.byte(0);
            match b {
                b'0'...b'9' => {
                    ch *= 0x10;
                    ch += (b - b'0') as u32;
                    self.idx += 1;
                }
                b'a'...b'f' => {
                    ch *= 0x10;
                    ch += (10 + b - b'a') as u32;
                    self.idx += 1;
                }
                b'A'...b'F' => {
                    ch *= 0x10;
                    ch += (10 + b - b'A') as u32;
                    self.idx += 1;
                }
                b'}' => break,
                _ => fmt_err!(self, "unexpected non-hex character after \\u"),
            }
        }
        if self.byte(0) != b'}' {
            fmt_err!(self, "expected }} to terminate \\u unicode hexcode");
        }
        self.idx += 1;
        if let Some(ch) = char::from_u32(ch) {
            Ok(ch)
        } else {
            fmt_err!(self,
                     "character code {:x} is not a valid unicode character",
                     ch);
        }
    }

    fn str_lit(&mut self) -> LexResult<String> {
        let mut s = String::new();
        loop {
            let ch = match self.opt_byte(0) {
                Some(b'"') => {
                    self.idx += 1;
                    return Ok(s.into());
                }
                Some(b'\\') => {
                    match self.opt_byte(1) {
                        Some(b'x') => {
                            self.idx += 2;
                            let byte = self.backslash_x()?;
                            if byte > 0x80 {
                                fmt_err!(self, "invalid \\x byte {:x} in string literal", byte);
                            }
                            char::from_u32(byte as u32).unwrap()
                        }
                        Some(b'u') => {
                            self.idx += 2;
                            self.backslash_u()?
                        }
                        Some(b'n') => {
                            self.idx += 2;
                            '\n'
                        }
                        Some(b'r') => {
                            self.idx += 2;
                            '\r'
                        }
                        Some(b't') => {
                            self.idx += 2;
                            '\t'
                        }
                        Some(b'\\') => {
                            self.idx += 2;
                            '\\'
                        }
                        Some(b'0') => {
                            self.idx += 2;
                            '\0'
                        }
                        Some(b'\'') => {
                            self.idx += 2;
                            '\''
                        }
                        Some(b'"') => {
                            self.idx += 2;
                            '"'
                        }
                        Some(b'\r') |
                        Some(b'\n') => {
                            self.idx += 2;
                            while let Some(ch) = self.opt_next_char() {
                                if ch.is_whitespace() {
                                    self.idx += ch.len_utf8();
                                } else {
                                    break;
                                }
                            }
                            continue;
                        }
                        Some(b) => {
                            fmt_err!(self,
                                     "unexpected byte {:?} after \\ character \
                                                   in byte literal",
                                     b)
                        }
                        None => fmt_err!(self, "unexpected Eof after \\ character in byte literal"),
                    }
                }
                Some(b'\r') => {
                    match self.opt_byte(1) {
                        Some(b'\n') => {
                            self.idx += 2;
                            '\n'
                        }
                        _ => {
                            fmt_err!(self, "Bare CR not allowed in string");
                        }
                    }
                }
                Some(_) => {
                    let ch = self.next_char();
                    self.idx += ch.len_utf8();
                    ch
                }
                None => fmt_err!(self, "unexpected Eof while parsing byte literal"),
            };
            s.push(ch);
        }
    }

    fn byte_str_lit(&mut self) -> LexResult<Vec<u8>> {
        let mut s = Vec::new();
        loop {
            let byte = match self.opt_byte(0) {
                Some(b'"') => {
                    self.idx += 1;
                    return Ok(s);
                }
                Some(b'\\') => {
                    match self.opt_byte(1) {
                        Some(b'x') => {
                            self.idx += 2;
                            self.backslash_x()?
                        }
                        Some(b'n') => {
                            self.idx += 2;
                            b'\n'
                        }
                        Some(b'r') => {
                            self.idx += 2;
                            b'\r'
                        }
                        Some(b't') => {
                            self.idx += 2;
                            b'\t'
                        }
                        Some(b'\\') => {
                            self.idx += 2;
                            b'\\'
                        }
                        Some(b'0') => {
                            self.idx += 2;
                            b'\0'
                        }
                        Some(b'\'') => {
                            self.idx += 2;
                            b'\''
                        }
                        Some(b'"') => {
                            self.idx += 2;
                            b'"'
                        }
                        Some(b'\r') |
                        Some(b'\n') => {
                            self.idx += 2;
                            while let Some(ch) = self.opt_next_char() {
                                if ch.is_whitespace() {
                                    self.idx += ch.len_utf8();
                                } else {
                                    break;
                                }
                            }
                            continue;
                        }
                        Some(b) => {
                            fmt_err!(self,
                                     "unexpected byte {:?} after \\ character \
                                                   in byte literal",
                                     b)
                        }
                        None => fmt_err!(self, "unexpected Eof after \\ character in byte literal"),
                    }
                }
                Some(b'\r') => {
                    match self.opt_byte(1) {
                        Some(b'\n') => {
                            self.idx += 2;
                            b'\n'
                        }
                        _ => {
                            fmt_err!(self, "Bare CR not allowed in string");
                        }
                    }
                }
                Some(b) => {
                    self.idx += 1;
                    b
                }
                None => fmt_err!(self, "unexpected Eof while parsing byte literal"),
            };
            s.push(byte);
        }
    }

    fn char_lit(&mut self) -> LexResult<char> {
        let ch = match self.opt_byte(0) {
            Some(b'\\') => {
                match self.opt_byte(1) {
                    Some(b'x') => {
                        self.idx += 2;
                        let byte = self.backslash_x()?;
                        assert!(byte <= 0x80);
                        char::from_u32(byte as u32).unwrap()
                    }
                    Some(b'u') => {
                        self.idx += 2;
                        self.backslash_u()?
                    }
                    Some(b'n') => {
                        self.idx += 2;
                        '\n'
                    }
                    Some(b'r') => {
                        self.idx += 2;
                        '\r'
                    }
                    Some(b't') => {
                        self.idx += 2;
                        '\t'
                    }
                    Some(b'\\') => {
                        self.idx += 2;
                        '\\'
                    }
                    Some(b'0') => {
                        self.idx += 2;
                        '\0'
                    }
                    Some(b'\'') => {
                        self.idx += 2;
                        '\''
                    }
                    Some(b'"') => {
                        self.idx += 2;
                        '"'
                    }
                    Some(b) => {
                        fmt_err!(self,
                                 "unexpected byte {:?} after \\ character \
                                               in byte literal",
                                 b)
                    }
                    None => fmt_err!(self, "unexpected Eof after \\ character in byte literal"),
                }
            }
            Some(_) => {
                let ch = self.next_char();
                self.idx += ch.len_utf8();
                ch
            }
            None => fmt_err!(self, "unexpected Eof while parsing byte literal"),
        };

        if self.byte(0) != b'\'' {
            self.idx += 1;
            Ok(ch)
        } else {
            fmt_err!(self, "unexpected error while parsing char literal");
        }
    }

    fn byte_lit(&mut self) -> LexResult<u8> {
        let byte = match self.opt_byte(0) {
            Some(b'\\') => {
                match self.opt_byte(1) {
                    Some(b'x') => {
                        self.idx += 2;
                        self.backslash_x()?
                    }
                    Some(b'n') => {
                        self.idx += 2;
                        b'\n'
                    }
                    Some(b'r') => {
                        self.idx += 2;
                        b'\r'
                    }
                    Some(b't') => {
                        self.idx += 2;
                        b'\t'
                    }
                    Some(b'\\') => {
                        self.idx += 2;
                        b'\\'
                    }
                    Some(b'0') => {
                        self.idx += 2;
                        b'\0'
                    }
                    Some(b'\'') => {
                        self.idx += 2;
                        b'\''
                    }
                    Some(b'"') => {
                        self.idx += 2;
                        b'"'
                    }
                    Some(b) => {
                        fmt_err!(self,
                                 "unexpected byte {:?} after \\ character \
                                               in byte literal",
                                 b)
                    }
                    None => fmt_err!(self, "unexpected Eof after \\ character in byte literal"),
                }
            }
            Some(b) => {
                // NOTE: At this point, self.idx doesn't necessarially point to
                // a byte boundary, however, the below assertion will assert
                // that we are.
                self.idx += 1;
                b
            }
            None => fmt_err!(self, "unexpected Eof while parsing byte literal"),
        };

        assert!(self.byte(0) == b'\'');
        self.idx += 1;
        Ok(byte)
    }

    fn stringlike(&mut self) -> LexResult<Option<LToken>> {
        let node = match (self.byte(0), self.byte(1), self.byte(2)) {
            (b'b', b'\'', ..) => {
                self.idx += 2;
                LToken::Lit(Lit::Byte(self.byte_lit()?))
            }
            (b'b', b'"', ..) => {
                self.idx += 2;
                LToken::Lit(Lit::ByteStr(self.byte_str_lit()?, StrStyle::Cooked))
            }
            (b'\'', ..) => {
                self.idx += 1;
                LToken::Lit(Lit::Char(self.char_lit()?))
            }
            (b'"', ..) => {
                self.idx += 1;
                LToken::Lit(Lit::Str(self.str_lit()?, StrStyle::Cooked))
            }
            (b'b', b'r', ..) => {
                self.idx += 2;
                let (raw, pounds) = self.raw_str()?;
                LToken::Lit(Lit::ByteStr(raw.into_bytes(), StrStyle::Raw(pounds)))
            }
            (b'r', ..) => {
                self.idx += 1;
                let (raw, pounds) = self.raw_str()?;
                LToken::Lit(Lit::Str(raw, StrStyle::Raw(pounds)))
            }
            _ => return Ok(None),
        };

        Ok(Some(node))
    }

    pub fn lex_literal(&mut self) -> LexResult<LToken> {
        let t = if let Some(t) = self.doc_comment()? {
            t
        } else if let Some(t) = self.num()? {
            t
        } else if let Some(t) = self.stringlike()? {
            t
        } else {
            fmt_err!(self,
                     "unexpected character {:?} does not start any literal-like token",
                     self.next_char());
        };

        if self.rest().len() > 0 {
            fmt_err!(self,
                     "unexpected character {:?} while parsing literal-like token",
                     self.next_char());
        }

        Ok(t)
    }
}

pub fn relex_literal(input: &str) -> LexResult<LToken> {
    Lexer::new(input).lex_literal()
}

pub fn relex_ident(input: &str) -> LexResult<IToken> {
    if input.starts_with('\'') {
        Ok(IToken::Lifetime(input.to_owned()))
    } else {
        Ok(match input {
            "_" => IToken::Underscore,
            "abstract" => IToken::Abstract,
            "alignof" => IToken::Alignof,
            "as" => IToken::As,
            "become" => IToken::Become,
            "box" => IToken::Box,
            "break" => IToken::Break,
            "const" => IToken::Const,
            "continue" => IToken::Continue,
            "crate" => IToken::Crate,
            "do" => IToken::Do,
            "else" => IToken::Else,
            "enum" => IToken::Enum,
            "extern" => IToken::Extern,
            "false" => IToken::False,
            "final" => IToken::Final,
            "fn" => IToken::Fn,
            "for" => IToken::For,
            "if" => IToken::If,
            "impl" => IToken::Impl,
            "in" => IToken::In,
            "let" => IToken::Let,
            "loop" => IToken::Loop,
            "macro" => IToken::Macro,
            "match" => IToken::Match,
            "mod" => IToken::Mod,
            "move" => IToken::Move,
            "mut" => IToken::Mut,
            "offsetof" => IToken::Offsetof,
            "override" => IToken::Override,
            "priv" => IToken::Priv,
            "proc" => IToken::Proc,
            "pub" => IToken::Pub,
            "pure" => IToken::Pure,
            "ref" => IToken::Ref,
            "return" => IToken::Return,
            "Self" => IToken::CapSelf,
            "self" => IToken::LowSelf,
            "sizeof" => IToken::Sizeof,
            "static" => IToken::Static,
            "struct" => IToken::Struct,
            "super" => IToken::Super,
            "trait" => IToken::Trait,
            "true" => IToken::True,
            "type" => IToken::Type,
            "typeof" => IToken::Typeof,
            "unsafe" => IToken::Unsafe,
            "unsized" => IToken::Unsized,
            "use" => IToken::Use,
            "virtual" => IToken::Virtual,
            "where" => IToken::Where,
            "while" => IToken::While,
            "yield" => IToken::Yield,
            id => IToken::Ident(id.into()),
        })
    }
}
