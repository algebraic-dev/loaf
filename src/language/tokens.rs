use std::fmt;

#[derive(Debug, Clone)]
pub enum Token {
    Id(String),  // x
    Num(usize),  // 1
    Str(String), // "x"
    LPar,        // (
    RPar,        // )
    Dot,         // .
    Colon,       // :
    Comma,       // ,
    Pipe,        // |
    Eq,          // =
    LBracket,    // ⟨
    RBracket,    // ⟩
    Sigma,       // Σ
    Hlp,         // ?

    // Keywords
    In,   // in
    Let,  // let
    Type, // type
    Eof,  // eof
    If,   // if
    Then, // then
    Else, // else

    // Special ones (Normally unicode)
    Lambda, // λ
    Arrow,  // →
    Star,   // ★
    Pi,     // Π

    Left,   // π-1
    Right   // π-2
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use Token::*;

        match self {
            Id(string) => write!(f, "Id '{}'", string),
            Num(num) => write!(f, "Num '{}'", num),
            Str(string) => write!(f, "Str '{}'", string),
            LPar => write!(f, "'('"),
            RPar => write!(f, "')'"),
            Dot => write!(f, "'.'"),
            Colon => write!(f, "':'"),
            Pipe => write!(f, "'|'"),
            Eq => write!(f, "'='"),
            In => write!(f, "'in'"),
            Let => write!(f, "'let'"),
            If => write!(f, "'if'"),
            Then => write!(f, "'then'"),
            Else => write!(f, "'else'"),
            Comma => write!(f, "','"),
            Type => write!(f, "'type'"),
            Eof => write!(f, "'eof'"),
            Lambda => write!(f, "'λ'"),
            Arrow => write!(f, "'→'"),
            Star => write!(f, "'★'"),
            Hlp => write!(f, "?"),
            Pi => write!(f, "'Π'"),
            LBracket => write!(f, "'⟨'"),
            RBracket => write!(f, "'⟩'"),
            Sigma => write!(f, "'Σ'"),
            Left => write!(f, "'π-1'"),
            Right => write!(f, "'π-2'"),
        }
    }
}
