use loaf_report::error::{Annotation, Color, ErrorDescription, Marked, Phrase, Severity, Style};
use loaf_report::render::{Chars, Colors, RenderConfig};
use loaf_span::{Position, Range};

fn main() {
    let desc = ErrorDescription {
        code: 1,
        severity: Severity::Error,
        filename: "./pudim.ume".to_string(),
        title: Phrase {
            style: Style::Bright,
            words: vec![Marked::Normal("Type Mismatch".to_string())],
        },
        subtitles: vec![
            (
                Color::Fst,
                Phrase {
                    style: Style::Bright,
                    words: vec![
                        Marked::Normal("Expected:".to_string()),
                        Marked::Colored(Color::Fst, Style::Normal, Box::new(Marked::Normal("Int".to_string()))),
                    ],
                },
            ),
            (
                Color::Snd,
                Phrase {
                    style: Style::Bright,
                    words: vec![
                        Marked::Normal("     Got:".to_string()),
                        Marked::Colored(Color::Snd, Style::Normal, Box::new(Marked::Normal("String".to_string()))),
                    ],
                },
            ),
        ],
        hints: vec![Phrase {
            style: Style::Normal,
            words: vec![Marked::Normal("Awoooo apapraamrpamrap".to_string())],
        }],
        positions: vec![
            Annotation {
                color: Color::Fst,
                phrase: Phrase {
                    style: Style::Normal,
                    words: vec![Marked::Normal("This part is wrong bro".to_string())],
                },
                range: Range {
                    start: Position { line: 11, column: 12, index: 3 },
                    end: Position { line: 15, column: 13, index: 7 },
                },
            },
            Annotation {
                color: Color::Snd,
                phrase: Phrase {
                    style: Style::Normal,
                    words: vec![Marked::Normal("This entire thing is wrong".to_string())],
                },
                range: Range {
                    start: Position { line: 13, column: 12, index: 3 },
                    end: Position { line: 13, column: 13, index: 7 },
                },
            },
            Annotation {
                color: Color::Trd,
                phrase: Phrase {
                    style: Style::Normal,
                    words: vec![Marked::Normal("Awo".to_string())],
                },
                range: Range {
                    start: Position { line: 13, column: 16, index: 3 },
                    end: Position { line: 13, column: 18, index: 7 },
                },
            },
        ],
    };

    let render_config = RenderConfig {
        chars: Chars {
            vbar: "│",
            bullet: "•",
            vbar_pont: "┆",
            turnl: "└",
            hbar: "─",
            downright: "└",
            upright: "┌",
            sidedown: "┬",
        },
        colors: Colors {
            fg_fst: "\x1b[31m",
            fg_snd: "\x1b[34m",
            fg_trd: "\x1b[32m",
            fg_fth: "\x1b[33m",
            fg_fft: "\x1b[36m",
            bg_fst: "\x1b[41m",
            bg_snd: "\x1b[44m",
            bg_trd: "\x1b[42m",
            bg_fth: "\x1b[43m",
            bright: "\x1b[1m",
            dim: "\x1b[2m",
            reset: "\x1b[0m",
        },
        indent: 4,
    };

    let code = "let Void   : ★ = (x: ★) -> x in
let elim-0 : (a: Void -> ★) -> (x: Void) -> a x = λmotive. λob. ob (motive ob) in
let absurd : (a: ★) -> (x: Void) -> a = λt. λvoid. elim-0 (λx. t) void in

let Nat : ★ = (nat: ★) -> (s: nat -> nat) -> (z: nat) -> nat in
let z   : Nat = λnat. λs. λz. z in
let s   : Nat -> Nat = λn. λnat. λs. λz. s (n nat s z) in

let Unit : ★  = (x: ★) -> x -> x in
let unit : Unit = λx. λu. u in
let elim-1 : 
    (a: Unit -> ★) -> (x: a unit) -> 
    (y: Unit) -> a y = λmotive.
        λx. λy. π-2 
        (y (Σ xe : Unit . motive xe) 
        (⟨unit, x⟩)) in
★
Footer";

    let mut buf = String::new();

    desc.to_message(code).render(&render_config, &mut buf).expect("Cannot write");

    println!("{}", buf);
}
