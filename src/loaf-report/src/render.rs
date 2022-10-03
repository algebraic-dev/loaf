use crate::error::{Annotation, Color, ErrorMessage, Marked, Phrase, Severity, Style};
use core::fmt;
use std::{cmp::Ordering, collections::HashMap, fmt::Write};

#[macro_export]
macro_rules! write_mult {
    ( $channel:expr, $len:expr, $chr:expr ) => {{
        for _ in 0..$len {
            write!($channel, "{}", $chr)?;
        }
        Ok(())
    }};
}

pub struct Chars<'a> {
    pub vbar: &'a str,
    pub hbar: &'a str,
    pub vbar_pont: &'a str,
    pub bullet: &'a str,
    pub turnl: &'a str,
    pub downright: &'a str,
    pub upright: &'a str,
    pub sidedown: &'a str,
}

pub struct Colors<'a> {
    pub fg_fst: &'a str,
    pub fg_snd: &'a str,
    pub fg_trd: &'a str,
    pub fg_fth: &'a str,
    pub fg_fft: &'a str,
    pub bg_fst: &'a str,
    pub bg_snd: &'a str,
    pub bg_trd: &'a str,
    pub bg_fth: &'a str,
    pub bright: &'a str,
    pub dim: &'a str,
    pub reset: &'a str,
}

pub struct RenderConfig<'a> {
    pub chars: Chars<'a>,
    pub colors: Colors<'a>,
    pub indent: usize,
}

impl<'a> RenderConfig<'a> {
    pub fn ascii() -> RenderConfig<'a> {
        RenderConfig {
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
        }
    }
}

pub fn group_markers<'a>(markers: &'a mut [Annotation]) -> (HashMap<u32, Vec<&'a Annotation>>, Vec<&'a Annotation>, Vec<u32>) {
    markers.sort_by(|l, r| match l.range.start.line.cmp(&r.range.start.line) {
        Ordering::Equal => l.range.start.column.cmp(&r.range.start.column),
        other => other,
    });

    let mut lines = Vec::new();

    let mut inline: HashMap<u32, Vec<&'a Annotation>> = HashMap::new();
    let mut multilines: Vec<&Annotation> = Vec::new();

    for marker in markers {
        if marker.range.start.line == marker.range.end.line {
            lines.push(marker.range.start.line);
            if let Some(vec) = inline.get_mut(&marker.range.start.line) {
                vec.push(marker);
            } else {
                inline.insert(marker.range.start.line, vec![marker]);
            }
        } else {
            if marker.range.end.line - marker.range.start.line > 3 {
                lines.push(marker.range.start.line);
                lines.push(marker.range.end.line);
            } else {
                for i in marker.range.start.line..=marker.range.end.line {
                    lines.push(i);
                }
            }
            multilines.push(marker);
        }
    }

    lines.sort();
    lines.dedup();

    (inline, multilines, lines)
}

#[derive(PartialEq, Eq)]
pub enum Mode {
    Start,
    End,
    Middle,
    Out,
}

pub fn mode_to_str(config: &RenderConfig, mode: &Mode, multi_line: &Option<&Annotation>) -> Result<String, fmt::Error> {
    let mut line = String::new();
    match multi_line {
        Some(multi_line) => {
            colorize(&config.colors, &multi_line.color, &mut line)?;
            stylize(&config.colors, &Style::Bright, &mut line)?;
        }
        None => (),
    }
    match mode {
        Mode::Start => write!(line, " {} ", config.chars.upright)?,
        Mode::End => write!(line, " {} ", config.chars.downright)?,
        Mode::Middle => write!(line, " {} ", config.chars.vbar)?,
        Mode::Out => write!(line, "   ")?,
    }
    write!(line, "{}", config.colors.reset)?;
    Ok(line)
}

// Useful for unicode but probably we have a better way to do that?
pub fn split_on(text: String, on: usize) -> (String, String) {
    let mut st = String::with_capacity(on);
    let mut iter = text.chars();
    for _ in 0..on {
        match iter.next() {
            Some(x) => st.push(x),
            None => (),
        };
    }
    (st, iter.collect())
}

pub fn color_text(config: &RenderConfig, text: String, markers: &[&Annotation]) -> Result<String, fmt::Error> {
    let mut line = String::new();
    let mut old = text;
    let mut max = 0;
    // abcdefg
    for marker in markers {
        if marker.range.end.column > max {
            let end = {
                let (start, middle) = split_on(old, marker.range.start.column.saturating_sub(max) as usize);
                let (middle, end) = split_on(middle, (marker.range.end.column - marker.range.start.column + 1) as usize);
                write!(line, "{}", start)?;
                colorize(&config.colors, &marker.color, &mut line)?;
                stylize(&config.colors, &Style::Bright, &mut line)?;
                write!(line, "{}", middle)?;
                write!(line, "{}", config.colors.reset)?;
                end
            };
            max = marker.range.end.column + 1;
            old = end.to_string();
        }
    }
    write!(line, "{}", old)?;
    Ok(line)
}

pub fn render_code<'a>(config: &RenderConfig, code: &'a str, markers: &mut [Annotation], channel: &mut dyn Write) -> Result<(), fmt::Error> {
    let code_lines = code.lines().collect::<Vec<&str>>();
    let (mut inline, multi_lines, lines) = group_markers(markers);

    for i in 0..lines.len() {
        let line = lines[i];
        let mut vec = Vec::new();

        let inlined = inline.get_mut(&(line as u32)).unwrap_or(&mut vec);
        inlined.sort_by(|l, r| l.range.start.column.cmp(&r.range.start.column));

        let mut mode = Mode::Out;
        let mut chosen = None;
        let mut finalizing = false;

        for multi_line in &multi_lines {
            chosen = Some(*multi_line);
            if multi_line.range.start.line == line {
                mode = Mode::Start;
                break;
            } else if multi_line.range.end.line == line {
                mode = Mode::Middle;
                finalizing = true;
            } else if line > multi_line.range.start.line && line < multi_line.range.end.line {
                mode = Mode::Middle;
                break;
            }
        }

        let multi = mode_to_str(config, &mode, &chosen)?;

        let mut code = color_text(config, code_lines[line as usize].to_string(), inlined)?;

        if let Some(marker) = chosen {
            if inlined.is_empty() {
                let mut line = String::with_capacity(code.len());
                colorize(&config.colors, &marker.color, &mut line)?;
                stylize(&config.colors, &Style::Bright, &mut line)?;
                write!(line, "{}", code)?;
                write!(line, "{}", config.colors.reset)?;
                code = line;
            }
        }

        writeln!(channel, "{:>w$} {}{}{}", line + 1, config.chars.vbar, multi, code, w = config.indent - 1)?;

        write!(channel, "{}", config.colors.reset)?;

        mode = match mode {
            Mode::Start => Mode::Middle,
            Mode::Middle => Mode::Middle,
            Mode::End => Mode::Out,
            Mode::Out => Mode::Out,
        };

        let multi = mode_to_str(config, &mode, &chosen)?;

        // Prints mark

        if !inlined.is_empty() {
            let mut line = String::new();
            let mut size = 0;
            for ann in &*inlined {
                colorize(&config.colors, &ann.color, &mut line)?;
                stylize(&config.colors, &Style::Bright, &mut line)?;
                let left = ann.range.start.column - size;
                write_mult!(line, left, " ")?;
                write!(line, "{}", config.chars.sidedown)?;
                let mark = (ann.range.end.column - ann.range.start.column).saturating_sub(1);
                write_mult!(line, mark, config.chars.hbar)?;
                write!(line, "{}", config.colors.reset)?;
                size += left + 1 + mark;
            }

            writeln!(channel, "{:>w$}{}{}", config.chars.vbar_pont, multi, line, w = config.indent + 1)?;

            for i in 0..inlined.len() {
                let mut line = String::new();
                let mut size = 0;
                let to = inlined.len() - i;
                for (j, ann) in inlined.iter().enumerate().take(to) {
                    let is_last = j == to - 1;
                    let chr = if is_last { config.chars.downright } else { config.chars.vbar };
                    colorize(&config.colors, &ann.color, &mut line)?;
                    stylize(&config.colors, &Style::Bright, &mut line)?;
                    let left = ann.range.start.column - size;
                    write_mult!(line, left, " ")?;
                    write!(line, "{}", chr)?;
                    if is_last {
                        write!(line, " ")?;
                        ann.phrase.render(config, &mut line)?;
                    }
                    write!(line, "{}", config.colors.reset)?;
                    size += left + 1;
                }
                writeln!(channel, "{:>w$}{}{}", config.chars.vbar_pont, multi, line, w = config.indent + 1)?;
            }
        }

        if finalizing || i == lines.len() - 1 {
            if let Some(ann) = chosen {
                writeln!(channel, "{:>w$} {}{}", "", config.chars.vbar_pont, multi, w = config.indent - 1)?;
                write!(channel, "{:>w$} {}", "", config.chars.vbar_pont, w = config.indent - 1)?;
                colorize(&config.colors, &ann.color, channel)?;
                stylize(&config.colors, &Style::Bright, channel)?;
                write!(channel, " {} ", config.chars.downright)?;
                ann.phrase.render(config, channel)?;
                writeln!(channel, "{}", config.colors.reset)?;
            }
        }

        if i < lines.len() - 1 && lines[i + 1] - lines[i] > 1 {
            stylize(&config.colors, &Style::Dim, channel)?;
            writeln!(channel, "{:>w$} {}{}", "", config.chars.vbar_pont, multi, w = config.indent - 1)?;
            write!(channel, "{}", config.colors.reset)?;
        }
    }

    Ok(())
}

pub fn stylize<'a>(colors: &Colors<'a>, style: &Style, channel: &mut dyn Write) -> Result<(), fmt::Error> {
    let style = match style {
        Style::Bright => colors.bright,
        Style::Dim => colors.dim,
        Style::Normal => "",
    };
    write!(channel, "{}", style)
}

pub fn colorize<'a>(colors: &Colors<'a>, color: &Color, channel: &mut dyn Write) -> Result<(), fmt::Error> {
    let color = match color {
        Color::Fst => colors.fg_fst,
        Color::Snd => colors.fg_snd,
        Color::Trd => colors.fg_trd,
        Color::Fth => colors.fg_fth,
        Color::Fft => colors.fg_fft,
    };
    write!(channel, "{}", color)
}

impl Severity {
    pub fn render<'a>(&self, colors: &Colors<'a>, channel: &mut dyn Write) -> Result<(), fmt::Error> {
        match self {
            Severity::Error => write!(channel, " {}{} ERROR {} ", colors.bright, colors.bg_fst, colors.reset),
            Severity::Information => write!(channel, " {}{} INFO {} ", colors.bright, colors.bg_snd, colors.reset),
            Severity::Warning => write!(channel, " {}{} WARNING {} ", colors.bright, colors.bg_fth, colors.reset),
        }
    }
}

impl Marked {
    pub fn render<'a>(&self, config: &RenderConfig<'a>, channel: &mut dyn Write) -> Result<(), fmt::Error> {
        match self {
            Marked::Normal(s) => write!(channel, "{}", s),
            Marked::Quoted(m) => write!(channel, "\"{}\"", m),
            Marked::Colored(c, s, m) => {
                colorize(&config.colors, c, channel)?;
                stylize(&config.colors, s, channel)?;
                m.render(config, channel)?;
                write!(channel, "{}", config.colors.reset)
            }
        }
    }
}

impl Phrase {
    pub fn render<'a>(&self, config: &RenderConfig<'a>, channel: &mut dyn Write) -> Result<(), fmt::Error> {
        if !self.words.is_empty() {
            stylize(&config.colors, &self.style, channel)?;
            self.words[0].render(config, channel)?;
            for i in 1..self.words.len() {
                stylize(&config.colors, &self.style, channel)?;
                write!(channel, " ")?;
                self.words[i].render(config, channel)?;
            }
            write!(channel, "{}", config.colors.reset)?;
        }
        Ok(())
    }
}

impl<'a> ErrorMessage<'a> {
    pub fn render(&self, config: &RenderConfig<'a>, w: &mut dyn Write) -> Result<(), fmt::Error> {
        // Header

        writeln!(w)?;
        self.desc.severity.render(&config.colors, w)?;
        self.desc.title.render(config, w)?;

        // Subtitles

        writeln!(w)?;
        if !self.desc.subtitles.is_empty() {
            writeln!(w)?;
        }
        for (color, text) in &self.desc.subtitles {
            write!(w, "{: >1$}", "", config.indent)?;
            colorize(&config.colors, color, w)?;
            stylize(&config.colors, &Style::Bright, w)?;
            write!(w, "{} ", config.chars.bullet)?;
            write!(w, "{}", config.colors.reset)?;
            text.render(config, w)?;
            writeln!(w)?;
        }

        let mut pos = self.desc.positions.clone();

        writeln!(w)?;
        colorize(&config.colors, &Color::Fft, w)?;
        stylize(&config.colors, &Style::Dim, w)?;
        writeln!(w, "{:>w$} {} {}:{}", "", config.chars.upright, self.filename, self.desc.canon_pos, w = config.indent - 1)?;
        write!(w, "{}", config.colors.reset)?;
        writeln!(w, "{:>w$} {}", "", config.chars.vbar, w = config.indent - 1)?;
        render_code(config, self.code, &mut pos, w)?;

        // Hints

        if !self.desc.hints.is_empty() {
            writeln!(w)?;
        }

        for phrase in &self.desc.hints {
            write!(w, "{: >1$}", "", config.indent)?;
            colorize(&config.colors, &Color::Fft, w)?;
            stylize(&config.colors, &Style::Bright, w)?;
            write!(w, "Hint: {}{}", config.colors.reset, config.colors.fg_fft)?;
            phrase.render(config, w)?;
            write!(w, "{}", config.colors.reset)?;
            writeln!(w)?;
        }
        Ok(())
    }
}
