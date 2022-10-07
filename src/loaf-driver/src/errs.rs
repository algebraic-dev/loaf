use loaf_elaborate::{
    errors::{BinderKind, ElaborationError},
    eval::quote,
};
use loaf_parser::errors::SyntaxError;
use loaf_report::error::{Annotation, Color, ErrorDescription, Marked::*, Phrase, Severity, Style};

pub fn syntax_err_to_description(err: &SyntaxError) -> ErrorDescription {
    use SyntaxError::*;
    match err {
        UnfinishedString(pos) => {
            let mut err = ErrorDescription::new(1, pos.until_next());
            err.set_title(Phrase::bright("Unfinished string."));
            err.add_pos(Annotation::bright(Color::Fst, "The string ends here!", pos.until_next()));
            err
        }
        UnexpectedChar(pos, chr) => {
            let mut err = ErrorDescription::new(1, pos.until_next());
            err.set_title(Phrase::new(Style::Bright, vec![Normal("Unexpected char".to_string()), Quoted(chr.to_string())]));
            err.add_pos(Annotation::bright(Color::Fst, "The string ends here!", pos.until_next()));
            err
        }
        UnexpectedToken(range, tkn) => {
            let mut err = ErrorDescription::new(1, range.clone());
            err.set_title(Phrase::new(Style::Bright, vec![Normal("Unexpected token".to_string()), Normal(tkn.to_string())]));
            err.add_pos(Annotation::bright(Color::Fst, "Here!", range.clone()));
            err
        }
    }
}

pub fn elaboration_err_to_desc(err: &ElaborationError) -> ErrorDescription {
    match err {
        ElaborationError::NotATypeConstructor(range) => {
            let mut err = ErrorDescription::new(1, range.clone());
            err.set_title(Phrase::new(Style::Bright, vec![Normal("Not a type constructor".to_string())]));
            err.add_pos(Annotation::bright(Color::Fst, "Here!", range.clone()));
            err
        }
        ElaborationError::NotADataConstructor(range, name) => {
            let mut err = ErrorDescription::new(1, range.clone());
            err.set_title(Phrase::new(Style::Bright, vec![Normal("Not a data constructor of".to_string()), Quoted(name.to_string())]));
            err.add_pos(Annotation::bright(Color::Fst, "Here!", range.clone()));
            err
        }
        ElaborationError::CannotFindVariable(range, name) => {
            let mut err = ErrorDescription::new(1, range.clone());
            err.set_title(Phrase::new(Style::Bright, vec![Normal("Cannot find variable".to_string()), Quoted(name.to_string())]));
            err.add_pos(Annotation::bright(Color::Fst, "Here!", range.clone()));
            err
        }
        ElaborationError::CantInfer(range) => {
            let mut err = ErrorDescription::new(1, range.clone());
            err.set_title(Phrase::bright("Cannot infer type of the expression"));
            err.add_pos(Annotation::bright(Color::Fst, "Here!", range.clone()));
            err
        }
        ElaborationError::Inspection(ctx, got) => {
            let mut err = ErrorDescription::new(1, ctx.pos.clone());
            err.severity = Severity::Information;
            err.set_title(Phrase::bright("Inspection"));

            let got_str = format!("{}", got.with_ctx(&ctx.env));

            err.add_subtitle(
                Color::Snd,
                Phrase {
                    style: Style::Bright,
                    words: vec![Normal("Expected:".to_string()), Colored(Color::Snd, Style::Bright, Box::new(Normal(got_str)))],
                },
            );

            for (name, (entry, _)) in &ctx.types {
                let got_str = format!("{}", quote(entry, ctx.env.depth).with_ctx(&ctx.env));
                err.add_subtitle(
                    Color::Snd,
                    Phrase {
                        style: Style::Bright,
                        words: vec![Normal(format!("{:<8}:", name)), Colored(Color::Snd, Style::Bright, Box::new(Normal(got_str)))],
                    },
                );
            }

            err.add_pos(Annotation::bright(Color::Snd, "Here!", ctx.pos.clone()));
            err
        }
        ElaborationError::Mismatch(ctx, got, expected) => {
            let mut err = ErrorDescription::new(1, ctx.pos.clone());
            let expected_str = format!("{}", expected.with_ctx(&ctx.env));
            let got_str = format!("{}", got.with_ctx(&ctx.env));

            err.add_subtitle(
                Color::Fst,
                Phrase {
                    style: Style::Bright,
                    words: vec![Normal("Expected:".to_string()), Colored(Color::Fst, Style::Bright, Box::new(Normal(expected_str)))],
                },
            );

            err.add_subtitle(
                Color::Snd,
                Phrase {
                    style: Style::Bright,
                    words: vec![Normal("     Got:".to_string()), Colored(Color::Snd, Style::Bright, Box::new(Normal(got_str)))],
                },
            );

            err.set_title(Phrase::bright("Type mismatch."));

            err.add_pos(Annotation::bright(Color::Fst, "Here!", ctx.pos.clone()));

            err
        }
        ElaborationError::ExpectedType(ctx, kind, _, got) => {
            let mut err = ErrorDescription::new(1, ctx.pos.clone());
            let got_str = format!("{}", got.with_ctx(&ctx.env));

            err.add_subtitle(
                Color::Fst,
                Phrase {
                    style: Style::Bright,
                    words: vec![Normal("Got the type".to_string()), Colored(Color::Fst, Style::Bright, Box::new(Normal(got_str.clone())))],
                },
            );

            let kind = match kind {
                BinderKind::PiType => "a function".to_string(),
                BinderKind::SigmaType => "a tuple".to_string(),
            };

            err.add_pos(Annotation::bright(Color::Fst, &format!("It's not {}!", kind), ctx.pos.clone()));

            err.set_title(Phrase {
                style: Style::Bright,
                words: vec![Normal("Was expecting".to_string()), Colored(Color::Fst, Style::Bright, Box::new(Normal(kind)))],
            });

            err
        }
    }
}
