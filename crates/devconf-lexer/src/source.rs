use ariadne::{Label, Report, ReportKind, Source};
use std::{backtrace::Backtrace, fmt};
use winnow::{
    LocatingSlice,
    error::{AddContext, ParserError},
    stream::{Location, Stream},
};

use crate::token::Span;

pub type SourceLexer<'a> = LocatingSlice<&'a str>;
pub type LexerResult<'a, T = ()> = Result<T, LexerError<'a>>;

#[derive(Debug)]
pub struct LexerError<'a> {
    base: &'a str,
    span: Span,
    labels: Vec<(Span, String)>,
    backtrace: Backtrace,
}

pub trait SourceLexerExt<'a> {
    fn base(&self) -> &'a str;
    fn span(&self) -> Span;
    fn error(&self, msg: impl fmt::Display) -> !;
}

impl<'a> SourceLexerExt<'a> for SourceLexer<'a> {
    fn base(&self) -> &'a str {
        let mut base = *self;
        base.reset_to_start();
        *base
    }

    fn span(&self) -> Span {
        Span::char(self.current_token_start())
    }

    fn error(&self, msg: impl fmt::Display) -> ! {
        _ = Report::build(ReportKind::Error, self.span())
            .with_message(&msg)
            .with_label(
                Label::new(self.span())
                    .with_message(msg)
                    .with_color(ariadne::Color::BrightRed),
            )
            .finish()
            .eprint(Source::from(self.base()));

        std::process::exit(1);
    }
}

impl<'i> ParserError<SourceLexer<'i>> for LexerError<'i> {
    type Inner = Self;

    fn into_inner(self) -> winnow::Result<Self::Inner, Self> {
        Ok(self)
    }

    fn from_input(input: &SourceLexer<'i>) -> Self {
        Self {
            base: input.base(),
            span: input.span(),
            labels: Vec::new(),
            backtrace: Backtrace::force_capture(),
        }
    }
}

impl<'i, C: ToString> AddContext<SourceLexer<'i>, C> for LexerError<'i> {
    fn add_context(
        mut self,
        input: &SourceLexer<'i>,
        _token_start: &<SourceLexer<'i> as Stream>::Checkpoint,
        context: C,
    ) -> Self {
        self.labels.push((input.span(), context.to_string()));
        self
    }
}

impl fmt::Display for LexerError<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        _ = Report::build(ReportKind::Error, self.span)
            .with_labels(
                self.labels
                    .iter()
                    .map(|(span, msg)| Label::new(*span).with_message(msg)),
            )
            .finish()
            .eprint(Source::from(self.base));

        let backtrace = self.backtrace.to_string();

        let mut backtrace = backtrace.split('\n');

        writeln!(f, "backtrace:")?;
        while let Some(line) = backtrace.next() {
            let Some(colon_idx) = line.find(':') else {
                break;
            };

            let module = &line[colon_idx + 2..];
            let Some(file_line) = backtrace.next() else {
                break;
            };

            if module.starts_with("std") || module.starts_with("core") {
                continue;
            }

            let Some(at_idx) = file_line.find("at ") else {
                break;
            };

            let file = &file_line[at_idx + 3..];

            if !file.starts_with('.') {
                continue;
            }

            writeln!(f, "  \x1b[31mat \x1b[33m{file} \x1b[31m({module})\x1b[0m")?;
        }

        Ok(())
    }
}
