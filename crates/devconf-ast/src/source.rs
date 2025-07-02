use core::fmt;
use std::collections::VecDeque;
use std::ops;

use ariadne::{Color, Label, Report, ReportBuilder, ReportKind, Source};

use devconf_lexer::token::{Span, SpannedToken, Token};
use devconf_tychecker::TypeChecker;

#[derive(Clone, Debug)]
pub struct SourceAst<'i> {
    pub base: &'i str,
    pub last_offset: usize,
    pub tokens: VecDeque<SpannedToken>,
    pub checker: TypeChecker,
}

#[derive(Debug)]
pub struct PeekedToken<'i, 's> {
    pub source: &'s mut SourceAst<'i>,
    pub token: SpannedToken,
    pub last_offset: usize,
}

impl<'i> SourceAst<'i> {
    pub fn new(base: &'i str, tokens: VecDeque<SpannedToken>) -> Self {
        Self {
            base,
            tokens,
            last_offset: 0,
            checker: TypeChecker,
        }
    }

    pub fn with(&self, tokens: VecDeque<SpannedToken>) -> Self {
        Self {
            tokens,
            base: self.base,
            last_offset: self.last_offset,
            checker: TypeChecker,
        }
    }

    pub fn peek<'a>(&'a mut self) -> Option<PeekedToken<'i, 'a>> {
        self.tokens.pop_front().map(|token| {
            let last_span = self.last_offset;
            self.last_offset = token.span.to;
            PeekedToken {
                token,
                last_offset: last_span,
                source: self,
            }
        })
    }

    pub fn peek_expect<'a>(&'a mut self) -> PeekedToken<'i, 'a> {
        // The implementation cannot be done with `peek` call
        // because of borrow checker :|
        let Some(token) = self.tokens.pop_front() else {
            self.error_in_place("Unexpected EOF");
        };

        let last_offset = self.last_offset;
        self.last_offset = token.span.to;
        PeekedToken {
            token,
            last_offset,
            source: self,
        }
    }

    pub fn expect(&mut self) -> SpannedToken {
        self.tokens
            .pop_front()
            .inspect(|t| self.last_offset = t.span.to)
            .unwrap_or_else(|| self.error_in_place("Unexpected EOF"))
    }

    pub fn expect_msg(&mut self, msg: impl fmt::Display) -> SpannedToken {
        self.tokens
            .pop_front()
            .inspect(|t| self.last_offset = t.span.to)
            .unwrap_or_else(|| self.error_in_place(format!("Unexpected EOF. {msg}")))
    }

    pub fn expect_match<T>(
        &mut self,
        msg: impl fmt::Display + Clone,
        predicate: impl Fn(SpannedToken) -> Option<T>,
    ) -> T {
        let expected_err = format!("Expected {msg}");
        let first = self.expect_msg(&expected_err);

        let span = first.span;
        let unexpected_err = format!("Unexpected token: {first:?}");

        if let Some(t) = predicate(first) {
            self.last_offset = span.to;
            t
        } else {
            self.error_build(
                span,
                |b| {
                    b.with_message(unexpected_err).with_label(
                        Label::new(span)
                            .with_color(Color::BrightRed)
                            .with_message(expected_err),
                    )
                },
                msg.clone(),
            );
        }
    }

    pub fn expect_token(&mut self, token: Token) -> SpannedToken {
        self.expect_match(format!("{token:#?}"), |t| (t == token).then_some(t))
    }

    pub fn error_in_place(&self, msg: impl fmt::Display + Clone) -> ! {
        self.error_build(
            Span::char(self.last_offset),
            |b| b.with_message(&msg),
            msg.clone(),
        )
    }

    pub fn error_at(&self, span: Span, msg: impl fmt::Display + Clone) -> ! {
        let cloned_msg = msg.clone();
        self.error_build(
            span,
            |b| {
                b.with_message(&msg).with_label(
                    Label::new(span)
                        .with_message(msg)
                        .with_color(ariadne::Color::BrightRed),
                )
            },
            cloned_msg,
        )
    }

    pub fn error_build(
        &self,
        span: Span,
        fun: impl FnOnce(ReportBuilder<Span>) -> ReportBuilder<Span>,
        msg: impl fmt::Display,
    ) -> ! {
        if !cfg!(test) {
            _ = fun(Report::build(ReportKind::Error, span))
                .finish()
                .print(Source::from(self.base));
        }

        panic!("{msg}");
    }
}

impl PeekedToken<'_, '_> {
    pub fn accept(self) -> SpannedToken {
        self.token
    }

    pub fn recover(self) {
        self.source.last_offset = self.last_offset;
        self.source.tokens.push_front(self.token);
    }
}

impl ops::Deref for PeekedToken<'_, '_> {
    type Target = SpannedToken;

    fn deref(&self) -> &Self::Target {
        &self.token
    }
}
