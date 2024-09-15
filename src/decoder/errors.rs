use nom::{
    error::{ErrorKind, ParseError},
    ErrorConvert,
};

#[derive(PartialEq, Clone, Debug)]

pub(super) struct ExiError<I> {
    pub(super) kind: ExiErrorKind,
    pub(super) input: I,
}

#[derive(Debug, PartialEq, Clone)]
pub(super) enum ExiErrorKind {
    /// Called parse on a terminated grammar
    GrammarTerminated,
    BadString,
    NomError(ErrorKind),
}

pub(super) fn make_exierror<I>(input: I, kind: ExiErrorKind) -> ExiError<I> {
    ExiError { input, kind: kind }
}

impl<I> ExiError<I> {
    pub(crate) fn map_input<F, J>(self, f: F) -> ExiError<J>
    where
        F: FnOnce(I) -> J,
    {
        ExiError {
            input: f(self.input),
            kind: self.kind,
        }
    }
}

impl From<ErrorKind> for ExiErrorKind {
    fn from(value: ErrorKind) -> Self {
        Self::NomError(value)
    }
}

impl<I> ParseError<I> for ExiError<I> {
    fn from_error_kind(input: I, kind: ErrorKind) -> Self {
        Self {
            input,
            kind: kind.into(),
        }
    }

    fn append(_: I, _: ErrorKind, other: Self) -> Self {
        other
    }
}

// Turn nom errors into exierrors
impl<I> From<nom::error::Error<I>> for ExiError<I> {
    fn from(value: nom::error::Error<I>) -> Self {
        Self {
            input: value.input,
            kind: ExiErrorKind::NomError(value.code),
        }
    }
}

// We need to implement this so that we can convert back to "byte-level" errors at the top
// of our parse tree (where the nom::bits conversion happens)
impl<I> ErrorConvert<ExiError<I>> for ExiError<(I, usize)> {
    fn convert(self) -> ExiError<I> {
        ExiError {
            kind: self.kind,
            input: self.input.0,
        }
    }
}
