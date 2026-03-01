use nom::{
    ErrorConvert, IResult,
    error::{ErrorKind, ParseError},
};

#[derive(PartialEq, Clone)]

pub(super) struct ExiError<I> {
    pub(super) kind: ExiErrorKind,
    pub(super) input: I,
}

impl<I> core::fmt::Debug for ExiError<I> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.kind)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub(super) enum ExiErrorKind {
    /// Functionality not implemented (yet)
    NotImplemented(String),
    /// Called parse on a terminated grammar
    GrammarTerminated,
    BadString,
    NomError(ErrorKind),
    /// External options were provided when the decoded EXI contained options
    ExternalOptionsClash,
}

pub(super) fn make_exierror<I>(input: I, kind: ExiErrorKind) -> ExiError<I> {
    ExiError { input, kind }
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

impl<I, O> From<ExiError<I>> for IResult<I, O, ExiError<I>> {
    fn from(val: ExiError<I>) -> Self {
        Err(nom::Err::Failure(val))
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
