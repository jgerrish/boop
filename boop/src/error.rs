//! Forth stack wrapper with Rust Error results and behavior
#![warn(missing_docs)]

use core::fmt::{Debug, Display, Formatter, Result};
// use heapless::pool::singleton::Box;
// use heapless::pool::Box;

/// An error that can occur when operating on a stack
#[derive(Eq, PartialEq)]
pub struct Error {
    /// A member structure representing the type or kind of error
    pub kind: ErrorKind,
}

impl Debug for Error {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "{}", self.kind)
    }
}

impl Display for Error {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "{}", self.kind)
    }
}

impl Error {
    /// Create a new Error with a given ErrorKind variant
    pub fn new(kind: ErrorKind) -> Error {
        Error { kind }
    }
}

/// The kinds of errors that can occur working with stacks
#[derive(Eq, PartialEq)]
pub enum ErrorKind {
    /// ASCII Encode error
    /// Indicates a character or string is not properly ASCII encoded
    /// This might be confusing, and reworking ASCIIEncode and
    /// UnicodeEncode to be consistent is probably a good idea.
    ASCIIEncode,
    /// Out of memory error
    OutOfMemory,
    /// Word not found in dictionary
    WordNotFound,
    /// Word is too long to add to the dictionary
    WordTooLong,
    /// Word is too short to add to the dictionary
    WordTooShort,
    /// A stack overflow would occur if an item is pushed
    StackOverflow,
    /// A stack underflow would occur if an item is popped
    StackUnderflow,
    /// An error encoding a value as UTF-32 Unicode
    UnicodeEncode,
    /// An unknown error type
    Unknown,
}

impl Display for ErrorKind {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match self {
            ErrorKind::ASCIIEncode => write!(f, "ASCII encoding error"),
            ErrorKind::OutOfMemory => write!(f, "Out of Memory"),
            ErrorKind::WordNotFound => write!(f, "Word not found in dictionary"),
            ErrorKind::WordTooLong => write!(f, "Word is too long to add to dictionary"),
            ErrorKind::WordTooShort => write!(f, "Word is too short to add to dictionary"),
            ErrorKind::StackOverflow => write!(f, "A stack overflow occurred"),
            ErrorKind::StackUnderflow => write!(f, "A stack underflow occurred"),
            ErrorKind::UnicodeEncode => write!(f, "Unicode encoding error"),
            ErrorKind::Unknown => write!(f, "An unknown error occurred"),
        }
    }
}

impl Debug for ErrorKind {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "{}", self)
    }
}
