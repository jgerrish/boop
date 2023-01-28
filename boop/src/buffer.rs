//! Rust wrapper and test code for the buffer / queue
use core::fmt::Write;

/// The basic operations for a queue
pub trait Queue {
    /// Initialize the queue
    fn init(
        &self,
        buffer_start: *mut u32,
        buffer_end: *const u32,
        buffer_init: fn(*mut u32, *const u32),
    );

    /// Clear the queue
    fn clear(&self, buffer_clear: fn());
}

/// Settings for a buffer or queue
pub struct BufferSettings {
    /// Buffer address
    pub buffer_addr: *mut u32,
    /// Buffer size
    pub buffer_size: usize,
    /// Buffer end
    pub buffer_end: *const u32,
}

/// Collection of functions for the buffer data structure
/// This is primarily used to make testing easier
pub struct BufferFunctions {
    /// The initialization function for the buffer
    pub buffer_init_safe: fn(*mut u32, *const u32),
    /// The clear or reset function for the buffer
    pub buffer_clear_safe: fn(),
    /// The enqueue function for the buffer
    pub buffer_write_word_safe: fn(u32) -> core::result::Result<(), Error>,
    /// The dequeue function for the buffer
    pub buffer_read_word_safe: fn() -> core::result::Result<u32, Error>,
}

/// Circular buffer array-based implementation for a queue
pub struct Buffer<'a> {
    /// The output stream for writing debug and log messages
    pub writer: &'a mut dyn Write,
    /// Settings for the buffer
    pub settings: BufferSettings,
    /// Functions for the buffer
    pub functions: BufferFunctions,
}

impl<'a> Buffer<'a> {
    /// Create a new buffer
    pub fn new(
        writer: &'a mut dyn Write,
        settings: BufferSettings,
        functions: BufferFunctions,
    ) -> Buffer<'a> {
        Buffer {
            writer,
            settings,
            functions,
        }
    }
}

impl<'a> Queue for Buffer<'a> {
    /// Initialize the buffer
    fn init(
        &self,
        buffer_start: *mut u32,
        buffer_end: *const u32,
        buffer_init: fn(*mut u32, *const u32),
    ) {
        buffer_init(buffer_start, buffer_end);
    }

    /// Clear the buffer
    fn clear(&self, buffer_clear: fn()) {
        buffer_clear();
    }
}

use core::fmt::{Debug, Display, Formatter, Result};

/// An error that can occur when operating on a queue
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
    /// A queue error when dequeue is called on an empty queue
    Empty,
    /// A queue error when enqueue is called on a full queue
    Full,
    /// An unknown error type
    Unknown,
}

impl Display for ErrorKind {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match self {
            ErrorKind::Empty => write!(f, "Dequeue called on an empty queue"),
            ErrorKind::Full => write!(f, "Enqueue called on a full queue"),
            ErrorKind::Unknown => write!(f, "An unknown error occurred"),
        }
    }
}

impl Debug for ErrorKind {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "{}", self)
    }
}

/// Run the buffer tests
pub fn run_tests(buffer: &mut Buffer) {
    write!(
        buffer.writer,
        "buffer end in Rust: 0x{:X}\r\n",
        buffer.settings.buffer_end as u32
    )
    .unwrap();

    write!(
        buffer.writer,
        "FORTH_BUFFER: 0x{:X}\r\n",
        buffer.settings.buffer_addr as u32
    )
    .unwrap();

    write!(buffer.writer, "before buffer init\r\n").unwrap();
    buffer.init(
        buffer.settings.buffer_addr,
        buffer.settings.buffer_end,
        buffer.functions.buffer_init_safe,
    );
    write!(buffer.writer, "after buffer init\r\n").unwrap();

    tests::test_write_word(buffer.writer, &buffer.functions, 0x34852052);

    // reset for the next test
    buffer.init(
        buffer.settings.buffer_addr,
        buffer.settings.buffer_end,
        buffer.functions.buffer_init_safe,
    );

    // Test the clear function
    tests::test_clear_works(buffer.writer, &buffer.settings, &buffer.functions);

    // reset for the next test
    (buffer.functions.buffer_clear_safe)();

    tests::test_read_word(buffer.writer, &buffer.functions, 0x34852052);

    // reset for the next test
    (buffer.functions.buffer_clear_safe)();

    // test that writing two words and reading two words works
    tests::test_read_two_words_works(buffer.writer, &buffer.functions, 0x34852052, 0x58602760);

    // reset for the next test
    (buffer.functions.buffer_clear_safe)();

    tests::test_read_word_empty_fails(buffer.writer, &buffer.functions, 0x34852052);

    // reset for the next test
    (buffer.functions.buffer_clear_safe)();

    tests::test_write_and_read_after_read_fail_works(buffer.writer, &buffer.functions, 0x34852052);

    // reset for the next test
    (buffer.functions.buffer_clear_safe)();

    tests::test_simple_buffer_overflow_fails(buffer.writer, &buffer.settings, &buffer.functions);

    // reset for the next test
    (buffer.functions.buffer_clear_safe)();

    tests::test_complex_buffer_overflow_fails(
        buffer.writer,
        &buffer.settings,
        &buffer.functions,
        0x34852052,
    );
}

/// Test the buffer code
#[allow(unused_imports)]
pub mod tests {
    use crate::{
        buffer::{BufferFunctions, BufferSettings},
        tests::write_test_result,
    };
    use core::{arch::asm, fmt::Write};

    /// Test that writing a buffer clears it
    pub fn test_clear_works(
        writer: &mut dyn Write,
        settings: &BufferSettings,
        functions: &BufferFunctions,
    ) {
        let word: u32 = 0x23681068;
        write!(writer, "Testing clearing a buffer works\r\n").unwrap();
        let res = (functions.buffer_write_word_safe)(word);
        assert!(res.is_ok());

        // clear the buffer
        (functions.buffer_clear_safe)();

        // Test that trying to read the word written to the buffer
        // fails after clearing
        let res = (functions.buffer_read_word_safe)();
        crate::tests::write_test_result(writer, res.is_err(), "read after clear should fail");
        assert!(res.is_err());

        // Test that the full buffer space is available after clearing
        let buffer_size = settings.buffer_size / 4;
        write!(writer, "buffer_size {}\r\n", buffer_size).unwrap();

        let mut cnt = 0;
        for i in 0..buffer_size - 1 {
            cnt += 1;
            let res = (functions.buffer_write_word_safe)(i as u32);
            assert!(res.is_ok());
        }
        write_test_result(
            writer,
            cnt == buffer_size - 1,
            "all buffer space should be available after clear",
        );

        let res = (functions.buffer_write_word_safe)(0x69238632);

        write_test_result(
            writer,
            res.is_err(),
            "write beyond buffer capacity should fail",
        );

        assert!(res.is_err());
    }

    /// Test that writing a word works
    pub fn test_write_word(writer: &mut dyn Write, functions: &BufferFunctions, word: u32) {
        let res = (functions.buffer_write_word_safe)(word);

        write!(writer, "Testing write word works\r\n").unwrap();
        assert!(res.is_ok());
    }

    /// Test that reading a word works
    pub fn test_read_word(writer: &mut dyn Write, functions: &BufferFunctions, word: u32) {
        let res = (functions.buffer_write_word_safe)(word);

        write!(writer, "Testing write word works\r\n").unwrap();
        assert!(res.is_ok());

        write!(writer, "Testing read word works\r\n").unwrap();

        let res = (functions.buffer_read_word_safe)();
        assert!(res.is_ok());
        match res {
            Ok(val) => {
                assert_eq!(val, word);
            }
            Err(_) => {
                panic!();
            }
        }
    }

    /// Test that reading two word works
    pub fn test_read_two_words_works(
        writer: &mut dyn Write,
        functions: &BufferFunctions,
        word1: u32,
        word2: u32,
    ) {
        write!(writer, "Testing write word one works\r\n").unwrap();
        let res = (functions.buffer_write_word_safe)(word1);
        assert!(res.is_ok());

        write!(writer, "Testing write word two works\r\n").unwrap();
        let res = (functions.buffer_write_word_safe)(word2);
        assert!(res.is_ok());

        write!(writer, "Testing read word one works\r\n").unwrap();

        let res = (functions.buffer_read_word_safe)();
        assert!(res.is_ok());
        match res {
            Ok(val) => {
                assert_eq!(val, word1);
            }
            Err(_) => {
                panic!();
            }
        }

        write!(writer, "Testing read word two works\r\n").unwrap();

        let res = (functions.buffer_read_word_safe)();
        assert!(res.is_ok());
        match res {
            Ok(val) => {
                assert_eq!(val, word2);
            }
            Err(_) => {
                panic!();
            }
        }
    }

    /// Test that reading more words than available fails
    pub fn test_read_word_empty_fails(
        writer: &mut dyn Write,
        functions: &BufferFunctions,
        word: u32,
    ) {
        let res = (functions.buffer_write_word_safe)(word);

        write!(writer, "Testing write word works\r\n").unwrap();
        assert!(res.is_ok());

        write!(writer, "Testing read word works\r\n").unwrap();

        let res = (functions.buffer_read_word_safe)();
        assert!(res.is_ok());
        match res {
            Ok(val) => {
                assert_eq!(val, word);
            }
            Err(_) => {
                panic!();
            }
        }

        write!(writer, "Testing read second word when empty fails\r\n").unwrap();
        let res = (functions.buffer_read_word_safe)();
        assert!(res.is_err());
    }

    /// Test writing and reading a word after a read failure
    pub fn test_write_and_read_after_read_fail_works(
        writer: &mut dyn Write,
        functions: &BufferFunctions,
        word: u32,
    ) {
        let res = (functions.buffer_write_word_safe)(word);

        write!(writer, "Testing write word works\r\n").unwrap();
        assert!(res.is_ok());

        write!(writer, "Testing read word works\r\n").unwrap();

        let res = (functions.buffer_read_word_safe)();
        assert!(res.is_ok());
        match res {
            Ok(val) => {
                assert_eq!(val, word);
            }
            Err(_) => {
                panic!();
            }
        }

        write!(writer, "Testing read second word when empty fails\r\n").unwrap();
        let res = (functions.buffer_read_word_safe)();
        assert!(res.is_err());

        let res = (functions.buffer_write_word_safe)(word);

        write!(writer, "Testing write word works\r\n").unwrap();
        assert!(res.is_ok());

        write!(writer, "Testing read second word when empty fails\r\n").unwrap();
        let res = (functions.buffer_read_word_safe)();
        assert!(res.is_ok());
        match res {
            Ok(val) => {
                assert_eq!(val, word);
            }
            Err(_) => {
                panic!();
            }
        }
    }

    /// Test that writing more than the buffer size fails
    pub fn test_simple_buffer_overflow_fails(
        writer: &mut dyn Write,
        settings: &BufferSettings,
        functions: &BufferFunctions,
    ) {
        write!(
            writer,
            "Testing that writing more data than the buffer holds fails\r\n"
        )
        .unwrap();

        let buffer_size = settings.buffer_size as u32;

        // We should parameterize this
        let buffer_size = buffer_size / 4;
        write!(writer, "buffer_size {}\r\n", buffer_size).unwrap();

        for i in 0..buffer_size - 1 {
            let res = (functions.buffer_write_word_safe)(i);
            assert!(res.is_ok());
        }

        write!(writer, "last write should fail\r\n").unwrap();
        let res = (functions.buffer_write_word_safe)(0x69238632);
        assert!(res.is_err());
    }

    /// Test that writing more than the buffer size fails
    /// This is a complicated test where we write and read a word
    /// first, then fill the buffer
    pub fn test_complex_buffer_overflow_fails(
        writer: &mut dyn Write,
        settings: &BufferSettings,
        functions: &BufferFunctions,
        word: u32,
    ) {
        write!(writer, "Testing that writing more data than the buffer holds fails with initial write and read\r\n").unwrap();

        let buffer_size = settings.buffer_size as u32;

        let res = (functions.buffer_write_word_safe)(word);

        write!(writer, "Testing write word works\r\n").unwrap();
        assert!(res.is_ok());

        write!(writer, "Testing read word works\r\n").unwrap();

        let res = (functions.buffer_read_word_safe)();
        assert!(res.is_ok());
        match res {
            Ok(val) => {
                assert_eq!(val, word);
            }
            Err(_) => {
                panic!();
            }
        }

        let buffer_size = buffer_size / 4;
        write!(writer, "buffer_size {}\r\n", buffer_size).unwrap();

        for i in 0..buffer_size - 1 {
            let res = (functions.buffer_write_word_safe)(i);
            assert!(res.is_ok());
        }

        write!(writer, "last write should fail\r\n").unwrap();
        let res = (functions.buffer_write_word_safe)(0x69238632);
        assert!(res.is_err());
    }
}
