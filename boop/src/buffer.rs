//! Rust wrapper and test code for the buffer / queue
//! The basic size of the data type for this queue is 32-bits
//! This may be added as a type-parameter in an another version
//! Right now, simplicity is the goal.  This means simple integration
//! with serial input and output and UTF-32 representation.
#![warn(missing_docs)]

use core::{fmt::Write, marker::PhantomData};

use crate::ArrayHandle;

/// The basic operations for a queue
pub trait Queue {
    /// Initialize the queue
    ///
    /// # Safety
    ///
    /// The caller is responsible for making sure buffer_start and
    /// buffer_end are valid pointers into a contiguous memory region.
    /// In addition, buffer_start should be less than or equal to
    /// buffer_end.
    /// That memory region should last for the duration of the Queue.
    /// This function is primarily used for testing purposes.  Most
    /// users should use the new function.
    ///
    /// # Examples
    /// ```
    /// use crate::{
    ///     buffer::{Buffer, BufferFunctions, BufferStruct, Error, Queue},
    ///     ArrayHandle,
    /// };
    /// use core::marker::PhantomData;
    ///
    /// let mut array: [u32; 4] = [0; 4];
    /// let handle = ArrayHandle::new(array.as_mut_ptr(), array.len());
    ///
    /// // This is a mock BufferFunctions structure
    /// // In a real program you would use the real FFI interface
    /// // Look at the corresponding implementation crate for Cortex-M
    /// // devices in this project
    /// let bf = BufferFunctions {
    ///     buffer_init_safe: |_buffer: *const BufferStruct,
    ///                        _buffer_start: *const u32,
    ///                        _buffer_end: *const u32|
    ///      -> core::result::Result<(), Error> { Ok(()) },
    ///     buffer_clear_safe: |_buffer: *const BufferStruct| -> core::result::Result<(), Error> {
    ///         Ok(())
    ///     },
    ///     buffer_write_word_safe: |_buffer: *const BufferStruct,
    ///                              _value: u32|
    ///      -> core::result::Result<(), Error> { Ok(()) },
    ///     buffer_read_word_safe:
    ///         |_buffer: *const BufferStruct| -> core::result::Result<u32, Error> { Ok(0xA0) },
    /// };
    ///
    /// let buffer_addr = handle.ptr as *mut u32;
    ///
    /// let buffer_len = handle.len;
    /// let buffer_size = buffer_len * core::mem::size_of::<u32>();
    /// let buffer_end: *const u32 =
    ///     (buffer_addr as usize + buffer_size - core::mem::size_of::<u32>()) as *const u32;
    ///
    /// let mut buffer = Buffer {
    ///     handle: &handle,
    ///     buffer: BufferStruct {
    ///         start: core::ptr::null::<u32>() as *const u32,
    ///         end: core::ptr::null::<u32>() as *const u32,
    ///         currkey: core::ptr::null_mut::<u32>() as *mut u32,
    ///         bufftop: core::ptr::null_mut::<u32>() as *mut u32,
    ///         _marker: PhantomData,
    ///     },
    ///     functions: bf,
    /// };
    ///
    /// let res = unsafe { buffer.init(buffer_addr, buffer_end) };
    ///
    /// assert!(res.is_ok());
    /// ```
    unsafe fn init(
        &mut self,
        buffer_start: *const u32,
        buffer_end: *const u32,
    ) -> core::result::Result<(), Error>;

    /// Clear the queue
    ///
    /// # Examples
    ///
    /// ```
    /// use crate::{
    ///     ArrayHandle,
    ///     buffer::{Error, Buffer, BufferFunctions, BufferStruct, Queue}
    /// };
    ///
    /// let mut array: [u32; 4] = [0; 4];
    /// let handle = ArrayHandle::new(array.as_mut_ptr(), array.len());
    ///
    /// // This is a mock BufferFunctions structure
    /// // In a real program you would use the real FFI interface
    /// // Look at the corresponding implementation crate for Cortex-M
    /// // devices in this project
    /// let bf = BufferFunctions {
    ///     buffer_init_safe: |_buffer: *const BufferStruct,
    ///     _buffer_start: *const u32,
    ///     _buffer_end: *const u32|
    ///  -> core::result::Result<(), Error> { Ok(()) },
    ///     buffer_clear_safe: |_buffer: *const BufferStruct| -> core::result::Result<(), Error> {
    ///         Ok(())
    ///     },
    ///     buffer_write_word_safe: |_buffer: *const BufferStruct, _value: u32| -> core::result::Result<(), Error> { Ok(()) },
    ///     buffer_read_word_safe:
    ///     |_buffer: *const BufferStruct| -> core::result::Result<u32, Error> { Ok(0xA0) },
    /// };
    ///
    /// let mut buffer = Buffer::new(&handle, bf).unwrap();
    /// let res = buffer.clear();
    /// assert!(res.is_ok());
    /// ```
    fn clear(&mut self) -> core::result::Result<(), Error>;

    /// Get an item from the queue
    ///
    /// # Examples
    ///
    /// ```
    /// use crate::{
    ///     ArrayHandle,
    ///     buffer::{Error, Buffer, BufferFunctions, BufferStruct, Queue}
    /// };
    ///
    /// let mut array: [u32; 4] = [0; 4];
    /// let handle = ArrayHandle::new(array.as_mut_ptr(), array.len());
    ///
    /// // This is a mock BufferFunctions structure
    /// // In a real program you would use the real FFI interface
    /// // Look at the corresponding implementation crate for Cortex-M
    /// // devices in this project
    /// let bf = BufferFunctions {
    ///     buffer_init_safe: |_buffer: *const BufferStruct,
    ///     _buffer_start: *const u32,
    ///     _buffer_end: *const u32|
    ///  -> core::result::Result<(), Error> { Ok(()) },
    ///     buffer_clear_safe: |_buffer: *const BufferStruct| -> core::result::Result<(), Error> {
    ///         Ok(())
    ///     },
    ///     buffer_write_word_safe: |_buffer: *const BufferStruct, _value: u32| -> core::result::Result<(), Error> { Ok(()) },
    ///     buffer_read_word_safe:
    ///     |_buffer: *const BufferStruct| -> core::result::Result<u32, Error> { Ok(0xA0) },
    /// };
    ///
    /// let mut buffer = Buffer::new(&handle, bf).unwrap();
    /// buffer.put(0xA0).unwrap();
    /// let res = buffer.get();
    /// assert!(res.is_ok());
    /// assert_eq!(res.expect("Get an Ok result"), 0xA0);
    /// ```
    fn get(&mut self) -> core::result::Result<u32, Error>;

    /// Put an item into the queue
    ///
    /// # Examples
    ///
    /// ```
    /// use crate::{
    ///     ArrayHandle,
    ///     buffer::{Error, Buffer, BufferFunctions, BufferStruct, Queue}
    /// };
    ///
    /// let mut array: [u32; 4] = [0; 4];
    /// let handle = ArrayHandle::new(array.as_mut_ptr(), array.len());
    ///
    /// // This is a mock BufferFunctions structure
    /// // In a real program you would use the real FFI interface
    /// // Look at the corresponding implementation crate for Cortex-M
    /// // devices in this project
    /// let bf = BufferFunctions {
    ///     buffer_init_safe: |_buffer: *const BufferStruct,
    ///     _buffer_start: *const u32,
    ///     _buffer_end: *const u32|
    ///  -> core::result::Result<(), Error> { Ok(()) },
    ///     buffer_clear_safe: |_buffer: *const BufferStruct| -> core::result::Result<(), Error> {
    ///         Ok(())
    ///     },
    ///     buffer_write_word_safe: |_buffer: *const BufferStruct, _value: u32| -> core::result::Result<(), Error> { Ok(()) },
    ///     buffer_read_word_safe:
    ///     |_buffer: *const BufferStruct| -> core::result::Result<u32, Error> { Ok(0xA0) },
    /// };
    ///
    /// let mut buffer = Buffer::new(&handle, bf).unwrap();
    /// let res = buffer.put(0xA0);
    /// assert!(res.is_ok());
    /// ```
    fn put(&mut self, item: u32) -> core::result::Result<(), Error>;
}

/// This is the basic structure for a buffer object used by the
/// underlying FFI code.
///
/// currkey and bufftop point to data that can be mutated and are used
/// to mutate data.  start and end aren't dereferenced to mutate data.
/// But start IS copied to currkey, and currkey is then used to mutate
/// data.  The semantics of that cast aren't quite clear, and maybe
/// they should all be mut pointers.
#[repr(C)]
pub struct BufferStruct<'a> {
    /// The start of the buffer
    pub start: *const u32,
    /// The end of the buffer
    pub end: *const u32,
    /// The current key
    pub currkey: *mut u32,
    /// The current top
    pub bufftop: *mut u32,
    /// Marker so we can link lifetimes to the backing array
    _marker: PhantomData<&'a u32>,
}

/// Debug formatter implementation for BufferStruct
impl Debug for BufferStruct<'_> {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "  currkey: 0x{:02X}", &(self.currkey as usize))?;
        write!(f, ", bufftop: 0x{:02X}", &(self.bufftop as usize))?;
        write!(f, ", currkey item: 0x{:02X}", &(unsafe { *(self.currkey) }))?;
        write!(f, ", start: 0x{:02X}", &(self.start as usize))?;
        write!(f, ", end: 0x{:02X}", &(self.end as usize))
    }
}

/// Collection of functions for the buffer data structure
/// This is primarily used to make testing easier
#[derive(Clone, Copy)]
pub struct BufferFunctions {
    /// The initialization function for the buffer
    pub buffer_init_safe:
        unsafe fn(*const BufferStruct, *const u32, *const u32) -> core::result::Result<(), Error>,
    /// The clear or reset function for the buffer
    pub buffer_clear_safe: unsafe fn(*const BufferStruct) -> core::result::Result<(), Error>,
    /// The enqueue function for the buffer
    pub buffer_write_word_safe:
        unsafe fn(*const BufferStruct, u32) -> core::result::Result<(), Error>,
    /// The dequeue function for the buffer
    pub buffer_read_word_safe: unsafe fn(*const BufferStruct) -> core::result::Result<u32, Error>,
}

/// Circular buffer array-based implementation for a queue
#[allow(dead_code)]
pub struct Buffer<'a> {
    /// The handle to the buffer array
    pub handle: &'a ArrayHandle<'a, u32>,
    /// The native buffer pointers
    pub buffer: BufferStruct<'a>,
    /// Functions for the buffer
    pub functions: BufferFunctions,
}

/// Debug formatter implementation for Buffer
impl<'a> Debug for Buffer<'a> {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "{:?}", self.buffer)
    }
}

/// Create a testing structure to run tests against a buffer
pub struct BufferTester<'a> {
    /// The Buffer structure to test
    pub buffer: Buffer<'a>,
    /// The output stream for writing debug and log messages
    pub writer: &'a mut dyn Write,
}

impl<'a> Buffer<'a> {
    /// Create a new buffer
    ///
    /// This takes ownership of the array, which is owned by the new
    /// Buffer structure.
    ///
    /// A reference is used here for the array, but a pointer may be
    /// more appropriate.  Previous versions copied the data on the
    /// stack, but that led to undefined behavior because pointers
    /// weren't updated properly.
    ///
    /// Some of the Rust books like Learning Rust With Entirely Too
    /// Many Linked Lists and The Rustonomicon can help in the next
    /// refactor.  Getting this working with linker sections may
    /// require a custom allocator: The Embedded Rust Book has some
    /// info on that.
    pub fn new(
        handle: &'a ArrayHandle<'a, u32>,
        functions: BufferFunctions,
    ) -> core::result::Result<Buffer<'a>, Error> {
        // Get the buffer address
        // let buffer_addr = core::ptr::addr_of_mut!(array) as *const [u32; 256];
        let buffer_addr = handle.ptr as *mut u32;

        // len() returns the length in terms of number of elements,
        // not the size
        let buffer_len = handle.len;
        let buffer_size = buffer_len * core::mem::size_of::<u32>();
        let buffer_end: *const u32 =
            (buffer_addr as usize + buffer_size - core::mem::size_of::<u32>()) as *const u32;

        let mut buffer = Buffer {
            handle,
            buffer: BufferStruct {
                start: core::ptr::null::<u32>() as *const u32,
                end: core::ptr::null::<u32>() as *const u32,
                currkey: core::ptr::null_mut::<u32>() as *mut u32,
                bufftop: core::ptr::null_mut::<u32>() as *mut u32,
                _marker: PhantomData,
            },
            functions,
        };

        let res = unsafe { buffer.init(buffer_addr, buffer_end) };

        match res {
            Ok(()) => Ok(buffer),
            Err(e) => Err(e),
        }
    }
}

impl<'a> Queue for Buffer<'a> {
    /// Initialize the buffer
    unsafe fn init(
        &mut self,
        buffer_start: *const u32,
        buffer_end: *const u32,
    ) -> core::result::Result<(), Error> {
        let addr = core::ptr::addr_of!(self.buffer) as *const BufferStruct;
        unsafe { (self.functions.buffer_init_safe)(addr, buffer_start, buffer_end) }
    }

    /// Clear the buffer
    fn clear(&mut self) -> core::result::Result<(), Error> {
        let addr = core::ptr::addr_of!(self.buffer) as *const BufferStruct;
        unsafe { (self.functions.buffer_clear_safe)(addr) }
    }

    /// Get an item from the queue
    fn get(&mut self) -> core::result::Result<u32, Error> {
        let addr = core::ptr::addr_of!(self.buffer) as *const BufferStruct;
        unsafe { (self.functions.buffer_read_word_safe)(addr) }
    }

    /// Put an item into the queue
    fn put(&mut self, item: u32) -> core::result::Result<(), Error> {
        let addr = core::ptr::addr_of!(self.buffer) as *const BufferStruct;
        unsafe { (self.functions.buffer_write_word_safe)(addr, item) }
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
    /// Null pointer error
    NullPointer,
    /// An unknown error type
    Unknown,
}

impl Display for ErrorKind {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match self {
            ErrorKind::Empty => write!(f, "Dequeue called on an empty queue"),
            ErrorKind::Full => write!(f, "Enqueue called on a full queue"),
            ErrorKind::NullPointer => write!(f, "Null pointer error"),
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
pub fn run_tests(buffer_tester: &mut BufferTester) {
    let buffer_addr = buffer_tester.buffer.handle.ptr as *mut u32;
    let buffer_size = buffer_tester.buffer.handle.len * core::mem::size_of::<u32>();
    let buffer_end: *const u32 =
        (buffer_addr as usize + buffer_size - core::mem::size_of::<u32>()) as *const u32;

    unsafe {
        buffer_tester.buffer.init(buffer_addr, buffer_end).unwrap();
    }

    tests::test_write_word(buffer_tester, 0x34852052);

    // reset for the next test
    unsafe {
        buffer_tester.buffer.init(buffer_addr, buffer_end).unwrap();
    }

    // Test the clear function
    tests::test_clear_works(buffer_tester);

    // reset for the next test
    buffer_tester.buffer.clear().unwrap();

    tests::test_read_word(buffer_tester, 0x34852052);

    // reset for the next test
    buffer_tester.buffer.clear().unwrap();

    // test that writing two words and reading two words works
    tests::test_read_two_words_works(buffer_tester, 0x34852052, 0x58602760);

    // reset for the next test
    buffer_tester.buffer.clear().unwrap();

    tests::test_read_word_empty_fails(buffer_tester, 0x34852052);

    // reset for the next test
    buffer_tester.buffer.clear().unwrap();

    tests::test_write_and_read_after_read_fail_works(buffer_tester, 0x34852052);

    // reset for the next test
    buffer_tester.buffer.clear().unwrap();

    tests::test_simple_buffer_overflow_fails(buffer_tester);

    tests::test_queue_head_wraps_works(buffer_tester.writer, buffer_tester.buffer.functions);
    tests::test_queue_head_wraps_nonfilled_works(
        buffer_tester.writer,
        buffer_tester.buffer.functions,
    );
    tests::test_queue_head_wraps_nonemptied_works(
        buffer_tester.writer,
        buffer_tester.buffer.functions,
    );
    tests::test_queue_last_head_update(buffer_tester.writer, buffer_tester.buffer.functions);

    // reset for the next test
    buffer_tester.buffer.clear().unwrap();

    tests::test_complex_buffer_overflow_fails(buffer_tester, 0x34852052);

    tests::test_init_null_pointer_fails(buffer_tester);
    tests::test_get_null_pointer_fails(buffer_tester);
    tests::test_put_null_pointer_fails(buffer_tester);
    tests::test_clear_null_pointer_fails(buffer_tester);
}

/// Test the buffer code
#[allow(unused_imports)]
pub mod tests {
    use crate::{
        buffer::{Buffer, BufferFunctions, BufferStruct, BufferTester, Error, ErrorKind, Queue},
        tests::write_test_result,
        ArrayHandle,
    };
    use core::{arch::asm, fmt::Write};

    /// Test that writing a buffer clears it
    pub fn test_clear_works(buffer_tester: &mut BufferTester) {
        let word: u32 = 0x23681068;
        let res = buffer_tester.buffer.put(word);
        assert!(res.is_ok());

        // clear the buffer
        let res = buffer_tester.buffer.clear();
        write_test_result(
            buffer_tester.writer,
            res.is_ok(),
            "test_clear_works clear should work",
        );

        // Test that trying to read the word written to the buffer
        // fails after clearing
        let res = buffer_tester.buffer.get();
        crate::tests::write_test_result(
            buffer_tester.writer,
            res.is_err(),
            "test_clear_works read after clear should fail",
        );
        assert!(res.is_err());

        // Test that the full buffer space is available after clearing
        let buffer_len = buffer_tester.buffer.handle.len;

        let mut cnt = 0;
        for i in 0..buffer_len - 1 {
            cnt += 1;
            let res = buffer_tester.buffer.put(i as u32);
            assert!(res.is_ok());
        }
        write_test_result(
            buffer_tester.writer,
            cnt == buffer_len - 1,
            "test_clear_works all buffer space should be available after clear",
        );

        let res = buffer_tester.buffer.put(0x69238632);

        write_test_result(
            buffer_tester.writer,
            res.is_err(),
            "test_clear_works write beyond buffer capacity should fail",
        );

        assert!(res.is_err());
    }

    /// Test that writing a word works
    pub fn test_write_word(buffer_tester: &mut BufferTester, word: u32) {
        let res = buffer_tester.buffer.put(word);

        write_test_result(
            buffer_tester.writer,
            res.is_ok(),
            "test_write_word put should work",
        );
        assert!(res.is_ok());
    }

    /// Test that reading a word works
    pub fn test_read_word(buffer_tester: &mut BufferTester, word: u32) {
        let res = buffer_tester.buffer.put(word);
        write_test_result(
            buffer_tester.writer,
            res.is_ok(),
            "test_read_word put should work",
        );

        assert!(res.is_ok());

        let res = buffer_tester.buffer.get();
        write_test_result(
            buffer_tester.writer,
            res.is_ok(),
            "test_read_word get should work",
        );
        match res {
            Ok(val) => {
                write_test_result(
                    buffer_tester.writer,
                    val == word,
                    "test_read_word get should equal put word",
                );
                assert_eq!(val, word);
            }
            Err(_) => {
                write_test_result(
                    buffer_tester.writer,
                    false,
                    "test_read_word get should equal put word",
                );
                panic!();
            }
        }
    }

    /// Test that reading two word works
    pub fn test_read_two_words_works(buffer_tester: &mut BufferTester, word1: u32, word2: u32) {
        let res = buffer_tester.buffer.put(word1);
        write_test_result(
            buffer_tester.writer,
            res.is_ok(),
            "test_read_two_words first put should work",
        );
        assert!(res.is_ok());

        let res = buffer_tester.buffer.put(word2);
        write_test_result(
            buffer_tester.writer,
            res.is_ok(),
            "test_read_two_words second put should work",
        );
        assert!(res.is_ok());

        let res = buffer_tester.buffer.get();
        write_test_result(
            buffer_tester.writer,
            res.is_ok(),
            "test_read_two_words first get should work",
        );
        assert!(res.is_ok());
        match res {
            Ok(val) => {
                write_test_result(
                    buffer_tester.writer,
                    val == word1,
                    "test_read_two_words first get should equal put word",
                );
                assert_eq!(val, word1);
            }
            Err(_) => {
                write_test_result(
                    buffer_tester.writer,
                    false,
                    "test_read_two_words first get should work",
                );
                panic!();
            }
        }

        let res = buffer_tester.buffer.get();
        write_test_result(
            buffer_tester.writer,
            res.is_ok(),
            "test_read_two_words second get should work",
        );
        assert!(res.is_ok());
        match res {
            Ok(val) => {
                write_test_result(
                    buffer_tester.writer,
                    val == word2,
                    "test_read_two_words second get should equal put word",
                );
                assert_eq!(val, word2);
            }
            Err(_) => {
                write_test_result(
                    buffer_tester.writer,
                    false,
                    "test_read_two_words second get should work",
                );
                panic!();
            }
        }
    }

    /// Test that reading more words than available fails
    pub fn test_read_word_empty_fails(buffer_tester: &mut BufferTester, word: u32) {
        let res = buffer_tester.buffer.put(word);
        write_test_result(
            buffer_tester.writer,
            res.is_ok(),
            "test_read_word_empty_fails put should work",
        );
        assert!(res.is_ok());

        let res = buffer_tester.buffer.get();
        write_test_result(
            buffer_tester.writer,
            res.is_ok(),
            "test_read_word_empty_fails get should work",
        );
        assert!(res.is_ok());
        match res {
            Ok(val) => {
                write_test_result(
                    buffer_tester.writer,
                    val == word,
                    "test_read_word_empty_fails get should equal put word",
                );
                assert_eq!(val, word);
            }
            Err(_) => {
                write_test_result(
                    buffer_tester.writer,
                    false,
                    "test_read_word_empty_fails get should equal put word",
                );
                panic!();
            }
        }

        let res = buffer_tester.buffer.get();
        write_test_result(
            buffer_tester.writer,
            res.is_err(),
            "test_read_word_empty_fails get from empty should fail",
        );
        assert!(res.is_err());
        match res {
            Ok(_) => {
                write_test_result(
                    buffer_tester.writer,
                    false,
                    "test_read_word_empty_fails get from empty should be empty error",
                );
                panic!();
            }
            Err(e) => {
                write_test_result(
                    buffer_tester.writer,
                    e == Error::new(ErrorKind::Empty),
                    "test_read_word_empty_fails get from empty should be empty error",
                );
                assert_eq!(e, Error::new(ErrorKind::Empty));
            }
        }
    }

    /// Test writing and reading a word after a read failure
    pub fn test_write_and_read_after_read_fail_works(buffer_tester: &mut BufferTester, word: u32) {
        let res = buffer_tester.buffer.put(word);
        write_test_result(
            buffer_tester.writer,
            res.is_ok(),
            "test_write_and_read_word_fail_works put should work",
        );
        assert!(res.is_ok());

        let res = buffer_tester.buffer.get();
        write_test_result(
            buffer_tester.writer,
            res.is_ok(),
            "test_write_and_read_word_fail_works get should work",
        );
        assert!(res.is_ok());
        match res {
            Ok(val) => {
                write_test_result(
                    buffer_tester.writer,
                    val == word,
                    "test_write_and_read_word_fail_works get should equal put word",
                );
                assert_eq!(val, word);
            }
            Err(_) => {
                write_test_result(
                    buffer_tester.writer,
                    false,
                    "test_write_and_read_word_fail_works get should equal put word",
                );
                panic!();
            }
        }

        let res = buffer_tester.buffer.get();
        write_test_result(
            buffer_tester.writer,
            res.is_err(),
            "test_write_and_read_word_fail_works get second word when empty should fail",
        );
        assert!(res.is_err());

        let res = buffer_tester.buffer.put(word);
        write_test_result(
            buffer_tester.writer,
            res.is_ok(),
            "test_write_and_read_word_fail_works second put should work",
        );
        assert!(res.is_ok());

        let res = buffer_tester.buffer.get();
        write_test_result(
            buffer_tester.writer,
            res.is_ok(),
            "test_write_and_read_word_fail_works second get should work",
        );
        assert!(res.is_ok());
        match res {
            Ok(val) => {
                write_test_result(
                    buffer_tester.writer,
                    val == word,
                    "test_write_and_read_word_fail_works second get should equal put word",
                );
                assert_eq!(val, word);
            }
            Err(_) => {
                write_test_result(
                    buffer_tester.writer,
                    false,
                    "test_write_and_read_word_fail_works second get should equal put word",
                );
                panic!();
            }
        }
    }

    /// Test that writing more than the buffer size fails
    pub fn test_simple_buffer_overflow_fails(buffer_tester: &mut BufferTester) {
        let buffer_len = buffer_tester.buffer.handle.len as u32;

        for i in 0..buffer_len - 1 {
            let res = buffer_tester.buffer.put(i);
            assert!(res.is_ok());
        }

        let res = buffer_tester.buffer.put(0x69238632);
        write_test_result(
            buffer_tester.writer,
            res.is_err(),
            "test_simple_buffer_overflow_fails last write should fail",
        );
        assert!(res.is_err());
        match res {
            Ok(_) => {
                write_test_result(
                    buffer_tester.writer,
                    false,
                    "test_simple_buffer_overflow_fails last write should fail with full error",
                );
                panic!();
            }
            Err(e) => {
                write_test_result(
                    buffer_tester.writer,
                    e == Error::new(ErrorKind::Full),
                    "test_simple_buffer_overflow_fails last write should fail with full error",
                );
                assert_eq!(e, Error::new(ErrorKind::Full));
            }
        }
    }

    /// Test that writing more than the buffer size fails
    /// This is a complicated test where we write and read a word
    /// first, then fill the buffer
    pub fn test_complex_buffer_overflow_fails(buffer_tester: &mut BufferTester, word: u32) {
        let buffer_len = buffer_tester.buffer.handle.len as u32;

        let res = buffer_tester.buffer.put(word);
        write_test_result(
            buffer_tester.writer,
            res.is_ok(),
            "test_complex_buffer_overflow_fails first put should work",
        );
        assert!(res.is_ok());

        let res = buffer_tester.buffer.get();
        write_test_result(
            buffer_tester.writer,
            res.is_ok(),
            "test_complex_buffer_overflow_fails first get should work",
        );
        assert!(res.is_ok());
        match res {
            Ok(val) => {
                assert_eq!(val, word);
            }
            Err(_) => {
                panic!();
            }
        }

        for i in 0..buffer_len - 1 {
            let res = buffer_tester.buffer.put(i);
            assert!(res.is_ok());
        }

        let res = buffer_tester.buffer.put(0x69238632);
        write_test_result(
            buffer_tester.writer,
            res.is_err(),
            "test_complex_buffer_overflow_fails last write should fail",
        );
        assert!(res.is_err());
        match res {
            Ok(_) => {
                write_test_result(
                    buffer_tester.writer,
                    false,
                    "test_complex_buffer_overflow_fails last write should fail with full error",
                );
                panic!();
            }
            Err(e) => {
                write_test_result(
                    buffer_tester.writer,
                    e == Error::new(ErrorKind::Full),
                    "test_complex_buffer_overflow_fails last write should fail with full error",
                );
                assert_eq!(e, Error::new(ErrorKind::Full));
            }
        }
    }

    /// Test a case where the head wraps around
    pub fn test_queue_head_wraps_works(writer: &mut dyn Write, functions: BufferFunctions) {
        // Test with a smaller test case
        let mut array: [u32; 4] = [0; 4];
        let handle = ArrayHandle::new(array.as_mut_ptr(), array.len());

        let buffer = Buffer::new(&handle, functions).unwrap();

        let mut buffer_tester = BufferTester { buffer, writer };
        // Iterate through [1, 2, 3]
        for i in 1..4 {
            let res = buffer_tester.buffer.put((i % 256) as u32);
            write_test_result(
                buffer_tester.writer,
                res.is_ok(),
                "should be able to fill queue",
            );
        }

        for i in 1..4 {
            let res = buffer_tester.buffer.get();
            write_test_result(
                buffer_tester.writer,
                res.unwrap() == ((i % 256) as u32),
                "should be able to get filled values ",
            );
        }

        for i in 1..=2 {
            let res = buffer_tester.buffer.put(i as u32);
            write_test_result(
                buffer_tester.writer,
                res.is_ok(),
                "should be able to put in 2 more values",
            );
        }

        for i in 1..=2 {
            let res = buffer_tester.buffer.get();
            write_test_result(
                buffer_tester.writer,
                res.unwrap() == (i as u32),
                "should be able to get values",
            );
        }
    }

    /// Test a case where the head wraps around
    /// This tests a case where we don't fill the queue all the way,
    /// then read those values, then try to wrap
    pub fn test_queue_head_wraps_nonfilled_works(
        writer: &mut dyn Write,
        functions: BufferFunctions,
    ) {
        // Test with a smaller test case
        let mut array: [u32; 4] = [0; 4];
        let handle = ArrayHandle::new(array.as_mut_ptr(), array.len());

        let buffer = Buffer::new(&handle, functions).unwrap();

        let mut buffer_tester = BufferTester { buffer, writer };

        // This iterates through [1, 2]
        for i in 1..=2 {
            let res = buffer_tester.buffer.put((i % 256) as u32);
            write_test_result(
                buffer_tester.writer,
                res.is_ok(),
                "should be able to add two items",
            );
        }

        for i in 1..=2 {
            let res = buffer_tester.buffer.get();
            write_test_result(
                buffer_tester.writer,
                res.unwrap() == ((i % 256) as u32),
                "should be able to get two items",
            );
        }

        for i in 1..=2 {
            let res = buffer_tester.buffer.put(i as u32);
            write_test_result(
                buffer_tester.writer,
                res.is_ok(),
                "should be able to put in 2 more items",
            );
        }

        for i in 1..=2 {
            let res = buffer_tester.buffer.get();
            write_test_result(
                buffer_tester.writer,
                res.unwrap() == (i as u32),
                "should be able to get items",
            );
        }

        // The queue should now be empty
        let res = buffer_tester.buffer.get();
        write_test_result(
            buffer_tester.writer,
            res.is_err(),
            "get from empty queue should fail",
        );
    }

    /// Test where we wrap the tail and head with gets in between filling the queue
    /// Don't empty the queue all the way when getting values before the wrap
    pub fn test_queue_head_wraps_nonemptied_works(
        writer: &mut dyn Write,
        functions: BufferFunctions,
    ) {
        // Test with a smaller test case
        let mut array: [u32; 4] = [0; 4];
        let handle = ArrayHandle::new(array.as_mut_ptr(), array.len());

        let buffer = Buffer::new(&handle, functions).unwrap();

        let mut buffer_tester = BufferTester { buffer, writer };

        // put in 2 items [1, 2]
        for i in [1, 2] {
            let res = buffer_tester.buffer.put((i % 256) as u32);
            write_test_result(
                buffer_tester.writer,
                res.is_ok(),
                "should be able to put in 2 items",
            );
        }

        // Remove one item
        let res = buffer_tester.buffer.get();
        write_test_result(
            buffer_tester.writer,
            *res.as_ref().unwrap() == 1_u32,
            "should be able to get one item ",
        );

        // Put in 2 items, [3, 4]
        for i in [3, 4] {
            let res = buffer_tester.buffer.put(i as u32);
            write_test_result(
                buffer_tester.writer,
                res.is_ok(),
                "should be able to put in 2 more values",
            );
        }

        // Get three items
        for i in [2, 3, 4] {
            let res = buffer_tester.buffer.get();
            write_test_result(
                buffer_tester.writer,
                res.unwrap() == (i as u32),
                "should be able to get all remaining values",
            );
        }

        // The queue should now be empty
        let res = buffer_tester.buffer.get();
        write_test_result(
            buffer_tester.writer,
            res.is_err(),
            "get from empty queue should fail",
        );
    }

    /// Test a case where the last head wasn't being updated in the
    /// end-of-queue code path
    pub fn test_queue_last_head_update(writer: &mut dyn Write, functions: BufferFunctions) {
        // Test with a smaller test case
        let mut array: [u32; 4] = [0; 4];
        let handle = ArrayHandle::new(array.as_mut_ptr(), array.len());

        let buffer = Buffer::new(&handle, functions).unwrap();

        let mut buffer_tester = BufferTester { buffer, writer };

        // This iterates through [1, 2]
        for i in 1..=2 {
            let res = buffer_tester.buffer.put((i % 256) as u32);
            write_test_result(
                buffer_tester.writer,
                res.is_ok(),
                "should be able to add two items",
            );
        }

        for i in 1..=2 {
            let res = buffer_tester.buffer.get();
            write_test_result(
                buffer_tester.writer,
                res.unwrap() == ((i % 256) as u32),
                "should be able to get two items",
            );
        }

        for i in 1..=2 {
            let res = buffer_tester.buffer.put(i as u32);
            write_test_result(
                buffer_tester.writer,
                res.is_ok(),
                "should be able to put in 2 more values",
            );
        }

        for i in 1..=2 {
            let res = buffer_tester.buffer.get();
            write_test_result(
                buffer_tester.writer,
                res.unwrap() == (i as u32),
                "should be able to get values",
            );
        }

        // The queue should now be empty
        let res = buffer_tester.buffer.get();
        write_test_result(
            buffer_tester.writer,
            res.is_err(),
            "get from empty queue should fail",
        );

        for i in 1..=3 {
            let res = buffer_tester.buffer.put(i as u32);
            write_test_result(
                buffer_tester.writer,
                res.is_ok(),
                "should be able to put in 3 more values",
            );
        }
    }

    /// Calling init with a null pointer should fail
    pub fn test_init_null_pointer_fails(buffer_tester: &mut BufferTester) {
        let addr = core::ptr::null_mut() as *mut BufferStruct;
        let start_addr = core::ptr::null_mut() as *mut u32;
        let end_addr = core::ptr::null() as *const u32;
        let res = unsafe {
            (buffer_tester.buffer.functions.buffer_init_safe)(addr, start_addr, end_addr)
        };

        assert!(res.is_err());

        match res {
            Ok(_) => {
                write_test_result(
                    buffer_tester.writer,
                    false,
                    "test_init_null_pointer_fails should fail with correct error",
                );
                panic!();
            }
            Err(e) => {
                write_test_result(
                    buffer_tester.writer,
                    e == Error::new(ErrorKind::NullPointer),
                    "test_init_null_pointer_fails should fail with correct error",
                );
                assert_eq!(e, Error::new(ErrorKind::NullPointer));
            }
        }
    }

    /// Calling get with a null pointer should fail
    pub fn test_get_null_pointer_fails(buffer_tester: &mut BufferTester) {
        let addr = core::ptr::null_mut() as *mut BufferStruct;
        let res = unsafe { (buffer_tester.buffer.functions.buffer_read_word_safe)(addr) };

        assert!(res.is_err());
        match res {
            Ok(_) => {
                write_test_result(
                    buffer_tester.writer,
                    false,
                    "test_get_null_pointer_fails should fail with correct error",
                );
                panic!();
            }
            Err(e) => {
                write_test_result(
                    buffer_tester.writer,
                    e == Error::new(ErrorKind::NullPointer),
                    "test_get_null_pointer_fails should fail with correct error",
                );
                assert_eq!(e, Error::new(ErrorKind::NullPointer));
            }
        }
    }

    /// Calling put with a null pointer should fail
    pub fn test_put_null_pointer_fails(buffer_tester: &mut BufferTester) {
        let addr = core::ptr::null_mut() as *mut BufferStruct;
        let word: u32 = 0x23681068;

        let res = unsafe { (buffer_tester.buffer.functions.buffer_write_word_safe)(addr, word) };
        assert!(res.is_err());

        match res {
            Ok(_) => {
                write_test_result(
                    buffer_tester.writer,
                    false,
                    "test_put_null_pointer_fails should fail with correct error",
                );
                panic!();
            }
            Err(e) => {
                write_test_result(
                    buffer_tester.writer,
                    e == Error::new(ErrorKind::NullPointer),
                    "test_put_null_pointer_fails should fail with correct error",
                );
                assert_eq!(e, Error::new(ErrorKind::NullPointer));
            }
        }
    }

    /// Calling clear with a null pointer should fail
    pub fn test_clear_null_pointer_fails(buffer_tester: &mut BufferTester) {
        let addr = core::ptr::null_mut() as *mut BufferStruct;
        let res = unsafe { (buffer_tester.buffer.functions.buffer_clear_safe)(addr) };

        assert!(res.is_err());

        match res {
            Ok(_) => {
                write_test_result(
                    buffer_tester.writer,
                    false,
                    "test_clear_null_pointer_fails should fail with correct error",
                );
                panic!();
            }
            Err(e) => {
                write_test_result(
                    buffer_tester.writer,
                    e == Error::new(ErrorKind::NullPointer),
                    "test_clear_null_pointer_fails should fail with correct error",
                );
                assert_eq!(e, Error::new(ErrorKind::NullPointer));
            }
        }
    }
}
