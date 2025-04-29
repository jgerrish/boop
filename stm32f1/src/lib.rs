//! STM32F1xx implementations of Forth subroutines and tests.
//! The STM32F1xx is a Cortex-M3 device that is used as the basis for
//! QEMU testing.
#![warn(missing_docs)]
#![no_std]

use cortex_m::interrupt::Mutex;

use core::{cell::RefCell, marker::PhantomData};

use boop::{
    buffer::BufferStruct,
    dict::{Flag, Word},
    error::Error,
    stack::StackStruct,
    ArrayHandle,
};

// Create a new section for a FORTH stack.
// This memory will be managed by Rust, not assembly code.
// These variables are not modified directly in Rust, but the data is
// mutated by the assembly code.

// There should be some indication that this can be changed by the programs.
// a.  Something like volatile, or
// b.  Make it mutable, but add locking around it.
// c.  Keep it immutable, but make it private except for metadata.
// Section flags in the ELF header indicate that normal .bss is WA
// and .ram2bss is only A.  Bare ARM devices may not automatically incorporate
// memory protection, but further OS development might be more strict.
// Regardless, flags should represent the intent.

// There is a #[used] attribute outlined in RFC 2386 which handles
// cases related to this.

// Making the variable mutable in Rust marks it as W.

/// The Forth return stack
/// This needs to be mutable because we read and write to the data.
#[link_section = ".ram2bss"]
static mut FORTH_RETURN_STACK: [u32; 512] = [0; 512];

/// A handle to the forth return stack data
pub static mut FORTH_RETURN_STACK_HANDLE: Mutex<RefCell<Option<ArrayHandle<u32>>>> = unsafe {
    Mutex::new(RefCell::new(Some(ArrayHandle {
        ptr: FORTH_RETURN_STACK.as_ptr(),
        len: FORTH_RETURN_STACK.len(),
        _marker: PhantomData,
    })))
};

/// Allocate space for a buffer.
/// The buffer is implemented as a queue.
/// The number of items that can be stored is one less than the (data
/// type) size of the queue.
/// For example, if the size allocated below is 1024, and the data
/// type size is four bytes, then the number of items that can be
/// stored is 255, not 256.
/// This needs to be mutable because we read and write to the data.
#[link_section = ".ram2bss"]
static mut FORTH_BUFFER: [u32; 256] = [0; 256];

/// wrapper around the buffer
pub static mut FORTH_BUFFER_HANDLE: Mutex<RefCell<Option<ArrayHandle<u32>>>> = unsafe {
    Mutex::new(RefCell::new(Some(ArrayHandle {
        ptr: FORTH_BUFFER.as_ptr(),
        len: FORTH_BUFFER.len(),
        _marker: PhantomData,
    })))
};

/// temporary buffer to store UTF-32 encoded strings
/// This needs to be mutable because we read and write to the data.
#[link_section = ".ram2bss"]
pub static mut FORTH_WORD_TMP_BUFFER: [u32; 128] = [0; 128];

/// dictionary storage
/// This needs to be mutable because we read and write to the data.
#[link_section = ".ram2bss"]
pub static mut FORTH_DICTIONARY: [u8; 1024] = [0; 1024];

// The jjforth function and data structure FFI
//
// We use unsigned 32-bit integers for parameters and return values
//
// The data is treated as an opaque value which may contain Forth word
// pointers, data like keystrokes, error results or other data.
//
// Some Forth functions may operate on this data as if it was signed
// data, in particular the BRANCH word, which does pointer arithmetic
// with possibly negative offsets.  But casting signed data for
// storage will work as expected.
#[link(name = "jjforth-cortex-m3")]
extern "C" {
    // Stack functions
    /// Initialize the stack
    pub fn jjforth_stack_init(
        stack: *mut StackStruct,
        memory_start: *const u32,
        stack_len: u32,
    ) -> u32;

    /// Pop a value from the stack
    /// data is a pointer to a variable to contain the returned data
    /// The function returns a u32 value indicating the error code
    pub fn jjforth_stack_pop(stack: *const StackStruct, data: *mut u32) -> u32;

    /// Push a value onto the stack
    /// The value to be pushed should be in value
    /// It returns a u32 value indicating the error code
    pub fn jjforth_stack_push(stack: *const StackStruct, value: u32) -> u32;

    /// Initialize the Forth system
    pub fn jjforth_start();

    /// Get the stack bottom address
    pub fn jjforth_get_stack_bottom(stack: *const StackStruct, result: *mut u32) -> u32;

    /// Initialize the buffer
    pub fn jjforth_buffer_init(
        buffer: *const BufferStruct,
        buffer_start: *const u32,
        buffer_end: *const u32,
    ) -> u32;

    /// Clear / empty the buffer
    pub fn jjforth_buffer_clear(buffer: *const BufferStruct) -> u32;

    /// Write a single byte to the input buffer
    pub fn jjforth_buffer_write(buffer: *const BufferStruct, word: u32) -> u32;

    /// Read a single byte from the input buffer
    pub fn jjforth_buffer_read(buffer: *const BufferStruct, data: &mut u32) -> u32;

    // Dictionary functions

    /// Initialize the dictionary
    pub fn dict_init(dictionary_addr: *mut u32, dictionary_size: u32);

    /// Get the length of a word given a length / flag field
    pub fn dict_word_length(flag_field: *const u8) -> u8;

    /// Get the length of a word given a length / flag field
    pub fn dict_add_word(word: *const u32, length: u32, flags: u8) -> u32;

    /// Encode an ASCII character as UTF-32
    pub fn dict_encode_ascii_as_utf32(character: u32, result: *mut u32) -> u32;

    /// Encode an ASCII string as a UTF-32 string
    pub fn dict_encode_ascii_string_as_utf32(src: *const u8, length: u32, dst: *mut u32) -> u32;

    /// Find a word in the dictionary
    pub fn dict_find(word_pointer: *const u32, length: u32, result: *mut u32) -> *const u32;
}

// Wrapper functions for the assembly code
//
// These wrapper could be moved into the boop crate
// They would have to be re-architected as builders that yield functions.
// As they are now, it keeps most of the unsafe functionality constrained
// the architecture-dependent crates.

/// Initialize the stacks
///
/// The stack len is in terms of the element size.  For example, a
/// u32 stack that can hold 10 u32 values has a stack size of 40.
///
/// # Safety
///
/// This function must be called before any other stack functions are
/// called.  The caller is responsible for allocating and deallocating
/// the stack data.
#[allow(clippy::not_unsafe_ptr_arg_deref)]
pub fn stack_init_safe(
    stack: *mut StackStruct,
    memory_start: *const u32,
    stack_len: u32,
) -> Result<(), boop::stack::Error> {
    let res = unsafe { jjforth_stack_init(stack, memory_start, stack_len) };

    match res {
        0 => Ok(()),
        1 => Err(boop::stack::Error::new(boop::stack::ErrorKind::NullPointer)),
        2 => Err(boop::stack::Error::new(
            boop::stack::ErrorKind::InvalidArguments,
        )),
        _ => Err(boop::stack::Error::new(boop::stack::ErrorKind::Unknown)),
    }
}

/// Wrapper to pop a value from the stack.
/// This function pops a value from the stack and returns a Result
/// with the popped value.
/// It fails with a StackUnderflow error if there is no data available
/// on the stack.
///
/// # Safety
///
/// This function must be called after stack_init_safe has been called
#[allow(clippy::not_unsafe_ptr_arg_deref)]
pub fn stack_pop_safe(stack: *const StackStruct) -> Result<u32, boop::stack::Error> {
    let mut data: u32 = 0x79327523;
    let data_ptr = core::ptr::addr_of_mut!(data);

    let res = unsafe { jjforth_stack_pop(stack, data_ptr) };
    match res {
        0 => Ok(data),
        1 => Err(boop::stack::Error::new(boop::stack::ErrorKind::NullPointer)),
        2 => Err(boop::stack::Error::new(
            boop::stack::ErrorKind::StackUnderflow,
        )),
        _ => Err(boop::stack::Error::new(boop::stack::ErrorKind::Unknown)),
    }
}

/// Wrapper to push a value onto the stack
///
/// # Safety
///
/// This function must be called after stack_init_safe has been called
#[allow(clippy::not_unsafe_ptr_arg_deref)]
pub fn stack_push_safe(stack: *const StackStruct, value: u32) -> Result<(), boop::stack::Error> {
    let res = unsafe { jjforth_stack_push(stack, value) };

    match res {
        0 => Ok(()),
        1 => Err(boop::stack::Error::new(boop::stack::ErrorKind::NullPointer)),
        2 => Err(boop::stack::Error::new(
            boop::stack::ErrorKind::StackOverflow,
        )),
        _ => Err(boop::stack::Error::new(boop::stack::ErrorKind::Unknown)),
    }
}

/// Safe wrapper to the start subroutine
pub fn start_safe() {
    unsafe { jjforth_start() }
}

/// Safe wrapper to get the stack bottom address
///
/// # Safety
///
/// This function must be called after stack_init_safe has been called
#[allow(clippy::not_unsafe_ptr_arg_deref)]
pub fn get_stack_bottom_safe(stack: *const StackStruct) -> Result<u32, boop::stack::Error> {
    let mut result: u32 = 0x00;

    let result_ptr = core::ptr::addr_of_mut!(result);

    let res = unsafe { jjforth_get_stack_bottom(stack, result_ptr) };

    match res {
        0 => Ok(result),
        1 => Err(boop::stack::Error::new(boop::stack::ErrorKind::NullPointer)),
        _ => Err(boop::stack::Error::new(boop::stack::ErrorKind::Unknown)),
    }
}

/// Initialize the buffer
///
/// # Safety
///
/// The caller must ensure that the BufferStruct pointer points to a
/// valid BufferStruct structure.
/// The caller must ensure that buffer_start and buffer_end are valid
/// pointers to elements in an array and that buffer_end is greater
/// than or equal to buffer_start.
///
pub unsafe fn buffer_init_safe(
    buffer: *const BufferStruct,
    buffer_start: *const u32,
    buffer_end: *const u32,
) -> Result<(), boop::buffer::Error> {
    let res = unsafe { jjforth_buffer_init(buffer, buffer_start, buffer_end) };

    match res {
        0 => Ok(()),
        1 => Err(boop::buffer::Error::new(
            boop::buffer::ErrorKind::NullPointer,
        )),
        _ => Err(boop::buffer::Error::new(boop::buffer::ErrorKind::Unknown)),
    }
}

/// Clear / empty the buffer
///
/// # Safety
///
/// The caller must ensure that the BufferStruct pointer points to a
/// valid BufferStruct structure.
///
pub unsafe fn buffer_clear_safe(buffer: *const BufferStruct) -> Result<(), boop::buffer::Error> {
    let res = unsafe { jjforth_buffer_clear(buffer) };

    match res {
        0 => Ok(()),
        1 => Err(boop::buffer::Error::new(
            boop::buffer::ErrorKind::NullPointer,
        )),
        _ => Err(boop::buffer::Error::new(boop::buffer::ErrorKind::Unknown)),
    }
}

/// Write to the input buffer
///
/// # Safety
///
/// The caller must ensure that the BufferStruct pointer points to a
/// valid BufferStruct structure.
///
pub unsafe fn buffer_write_safe(
    buffer: *const BufferStruct,
    data: &[u32],
) -> Result<(), boop::buffer::Error> {
    for word in data {
        let res = unsafe { jjforth_buffer_write(buffer, *word) };
        match res {
            0 => continue,
            1 => {
                return Err(boop::buffer::Error::new(
                    boop::buffer::ErrorKind::NullPointer,
                ))
            }
            2 => return Err(boop::buffer::Error::new(boop::buffer::ErrorKind::Full)),
            _ => return Err(boop::buffer::Error::new(boop::buffer::ErrorKind::Unknown)),
        }
    }
    Ok(())
}

/// Write to the input buffer
///
/// # Safety
///
/// The caller must ensure that the BufferStruct pointer points to a
/// valid BufferStruct structure.
///
pub unsafe fn buffer_write_word_safe(
    buffer: *const BufferStruct,
    data: u32,
) -> Result<(), boop::buffer::Error> {
    let res = unsafe { jjforth_buffer_write(buffer, data) };
    match res {
        0 => Ok(()),
        1 => Err(boop::buffer::Error::new(
            boop::buffer::ErrorKind::NullPointer,
        )),
        2 => Err(boop::buffer::Error::new(boop::buffer::ErrorKind::Full)),
        _ => Err(boop::buffer::Error::new(boop::buffer::ErrorKind::Unknown)),
    }
}

/// Read from the input buffer
///
/// # Safety
///
/// The caller must ensure that the BufferStruct pointer points to a
/// valid BufferStruct structure.
///
pub unsafe fn buffer_read_word_safe(
    buffer: *const BufferStruct,
) -> Result<u32, boop::buffer::Error> {
    let mut error_code: u32 = 0;

    let res = unsafe { jjforth_buffer_read(buffer, &mut error_code) };
    match error_code {
        0 => Ok(res),
        1 => Err(boop::buffer::Error::new(
            boop::buffer::ErrorKind::NullPointer,
        )),
        2 => Err(boop::buffer::Error::new(boop::buffer::ErrorKind::Empty)),
        _ => Err(boop::buffer::Error::new(boop::buffer::ErrorKind::Unknown)),
    }
}

// Define a panic handler that uses semihosting to exit immediately,
// so that any panics cause qemu to quit instead of hang.
// #[panic_handler]
// fn panic(_: &core::panic::PanicInfo) -> ! {
//     loop {
//         cortex_m_semihosting::hprintln!("Panic!\n");
//         cortex_m_semihosting::debug::exit(cortex_m_semihosting::debug::EXIT_FAILURE);
//     }
// }

// fn main() {
// }

// Dictionary helpers for the boop library

/// Initialize the dictionary
///
/// Initialize the dictionary with an unsigned 32-bit buffer and the
/// size of the dictionary.
#[allow(clippy::not_unsafe_ptr_arg_deref)]
pub fn dict_init_safe(dictionary_addr: *mut &'static [u32], dictionary_size: u32) {
    unsafe { dict_init(dictionary_addr as *mut u32, dictionary_size) }
}

/// Find the length of a word given a pointer to the word flag field
///
/// word_flag_ptr is a pointer to the word flags and length byte
/// Return the length of the word
#[allow(clippy::not_unsafe_ptr_arg_deref)]
pub fn dict_word_length_safe(word_flag_ptr: *const u8) -> u8 {
    unsafe { dict_word_length(word_flag_ptr) }
}

/// Encode a Rust string as UTF-32 and store in a u32 word buffer
/// Rust strings are encoded as UTF-8 currently, but we can get individual
/// characters.
/// This handles "Unicode scalar values".  This is most Unicode
/// code points but not high-surrogates and low-surrogates (values
/// in 0 to 0xD7FF and 0xE000 to 0x10FFFF inclusive)
fn encode_string_as_utf32(src: &str, dst: &mut [u32]) -> Result<(), Error> {
    for (i, c) in src.chars().enumerate() {
        if i >= dst.len() {
            return Err(boop::error::Error::new(boop::error::ErrorKind::WordTooLong));
        }
        dst[i] = c as u32;
    }
    Ok(())
}

/// Add a word to the dictionary
///
/// Adds a word to the dictionary
/// word is the word to add.  It must be less than or equal to 31 characters.
/// flags is the set of flags to set on the word.
pub fn dict_add_word_safe(word: &str, flags: &[Flag]) -> Result<(), Error> {
    let word_len = boop::dict::number_of_characters_in_string(word);

    // First encode as UTF-32
    unsafe {
        encode_string_as_utf32(word, &mut FORTH_WORD_TMP_BUFFER)?;
    }

    let dst = unsafe { FORTH_WORD_TMP_BUFFER.as_mut_ptr() };

    // dict_encode_ascii_string_as_utf32_safe(word, dst)?;

    // Set flags
    let mut flags_byte: u8 = 0x00;
    for flag in flags {
        flags_byte |= match flag {
            Flag::Hidden => 0x20,
            Flag::Immediate => 0x80,
        }
    }

    let res = unsafe { dict_add_word(dst, word_len, flags_byte) };

    match res {
        0 => Ok(()),
        1 => Err(boop::error::Error::new(boop::error::ErrorKind::OutOfMemory)),
        2 => Err(boop::error::Error::new(boop::error::ErrorKind::WordTooLong)),
        3 => Err(boop::error::Error::new(
            boop::error::ErrorKind::WordTooShort,
        )),
        _ => Err(boop::error::Error::new(boop::error::ErrorKind::Unknown)),
    }
}

/// Find a word in the dictionary
/// This function returns the Word
/// word is the word to search for
pub fn dict_find_safe(word: &str) -> Result<Word, Error> {
    let word_len = boop::dict::number_of_characters_in_string(word);
    let mut result: u32 = 0;

    // First encode as UTF-32
    unsafe {
        encode_string_as_utf32(word, &mut FORTH_WORD_TMP_BUFFER)?;
    }

    let dst = unsafe { FORTH_WORD_TMP_BUFFER.as_ptr() };

    // dict_encode_ascii_string_as_utf32_safe(word, dst)?;

    let res = unsafe { dict_find(dst, word_len, &mut result) };

    // Convert the result to a Word structure

    match result {
        0 => {
            let word_flags_and_len = unsafe { *(((res as usize) + 4) as *const u8) };
            let word_len = word_flags_and_len & 0x1F;
            let flags: &[Flag] = match word_flags_and_len & 0xE0 {
                0xE0 => &[Flag::Hidden, Flag::Immediate],
                0xC0 => &[Flag::Immediate],
                0xA0 => &[Flag::Hidden, Flag::Immediate],
                0x80 => &[Flag::Immediate],
                0x60 => &[Flag::Hidden],
                0x40 => &[],
                0x20 => &[Flag::Hidden],
                0x00 => &[],
                _ => &[],
            };

            let word_ptr = ((res as usize) + 5) as *const u32;
            Ok(Word {
                link: res as u32,
                length: word_len,
                flags,
                word: word_ptr,
            })
        }
        1 => Err(boop::error::Error::new(
            boop::error::ErrorKind::WordNotFound,
        )),
        _ => Err(boop::error::Error::new(boop::error::ErrorKind::Unknown)),
    }
}

/// Encode an ASCII character as UTF-32.
/// If the character isn't valid ASCII, an ASCIIEncode error is returned.
pub fn dict_encode_ascii_as_utf32_safe(character: u32) -> Result<u32, Error> {
    let mut result: u32 = 0;

    let res = unsafe { dict_encode_ascii_as_utf32(character, &mut result) };

    match result {
        0 => Ok(res),
        1 => Err(boop::error::Error::new(boop::error::ErrorKind::ASCIIEncode)),
        _ => Err(boop::error::Error::new(boop::error::ErrorKind::Unknown)),
    }
}

/// Encode an ASCII string as a UTF-32 string
/// If the string isn't valid ASCII, an ASCIIEncode error is returned.
#[allow(clippy::not_unsafe_ptr_arg_deref)]
pub fn dict_encode_ascii_string_as_utf32_safe(src: &[u8], dst: *mut u32) -> Result<(), Error> {
    let word_buf = src.as_ptr();
    let word_len: usize = src.len();

    let res = unsafe { dict_encode_ascii_string_as_utf32(word_buf, word_len as u32, dst) };

    match res {
        0 => Ok(()),
        1 => Err(boop::error::Error::new(boop::error::ErrorKind::ASCIIEncode)),
        2 => Err(boop::error::Error::new(
            boop::error::ErrorKind::WordTooShort,
        )),
        _ => Err(boop::error::Error::new(boop::error::ErrorKind::Unknown)),
    }
}
