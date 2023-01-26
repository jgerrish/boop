//! STM32F1xx implementations of Forth subroutines and tests.
//! The STM32F1xx is a Cortex-M3 device that is used as the basis for
//! QEMU testing.
#![warn(missing_docs)]
#![no_std]

use boop::{
    dict::{Flag, Word},
    error::Error,
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

/// The address of the Forth return stack
#[link_section = ".ram2bss"]
pub static mut FORTH_RETURN_STACK: [u8; 2048] = [0; 2048];

/// The address of the Forth parameter stack
#[link_section = ".ram2bss"]
pub static mut FORTH_PARAMETER_STACK: [u8; 2048] = [0; 2048];

/// Allocate space for a buffer.
/// The buffer is implemented as a queue.
/// The number of items that can be stored is one less than the (data
/// type) size of the queue.
/// For example, if the size allocated below is 1024, and the data
/// type size is four bytes, then the number of items that can be
/// stored is 255, not 256.
#[link_section = ".ram2bss"]
pub static mut FORTH_BUFFER: [u8; 1024] = [0; 1024];

/// temporary buffer to store UTF-32 encoded strings
#[link_section = ".ram2bss"]
pub static mut FORTH_WORD_TMP_BUFFER: [u32; 128] = [0; 128];

/// dictionary storage
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
    pub fn jjforth_stack_init(memory_start: u32, stack_bottom: u32);

    /// Pop a value from the stack
    pub fn jjforth_stack_pop(data: &mut u32) -> u32;

    /// Push a value onto the stack
    pub fn jjforth_stack_push(value: u32) -> u32;

    /// Initialize the Forth system
    pub fn jjforth_start();

    /// Get the stack bottom address
    pub fn jjforth_get_stack_bottom() -> u32;

    /// Initialize the buffer
    pub fn jjforth_buffer_init(buffer_start: u32, buffer_end: u32);

    /// Clear / empty the buffer
    pub fn jjforth_buffer_clear();

    /// Write a single byte to the input buffer
    pub fn jjforth_buffer_write(word: u32) -> u32;

    /// Read a single byte from the input buffer
    pub fn jjforth_buffer_read(data: &mut u32) -> u32;

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
pub fn stack_init_safe(memory_start: u32, stack_bottom: u32) {
    unsafe { jjforth_stack_init(memory_start, stack_bottom) }
}

/// Wrapper to pop a value from the stack
pub fn stack_pop_safe() -> Result<u32, boop::error::Error> {
    let mut data: u32 = 0x79327523;
    let res = unsafe { jjforth_stack_pop(&mut data) };
    match res {
        0 => Ok(data),
        1 => Err(boop::error::Error::new(
            boop::error::ErrorKind::StackUnderflow,
        )),
        _ => Err(boop::error::Error::new(boop::error::ErrorKind::Unknown)),
    }
}

/// Wrapper to push a value onto the stack
pub fn stack_push_safe(value: u32) -> Result<(), boop::error::Error> {
    let res = unsafe { jjforth_stack_push(value) };

    match res {
        0 => Ok(()),
        1 => Err(boop::error::Error::new(
            boop::error::ErrorKind::StackOverflow,
        )),
        _ => Err(boop::error::Error::new(boop::error::ErrorKind::Unknown)),
    }
}

/// Safe wrapper to the start subroutine
pub fn start_safe() {
    unsafe { jjforth_start() }
}

/// Safe wrapper to get the stack bottom address
pub fn get_stack_bottom_safe() -> u32 {
    unsafe { jjforth_get_stack_bottom() }
}

/// Initialize the buffer
pub fn buffer_init_safe(buffer_start: u32, buffer_end: u32) {
    unsafe { jjforth_buffer_init(buffer_start, buffer_end) }
}

/// Clear / empty the buffer
pub fn buffer_clear_safe() {
    unsafe { jjforth_buffer_clear() }
}

/// Write to the input buffer
pub fn buffer_write_safe(data: &[u32]) -> Result<(), boop::buffer::Error> {
    for word in data {
        let res = unsafe { jjforth_buffer_write(*word) };
        match res {
            0 => continue,
            _ => return Err(boop::buffer::Error::new(boop::buffer::ErrorKind::Full)),
        }
    }
    Ok(())
}

/// Write to the input buffer
pub fn buffer_write_word_safe(data: u32) -> Result<(), boop::buffer::Error> {
    let res = unsafe { jjforth_buffer_write(data) };
    match res {
        0 => Ok(()),
        _ => Err(boop::buffer::Error::new(boop::buffer::ErrorKind::Full)),
    }
}

/// Read from the input buffer
pub fn buffer_read_word_safe() -> Result<u32, boop::buffer::Error> {
    let mut error_code: u32 = 0;

    let res = unsafe { jjforth_buffer_read(&mut error_code) };
    match error_code {
        0 => Ok(res),
        _ => Err(boop::buffer::Error::new(boop::buffer::ErrorKind::Empty)),
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

    let dst = unsafe { FORTH_WORD_TMP_BUFFER.as_mut_ptr() };

    // dict_encode_ascii_string_as_utf32_safe(word, dst)?;

    let res = unsafe { dict_find(dst, word_len, &mut result) };

    // Convert the result to a Word structure

    match result {
        0 => {
            let word_len = unsafe { *(((res as usize) + 4) as *const u8) };
            let word_ptr = ((res as usize) + 5) as *const u32;
            let slice = unsafe { core::slice::from_raw_parts(word_ptr, word_len as usize) };
            Ok(Word {
                link: res as u32,
                flags: word_len,
                word: slice,
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
