//! STM32F1xx implementations of Forth subroutines and tests.
//! The STM32F1xx is a Cortex-M3 device that is used as the basis for
//! QEMU testing.
#![warn(missing_docs)]
#![no_std]

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
pub fn stack_pop_safe() -> Result<u32, boop::stack::Error> {
    let mut data: u32 = 0x79327523;
    let res = unsafe { jjforth_stack_pop(&mut data) };
    match res {
        0 => Ok(data),
        1 => Err(boop::stack::Error::new(
            boop::stack::ErrorKind::StackUnderflow,
        )),
        _ => Err(boop::stack::Error::new(boop::stack::ErrorKind::Unknown)),
    }
}

/// Wrapper to push a value onto the stack
pub fn stack_push_safe(value: u32) -> Result<(), boop::stack::Error> {
    let res = unsafe { jjforth_stack_push(value) };

    match res {
        0 => Ok(()),
        1 => Err(boop::stack::Error::new(
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
pub fn get_stack_bottom_safe() -> u32 {
    unsafe { jjforth_get_stack_bottom() }
}

/// Initialize the buffer
pub fn buffer_init_safe(buffer_start: u32, buffer_end: u32) {
    unsafe { jjforth_buffer_init(buffer_start as u32, buffer_end as u32) }
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
