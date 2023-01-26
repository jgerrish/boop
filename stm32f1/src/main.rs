//! Test driver for ARM assembly code that is compatible with QEMU
#![allow(clippy::empty_loop)]
#![no_main]
#![no_std]

use core::fmt::Write;

use stm32f1xx_hal as _;

use boop::{buffer::Buffer, stack::Stack};

use boop_stm32f1::{
    buffer_clear_safe, buffer_init_safe, buffer_read_word_safe, buffer_write_word_safe,
    dict_add_word_safe, dict_encode_ascii_as_utf32_safe, dict_encode_ascii_string_as_utf32_safe,
    dict_find_safe, dict_init_safe, dict_word_length_safe, get_stack_bottom_safe, stack_init_safe,
    stack_pop_safe, stack_push_safe, start_safe,
};

// use cortex_m::iprintln;
use cortex_m_rt::entry;
use cortex_m_semihosting::hio;

#[entry]
fn main() -> ! {
    let core = cortex_m::Peripherals::take().unwrap();

    let mpu = core.MPU;

    let mut hstdout = hio::hstdout().map_err(|_| core::fmt::Error).unwrap();

    boop::init(&mut hstdout, &mpu);

    write!(hstdout, "test1\r\n").unwrap();

    run_tests(&mut hstdout);

    loop {
        write!(hstdout, "In runtime loop\r\n").unwrap();
        cortex_m_semihosting::debug::exit(cortex_m_semihosting::debug::EXIT_SUCCESS);
    }
}

// Define a panic handler that uses semihosting to exit immediately,
// so that any panics cause qemu to quit instead of hang.
#[panic_handler]
fn panic(_: &core::panic::PanicInfo) -> ! {
    loop {
        cortex_m_semihosting::hprintln!("Panic!\n");
        cortex_m_semihosting::debug::exit(cortex_m_semihosting::debug::EXIT_FAILURE);
    }
}

/// Run the buffer tests
fn run_buffer_tests(writer: &mut dyn Write) {
    // Get the return stack address from the memory layout
    let buffer_addr: u32 = unsafe { boop_stm32f1::FORTH_BUFFER.as_ptr() as u32 };
    write!(writer, "buffer_addr in Rust: 0x{:X}\r\n", buffer_addr).unwrap();

    let buffer_size: u32 = unsafe { boop_stm32f1::FORTH_BUFFER.len() as u32 };

    let buffer_end: u32 = buffer_addr + buffer_size - 4;

    let mut buffer = Buffer::new(
        writer,
        boop::buffer::BufferSettings {
            buffer_addr,
            buffer_size,
            buffer_end,
        },
        boop::buffer::BufferFunctions {
            buffer_init_safe,
            buffer_clear_safe,
            buffer_write_word_safe,
            buffer_read_word_safe,
        },
    );

    boop::buffer::run_tests(&mut buffer);
}

fn run_tests(writer: &mut dyn Write) {
    // Get the return stack address from the memory layout
    let return_stack_addr = unsafe {
        boop_stm32f1::FORTH_RETURN_STACK.as_ptr() as usize + boop_stm32f1::FORTH_RETURN_STACK.len()
    };

    let return_stack_size = unsafe { boop_stm32f1::FORTH_RETURN_STACK.len() };

    let return_stack_bottom = return_stack_addr - return_stack_size;
    write!(
        writer,
        "return stack bottom in Rust: 0x{:X}\r\n",
        return_stack_bottom
    )
    .unwrap();

    write!(writer, "FORTH_RETURN_STACK: 0x{:X}\r\n", return_stack_addr).unwrap();

    write!(writer, "before start\r\n").unwrap();
    boop::tests::test_start_works(writer, start_safe);
    write!(writer, "after start\r\n").unwrap();

    let mut stack = Stack {
        writer,
        settings: boop::stack::StackSettings {
            stack_addr: return_stack_addr,
            stack_bottom: return_stack_bottom,
        },
        functions: boop::stack::StackFunctions {
            init: stack_init_safe,
            push: stack_push_safe,
            pop: stack_pop_safe,
            get_stack_bottom_safe,
        },
    };

    write!(stack.writer, "After stack_init\r\n").unwrap();

    boop::stack::run_tests(&mut stack);

    run_buffer_tests(writer);

    let dictionary_addr = unsafe { boop_stm32f1::FORTH_DICTIONARY.as_ptr() as *mut &'static [u32] };

    let dictionary_size = unsafe { boop_stm32f1::FORTH_DICTIONARY.len() as u32 };

    boop::dict::run_tests(
        writer,
        unsafe { &mut boop_stm32f1::FORTH_WORD_TMP_BUFFER },
        boop::dict::DictSettings {
            dictionary_addr,
            dictionary_size,
        },
        boop::dict::DictFunctions {
            dict_init_safe,
            dict_add_word_safe,
            dict_find_safe,
            dict_word_length_safe,
            dict_encode_ascii_as_utf32_safe,
            dict_encode_ascii_string_as_utf32_safe,
        },
    );
}
