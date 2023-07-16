//! Test driver for ARM assembly code that is compatible with QEMU
#![warn(missing_docs)]
#![allow(clippy::empty_loop)]
#![no_main]
#![no_std]

use core::fmt::Write;

use stm32f1xx_hal as _;

use boop::{
    buffer::{Buffer, BufferTester},
    stack::{Stack, StackTester},
};

use boop_stm32f1::{
    buffer_clear_safe, buffer_init_safe, buffer_read_word_safe, buffer_write_word_safe,
    dict_add_word_safe, dict_encode_ascii_as_utf32_safe, dict_encode_ascii_string_as_utf32_safe,
    dict_find_safe, dict_init_safe, dict_word_length_safe, get_stack_bottom_safe, stack_init_safe,
    stack_pop_safe, stack_push_safe, start_safe, FORTH_RETURN_STACK_HANDLE,
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
    // Create a test buffer.
    //
    // In the normal program, we might use statics and lock them for
    // the duration of their use, for example during serial interrupt
    // routines.  For the test use case, we can just create a temporary
    // buffer scoped to the test function.

    // POSSIBLE BUG: This doesn't work as expected.
    // It overwrites the FORTH_RETURN_STACK allocation in the crate
    // global scope.
    // #[link_section = ".ram2bss"]
    // static mut forth_test_buffer: [u32; 256] = [0; 256];

    let mut array: [u32; 256] = [0; 256];

    let buffer = Buffer::new(
        &mut array,
        boop::buffer::BufferFunctions {
            buffer_init_safe,
            buffer_clear_safe,
            buffer_write_word_safe,
            buffer_read_word_safe,
        },
    )
    .unwrap();

    let mut buffer_tester = BufferTester { buffer, writer };
    boop::buffer::run_tests(&mut buffer_tester);
}

fn run_tests(writer: &mut dyn Write) {
    let return_stack_handle = cortex_m::interrupt::free(|cs| unsafe {
        FORTH_RETURN_STACK_HANDLE.borrow(cs).replace(None).unwrap()
    });

    // Get the return stack address from the memory layout
    let return_stack_addr = return_stack_handle.data as usize + return_stack_handle.len;
    let return_stack_size = return_stack_handle.len;
    let return_stack_bottom_addr = return_stack_addr - return_stack_size;

    write!(
        writer,
        "return stack bottom in Rust: 0x{:X}\r\n",
        return_stack_bottom_addr,
    )
    .unwrap();

    write!(
        writer,
        "FORTH_RETURN_STACK: 0x{:X}\r\n",
        return_stack_addr as u32
    )
    .unwrap();

    boop::tests::test_start_works(writer, start_safe);

    let stack = Stack::new(
        boop::stack::StackSettings {
            stack_addr: return_stack_addr,
            stack_bottom_addr: return_stack_bottom_addr,
        },
        boop::stack::StackFunctions {
            init: stack_init_safe,
            push: stack_push_safe,
            pop: stack_pop_safe,
            get_stack_bottom_safe,
        },
    );

    let mut stack_tester = StackTester { writer, stack };

    boop::stack::run_tests(&mut stack_tester);

    let res = cortex_m::interrupt::free(|cs| unsafe {
        FORTH_RETURN_STACK_HANDLE
            .borrow(cs)
            .replace(Some(return_stack_handle))
    });

    if res.is_none() {
        writeln!(writer, "SUCCESS replaced FORTH_RETURN_STACK_HANDLE").unwrap();
    };

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
