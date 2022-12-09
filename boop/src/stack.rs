//! Forth stack wrapper with Rust Error results and behavior
#![warn(missing_docs)]

use core::fmt::{Debug, Display, Formatter, Result, Write};
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
    /// A stack overflow would occur if an item is pushed
    StackOverflow,
    /// A stack underflow would occur if an item is popped
    StackUnderflow,
    /// An unknown error type
    Unknown,
}

impl Display for ErrorKind {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match self {
            ErrorKind::StackOverflow => write!(f, "A stack overflow occurred"),
            ErrorKind::StackUnderflow => write!(f, "A stack underflow occurred"),
            ErrorKind::Unknown => write!(f, "An unknown error occurred"),
        }
    }
}

impl Debug for ErrorKind {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "{}", self)
    }
}

/// The settings for the stacks
pub struct StackSettings {
    /// The stack address
    pub stack_addr: usize,
    /// The stack bottom
    pub stack_bottom: usize,
}

/// Collection of functions for the stack data structure
/// This is primarily used to make testing easier
pub struct StackFunctions {
    /// The stack initialization function
    pub init: fn(u32, u32),
    /// The push function for the stack
    pub push: fn(u32) -> core::result::Result<(), crate::stack::Error>,
    /// The pop function for the stack
    pub pop: fn() -> core::result::Result<u32, crate::stack::Error>,
    /// Get the bottom address of the stack
    pub get_stack_bottom_safe: fn() -> u32,
}

// /// The operations that a stack can perform
// pub trait StackOps {
//     /// Initialize the stack
//     fn stack_init(stack_addr: u32, stack_bottom: u32);
//     /// The push function for the stack
//     fn push(value: u32) -> core::result::Result<(), crate::stack::Error>;
//     /// The pop function for the stack
//     fn pop() -> core::result::Result<u32, crate::stack::Error>;
//     /// Get the bottom address of the stack
//     fn get_stack_bottom_safe() -> u32;
// }

/// The data structure representing a stack.
/// This contains the platform-dependent functions and data for a
/// stack.
pub struct Stack<'a> {
    /// The output stream for writing debug and log messages
    pub writer: &'a mut dyn Write,
    /// The settings for the stack
    pub settings: StackSettings,
    /// The functions for the stack
    pub functions: StackFunctions,
}

impl<'a> Stack<'a> {
    /// Create a new stack
    pub fn new(
        writer: &'a mut dyn Write,
        settings: StackSettings,
        functions: StackFunctions,
    ) -> Stack<'a> {
        Stack {
            writer,
            settings,
            functions,
        }
    }
}

/// Initialize the stacks
pub fn stack_init_safe(stack_init: fn(u32, u32), memory_start: u32, stack_bottom: u32) {
    stack_init(memory_start, stack_bottom)
}

/// Run the stack tests
pub fn run_tests(stack: &mut Stack) {
    // Initialize the return and parameter stacks
    write!(stack.writer, "before stack init\r\n").unwrap();
    tests::test_stack_init_works(stack);
    write!(stack.writer, "after stack init\r\n").unwrap();

    let return_stack_bottom_res = tests::get_stack_bottom_test(stack);

    write!(
        stack.writer,
        "return stack bottom: 0x{:X}\r\n",
        return_stack_bottom_res
    )
    .unwrap();

    assert_ne!(return_stack_bottom_res, 0);

    tests::test_poprsp_twice_works(stack);

    tests::test_poprsp_stack_underflow_fails(stack);
    tests::test_pop_rsp_works(stack);
    tests::test_pushrsp_stack_overflow_fails(stack);
}

/// Test the stack code
#[allow(unused_imports)]
pub mod tests {
    use crate::stack::Stack;
    use core::{arch::asm, fmt::Write};

    /// Test initialization of the Forth system
    /// The return stack should be set to the value RETURN_STACK_BOTTOM
    /// is set to in the ELF binary sections
    pub fn test_stack_init_works(stack: &mut Stack) {
        let mut stack_pointer: u32;
        let mut stack_addr: u32;
        let stack_bottom: u32 = stack.settings.stack_bottom as u32;

        write!(stack.writer, "Testing stack_init works\r\n").unwrap();

        unsafe {
            asm!(
                "mov r5, {}",
                "mov {}, r5",
                in(reg) stack.settings.stack_addr,
                out(reg) stack_addr,
                options(nomem, nostack, preserves_flags));
        }
        write!(
            stack.writer,
            "The Forth memory starts at 0x{:X}\r\n",
            stack_addr
        )
        .unwrap();

        unsafe {
            asm!(
                "mov {}, r5",
                out(reg) stack_pointer,
                options(nomem, nostack, preserves_flags));
        }
        write!(
            stack.writer,
            "Current value of stack pointer: 0x{:X}\r\n",
            stack_pointer
        )
        .unwrap();

        crate::stack::stack_init_safe(stack.functions.init, stack_addr, stack_bottom);

        unsafe {
            asm!(
                "mov {}, r5",
                out(reg) stack_pointer,
                options(nomem, nostack, preserves_flags));
        }
        write!(
            stack.writer,
            "Current value of stack pointer: 0x{:X}\r\n",
            stack_pointer
        )
        .unwrap();
    }

    /// Test that pushing a value onto the return stack works
    pub fn test_push_rsp_works(
        writer: &mut dyn Write,
        push_rsp: fn(u32) -> Result<(), crate::stack::Error>,
    ) {
        let mut stack_pointer_old: u32;

        unsafe {
            asm!(
                "mov {}, r5",
                out(reg) stack_pointer_old,
                options(nomem, nostack, preserves_flags));
        }
        write!(
            writer,
            "Current value of stack pointer: 0x{:X}\r\n",
            stack_pointer_old
        )
        .unwrap();

        let _res = push_rsp(3);

        let mut stack_pointer_new: u32;
        unsafe {
            asm!(
                "mov {}, r5",
                out(reg) stack_pointer_new,
                options(nomem, nostack, preserves_flags));
        }

        write!(
            writer,
            "New value of stack pointer: 0x{:X}\r\n",
            stack_pointer_new
        )
        .unwrap();

        assert_eq!(stack_pointer_old - stack_pointer_new, 4);
    }

    /// Test that pushing a value on a full stack fails
    pub fn test_pushrsp_stack_overflow_fails(stack: &Stack) {
        for _i in 0..512 {
            let res = (stack.functions.push)(3);
            assert!(res.is_ok());
        }
        let res = (stack.functions.push)(3);
        assert!(res.is_err());
    }

    /// Test that popping a value when there are no values on the
    /// stack fails
    pub fn test_poprsp_stack_underflow_fails(stack: &Stack) {
        let res = (stack.functions.pop)();
        assert!(res.is_err());

        let res = (stack.functions.pop)();
        assert!(res.is_err());
    }

    /// Test that popping value from the return stack works
    pub fn test_pop_rsp_works(stack: &mut Stack) {
        let mut stack_pointer_old: u32;
        let test_val: u32 = 0x798C6FD6;

        unsafe {
            asm!(
                "mov {}, r5",
                out(reg) stack_pointer_old,
                options(nomem, nostack, preserves_flags));
        }
        write!(
            stack.writer,
            "Current value of stack pointer: 0x{:X}\r\n",
            stack_pointer_old
        )
        .unwrap();

        let _res = (stack.functions.push)(test_val);

        let mut stack_pointer_new: u32;
        unsafe {
            asm!(
                "mov {}, r5",
                out(reg) stack_pointer_new,
                options(nomem, nostack, preserves_flags));
        }

        write!(
            stack.writer,
            "New value of stack pointer: 0x{:X}\r\n",
            stack_pointer_new
        )
        .unwrap();

        assert_eq!(stack_pointer_old - stack_pointer_new, 4);

        let res = (stack.functions.pop)();

        let mut stack_pointer_new: u32;
        unsafe {
            asm!(
                "mov {}, r5",
                out(reg) stack_pointer_new,
                options(nomem, nostack, preserves_flags));
        }

        // Ok, the stack pointer is getting adjusted correctly
        assert_eq!(stack_pointer_old - stack_pointer_new, 0);

        write!(
            stack.writer,
            "New value of stack pointer: 0x{:X}\r\n",
            stack_pointer_new
        )
        .unwrap();

        if let Ok(r) = res {
            write!(
                stack.writer,
                "res: 0x{:X}, test_val: 0x{:X}\r\n",
                r, test_val
            )
            .unwrap();
            assert_eq!(r, test_val);
        } else {
            write!(stack.writer, "res: Err(()), test_val: 0x{:X}\r\n", test_val).unwrap();
            panic!();
        }
    }

    /// Test that popping two values from the stack works.
    pub fn test_poprsp_twice_works(stack: &Stack) {
        let test_val_1 = 0x23481801;
        let test_val_2 = 0x25692815;

        let _res = (stack.functions.push)(test_val_1);
        let _res = (stack.functions.push)(test_val_2);

        let res = (stack.functions.pop)();

        match res {
            Ok(v) => assert_eq!(v, test_val_2),
            Err(_) => panic!(),
        }

        let res = (stack.functions.pop)();

        match res {
            Ok(v) => assert_eq!(v, test_val_1),
            Err(_) => panic!(),
        }

        let res = (stack.functions.pop)();
        match res {
            Ok(_) => panic!(),
            Err(e) => {
                assert_eq!(e.kind, crate::stack::ErrorKind::StackUnderflow);
            }
        }
    }

    /// Test getting the address of the return stack bottom
    pub fn get_stack_bottom_test(stack: &Stack) -> u32 {
        (stack.functions.get_stack_bottom_safe)()
    }
}
