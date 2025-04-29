//! Forth stack wrapper with Rust Error results and behavior
#![warn(missing_docs)]

use core::{
    fmt::{Debug, Display, Formatter, Result, Write},
    marker::PhantomData,
};

use crate::ArrayHandle;

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
    /// A NULL pointer error occurred
    NullPointer,
    /// Invalid arguments were passed into a function.
    /// Example includes trying to initialize a stack with the bottom
    /// greater than the top.
    InvalidArguments,
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
            ErrorKind::NullPointer => write!(f, "A NULL pointer was passed to the function"),
            ErrorKind::InvalidArguments => {
                write!(f, "Invalid arguments were passed to the function")
            }
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

/// The stack structure used for FFI
#[repr(C)]
pub struct StackStruct<'a> {
    /// The number of entries the stack can store
    pub len: usize,
    /// The bottom of the stack.
    /// This the location of one item beyond the end of the array.
    /// Data can't be stored there, but it is used for checking for
    /// stack underflow when popping an item.
    pub bottom: *const u32,
    /// The current top of the stack
    pub top: *mut u32,
    /// The maximum top of the stack
    /// This is the start of the array
    pub maximum_top: *const u32,
    /// Marker so we can link lifetimes to the backing array
    _marker: PhantomData<&'a u32>,
}

impl<'a> Debug for StackStruct<'a> {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "len: {}, ", self.len)?;
        write!(f, "bottom: {:?}, ", self.bottom)?;
        write!(f, "top: {:?}, ", self.top)?;
        write!(f, "maximum_top: {:?}, ", self.maximum_top)
    }
}

/// Collection of functions for the stack data structure
/// This is primarily used to make testing easier
pub struct StackFunctions {
    /// The stack initialization function
    pub init: fn(*mut StackStruct, *const u32, u32) -> core::result::Result<(), Error>,
    /// The push function for the stack
    pub push: fn(*const StackStruct, u32) -> core::result::Result<(), Error>,
    /// The pop function for the stack
    pub pop: fn(*const StackStruct) -> core::result::Result<u32, Error>,
    /// Get the bottom address of the stack
    pub get_stack_bottom_safe: fn(*const StackStruct) -> core::result::Result<u32, Error>,
}

/// Basic operations that a stack can perform
pub trait StackOps<T> {
    /// Push an item onto the stack
    ///
    /// # Arguments
    /// # Returns
    ///
    /// # Examples
    /// ```
    /// use crate::{
    ///     ArrayHandle,
    ///     stack::{Error, Stack, StackFunctions, StackOps, StackStruct}
    /// };
    ///
    /// let mut arr: [u32; 4] = [0; 4];
    /// let handle = ArrayHandle::new(arr.as_mut_ptr(), arr.len());
    /// let sf = StackFunctions {
    ///     init: |_stack: *mut StackStruct,
    ///            _memory_start: *const u32,
    ///            _stack_len: u32|
    ///      -> core::result::Result<(), Error> { Ok(()) },
    ///     push: |_stack: *const StackStruct, _value: u32| -> core::result::Result<(), Error> {
    ///         Ok(())
    ///     },
    ///     pop: |_stack: *const StackStruct| -> core::result::Result<u32, Error> { Ok(0xF4) },
    ///     get_stack_bottom_safe:
    ///         |_stack: *const StackStruct| -> core::result::Result<u32, Error> { Ok(0) },
    /// };
    /// let mut stack = Stack::new(&handle, &sf).unwrap();
    ///
    /// let res = stack.push(0xF4);
    ///
    /// assert!(res.is_ok());
    ///
    /// ```
    fn push(&mut self, value: T) -> core::result::Result<(), crate::stack::Error>;
    /// Pop an item from the stack
    ///
    /// # Arguments
    /// # Returns
    ///
    /// # Examples
    /// ```
    /// use crate::{
    ///     ArrayHandle,
    ///     stack::{Error, Stack, StackFunctions, StackOps, StackStruct}
    /// };
    ///
    ///
    /// let mut arr: [u32; 4] = [0; 4];
    /// let handle = ArrayHandle::new(arr.as_mut_ptr(), arr.len());
    /// let sf = StackFunctions {
    ///     init: |_stack: *mut StackStruct,
    ///            _memory_start: *const u32,
    ///            _stack_len: u32|
    ///      -> core::result::Result<(), Error> { Ok(()) },
    ///     push: |_stack: *const StackStruct, _value: u32| -> core::result::Result<(), Error> {
    ///         Ok(())
    ///     },
    ///     pop: |_stack: *const StackStruct| -> core::result::Result<u32, Error> { Ok(0x35) },
    ///     get_stack_bottom_safe:
    ///         |_stack: *const StackStruct| -> core::result::Result<u32, Error> { Ok(0) },
    /// x};
    ///
    /// let mut stack = Stack::new(&handle, &sf).unwrap();
    ///
    /// let _res = stack.push(0x35);
    /// let res = stack.pop().unwrap();
    ///
    /// assert_eq!(res, 0x35);
    ///
    ///
    /// ```
    fn pop(&mut self) -> core::result::Result<T, crate::stack::Error>;
}

/// The data structure representing a stack.
/// This contains the platform-dependent functions and data for a
/// stack.
pub struct Stack<'a> {
    /// The C structure
    pub stack_struct: StackStruct<'a>,
    /// The functions for the stack
    pub functions: &'a StackFunctions,
}

impl<'a> Debug for Stack<'a> {
    fn fmt(&self, f: &mut Formatter) -> Result {
        let slice = unsafe {
            core::slice::from_raw_parts(self.stack_struct.maximum_top, self.stack_struct.len)
        };
        write!(f, "stack struct: {:?}, ", self.stack_struct)?;
        write!(f, "contents: {:?}, ", slice)
    }
}

impl<'a> Stack<'a> {
    /// Create a new stack
    pub fn new(
        handle: &'a ArrayHandle<'a, u32>,
        functions: &'a StackFunctions,
    ) -> core::result::Result<Self, Error> {
        let stack_struct = StackStruct {
            len: 0,
            bottom: core::ptr::null_mut::<u32>(),
            top: core::ptr::null_mut::<u32>(),
            maximum_top: core::ptr::null_mut::<u32>(),
            _marker: PhantomData,
        };

        let mut stack = Stack {
            stack_struct,
            functions,
        };

        let res = stack_init_safe(&mut stack, handle.ptr, handle.len as u32);

        match res {
            Ok(_) => Ok(stack),
            Err(e) => Err(e),
        }
    }
}

impl<'a> StackOps<u32> for Stack<'a> {
    fn push(&mut self, value: u32) -> core::result::Result<(), crate::stack::Error> {
        (self.functions.push)(core::ptr::addr_of!(self.stack_struct), value)
    }
    fn pop(&mut self) -> core::result::Result<u32, crate::stack::Error> {
        (self.functions.pop)(core::ptr::addr_of!(self.stack_struct))
    }
}

/// Create a testing structure to run tests against a stack
pub struct StackTester<'a> {
    /// The Stack structure to test
    pub stack: Stack<'a>,
    /// The output stream for writing debug and log messages
    pub writer: &'a mut dyn Write,
}

/// Initialize the stacks
pub fn stack_init_safe(
    stack: &mut Stack,
    memory_start: *const u32,
    stack_len: u32,
) -> core::result::Result<(), Error> {
    let addr = core::ptr::addr_of_mut!(stack.stack_struct);

    (stack.functions.init)(addr, memory_start, stack_len)
}

/// Run the stack tests
pub fn run_tests(stack_tester: &mut StackTester) {
    // Initialize the return and parameter stacks
    // tests::test_stack_init_works(stack_tester);

    tests::test_init_works(stack_tester);
    tests::test_init_zero_length_fails(stack_tester);
    tests::test_init_end_of_memory_works(stack_tester);
    tests::test_init_carry_fails(stack_tester);

    tests::test_pop_twice_works(stack_tester);
    tests::test_pop_stack_underflow_fails(stack_tester);
    tests::test_pop_works(stack_tester);
    tests::test_push_works(stack_tester);
    tests::test_stack_overflow_fails(stack_tester);
    tests::test_fill_empty_fill_works(stack_tester);

    doc_tests::test_push();
    doc_tests::test_pop();
}

/// Test the stack code
#[allow(unused_imports)]
pub mod tests {
    use crate::{
        stack::{Error, ErrorKind, Stack, StackOps, StackStruct, StackTester},
        tests::write_test_result,
        ArrayHandle,
    };
    use core::{arch::asm, fmt::Write, marker::PhantomData};

    /// Test that init works
    pub fn test_init_works(stack_tester: &mut StackTester) {
        let mut arr: [u32; 4] = [0; 4];
        let handle = ArrayHandle::new(arr.as_mut_ptr(), arr.len());

        let stack_struct = StackStruct {
            len: 0,
            bottom: core::ptr::null_mut::<u32>(),
            top: core::ptr::null_mut::<u32>(),
            maximum_top: core::ptr::null_mut::<u32>(),
            _marker: PhantomData,
        };

        let mut stack = Stack {
            stack_struct,
            functions: stack_tester.stack.functions,
        };

        let res = crate::stack::stack_init_safe(&mut stack, handle.ptr, handle.len as u32);

        write_test_result(
            stack_tester.writer,
            res.is_ok(),
            "stack::tests::test_init_works init result was ok",
        );

        write_test_result(
            stack_tester.writer,
            stack.stack_struct.len == arr.len(),
            "stack::tests::test_init_works stack size is correct",
        );

        let bottom =
            (arr.as_ptr() as usize + (arr.len() * ((u32::BITS / 8) as usize))) as *const u32;

        write_test_result(
            stack_tester.writer,
            stack.stack_struct.bottom == bottom,
            "stack::tests::test_init_works stack bottom is correct",
        );

        write_test_result(
            stack_tester.writer,
            stack.stack_struct.top == bottom as *mut u32,
            "stack::tests::test_init_works stack top is correct",
        );

        write_test_result(
            stack_tester.writer,
            stack.stack_struct.maximum_top == arr.as_mut_ptr(),
            "stack::tests::test_init_works stack maximum top is correct",
        );
    }

    /// Test that init with a zero length fails
    pub fn test_init_zero_length_fails(stack_tester: &mut StackTester) {
        let mut arr: [u32; 0] = [0; 0];
        let handle = ArrayHandle::new(arr.as_mut_ptr(), arr.len());

        let stack_struct = StackStruct {
            len: 0,
            bottom: core::ptr::null_mut::<u32>(),
            top: core::ptr::null_mut::<u32>(),
            maximum_top: core::ptr::null_mut::<u32>(),
            _marker: PhantomData,
        };

        let mut stack = Stack {
            stack_struct,
            functions: stack_tester.stack.functions,
        };

        let res = crate::stack::stack_init_safe(&mut stack, handle.ptr, handle.len as u32);

        match res {
            Ok(_) => {
                write_test_result(
                    stack_tester.writer,
                    false,
                    "stack::tests::test_init_zero_length_fails init zero length fails",
                );
            }
            Err(e) => {
                write_test_result(
                    stack_tester.writer,
                    e.kind == ErrorKind::InvalidArguments,
                    "stack::tests::test_init_zero_length_fails init zero length fails",
                );
            }
        }
    }

    /// Test that init at the end of memory works
    pub fn test_init_end_of_memory_works(stack_tester: &mut StackTester) {
        let mut arr: [u32; 1] = [0; 1];
        let handle = ArrayHandle::new(arr.as_mut_ptr(), arr.len());

        let stack_struct = StackStruct {
            len: 0,
            bottom: core::ptr::null_mut::<u32>(),
            top: core::ptr::null_mut::<u32>(),
            maximum_top: core::ptr::null_mut::<u32>(),
            _marker: PhantomData,
        };

        let mut stack = Stack {
            stack_struct,
            functions: stack_tester.stack.functions,
        };

        let ptr = (u32::MAX - (u32::BITS / 8)) as *const u32;
        let res = crate::stack::stack_init_safe(&mut stack, ptr, handle.len as u32);

        write_test_result(
	    stack_tester.writer,
	    res.is_ok(),
	    "stack::tests::test_init_end_of_memory_works init with ptr + length at end of memory should work",
	);
    }

    /// Test that init at the end of memory that would wrap fails
    pub fn test_init_carry_fails(stack_tester: &mut StackTester) {
        let mut arr: [u32; 1] = [0; 1];
        let handle = ArrayHandle::new(arr.as_mut_ptr(), arr.len());

        let stack_struct = StackStruct {
            len: 0,
            bottom: core::ptr::null_mut::<u32>(),
            top: core::ptr::null_mut::<u32>(),
            maximum_top: core::ptr::null_mut::<u32>(),
            _marker: PhantomData,
        };

        let mut stack = Stack {
            stack_struct,
            functions: stack_tester.stack.functions,
        };

        let ptr = u32::MAX as *const u32;
        let res = crate::stack::stack_init_safe(&mut stack, ptr, handle.len as u32);

        match res {
            Ok(_) => {
                write_test_result(
		    stack_tester.writer,
		    false,
		    "stack::tests::test_init_carry_fails init with ptr + length that would carry fails",
		);
            }
            Err(e) => {
                write_test_result(
		    stack_tester.writer,
		    e.kind == ErrorKind::InvalidArguments,
		    "stack::tests::test_init_carry_fails init with ptr + length that would carry fails",
		);
            }
        }
    }

    /// Test that pushing a value onto the return stack works
    pub fn test_push_works(stack_tester: &mut StackTester) {
        let stack_pointer_old = stack_tester.stack.stack_struct.top;

        let res = stack_tester.stack.push(3);
        write_test_result(
            stack_tester.writer,
            res.is_ok(),
            "stack::tests::test_push_works push result was ok",
        );

        let stack_pointer_new = stack_tester.stack.stack_struct.top;

        write_test_result(
            stack_tester.writer,
            stack_pointer_new as usize == stack_pointer_old as usize - 4,
            "stack::tests::test_push_works stack pointer is decremented by proper ammount",
        );
        assert_eq!(stack_pointer_old as usize - stack_pointer_new as usize, 4);
    }

    /// Test that pushing a value on a full stack fails
    pub fn test_stack_overflow_fails(stack_tester: &mut StackTester) {
        stack_tester.stack.pop().unwrap();

        let num_items = stack_tester.stack.stack_struct.len;
        for i in 0..num_items {
            let res = stack_tester.stack.push(i as u32);
            assert!(res.is_ok());
        }

        let res = stack_tester.stack.push(3);
        assert!(res.is_err());

        // Empty out the stack
        for _i in 0..num_items {
            let res = stack_tester.stack.pop();
            assert!(res.is_ok());
        }
    }

    /// Test that popping a value when there are no values on the
    /// stack fails
    pub fn test_pop_stack_underflow_fails(stack_tester: &mut StackTester) {
        let res = stack_tester.stack.pop();
        assert!(res.is_err());

        let res = stack_tester.stack.pop();
        assert!(res.is_err());
    }

    /// Test that popping value from the return stack works
    pub fn test_pop_works(stack_tester: &mut StackTester) {
        let stack_pointer_old = stack_tester.stack.stack_struct.top;
        let test_val: u32 = 0x798C6FD6;

        let res = stack_tester.stack.push(test_val);
        write_test_result(
            stack_tester.writer,
            res.is_ok(),
            "stack::tests::test_pop_works push result was ok",
        );

        let stack_pointer_new = stack_tester.stack.stack_struct.top;

        write_test_result(
            stack_tester.writer,
            stack_pointer_new as usize == stack_pointer_old as usize - 4,
            "stack::tests::test_pop_works stack pointer is decremented by proper ammount",
        );
        assert_eq!(stack_pointer_old as usize - stack_pointer_new as usize, 4);

        let res = stack_tester.stack.pop();
        write_test_result(
            stack_tester.writer,
            res.is_ok(),
            "stack::tests::test_pop_works pop result was ok",
        );

        let stack_pointer_new = stack_tester.stack.stack_struct.top;

        // Ok, the stack pointer is getting adjusted correctly
        write_test_result(
            stack_tester.writer,
            stack_pointer_new as usize == stack_pointer_old as usize,
            "stack::tests::test_pop_works stack pointer is incremented by proper ammount",
        );
        assert_eq!(stack_pointer_old as usize - stack_pointer_new as usize, 0);

        let mut passed = false;
        if let Ok(r) = res {
            if r == test_val {
                passed = true;
            }
        }
        write_test_result(
            stack_tester.writer,
            passed,
            "stack::tests::test_pop_works current value of stack pointer should equal expected",
        );
        assert!(passed);
    }

    /// Test that popping two values from the stack works.
    pub fn test_pop_twice_works(stack_tester: &mut StackTester) {
        let test_val_1 = 0x23481801;
        let test_val_2 = 0x25692815;

        let res = stack_tester.stack.push(test_val_1);
        write_test_result(
            stack_tester.writer,
            res.is_ok(),
            "stack::tests::test_pop_twice_works first push was ok",
        );

        let res = stack_tester.stack.push(test_val_2);
        write_test_result(
            stack_tester.writer,
            res.is_ok(),
            "stack::tests::test_pop_twice_works second push was ok",
        );

        let res = stack_tester.stack.pop();

        match res {
            Ok(v) => {
                write_test_result(
                    stack_tester.writer,
                    res.is_ok(),
                    "stack::tests::test_pop_twice_works first pop was ok",
                );
                assert_eq!(v, test_val_2)
            }
            Err(_) => panic!(),
        }

        let res = stack_tester.stack.pop();

        match res {
            Ok(v) => {
                assert_eq!(v, test_val_1);
            }
            Err(_) => panic!(),
        }

        let res = stack_tester.stack.pop();
        match res {
            Ok(_) => panic!(),
            Err(e) => {
                assert_eq!(e.kind, ErrorKind::StackUnderflow);
            }
        }
    }

    /// Test getting the address of the return stack bottom
    pub fn get_stack_bottom_test(stack_tester: &StackTester) -> core::result::Result<u32, Error> {
        // stack_tester.stack.get_stack_bottom()
        (stack_tester.stack.functions.get_stack_bottom_safe)(core::ptr::addr_of!(
            stack_tester.stack.stack_struct
        ))
    }

    /// Test that pushing a value on a full stack fails
    pub fn test_fill_empty_fill_works(stack_tester: &mut StackTester) {
        let num_items = stack_tester.stack.stack_struct.len;

        let res = stack_tester.stack.pop();
        assert!(res.is_err());

        // filling the stack should work
        let mut res: Option<Result<(), Error>> = None;
        for _i in 0..num_items {
            let r = stack_tester.stack.push(3);
            res = Some(r);
            assert!(res.as_ref().expect("should be Some").is_ok());
        }

        write_test_result(
            stack_tester.writer,
            res.unwrap().is_ok(),
            "stack::tests::test_fill_empty_fill_works should fill stack",
        );

        // Pushing onto a full stack should fail
        let res = stack_tester.stack.push(3);
        match res {
            Ok(_) => {
                write_test_result(
                    stack_tester.writer,
                    false,
                    "stack::tests::test_fill_empty_fill_works overflow should fail",
                );
            }
            Err(e) => {
                write_test_result(
                    stack_tester.writer,
                    e == Error::new(ErrorKind::StackOverflow),
                    "stack::tests::test_fill_empty_fill_works overflow should fail",
                );
            }
        }

        // Popping every item from a full stack should work
        for _i in 0..num_items {
            let res = stack_tester.stack.pop();
            assert!(res.is_ok());
        }
        // Popping from an empty stack should fail
        let res = stack_tester.stack.pop();
        assert!(res.is_err());

        // Filling the stack again should work
        for _i in 0..num_items {
            let res = stack_tester.stack.push(3);
            assert!(res.is_ok());
        }
        // Pushing onto a full stack should fail
        let res = stack_tester.stack.push(3);
        assert!(res.is_err());
    }
}

mod doc_tests {
    use crate::{
        stack::{Error, Stack, StackFunctions, StackOps, StackStruct},
        ArrayHandle,
    };

    pub fn test_push() {
        let mut arr: [u32; 4] = [0; 4];
        let handle = ArrayHandle::new(arr.as_mut_ptr(), arr.len());
        let sf = StackFunctions {
            init: |_stack: *mut StackStruct,
                   _memory_start: *const u32,
                   _stack_len: u32|
             -> core::result::Result<(), Error> { Ok(()) },
            push: |_stack: *const StackStruct, _value: u32| -> core::result::Result<(), Error> {
                Ok(())
            },
            pop: |_stack: *const StackStruct| -> core::result::Result<u32, Error> { Ok(0xF4) },
            get_stack_bottom_safe:
                |_stack: *const StackStruct| -> core::result::Result<u32, Error> { Ok(0) },
        };
        let mut stack = Stack::new(&handle, &sf).unwrap();

        let res = stack.push(0xF4);

        assert!(res.is_ok());
    }
    pub fn test_pop() {
        let mut arr: [u32; 4] = [0; 4];
        let handle = ArrayHandle::new(arr.as_mut_ptr(), arr.len());
        let sf = StackFunctions {
            init: |_stack: *mut StackStruct,
                   _memory_start: *const u32,
                   _stack_len: u32|
             -> core::result::Result<(), Error> { Ok(()) },
            push: |_stack: *const StackStruct, _value: u32| -> core::result::Result<(), Error> {
                Ok(())
            },
            pop: |_stack: *const StackStruct| -> core::result::Result<u32, Error> { Ok(0x35) },
            get_stack_bottom_safe:
                |_stack: *const StackStruct| -> core::result::Result<u32, Error> { Ok(0) },
        };

        let mut stack = Stack::new(&handle, &sf).unwrap();

        let _res = stack.push(0x35);
        let res = stack.pop().unwrap();

        assert_eq!(res, 0x35);
    }
}
