//! A length-prefixed Pascal style string
//!
//! This is a FFI crate for a length-prefixed Pascal string
//!
//! Parent structure:
//!       +------------------+
//!       |  0x12345678 | 32 |
//!       +------------------+
//!           address     len
//!
//!  Array at address 0x12345678:
//!        len               string data
//!       +------------------------------------------+
//!       | 5 | H | e | l | l | o | .. unused .. |   |
//!       +------------------------------------------+
//!         0   1   2   3   4   5       ....      31
use crate::ArrayHandle;

// use alloc::string::{String, ToString};
use core::{
    fmt::{Debug, Display, Formatter, Result, Write},
    marker::PhantomData,
};

/// An error that can occur when operating on a pstring
#[derive(Eq, PartialEq)]
pub struct Error {
    /// A member structure representing the type or kind of error
    pub kind: ErrorKind,
}

impl Debug for Error {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "{:?}", self.kind)
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

/// The kinds of errors that can occur working with pstrings
#[derive(Eq, PartialEq)]
pub enum ErrorKind {
    /// Null pointer error
    NullPointer,
    /// String is full
    Full,
    /// Invalid arguments
    InvalidArguments,
    /// An unknown error type
    Unknown,
}

impl Display for ErrorKind {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match self {
            ErrorKind::NullPointer => write!(f, "Null pointer error"),
            ErrorKind::Full => write!(f, "String is full"),
            ErrorKind::InvalidArguments => write!(f, "Invalid arguments"),
            ErrorKind::Unknown => write!(f, "An unknown error occurred"),
        }
    }
}

impl Debug for ErrorKind {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "{}", self)
    }
}

/// Rust FFI Wrapper to the C functions
#[derive(Clone, Copy, Eq, PartialEq)]
pub struct PStringFunctions {
    /// The initialization function for the pstring
    pub pstring_init_safe:
        fn(&mut PString, *mut PStringStringStruct, u32) -> core::result::Result<(), Error>,
    /// Append a character to the string
    pub pstring_put_safe: fn(&mut PString, u32) -> core::result::Result<(), Error>,
}

/// A PString is a length-prefixed string
/// This is the parent data structure.
/// It mirrors the ArrayHandle, but it represented separately to
/// help manage the interface between code modules
#[repr(C)]
pub struct PStringStruct {
    /// A pointer to the array backing the string
    pub data: *mut PStringStringStruct,
    /// The length of the array backing the string
    pub length: u32,
    _marker: PhantomData<u32>,
}

/// The string data structure in the array
/// This is the structure that is like a Pascal style string.
#[repr(C)]
#[derive(Clone, Copy, Debug)]
pub struct PStringStringStruct {
    /// The length of the string
    pub length: u32,
    // This is followed by the actual string data
    // Which is an array of u32 data determined by the length in
    // PStringStruct
    //
    // The size isn't known at compile-time.
    // There should be some placeholder in here, or something.
    // I still haven't figured out how I'm going to initialize the
    // memory at the start of the application.  Right now it's static
    // fixed-size memory allocations.  This will likely be refactored.
    //
    // But the basics of a string data structure are in place now and
    // should provide a bridge between serial input / output, pads,
    // the parser and dictionary.
    //
    // Did I define this as much as I could or understand all the
    // options Rust provides?  No, I didn't.  The design space around
    // PString, allocation and type-checking can hopefully provide fun
    // for the next explorer.
}

impl Debug for PStringStruct {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "length: {:?}, ", self.length)?;
        writeln!(f, "data: {:?}", self.data)
    }
}

/// The PString Rust structure
pub struct PString<'a> {
    /// The underyling data structure for FFI
    pub pstring: PStringStruct,
    /// Rust wrapper to the FFI functions
    pub functions: &'a PStringFunctions,
}

impl<'a> Debug for PString<'a> {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "pstring: {:?}", self.pstring)?;
        let pstr_string_struct = unsafe { *self.pstring.data };

        write!(f, "pstring_string_struct: {:?}", pstr_string_struct)?;
        write!(f, "as slice: {:?}", self.as_slice())
    }
}

/// Create a testing structure to run tests against a PString
pub struct PStringTester<'a> {
    /// The PString structure to test
    pub pstring: PString<'a>,
    /// The output stream for writing debug and log messages
    pub writer: &'a mut dyn Write,
}

impl<'a> PString<'a> {
    /// Create a new PString
    /// TODO: Add example and docs
    pub fn new(
        handle: &'a ArrayHandle<'a, u32>,
        functions: &'a PStringFunctions,
    ) -> core::result::Result<Self, Error> {
        let ps = PStringStruct {
            data: core::ptr::null_mut::<PStringStringStruct>(),
            length: 0,
            _marker: PhantomData,
        };

        let mut pstring = PString {
            pstring: ps,
            functions,
        };

        let res = (pstring.functions.pstring_init_safe)(
            &mut pstring,
            handle.ptr as *mut PStringStringStruct,
            handle.len as u32,
        );

        match res {
            Ok(()) => Ok(pstring),
            Err(e) => Err(e),
        }
    }

    /// Append a character to the PString
    pub fn put(&mut self, character: u32) -> core::result::Result<(), Error> {
        (self.functions.pstring_put_safe)(self, character)
    }

    /// Return this string as a slice
    pub fn as_slice(&self) -> &[u32] {
        let pstr_string = self.pstring.data;
        let pstr_string_addr =
            unsafe { (core::ptr::addr_of_mut!((*pstr_string).length) as usize) + 0x04 };
        let pstr_string_data = pstr_string_addr as *const u32;

        unsafe {
            core::slice::from_raw_parts(pstr_string_data, (*pstr_string).length.try_into().unwrap())
        }
    }

    /// Return an iterator over this list
    pub fn iter(&self) -> core::slice::Iter<'_, u32> {
        self.as_slice().iter()
    }
}

/// Run the tests for this crate
pub fn run_tests(pstring_tester: &mut PStringTester) {
    tests::test_init_works(pstring_tester);
    // tests::test_init_null_pointer_fails(pstring_tester);
    tests::test_init_null_pointer_data_fails(pstring_tester);
    tests::test_init_wraps_fails(pstring_tester);
    tests::test_as_slice_works(pstring_tester);
    tests::test_put_single_character_works(pstring_tester);
    tests::test_put_two_characters_works(pstring_tester);
    tests::test_put_filled_fails(pstring_tester);
    tests::test_iter_works(pstring_tester);
}

/// Test the buffer code
#[allow(unused_imports)]
pub mod tests {
    use crate::{
        pstring::{
            Error, ErrorKind, PString, PStringFunctions, PStringStringStruct, PStringStruct,
            PStringTester,
        },
        tests::write_test_result,
        ArrayHandle,
    };
    use core::{arch::asm, fmt::Write, marker::PhantomData};

    /// Test that init works
    pub fn test_init_works(pstring_tester: &mut PStringTester) {
        let mut arr: [u32; 31] = [0; 31];
        let handle = ArrayHandle::new(arr.as_mut_ptr(), arr.len());

        // initialize the pstring
        let res = PString::new(&handle, pstring_tester.pstring.functions);

        match res {
            Ok(pstr) => {
                write_test_result(
                    pstring_tester.writer,
                    true,
                    "pstring::tests::test_init_works should initialize length to thirty one",
                );
                write_test_result(
                    pstring_tester.writer,
                    pstr.pstring.length == 31,
                    "pstring::tests::test_init_works should initialize length to thirty one",
                );
            }
            Err(_e) => {
                write_test_result(
                    pstring_tester.writer,
                    false,
                    "pstring::tests::test_init_works should initialize length to thirty one",
                );
            }
        }
    }

    // /// Test that init with a null pointer fails
    // Ugh, using PString references between here and the *_safe functions means
    // this requires some more test mocks or helpers.  Future work.
    // pub fn test_init_null_pointer_fails(pstring_tester: &mut PStringTester) {
    //     let mut arr: [u32; 31] = [0; 31];

    //     let addr = core::ptr::null_mut::<PStringStruct>();

    //     let handle = ArrayHandle::new(arr.as_mut_ptr(), arr.len());

    //     // let mut pstring = PString {
    //     //     pstring: unsafe { *addr },
    //     //     functions: pstring_tester.pstring.functions,
    //     // };

    //     // let res = (pstring_tester.pstring.functions.pstring_init_safe)(
    //     //     &mut pstring,
    //     //     handle.ptr as *mut PStringStringStruct,
    //     //     handle.len as u32,
    //     // );

    // 	let array_ptr = array_pointer as *mut u32;
    // 	let res = unsafe { jjforth_pstring_init(addr, array_ptr, length) };
    // 	match res {
    //         0 => Ok(()),
    //         1 => Err(boop::pstring::Error::new(
    // 		boop::pstring::ErrorKind::NullPointer,
    //         )),
    //         2 => Err(boop::pstring::Error::new(
    // 		boop::pstring::ErrorKind::InvalidArguments,
    //         )),
    //         _ => Err(boop::pstring::Error::new(boop::pstring::ErrorKind::Unknown)),
    // 	}

    //     match res {
    //         Ok(_) => {
    //             write_test_result(
    //                 pstring_tester.writer,
    //                 false,
    //                 "pstring::tests::test_init_null_pointer_fails init should fail",
    //             );
    //             panic!();
    //         }
    //         Err(e) => {
    //             write_test_result(
    //                 pstring_tester.writer,
    //                 e == Error::new(ErrorKind::NullPointer),
    //                 "pstring::tests::test_init_null_pointer_fails init should fail with NullPointer",
    //             );
    //             assert_eq!(e, Error::new(ErrorKind::NullPointer));
    //         }
    //     }
    // }

    /// Test that init where the buffer wraps fails
    #[allow(unused_mut)]
    pub fn test_init_null_pointer_data_fails(pstring_tester: &mut PStringTester) {
        let arr_ptr = core::ptr::null_mut::<PStringStringStruct>();

        let mut ps = PStringStruct {
            length: 0,
            data: arr_ptr,
            _marker: PhantomData,
        };

        let mut pstring = PString {
            pstring: ps,
            functions: pstring_tester.pstring.functions,
        };

        let res = (pstring_tester.pstring.functions.pstring_init_safe)(&mut pstring, arr_ptr, 0);

        match res {
            Ok(_) => {
                write_test_result(
                    pstring_tester.writer,
                    false,
                    "pstring::tests::test_init_null_pointer_data_fails init should fail",
                );
                panic!();
            }
            Err(e) => {
                write_test_result(
                    pstring_tester.writer,
                    e == Error::new(ErrorKind::NullPointer),
                    "pstring::tests::test_init_null_pointer_data_fails init should fail with NullPointer",
                );
                assert_eq!(e, Error::new(ErrorKind::NullPointer));
            }
        }
    }

    /// Test that init where the buffer wraps fails
    pub fn test_init_wraps_fails(pstring_tester: &mut PStringTester) {
        let arr_ptr = (usize::MAX - 30) as *mut u32;

        let handle = ArrayHandle {
            len: 31,
            ptr: arr_ptr,
            _marker: PhantomData,
        };
        // initialize the pstring
        let res = PString::new(&handle, pstring_tester.pstring.functions);

        match res {
            Ok(_) => {
                write_test_result(
                    pstring_tester.writer,
                    false,
                    "pstring::tests::test_init_wraps_fails init should fail",
                );
                panic!();
            }
            Err(e) => {
                write_test_result(
                    pstring_tester.writer,
                    e == Error::new(ErrorKind::InvalidArguments),
                    "pstring::tests::test_init_wraps_fails init should fail with InvalidArguments",
                );
                assert_eq!(e, Error::new(ErrorKind::InvalidArguments));
            }
        }
    }

    /// Test that putting a single character works
    pub fn test_as_slice_works(pstring_tester: &mut PStringTester) {
        let mut arr: [u32; 31] = [0; 31];
        let handle = ArrayHandle::new(arr.as_mut_ptr(), arr.len());

        // initialize the pstring
        let pstring = PString::new(&handle, pstring_tester.pstring.functions).unwrap();

        let res = pstring.as_slice();

        write_test_result(
            pstring_tester.writer,
            res.is_empty(),
            "pstring::tests::test_as_slice_works return a zero length slice",
        );
    }

    /// Test that putting a single character works
    pub fn test_put_single_character_works(pstring_tester: &mut PStringTester) {
        let mut arr: [u32; 31] = [0; 31];
        let handle = ArrayHandle::new(arr.as_mut_ptr(), arr.len());

        // initialize the pstring
        let mut pstring = PString::new(&handle, pstring_tester.pstring.functions).unwrap();

        let res = pstring.put(0x41);

        write_test_result(
            pstring_tester.writer,
            res.is_ok(),
            "pstring::tests::test_put_single_character_works put a single character should work",
        );

        write_test_result(
            pstring_tester.writer,
            unsafe { *pstring.pstring.data }.length == 1,
            "pstring::tests::test_put_single_character_works put a single character should increment length",
        );

        write_test_result(
            pstring_tester.writer,
            pstring.as_slice()[0] == 0x41,
            "pstring::tests::test_put_single_character_works put a single character should append correct character",
        );
    }

    /// Test that putting a two characters works
    pub fn test_put_two_characters_works(pstring_tester: &mut PStringTester) {
        let mut arr: [u32; 31] = [0; 31];
        arr[0] = 0x55;

        let handle = ArrayHandle::new(arr.as_mut_ptr(), arr.len());

        // initialize the pstring
        let mut pstring = PString::new(&handle, pstring_tester.pstring.functions).unwrap();

        let res = pstring.put(0x41);

        write_test_result(
            pstring_tester.writer,
            res.is_ok(),
            "pstring::tests::test_put_two_characters_works put two characters should work",
        );

        let res = pstring.put(0x43);

        write_test_result(
            pstring_tester.writer,
            res.is_ok(),
            "pstring::tests::test_put_two_characters_works put two characters should work",
        );

        write_test_result(
            pstring_tester.writer,
            unsafe { *pstring.pstring.data }.length == 2,
            "pstring::tests::test_put_two_characters_works put two characters should increment length",
        );

        write_test_result(
            pstring_tester.writer,
            pstring.as_slice()[0] == 0x41,
            "pstring::tests::test_put_two_characters_works put two characters should append correct character",
        );

        write_test_result(
            pstring_tester.writer,
            pstring.as_slice()[1] == 0x43,
            "pstring::tests::test_put_two_characters_works put two characters should append correct character",
        );
    }

    /// Test that putting in a filled string fails
    pub fn test_put_filled_fails(pstring_tester: &mut PStringTester) {
        let mut arr: [u32; 1] = [0; 1];
        arr[0] = 0x55;

        let handle = ArrayHandle::new(arr.as_mut_ptr(), arr.len());

        // initialize the pstring
        let mut pstring = PString::new(&handle, pstring_tester.pstring.functions).unwrap();

        let res = pstring.put(0x41);

        write_test_result(
            pstring_tester.writer,
            res.is_ok(),
            "pstring::tests::test_put_filled_fails put first character should work",
        );

        let res = pstring.put(0x43);

        match res {
            Ok(_) => {
                write_test_result(
                    pstring_tester.writer,
                    false,
                    "pstring::tests::test_put_filled_fails put second character should fail",
                );
                panic!();
            }
            Err(e) => {
                write_test_result(
                    pstring_tester.writer,
                    e == Error::new(ErrorKind::Full),
                    "pstring::tests::test_put_filled_fails put second character should fail with Full error",
                );
                assert_eq!(e, Error::new(ErrorKind::Full));
            }
        }
    }

    /// Test that putting a two characters works
    pub fn test_iter_works(pstring_tester: &mut PStringTester) {
        let mut arr: [u32; 31] = [0; 31];
        arr[0] = 0x55;

        let handle = ArrayHandle::new(arr.as_mut_ptr(), arr.len());

        // initialize the pstring
        let mut pstring = PString::new(&handle, pstring_tester.pstring.functions).unwrap();

        let _res = pstring.put(0x41);
        let _res = pstring.put(0x43);

        let mut i: u8 = 0;

        for val in pstring.iter() {
            if i == 0 {
                write_test_result(
                    pstring_tester.writer,
                    *val == 0x41,
                    "pstring::tests::test_iter_works first item in iteration should work",
                );
            } else if i == 1 {
                write_test_result(
                    pstring_tester.writer,
                    *val == 0x43,
                    "pstring::tests::test_iter_works second item in iteration should work",
                );
            } else {
                write_test_result(
                    pstring_tester.writer,
                    false,
                    "pstring::tests::test_iter_works shouldn't get three items",
                );
            }
            i += 1;
        }
        write_test_result(
            pstring_tester.writer,
            i == 2,
            "pstring::tests::test_iter_works should get two items",
        );
    }
}
