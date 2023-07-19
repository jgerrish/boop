//! Library of tests and wrappers for the Forth system

#![warn(missing_docs)]
#![no_std]

use core::{
    fmt::{Debug, Formatter, Write},
    marker::PhantomData,
};

pub mod buffer;
pub mod dict;
pub mod error;
pub mod stack;

/// A handle to an array to manage lifetimes and concurrency
pub struct ArrayHandle<'a, T> {
    /// Pointer to the array
    pub ptr: *mut T,
    /// Length of the array
    pub len: usize,
    /// We want to have a lifetime on an ArrayHandle tied to the data
    pub _marker: PhantomData<&'a T>,
}

impl<'a, T> ArrayHandle<'a, T> {
    /// Create a new ArrayHandle from an array pointer and length.
    ///
    /// # Examples
    /// ```
    /// use crate::ArrayHandle;
    ///
    /// let mut arr: [u32; 4] = [0; 4];
    /// let _handle = ArrayHandle::new(arr.as_mut_ptr(), arr.len());
    /// ```
    pub fn new(ptr: *mut T, len: usize) -> Self {
        ArrayHandle {
            ptr,
            len,
            _marker: PhantomData,
        }
    }
}

impl<'a, T> Debug for ArrayHandle<'a, T> {
    fn fmt(&self, f: &mut Formatter) -> core::fmt::Result {
        write!(f, "ptr: 0x{:X}", self.ptr as usize)?;
        write!(f, ", len: 0x{:X}", self.len)
    }
}

/// Initialization of the boop system
pub fn init(writer: &mut dyn Write, mpu: &cortex_m::peripheral::MPU) {
    configure_mpu(writer, mpu);
}

/// Configure memory regions
pub fn configure_mpu(writer: &mut dyn Write, mpu: &cortex_m::peripheral::MPU) {
    let t = mpu._type.read();

    write!(writer, "MPU_TYPE settings: 0x{:X}\r\n", t).unwrap();
    write!(writer, "MPU_TYPE DREGION: 0x{:02X}\r\n", (t & 0xFF00) >> 8).unwrap();

    // After memory layout options are decided, we can setup the MPU here

    // let cur_mpu_ctrl =  mpu.ctrl.read();
    // write!(writer, "MPU_CTRL settings: 0x{:X}\r\n", cur_mpu_ctrl);

    // const MPU_ENABLE: u32 = 0x01;
    // const MPU_DEFAULT_MMAP_FOR_PRIVILEGED: u32 = 0x04;

    // unsafe {
    //     mpu.ctrl.modify(|r| r | MPU_DEFAULT_MMAP_FOR_PRIVILEGED | MPU_ENABLE);
    // }

    // let cur_mpu_ctrl =  mpu.ctrl.read();
    // write!(writer, "MPU_CTRL settings: 0x{:X}\r\n", cur_mpu_ctrl);
}

/// Test module for the top-level Forth system
#[allow(unused_imports)]
pub mod tests {
    use core::{arch::asm, fmt::Write};

    /// Write a test result status and message about the test
    ///
    /// writer is the trait object to write to
    /// test_result is the result of the test:
    ///   if it was true the test was successful
    ///   if it was false the test was a failure
    /// status_msg is a string describing the test
    pub fn write_test_result(writer: &mut dyn Write, test_result: bool, status_msg: &str) {
        if test_result {
            write!(writer, "SUCCESS").unwrap();
        } else {
            write!(writer, "FAILURE").unwrap();
        }
        write!(writer, " {}\r\n", status_msg).unwrap();
    }

    /// Test initialization of the Forth system
    /// The return stack should be set to the value RETURN_STACK_BOTTOM
    /// is set to in the ELF binary sections
    pub fn test_start_works(writer: &mut dyn Write, start: fn()) {
        write!(writer, "Testing start works\r\n").unwrap();

        start();
    }
}
