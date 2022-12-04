//! Library of tests and wrappers for the Forth system

#![warn(missing_docs)]
#![no_std]

use core::fmt::Write;

pub mod buffer;
pub mod stack;

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

    /// Test initialization of the Forth system
    /// The return stack should be set to the value RETURN_STACK_BOTTOM
    /// is set to in the ELF binary sections
    pub fn test_start_works(writer: &mut dyn Write, start: fn()) {
        write!(writer, "Testing start works\r\n").unwrap();

        start();
    }
}
