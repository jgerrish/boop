# stm32f1: Platform-dependent Cortex-M3 QEMU test driver

stm32f1 is a Rust project that contains the platform-dependent code
for testing the system in QEMU.  It targets a Cortex-M3 device.

Although this project targets the lm3s6965evb machine in QEMU, it uses
the stm32f1 and stm32f1xx-hal crates to build stm32f103 code.  This
was chosen because the stm32f1* crates are still in development and
have more support than the lm3s6965 Rust crate.  It doesn't use any
peripherals that are specific to the STM32F1* devices.

