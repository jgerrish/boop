# boop

Test Forth compiler and interpreter for ARM Cortex-M systems.

Targeting older M3 systems.  These systems have the broadest support
in QEMU and the ecosystem.

This provides a basic wrapper for a Forth compiler.

After getting the basics working, work will be done to replace the
system with a native assembly version.

# Safety

This is meant as an example of implementing a Forth system on a
Cortex-M device.

It has not been fully tested.  Don't use it in production code.

It's also not optimized.  It uses a lot of globals to store stack and
queue settings instead of using parameters or registers to manage that
information for the lifetime of the application.  And the register
usage is not standardized or consistent, but should work with systems
using the standard ARM ABI.  It's been tested with Rust as a test
driver.

It is also not thread-safe and assumes memory models for current
generation (ARMv7-M) Cortex-M devices.

With those caveats out of the way, it can hopefully provide an
example of using some of the unique features of ARM devices.  For
example, the it instruction and it blocks are used to handle some
conditionals.

# Data Structures

This crate provides a simple implementation of a stack and queue /
buffer on ARM Cortex-M devices.

