# boop

Forth data structures for embedded devices

This is a set of projects to build a Forth system targeting Cortex-M
devices.  It isn't meant to be a production ready project, but mostly
an exploration of the Rust embedded ecosystem and ARM devices.

Right now, it includes a basic stack and queue / buffer.  The stack
and queue include different compilation-time codepaths for using IT
instruction blocks.  Future versions may include concurrency features.

It includes assembly code and a set of Rust crates to test the code.
One crate is used for testing QEMU emulated Cortex-M3 devices.  These
systems have the broadest support in QEMU and the ecosystem.

It is based on jonesforth: http://annexia.org/forth by Richard
W.M. Jones

Like a lot of earlier Forths, the assembly code for jonesforth exists
in a single file, jonesforth.S.  jonesforth is a heavily commented
tutorial on how to build a Forth and serves as a great introduction
and tutorial.

This port is still a work in progress and doesn't contain as much
helpful information as jonesforth.  In the process of implementing
this several detours were taken.  It was split into separate files for
individual components of the Forth.  These individual components, such
as the stack and buffer, have been used to explore different features
of processor architectures and concurrency models.


# Installation

## Prerequisites

arm-none-eabi-ar
arm-none-eabi-as
arm-none-eabi-gcc
gdb-multiarch
rust
qemu
make

On Debian-based systems:

apt-get install make binutils-arm-none-eabi gcc-arm-none-eabi gdb-multiarch qemu-system-arm


Details are included below:

### Build toolchains

GMU make, available as make on Debian-based systems

arm GCC toolchain:

binutils-arm-none-eabi
gcc-arm-none-eabi
gdb-multiarch

### Rust install

Installing Rust dependencies:

Rust requires the thumbv7m-none-eabi target:

rustup target add thumbv7m-none-eabi

### QEMU install

To test this code, QEMU is suggested.  The name of the binary is:

qemu-system-arm

The name of the package on Debian based systems is:

qemu-system-arm

## Building

In the top-level directory:

make

## Running

Run make in the top-level directory.  Then change into stm32f1 and run:

make run

To see the test / debug output.

# Architecture

The project is split into several main components.

## jjforth: Forth compiler

The main code for the Forth compiler is in the jjforth subdirectory.
This is written in ARM assembly using GNU assembler syntax.

## boop: Platform-independent test and support Rust library

The boop subdirectory contains a Rust project with
platform-independent test and support code for the Forth compiler.  It
uses a lot of function callbacks and other patterns to allow
platform-dependent code to use it.

# stm32f1: Platform-dependent Cortex-M3 QEMU test driver

stm32f1 is a Rust project that contains the platform-dependent code
for testing the system in QEMU.  It targets a Cortex-M3 device.

Although this project targets the lm3s6965evb machine in QEMU, it uses
the stm32f1 and stm32f1xx-hal crates to build stm32f103 code.  This
was chosen because the stm32f1* crates are still in development and
have more support than the lm3s6965 Rust crate.  It doesn't use any
peripherals that are specific to the STM32F1* devices.

The wrapper functions in the platform-dependent crates incorporate
some higher-level design decisions that could be abstracted to the
boop crate.  For example, pop_rsp_safe converts C-style error returns
to Rust-style Results.  This could be moved into the boop crate.

# Safety

This is meant as an example of implementing a Forth system on a
Cortex-M device.

It has not been fully tested.  Don't use it in production code.

It's also not optimized.  It uses a combination of registers and
globals to store stack and queue settings instead of using parameters
or registers alone to manage that information for the lifetime of the
application.  And the register usage is not standardized or
consistent, but should work with systems using the standard ARM ABI.
It's been tested with Rust as a test driver.

It is also not thread-safe and assumes memory models for current
generation (ARMv7-M) Cortex-M devices.

With those caveats out of the way, it can hopefully provide an
example of using some of the unique features of ARM devices.  For
example, the it instruction and it blocks are used to handle some
conditionals.

# Data Structures

This crate provides a simple implementation of a stack and queue /
buffer on ARM Cortex-M devices.

# References

[jonesforth](http://annexia.org/forth "Jonesforth Forth compiler")
Very well documented Forth system written in x86 assembly.  Serves as
a great introduction to building a Forth system.

[inline-assembly](https://doc.rust-lang.org/reference/inline-assembly.html "Inline assembly - The Rust Reference")
Documentation on the Rust ABI and what registers get clobbered.

[arm-abi](https://developer.arm.com/Architectures/Application%20Binary%20Interface "ABI - Arm Developer")
Documentation on the ABI for Arm.

[oldnewthing](https://devblogs.microsoft.com/oldnewthing/20210601-00/ "The ARM processor (Thumb-2), part 2: Differences between classic ARM and Thumb-2")
Discussion of using the IT instruction.

[BinarioBoard](https://github.com/ElectronicToast/BinarioBoard "BinarioBoard")
A project by Ray Sun from Caltech EE10b, with excellent examples of assembly comments.
Some of the comments in this project are based on best practices there.
