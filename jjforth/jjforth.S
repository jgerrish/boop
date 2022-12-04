/* This is a Forth implementation targeting embedded ARM Cortex-M
   devices.

   It is based on jonesforth: http://annexia.org/forth by Richard
   W.M. Jones

   Like a lot of earlier Forths,
   the assembly code for jonesforth exists in a single file, jonesforth.S.
   but this code is a heavily commented tutorial on how to build a Forth
   and serves as a great introduction and tutorial.

   This port is still a work in progress and doesn't contain as much
   helpful information as jonesforth.  In the process of implementing this
   several detours were taken.  It was split into separate files for individual
   components of the Forth.  These individual components, such as the stack and
   buffer, have been used to show different features of the ARM
   programming model.
*/

.thumb
.syntax unified
.text

.text
.globl jjforth_start

/*
   Initialize the system

   Currently, this doesn't set up anything.
   The individual data structures have their own initialization
   routines.
*/
jjforth_start:
	push { r6, r7, r9, r11 }

	pop { r6, r7, r9, r11 }

	bx lr
