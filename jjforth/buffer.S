/* ARM Cortex-M 32-bit Thumb2 buffer / queue implementation

   The buffer is implemented as a queue.

   The number of items that can be stored is one less than the (data
   type) size of the queue.

   For example, if the size allocated below is 1024, and the data
   type size is four bytes, then the number of items that can be
   stored is 255, not 256.

   It was built as part of a project to investigate Forth on
   embedded devices.

   Safety
   ------

   It has not been fully tested.  Don't use it in production code.

   It's also not optimized.  It uses a set of globals to store queue
   start, end and current positions. instead of using parameters or
   registers to manage that information for the lifetime of the
   application.  And the register usage is not standardized or
   consistent, but should work with systems using the standard ARM
   ABI.

   It is also not thread-safe.  Some protections have been added,
   but there are still edge-cases and it hasn't been fully reviewed.

   With those caveats out of the way, it can hopefully provide an
   example of using some of the unique features of ARM devices.  For
   example, the it instruction and it blocks are used to handle some
   conditionals.
*/

.thumb
.syntax unified
.text

.globl jjforth_buffer_init, jjforth_buffer_clear, jjforth_buffer_write, jjforth_buffer_read, jjforth_buffer_reset

/* Initialize the buffer */

/* Because jjforth_buffer_start, jjforth_buffer_end, bufftop and currkey all depend on
   each other, we need to have a higher-level mutex that coordinates
   access to all of them.
*/
jjforth_buffer_init:
	/* We use Rust as a test driver and for easy access to runtime
	   services such as pin and peripheral configuration.  Rust
	   reserves some registers for use and they cannot be used by FFI
	   routines without saving and restoring them on subroutine entry
	   and exit.

           Inline assembly - The Rust Reference (localhost)
           http://localhost:3000/inline-assembly.html

           Inline assembly - The Rust Reference
           https://doc.rust-lang.org/reference/inline-assembly.html
        */
	push { r6, r7, r9, r11 }

	ldr r3, =jjforth_buffer_start
	str r0, [r3]

	ldr r3, =currkey
	str r0, [r3]
	ldr r3, =bufftop
	str r0, [r3]
	ldr r3, =jjforth_buffer_end
	str r1, [r3]

	pop { r6, r7, r9, r11 }

	bx lr

/* Write data to the buffer or add an item to the queue.

   The first parameter should be the item to add.

   On success, returns a zero.  On failure returns a non-zero value.
*/
jjforth_buffer_write:
	push { r6, r7, r9, r11 }

	ldr r9, =bufftop

	ldr r1, [r9]
	mov r12, r1		/* copy bufftop into r12, to use for storing the data */
	ldr r3, =jjforth_buffer_end
	ldr r2, [r3]

	/* Check if bufftop is equal to jjforth_buffer_end */
	cmp r1, r2
	/* Here's the condition code using an it instruction block */
#ifdef IT_INSTRUCTION
	iteee ne
	/* If we don't need to wrap */
	addne r1, r1, #4
	/* If we need to wrap */
	ldreq r3, =jjforth_buffer_start
	ldreq r8, [r3]
	moveq r1, r8
	/* End of it instruction block */
#else
	/* here's the code using normal branch instructions */
	beq jjforth_buffer_write_wrap
        add r1, r1, #4
	b jjforth_buffer_write_check
jjforth_buffer_write_wrap:
	ldr r3, =jjforth_buffer_start
	ldr r8, [r3]
	mov r1, r8
	b jjforth_buffer_write_check
	/* End of comparison and branch block */
#endif

jjforth_buffer_write_check:
	ldr r10, =currkey
	ldr r11, [r10]
	cmp r1, r11

#ifdef IT_INSTRUCTION
	ittte ne
	/* If there is space available */
	strne r0, [r12]		/* Write the data to the saved bufftop index */
	strne r1, [r9]		/* update the bufftop with the new bufftop index  */
	movne r0, #0		/* return a successful status code */
	/* If there is not space available */
	moveq r0, #1		/* return an unsuccessful status code */
#else
	beq jjforth_buffer_write_full
	/* We're safe to write the data */
	str r0, [r12]           /* Write the data to the saved bufftop index */
	str r1, [r9]	/* update the bufftop with the new bufftop index  */

	mov r0, #0		/* return a successful status code */
	b jjforth_buffer_write_done
jjforth_buffer_write_full:
	mov r0, #1		/* return an unsuccessful status code */
	b jjforth_buffer_write_done
#endif

jjforth_buffer_write_done:
	pop { r6, r7, r9, r11 }

	bx lr

/*
   Read data from the buffer

   This function expects a single argument, a reference to a buffer to
   fill with the data read.  It returns an error code indicating whether
   the read was successful.  On success, a zero is returned and the
   referenced variable contains valid data.  On failure, a non-zero value
   is returned and the data is unspecified.  (It's set to one in this
   version, but that is subject to change).
*/
jjforth_buffer_read:
	push { r6, r7, r9, r11 }

	ldr r9, =jjforth_buffer_start
	ldr r10, [r9]
	ldr r9, =jjforth_buffer_end
	ldr r1, [r9]
	ldr r11, =currkey

	ldr r2, [r11]
	ldr r9, =bufftop
	ldr r3, [r9]

	/* Check if there's any data to read.
           If currkey != bufftop, there is data to read.
        */
	cmp r2, r3
	beq jjforth_buffer_read_empty

	mov r10, #0
	str r10, [r0] 		/* store to first parameter */
	ldr r0, [r2]		/* read the next piece of data */

	/* Advance the current key pointer */
	cmp r9, r3		/* Check if currkey is at the end of the buffer */

#ifdef IT_INSTRUCTION
	ittee ne
	/* If we don't need to wrap to the beginning of the buffer */
	addne r2, r2, #4
	strne r2, [r11]
	/* If we need to wrap to the beginning of the buffer */
	moveq r2, r10
	streq r2, [r11]
	/* End of it block */
	b jjforth_buffer_read_done
#else
	beq jjforth_buffer_read_wrap
	add r2, r2, #4
	str r2, [r11]

	b jjforth_buffer_read_done
#endif

jjforth_buffer_read_wrap:
	mov r2, r10
	str r2, [r11]

	b jjforth_buffer_read_done

jjforth_buffer_read_empty:
	mov r10, #1
	str r10, [r0] 		/* store to first parameter */
	mov r0, r10		/* set the return value */
	b jjforth_buffer_read_done

jjforth_buffer_read_done:
	mov r11, #1		/* It doesn't matter what we store in currkey,
                                   so just use 1. */
	pop { r6, r7, r9, r11 }

	bx lr


/* Clear the buffer */
jjforth_buffer_clear:
	push { r6, r7, r9, r11 }
	ldr r3, =jjforth_buffer_start	/* Load the address of the jjforth_buffer_start variable */
	ldr r0, [r3]		/* Load the start of the buffer in general register r0 */

	/* reset the read pointer to the beginning of the buffer */
	ldr r3, =currkey
	str r0, [r3]

	/* reset the write pointer to the beginning of the buffer */
	ldr r3, =bufftop
	str r0, [r3]

	mov r11, #1		/* It doesn't matter what we store in jjforth_buffer_lock,
                                   so just use 1. */
	pop { r6, r7, r9, r11 }

	bx lr


/* Start of data section and variables */
	.data
	.align 4


/* This data should be protected if locks are implemented
   The locations of these variables should be safe through the
   lifetime of the program.

   The contents of the jjforth_buffer_start and jjforth_buffer_end may change when the
   queue is initialized, but not by read or write.

   The contents of currkey and bufftop change when the buffer is
   initialized.  bufftop is changed when the buffer is written.
   currkey is changed when the buffer is read.  They are both changed
   when the buffer is cleared.
*/
jjforth_buffer_start:
	.int 0
jjforth_buffer_end:
	.int 0
currkey:
	.int 0 			/* Current place in input buffer (next character to read). */
bufftop:
	.int 0		        /* Last valid data in input buffer + 1. */

