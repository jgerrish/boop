/* ARM Cortex-M 32-bit Thumb2 stack implementation
   A simple implementation of a stack on ARM Cortex-M devices.

   This is meant as an example of implementing a stack on a Cortex-M
   device.

   It was built as part of a project to investigate Forth on
   embedded devices.

   Safety
   ------

   It has not been fully tested.  Don't use it in production code.

   It's also not optimized.  It uses a set of globals to store stack
   top, bottom, size and other info.  And the register usage is not
   standardized or consistent, but should work with systems using the
   standard Rust and ARM ABIs.

   It is also not thread-safe.

   With those caveats out of the way, it can hopefully provide an
   example of using some of the unique features of ARM devices.  For
   example, the it instruction and it blocks are used to handle some
   conditionals.
*/

.thumb
.syntax unified
.text

.globl jjforth_stack_init, jjforth_stack_push, jjforth_stack_pop


/* Store the value onto the stack and decrement the stack register
   r5 afterwards
   On success zero is returned.
   On failure, one is returned.
*/
/*
   This used to be a macro with two macro arguments, register and
   jjforth_stack_bottom variable.  The register argument can be replaced at
   the call site with an additional mov.

   To support both a return and parameter stack, there will need to be
   code duplication or switching from using registers to a variable.
*/
jjforth_stack_push:
	push { r6, r7, r9, r11 }

	ldr r2, =jjforth_stack_bottom /* Load the address of the jjforth_stack_bottom variable */
	ldr r1, [r2]     	     /* Load the value of the jjforth_stack_bottom variable */

	cmp r5, r1

#ifdef IT_INSTRUCTION
	itte ne
	/* If pushing wouldn't result in a stack overflow */
	stmfdne r5!, {r0} 	/* save register onto the stack */
	movne r0, #0		/* set the error return code to 0 */
	/* If pushing would result in a stack overflow */
	moveq r0, #1		/* set the error return code to 1 */
	b jjforth_stack_push_end
#else
	beq jjforth_stack_push_stack_overflow
	stmfd r5!, {r0}         /* save register onto the stack */
	mov r0, #0	        /* set the error return code to 0 */
	b jjforth_stack_push_end
jjforth_stack_push_stack_overflow:
	mov r0, #1		/* set the error return code to 1 */
	b jjforth_stack_push_end
#endif

jjforth_stack_push_end:
	pop { r6, r7, r9, r11 }

	bx lr


/* Load the value on the stack into the register, and increment the
   stack register r5 afterwards */

/* The API of this is: pop expects a single parameter,
   the address of the data to return.  It changes the value of this parameter
   and returns a value of zero if it was successful.

   If there is an error trying to pop a value, the parameter value is
   undefined and the error code is set to a non-zero value.

   It doesn't use a global errno like classic UNIX and POSIX
   applications.
*/
jjforth_stack_pop:
	push { r6, r7, r9, r11 }

	mov r3, r0		/* Copy the popped data reference to r3 */
	ldr r2, =jjforth_stack_maximum_top
	ldr r1, [r2]
	cmp r5, r1

#ifdef IT_INSTRUCTION
	ittee ne
	ldmfdne r5!, {r1}       /* restore register from stack */
	movne r0, #0
	/* If there is a stack underflow */
	moveq r0, #1		/* set the return to some known value */
	moveq r1, #1		/* set the saved first parameter to 1 */
	b jjforth_stack_pop_end
#else
	beq jjforth_stack_pop_stack_underflow
	ldmfd r5!, {r1}         /* restore register from stack */
	mov r0, #0		/* set the saved first parameter to 0 */
	b jjforth_stack_pop_end
jjforth_stack_pop_stack_underflow:
	mov r0, #1		/* set the return to some known value */
	mov r1, #1		/* set the saved first parameter to 1 */
	b jjforth_stack_pop_end
#endif

jjforth_stack_pop_end:
	str r1, [r3]		/* store to saved first parameter */

	pop { r6, r7, r9, r11 }

	bx lr


jjforth_stack_init:
	push { r6, r7, r9, r11 }

	mov r5, r0

	ldr r3, =jjforth_stack_maximum_top
	str r0, [r3]

	ldr r3, =jjforth_stack_bottom
	str r1, [r3]

	pop { r6, r7, r9, r11 }

	bx lr


.globl jjforth_get_stack_bottom

jjforth_get_stack_bottom:
	push { r6, r7, r9, r11 }

	ldr r2, =jjforth_stack_bottom
	ldr r0, [r2]

	pop { r6, r7, r9, r11 }

	bx lr

.data
.align 4


/* This data should be protected if concurrency-safe behavior is
   desired.
   The locations of these variables should be safe through
   the lifetime of the program.
   The contents of the variables may change when the stacks
   are initialized, but not by pop or push.
*/

/* FORTH return stack metadata */
jjforth_stack_size:
	.int 0
jjforth_stack_bottom:
	.int 0
jjforth_stack_maximum_top:
	.int 0

