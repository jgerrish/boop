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
   top, bottom, length and other info.  And the register usage is not
   standardized or consistent, but should work with systems using the
   standard Rust and ARM ABIs.

   It is also not thread-safe.

   With those caveats out of the way, it can hopefully provide an
   example of using some of the unique features of ARM devices.  For
   example, the it instruction and it blocks are used to handle some
   conditionals.

   The stack after the following operations:
     push(1)
     push(2)
     push(3)

        maximum_top                 top           bottom
          |                          |               |
          V                          V               V
        +------------------------------------------+
        |   |   |     unused       |   | 3 | 2 | 1 |
        +------------------------------------------+
   addr:  0   1              .....          n-2 n-1  n


   The bottom points to the memory location one beyond the end of the array.
   The init function checks to make sure this is a valid memory
   location, even though it isn't read or written to.
*/

.thumb
.syntax unified
.text

.globl jjforth_stack_init, jjforth_stack_push, jjforth_stack_pop

/* The size of the data elements we store in bits */
.set STACK_DATA_SIZE, 32
/* How much to shift the length to get the data element size */
.set STACK_SHIFT_AMOUNT, 2

/*
 * The fields of the stack data structure
 *
 */
.set STACK_LEN_OFFSET, 0
.set STACK_BOTTOM_OFFSET, 4
.set STACK_TOP_OFFSET, 8
.set STACK_MAXIMUM_TOP_OFFSET, 12

sptr .req r4			/* Stack structure address */

/* Push a value onto the stack
 *
 * This function pushes a value onto the stack
 *
 * Parameters:
 *   parameter 1: A pointer to the stack structure
 *   parameter 2: The value to push
 *
 * Returns:
 *   0 on success
 *   1 if the stack structure pointer is a null pointer.
 *   2 if the there was a stack overflow (stack was full)
 */
jjforth_stack_push:
	push { r3, sptr, r5, r6, r7, r9, r11 }

	/* Check for null pointer */
	cmp r0, #0
	beq jjforth_stack_push_null_pointer

	mov sptr, r0

     	/* Load the value of the jjforth_stack_bottom variable */
	ldr r3, [sptr, #STACK_MAXIMUM_TOP_OFFSET]
	/* ldr r2, [r3] */

	/* See if the current stack top is at the maximum top */
	ldr r5, [sptr, #STACK_TOP_OFFSET]
	cmp r5, r3

	/*
	mov r0, r1
	b jjforth_stack_push_end
	*/

#ifdef IT_INSTRUCTION
	ittte ne
	/* If pushing wouldn't result in a stack overflow */
	stmfdne r5!, {r1} 	/* save register onto the stack */
	strne r5, [sptr, #STACK_TOP_OFFSET]
	movne r0, #0		/* set the error return code to 0 */
	/* If pushing would result in a stack overflow */
	moveq r0, #2		/* set the error return code to 2 */
	b jjforth_stack_push_end
#else
	beq jjforth_stack_push_stack_overflow
	stmfd r5!, {r1}         /* save register onto the stack */
	mov r0, #0	        /* set the error return code to 0 */
	b jjforth_stack_push_success

jjforth_stack_push_stack_overflow:
	mov r0, #2		/* set the error return code to 2 */
	b jjforth_stack_push_end
#endif


jjforth_stack_push_success:
	/* Save the updated stack top */
	str r5, [sptr, #STACK_TOP_OFFSET]
	/* mov r0, #0 */
	b jjforth_stack_push_end

jjforth_stack_push_null_pointer:
	mov r0, #1
	b jjforth_stack_push_end

jjforth_stack_push_end:
	pop { r3, sptr, r5, r6, r7, r9, r11 }

	bx lr


/* Pop a value from the stack
 *
 * This function pops the most recent value pushed.
 *
 * Parameters:
 *   parameter 1: A pointer to the stack structure
 *   parameter 2: A pointer to the location to store the result in
 *
 * Returns:
 *   0 on success
 *   1 if the stack structure pointer is a null pointer.
 *   2 if the there was a stack underflow (no items available)
 *
 * The actual popped value is stored in the variable parameter two
 * points to.
 */
jjforth_stack_pop:
	push { r3, sptr, r5, r6, r7, r9, r11 }

	/* Check for null pointer */
	cmp r0, #0
	beq jjforth_stack_pop_null_pointer

	mov sptr, r0

	mov r3, r1		/* Copy the popped data reference to r3 */
	ldr r2, [sptr, #STACK_BOTTOM_OFFSET]

	/* See if the current stack top is at the maximum top */
	ldr r5, [sptr, #STACK_TOP_OFFSET]
	cmp r5, r2

#ifdef IT_INSTRUCTION
	ittee ne
	ldmfdne r5!, {r1}       /* restore register from stack */
	movne r0, #0
	/* If there is a stack underflow */
	moveq r0, #2		/* set the return to some known value */
	moveq r1, #2		/* set the saved first parameter to 1 */
	b jjforth_stack_pop_success
#else
	beq jjforth_stack_pop_stack_underflow
	ldmfd r5!, {r1}         /* restore register from stack */
	mov r0, #0		/* set the saved first parameter to 0 */
	b jjforth_stack_pop_success
jjforth_stack_pop_stack_underflow:
	mov r0, #2		/* set the return to some known value */
	mov r1, #2		/* set the saved first parameter to 1 */
	b jjforth_stack_pop_end
#endif

jjforth_stack_pop_success:
	/* Save the updated stack top */
	str r5, [sptr, #STACK_TOP_OFFSET]

	b jjforth_stack_pop_end

jjforth_stack_pop_null_pointer:
	mov r0, #1
	b jjforth_stack_pop_end

jjforth_stack_pop_end:
	str r1, [r3]		/* store to saved first parameter */

	pop { r3, sptr, r5, r6, r7, r9, r11 }

	bx lr

/* Initialize the stack
 *
 * This function initializes the stack structure.
 *
 * Parameters:
 *   parameter 1: A pointer to the stack structure
 *   parameter 2: A pointer to the array to store the stack data in
 *   parameter 3: The length of the array, in terms of the number of elements it can hold.
 *
 * Returns:
 *   0 on success
 *   1 if the stack structure pointer is a null pointer.
 *   2 if the arguments are invalid (they go beyond the memory limits, etc.)
 */
jjforth_stack_init:
	push { r3, sptr, r5, r6, r7, r9, r11 }

	/* Check for null pointer */
	cmp r0, #0
	beq jjforth_stack_init_null_pointer

	/* Check if length > 0 */
	cmp r2, #0
	bls jjforth_stack_init_invalid_arguments

	/* Multiply the stack length by the data element size */
	lsls r5, r2, #STACK_SHIFT_AMOUNT
	bcs jjforth_stack_init_invalid_arguments

	/* The following check is to make sure we can use the byte after
	   the array as a sentinel */
	/* Check if start of stack memory + size carries */
	adds r3, r1, r5
	bhs jjforth_stack_init_invalid_arguments

	mov sptr, r0

	str r1, [sptr, #STACK_MAXIMUM_TOP_OFFSET]
	str r3, [sptr, #STACK_TOP_OFFSET]
	str r3, [sptr, #STACK_BOTTOM_OFFSET]
	str r2, [sptr, #STACK_LEN_OFFSET]

	b jjforth_stack_init_success

jjforth_stack_init_success:
	mov r0, #0
	b jjforth_stack_init_end

jjforth_stack_init_null_pointer:
	mov r0, #1
	b jjforth_stack_init_end

jjforth_stack_init_invalid_arguments:
	mov r0, #2
	b jjforth_stack_init_end

jjforth_stack_init_end:
	pop { r3, sptr, r5, r6, r7, r9, r11 }

	bx lr


.globl jjforth_get_stack_bottom

/* Get the address of the bottom of the stack
 *
 * This function returns the address of the bottom of the stack
 *
 * Parameters:
 *   parameter 1: A pointer to the stack structure]
 *   parameter 2: A pointer to the location to store the result in
 *
 * Returns:
 *   0 on success
 *   1 if the stack structure pointer is a null pointer.
 *
 * The actual address is stored in the parameter 2 variable.
 *
 */
jjforth_get_stack_bottom:
	push { r2, sptr, r6, r7, r9, r11 }

	/* Check for null pointer */
	cmp r0, #0
	beq jjforth_stack_get_stack_bottom_null_pointer

	mov sptr, r0

	ldr r2, [sptr, #STACK_BOTTOM_OFFSET]
	str r2, [r1]

	b jjforth_stack_get_stack_bottom_success

jjforth_stack_get_stack_bottom_null_pointer:
	mov r0, #1
	b jjforth_stack_get_stack_bottom_end

jjforth_stack_get_stack_bottom_success:
	mov r0, #0
	b jjforth_stack_get_stack_bottom_end

jjforth_stack_get_stack_bottom_end:
	pop { r2, sptr, r6, r7, r9, r11 }

	bx lr
