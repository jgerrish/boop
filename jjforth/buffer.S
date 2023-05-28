/* ARM Cortex-M 32-bit Thumb2 buffer / queue implementation

   The buffer is implemented as a queue.

   The number of items that can be stored is one less than the (data
   type) size of the queue.

   For example, if the size allocated below is 1024, and the data
   type size is four bytes, then the number of items that can be
   stored is 255, not 256.

   It was built as part of a project to investigate Forth on
   embedded devices.

   Diagram of the buffer (inspired by the jonesforth Forth compiler)
   after putting five 32-bit width characters: H e l l o.
   Each of these elements holds 32-bits.

        start                                   end
          |                                      |
          V                                      V
        +------------------------------------------+
        | H | e | l | l | o |   | .. unused .. | X |
        +------------------------------------------+
	  ^                   ^
	  |                   |
       currkey             bufftop

   Previous documentation indicated that the last element (where end
   is pointing) couldn't be modified.  This was confusing.
   The "reserved" location depends on the location of currkey.  It
   is the location right "before" currkey.  In this case, it is where
   end points.  But as currkey moves, it changes:

        start                                    end
          |                                       |
          V                                       V
        +-------------------------------------------+
        | HX | e | l | l | o |   | .. unused .. |   |
        +-------------------------------------------+
	       ^               ^
	       |               |
            currkey         bufftop

   Here HX indicates that the current implementation doesn't
   clear H after a successful get, and the X indicates that
   this location won't be written with currkey's current value.
   Where end points is now a valid location to put an item.

   To help increase code readability and decrease cognitive load, the
   same registers were used throughout this file to refer to each
   queue pointer.

   First, r4 is used as the first argument, the pointer to the
   queue structure.  It is aliased as rbuf.

   The following registers are used as pointers:

   r7:  start
   r8:  end
   r9:  currkey
   r10: bufftop

   Safety
   ------

   It has not been fully tested.  Don't use it in production code.

   The register usage should work with systems using the standard
   GCC ARM ABI.

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

.globl jjforth_buffer_init, jjforth_buffer_clear, jjforth_buffer_write, jjforth_buffer_read, jjforth_buffer_size


/*
 * The following offsets describe the fields of the buffer structure
 * passed into the buffer functions.
 */

/* The start of the buffer, a pointer to the first address */
.set BUFFER_START_OFFSET, 0
/* The end of the buffer, a pointer to the last address */
.set BUFFER_END_OFFSET, 4
/*
 * The currkey points to the next object to read.  It's equivalent to
 * the head in some queue definitions.
 */
.set BUFFER_CURRKEY_OFFSET, 8
/*
 * The bufftop points to the address to write the next object.  It's equivalent to
 * the tail in some queue definitions.
 */
.set BUFFER_BUFFTOP_OFFSET, 12

/* Set up some register aliases to hopefully make this code easier to follow and
   be more consistent in register usage. */
rbuf .req r4			/* Buffer structure address */

/* Initialize the buffer

   This should be the first subroutine you call before using the
   buffer.

   The buffer expects an array or contiguous region of memory to store
   items in.  Most of the functions take a structure with pointers
   into this array as the first argument.  The structure looks like
   this in C:

   struct buffer {
     uint32_t *start,
     uint32_t *end,
     uint32_t *currkey,
     uint32_t *bufftop
   };

   The first parameter is a pointer to a buffer structure.

   Return:
     0 for success.
     1 for a null pointer exception
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
	push { r1, r2, rbuf, r6, r7, r9, r11 }

	cmp r0, #0
	beq jjforth_buffer_init_null_pointer

	mov rbuf, r0		/* Copy the structure pointer to rbuf */

	str r1, [rbuf, #BUFFER_START_OFFSET]
	str r2, [rbuf, #BUFFER_END_OFFSET]
	str r1, [rbuf, #BUFFER_CURRKEY_OFFSET]
	str r1, [rbuf, #BUFFER_BUFFTOP_OFFSET]

	mov r0, #0
	b jjforth_buffer_init_done

jjforth_buffer_init_null_pointer:
	mov r0, #1
	b jjforth_buffer_init_done

jjforth_buffer_init_done:
	pop { r1, r2, rbuf, r6, r7, r9, r11 }

	bx lr

/* Write data to the buffer or add an item to the queue.

   The first parameter is a pointer to the buffer structure.
   The second parameter should be the item to add. as a 32-bit word.

   On success, returns a zero.  On failure returns a non-zero value.

   Return:
     0 for success.
     1 for a null pointer exception
     2 if the buffer is full.
*/
jjforth_buffer_write:
	push { rbuf, r6, r7, r8, r9, r10, r11, r12 }

	cmp r0, #0
	beq jjforth_buffer_write_null_pointer

	mov rbuf, r0
	ldr r10, [rbuf, #BUFFER_BUFFTOP_OFFSET]
	mov r12, r10		/* copy bufftop into r12, to use for storing the data */
	ldr r8, [rbuf, #BUFFER_END_OFFSET]

	/* Check if bufftop is equal to jjforth_buffer_end */
	cmp r10, r8
	/* Here's the condition code using an it instruction block */
#ifdef IT_INSTRUCTION
	itee ne
	/* If we don't need to wrap */
	addne r10, r10, #4
	/* If we need to wrap */
	ldreq r7, [rbuf, #BUFFER_START_OFFSET]
	moveq r10, r7
	/* End of it instruction block */
#else
	/* here's the code using normal branch instructions */
	beq jjforth_buffer_write_wrap
        add r10, r10, #4
	b jjforth_buffer_write_check
jjforth_buffer_write_wrap:
	ldr r7, [rbuf, #BUFFER_START_OFFSET]
	mov r10, r7
	b jjforth_buffer_write_check
	/* End of comparison and branch block */
#endif

jjforth_buffer_write_check:
	ldr r9, [rbuf, #BUFFER_CURRKEY_OFFSET]
	cmp r10, r9

#ifdef IT_INSTRUCTION
	ittte ne
	/* If there is space available */
	strne r1, [r12]		/* Write the data to the saved bufftop index */
	/* update the bufftop with the new bufftop index  */
	strne r10, [rbuf, #BUFFER_BUFFTOP_OFFSET]
	movne r0, #0		/* return a successful status code */
	/* If there is not space available */
	moveq r0, #2		/* return an unsuccessful status code */
#else
	beq jjforth_buffer_write_full
	/* We're safe to write the data */
	str r1, [r12]           /* Write the data to the saved bufftop index */
	/* update the bufftop with the new bufftop index  */
	str r10, [rbuf, #BUFFER_BUFFTOP_OFFSET]

	mov r0, #0		/* return a successful status code */
	b jjforth_buffer_write_done
jjforth_buffer_write_full:
	mov r0, #2		/* return an unsuccessful status code */
	b jjforth_buffer_write_done
#endif
	/* For the IT instruction block */
	b jjforth_buffer_write_done

jjforth_buffer_write_null_pointer:
	mov r0, #1
	b jjforth_buffer_write_done

jjforth_buffer_write_done:
	pop { rbuf, r6, r7, r8, r9, r10, r11, r12 }

	bx lr

/* Read data from the buffer

   This function expects two arguments.
   The first parameter is a pointer to the buffer structure.
   The second parameter should be a reference to a result code to
   fill with the result of the read.

   Error code:
    0 for success
    1 for a null pointer exception
    2 if the buffer is empty

   Return:
     It returns the data read if it was successful, as a 32-bit word.
*/
jjforth_buffer_read:
	push { rbuf, r6, r7, r8, r9, r10, r11 }

	cmp r0, #0
	beq jjforth_buffer_read_null_pointer

	mov rbuf, r0	      /* Copy the structure pointer to rbuf */

	ldr r7, [rbuf, #BUFFER_START_OFFSET]
	ldr r8, [rbuf, #BUFFER_END_OFFSET]

	ldr r9, [rbuf, #BUFFER_CURRKEY_OFFSET]
	ldr r10, [rbuf, #BUFFER_BUFFTOP_OFFSET]

	/* Check if there's any data to read.
           If currkey != bufftop, there is data to read.
        */
	cmp r9, r10
	beq jjforth_buffer_read_empty

	/* Read the data */
	ldr r0, [r9]		/* read the next piece of data */

	/* Check if currkey is at the end of the buffer */
	cmp r9, r8

#ifdef IT_INSTRUCTION
	ittee ne
	/* Start of it block */
	/* If we don't need to wrap to the beginning of the buffer */
	addne r9, r9, #4
	strne r9, [rbuf, #BUFFER_CURRKEY_OFFSET]
	/* If we need to wrap to the beginning of the buffer */
	moveq r9, r7
	streq r9, [rbuf, #BUFFER_CURRKEY_OFFSET]
	/* End of it block */
#else
	beq jjforth_buffer_read_wrap
	add r9, r9, #4
	str r9, [rbuf, #BUFFER_CURRKEY_OFFSET]
#endif
	b jjforth_buffer_read_success

jjforth_buffer_read_wrap:
	mov r9, r7
	str r9, [rbuf, #BUFFER_CURRKEY_OFFSET]

	b jjforth_buffer_read_success

jjforth_buffer_read_empty:
	mov r11, #2
	str r11, [r1] 		/* store to first parameter */
	mov r0, r11		/* set the return value */
	b jjforth_buffer_read_done

jjforth_buffer_read_null_pointer:
	mov r11, #1
	str r11, [r1]
	mov r0, r11
	b jjforth_buffer_read_done

jjforth_buffer_read_success:
	mov r11, #0
	str r11, [r1] 		/* store to first parameter */

	b jjforth_buffer_read_done

jjforth_buffer_read_done:
	pop { rbuf, r6, r7, r8, r9, r10, r11 }

	bx lr


/* Clear the buffer

   The first parameter is a pointer to the buffer structure.

   Return:
     0 for success.
     1 for a null pointer exception
*/
jjforth_buffer_clear:
	push { rbuf, r6, r7, r9, r11 }

	cmp r0, #0
	beq jjforth_buffer_clear_null_pointer

	mov rbuf, r0

	/* Load the start of the buffer in general register r0 */
	ldr r7, [rbuf, #BUFFER_START_OFFSET]

	/* reset the read pointer to the beginning of the buffer */
	str r7, [rbuf, #BUFFER_CURRKEY_OFFSET]

	/* reset the write pointer to the beginning of the buffer */
	str r7, [rbuf, #BUFFER_BUFFTOP_OFFSET]

	mov r0, #0

	b jjforth_buffer_clear_done


jjforth_buffer_clear_null_pointer:
	mov r0, #1
	b jjforth_buffer_clear_done

jjforth_buffer_clear_done:
	pop { rbuf, r6, r7, r9, r11 }

	bx lr
