/*
 * A length-prefixed string data structure that supports character
 * appends.
 *
 * This data structure has the same layout and functionality as a
 * Pascal string.  It's not NULL terminated and is prefixed with the
 * length of the string.
 *
 * This Pascal-style string is stored in a container that holds a
 * pointer to the string along with a size or capacity.
 *
 * Operations:
 *   initialize
 *   put(character)
 *
 * Other possible options in the future:
 *   finalize() -> lock for put operations and allow exporting
 *
 */

/* The structure of the string structure
 *
 * This structure is designed to support simple string operations
 * while at the same time have an internal memory layout that fits
 * a length-prefixed string.
 *
 * It consists of two objects.  The first is a structure that stores
 * a pointer to array along with the length of the array.
 * The second object is the array itself.  The first element is the current
 * string length.  The rest is the string data.
 *
 * The first structure element is the location of the array that backs
 * the string.
 * The second structure element is the size of the array (number of
 * 32-bit elements).
 *
 * The array itself starts with a 32-bit length, followed by
 * 32-bit characters of the string.
 *
 * Parent structure:
 *       +------------------+
 *       |  0x12345678 | 32 |
 *       +------------------+
 *           address     len (capacity)
 *
 *  Array at address 0x12345678:
 *        len               string data
 *       +------------------------------------------+
 *       | 5 | H | e | l | l | o | .. unused .. |   |
 *       +------------------------------------------+
 *         0   1   2   3   4   5       ....      31       address (32-bit)
 *        0-3                                    124-127  address (8-bit)
 *
 * The length field is 32bit (4 bytes)
 * strings are wide strings by default, each character is 32-bits.
 * So a 31 character string will use 1 + (31 * 4) bytes of memory.
 * The length field takes one byte, followed by 4 bytes for each character.
 * So, 125 bytes in total.
 */

.thumb
.syntax unified
.text

.globl jjforth_pstring_init, jjforth_pstring_put


/* TODO: Standardize the ordering of length / data pointer in these offsets and the
 * structures.
 */


/* The offset of the string array address */
.set PSTRING_ARRAY_DATA_OFFSET, 0

/*
 * This is the size of the backing array, and should be checked when
 * writing to the string.
 */
.set PSTRING_ARRAY_LEN_OFFSET, 4

/* The offset to the current length of the string */
/* This is a 32-bit value */
.set PSTRING_STRING_LEN_OFFSET, 0

/* The actual string data */
.set PSTRING_STRING_DATA_OFFSET, 4

/* Set up some register aliases to hopefully make this code easier to follow and
 * be more consistent in register usage. */

/* TODO: This is used for both the parent structure and the
 * substructure.  That's kind of confusing. */	
sbuf .req r4			/* String structure address */


/*
 * Initialize a Pascal-style string
 *
 * This function doesn't allocate memory for the string, it only
 * performs bounds checking and initialization of the length.
 *
 *
 * Parameters:
 *  parameter 1: string structure address
 *  parameter 2: string buffer address
 *  parameter 3: string buffer length
 *
 */
jjforth_pstring_init:
	push { r3, sbuf, r6, r7, r9, r11 }

	/* Check to see if the string pointer is NULL */
	cmp r0, #0
	beq jjforth_pstring_init_null_pointer

	cmp r1, #0
	beq jjforth_pstring_init_null_pointer

	/* Check to see if the length doesn't extend beyond the 32-bit
	   boundary */
	/* adds performs an add and sets the status flags */
	adds r3, r1, r2
	/* branch if carry is set */
	/* TODO: Verify with some test cases */
	bhs jjforth_pstring_init_invalid_params

	mov sbuf, r0		/* Copy the structure pointer to sbuf */

	str r1, [sbuf, #PSTRING_ARRAY_DATA_OFFSET]
	str r2, [sbuf, #PSTRING_ARRAY_LEN_OFFSET]

	/* Initialize the current length of the string to zero */
	mov sbuf, r1
	mov r3, #0
	str r3, [sbuf, #PSTRING_STRING_LEN_OFFSET]

	b jjforth_pstring_init_success

jjforth_pstring_init_success:
	mov r0, #0
	b jjforth_pstring_init_done

jjforth_pstring_init_null_pointer:
	mov r0, #1
	b jjforth_pstring_init_done

jjforth_pstring_init_invalid_params:
	mov r0, #2
	b jjforth_pstring_init_done

jjforth_pstring_init_done:
	pop { r3, sbuf, r6, r7, r9, r11 }
	bx lr

/*
 * Add a character to the string
 *
 * Parameters:
 *  parameter 1: A pointer to the string data structure
 *  parameter 2: The character to add
 *
 * Returns:
 *   0 for success.
 *   1 for a null pointer exception
 *   2 if the string is full
 */
jjforth_pstring_put:
	push { r1, r2, r3, sbuf, r5, r6, r7, r8, r9, r11 }

	/* Check for null pointer */
	cmp r0, #0
	beq jjforth_pstring_put_null_pointer

	mov sbuf, r0		/* Copy the structure pointer to sbuf */
	mov r7, r1		/* Copy the character to add to r7 */

	/* First, get the array length and pointer to the string data
	 * from the parent container */
	ldr r9, [sbuf, #PSTRING_ARRAY_LEN_OFFSET]
	ldr r3, [sbuf, #PSTRING_ARRAY_DATA_OFFSET]

	/* Then get the current string length and pointer to the
	 * character data */
	ldr r8, [r3, #PSTRING_STRING_LEN_OFFSET]
	adds r5, r3, #PSTRING_STRING_DATA_OFFSET

	/* Check if the string length > the string array length */
	cmp r8, r9
	bhs jjforth_pstring_put_full

	/* Append the character to the string */

	/* TODO: Check for integer overflow */
	/* TODO: Check for add carry */
#ifdef BARREL_BARREL_BARREL
	/* Should also check on what conditions the status flags are set */
	adds r11, r5, r8, lsl #2
	str r7, [r11]
#else
	lsls r9, r8, #2
	/* mov r6, #4 */
	/* mul r9, r8, r6 */
	str r7, [r5, r9]
#endif

	/* Update the length of the string */
	mov r5, #1
	adds r8, r5, r8
	bhs jjforth_pstring_put_unknown_error

	str r8, [r3, #PSTRING_STRING_LEN_OFFSET]

	b jjforth_pstring_put_success

jjforth_pstring_put_success:
	mov r0, #0
	b jjforth_pstring_put_done

jjforth_pstring_put_null_pointer:
	mov r0, #1
	b jjforth_pstring_put_done

jjforth_pstring_put_full:
	mov r0, #2
	b jjforth_pstring_put_done

jjforth_pstring_put_unknown_error:
	mov r0, #3
	b jjforth_pstring_put_done

jjforth_pstring_put_done:
	pop { r1, r2, r3, sbuf, r5, r6, r7, r8, r9, r11 }
	bx lr
