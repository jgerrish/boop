/*
 * Functions to transfer between pstrings and dictionaries.
 *
 * The pstring memory layout is the same as the layout of a word in a
 * dictionary entry.  It has a length followed by the characters.
 *
 * This isn't guaranteed, and may change over time.  The interface
 * between pstrings and dictionaries is in this separate file to help
 * decouple that choice.
 */

.thumb
.syntax unified
.text

.globl jjforth_dict_pstring_copy, jjforth_dict_pstring_add_pstring

/* The offset to the string array, which holds the string length and
 * data
 */
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


/*
 * Set up some register aliases to hopefully make this code easier to follow and
 * be more consistent in register usage.
 */
sbuf .req r4			/* String structure address */
rss .req r8			/* String source register */
rsd .req r9			/* String destination register */

/*
 * Copy a pstring into a dictionary at a given word address.
 *
 * This assumes the dictionary entry already exists and IS THE SAME SIZE
 * If it isn't, it should fail.
 * On review, it's not a really useful function, but it helped debug the
 * two data structures and can provide some additional code samples.
 * For example, at the assembly level, do we pass around substructures
 * (current length + data) or
 * full structures (allocated data and pointer + current length + data)
 * Substructures are used here, but there are other examples,
 * like input and output handling where the full structure makes sense.
 *
 * No memory is allocated here and no ownership is changed.
 * The dictionary (or function that created the dictionary) is
 * responsible for the entry.
 * The pstring (or function that created the pstring) is responsible
 * for it's own memory.
 *
 * Parameters:
 *  parameter 1: dictionary entry / word address
 *  parameter 2: pstring substructure address
 *
 *               This is the address of the start of the array containing the
 *               string.  It has the length followed by the actual character data.
 *
 * Returns:
 *   0 on success
 *   1 on null pointer
 *   2 on unknown error
 */
jjforth_dict_pstring_copy:
	push { r3, r4, r6, r7, r8, r9, r10, r11 }

	/* Make sure the dictionary entry pointer isn't NULL */
	cmp r0, #0
	beq jjforth_dict_pstring_copy_null_pointer

	/* Make sure the pstring pointer isn't NULL */
	cmp r1, #0
	beq jjforth_dict_pstring_copy_null_pointer

	/* Setup data for copying */
	mov r4, r0     /* Copy the word address into r4 */

	/* Get the pstring length and set it up for copying */

	/* The first field on the string is the current string length */
	/* adds r3, r1, #4 */ 	/* Advance to the current length field */
	mov r3, r1
	ldr r6,	[r3]   /* Copy the string length into r6 */

	mov r10, r6		/* Copy the length */

	/* Setup the string source pointer to point to the string
	   character data */
	/* TODO: Fix this constant and possibly others */
	adds r3, r3, #4
	mov rss, r3

	/* Now setup the dictionary word for copying */

	adds r4, r4, #4 	/* Advance to the length and flags field */

	/* Store the length, preserving any existing flags */
	ldr r9, [r4]		/* Get the current length and flags field value */

	/* Check if the lengths are equal, fail if they aren't */
	mov r7, #0x1F
	and r6, r9, r7
	cmp r10, r6
	bne jjforth_dict_pstring_copy_unequal_lengths

	/* advance to the character data */
	adds r4, r4, #1

	/* Setup the string registers for copying */
	mov rsd, r4		/* Copy the dictionary entry character address to rsd */

	/* Deal with the case of a zero-length string */
	mov r7, r10
	cmp r7, #0
	bne jjforth_dict_pstring_dict_copy_character_word_loop
	b jjforth_dict_pstring_copy_success

jjforth_dict_pstring_dict_copy_character_word_loop:
	/* Copy the string into the dictionary entry */
	ldr r10, [rss], #4
	str r10, [rsd], #4

	subs r7, r7, #1
	cmp r7, #0
	bne jjforth_dict_pstring_dict_copy_character_word_loop

	/* Successful copy */
	b jjforth_dict_pstring_copy_success

jjforth_dict_pstring_copy_unequal_lengths:
	/* If the words have unequal lengths, fail */
	mov r0, #2
	b jjforth_dict_pstring_copy_done

jjforth_dict_pstring_copy_null_pointer:
	mov r0, #1
	b jjforth_dict_pstring_copy_done

jjforth_dict_pstring_copy_success:
	mov r0, #0
	b jjforth_dict_pstring_copy_done

jjforth_dict_pstring_copy_done:
	pop { r3, r4, r6, r7, r8, r9, r10, r11 }

	bx lr

/*
 * Add a pstring to a dictionary
 * This is a wrapper to the dict_add_word function that
 * adds a pstring.
 *
 * Parameters:
 *  parameter 1: the PString to add
 *  parameter 2: the additional flags to set on the word
 *
 * Returns:
 *  result of the function, 0 on success, 1 on failure
 */
jjforth_dict_pstring_add_pstring:
	push { r2, r3, r4, r6, r7, r8, r9, r10, r11 }

	/* Check to see if the string pointer is NULL */
	cmp r0, #0
	beq jjforth_dict_pstring_add_pstring_null_pointer

	mov sbuf, r0		/* Copy the structure pointer to sbuf */

	/* Copy over parameter two, the flags, to parameter three */
	mov r2, r1

	/* First, get the container array location */
	ldr r3, [sbuf, #PSTRING_ARRAY_DATA_OFFSET]

	/* Now get the string length */
	ldr r1, [r3, #PSTRING_STRING_LEN_OFFSET]

	/* Then get the string location
	 * (the second field in the buffer, after the length)
         */
	adds r0, r3, #PSTRING_STRING_DATA_OFFSET

	/* Call the normal dictionary add function */
	push { lr }
	bl dict_add_word
	pop { lr }

	/* We manually map every return code to wrapper function return codes */
	cmp r0, #0
	beq jjforth_dict_pstring_add_pstring_success

	cmp r0, #1
	beq jjforth_dict_pstring_add_pstring_null_pointer

	cmp r0, #2
	beq jjforth_dict_pstring_add_pstring_word_too_long

	cmp r0, #3
	beq jjforth_dict_pstring_add_pstring_word_too_short

	b jjforth_dict_pstring_add_pstring_unknown_error

jjforth_dict_pstring_add_pstring_success:
	mov r0, #0
	b jjforth_dict_pstring_add_pstring_done

jjforth_dict_pstring_add_pstring_null_pointer:
	mov r0, #1
	b jjforth_dict_pstring_add_pstring_done

jjforth_dict_pstring_add_pstring_word_too_long:
	mov r0, #2
	b jjforth_dict_pstring_add_pstring_done

jjforth_dict_pstring_add_pstring_word_too_short:
	mov r0, #3
	b jjforth_dict_pstring_add_pstring_done

jjforth_dict_pstring_add_pstring_unknown_error:
	/* We lose stack error information here */
	mov r0, #4
	b jjforth_dict_pstring_add_pstring_done

jjforth_dict_pstring_add_pstring_done:
	pop { r2, r3, r4, r6, r7, r8, r9, r10, r11 }
	bx lr
