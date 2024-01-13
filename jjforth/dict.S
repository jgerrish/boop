/* Functions for dealing with Forth dictionaries
 *
 * These diagrams borrow heavily from the jonesforth Forth compiler.
 *
 * The dictionary is a linked list of words. Each word is a node with
 * a link to the next word,
 *
 * previous word
 *    ^
 *    |
 *    |
 * +-------------------------------------------+
 * | LINK   | 3 | B | O | P | DUP code pointer -------> pointer to DUP code
 * +-------------------------------------------+
 *    ^     len /               codeword
 *    |     flags
 * next word
 *
 *
 * The full word structure looks like this in memory:
 *
 * +-------------------------------------------------------------+
 * | LINK       | i?hnnnnn | B | O | P | DUP code pointer -------> pointer to DUP code
 * +- (4 bytes) --byte---- (nnnnn * 4 bytes) --------------------+
 *
 * length / flag byte:
 *  i:     immediate flag
 *  ?:     unknown flag
 *  h:     hidden flag
 *  nnnnn: 32-bit length field
 *
 *
 * Implementation and style notes:
 *
 * Any register that is used as a parameter to a function can be
 * clobbered by that function.  Don't expect the same value on return.
 * All other registers should be preserved in this file.  Other files
 * in this project may have different standards.
 */

.thumb
.syntax unified
.text

.globl dict_init, dict_add_word, dict_word_length, dict_find
.globl dict_encode_ascii_as_utf32, dict_encode_ascii_string_as_utf32

/* Set up some register aliases to hopefully make this code easier to follow and
   be more consistent in register usage. */
rss .req r8			/* String source register */
rsd .req r9			/* String destination register */

/* Initialize the dictionary
 *
 * Initialize the dictionary with a start address and length
 *
 * Parameters:
 *  parameter 1: dictionary start address
 *  parameter 2: dictionary length
 *
 */
dict_init:
	push { r3, r6, r7, r9, r11 }

	/* Set the dictionary address */
	ldr r3, =dict_test_dictionary
	str r0, [r3]

	/* Set the dictionary end marker */
	add r0, r0, r1
	ldr r3, =dict_test_dictionary_end
	str r0, [r3]

	/* Set the current head to NULL */
	ldr r3, =dict_test_dictionary_head
	mov r7, #0
	str r7, [r3]

	pop { r3, r6, r7, r9, r11 }

	bx lr

/*
 * Get the length of a word given a pointer to a flag field
 *
 * Parameters:
 *  parameter 1: A pointer to the flag / length field.
 *
 * Returns:
 *  return: The length of the word
 */
dict_word_length:
	push { r6, r7, r9, r11 }

	mov r7, #0x1F
	ldr r11, [r0]
	and r0, r11, r7

	pop { r6, r7, r9, r11 }

	bx lr

/*
 * Add a word to the dictionary
 *
 * The word is expected to be encoded in UTF-32 before calling this
 * function.
 *
 * This version doesn't link to the program code yet.
 * TODO: Add program code links
 *
 * Parameters:
 *  parameter 1: the word to add
 *  parameter 2: the length of the word
 *  parameter 3: the additional flags to set on the word
 *
 * Returns:
 *  result of the function, 0 on success, 1 on failure
 */
dict_add_word:
	push { r3, r4, r6, r7, r8, r9, r10, r11 }

	/* Make sure the word isn't too long */
	cmp r1, #0x1F
	bhi dict_add_word_word_too_long

	/* Make sure the word isn't too short */
	cmp r1, #0
	beq dict_add_word_word_too_short

	/* backup the word pointer */
	mov rss, r0

	/* Get the current head */
	ldr r3, =dict_test_dictionary_head
	ldr r4, [r3]

	cmp r4, #0
	beq dict_add_word_empty_dict

	/* Add another word to the dictionary */
	adds r4, r4, #4 	/* Advance to the length and flags */

	/* Get the word length */
	mov r0, r4
	push { lr }
	bl dict_word_length	/* Find the word length */
	pop { lr }

	mov r3, #4
	mul r0, r0, r3		/* Multiply by four for UTF-32 words */
	adds r4, r4, #1		/* Advance past the length and flags */
	adds r4, r4, r0		/* Advance past the word */
	b dict_add_word_check_memory

dict_add_word_empty_dict:
	ldr r4, =dict_test_dictionary
	ldr r4, [r4]

	b dict_add_word_check_memory

dict_add_word_check_memory:
	/* Check if there is enough memory to add the word */
	mov r0, #5		/* LINK and flag byte length */
	mov r3, #4		/* character size */
	mul r3, r3, r1		/* length of word */
	adds r0, r4, r3

	ldr r3, =dict_test_dictionary_end
	ldr r3, [r3]
	subs r3, r3, #5
	cmp r0, r3
	bhi dict_add_word_out_of_memory

	b dict_add_word_do_add

dict_add_word_out_of_memory:
	mov r0, #1

	b dict_add_word_end

dict_add_word_do_add:
	/* Create the entry */

	/* Store the word address for updating the head pointer */
	mov r6, r4

	/* Set the LINK pointer to the current head */
	ldr r3, =dict_test_dictionary_head
	ldr r7, [r3]
	str r7, [r4], #4

	orrs r2, r1, r2		/* bitwise OR the additional flags with the word length */
	str r2, [r4], #1	/* Store the word length and flags, and advance */

	/* Set up the string destination register */
	mov rsd, r4

dict_add_word_word_loop:
	ldr r10, [rss], #4
	str r10, [rsd], #4

	subs r1, r1, #1
	cmp r1, #0
	bne dict_add_word_word_loop

	/* Up to now, we haven't changed the head, and we can back out
	   of this function */
	ldr r3, =dict_test_dictionary_head
	ldr r7, [r3]
	str r7, [r6]		/* Set the LINK address to the current head pointer value */
	str r6, [r3]		/* Set the head pointer to the LINK address */

	mov r0, #0

	b dict_add_word_end

dict_add_word_word_too_long:
	mov r0, #2

	b dict_add_word_end

dict_add_word_word_too_short:
	mov r0, #3

	b dict_add_word_end

dict_add_word_end:
	pop { r3, r4, r6, r7, r8, r9, r10, r11 }

	bx lr


/*
 * Find a Forth word in the dictionary
 * Returns the word if it is found
 *
 * Parameters:
 *  parameter 1: The word to search for, encoded as UTF-32
 *  parameter 2: The length of the word to search for
 *  parameter 3: Pointer to the result
 *
 * Returns:
 *  return: A pointer to the word
 */
dict_find:
	push { r3, r4, r6, r7, r8, r9, r10, r11 }

	/* Copy the string pointer over to a backup */
	mov r10, r0
	mov r11, r0

	ldr r4, =dict_test_dictionary_head
	ldr r8, [r4]

	/* Check if the link is NULL */
	cmp r8, #0
	beq dict_find_word_not_found


dict_find_find:
	mov r10, r11		/* Restore the search string pointer */

	/* First, compare lengths and fail if they don't match
	 * Assume r8 contains a pointer to the current link
	 */
	mov r0, r8		/* Copy the current link pointer */
	mov r6, r8              /* Copy the current link pointer */
	add r0, r0, #4	        /* Point to the length / flag field */
	mov r3, r0		/* back up the length / flag field */

	push { lr }
	bl dict_word_length	/* Find the word length */
	pop { lr }

	cmp r0, r1
	bne dict_find_word_doesnt_match

	/* Ignore the word if it's hidden */
	ldr r3, [r3]
	and r0, r3, #0x20
	cmp r0, #0x20
	beq dict_find_word_doesnt_match

	add rss, r8, #5		/* advance to the word itself */

dict_find_find_word_loop:
	ldr r7, [rss], #4
	ldr r9, [r10], #4

	cmp r7, r9
	bne dict_find_word_doesnt_match

	subs r1, r1, #1
	cmp r1, #0
	bne dict_find_find_word_loop

	/* If everything matched to the end, success */
	mov r0, r6		/* Set the return to the start of the word */
	mov r4, #0		/* indicate success */
	b dict_find_end

dict_find_word_doesnt_match:
	/* advance to the next word */
	ldr r8, [r6]		/* Look at the saved LINK pointer */
	/* Check if the link is NULL */
	cmp r8, #0
	beq dict_find_word_not_found

	/* ldr r8, [r8] */

	b dict_find_find	/* just branch, don't need to branch and link */

dict_find_word_not_found:
	mov r4, #1		/* indicate failure */
	b dict_find_end

dict_find_end:
	/* Store the result in the parameter three reference */
	str r4, [r2]
	pop { r3, r4, r6, r7, r8, r9, r10, r11 }

	bx lr

/*
 * Encode a 7-bit ASCII code as UTF-32
 * Parameter one is an 32-bit value that should contain the 7-bit
 * ASCII character to encode in the lower 7 bits.
 *
 * ASCII to UTF-32 encoding is fairly simple, just put the ASCII
 * in the proper byte.
 * For little-endian (the default configuration for Cortex-M devices)
 * put it in the first byte, then every loop advance by
 * four bytes.
 *
 * For more information on endianness on ARMv7-M devices:
 * A3 ARM Architecture Memory Model
 * A3.3 Endian support
 * ARM DDI 0403E.b
 *
 *
 * Parameters:
 *   parameter 1: r0 is the first parameter, a 32-bit register with the ASCII value to encode
 *   parameter 2: r1 is the second parameter, a pointer to the result code
 *     Sets the return result code to 0 if the value can't be encoded
 *     Sets the return result code to 1 if the value can't be encoded
 *
 * Returns:
 *  return: Returns a 32-byte value representing the UTF-8 encoding
 */
dict_encode_ascii_as_utf32:
	push { r2, r6, r7, r9, r11 }

	cmp r0, #0x7F
	bhi dict_encode_ascii_invalid_encoding
	mov r2, #0
	b dict_encode_ascii_as_utf32_end

dict_encode_ascii_invalid_encoding:
	mov r2, #1
	b dict_encode_ascii_as_utf32_end

dict_encode_ascii_as_utf32_end:
	str r2, [r1]

	pop { r2, r6, r7, r9, r11 }

	bx lr


/*
 * Encode a 7-bit ASCII string as a UTF-32 string
 * Parameter one is an 8-bit unsigned character string that should
 * contain the 7-bit ASCII character to encode in the lower 7 bits.
 *
 * Returns a 32-byte string representing the UTF-8 encoding.
 * Caller is responsible for allocating the destination memory.
 * It should be four times the length of the source string.
 *
 *
 * Parameters:
 *   parameter 1: r0 is the first parameter, an 8-bit unsigned character string the
 *                ASCII values to encode.
 *   parameter 2: r1 is the second parameter, the length of the source string.
 *   parameter 3: r2 is the third parameter, the destination buffer.
 *
 * Returns:
 *  return: A result code
 *          Sets the return result code to 0 if the value can't be encoded
 *          Sets the return result code to 1 if the value can't be encoded
 *
 * Warning: Be careful about lifetimes with the destination buffer
 * in any caller code.
 */
dict_encode_ascii_string_as_utf32:
	push { r3, r6, r7, r8, r9, r11 }

	/* Make sure the word isn't too short */
	cmp r1, #0
	beq dict_encode_ascii_string_as_utf32_too_short

	mov rss, r0
	mov rsd, r2
	mov r3, r1		/* Copy over the length */

dict_encode_ascii_string_as_utf32_loop:
	/* Load the next character */
	ldrb r0, [rss], #1

	/* Convert to UTF-32 */
	ldr r1, =dict_result_variable

	push { lr }
	bl dict_encode_ascii_as_utf32
	pop { lr }

	ldr r2, [r1]
	cmp r2, #0

	bne dict_encode_ascii_string_as_utf32_invalid_encoding


	/* Save the encoded character */
	strb r0, [rsd], #4

	subs r3, r3, #1
	cmp r3, #0
	bne dict_encode_ascii_string_as_utf32_loop
	mov r0, #0

	b dict_encode_ascii_string_as_utf32_end

dict_encode_ascii_string_as_utf32_invalid_encoding:
	mov r0, #1
	b dict_encode_ascii_string_as_utf32_end

dict_encode_ascii_string_as_utf32_too_short:
	mov r0, #2
	b dict_encode_ascii_string_as_utf32_end

dict_encode_ascii_string_as_utf32_end:
	pop { r3, r6, r7, r8, r9, r11 }

	bx lr

.bss

.globl dict_test_dictionary, dict_test_dictionary_end

dict_test_dictionary_head:
	.int 0

dict_test_dictionary:
	.int 0
dict_test_dictionary_end:
	.int 0

dict_result_variable:
	.int 0
