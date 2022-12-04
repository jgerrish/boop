# Boop Error Design Document

This document discusses the design decisions that went into designing
error types for the boop Forth compiler project.

## Individual Components

There are several individual components in the boop project.  Each
component has different requirements for errors and return API
contracts.

### Stacks

Stacks have several basic operations that require return types:

#### Push

A push operation can be successful or result in a failure.  There are
several basic reasons for failure:

	1. StackOverflow: Stack overflow (stack if full)
	2. Unknown: Other system-related failure

Other system-related failures will get rolled up into an unknown
failure for now.

#### Pop

A push operation can be successful or result in a failure.  There are
several basic reasons for failure:

	1. StackUnderflow: Stack underflow (stack is empty)
	2. Unknown: Other system-related failure

Other system-related failures will get rolled up into an unknown
failure for now.


### Queues and Buffers

#### Read

A read operation can be successful or result in a failure.  If the
buffer is empty, an Empty error is returned.

	1. Empty (buffer is empty)
	2. Unknown: Other system-related failure

#### Write

A write operation can be successful or result in a failure.  If the
buffer is full, an Full error is returned.

	1. Full: (buffer is full)
	2. Unknown: Other system-related failure
