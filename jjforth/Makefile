AS=arm-none-eabi-as
CC=arm-none-eabi-gcc
AR=arm-none-eabi-gcc-ar

# The IT instruction is limited on some operating systems
# https://devblogs.microsoft.com/oldnewthing/20210601-00/

USE_IT=-DIT_INSTRUCTION

CFLAGS=-O0 -g -mthumb -mabi=aapcs -Wall -Werror $(USE_IT)

LDFLAGS=-g -mthumb --oformat elf32-littlearm -T memory.x -L ~/.local/lib
CFLAGS_M3=$(CFLAGS) -mcpu=cortex-m3 -march=armv7-m -T memory.x
LDFLAGS_M3=$(LDFLAGS)
CFLAGS_M4=$(CFLAGS) -mcpu=cortex-m4 -T memory.x
LDFLAGS_M4=$(LDFLAGS)

SRCS = buffer.S stack.S dict.S jjforth.S
M3_OBJS = $(SRCS:.S=-cortex-m3.o)
M4_OBJS = $(SRCS:.S=-cortex-m4.o)

PREFIXES=buffer stack dict jjforth

all: libjjforth-cortex-m3.a libjjforth-cortex-m4.a

clean:
	rm -f jjforth-*.o libjjforth-*.a libjjforth-*.so stack-*.o buffer-*.o dict-*.o

%-cortex-m3.o: %.S
	$(CC) $(CFLAGS_M3) -o $@ -c $<

%-cortex-m4.o: %.S
	$(CC) $(CFLAGS_M4) -o $@ -c $<

libjjforth-cortex-m3.a: $(M3_OBJS)
	arm-none-eabi-ar rc --target elf32-littlearm $@ $^

libjjforth-cortex-m3.so: $(M3_OBJS)
	arm-none-eabi-ar rc --target elf32-littlearm $@ $^

libjjforth-cortex-m4.a: $(M4_OBJS)
	arm-none-eabi-ar rc --target elf32-littlearm $@ $^

libjjforth-cortex-m4.so: $(M4_OBJS)
	arm-none-eabi-ar rc --target elf32-littlearm $@ $^
