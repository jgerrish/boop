.PHONY: boop jjforth stm32f1

all: jjforth boop stm32f1

clean: clean-jjforth clean-boop clean-stm32f1

jjforth:
	cd jjforth && make && cd ..

boop:
# We need to run cargo clean because sometimes Rust doesn't link in
# updated external libaries
	cd boop && cargo clean && cargo build && cd ..

stm32f1:
# We need to run cargo clean because sometimes Rust doesn't link in
# updated external libaries
	cd stm32f1 && cargo clean && cargo build && cd ..

clean-jjforth:
	cd jjforth && make clean && cd ..

clean-boop:
	cd boop && cargo clean && cd ..

clean-stm32f1:
	cd stm32f1 && cargo clean && cd ..

run:
	cd stm32f1 && cargo run & cd ..
