# Makefile for the hamming project
CC=gcc
CFLAGS=-std=c99 -pedantic -Wall -Werror
FILES=Hamming.hs
TEST_FILES=Test.hs
NOISE_FILES=noisegen.c

all: hamming encoder decoder noisegen test 

hamming: $(FILES)
	ghc $?

encoder: Encoder.hs
	ghc $? -o $@

decoder: Decoder.hs
	ghc $? -o $@

hamming_test: $(TEST_FILES)
	ghc $?

noisegen: $(NOISE_FILES)
	$(CC) $(CFLAGS) $? -o $@

test: hamming_test
	./Test

clean:
	rm *.{hi,o}
