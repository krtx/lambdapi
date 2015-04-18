
.PHONY: all clean

PROGRAM = main.byte
TEST = test.byte
SRC = $(wildcard *.ml) parser.mly lexer.mll
SRC_TEST = $(wildcard t/*.ml)
COREBUILD_OPT = -use-menhir -yaccflag -v -yaccflag --error-recovery

all: $(PROGRAM)

check: $(TEST)
	./$(TEST)

clean:
	corebuild -clean

main.byte: $(SRC)
	corebuild -use-menhir -yaccflag -v -yaccflag --error-recovery $(PROGRAM)

test.byte: $(SRC) $(SRC_TEST)
	corebuild -I t -pkg ounit $(COREBUILD_OPT) $(TEST)


