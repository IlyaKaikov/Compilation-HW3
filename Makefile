.PHONY: all clean

CC = g++
CFLAGS = -std=c++17 -ggdb

all: clean
	flex scanner.lex
	bison -Wcounterexamples -d parser.y
	$(CC) $(CFLAGS) -o hw3 *.c *.cpp
clean:
	rm -f lex.yy.* parser.tab.* *.out hw3
