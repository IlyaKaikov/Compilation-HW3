.PHONY: all clean

all: clean
	flex scanner.lex
	bison -Wcounterexamples -d parser.y
	g++ -std=c++17 -o hw3 *.c *.cpp
clean:
	rm -f lex.yy.c
	rm -f parser.tab.*
	rm -f hw3
	rm -f *.out
