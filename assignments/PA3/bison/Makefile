

BFLAGS=-d
CC=gcc
CFLAGS=-lm
CSRC = calc.tab.c	lex.yy.c
FLEX=flex ${FFLAGS}
BISON= bison ${BFLAGS}

mycalc:	${CSRC}
	${CC} ${CSRC} ${CFLAGS} -o mycalc

calc.tab.c calc.tab.h:	calc.y
	${BISON}	calc.y

lex.yy.c:	calc.flex	calc.tab.h
	${FLEX}	calc.flex

clean:
	rm -f calc.tab.h ${CSRC} mycalc
