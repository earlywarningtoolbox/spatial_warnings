all: zeta_func.o
	gcc -I/usr/include zeta_func.o -lgsl -lgslcblas -lm -o zeta_func

zeta_func.o:
	gcc -I/sw/include -c zeta_func.c

