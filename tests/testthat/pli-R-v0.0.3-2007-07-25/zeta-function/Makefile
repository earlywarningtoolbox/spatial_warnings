all: zeta_func.o
	gcc -L/sw/lib zeta_func.o -lgsl -lgslcblas -lm -o zeta_func

zeta_func.o:
	gcc -I/sw/include -c zeta_func.c

