all: exp_int.o
	gcc -L/sw/lib exp_int.o -lgsl -lgslcblas -lm -o exp_int

exp_int.o:
	gcc -I/sw/include -c exp_int.c

