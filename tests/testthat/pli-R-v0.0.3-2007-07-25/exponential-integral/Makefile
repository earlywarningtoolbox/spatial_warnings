all: exp_int.o
	gcc -L/usr/lib exp_int.o -lgsl -lgslcblas -lm -o exp_int

exp_int.o:
	gcc -I/usr/include -c -lm exp_int.c

