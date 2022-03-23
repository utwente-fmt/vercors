# -*- tab-width:4 ; indent-tabs-mode:true -*-

all: zero-spec.omp init-spec.omp copy-spec.omp main other add-spec.omp sum-spec.omp 

main: test-main.o
	gcc -std=c99 -fopenmp -o main $^

other: test-other.o
	gcc -std=c99 -fopenmp -o other $^

%.omp: %.c
	gcc -std=c99 -fopenmp -o $@ $<


