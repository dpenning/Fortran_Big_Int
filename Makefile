main:
	gfortran -Wall main.f project1.f
	./a.out
tests:
	gfortran -Wall -o test_program test.f project1.f
	./test_program
	rm test_program