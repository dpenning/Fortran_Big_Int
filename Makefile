main:
	f95 -Wall main.f project1.f
	./a.out
tests:
	f95 -Wall -o test_program test_files/test_1.f project1.f
	./test_program
	f95 -Wall -o test_program test_files/test_2.f project1.f
	./test_program
	f95 -Wall -o test_program test_files/test_3.f project1.f
	./test_program
	f95 -Wall -o test_program test_files/test_4.f project1.f
	./test_program
	rm test_program
