	program test

	call print_individual_digits(4,919)
	write(*,*)""

	end program test

	subroutine print_individual_digits(i_digits_required,int_value)
	integer int_value
	integer	digits_required
	integer check_value
	integer print_value

	check_value = 1
	print_value = 1
	int_value_copy = int_value

	do 4 iter = 1,i_digits_required-1
	print_value = print_value * 10
  4	continue

  1	if(int_value - check_value) 10,11,12
  10	check_value = check_value/10
  	goto 13	
  11	goto 13
  12	check_value = check_value * 10
  	goto 1
  13	continue

  2	if (print_value-check_value) 18,19,20
  18	goto 3
  19	goto 3
  20	write(*,'(I1,$)') 0
  	print_value = print_value/10
  	goto 2

  3	if (check_value) 14,15,16
  14	goto 17
  15	goto 17
  16	i_c = int_value_copy/check_value
  	write(*,'(I1,$)') i_c
  	int_value_copy = int_value_copy - i_c*check_value
  	check_value = check_value/10
  	goto 3
  17	continue
  	end