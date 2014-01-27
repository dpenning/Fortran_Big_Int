c 	David Pennington
c 	Nick Graczyk

c 	This Project can store and manipulate multiple bigints.
c 	We allocate space for a linked list of bigintegers
c 	Each BigInt is stored with its size as the first int
c 	and its sign as the second int. The rest of the integers
c 	from [2,size+2] are the values of the bigint
c 	The system is "BigEndian" in that the least signifigant

	program project_1

	integer i_bigint_storage(3000)
	integer i_val
	integer i_number_of_digits

	i_number_of_digits = 3
	i_iterations = 100
	
	call init_bigint_storage_space(i_bigint_storage)
	call allocate(1,i_bigint_storage)
	i_bigint_storage(2) = 1
	i_bigint_storage(3) = 1
	do 100 iter = 1,i_iterations
	call double_last_bigint(i_number_of_digits,i_bigint_storage)
	call pretty_print(1,i_number_of_digits,i_bigint_storage)
  100	continue

	end program project_1

c	This function allocates a new bigint and
c	returns the index of the new big int
	subroutine allocate(i_d,i_bis)

c 	Number of Digits
	integer i_d
c 	The Space for the BigInts
	integer i_bis(3000)

c 	start the index at the start of the linked list
	i_bii = 1

c 	move down the linked list until we find an open spot
  1	if(i_bis(i_bii)) 2,2,3
  2	continue
c  	write (*,*) "Can Write at ",i_bii
  	goto 4
  3 	i_bii = i_bii + i_bis(i_bii)
  	i_bii = i_bii + 2
  	goto 1

c 	I made my own while loop that exits out here
c 	This while loop looks for open space
  4	continue

c 	Now that We have the position for the BigInt
c 	Initialize the BigInt
c 	Set the Size to Number of Digits
  	i_bis(i_bii) = i_d
c 	Set the Sign to 0
  	i_bis(i_bii+1) = 0
  	do 5 iter = 1,i_d
  	i_bis(i_bii+1+iter) = 0
  5	continue
c 	set the value after to 1
  	i_bis(i_bii+1+iter) = -1
	end

c 	this subroutine inits the storage space
c 	it starts the linked list and sets all vals to 0
	subroutine init_bigint_storage_space(i_bis)

	integer i_bis(3000)
	do 6 iter = 1,3000
	i_bis(iter) = 0
  6	i_bis(1) = -1
	end

c 	This Subroutine Prints a bigint array section
	subroutine print_bigint_debug(i_start,i_stop,i_bis)
	integer i_start
	integer i_stop
	integer i_bis(3000)
	write(*,*) "-----------"
	do 7 iter = i_start,i_stop,4
	write(*,*) i_bis(iter),i_bis(iter+1),i_bis(iter+2),i_bis(iter+3)
  7	continue
  	write(*,*)"-----------"
  	end

c 	This Subroutine get the index of the last bigint
  	subroutine get_last_bigint_index(i_return_val,i_bis)
  	integer i_return_val
  	integer i_bis(3000)
  	integer i_bii

  	i_bii = 1
  8	if(i_bis(i_bii)) 9,10,10
  9	goto 11
  10 	i_return_val = i_bii
	i_bii = i_bii + i_bis(i_bii) + 2
  	goto 8
  11	end

c 	This subroutine normalizes the last bigint
  	subroutine normalize_last_big_int(i_digit_length,i_bis)
  	integer i_digit_length
  	integer i_bis(3000)
  	integer index
  	integer i_val
  	integer i_mod_val
  	integer i_quotient
  	integer i_new_val

  	i_mod_val = 1
  	do 12 iter = 1,i_digit_length
  	i_mod_val = i_mod_val*10
  12	continue
  	call get_last_bigint_index(index,i_bis)
  	do 17 iter = index + 2, index + 1 + i_bis(index)
  	if(iter-(index + 1 + i_bis(index))) 13,14,14
  13	i_val = i_bis(iter)
  	i_quotient = i_val/i_mod_val
  	i_new_val = i_val-(i_quotient*i_mod_val)
  	i_bis(iter) = i_new_val
  	i_bis(iter+1) = i_bis(iter+1) + i_quotient
  	goto 16
  14	i_val = i_bis(iter)
  	i_quotient = i_val/i_mod_val
  	i_new_val = i_val-(i_quotient*i_mod_val)
  	i_bis(iter) = i_new_val
  	if(i_quotient) 16,16,15
  15	i_bis(index) = i_bis(index)+1
  	i_bis(iter+1) = i_quotient
  	i_bis(iter+2) = -1
  16	continue
  17	continue
  	end

c 	This Subroutine doubles the last bigint
  	subroutine double_last_bigint(i_digit_length,i_bis)
  	integer i_digit_length
  	integer i_bis(3000)
  	integer index
  	call get_last_bigint_index(index,i_bis)
  	do 18 iter = index + 2, index + 1 + i_bis(index)
  	i_bis(iter) = i_bis(iter)+i_bis(iter)
  18	continue
  	call normalize_last_big_int(i_digit_length,i_bis)
  	end

c 	This subroutine prints an individual int for the specified size
	subroutine print_individual_digits(i_z,i_digits,int_value)
	integer i_z
	integer int_value
	integer	i_digits
	integer check_value
	integer print_value

	check_value = 1
	print_value = 1
	int_value_copy = int_value

	do 19 iter = 1,i_digits-1
	print_value = print_value * 10
  19	continue

  20	if(int_value - check_value) 21,22,23
  21	check_value = check_value/10
  	goto 24	
  22	goto 24
  23	check_value = check_value * 10
  	goto 20
  24	continue

  	if(i_z) 29,29,25
  25	if (print_value-check_value) 26,27,28
  26	goto 29
  27	goto 29
  28	write(*,'(I1,$)') 0
  	print_value = print_value/10
  	goto 25

  29	if (check_value) 30,31,32
  30	goto 33
  31	goto 33
  32	i_c = int_value_copy/check_value
  	write(*,'(I1,$)') i_c
  	int_value_copy = int_value_copy - i_c*check_value
  	check_value = check_value/10
  	goto 29
  33	continue
  	end

c 	This subroutine prints the Bigints nicely
	
	subroutine pretty_print(i_bii,i_d,i_bis)
	integer i_bii
	integer i_d
	integer i_bis(3000)
	integer i_z
	integer num_ints

	i_z = 0
	num_ints = i_bis(i_bii)
	do 35 iter = i_bii+num_ints+1,i_bii+2,-1
	call print_individual_digits(i_z,i_d,i_bis(iter))
	write(*,'(A,$)')" "
	if (i_bis(iter)) 35,35,34
  34	i_z = 1
  35	continue
  	write(*,'(A)')""


	end










