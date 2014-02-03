	program project_1
	integer i_bigint_storage(3000)
	integer i_digit_value
	integer i_digit_location

	i_digit_value = 8

	call init(i_bigint_storage)
	call print_bigint_debug(1,16,i_bigint_storage)
	call allocate(i_bigint_storage,2,i_digit_location)
	write(*,*)i_digit_location
	call print_bigint_debug(1,16,i_bigint_storage)
	i_bigint_storage(i_digit_location+1) = 1
	i_bigint_storage(i_digit_location+2) = 36
	call print_bigint_debug(1,16,i_bigint_storage)
	call normalize_bigint(i_bigint_storage,i_digit_location, 
     1  i_digit_value)
	call print_bigint_debug(1,16,i_bigint_storage)
	call add_bigints(i_bigint_storage,1,1,i_digit_value,i_new_loc)
	call print_bigint_debug(1,16,i_bigint_storage)
	end program project_1

c 	INIT
	subroutine init(i_bis)
	integer i_bis(3000)
	do 6 iter = 1,3000
	i_bis(iter) = 0
  6	i_bis(1) = -1
	end

c 	ALLOCATE
	subroutine allocate(i_bis,i_digits,i_loc)
	integer i_bis(3000)
	integer i_digits
	integer i_loc
	i_bii = 1
  1	if(i_bis(i_bii)) 2,2,3
  2	continue
  	goto 4
  3 	i_bii = i_bii + i_bis(i_bii)
  	i_bii = i_bii + 2
  	goto 1
  4	continue
  	i_loc = i_bii
  	i_bis(i_bii) = i_digits
  	i_bis(i_bii+1) = 0
  	do 5 iter = 1,i_digits
  	i_bis(i_bii+1+iter) = 0
  5	continue
  	i_bis(i_bii+1+iter) = -1
	end

c 	NORMALIZE
	subroutine normalize_bigint(i_bis,i_loc,i_digit)
	integer i_bis(3000)
	integer i_loc
	integer i_digit
	integer i_length
	integer i_quotient
	i_length = i_bis(i_loc)
  	do 100 i_iter = i_loc+2,i_loc+1+i_length
  	if (i_bis(i_iter)-i_digit) 101,102,102
  102	i_quotient = i_bis(i_iter)/i_digit
  	if (i_quotient) 103,103,104
  104	i_bis(i_iter) = i_bis(i_iter)-(i_quotient*i_digit)
  	i_bis(i_iter+1) = i_bis(i_iter+1) + i_quotient
  103	continue
  101	continue
  100	continue

  	end

c 	ADD
  	subroutine add_bigints(i_bis,i_bi1,i_bi2,i_digit,i_loc)
  	integer i_bis(3000)
  	integer i_bi1
  	integer i_bi2
  	integer i_max_length
  	integer i_loc
  	if(i_bi1-i_bi2) 111,112,112
  111	i_max_length = i_bis(i_bi1)
  	goto 113
  112	i_max_length = i_bis(i_bi2)
  113	continue
  	write(*,*)i_max_length
  	call allocate(i_bis,i_max_length+1,i_loc)
  	do 115 i_iter = 2,i_max_length+2
  	if (i_bis(i_bi1)+2-i_iter) 117,117,116
  116	i_bis(i_loc+i_iter) = 0+i_bis(i_bi1+i_iter)
  	goto 118
  117	i_bis(i_loc+i_iter) = 0
  118	continue
  	if (i_bis(i_bi2)+2-i_iter) 120,120,119
  119	i_bis(i_loc+i_iter) = i_bis(i_loc+i_iter)+i_bis(i_bi2+i_iter)
  120 	continue
  	write(*,*)i_bis(i_loc+i_iter)
  115	continue
  	call normalize_bigint(i_bis,i_loc,i_digit)
  	end

c 	PRINT
	subroutine print_bigint(i_bis,i_loc,i_digit)
	integer i_bis(3000)
	integer i_loc
	integer i_digit

	end

c 	This Subroutine Prints a bigint array section
	subroutine print_bigint_debug(i_start,i_stop,i_bis)
	integer i_start
	integer i_stop
	integer i_bis(3000)
	write(*,*) "-----------"
	do 105 iter = i_start,i_stop,4
	write(*,*) i_bis(iter),i_bis(iter+1),i_bis(iter+2),i_bis(iter+3)
  105	continue
  	write(*,*)"-----------"
  	end