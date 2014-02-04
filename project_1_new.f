c 	David Pennington
c 	Nick Graczyk
c
c 	This Project can store and manipulate multiple bigints.
c 	We allocate space for a linked list of bigintegers
c 	Each BigInt is stored with its size as the first int
c 	and its sign as the second int. The rest of the integers
c 	from [2,size+2] are the values of the bigint
c 	The system is "BigEndian" in that the least signifigant
c       value is the start of the bigint
c
c 	you will notice that the printing does not neccessarily 
c 	represent what is on the requirements. this is because
c 	you are misrepresenting the base with extra zeroes
c 	on the end of a number. This is why I wrote my own value
c 	printer that correctly prints the number in its base
c
c 	the easy fix for this is to just use '(*,I3)' but that
c 	seems lazy
c %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


c 	MAIN PROJECT
c %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
c 	This is the start of the main project

c 	the storage space for integers
c 	is located at i_bigint_storage
c 	the base is i_base
c 	the the number of iterations is defined by
c 	i_iterations

c 	a value of 1 is initialized
c 	then that value is added to itself to produce 2
c 	then that value is added to itself to make 4 and so on
c %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	program project_1
	integer i_bigint_storage(3000)
	integer i_base
	integer i_digit_location
	integer i_location
	integer i_tmp
	integer i_tmp_2

	i_base = 1000
	i_iterations = 50

	call init(i_bigint_storage)
	call allocate(i_bigint_storage,1,i_tmp)
	i_bigint_storage(3) = 1
	call print_bigint(i_bigint_storage,i_tmp,i_base)
	call add_bigints(i_bigint_storage,1,1,i_base,i_tmp)
	call print_bigint(i_bigint_storage,i_tmp,i_base)
	do 400 iter = 2,i_iterations
	call add_bigints(i_bigint_storage,i_tmp,i_tmp,i_base,i_tmp_2)
	i_tmp = i_tmp_2
	call print_bigint(i_bigint_storage,i_tmp,i_base)
  400	continue

	end program project_1

c 	INIT
c %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
c 	This subroutine initializes the Bigint Storage
c 	it puts the value of 0 in all 3000 spots
c 	in spot 1 it puts -1 to signal the end of the
c 	"linked list"

c 	This function takes in the bigint storage space as i_bis
c %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	subroutine init(i_bis)
	integer i_bis(3000)

c 	go through all 3000 spots putting 0
	do 6 iter = 1,3000
	i_bis(iter) = 0

c 	set the first value to -1 to signify start of list
  6	i_bis(1) = -1
	end

c 	ALLOCATE
c %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
c 	This suybroutines finds the next place to create a bigint
c 	in the linked list by finding the last index and
c 	building the bigint there

c 	This function takes in the bigint storage space as i_bis
c 	the number of digits as i_digits
c 	and the location of the new bigint is saved as i_loc
c %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	subroutine allocate(i_bis,i_digits,i_loc)
	integer i_bis(3000)
	integer i_digits
	integer i_loc
	integer i_bii
c 	set the integer for the index to 1
	i_bii = 1

c 	while i_bis[i_bii] does not equal 0 or -1
  1	if(i_bis(i_bii)) 2,2,3

c 	jump out of the while loop 
  2	continue
  	goto 4

c 	change the index to the next available bigint
  3 	i_bii = i_bii + i_bis(i_bii)
  	i_bii = i_bii + 2
  	if (i_bii-3000) 500,501,501
  501 	write(*,*)"Too Much Bigint Memory"
  	STOP
  500	goto 1
  4	continue

c 	set the return value
  	i_loc = i_bii
  	i_bis(i_bii) = i_digits
  	i_bis(i_bii+1) = 0
  	do 5 iter = 1,i_digits
  	i_bis(i_bii+1+iter) = 0
  5	continue
  	i_bis(i_bii+1+iter) = -1
	end

c 	NORMALIZE
c %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
c 	This subroutine normalizes the bigint
c 	in other words, it takes (36)base_10 and turns it 
c 	into (6,3)base_10 (big endian of course)
c 	This function assumes that you have the necessary storage space
c 	to perform this operation. in other words your bigint needs
c 	to have extra digits if it is going to be normalized and is 
c 	too large.

c 	the add function uses this to control the output bigint
c 	then the shorten function helps out the normalize function
c 	by shortening the added value if need be
c %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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

c 	SHORTEN
c %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
c 	This subroutine shortens a bigint if it is too long for its
c 	memory size. this means that it will change its length if it can
c 	if the bigint is not at the end of the linked list it will
c 	cause link problems and overwrite problems, but it should only
c 	be used by the add function so im not as worried about that
c %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  	subroutine shorten_bigint(i_bis,i_bii)
  	integer i_bis(3000)
  	integer i_bii
  	integer i_last_index

  	i_last_index = i_bis(i_bii) + 1 + i_bii
  	i_bis(i_last_index+1) = 0
  300  	if (i_bis(i_last_index)) 301,302,301
  302	i_bis(i_bii) = i_bis(i_bii) - 1
  	i_last_index = i_last_index - 1
  	goto 300
  301	continue
  	i_bis(i_last_index+1) = -1
  	end

c 	ADD
c %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
c 	This subroutine takes 2 indexs and performs an add on the 2 
c 	bigints in the program, storing the result in a new bigint
c %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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
  115	continue
  	call normalize_bigint(i_bis,i_loc,i_digit)
  	call shorten_bigint(i_bis,i_loc)
  	end 

c 	PRINT
c %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
c 	This function takes the bigint values and turns them into
c 	a printed formatted output
c %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	subroutine print_bigint(i_bis,i_loc,i_digit)
	integer i_bis(3000)
	integer i_loc
	integer i_digit
	integer i_print_zeroes
	i_print_zeroes = 0
	do 140 iter = i_loc+i_bis(i_loc)+2,i_loc+2,-1
	call print_one_character(i_bis(iter),i_print_zeroes)
	if (i_z) 141,141,142
  142 	write(*,'(A,$)')" "
  141 	continue
  140	continue
  	write(*,'(A)')" "
	end

c 	PRINT 1 Character
c %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
c	This function helps print the entire bigint
c 	it takes in an integer and another integer that tells
c 	us whether to print 0s or not
c %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	subroutine print_one_character(i_int_value,i_z)
	integer i_int_value
	integer i_i
	integer i_z
	integer i_check_value

	if (i_int_value) 131,130,131

  130 	if (i_z) 128,129,128
  128	write(*,'(I1,$)') 0
  129	return
  131 	i_z = 1
  	i_check_value = 1
  132 	if (i_int_value - i_check_value) 134,133,133
  133 	i_check_value = i_check_value * 10
  	goto 132
  134	i_check_value = i_check_value / 10
 	i_i = i_int_value + 0
  135	if (i_check_value) 137,137,136
  136	write(*,'(I1,$)') (i_i/i_check_value)
  	i_i = i_i - (i_i/i_check_value)*i_check_value
  	i_check_value = i_check_value / 10
  	goto 135
  137 	end


c  	DEBUG_PRINT
c %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
c 	This Subroutine Prints a bigint array section, just used for
c 	debugging
c %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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

c 	DEALLOCATE
c %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
c %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	subroutine deallocate(i_bis,i_loc)
	integer i_bis(3000)
	integer i_loc
	integer index1
	integer index2
	index1 = i_loc
	index2 = index1 + i_bis(i_loc) + 2
  69	if (3000 - index2) 666,666,665
  665	i_bis(index1) = i_bis(index2)
 	index1 = index1 + 1
 	index2 = index2 + 1
	goto 69
  666	if(3000 - index1) 777,777,776
  776	i_bis(index1) = 0
	index1 = index1 + 1
	goto 666
  777	end