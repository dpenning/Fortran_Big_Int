c 	This Project can store and manipulate multiple bigints.
c 	We allocate space for a linked list of bigintegers
c 	Each BigInt is stored with its size as the first int
c 	and its sign as the second int. The rest of the integers
c 	from [2,size+2] are the values of the bigint
c 	The system is "BigEndian" in that the least signifigant
c 	

c 	David Pennington
c 	Nick Graczyk

	program project_1
	integer i_bigint_storage(3000)
	
	call init_bigint_storage_space(i_bigint_storage)
	call print_bigint_debug(1,16,i_bigint_storage)
	call allocate(4,i_bigint_storage)
	call print_bigint_debug(1,16,i_bigint_storage)
	call allocate(5,i_bigint_storage)
	call print_bigint_debug(1,16,i_bigint_storage)
	end

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


	subroutine print_bigint_debug(i_start,i_stop,i_bis)
	integer i_start
	integer i_stop
	integer i_bis(3000)
	write(*,*)"-----------"
	do 7 iter = i_start,i_stop,4
	write(*,*) i_bis(iter),i_bis(iter+1),i_bis(iter+2),i_bis(iter+3)
  7	write(*,*)"-----------"
  	end