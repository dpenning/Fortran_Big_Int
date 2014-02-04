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
        integer i_tmp
        integer i_tmp_2

        i_base = 1000
        i_iterations = 50

        call init(i_bigint_storage)
        call allocate(i_bigint_storage,1,i_tmp)
        i_bigint_storage(3) = 1
        call print_bigint(i_bigint_storage,i_tmp,i_base)
        call add_bigints(i_bigint_storage,1,1,i_base,i_tmp)
        call print_bigint(i_bigint_storage,i_tmp)
        do 400 iter = 2,i_iterations
        call add_bigints(i_bigint_storage,i_tmp,i_tmp,i_base,i_tmp_2)
        i_tmp = i_tmp_2
        call print_bigint(i_bigint_storage,i_tmp)
  400   continue

        end program project_1