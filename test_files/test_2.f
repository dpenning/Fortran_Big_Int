        program test_2
        integer i_bis(3000)
        integer i_tmp

        write(*,*)""
        write(*,*)"------------"
        write(*,*)"TEST 2"
        write(*,*)"------------"
        write(*,*)""
        write(*,*)"This program tests the add"
        write(*,*)"Check the memtrace to make sure it worked"

        call init(i_bis)
        call allocate(i_bis,4,i_tmp)
        i_bis(3) = 1
        call print_bigint_debug(1,16,i_bis)
        call add_bigints(i_bis,i_tmp,i_tmp,10,i_loc)
        call print_bigint_debug(1,16,i_bis)
        end program test_2