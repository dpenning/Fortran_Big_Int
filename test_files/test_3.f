        program test_3
        integer i_bis(3000)
        integer i_tmp
        write(*,*)""
        write(*,*)"------------"
        write(*,*)"TEST 3"
        write(*,*)"------------"
        write(*,*)""
        write(*,*)"This program tests the deallocate"
        write(*,*)"Check the memtrace to make sure it worked"
        call init(i_bis)
        call print_bigint_debug(1,16,i_bis)
        call allocate(i_bis,4,i_tmp)
        call print_bigint_debug(1,16,i_bis)
        call deallocate(i_bis,i_tmp)
        call print_bigint_debug(1,16,i_bis)
        end program test_3