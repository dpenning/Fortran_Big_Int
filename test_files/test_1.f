        program test_1
        integer i_bis(3000)
        integer i_tmp

        write(*,*)""
        write(*,*)"------------"
        write(*,*)"TEST 1"
        write(*,*)"------------"
        write(*,*)""
        write(*,*)"test allocate"

        call init(i_bis)
        call print_bigint_debug(1,16,i_bis)
        call allocate(i_bis,4,i_tmp)
        call print_bigint_debug(1,16,i_bis)
        end program test_1