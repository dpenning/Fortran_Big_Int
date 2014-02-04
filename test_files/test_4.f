        program test_1
        integer i_bis(3000)
        integer i_tmp
        write(*,*)"This program tests the allocate"
        write(*,*)"should give us a debug print with"
        call init(i_bis)
        call allocate(i_bis,4,i_tmp)
        end program test_1