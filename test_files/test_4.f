        program test_4
        integer i_bis(3000)
        integer i_tmp
        write(*,*)""
        write(*,*)"------------"
        write(*,*)"TEST 4"
        write(*,*)"------------"
        write(*,*)""
        write(*,*)"This program tests the over allocate"
        write(*,*)"if you see Test Failed, it Failed"
        write(*,*)"You should see a memory allocation warning"
        write(*,*)"and the program will shut down"
        call init(i_bis)
        do 700 iter = 1,1000
        call allocate(i_bis,4,i_tmp)
  700   continue
        write(*,*)"Test Failed"
        end program test_4