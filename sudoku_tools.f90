module sudokutools
IMPLICIT NONE

contains

subroutine sudokuwrite(input)
  IMPLICIT NONE
  integer,dimension(9,9),intent(in) :: input
  integer :: i, j

  101 format(a)
  102 format(i2)
  
  write(*,101)
  write(*,101) '-------------------------'
  do i = 1,9
    if ((i .eq. 4) .or. (i .eq. 7)) then
      write(*,101) '|-------+-------+-------|'
    endif
    write(*,101,advance='no') '|'
    do j = 1,9
      if ((j .eq. 4) .or. (j .eq. 7)) then
        write(*,101,advance='no') ' |'
      endif
      write(*,102,advance='no') input(i,j)
    enddo
    write(*,101) ' |'
  enddo
  write(*,101) '-------------------------'
  write(*,101)

endsubroutine sudokuwrite

endmodule sudokutools