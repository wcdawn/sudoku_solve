program sudokusolve
IMPLICIT NONE

integer,dimension(9,9) :: input
integer :: ios
integer :: i, j
logical,dimension(9,9) :: original
character(80) :: fname

101 format(a)
102 format(i2) ! 2 integers (single integer w/ space)

write(*,101) 'input sudoku file name'
write(*,101) 'blank spaces as 0'
read(*,*) fname

open(unit = 11, file = fname, status = 'old', action = 'read', iostat = ios)
if (ios .ne. 0) then
	write(*,'(a,i3,a,a)') 'error opening unit', 11, ' -- ', fname
	stop('END PROGRAM')
endif

do i = 1,9
	read(11,*) input(i,:)
enddo

do i = 1,9
	do j = 1,9
		if (input(i,j) .eq. 0) then
			original(i,j) = .false.
		else
			original(i,j) = .true.
		endif
	enddo
enddo


! break this into 'sudokuwrite' method/subroutine
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
! break this into 'sudokuwrite' method/subroutine



endprogram sudokusolve