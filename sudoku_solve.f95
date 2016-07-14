program sudokusolve
use sudokutools
IMPLICIT NONE

integer,dimension(9,9) :: input, work
integer :: ios
integer :: i, j, guess, a, b
integer :: bound_l, bound_r, bound_u, bound_d
logical,dimension(9,9) :: original
logical :: error
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

call sudokuwrite(input)

do i = 1,9
	do j = 1,9
		if (input(i,j) .eq. 0) then
			original(i,j) = .false.
		else
			original(i,j) = .true.
		endif
	enddo
enddo

bound_l = 0
bound_r = 0
bound_u = 0
bound_d = 0
work = input
do i = 1,9
	do j = 1,9
		if (.not. original(i,j)) then
			select case (i)
			case (1:3)
				bound_l = 1
				bound_r = 3
			case(4:6)
				bound_l = 4
				bound_r = 6
			case(7:9)
				bound_l = 7
				bound_r = 9
			endselect
			select case(j)
			case (1:3)
				bound_u = 1
				bound_d = 3
			case(4:6)
				bound_u = 4
				bound_d = 6
			case(7:9)
				bound_u = 7
				bound_d = 9
			endselect

			do guess = 1,9
				work(i,j) = guess
				error = .false.

				! row check
				do a = 1,9
					if ((work(i,j) .eq. work(i,a)) .and. (a .ne. j)) then
						! bad guess
						error = .true.
						exit
					endif
				enddo
				! col check
				do a = 1,9
					if ((work(i,j) .eq. work(a,j)) .and. (a .ne. i)) then
						! bad guess
						error = .true.
						exit
					endif
				enddo
				! sector check
				do a = bound_l,bound_r
					do b = bound_u,bound_d
						if ((work(i,j) .eq. work(a,b)) .and. (a .ne. i) .and. (b .ne. j)) then
							! bad guess
							error = .true.
						endif
					enddo
				enddo

				if (.not. error) then
					exit
				endif
				if ((error) .and. (guess .eq. 9)) then
					write(*,'(a,2i3)') 'couldnt guess - ', i, j
					! stop
				endif
			enddo











		endif
	enddo
enddo

call sudokuwrite(work)


endprogram sudokusolve