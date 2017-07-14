program sudokusolve
use sudokutools
IMPLICIT NONE

integer,dimension(9,9) :: input, work
! integer,dimension(81,81) :: blank_coords
! integer :: blank_count
integer :: direction
integer :: start_guess

integer :: ios
integer :: i, j, guess, a, b
integer :: x, y
integer :: bound_l, bound_r, bound_u, bound_d
logical,dimension(9,9) :: original
logical :: error
character(80) :: fname

real :: final_time, init_time

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

close(unit = 11)

call cpu_time(init_time)
write(*,101)
write(*,101) '-------------------------'
write(*,101) 'input echo'
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

x = 1
y = 1
direction = 1
i = 0
! do i = 1,10000
do
	i = i + 1
	if (i .eq. 100000) then
		stop('failed to solve with 100000 iterations')
	endif
	if (.not. original(y,x)) then
		! write(12,'(i7,a,i2,a,i2)') i, ',', x, ',', y
		select case (y)
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
		select case(x)
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

		if (work(y,x) .eq. 9) then
			work(y,x) = 0
			x = x + direction
			if (x .eq. 0) then
				x = 9
				y = y - 1
			elseif (x .eq. 10) then
				x = 1
				y = y + 1
			endif
			cycle
		endif
		do guess = (work(y,x) + 1),9
			work(y,x) = guess
			error = .false.

			! row check
			do a = 1,9
				if ((work(y,x) .eq. work(y,a)) .and. (a .ne. x)) then
					! bad guess
					error = .true.
					exit
				endif
			enddo
			! col check
			do a = 1,9
				if ((work(y,x) .eq. work(a,x)) .and. (a .ne. y)) then
					! bad guess
					error = .true.
					exit
				endif
			enddo
			! sector check
			do a = bound_l,bound_r
				do b = bound_u,bound_d
					if ((work(y,x) .eq. work(a,b)) .and. (a .ne. y) .and. (b .ne. x)) then
						! bad guess
						error = .true.
					endif
				enddo
			enddo

			if (.not. error) then
				! good guess
				direction = 1
				! write(*,101) 'good guess'
				! call sudokuwrite(work)
				exit
			endif
			if ((error) .and. (guess .eq. 9)) then
				! write(*,'(i7,a,2i3)') i,' couldnt guess - ', y, x
				work(y,x) = 0
				direction = -1
				! stop
			endif
		enddo
	endif

	x = x + direction
	if (x .eq. 10) then
		x = 1
		y = y + 1
	elseif (x .eq. 0) then
		x = 9
		y = y - 1
	endif
	if (y .eq. 10) then
		write(*,101) 'completed'
		exit
	endif
	
	if (y .eq. 0) then
		write(*,101) 'ERROR y = 0, out of coords'
		exit
	endif
enddo

call cpu_time(final_time)
write(*,'(i6,a)') i, ' - iterations'
! write(*,'(e12.6,a)') (final_time - init_time), ' - seconds of runtime'
write(*,*)

write(*,101) '-------------------------'
write(*,101) 'solution'
call sudokuwrite(work)


endprogram sudokusolve