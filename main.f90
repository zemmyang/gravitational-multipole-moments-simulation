PROGRAM main
IMPLICIT NONE

    REAL, PARAMETER :: PI = 3.14159265
    REAL, PARAMETER :: GM = 39.4784176
    REAL, PARAMETER :: m = 3e-3
    REAL, DIMENSION(:), ALLOCATABLE :: r
    REAL :: rstep
    REAL :: phi, res
    INTEGER :: n,i
    REAL :: r0,rf,radius
    REAL, EXTERNAL :: func, multipolev
    character(len=20) :: filename

    ! Ask the user for input
    WRITE(*,*) "This program calculates the exact potential"
    WRITE(*,*) "of the ring on the equatorial plane."
    WRITE(*,*) "radius"
    READ(*,*) radius
    WRITE(*,*) "Set the range of the R"
    READ(*,*) r0, rf
    WRITE(*,*) "Step size of R"
    READ(*,*) rstep
    WRITE(*,*) "Filename of output"
    READ(*,*) filename

    n = INT( (rf-r0)/rstep )
    ALLOCATE(r(n))

    r(1) = r0
    DO i = 1,n-1
    r(i+1) = r(i) + rstep
    END DO

    ! This program generates five files:
    ! filename.exact where the program will create two-columns
    ! of data, the first for r and the second for the exact potential
    OPEN ( unit = 101, file = trim(filename)//".exact", status = ’replace’, action = ’write’)
    ! filename.mon, used to generate a r vs monopole potential at r
    OPEN ( unit = 103, file = trim(filename)//".mon", status = ’replace’, action = ’write’)
    ! filename.monquad, used to generate a r vs monopole and quadrupole potential
    OPEN ( unit = 104, file = trim(filename)//".monquad", status = ’replace’, action = ’write’)
    ! filename.monerr, used to generate a r vs the error assuming that
    ! we only use the monopole potential only
    OPEN ( unit = 105, file = trim(filename)//".monerr", status = ’replace’, action = ’write’)
    ! filename.mqerr, used to generate a r vs the error assuming that
    ! we only use the monopole and quadrupole potential
    OPEN ( unit = 106, file = trim(filename)//".mqerr", status = ’replace’, action = ’write’)

    DO i=1,n

    ! given a certain r(i), the subroutine qromb integrates the potential
    ! and stores it in the variable res. this is then written to file
    CALL qromb(func,0.0,2*PI,res,r(i),radius)
    WRITE(101,200) r(i), -(res*GM)/(2*PI)

    WRITE(103,200) r(i), multipolev(r(i),PI/2,radius, 0)
    WRITE(104,200) r(i), multipolev(r(i),PI/2,radius, 2)
    WRITE(105,200) r(i), ABS(-(res*GM)/(2*PI)) - ABS(multipolev(r(i),PI/2,radius, 0))
    WRITE(106,200) r(i), ABS(-(res*GM)/(2*PI)) - ABS(multipolev(r(i),PI/2,radius, 2))
    END DO

    200 FORMAT (f12.9, 3x, f12.3)
END PROGRAM
