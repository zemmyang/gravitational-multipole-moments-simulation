FUNCTION multipolev(r,th,a,mo)
IMPLICIT NONE

    REAL :: r,th,a,multipolev
    integer :: mo
    REAL, PARAMETER :: PI = 3.14159265
    REAL, PARAMETER :: GM = 39.4784176

    if (mo == 0) then
        IF (r>a) THEN
            multipolev = -GM/r
        ELSE IF (r<a) THEN
            multipolev = -GM/a
        ELSE
            WRITE(*,*) "r!=a"
            STOP
        END IF

    else if (mo == 1) then
        IF (r>a) THEN
            multipolev = -GM/r
        ELSE IF (r<a) THEN
            multipolev = -GM/a
        ELSE
            WRITE(*,*) "r!=a"
            STOP
        END IF

    else if (mo == 2) then
        IF (r>a) THEN
            multipolev = -GM/r + ((GM*a**2)/(4.0*r**3))*((3*(COS(th)**2))-1)
        ELSE IF (r<a) THEN
            multipolev = -GM/a + ((GM*r**2)/(4.0*a**3))*((3*(COS(th)**2))-1)
        ELSE
            WRITE(*,*) "r!=a"
            STOP
        END IF
    
    else
        write(*,*) "moment unavailable"
    end if

    RETURN
END FUNCTION multipolev