SUBROUTINE qromb(func,a,b,ss,r,radius)
    INTEGER :: jmax, jmaxp, k, km
    REAL :: a, b, func, ss, eps
    REAL :: r, radius
    EXTERNAL :: func
    PARAMETER (eps=1.e-6, jmax=20, jmaxp=jmax+1, k=5, km=k-1)
    INTEGER :: j
    REAL :: dss, h(jmaxp), s(jmaxp)

    h(1) = 1

    DO j=1, jmax
        CALL trapzd(func,a,b,s(j),j,r,radius)

        IF (j .GE. k) THEN
            CALL polint(h(j-km),s(j-km),k,0.,ss,dss)
        IF (ABS(dss).LE.eps*ABS(ss)) RETURN
        END IF

        s(j+1) = s(j)
        h(j+1) = 0.25*h(j)
    END DO

    STOP !’too many steps in qromb’
END SUBROUTINE qromb
