SUBROUTINE polint(xa,ya,n,x,y,dy)
    INTEGER n,nmax
    REAL dy,x,y,xa(n),ya(n)
    PARAMETER (nmax=10)
    INTEGER i,m,ns
    REAL den,dif,dift,ho,hp,w,c(nmax),d(nmax)

    ns=1
    dif=ABS(x-xa(1))

    DO i=1,n
        dift=ABS(x-xa(i))
        IF (dift.LT.dif) THEN
            ns=i
            dif=dift
        ENDIF
        c(i)=ya(i)
        d(i)=ya(i)
    END DO

    y=ya(ns)
    ns=ns-1

    DO m=1,n-1
        DO i=1,n-m
            ho=xa(i)-x
            hp=xa(i+m)-x
            w=c(i+1)-d(i)
            den=ho-hp

            IF(den.EQ.0) STOP !’failure in polint’
            den=w/den
            d(i)=hp*den
            c(i)=ho*den
        END DO

        IF (2*ns.LT.n-m) THEN
            dy=c(ns+1)
        ELSE
            dy=d(ns)
            ns=ns-1
        END IF

    y=y+dy

    END DO

    RETURN
END SUBROUTINE polint