SUBROUTINE trapzd(func,a,b,s,n,r,radius)
    INTEGER :: n
    REAL :: a,b,s,func
    REAL :: r, radius
    EXTERNAL func
    INTEGER :: it,j
    REAL :: del,sum,tnm,x
    
    IF (n.EQ.1) THEN
        s=0.5*(b-a)*(func(r,a,radius)+func(r,b,radius))
    ELSE
        it = 2**(n-2)
        tnm=it
        del=(b-a)/tnm
        x=a+0.5*del
        sum = 0.0
    
    DO j=1, it
        sum=sum+func(r,x,radius)
        x=x+del
    END DO
    
    s=0.5*(s+(b-a)*sum/tnm)
    
    ENDIF
    RETURN
END SUBROUTINE trapzd
    