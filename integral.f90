FUNCTION func(r,th,a)
    REAL r,th,a,func
    func = 1/SQRT(r**2+a**2-(2*a*r*COS(th)))
    RETURN
END FUNCTION func