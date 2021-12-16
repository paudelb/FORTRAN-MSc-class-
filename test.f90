program arith
implicit none
real :: X,Y,Z,W,P
X = 638
Y = 2157
Z = 89
W = 198
P = X-(Y+Z**2)/W
print* , "The required value is", P
end program
