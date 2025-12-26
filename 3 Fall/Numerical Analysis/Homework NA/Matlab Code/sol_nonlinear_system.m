x= 1;
y= 1;
z= 1;
X=[x y z]';
for i=1:15
 f=x*x+y*y+z*z-1;
 g=2*x*x +y*y -4*z;
 h=3*x*x-4*y+z*z;
 F=[f g h]';
 dfdx=2*x;
 dfdy=2*y;
 dfdz=2*z;
 dgdx=4*x;
 dgdy=2*y;
 dgdz=-4;
 dhdx=6*x;
 dhdy=-4;
 dhdz=2*z;
 Fderivat=[dfdx dfdy dfdz; dgdx dgdy dgdz; dhdx dhdy dhdz];
 X=X-inv(Fderivat)*F;
 x=X(1);
 y=X(2);
 z=X(3);
 disp(X); 
end