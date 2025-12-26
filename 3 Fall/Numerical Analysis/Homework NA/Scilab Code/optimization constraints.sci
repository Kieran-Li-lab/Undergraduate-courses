clear
for i=1:41
for j=1:31
x(i,j)=0.2*(i-1)+2+0.00001 ;
y(i,j)=0.2*(j-1)+0.00001 ;
end
end
F=x+(y+1).^2 + 0.0001*(1./(x-2) +1./y) ;
//surf(x,y,F)
// Using Gradient descent:
h=0.006
xx=5 ;
yy=5 ;
for k=1:500
dFdx= 1 -0.0001/(xx-2)^2 ;
dFdy= 2*(yy+1) - 0.0001/yy^2 ;
xx=xx-h*dFdx
yy=yy-h*dFdy
disp(k,xx,yy)
plot(xx,yy,'o')
end
