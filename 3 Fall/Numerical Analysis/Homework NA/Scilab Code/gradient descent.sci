for i=1:41 
for j=1:31 
 x(i,j)=0.125*(i-1)-2
 y(i,j)=0.2*(j-1)-2 end
end
F=x.^4+y.^4 -5*y+x
surf(x,y,F)

h=0.01 
xx=2.5
yy=3.5
plot(4,5)
for k=1:300
dFdx=4*xx^3 +1
dFdy= 4*yy^3-5
xx=xx-h*dFdx 
yy=yy-h*dFdy
plot(xx, yy,'or','LineWidth',2)
end
disp('xx=',xx,'yy=',yy,'k=',k)
