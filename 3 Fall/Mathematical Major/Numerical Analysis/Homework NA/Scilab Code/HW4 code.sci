clear
for i=1:41
for j=1:41 
 x(i,j)=0.1*(i-1)-2
 y(i,j)=0.1*(j-1)-2 end
end
F=sin(2.*x+1.5.*y-2)+0.5.*y.*(y-4/(12+1))
surf(x,y,F)
h=0.01 
xx=0
yy=0
plot(2,2)
for k=1:3000
 dFdx=2*cos(2.*xx+1.5.*yy-2)
 dFdy=1.5*cos(2.*xx+1.5.*yy-2)+yy-2/13
 xx=xx-h*dFdx 
 yy=yy-h*dFdy
 plot(xx, yy,'or','LineWidth',1)
end
disp('xx=',xx,'yy=',yy,'k=',k)




 
