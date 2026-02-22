x=[1.3 1.4 1.5 1.6 1.7 1.8];
y=[3.3 3.4 3.85 4.25 4.50 4.85];
xy=[x;y];
plot(x,y,'o','LineWidth',3);

a0=[0;0;0;0];
 
[a,err]=datafit(J,xy,a0);   %不正确的函数引用 

t=1.3:0.01:1.8;
poly=a(1)+a(2)*t+a(3)*t.^2+a(4)*t.^3;
plot(t,poly,'r');
xgrid
disp(t(41), poly(41))

function [fun]=J(a, xy)
fun=xy(2)-a(1)-a(2)*xy(1)-a(3)*xy(1)^2-a(4)*xy(1)^3;
end



