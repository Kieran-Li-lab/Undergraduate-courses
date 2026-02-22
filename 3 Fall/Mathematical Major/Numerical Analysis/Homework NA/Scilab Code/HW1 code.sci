x=1
for k=1:1000
   y=exp(2*x)-2.5+1/(12+x)
   dy=2*exp(2*x)-1/(12+x)^2
   x=x-y/dy 
end
disp(x,y)

x=0.4418065
