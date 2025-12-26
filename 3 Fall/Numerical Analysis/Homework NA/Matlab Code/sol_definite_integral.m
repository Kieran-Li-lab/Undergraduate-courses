%Method 1 
n=100;
h=1/n;
x= 0: h :1; 
y= exp(x.*sin(cos(sin(x)))); 
Int=(y(1)+y(n+1))/2 ;
for i=2: n 
    Int=Int+y(i); 
end 
Int=h*Int ;
disp(Int) ;
plot(x,y,'r');

hold on;

%Method 2

x= 0 : 0.00001 : 1 ;
y=exp(x.*sin(cos(sin(x)))) ;
Int=trapz(x,y);
plot(x,y,'b');
grid on;

