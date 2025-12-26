
A = [-1 -2 -2; 1 2 2];
b = [0; 72];
x0 = [10; 10; 10];
[x,fval] = fmincon(@J,x0,A,b);

function [fun]=J(x)
fun = -x(1)*x(2)*x(3);
end

