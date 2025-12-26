// Define points (xi, yi)
x = [1, 1.5, 2.8, 3.4];
y = [2, 3, 4, 5];

// Function to compute Lagrange Polynomial
function L = lagrange_polynomial(x, y, X)
    n = length(x);
    L = 0;  // Initialize the polynomial to zero
    for i = 1:n
        Li = 1;
        for j = 1:n
            if j ~= i then
                Li = Li * (X - x(j)) / (x(i) - x(j));
            end
        end
        L = L + Li * y(i);
    end
endfunction

// Generate plot points for the polynomial
X = linspace(min(x), max(x), 100);
Y = zeros(1, length(X));
for i = 1:length(X)
    Y(i) = lagrange_polynomial(x, y, X(i));
end

// Plot the Lagrange polynomial
clf;
plot(X, Y, 'b-', x, y, 'ro');
xlabel('x');
ylabel('P(x)');
title('Lagrange Polynomial');
legend('Lagrange Polynomial', 'Data Points');

// Evaluate P(2)
P2 = lagrange_polynomial(x, y, 2);
disp("P(2) = " + string(P2));

clear
x0= 1 
x1= 1.5
x2= 2.8
x3= 3.4 
y0= 2 
y1= 3
y2= 4 
y3= 5 
plot(x0,y0,'o',x1,y1,'o',x2,y2,'o',x3,y3,'o')
for i=1:1001
x= 1+ (i-1)*4*0.001
y=y0* (x-x1)*(x-x2)*(x-x3) / ((x0-x1)*(x0-x2)*(x0-x3) ) +...
 y1* (x-x0)*(x-x2)*(x-x3) / ((x1-x0)*(x1-x2)*(x1-x3) ) + ...
 y2* (x-x0)*(x-x1)*(x-x3) / ((x2-x0)*(x2-x1)*(x2-x3) ) +...
 y3* (x-x0)*(x-x1)*(x-x2) / ((x3-x0)*(x3-x1)*(x3-x2))
 xp(i)=x
 yp(i)=y
end
plot(xp,yp,'k','LineWidth',3)
disp("x="+string(xp(251)),"P(x)="+string(yp(251)))
