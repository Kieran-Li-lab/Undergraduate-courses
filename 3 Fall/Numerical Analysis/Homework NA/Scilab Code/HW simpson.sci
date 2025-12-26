clear
// Function to integrate
function y = f(x)
    y=exp(x .* sin(1 ./ (1 + x)))
endfunction

// Simpson's rule implementation
function integral = simpsons_rule(a, b, n)
    h = (b - a) / n;
    sum = f(a) + f(b);
    for i = 1:n-1
        x = a + i * h;
        if bitand(i, 1) == 0 then
            sum = sum + 2 * f(x);
        else
            sum = sum + 4 * f(x);
        end
    end

    integral = (h / 3) * sum;
endfunction

// Calculate the integral with n = 50
a = 0;
b = 2;
n = 50;
result = simpsons_rule(a, b, n);
disp("Integral result = " + string(result));




clear
x= 0:0.04:2
y=exp(x .* sin(1 ./ (1 + x)))
h=0.04
s= y(1)+y(51)
for i=1:24
s=s+2*y(2*i+1)
s=s+4*y(2*i)
end
s=s+4*y(50)
Int=h*s/3
disp(Int)
