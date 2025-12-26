% MATLAB code for solving the problem using Galerkin's method

% Number of basis functions (higher number gives better approximation)
n = 1; % You can adjust n to get more accurate results

% Define the basis functions (here, we use linear basis functions)
phi = @(x, i) x.^i - x.^(i+1);  % Example: Linear basis functions
d_phi=@(x, i) i*x.^(i-1)-(i+1)* x.^i;
dd_phi=@(x, i) i*(i-1)*x.^(i-2)-(i+1)*i* x.^(i-1);
f = @(x) x.^2; % Right-hand side function x^2

% Initialize coefficient matrix and right-hand side vector
A = zeros(n, n);
b = zeros(n, 1);

% Loop over the basis functions
for i = 1:n
    for j = 1:n
        integrand = @(x) phi(x, i) .* (dd_phi(x,i)+phi(x,i)); 
        A(i, j) = integral(integrand, 0, 1); 
    end
    % Compute the right-hand side
    integrand_b = @(x) f(x) .* phi(x, i);
    b(i) = integral(integrand_b, 0, 1);
end

% Solve the system of equations
a = A\b;

% Now compute the value of y(0.5) using the obtained coefficients
x_value = 0.5;
y_at_0_5 = 0; % Initialize y(0.5)

for i = 1:n
    y_at_0_5 = y_at_0_5 + a(i) * phi(x_value, i);
end

% Display the approximate value of y(0.5)
disp(['The approximate value of y(0.5) is: ', num2str(y_at_0_5)]);
