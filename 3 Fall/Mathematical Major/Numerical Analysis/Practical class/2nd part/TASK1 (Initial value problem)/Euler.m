% Initial Value Problem: dy/dx = y * (1 + x^2), y(0) = 1
% Euler Method Implementation

% Parameters
f = @(x, y) y * (1 + x^2); % Differential equation
x0 = 0; % Initial x
y0 = 1; % Initial y
h = 0.2; % Step size
x_end = 1.0; % End of the interval

% Discretization
x = x0:h:x_end; % Generate x values
y = zeros(size(x)); % Preallocate y array
y(1) = y0; % Initial condition

% Euler method iterations
for i = 1:(length(x) - 1)
    y(i + 1) = y(i) + h * f(x(i), y(i)); % Euler formula
end

% Display results
disp('Results using Euler Method:');
disp('x values:');
disp(x');
disp('y values:');
disp(y');

% Plot the results
figure;
plot(x, y, '-o', 'LineWidth', 1.5);
title('Euler Method Solution');
xlabel('x');
ylabel('y');
grid on;
