% Initial Value Problem: dy/dx = y * (1 + x^2), y(0) = 1
% Fourth-Order Runge-Kutta Method Implementation

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

% Runge-Kutta 4th order method iterations
for i = 1:(length(x) - 1)
    % Compute Runge-Kutta terms
    k1 = f(x(i), y(i));
    k2 = f(x(i) + h / 2, y(i) + h / 2 * k1);
    k3 = f(x(i) + h / 2, y(i) + h / 2 * k2);
    k4 = f(x(i) + h, y(i) + h * k3);
    
    % Update y using weighted average of slopes
    y(i + 1) = y(i) + (h / 6) * (k1 + 2 * k2 + 2 * k3 + k4);
end

% Display results
disp('Results using Fourth-Order Runge-Kutta Method:');
disp('x values:');
disp(x');
disp('y values:');
disp(y');

% Plot the results
figure;
plot(x, y, '-o', 'LineWidth', 1.5);
title('Fourth-Order Runge-Kutta Method Solution');
xlabel('x');
ylabel('y');
grid on;
