% Define the problem
f = @(x, y) y * (1 + x^2); % ODE
y_exact = @(x) exp((x.^3)/3 + x); % exact solution
x0 = 0; % initial value
y0 = 1; % initial value
h = 0.1; % step
x_vals = [0.2, 0.6, 1.0]; 
N = 100;
% Method Euler
fprintf('Euler Method:\n');
[x_euler, y_euler] = euler_method(f, x0, y0, h, max(x_vals));
disp_results(x_vals, x_euler, y_euler, y_exact);

% Method Modifier-Euler
fprintf('\nModified Euler Method:\n');
[x_mod_euler, y_mod_euler] = modified_euler_method(f, x0, y0, h, max(x_vals),N);
disp_results(x_vals, x_mod_euler, y_mod_euler, y_exact);

% 4th order Runge-Kutta Method
fprintf('\nFourth-Order Runge-Kutta Method:\n');
[x_rk4, y_rk4] = runge_kutta_4(f, x0, y0, h, max(x_vals));
disp_results(x_vals, x_rk4, y_rk4, y_exact);

% Function Euler 
function [x, y] = euler_method(f, x0, y0, h, x_end)
    n = ceil((x_end - x0) / h);
    x = x0:h:(x0 + n * h);
    y = zeros(size(x));
    y(1) = y0;
    for i = 1:n
        y(i+1) = y(i) + h * f(x(i), y(i));
    end
    x = x(1:length(y));
end

% Function modifiered-Euler
function [x, y] = modified_euler_method(f, x0, y0, h, x_end, N)
    n = ceil((x_end - x0) / h); 
    x = x0:h:(x0 + n * h); 
    y = zeros(size(x)); 
    y(1) = y0; 
    for i = 1:n
        y_predict = y(i) + h * f(x(i), y(i));
        for k = 1:N
            y_predict = y(i) + (h / 2) * (f(x(i), y(i)) + f(x(i+1), y_predict));
        end
        y(i+1) = y_predict;
    end
    x = x(1:(n+1)); 
end

% 4th ordered Runge-Kutta
function [x, y] = runge_kutta_4(f, x0, y0, h, x_end)
    n = ceil((x_end - x0) / h);
    x = x0:h:(x0 + n * h);
    y = zeros(size(x));
    y(1) = y0;
    for i = 1:n
        k1 = h * f(x(i), y(i));
        k2 = h * f(x(i) + h / 2, y(i) + k1 / 2);
        k3 = h * f(x(i) + h / 2, y(i) + k2 / 2);
        k4 = h * f(x(i) + h, y(i) + k3);
        y(i+1) = y(i) + (k1 + 2*k2 + 2*k3 + k4) / 6;
    end
    x = x(1:length(y));
end

% displace the result
function disp_results(x_vals, x_comp, y_comp, y_exact)
    for x = x_vals
        idx = find(abs(x_comp - x) < 1e-6);
        fprintf('x = %.1f: y_computed = %.6f, y_exact = %.6f, error = %.6f\n', ...
                x, y_comp(idx), y_exact(x), abs(y_comp(idx) - y_exact(x)));
    end
end
