% Define symbols
syms x c1 c2

% Basis functions
phi1 = x*(1-x);
phi2 = x^2*(1-x);

% Approximate solution
y_approx = c1*phi1 + c2*phi2;

% Residual
R = diff(y_approx, x, 2) + y_approx - x^2;

% Orthogonality conditions
eq1 = int(R * phi1, 0, 1);
eq2 = int(R * phi2, 0, 1);

% Solve for coefficients c1 and c2
coeffs = solve([eq1 == 0, eq2 == 0], [c1, c2]);

% Substitute coefficients into the approximate solution
y_approx = subs(y_approx, [c1, c2], [coeffs.c1, coeffs.c2]);

% Evaluate y(0.5)
y_half = subs(y_approx, x, 0.5);

% Display the result
disp('The approximate value of y(0.5) is:');
disp(vpa(y_half, 6));
