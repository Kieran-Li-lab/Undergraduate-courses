f = @(x) exp(x .* sin(cos(sin(x))));
a = 0;
b = 1;
Q = integral(f, a, b, 'AbsTol', 1e-6, 'RelTol', 1e-6);