
x = [1, 2];
y = [2, 2];

pp = [2, 2]; 

tck = csape(x, [pp; y], 'variational');

t_values = [1/3, 2/3];

spline_values = fnval(tck, t_values);

fprintf('Spline value at t=1/3,t=2/3: [%.3f, %.3f]\n', spline_values(1, 1), spline_values(2, 1));
