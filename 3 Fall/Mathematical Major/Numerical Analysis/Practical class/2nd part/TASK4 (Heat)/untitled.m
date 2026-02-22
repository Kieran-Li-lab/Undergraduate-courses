% Define parameters
a = 1;                                  % Coefficient a
f = @(x, t) 0;                          % Source term f(x,t)
g = @(x) 0 * x;                         % Initial condition g(x)
v1 = @(t) 0;                             % Boundary condition v1(t)
v2 = @(t) t;                             % Boundary condition v2(t)

x0 = 0;                                  % Starting value of x
xm = 1;                                  % Ending value of x
T = 1;                                   % Total time duration

h = 1/4;                                 % Spatial step size
tau = 1/160;                              % Time step size
lambda = tau / h^2;                      % Calculate lambda

n = round(T / tau) + 1;                  % Number of time steps
m = round((xm - x0) / h) + 1;            % Number of spatial nodes

% Initialize result matrix
u = zeros(n, m);

xx = x0:h:xm;                            % Spatial discretization

% Set initial condition
u(1, :) = g(xx);

% Time-stepping loop
for i = 2:n
    
    % Construct the coefficient matrix A
    A = create_matrix(m, a, lambda);
    
    % Apply boundary conditions to the first and last rows of A
    A(1, :) = [1, zeros(1, m-1)];         % Boundary condition at x0
    A(end, :) = [zeros(1, m-1), 1];       % Boundary condition at xm
    
    % Compute the right-hand side vector b
    b1 = v1((i-1) * tau);                % Compute b1
    b2 = calculate_b2(u, i, a, lambda, tau, f, xx); % Compute b2
    b3 = v2((i-1) * tau);                % Compute b3
    
    % Combine the components into the vector b
    b = [b1; b2'; b3];
    
    % Solve the linear system A * temp = b
    temp = A \ b;
    
    % Update the solution at the current time step
    u(i, :) = temp;
end

% Find the value of u at a specific point
x_index = find(xx == 1/2);              % Find index of x = 1/2
t_index = round((1/8) / tau) + 1;       % Find index of t = 1/8

% Display the result at the specified point
disp(['u(t=1/8, x=1/2) = ', num2str(u(t_index, x_index))]);

% ---------------------------------
% Subfunction: Create coefficient matrix A
function A = create_matrix(m, a, lambda)
    A = diag(ones(1, m) * (1 + a * lambda)) + ...
        diag(ones(1, m-1) * (-a * lambda / 2), -1) + ...
        diag(ones(1, m-1) * (-a * lambda / 2), 1);
end

% ---------------------------------
% Subfunction: Compute b2
function b2 = calculate_b2(u, i, a, lambda, tau, f, xx)
    b2 = a * lambda / 2 * u(i-1, 3:end) + (1 - a * lambda) * u(i-1, 2:end-1) + a * lambda / 2 * u(i-1, 1:end-2);
    b2 = b2 + tau / 2 * (f(xx(2:end-1), (i-1) * tau) + f(xx(2:end-1), (i-2) * tau));
end
