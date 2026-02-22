% Solving the 1D Wave Equation using Three-Level Finite Difference Scheme

L = 1;                 
T = 0.4;               
h = 0.25;              
k = 0.2;               
c = 1;                 
% Stability parameter
alpha = c * k / h;


% Grid setup
x = 0:h:L;             % Spatial grid
t = 0:k:T;             % Time grid
nx = length(x);        % Number of spatial points
nt = length(t);        % Number of time steps

% Initialize solution
u = zeros(nx, nt);     

u(:,1) = sin(pi * x); 

% First time step using initial velocity (du/dt = 0)
u(2:end-1,2) = u(2:end-1,1) + (alpha^2 / 2) * (u(1:end-2,1) - 2*u(2:end-1,1) + u(3:end,1));

% Time stepping loop using three-level scheme
for k_idx = 2:nt-1
    for j = 2:nx-1
        u(j,k_idx+1) = -u(j,k_idx-1) + 2*(1 - alpha^2)*u(j,k_idx) ...
                       + alpha^2 * (u(j-1,k_idx) + u(j+1,k_idx));
    end
end

% Extract Solution
u_exact = sin(pi * 0.5) * cos(pi * 0.4); % Exact solution at (0.5, 0.4)

% Output the result
fprintf('Numerical solution u(0.5, 0.4): %.6f\n', u_approx);
fprintf('Exact solution u(0.5, 0.4): %.6f\n', u_exact);
fprintf('Error: %.6f\n', abs(u_exact - u_approx));
fprintf('check alpha<1,alpha=:%.6f\n',alpha); 

