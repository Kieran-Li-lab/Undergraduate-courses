% Parameters
h = 0.25; % 0.25 or 0.5
N = 1 / h; 
x = linspace(0, 1, N+1); 

% Construct the coefficient matrix A
A = zeros(N-1, N-1); % Initialize the matrix
for i = 1:N-1
    if i > 1
        A(i, i-1) = 1; % 对角线下斜线
    end
    A(i, i) = -(2 + h^2); % Diagonal
    if i < N-1
        A(i, i+1) = 1; % 对角线上斜线
    end
end

% Construct the right-hand side vector b
b = zeros(N-1, 1);
b(end) = -1; % Boundary condition

% Solve the linear system
y_interior = A \ b;
y = [0; y_interior; 1]; % Full solution including boundaries

% Display the results
disp('x values:');
disp(x');
disp('y values:');
disp(y);

% Plot the results
figure;
plot(x, y, '-o', 'LineWidth', 1.5);
xlabel('x');
ylabel('y');
title('Finite Difference Solution');
grid on;
