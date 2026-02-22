% Matrix
A = [2, 11, 3, 6;
     11, 5, 11, 8;
     3, 12, 5, 4;
     7, 5, 2, 3];

% Initial guess
x =[-5;6;-4;1];%[1;1;1;1]get max

% Iteration setting
tolerance = 1e-6; % Stop condition(epislon)
max_iter = 1000; % Max iteration step
lamda_old = 0; % Initial eigenvalue

for k = 1:max_iter
    y = A * x;
    x = y / norm(y); % Normlize x
    lamda = x' * A * x; % Calculate current eigenvalue

    if abs(lamda - lamda_old) < tolerance % Check convergence
        fprintf('Iteration stopped at %d th step。\n', k);
        break;
    end

    lamda_old = lamda; % Update eigenvalue
end

fprintf('Max eigenvalue: %.6f\n', lamda);
disp('Corresponding eigenvector:');
disp(x);
