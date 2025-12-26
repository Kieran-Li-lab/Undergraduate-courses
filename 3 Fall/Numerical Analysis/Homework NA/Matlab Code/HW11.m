%initialization
N = 4;
iter = 1000;
u = zeros(N,iter);

for k = 1:iter
    u(1,k+1) = 1/2 * u(2,k)-2;
    u(2,k+1) = 1/4 * (u(1,k+1)+u(4,k))-8;
    u(3,k+1) = u(2,k+1);
    u(4,k+1) = 1/2 * u(2,k+1)-32;
end

fprintf('Numerical solution u_1: %.6f\n', u(1,iter));
fprintf('Numerical solution u_2: %.6f\n', u(2,iter));
fprintf('Numerical solution u_3: %.6f\n', u(3,iter));
fprintf('Numerical solution u_4: %.6f\n', u(4,iter));