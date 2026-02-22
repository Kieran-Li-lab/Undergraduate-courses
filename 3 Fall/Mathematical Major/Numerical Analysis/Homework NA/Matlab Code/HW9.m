% 参数定义
L = 1;      
T = 1/8;  
h = 1/4;   % 空间步长
k = 1/16;  % 时间步长

% 计算网格点数量
N = L / h;       
M = T / k;       

% 系数
lambda = k / (h^2); % λ

% 初始化u矩阵
u = zeros(N+1, M+1);

% 边界条件
u(1, :) = 0;          % x = 0时 u = 0
u(end, :) = linspace(0, T, M+1); % x = 1时 u = t

% 构造Crank-Nicholson方法的系数矩阵A
A = (2 + 2*lambda) * eye(N-1) - lambda * diag(ones(N-2, 1), 1) - lambda * diag(ones(N-2, 1), -1);

% 时间步进计算
for m = 1:M
    b = zeros(N-1, 1);
    for i = 2:N 
        b(i-1) = lambda * u(i-1, m) + (2 - 2*lambda) * u(i, m) + lambda * u(i+1, m);
    end
    
    % 修正边界条件
    b(1) = b(1) + lambda * u(1, m+1);       
    b(end) = b(end) + lambda * u(end, m+1); 
    u(2:end-1, m+1) = A \ b;
end

% 结果输出
result = u(N/2 + 1, M+1); % x = 1/2, t = 1/8的解
disp(['u(1/2, 1/8) = ', num2str(result)]);
