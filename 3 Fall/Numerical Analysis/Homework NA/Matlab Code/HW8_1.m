% 定义矩阵 A
A = [2 11 3 6; 11 5 11 8; 3 12 5 4; 7 5 2 3];

% 初始化变量
n = size(A, 1);  
x = [0 0.8 -0.3 -0.1]';  
tol = 1e-6;      
max_iter = 1000;  

%迭代过程
for k = 1:max_iter
    y = A * x;
    mu_k = max(abs(y));  
    x_new = y / mu_k;
    if norm(x_new - x, inf) < tol  
        break;
    end
    x = x_new;
end

disp('主特征值:');
disp(mu_k);
disp('对应的特征向量:');
disp(x/norm(x));