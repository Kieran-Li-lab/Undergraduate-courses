% 定义精确解
exact_solution = @(x) sinh(x) / sinh(1); % 精确解 y(x) = sinh(x) / sinh(1)

% 设置不同的网格大小
h_values = [0.5, 0.25];

% 初始化绘图
figure;

for i = 1:length(h_values)
    h = h_values(i);
    
    % 获取包括边界点的完整解
    y_full = finite_difference(h);
    
    % 创建对应的x值（包括边界点）
    x = linspace(0, 1, round(1/h) + 1);
    
    % 比较在 x = 0.5 处的数值解与精确解
    approx_y = y_full(round(0.5 / h) + 1); % 在 x = 0.5 处的数值解
    exact_y = exact_solution(0.5);  % 在 x = 0.5 处的精确解
    
    % 输出结果
    fprintf('对于 h = %.2f:\n', h);
    fprintf('数值解 y(0.5) = %.6f\n', approx_y);
    fprintf('精确解 y(0.5) = %.6f\n\n', exact_y);
    
    % 绘制数值解（去除边界点）
    % 仅绘制内部点，避免边界点引发长度不一致的错误
    x_interior = x(2:end-1);  % 内部点的x值
    y_interior = y_full(2:end-1);  % 内部点的y值
    plot(x_interior, y_interior, '-o', 'DisplayName', sprintf('h = %.2f', h));
    hold on;
end

% 添加标签和图例
legend show;
xlabel('x');
ylabel('y');
title('有限差分法解边值问题');
grid on;

% 定义求解BVP的有限差分方法
function y_full = finite_difference(h)
    % 网格点数（包括边界点）
    N = round(1/h) + 1; % 包括边界点

    % 网格点
    x = linspace(0, 1, N);

    % 设置系数矩阵A
    A = zeros(N-2, N-2); % 仅考虑内部点（排除边界点）
    b = zeros(N-2, 1);    % 右侧向量 f(x) = 0

    % 填充矩阵A，基于有限差分近似
    for i = 1:N-2
        if i == 1
            A(i,i) = 2 + h^2;   % 第一个内部方程
            A(i,i+1) = -1;      % 与下一个点耦合
        elseif i == N-2
            A(i,i-1) = -1;      % 与前一个点耦合
            A(i,i) = 2 + h^2;   % 最后一个内部方程
        else
            A(i,i-1) = -1;      % 与前一个点耦合
            A(i,i) = 2 + h^2;   % 对角线元素
            A(i,i+1) = -1;      % 与下一个点耦合
        end
    end

    % 应用边界条件 y(0) = 0 和 y(1) = 1
    b(1) = 0;   % 在 x = 0 处，y = 0
    b(end) = 1;  % 在 x = 1 处，y = 1

    % 求解线性方程组
    y = A \ b;

    % 创建完整的解向量，包括边界点
    y_full = [0; y; 1];
end
