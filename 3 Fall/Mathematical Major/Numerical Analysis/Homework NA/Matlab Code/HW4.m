format long g
% 定义函数
fun = @(x) sin(2*x(1) + 1.5*x(2) - 2) + 0.5*x(2)*(x(2) - 4/(21+1));

% 定义初始猜测值
x0 = [0, 0];

% 定义选项，使用默认算法
options = optimoptions('fminunc', 'Algorithm', 'quasi-newton','TolFun',1e-14,'TolX',1e-14);

% 调用fminunc函数找到最小值
[x_min, fval] = fminunc(fun, x0, options);

% 显示结果
format long g
fprintf('The minimum occurs at x = %8.8g, y = %8.8g\n', x_min(1), x_min(2));
fprintf('The minimum value of the function is %8.8g\n', fval);