% 定义点向量
x = [2, 3];
y = [2, 1];

% 定义切线向量
% 假设切线向量为 P0', P1', P2'
pp = [4, 2];  % 假设 P1' 为 [0, 0]，表示 P1 处的切线与 P0 到 P1 的线段平行

% 使用 csape 函数计算三次样条插值
% 注意：第二参数是 [pp; y]，其中 pp 是切线向量，y 是控制点
tck = csape(x, [pp; y], 'variational');

% 定义要计算样条值的参数
t_values = [4/3, 5/3];

% 计算样条曲线的值
spline_values = fnval(tck, t_values);

% 打印结果，保留三位小数
fprintf('Spline value at t=1/3: [%.3f, %.3f]\n', spline_values(1, 1), spline_values(2, 1));
