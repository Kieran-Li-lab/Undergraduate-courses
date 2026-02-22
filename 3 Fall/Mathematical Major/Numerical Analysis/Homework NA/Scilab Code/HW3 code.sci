clear
Int=integrate('x^2/sqrt(12+sin(x))','x', 0, 1)
printf("%1.12f",Int)
x1 = %pi / 2
Y = integrate('sin(x)', 'x', 0, x1)
disp(Y)

x1 = 1;
expr = 'x^2/sqrt(12+sin(x))'; // 定义表达式
var = 'x'; // 定义变量
Int = integrate(expr, var, 0, x1); // 执行积分
disp(Int)

clear
Int=integrate('exp(x*sin(cos(sin(x))))','x',0,1) // error< 1e-13
printf("%1.12f",Int)

clear
x1 = 1;
Int = integrate('x^2/sqrt(12+sin(x))', 'x', 0, x1);
disp(Int)
