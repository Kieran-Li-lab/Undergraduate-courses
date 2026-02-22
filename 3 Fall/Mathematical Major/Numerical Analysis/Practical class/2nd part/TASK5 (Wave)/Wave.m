% Parameterization
L=1;
T=0.4;
h=0.25;
l=0.2;
lamda=l/h;% Note lamda should less than 1
N=L/h;
M=T/l;

% Initialization
u=zeros(N+1,M+1);% u(x,t)

% Initial condition
for i=1:N+1
    x=(i-1)*h;
    u(i,1)=sin(pi*x);
end

% Calculate '第二行'，以便进行后续运算
for i=2:N
    u(i,2)=u(i,1)+(lamda^2/2)*(u(i+1,1)-2*u(i,1)+u(i-1,1));
end

% Boundary condition
u(1,:)=0;
u(N+1,:)=0;

% Iterration
for n=2:M
    for i=2:N
        u(i,n+1)=2*u(i,n)-u(i,n-1)+lamda^2*(u(i+1,n)-2*u(i,n)+u(i-1,n));
    end
end

% Numerical Solution
x_0=0.5/h+1; % x=0.5 对应的索引
t_0=0.4/l+1;
fprintf('u(0.5,0.4)=%.6f\n', u(x_0,t_0));

% Exact solution
x=0.5;
t=0.4;
u_exact=sin(pi*x)*cos(pi*t);
fprintf('Exact solution: u_exact(0.5,0.4)=%.6f\n', u_exact);
