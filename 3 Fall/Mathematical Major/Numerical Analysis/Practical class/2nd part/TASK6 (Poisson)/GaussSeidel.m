% Parameterization
h=1;
epsilon=1e-6;
max_iter=1000;

% Initialization
Nx=4;
Ny=4;
u=zeros(Nx, Ny);
f=@(x,y) 8*x.^2 .* y.^2;

% Value of f
x=[1,2];
y=[1,2];
F=zeros(2,2);
for i=1:2
    for j=1:2
        F(i,j)=f(x(i),y(j));
    end
end

% Iterration
for iter=1:max_iter
    u_old=u;
    u(2,2)=(1/4)*(u(1,2)+u(3,2)+u(2,1)+u(2,3)-h^2*F(1,1));
    u(2,3)=(1/4)*(u(1,3)+u(3,3)+u(2,2)+u(2,4)-h^2*F(1,2));
    u(3,2)=(1/4)*(u(2,2)+u(4,2)+u(3,1)+u(3,3)-h^2*F(2,1));
    u(3,3)=(1/4)*(u(2,3)+u(4,3)+u(3,2)+u(3,4)-h^2*F(2,2));
    error=max(max(abs(u-u_old)));
    if error<epsilon % Check convergence
        fprintf('Convergent at %dth iteration\n', iter);
        break;
    end
end

% Print Solution
fprintf('u1=%.6f\n',u(2,2));
fprintf('u2=%.6f\n',u(2,3));
fprintf('u3=%.6f\n',u(3,2));
fprintf('u4=%.6f\n',u(3,3));
disp('final u:');
disp(u);
