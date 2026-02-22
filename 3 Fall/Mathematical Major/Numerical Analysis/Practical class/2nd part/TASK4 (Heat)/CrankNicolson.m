% Construct coordinate system
h=0.125;
l=0.0625; 
alpha=1;
lamda = alpha^2*l/h^2; % Calculate by lamda=(alpha)^2*l/h^2
x=0:h:1; % End point determined by the condition
y=0:l:0.125; % End point determined by the condition

% Conditions
u=zeros(length(x),length(y));
for i=1:length(x)
    u(i,1)=0; % Depend on condition
end
u(1,:)=0; % Depend on condition
u(length(x),:)=y; % Depend on condition

% Crank-Nicolson coefficient matrix
a=[-lamda, 2+2*lamda, -lamda];
b=[lamda, 2-2*lamda, lamda];
aa=repmat(a,length(x)-2,1);
bb=repmat(b,length(x)-2,1);
d=[-1,0,1];
A=spdiags(aa,d,length(x)-2,length(x)-2);
B=spdiags(bb,d,length(x)-2,length(x)-2);
AA=full(A);
BB=full(B);

% Crank-Nicolson iteration
for k = 1:length(y)-1
    b = BB * u(2:length(x)-1, k); 
    b(1) = b(1) + lamda * u(1, k);
    b(end) = b(end) + lamda * u(end, k+1);
    u(2:length(x)-1, k+1) = AA \ b; 
end
disp(u)