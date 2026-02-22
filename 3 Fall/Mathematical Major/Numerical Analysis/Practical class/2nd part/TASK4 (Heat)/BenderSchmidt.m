% Construct coordinate system
h=0.25;
l=0.0625; % Use formula l=(lamda)*h^2/(alpha)^2 get l
x=0:h:1; % End point determined by the condition
y=0:l:0.125; % End point determined by the condition

% Conditions
u=zeros(length(x),length(y));
for i=1:length(x)
    u(i,1)=0; % Depend on condition
end
u(1,:)=0; % Depend on condition
u(length(x),:)=y; % Depend on condition

% Iteration based on Bender-Schmidt
for j=1:length(y)-1
    for i=2:length(x)-1
        u(i,j+1)=0.5.*(u(i-1,j)+u(i+1,j));
    end
end

% Visualization
figure;
mesh(x, y, u');
title('Numerical Solution using Bender-Schmidt Method');
xlabel('x'); ylabel('y'); zlabel('u');
