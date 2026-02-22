% Input matrix
A=[2, 11, 3, 6;
     11, 5, 11, 8;
     3, 12, 5, 4;
     7, 5, 2, 3]; % Given matrix
E=eye(size(A));

% Construct characteristic function
syms lamda;
eqa=det(A-lamda*E)==0;
S=solve(eqa,lamda);
disp('The eigenvalues of A is:');
disp(S)

% Compute the corresponding eigenvector
disp('The corresponding eigenvectors are:')
V=cell(length(S),1);
for i=1:length(S)
    lamda_i=S(i);
    v_i=null(A-(lamda_i)*E);
    V{i}=v_i;
    fprintf('Eigenvalue%d: %s\n', i, lamda_i);
    disp(v_i)
end
