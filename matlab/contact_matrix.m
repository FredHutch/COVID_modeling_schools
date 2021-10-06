A=load('ukmatrix.txt');
A(16,:)=[];
B=A./sum(A);
C=[mean(B(:,1:4),2) mean(B(:,5:10),2) mean(B(:,11:14),2) B(:,15)];
D=[sum(C(1:4,:)); sum(C(5:10,:)); sum(C(11:14,:)); C(15,:)];