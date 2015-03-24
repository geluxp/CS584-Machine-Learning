function r=ksr(x,y)



d=size(x,2);
    z=x;

r.x=z;
N=size(z,1);

% % clean missing or invalid data points
% inv=(y~=y);
% x(inv,:)=[];
% y(inv)=[];
r.n=numel(y);
%     % optimal bandwidth suggested by Bowman and Azzalini (1997) p.31
%     hy=median(abs(y-median(y)))/0.6745*(4/(d+2)/r.n)^(1/(d+4));
%     hx=median(abs(x-repmat(median(x),r.n,1)))/0.6745*(4/(d+2)/r.n)^(1/(d+4));
%     hx=sqrt(hy*hx);
% 
% r.h=hx;
% 
% 
% 
% 
% % Improved efficient code
% 
% % Scaling first
% H=diag(1./hx);
x=x;
x1=r.x;

% Gaussian kernel function
kerf=@(z)exp(-sum(z.*z,2)/2);

% allocate memory
r.f=zeros(N,1);

% Loop through each regression point
for k=1:N
    % scaled deference from regression point
    xx=abs(x-x1(k+zeros(r.n,1),:));
    % select neighbours using exp(-5^2/2)<5e-6
    idx=all(xx<5,2);
    % kernel function
    z=kerf(xx(idx,:));
    % regression
    r.f(k)=sum(z.*y(idx))/sum(z);
end
