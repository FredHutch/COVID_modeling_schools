% COVID-19 model with asymptomatic and pre-symptomatic infections
% differentiated by age (0-19,20-49,50-69,70+)
%asymptomatic and pre-symptomatic may be infectious
% frequency-dependent transmission 
% version 2: implementing social distancing scaled in between de(1) and
% de(2)
function z=COVID2(t,x)
global agG agC agO b c d de f fs ga h icu m p q r sd

%agG=4; % age groups (0-19,20-49,50-69,70+)
%agC=8; % compartments per age group
%agO=4; % outcomes per age group
z=zeros(agG*(agC+agO),1); % x is a column!!!
N=sum(reshape(x(1:agG*agC),agC,agG))'; % total population by age (column)

% bb=b(1:4); % NO social distancing
bb=b(1:4)*((t<de(1))+(1-(t-de(1))/(de(2)-de(1))*sd)*((t>=de(1)) & (t<de(2)))...
          + (1-sd)*(t>=de(2))); % gradual reduction of transmission due to social distancing (sd)
dd=d(1)*(t<de(1))+d(2)*(t>=de(1)); % increase of testing rates around March 5
for ag=0:(agG-1)

%%%%%%%%%% POPULATION COMPARTMENTS %%%%%%%%%%%    
% 1 - susceptible ages (0-19,20-49,50-69,70+) - NO recruitment !!!
z(agC*ag+1)=-dot(c(ag+1,:),(bb(1)*x(3:agC:agG*agC)+bb(2)*x(4:agC:agG*agC)...
            +bb(3)*x(5:agC:agG*agC)+bb(4)*x(6:agC:agG*agC)+b(5)*x(7:agC:agG*agC))./N)*x(agC*ag+1);

% 2 - exposed ages (0-19,20-49,50-69,70+)
z(agC*ag+2)=dot(c(ag+1,:),(bb(1)*x(3:agC:agG*agC)+bb(2)*x(4:agC:agG*agC)...
            +bb(3)*x(5:agC:agG*agC)+bb(4)*x(6:agC:agG*agC)+b(5)*x(7:agC:agG*agC))./N)*x(agC*ag+1) - ga(1)*x(agC*ag+2);

% 3 - asymptomatic ages (0-19,20-49,50-69,70+)
z(agC*ag+3)=(1-p(ag+1))*ga(1)*x(agC*ag+2)-r(1)*x(agC*ag+3);

% 4 - pre-symptomatic infected ages (0-19,20-49,50-69,70+)
z(agC*ag+4)=p(ag+1)*ga(1)*x(agC*ag+2)-ga(2)*x(agC*ag+4);

% 5 - symptomatic infected ages (0-19,20-49,50-69,70+)
z(agC*ag+5)=ga(2)*x(agC*ag+4)-(dd+(1-m(ag+1))*h(ag+1)+m(ag+1)*r(2))*x(agC*ag+5);

%6 - diagnosed infected ages (0-19,20-49,50-69,70+)
z(agC*ag+6)=dd*x(agC*ag+5)-((1-m(ag+1))*h(ag+1)+m(ag+1)*r(2))*x(agC*ag+6);

%7 - hospitalized ages (0-19,20-49,50-69,70+)
z(agC*ag+7)=(1-m(ag+1))*h(ag+1)*(x(agC*ag+5)+x(agC*ag+6))...
    -(sigmoid(dot(x(7:agC:agG*agC),q),f(ag+1,1),f(ag+1,2),icu,fs)+r(3))*x(agC*ag+7);

% 8 - recovered ages (0-19,20-49,50-69,70+)
z(agC*ag+8)=r(1)*x(agC*ag+3)+m(ag+1)*r(2)*(x(agC*ag+5)+x(agC*ag+6))+r(3)*x(agC*ag+7);


%%%%%%%%%% SUMMARY STATISTICS %%%%%%%%%%%
% cumulative new infections ages (0-19,20-49,50-69,70+)
z(agC*agG+agO*ag+1)=dot(c(ag+1,:),(bb(1)*x(3:agC:agG*agC)+bb(2)*x(4:agC:agG*agC)...
            +bb(3)*x(5:agC:agG*agC)+bb(4)*x(6:agC:agG*agC)+b(5)*x(7:agC:agG*agC))./N)*x(agC*ag+1);

% cumulative new symptomatic ages (0-19,20-49,50-69,70+)
z(agC*agG+agO*ag+2)=ga(2)*x(agC*ag+4);

% cumulative new cases (diagnosed or hospitalized directly) ages (0-19,20-49,50-69,70+)
z(agC*agG+agO*ag+3)=(dd+(1-m(ag+1))*h(ag+1))*x(agC*ag+5);

% cumulative deaths ages (0-19,20-49,50-69,70+)
z(agC*agG+agO*ag+4)=(sigmoid(dot(x(7:agC:agG*agC),q),f(ag+1,1),f(ag+1,2),icu,fs))*x(agC*ag+7);

end





