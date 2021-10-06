% simulate COVID epidemic using COVID1

clear all;
global agG agC agO b c d f fs ga h icu m p q r

agG=4; % age groups (0-19,20-49,50-69, 70+)
agC=8; % compartments per age group
agO=4; % outcomes per age group

% parameters
c=CM; % daily contact matrix = proportions of contacts between age groups
p=[0.3 0.5 0.6 0.7]; % proportion symptomatic by age
ga=[0.25 0.5]; % progression to pre-sympt and sympt
m=[0.998 0.969 0.866 0.74];  % proportion symptomatic who remain mild by age
d=0.01; % daily proportion diagnosed (initially)
h=[0.1 0.2 0.25 0.25]; % daily rate of hospitalized among severe cases
hd=11.2; % time from hospitalization to death
q=[0.05 0.055 0.2 0.57]; % proportion hospitalized who need critical care
r=[0.15 0.1 0.07]; % recovery rate of asympt, mild sympt and hospitalized
cfr=[0 0.001; 0.002 0.004; 0.022 0.044; 0.16 0.32]; % case-fatality rate by age Italy
% daily range of mortality rate among hospitalized by age group based on % death x time from host to death 
f=4*cfr./repmat(1-m',1,2)/hd;
fs=100; % sigmoid slope
icu=1000; % ICU capacity
rd=0.5; %reduction of contactness upon diagnosis
rh=0.75; %reduction of contactness upon hospitalization
bs=0.5; % daily infectivity of symptomatic, unduagnosed
b=[0.25 1 1 (1-rd) (1-rh)]*bs; % daily transmission rate with asympt, pre-sympt, sympt, diagnosed, hosp

simN=1; % number of simulations
p1=60; %initial period (60 days) - before more intense testing
p2=140; %Follow up period (140 days)
TT1=0:p1;
n1=length(TT1);
TT2=0:p2;
n2=length(TT2);
pN=p1+p2;

%storePar=zeros(simN,3*yrN); %store de sampled each year

%CI=zeros(simN,10*(scN+1)); % save cumulative number of inf over 10 and 20 years (1-no vacc + scN-vacc.)
%vacN=zeros(simN,2*scN); % vaccine doses needed over 10 and 20 years
popDist=zeros(simN*(pN+1),(agC+agO)*agG); % store population disctribution


initN=2190000;
y0=zeros(agC,agG);
y0(1,:)=[0.2293 0.4552 0.235 0.0805]*initN;
y0(2,2)=2;
y0(1,2)=y0(1,2)-y0(2,2);
y0(2,3)=2;
y0(1,3)=y0(1,3)-y0(2,3);
y0=reshape(y0,1,agC*agG);
y1=zeros(1,(agC+agO)*agG);
y1(1:agC*agG)=y0;
for sim=1:simN
    sim
    % run without intervention
   [T,Y] = ode45(@COVID1,TT1,y1);
   popDist((sim-1)*pN+1:(sim-1)*pN+p1+1,:)=Y;
   y1=Y(n1,:);
   d=0.2; % daily proportion diagnosed
   [T,Y] = ode45(@COVID1,TT2,y1);
   popDist((sim-1)*pN+p1+1:sim*pN,:)=Y(2:n2,:);
   
end    
    
    
   
%save(['vac' fname '_53' char(vacTypeL(vacType))],'par','b','ageSusc','storePar','CI','vacN');
save('covid1_1','popDist');


    
    
    
