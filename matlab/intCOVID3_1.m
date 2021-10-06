% simulate COVID epidemic using COVID1
% version 2.1: implementing social distancing scaled in between de(1) and
% de(2)
% fixed problem with CM, now transposed, 
% increased number of icu to 3000
% version 2.2: save parameters in a structure param 
% f_max is calculated differently to avoid high levels for youth 
% version 3.1: new parameter set Apr 16 - age-specific diagnostic rates

clear all;
global agG agC agO b c d de f fs ga h icu m p q r sd

agG=4; % age groups (0-19,20-49,50-69, 70+)
agC=8; % compartments per age group
agO=4; % outcomes per age group

% parameters

param=struct();
pp=0.44; % proportion of infectiousness during pre-sympt stage
param.pp=pp;
id=7; % infectivity duration
param.id=id;
c=CM'; % daily contact matrix = proportions of contacts between age groups
param.c=c;
p=[0.7 0.7 0.7 0.7]; % proportion symptomatic by age
param.p=p;
ga=[0.33 0.5]; % progression to pre-sympt and sympt
param.ga=ga;
m=[0.998 0.969 0.871 0.745];  % proportion symptomatic who remain mild by age
param.m=m;
d=[0.01 0.2; 0.01 0.2; 0.01 0.2; 0.01 0.2]; % daily proportion diagnosed by age (initially and after start of intervention)
param.d=d;
h=[0.1 0.15 0.15 0.15]; % daily rate of hospitalized among severe cases
param.h=h;
hd=11.2; % time from hospitalization to death
param.hd=hd;
q=[0.05 0.054 0.188 0.541]; % proportion hospitalized who need critical care
param.q=q;
r=[0.2 1/id 1/id]; % recovery rate of asympt, mild sympt and hospitalized
param.r=r;
cfr=[0; 0.002; 0.021; 0.159]; % case-fatality rate by age Italy
param.cfr=cfr;
% daily range of mortality rate among hospitalized by age group based on % death x time from host to death 
fmult=1.3; % correction of the mortality because underestimate due to the assumption that all severe cases are hosp.
param.fmult=fmult;
f=repmat(fmult*cfr./((1-m')*hd),1,2).*[1 2];
param.f=f;
fs=100; % sigmoid slope
param.fs=fs;
icu=3000; % ICU capacity
param.icu=icu;
rd=0.25; %reduction of contactness upon diagnosis
param.rd=rd;
rh=1; %reduction of contactness upon hospitalization (full)
param.rh=rh;
bs=0.455; % daily infectivity of symptomatic, unduagnosed
param.bs=bs;
bp=id*ga(2)*pp/(1-pp); % daily infectivity of pre-symptomatic
param.bs=bs;
b=[0.25 1 1 (1-rd) (1-rh)]*bs; % daily transmission rate with asympt, pre-sympt, sympt, diagnosed, hosp
param.b=b;
sd=0.7; %maximum reduction of transmission due to social distancing
param.sd=sd;
de=[50 71]; % period of implementing social distancing
param.de=de;

simN=1; % number of simulations
p1=200; %initial period (60 days) - before more intense testing
p2=0; %Follow up period (140 days)
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
   opts = odeset('RelTol',1e-10,'AbsTol',1e-8);
   [T,Y] = ode45(@COVID2,TT1,y1,opts);
   popDist((sim-1)*pN+1:(sim-1)*pN+p1+1,:)=Y;
%    y1=Y(n1,:);
%    d(1)=d(2);
%    [T,Y] = ode45(@COVID2,TT2,y1);
%    popDist((sim-1)*pN+1:(sim-1)*pN+p1+1,:)=Y;
%    
end    
    
%save('covid2_2_simple_deaths','popDist','param');
%save('matlab_output2nn','popDist','param');
%save('matlab_output_sc11nn','popDist','param');
save('covid3_2','popDist','param');


    
    
    
