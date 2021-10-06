load covid1_1;
A=load('king_cases.txt');
n=length(A);
agG=4; % age groups (0-19,20-49,50-69, 70+)
agC=8; % compartments per age group
agO=4; % outcomes per age group

lastC=agC*agG; %last pop. compartment
lastO=(agC+agO)*agG; % last outcome
TT=0:200;
figure(1);
hold on
plot(TT,sum(popDist(:,lastC+1:agC:lastO),2),'r');
plot(TT,sum(popDist(:,lastC+2:agC:lastO),2),'g');
plot(TT,sum(popDist(:,lastC+3:agC:lastO),2));
plot(TT,sum(popDist(:,lastC+4:agC:lastO),2),'k');
fitDate=52;
plot(fitDate:fitDate+n-1,A,'o');
ylim([0 2000]);