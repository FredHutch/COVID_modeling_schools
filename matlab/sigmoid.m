function y=sigmoid(x,a,b,c,d)
% sigmoid response function betweeen min(a) and max(b) with inflection
% point at c and slope controlled by d
    y = a+(b-a)./(1 + exp(d*(c-x)));