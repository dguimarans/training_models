int: Period;
int: TSSweek;
int: CTLconst;
int: ATLconst;
float: CTLramp;
float: TSBlb;
float: TSBub;
float: CTL0;
float: ATL0;

array[1..Period] of var 0..TSSweek: TSS;
array[1..Period] of var 0..100.0: CTL;
array[1..Period] of var 0..200.0: ATL;
array[1..Period] of var -15..15.0: TSB;

% constraint sum(TSS) = TSSweek;

constraint CTL[Period] - CTL0 <= CTLramp;

constraint forall(i in 1..Period) (
      TSB[i] >= TSBlb /\ TSB[i] <= TSBub );
      
constraint forall(i in 1..Period) (
      TSB[i] = CTL[i] - ATL[i] );
      
constraint CTL[1] = TSS[1] * (1 - exp(-1/CTLconst)) + CTL0 * exp(-1/CTLconst);
constraint forall(i in 2..Period) (
      CTL[i] = TSS[i] * (1 - exp(-1/CTLconst)) + CTL[i-1] * exp(-1/CTLconst) );
        
constraint ATL[1] = TSS[1] * (1 - exp(-1/ATLconst)) + ATL0 * exp(-1/ATLconst);
constraint forall(i in 2..Period) (
      ATL[i] = TSS[i] * (1 - exp(-1/ATLconst)) + ATL[i-1] * exp(-1/ATLconst) );
        
% constraint forall(i in 1..5) (
%       TSS[i] <= TSS[6] /\ TSS[i] <= TSS[7] );
      
constraint forall(i in 1..Period) (
      TSS[i] >= 0 * CTL[i] );
constraint forall(i in 1..Period) (
      TSS[i] <= CTL[i] * 4 );

constraint TSS[2] = 0 /\ TSS[3] = 0 /\ TSS[4] = 0 /\ TSS[5] = 0;

solve :: int_search(TSS,impact,indomain_reverse_split,complete) 
      maximize(CTL[Period]); 
%       minimize(ATL[Period]);
%       maximize(TSB[Period]);
%       minimize(sum(ATL) / Period);
%       maximize(sum(TSB) / Period);