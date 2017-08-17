int: TSSweek;
int: CTLconst;
int: ATLconst;
float: CTLramp;
float: TSBlb;
float: TSBub;
float: CTL0;
float: ATL0;

array[1..14] of var 0..200: TSS;
array[1..14] of var 0..100.0: CTL;
array[1..14] of var 0..200.0: ATL;
array[1..14] of var -34..10.0: TSB;

% constraint sum(TSS) = TSSweek;

constraint CTL[7] - CTL0 <= CTLramp;
constraint forall(i in 8..14) (
      CTL[i] - CTL[i-7] <= CTLramp );

constraint forall(i in 1..14) (
      TSB[i] >= TSBlb /\ TSB[i] <= TSBub );
      
constraint forall(i in 1..14) (
      TSB[i] = CTL[i] - ATL[i] );
      
constraint CTL[1] = TSS[1] * (1 - exp(-1/CTLconst)) + CTL0 * exp(-1/CTLconst);
constraint forall(i in 2..14) (
      CTL[i] = TSS[i] * (1 - exp(-1/CTLconst)) + CTL[i-1] * exp(-1/CTLconst) );
        
constraint ATL[1] = TSS[1] * (1 - exp(-1/ATLconst)) + ATL0 * exp(-1/ATLconst);
constraint forall(i in 2..14) (
      ATL[i] = TSS[i] * (1 - exp(-1/ATLconst)) + ATL[i-1] * exp(-1/ATLconst) );
        
constraint forall(i in 1..5) (
      TSS[i] <= TSS[6] /\ TSS[i] <= TSS[7] );
constraint forall(i in 8..12) (
      TSS[i] <= TSS[13] /\ TSS[i] <= TSS[14] );
      
constraint forall(i in 1..14) (
      TSS[i] >= 0.5 * CTL[i] );
constraint forall(i in 1..14) (
      TSS[i] <= CTL[i] * 2 );

solve :: int_search(TSS,impact,indomain_reverse_split,complete) 
%       maximize(CTL[14]); 
%       minimize(ATL[7]);
%       minimize(sum(ATL) / 14);
%       maximize(sum(TSB) / 14);
        maximize(0.5 * CTL[14] - 0.5 * sum(ATL) / 14);