% General parameters for the model
int: CTLconst;
int: ATLconst;
float: CTLramp;
float: TSBlb;

% Sports specification
array[int] of string: sport_names;
set of int: SPORTS = 1..length(sport_names);

% Planning parameters
int: Period;
set of int: DAYS = 1..Period;
array[SPORTS,DAYS] of 0..1: availability;

% Parameters and variables for 80%-20% planning
float: IF_80;
float: IF_20;
array[DAYS] of 0.0..24.0: available_time;
array[SPORTS,DAYS] of var 0.0..24.0: t_80;  
array[SPORTS,DAYS] of var 0.0..24.0: t_20;

% Initial status
array[SPORTS] of float: CTL_0;
array[SPORTS] of float: ATL_0;

% We calculate an upper bound on the training stress for the whole period
float: TSS_period = (sum(CTL_0) + CTLramp * (Period div 7 - 1)) * 2 * Period;

% Variables of the problem
array[SPORTS,DAYS] of var 0..TSS_period: TSS;
array[SPORTS,DAYS] of var 0..(sum(CTL_0) * CTLramp * (Period div 7)): CTL;
array[SPORTS,DAYS] of var float: ATL;
array[SPORTS,DAYS] of var -20.0..10.0: TSB;

array[DAYS] of var 0..TSS_period: TSSoverall;
array[DAYS] of var 0..(sum(CTL_0) * CTLramp * (Period div 7)): CTLoverall;
array[DAYS] of var float: ATLoverall;
array[DAYS] of var -TSBlb..10.0: TSBoverall;


% Constraints per sport
constraint forall(sport in SPORTS) (
              TSB[sport,1] = CTL_0[sport] - ATL_0[sport]
           );
           
constraint forall(sport in SPORTS) (
              forall(i in 2..Period) (
                 TSB[sport,i] = CTL[sport,i-1] - ATL[sport,i-1] 
              )
           );
      
constraint forall(sport in SPORTS) (
              CTL[sport,1] = TSS[sport,1] * (1 - exp(-1/CTLconst)) + CTL_0[sport] * exp(-1/CTLconst)
           );

constraint forall(sport in SPORTS) (
              forall(i in 2..Period) (
                 CTL[sport,i] = TSS[sport,i] * (1 - exp(-1/CTLconst)) + CTL[sport,i - 1] * exp(-1/CTLconst) 
              )
           );
        
constraint forall(sport in SPORTS) (
              ATL[sport,1] = TSS[sport,1] * (1 - exp(-1/ATLconst)) + ATL_0[sport] * exp(-1/ATLconst)
           );
           
constraint forall(sport in SPORTS) (
              forall(i in 2..Period) (
                 ATL[sport,i] = TSS[sport,i] * (1 - exp(-1/ATLconst)) + ATL[sport,i - 1] * exp(-1/ATLconst) 
              )
           );
      
% constraint forall(sport in SPORTS) (
%               forall(i in DAYS) (
%                  TSS[sport,i] <= CTL[sport,i] * 3 
%               )
%            );
           
%% Constraints for planning purposes (structured week)
constraint forall(sport in SPORTS) (
              forall(i in DAYS) (
                 if availability[sport,i] > 0 then
                    TSS[sport,i] >= 0.5 * CTLoverall[i]
                 else
                    TSS[sport,i] = 0
                 endif
              )
           );
           
% Overall constraints
           
constraint sum(TSSoverall) = sum(TSS);
constraint forall(i in DAYS) (
              TSSoverall[i] = sum(sport in SPORTS) (TSS[sport,i])
           );

constraint forall(i in DAYS) (
              CTLoverall[i] = sum(sport in SPORTS) (CTL[sport,i]) 
           );

constraint CTLoverall[7] - sum(CTL_0) <= CTLramp;
constraint forall(i in 8..Period) (
              CTLoverall[i] - CTLoverall[i - 7] <= CTLramp 
           );
% constraint forall(i in 1..(Period - 7)) (
%               CTLoverall[i + 7] - CTLoverall[i] <= CTLramp
%            ); 

constraint forall(i in DAYS) (
              ATLoverall[i] = sum(sport in SPORTS) (ATL[sport,i]) 
           );

constraint TSBoverall[1] = sum(CTL_0) - sum(ATL_0);
constraint forall(i in 2..Period) (
              TSBoverall[i] = CTLoverall[i-1] - ATLoverall[i-1] 
           );
      
constraint CTLoverall[1] = TSSoverall[1] * (1 - exp(-1/CTLconst)) + sum(CTL_0) * exp(-1/CTLconst);
constraint forall(i in 2..Period) (
              CTLoverall[i] = TSSoverall[i] * (1 - exp(-1/CTLconst)) + CTLoverall[i-1] * exp(-1/CTLconst) 
           );
        
constraint ATLoverall[1] = TSSoverall[1] * (1 - exp(-1/ATLconst)) + sum(ATL_0) * exp(-1/ATLconst);
constraint forall(i in 2..Period) (
              ATLoverall[i] = TSSoverall[i] * (1 - exp(-1/ATLconst)) + ATLoverall[i-1] * exp(-1/ATLconst) 
           );
      
constraint forall(i in DAYS) (
              TSSoverall[i] <= CTLoverall[i] * 2 
           );           

% Constraints for planning a 80%-20% time distribution
constraint forall(i in DAYS) (
              sum([t_80[j,i] | j in SPORTS]) + sum([t_20[j,i] | j in SPORTS]) <= available_time[i]
           );
           
constraint forall(sport in SPORTS) (
              forall(i in DAYS) (
                 TSS[sport,i] = (IF_80 * IF_80 * t_80[sport,i] + IF_20 * IF_20 * t_20[sport,i]) * 100
              )
           );     
           
constraint sum(t_80) = 0.8 * (sum(t_80) + sum(t_20));

% Constraint to ensure warm up time is always included on hard days.
constraint forall(sport in SPORTS) (
              forall(i in DAYS) (
                 if t_20[sport,i] > 0 then t_80[sport,i] >= 0.15 endif
              )
           );
      
% Constraint to try and limit the hard sessions to days where recovery is high     
constraint forall(i in DAYS) (
              sum(sport in SPORTS) (t_20[sport,i]) <= (TSBoverall[i] + TSBlb) / 60
           );
                 


solve %:: int_search(TSSoverall,impact,indomain_reverse_split,complete) 
%         maximize(10 * CTL[Period] + 3 * TSB[Period]);
      maximize(CTLoverall[Period]); 
%       maximize(CTL[Period - 7] + CTL[Period] + TSB[Period]);
%       minimize(ATL[Period]);
%       maximize(TSB[Period]);
%       minimize(sum(ATL) / Period);
%       maximize(sum(TSB) / Period);

output ["Day,Sport,t80,t20,TSS,CTL,ATL,TSB,TSSo,CTLo,ATLo,TSBo\n"] ++
        [show(i) ++ "," ++ show(sport_names[s]) ++ "," ++
        show_float(8,0,t_80[s,i] * 60) ++ "," ++ show_float(8,0,t_20[s,i] * 60) ++ "," ++
        show_float(8,2,TSS[s,i]) ++ "," ++ show_float(8,2,CTL[s,i]) ++ "," ++ show_float(8,2,ATL[s,i]) ++ "," ++ show_float(8,2,TSB[s,i]) ++ "," ++
        show_float(8,2,TSSoverall[i]) ++ "," ++ show_float(8,2,CTLoverall[i]) ++ "," ++ show_float(8,2,ATLoverall[i]) ++ "," ++ show_float(8,2,TSBoverall[i]) ++ "\n"  
        | i in 1..Period, s in SPORTS];
