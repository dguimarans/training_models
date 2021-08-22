% General parameters for the model
int: CTLconst;
int: ATLconst;
float: CTLramp;

% Sports specification
array[int] of string: sport_names;
set of int: SPORTS = 1..length(sport_names);

% Planning parameters
int: Period;
set of int: DAYS = 1..Period;
array[SPORTS,DAYS] of 0..1: availability;

% Initial status
array[SPORTS] of float: CTL_0;
array[SPORTS] of float: ATL_0;

% We calculate an upper bound on the training stress for the whole period
float: TSS_period = (sum(CTL_0) + CTLramp * (Period div 7 - 1)) * 2 * Period;
% int: TSSb_period;
% int: TSSr_period;
% int: TSSs_period;

% Variables of the problem
array[SPORTS,DAYS] of var 0..TSS_period: TSS;
array[SPORTS,DAYS] of var 0..(sum(CTL_0) * CTLramp * (Period div 7)): CTL;
array[SPORTS,DAYS] of var float: ATL;
array[SPORTS,DAYS] of var -20.0..10.0: TSB;

array[DAYS] of var 0..TSS_period: TSSoverall;
array[DAYS] of var 0..(sum(CTL_0) * CTLramp * (Period div 7)): CTLoverall;
array[DAYS] of var float: ATLoverall;
array[DAYS] of var -20.0..10.0: TSBoverall;


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


% float: CTL_0;
% float: CTLb_0;
% float: CTLr_0;
% float: CTLs_0;

% float: ATL_0;
% float: ATLb_0;
% float: ATLr_0;
% float: ATLs_0;

% array[1..Period] of var 0..TSS_period: TSS;
% array[1..Period] of var 0..TSS_period: TSSb;
% array[1..Period] of var 0..TSS_period: TSSr;
% array[1..Period] of var 0..TSS_period: TSSs;

% array[1..Period] of var 0..200.0: CTL;
% array[1..Period] of var 0..200.0: CTLb;
% array[1..Period] of var 0..200.0: CTLr;
% array[1..Period] of var 0..200.0: CTLs;

% array[1..Period] of var 0..200.0: ATL;
% array[1..Period] of var 0..200.0: ATLb;
% array[1..Period] of var 0..200.0: ATLr;
% array[1..Period] of var 0..200.0: ATLs;

%% We limit fitness/fatigue levels by definition of domains for form. 
% array[1..Period] of var -20.0..15.0: TSB;
% array[1..Period] of var -20.0..15.0: TSBb;
% array[1..Period] of var -20.0..15.0: TSBr;
% array[1..Period] of var -20.0..15.0: TSBs;

% array[int] of int: bike_availability;
% array[1..Period] of int: run_availability;
% array[1..Period] of int: swim_availability;


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Cycling model
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% constraint sum(TSSb) = TSSb_period;

% constraint CTLb[Period] - CTLb_0 <= CTLramp;

% constraint forall(i in 1..Period) (
%       TSBb[i] = CTLb[i] - ATLb[i] );      
                  
% constraint TSBb[1] = CTLb_0 - ATLb_0;
% constraint forall(i in 2..Period) (
%       TSBb[i] = CTLb[i-1] - ATLb[i-1] );
      
% constraint CTLb[1] = TSSb[1] * (1 - exp(-1/CTLconst)) + CTLb_0 * exp(-1/CTLconst);
% constraint forall(i in 2..Period) (
%       CTLb[i] = TSSb[i] * (1 - exp(-1/CTLconst)) + CTLb[i-1] * exp(-1/CTLconst) );
        
% constraint ATLb[1] = TSSb[1] * (1 - exp(-1/ATLconst)) + ATLb_0 * exp(-1/ATLconst);
% constraint forall(i in 2..Period) (
%       ATLb[i] = TSSb[i] * (1 - exp(-1/ATLconst)) + ATLb[i-1] * exp(-1/ATLconst) );
      
% constraint forall(i in 1..Period) (
%       TSSb[i] >= 0 /\ TSSb[i] <= CTLb[i] * 4 );


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Running model
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% constraint sum(TSSr) = TSSr_period;

% constraint CTLr[Period] - CTLr_0 <= CTLramp;
      
% constraint forall(i in 1..Period) (
%       TSBr[i] = CTLr[i] - ATLr[i] );

% constraint TSBr[1] = CTLr_0 - ATLr_0;
% constraint forall(i in 2..Period) (
%       TSBr[i] = CTLr[i-1] - ATLr[i-1] );
      
% constraint CTLr[1] = TSSr[1] * (1 - exp(-1/CTLconst)) + CTLr_0 * exp(-1/CTLconst);
% constraint forall(i in 2..Period) (
%       CTLr[i] = TSSr[i] * (1 - exp(-1/CTLconst)) + CTLr[i-1] * exp(-1/CTLconst) );
        
% constraint ATLr[1] = TSSr[1] * (1 - exp(-1/ATLconst)) + ATLr_0 * exp(-1/ATLconst);
% constraint forall(i in 2..Period) (
%       ATLr[i] = TSSr[i] * (1 - exp(-1/ATLconst)) + ATLr[i-1] * exp(-1/ATLconst) );
      
% constraint forall(i in 1..Period) (
%       TSSr[i] >= 0 /\ TSSr[i] <= CTLr[i] * 4 );
      

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Swimming model
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% constraint sum(TSSs) = TSSs_period;

% constraint CTLs[Period] - CTLs_0 <= CTLramp;
      
% constraint forall(i in 1..Period) (
%       TSBs[i] = CTLs[i] - ATLs[i] );

% constraint TSBs[1] = CTLs_0 - ATLs_0;
% constraint forall(i in 2..Period) (
%       TSBs[i] = CTLs[i-1] - ATLs[i-1] );
      
% constraint CTLs[1] = TSSs[1] * (1 - exp(-1/CTLconst)) + CTLs_0 * exp(-1/CTLconst);
% constraint forall(i in 2..Period) (
%       CTLs[i] = TSSs[i] * (1 - exp(-1/CTLconst)) + CTLs[i-1] * exp(-1/CTLconst) );
        
% constraint ATLs[1] = TSSs[1] * (1 - exp(-1/ATLconst)) + ATLs_0 * exp(-1/ATLconst);
% constraint forall(i in 2..Period) (
%       ATLs[i] = TSSs[i] * (1 - exp(-1/ATLconst)) + ATLs[i-1] * exp(-1/ATLconst) );
      
% constraint forall(i in 1..Period) (
%       TSSs[i] >= 0 /\ TSSs[i] <= CTLs[i] * 4 );

% constraint forall(i in 1..Period) (
%       TSSs[i] >= 22.0 );

% constraint forall(i in 1..Period) (
%       TSSs[i] = 0 \/ TSSs[i] >= 22 );
% constraint forall(i in 1..Period) (
%       TSSs[i] <= 44 );


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Overall model
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% constraint sum(TSS) = TSS_period;

% constraint sum(TSS) = sum(TSSb) + sum(TSSr) + sum(TSSs);

% constraint forall(i in 1..Period) (
%       TSS[i] = TSSb[i] + TSSr[i] + TSSs[i] );

% constraint forall(i in 1..Period) (
%       CTL[i] = CTLb[i] + CTLr[i] + CTLs[i] );
      
% % constraint CTL[Period] - CTL_0 <= CTLramp;
% % constraint forall(i in 1..Period) (
% %       CTL[i] <= CTL_0 + CTLramp );
% constraint CTL[7] - CTL_0 <= CTLramp;
% constraint forall(i in 8..Period) (
%       CTL[i] - CTL[i - 7] <= CTLramp );

% constraint forall(i in 1..Period) (
%       ATL[i] = ATLb[i] + ATLr[i] + ATLs[i] );

% constraint TSB[1] = CTL_0 - ATL_0;
% constraint forall(i in 2..Period) (
%       TSB[i] = CTL[i-1] - ATL[i-1] );
      
% constraint CTL[1] = TSS[1] * (1 - exp(-1/CTLconst)) + CTL_0 * exp(-1/CTLconst);
% constraint forall(i in 2..Period) (
%       CTL[i] = TSS[i] * (1 - exp(-1/CTLconst)) + CTL[i-1] * exp(-1/CTLconst) );
        
% constraint ATL[1] = TSS[1] * (1 - exp(-1/ATLconst)) + ATL_0 * exp(-1/ATLconst);
% constraint forall(i in 2..Period) (
%       ATL[i] = TSS[i] * (1 - exp(-1/ATLconst)) + ATL[i-1] * exp(-1/ATLconst) );
      
% constraint forall(i in 1..Period) (
%       TSS[i] >= 0 /\ TSS[i] <= CTL[i] * 2 );


% %% Constraints for planning purposes (structured week)
% constraint forall(i in 1..Period) (
%       if bike_availability[i] > 0 then
%           TSSb[i] >= 0
%       else
%           TSSb[i] = 0
%       endif
%       );
% constraint forall(i in 1..Period) (
%       if run_availability[i] > 0 then
%           TSSr[i] >= 0
%       else
%           TSSr[i] = 0
%       endif
%       );
% constraint forall(i in 1..Period) (      
%       if swim_availability[i] > 0 then
%           TSSs[i] >= 0
%       else
%           TSSs[i] = 0
%       endif
%       );

% constraint sum(i in (Period - 7)..Period) (TSS[i]) = CTL[Period - 7] * 7;


solve %:: int_search(TSSoverall,impact,indomain_reverse_split,complete) 
%         maximize(10 * CTL[Period] + 3 * TSB[Period]);
      maximize(CTLoverall[Period]); 
%       maximize(CTL[Period - 7] + CTL[Period] + TSB[Period]);
%       minimize(ATL[Period]);
%       maximize(TSB[Period]);
%       minimize(sum(ATL) / Period);
%       maximize(sum(TSB) / Period);

output ["Day,Sport,TSS,CTL,ATL,TSB,TSSo,CTLo,ATLo,TSBo\n"] ++
        [show(i) ++ "," ++ show(sport_names[s]) ++ "," ++
        show_float(8,2,TSS[s,i]) ++ "," ++ show_float(8,2,CTL[s,i]) ++ "," ++ show_float(8,2,ATL[s,i]) ++ "," ++ show_float(8,2,TSB[s,i]) ++ "," ++
        show_float(8,2,TSSoverall[i]) ++ "," ++ show_float(8,2,CTLoverall[i]) ++ "," ++ show_float(8,2,ATLoverall[i]) ++ "," ++ show_float(8,2,TSBoverall[i]) ++ "\n"  
        | i in 1..Period, s in SPORTS];
