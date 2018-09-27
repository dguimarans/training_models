int: Period;

int: TSS_period;
int: TSSb_period;
int: TSSr_period;
int: TSSs_period;

int: CTLconst;
int: ATLconst;
float: CTLramp;

float: CTL_0;
float: CTLb_0;
float: CTLr_0;
float: CTLs_0;

float: ATL_0;
float: ATLb_0;
float: ATLr_0;
float: ATLs_0;

array[1..Period] of var 0..TSS_period: TSS;
array[1..Period] of var 0..TSS_period: TSSb;
array[1..Period] of var 0..TSS_period: TSSr;
array[1..Period] of var 0..TSS_period: TSSs;

array[1..Period] of var 0..200.0: CTL;
array[1..Period] of var 0..200.0: CTLb;
array[1..Period] of var 0..200.0: CTLr;
array[1..Period] of var 0..200.0: CTLs;

array[1..Period] of var 0..200.0: ATL;
array[1..Period] of var 0..200.0: ATLb;
array[1..Period] of var 0..200.0: ATLr;
array[1..Period] of var 0..200.0: ATLs;

%% We limit fitness/fatigue levels by definition of domains for form. 
array[1..Period] of var -20.0..15.0: TSB;
array[1..Period] of var -20.0..15.0: TSBb;
array[1..Period] of var -15.0..15.0: TSBr;
array[1..Period] of var -15.0..15.0: TSBs;


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Cycling model
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% constraint sum(TSSb) = TSSb_period;

% constraint CTLb[Period] - CTLb_0 <= CTLramp;

% constraint forall(i in 1..Period) (
%       TSBb[i] = CTLb[i] - ATLb[i] );      
                  
constraint TSBb[1] = CTLb_0 - ATLb_0;
constraint forall(i in 2..Period) (
      TSBb[i] = CTLb[i-1] - ATLb[i-1] );
      
constraint CTLb[1] = TSSb[1] * (1 - exp(-1/CTLconst)) + CTLb_0 * exp(-1/CTLconst);
constraint forall(i in 2..Period) (
      CTLb[i] = TSSb[i] * (1 - exp(-1/CTLconst)) + CTLb[i-1] * exp(-1/CTLconst) );
        
constraint ATLb[1] = TSSb[1] * (1 - exp(-1/ATLconst)) + ATLb_0 * exp(-1/ATLconst);
constraint forall(i in 2..Period) (
      ATLb[i] = TSSb[i] * (1 - exp(-1/ATLconst)) + ATLb[i-1] * exp(-1/ATLconst) );
      
constraint forall(i in 1..Period) (
      TSSb[i] >= 0 * CTLb[i] /\ TSSb[i] <= CTLb[i] * 2 );


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Running model
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% constraint sum(TSSr) = TSSr_period;

% constraint CTLr[Period] - CTLr_0 <= CTLramp;
      
% constraint forall(i in 1..Period) (
%       TSBr[i] = CTLr[i] - ATLr[i] );

constraint TSBr[1] = CTLr_0 - ATLr_0;
constraint forall(i in 2..Period) (
      TSBr[i] = CTLr[i-1] - ATLr[i-1] );
      
constraint CTLr[1] = TSSr[1] * (1 - exp(-1/CTLconst)) + CTLr_0 * exp(-1/CTLconst);
constraint forall(i in 2..Period) (
      CTLr[i] = TSSr[i] * (1 - exp(-1/CTLconst)) + CTLr[i-1] * exp(-1/CTLconst) );
        
constraint ATLr[1] = TSSr[1] * (1 - exp(-1/ATLconst)) + ATLr_0 * exp(-1/ATLconst);
constraint forall(i in 2..Period) (
      ATLr[i] = TSSr[i] * (1 - exp(-1/ATLconst)) + ATLr[i-1] * exp(-1/ATLconst) );
      
constraint forall(i in 1..Period) (
      TSSr[i] >= 0 * CTLr[i] /\ TSSr[i] <= CTLr[i] * 3 );
      

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Swimming model
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% constraint sum(TSSs) = TSSs_period;

% constraint CTLs[Period] - CTLs_0 <= CTLramp;
      
% constraint forall(i in 1..Period) (
%       TSBs[i] = CTLs[i] - ATLs[i] );

constraint TSBs[1] = CTLs_0 - ATLs_0;
constraint forall(i in 2..Period) (
      TSBs[i] = CTLs[i-1] - ATLs[i-1] );
      
constraint CTLs[1] = TSSs[1] * (1 - exp(-1/CTLconst)) + CTLs_0 * exp(-1/CTLconst);
constraint forall(i in 2..Period) (
      CTLs[i] = TSSs[i] * (1 - exp(-1/CTLconst)) + CTLs[i-1] * exp(-1/CTLconst) );
        
constraint ATLs[1] = TSSs[1] * (1 - exp(-1/ATLconst)) + ATLs_0 * exp(-1/ATLconst);
constraint forall(i in 2..Period) (
      ATLs[i] = TSSs[i] * (1 - exp(-1/ATLconst)) + ATLs[i-1] * exp(-1/ATLconst) );
      
constraint forall(i in 1..Period) (
      TSSs[i] >= 0 * CTLs[i] /\ TSSs[i] <= CTLs[i] * 5 );

% constraint forall(i in 1..Period) (
%       TSSs[i] >= 22.0 );

constraint forall(i in 1..Period) (
      TSSs[i] = 0 \/ TSSs[i] >= 22 );
% constraint forall(i in 1..Period) (
%       TSSs[i] <= 44 );


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Overall model
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% constraint sum(TSS) = TSS_period;

constraint sum(TSS) = sum(TSSb) + sum(TSSr) + sum(TSSs);

constraint forall(i in 1..Period) (
      TSS[i] = TSSb[i] + TSSr[i] + TSSs[i] );

constraint forall(i in 1..Period) (
      CTL[i] = CTLb[i] + CTLr[i] + CTLs[i] );
      
% constraint CTL[Period] - CTL_0 <= CTLramp;
% constraint forall(i in 1..Period) (
%       CTL[i] <= CTL_0 + CTLramp );
constraint CTL[7] - CTL_0 <= CTLramp;
constraint forall(i in 8..Period) (
      CTL[i] - CTL[i - 7] <= CTLramp );

constraint forall(i in 1..Period) (
      ATL[i] = ATLb[i] + ATLr[i] + ATLs[i] );

constraint TSB[1] = CTL_0 - ATL_0;
constraint forall(i in 2..Period) (
      TSB[i] = CTL[i-1] - ATL[i-1] );
      
constraint CTL[1] = TSS[1] * (1 - exp(-1/CTLconst)) + CTL_0 * exp(-1/CTLconst);
constraint forall(i in 2..Period) (
      CTL[i] = TSS[i] * (1 - exp(-1/CTLconst)) + CTL[i-1] * exp(-1/CTLconst) );
        
constraint ATL[1] = TSS[1] * (1 - exp(-1/ATLconst)) + ATL_0 * exp(-1/ATLconst);
constraint forall(i in 2..Period) (
      ATL[i] = TSS[i] * (1 - exp(-1/ATLconst)) + ATL[i-1] * exp(-1/ATLconst) );
      
constraint forall(i in 1..Period) (
      TSS[i] >= 0 * CTL[i] /\ TSS[i] <= CTL[i] * 2 );

%% Constraints for recovery weeks
% constraint CTL[Period] >= CTL_0 - CTL_0 * 0.05 /\ CTL[Period] <= CTL_0;
% constraint TSB[Period] <= 0;
% constraint sum(TSSs) > 0;

%% Constraints for planning purposes (structured week)
% Week 1
constraint TSSb[1] > 0 /\ TSSb[2] = 0 /\ TSSb[3] > 0 /\ TSSb[4] = 0 /\ TSSb[5] > 0 /\ TSSb[6] = 0 /\ TSSb[7] > 0;
constraint TSSr[1] = 0 /\ TSSr[2] > 0 /\ TSSr[3] = 0 /\ TSSr[4] > 0 /\ TSSr[5] = 0 /\ TSSr[6] > 0 /\ TSSr[7] = 0;
constraint TSSs[1] = 0 /\ TSSs[2] > 0 /\ TSSs[3] = 0 /\ TSSs[4] > 0 /\ TSSs[5] = 0 /\ TSSs[6] = 0 /\ TSSs[7] = 0;
% Week 2
constraint TSSb[8] > 0 /\ TSSb[9] = 0 /\ TSSb[10] > 0 /\ TSSb[11] = 0 /\ TSSb[12] > 0 /\ TSSb[13] = 0 /\ TSSb[14] > 0;
constraint TSSr[8] = 0 /\ TSSr[9] > 0 /\ TSSr[10] = 0 /\ TSSr[11] > 0 /\ TSSr[12] = 0 /\ TSSr[13] > 0 /\ TSSr[14] = 0;
constraint TSSs[8] = 0 /\ TSSs[9] > 0 /\ TSSs[10] = 0 /\ TSSs[11] > 0 /\ TSSs[12] = 0 /\ TSSs[13] = 0 /\ TSSs[14] = 0;
% Week 3
constraint TSSb[15] > 0 /\ TSSb[16] = 0 /\ TSSb[17] > 0 /\ TSSb[18] = 0 /\ TSSb[19] > 0 /\ TSSb[20] = 0 /\ TSSb[21] > 0;
constraint TSSr[15] = 0 /\ TSSr[16] > 0 /\ TSSr[17] = 0 /\ TSSr[18] > 0 /\ TSSr[19] = 0 /\ TSSr[20] > 0 /\ TSSr[21] = 0;
constraint TSSs[15] = 0 /\ TSSs[16] > 0 /\ TSSs[17] = 0 /\ TSSs[18] > 0 /\ TSSs[19] = 0 /\ TSSs[20] = 0 /\ TSSs[21] = 0;
% % Week 4     
constraint TSSb[22] > 0 /\ TSSb[23] = 0 /\ TSSb[24] > 0 /\ TSSb[25] = 0 /\ TSSb[26] > 0 /\ TSSb[27] = 0 /\ TSSb[28] > 0;
constraint TSSr[22] = 0 /\ TSSr[23] > 0 /\ TSSr[24] = 0 /\ TSSr[25] > 0 /\ TSSr[26] = 0 /\ TSSr[27] > 0 /\ TSSr[28] = 0;
constraint TSSs[22] = 0 /\ TSSs[23] > 0 /\ TSSs[24] = 0 /\ TSSs[25] > 0 /\ TSSs[26] = 0 /\ TSSs[27] = 0 /\ TSSs[28] = 0;


solve :: int_search(TSS,impact,indomain_reverse_split,complete) 
      maximize(CTL[Period]); 
%       minimize(ATL[Period]);
%       maximize(TSB[Period]);
%       minimize(sum(ATL) / Period);
%       maximize(sum(TSB) / Period);
