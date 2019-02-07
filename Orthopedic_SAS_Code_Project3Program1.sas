OPTIONS PS=55 LS=70;
/* Read dataset from a text file comma delimited */

DATA hospital;
INFILE '/home/mtw950/sasuser.v94/hospital.txt' DELIMITER=',';
INPUT ZIP $  HID $ CITY $ STATE $ BEDS RBEDS OUTV ADM SIR SALESY
         SALES12 TH TRAUMA REHAB HIP95 KNEE95 HIP96
         KNEE96 FEMUR96;
run;
data hospital;

set hospital;
SALES= SALES12 +SALESY;
if SALES=0 then SALES=.;
data hospital;

set hospital;

proc sgplot data=hospital;
 	 title "Scatterplot for Beds";
  		scatter x=BEDS y=sales;
	run;
	proc sgplot data=hospital;
 	 title "Scatterplot for rBeds";
  		scatter x=rBEDS y=sales;
	run;
	proc sgplot data=hospital;
 	 title "Scatterplot for hip95";
  		scatter x=hip95 y=sales;
	run;
	proc sgplot data=hospital;
 	 title "Scatterplot for knee95";
  		scatter x=knee95 y=sales;
	run;
	proc sgplot data=hospital;
 	 title "Scatterplot for hip96";
  		scatter x=hip96 y=sales;
	run;
	proc sgplot data=hospital;
 	 title "Scatterplot for knee96";
  		scatter x=knee96 y=sales;
	run;
	proc sgplot data=hospital;
 	 title "Scatterplot for femur96";
  		scatter x=femur96 y=sales;
	run;
	proc sgplot data=hospital;
 	 title "Scatterplot for outv";
  		scatter x=outv y=sales;
	run;
	proc sgplot data=hospital;
 	 title "Scatterplot for ADM";
  		scatter x=ADM y=sales;
	run;
	proc sgplot data=hospital;
 	 title "Scatterplot for SIR";
  		scatter x=SIR y=sales;
	run;

ARRAY X {13} BEDS RBEDS HIP95 KNEE95 HIP96 KNEE96 FEMUR96
         OUTV ADM SIR SALESY SALES12 SALES;

DO I=3 TO 7 ;
         X{I} = SQRT(X{I});
END;

DO I=8 TO 13 ;
         X{I} = LOG(1+X{I});
 END;

RUN;
/* code for selecting subsets based on hospital type */
*  gr = TH*4 + trauma*2 + rehab;
*  if gr > 1  ;
/* code for selecting subsets based on hospital location */
* IF STATE EQ 'FL' OR STATE EQ 'GA' or state='TX';
DATA hospital2;
   SET hospital;
   if state='FL' OR state='NJ' OR state='PA' OR state='NY' OR state='CT' OR state='TX' OR state='CA' OR state='VA' OR state='WA' OR state='OH';
RUN;

	proc sgplot data=hospital2;
 	 title "Scatterplot for Beds";
  		scatter x=BEDS y=sales;
	run;
	proc sgplot data=hospital2;
 	 title "Scatterplot for rBeds";
  		scatter x=rBEDS y=sales;
	run;
	proc sgplot data=hospital2;
 	 title "Scatterplot for hip95";
  		scatter x=hip95 y=sales;
	run;
	proc sgplot data=hospital2;
 	 title "Scatterplot for knee95";
  		scatter x=knee95 y=sales;
	run;
	proc sgplot data=hospital2;
 	 title "Scatterplot for hip96";
  		scatter x=hip96 y=sales;
	run;
	proc sgplot data=hospital2;
 	 title "Scatterplot for knee96";
  		scatter x=knee96 y=sales;
	run;
	proc sgplot data=hospital2;
 	 title "Scatterplot for femur96";
  		scatter x=femur96 y=sales;
	run;
	proc sgplot data=hospital2;
 	 title "Scatterplot for outv";
  		scatter x=outv y=sales;
	run;
	proc sgplot data=hospital2;
 	 title "Scatterplot for ADM";
  		scatter x=ADM y=sales;
	run;
	proc sgplot data=hospital2;
 	 title "Scatterplot for SIR";
  		scatter x=SIR y=sales;
	run;
RUN;

/* Code for factor analysis in two stages, grouping the variables */
/* into two subgroups: operation and demographic*/

PROC FACTOR data=hospital2 METHOD=PRIN NFACT=1 out=z;
var  HIP95 KNEE95 HIP96 KNEE96 FEMUR96;
RUN;

PROC FACTOR data=hospital2 METHOD=PRIN NFACT=3 ROTATE=VARIMAX out=z1;
var  BEDS RBEDS OUTV ADM SIR TH trauma rehab;
RUN;

data z1;
set z1;
factor4 = factor1;
keep factor2 factor3 factor4;
run;

data hospout;
merge z z1;
run;

/* Code for factor analysis into two stages */

PROC FACTOR data=hospital2 METHOD=PRIN NFACT=4 ROTATE=VARIMAX out=hospout;
var  BEDS RBEDS OUTV ADM SIR TH trauma rehab HIP95 KNEE95 HIP96 KNEE96 FEMUR96;
RUN;

/*cluster analysis using WARD -> shows a jump in SPRS from clusters 13-14 */

PROC CLUSTER data=hospout METHOD=WARD;
VAR factor1-factor4;
COPY ZIP CITY STATE HID BEDS RBEDS OUTV ADM SIR
         HIP95 KNEE95 TH TRAUMA REHAB HIP96
         KNEE96 FEMUR96 SALES12 SALESY SALES factor1-factor4 ;
run;
PROC TREE NOPRINT NCL=14 OUT=TXCLUST;
COPY ZIP CITY STATE HID BEDS RBEDS OUTV ADM SIR
         HIP95 KNEE95 TH TRAUMA REHAB HIP96
         KNEE96 FEMUR96 SALES12 SALESY SALES factor1-factor4 ;
RUN;

/* produce the cluster summary and pick the best cluster*/

data hout;
set txclust;
keep factor1-factor4 sales cluster; run ;


proc sort data=Txclust;
by cluster;
run;
proc means noprint;
by cluster;
var SALES  factor1-factor4;
output out=c mean= msales mf1-mf4;
run;

proc print;
run;

/*my boxplot analysis of the 14 clusters*/
proc boxplot data=txclust;
   plot sales*cluster;
run;

PROC SORT DATA=HOUT;
BY CLUSTER;
RUN;

/* This assumes that cluster 8 is the best one */
data cl8;
 set txclust;
if cluster=8;
if SALES=. then SALES=0;
run;

PROC reg DATA=cl8;
model sales =  Factor1-factor4/ P R selection=b  ;
OUTPUT   OUT=C P=PRED R=RESID STDP=STDP;
run;

data C ;
set C;

rowp = exp(PRED+ 0.5*STDP*STDP)-1;
epred = exp(pred)-1;
sales = exp(sales) -1;
gain = rowp - sales;
run;

proc sort;
by resid;
proc print;
*var gain PRED STDP SALES12 epred;
run;

/*Displays the hospitals in cluster 8 with the largest negative residuals and their gains*/
data CU;
   set C (keep=zip city state hid sales resid gain);
run;
proc sort;
by resid;
proc print;
*var gain PRED STDP SALES12 epred;
run;

/* This assumes that cluster 13 is the best one */
data cl13;
 set txclust;
if cluster=13;
if SALES=. then SALES=0;
run;

PROC reg DATA=cl13;
model sales =  Factor1-factor4/ P R selection=b  ;
OUTPUT   OUT=D P=PRED R=RESID STDP=STDP;
run;

data D;
set D;

rowp = exp(PRED+ 0.5*STDP*STDP)-1;
epred = exp(pred)-1;
sales = exp(sales) -1;
gain = rowp - sales;
run;

proc sort;
by resid;
proc print;
*var gain PRED STDP SALES12 epred;
run;

/*Displays the hospitals in cluster 13 with the largest negative residuals and their gains*/
data DU;
   set D (keep=zip city state hid sales resid gain);
run;
proc sort;
by resid;
proc print;
*var gain PRED STDP SALES12 epred;
run;

data finalCU;
set CU(obs=8);
run;
data finalDU;
set DU(obs=4);
run;

/*CREATE FINAL LIST OF HOSPITALS SORTED BY GAINS*/
data FINAL;
set finalCU finalDU;
run;

proc sort;
by descending gain;
proc print;
*var gain PRED STDP SALES12 epred;
run;
/*calculate sum of gains*/
proc means data=final sum;
  var gain;
run;

/* OTHER CODE ADDITIONS */

/* CODE for using  K-MEANS clustering */
/*
proc fastclus data=hospout out=out maxc=8 maxiter=20;
VAR factor1-factor4;
run;


/* CODE FOR ESTIMATING GAIN for the special case when the cluster size is very small*/
/*
data Cl9 ;
set Cl9;
sales = exp(sales) -1;
run;

proc means data=cl9; var sales;
run;
          /* suppose the mean of sales12 is 181.5 */
/*data cl9; set cl9;
gain = 181.5 - sales;
run;

proc sort;
by gain;
proc print;
*var gain PRED STDP SALES12 epred;
run;
*/

/*
################## R Code for robust clustering with PAM #######################

h = read.csv("hospital.csv")
x = log(1+0.001*h[sample(4703,1000),-c(1:4,10:11)])
pairs(x,pch=".")
z = princomp(x)$scores[,c(1:4)]
pairs(z)
library(cluster)
# If you want to use the same factors that you got from SAS
# then save the SAS dataset as CSV using the export
hout = read.csv("hout.csv")
pairs(hout)
pam(hout[,-c(1,5)],k=10)
plot(silhouette(pam(hout[,-c(1,5)],k=10)))
plot(silhouette(pam(hout[,-c(1,5)],k=12)))
plot(silhouette(pam(hout[,-c(1,5)],k=3)))
plot(hclust(dist(hout[,-c(1,5)])))
cutree(hclust(dist(hout[,-c(1,5)])),17) -> cl
table(cl)*/
