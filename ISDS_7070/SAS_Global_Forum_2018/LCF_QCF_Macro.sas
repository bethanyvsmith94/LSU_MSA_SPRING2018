*************************************************************
* Program Purpose = to run all possible subsets linear      *
*  or quadratic classification analysis                     *
*************************************************************;
*************************************************************;
OPTIONS PAGESIZE=60;
LIBNAME Breast  "C:\Users\betha\Desktop\Breast_Cancer_Data";
***********************************************************
* MACRO SECTION
***********************************************************
***********************************************************
* MACRO TO DEFINE VARIABLE LIST AND CONDUCT PROC DISCRIM  *
***********************************************************;
%MACRO classify(DSNAME);
  %DO Q = 0 %TO 1; 
  %DO R = 0 %TO 1;
  %DO S = 0 %TO 1;
  %DO T = 0 %TO 1;
  %DO U = 0 %TO 1;
  %DO V = 0 %TO 1;
  %DO W = 0 %TO 1;
  /*%DO X = 0 %TO 1;
  %DO Y = 0 %TO 1;
  %DO Z = 0 %TO 1;
  %DO ZZ = 0 %TO 1;
  %DO AA = 0 %TO 1;*/
PROC DISCRIM DATA=&DSNAME POOL=NO testout=two noprint
        TESTLIST TESTDATA=Breast.validate;*****use data for validation; 
        CLASS Diagnosis; **** use pool=NO for quadratic;
		PRIORS PROPORTIONAL; ****unequal priors;
  VAR
  %IF &Q=0 %THEN %GOTO X1LBL;
     Smoothness_Mean
    %X1LBL:
  %IF &R=0 %THEN %GOTO X2LBL;
     Radius_SE
    %X2LBL:
  %IF &S=0 %THEN %GOTO X3LBL;
     Symmetry_SE
    %X3LBL:
  %IF &T=0 %THEN %GOTO X4LBL;
     Fractal_Dimension_SE
    %X4LBL:
  %IF &U=0 %THEN %GOTO X5LBL;
     Texture_Worst
    %X5LBL:
  %IF &V=0 %THEN %GOTO X6LBL;
     Perimeter_Worst
    %X6LBL:
  %IF &W=0 %THEN %GOTO X7LBL;
     Compactness_Worst
    %X7LBL:
  /*%IF &X=0 %THEN %GOTO X8LBL;
     CDACCOUNT
    %X8LBL:
  %IF &Y=0 %THEN %GOTO X9LBL;
     ONLINE
    %X9LBL:
  %IF &Z=0 %THEN %GOTO X10LBL;
     CREDITCARD
    %X10LBL:
  %IF &ZZ=0 %THEN %GOTO X11LBL;
     EDUCGRAD
    %X11LBL:
  %IF &AA=0 %THEN %GOTO X12LBL;
     EDUCPROF
    %X12LBL: */
   ;
   %BEST;                 *<<<THIS INVOKES THE BEST MACRO<<<<;
   %END; %END; %END; %END;
   %END; %END; %END; %END;
   %END; %END; %END; %END; 
  %MEND classify;         *<<<THIS STATEMENT ENDS THE CLASSIFY MACRO<<<;
*************************************************************
* MACRO APPEND DATA FOR (BEST) ALL POSSIBLE SUBSET ANALYSIS *
*************************************************************;
%MACRO BEST;
DATA SAMPLHIT;
   SET TWO;
   IF Diagnosis =_INTO_ THEN HIT=1;
PROC MEANS DATA=SAMPLHIT NOPRINT;  *<<<THIS PROC IS USED TO PRODUCE NUMBER OF HITS<<<;
   VAR HIT;          *<<<AND SAMPLE SIZE (_FREQ_) TO BE USED IN CALC OF<<<;
   OUTPUT OUT=THREE;               *<<<HITRATE<<<;
DATA FOUR;
  length d $ 1 e $ 1 f $ 1 g $ 1 h $ 1 i $ 1
         j $ 1 /*k $ 1 l $ 1 m $ 1 n $ 1 p $ 1*/;
   SET THREE;
   IF _STAT_='N';
   HITRATE=HIT/_FREQ_;                       *<<<CALCULATE HIT RATE<<<;
   d=&q; e=&r; f=&s; g=&t; h=&u;
   i=&v; j=&w; /*k=&x; l=&y; m=&z; n=&zz; p=&aa;*/
  SUBSIZ=D+E+F+G+H+I+J/*+K+L+M+N+P*/;              *<<<CALCULATE SUBSET SIZE<<<;
  VCOMB = D||E||F||G||H||I||J/*||K||L||M||N||P*/; *<<<DEFINES VAR COMBINATION<<<;
DATA ALL; SET ALL FOUR;
%MEND BEST;                                  *<<<END OF BEST MACRO<<<;

*****************************************************************************
*****************************************************************************
*  MAIN PROGRAM BODY                                                        *
*****************************************************************************
*****************************************************************************

*******************************************************
*READ DATA UPON WHICH THE ANALYSIS IS TO BE CONDUCTED *
*******************************************************;
DATA ONE;
  SET Breast.Train;******put training data set name here;
DATA ALL;
  SET ONE;
   IF Diagnosis='';         *******************************;
   IF Diagnosis='' THEN DELETE; *THIS SECTION CREATES EMPTY DATASET  *;
   A='';             *TO LATER SAVE RESULTS OF 2^P-1 POSSIBLE ANALYSES *;
   KEEP A; ***************************************************;

%CLASSIFY(ONE);                    *<<<INVOKES MACRO TO GENERATE ALL POSSIBLE SUBSETS <<<;
**************************************************************************************;
* the following text is from the original program                                    *;
**************************************************************************************;
*PROC SORT DATA=ALL; *BY SUBSIZ HITRATE;
*PROC PRINT DATA=ALL; *BY SUBSIZ;
    *var _freq_ hit hitrate vcomb;
    *TITLE 'RESULTS OF EACH OF THE DISCRIMINANT ANALYSIS BY SUBSET SIZE';
*DATA BESTSUB;
   *SET ALL; *BY SUBSIZ HITRATE;
   *IF LAST.SUBSIZ;               *<<<KEEPS ONLY THE BEST OF EACH SIZE<<<;
*PROC PRINT DATA=BESTSUB;
   *var _freq_ hit hitrate vcomb;
   *TITLE 'RESULTS OF DISCRIMINANT ANALYSES FOR THE BEST SUBSETS OF EACH SIZE';
**************************************************************************************;
PROC SORT DATA=ALL; BY DESCENDING HITRATE;
PROC PRINT DATA=ALL;* (OBS=200);
    var _freq_ hit hitrate vcomb SUBSIZ;
    TITLE1 'VALIDATION RESULTS FOR DISCRIMINANT ANALYSIS BY SUBSET SIZE';
	TITLE2 'POOL = NO and UNEQUAL PRIORS';
	******CHANGE TITLE2 TO REFLECT DETAILS OF THE MODEL;
RUN;

