LIBNAME BETH "C:\Users\betha\Desktop\ISDS_7070"; 


DATA BETH.AMES_02; 
	SET BETH.AMES0118; 



**********NOMINAL VARIABLES******; 

	IF Alley = "NA"        THEN ALLEY = 0; 
	ELSE IF Alley = 'GRVL' THEN ALLEY = 1; 
	ELSE IF Alley = 'PAVE' THEN ALLEY = 2; 

	***two unique levels ****; 

IF Street = 'Grvl' THEN STREET = 1; 
ELSE IF Street = 'Pave' THEN STREET = 2;
 
****Land Contour four unique levels*****; 

IF Land_Contour = 'Lvl' then LAND_CONTOUR = 1; 
ELSE IF Land_Contour = 'Bnk' then LAND_CONTOUR = 2; 
ELSE IF Land_Contour = 'HLS' then LAND_CONTOUR = 3;
ELSE IF Land_Contour = 'Low' then LAND_CONTOUR = 4; 

IF Lot_Config = 'Inside' then LOT_CONFIG = 1;  
ELSE IF Lot_Config = 'Corner' then LOT_CONFIG = 2;
ELSE IF Lot_Config = 'CulDSac' then LOT_CONFiG = 3; 
ELSE IF Lot_Config = 'FR2' then LOT_CONFIG = 4; 
ELSE IF Lot_Config = 'FR3' then LOT_CONFIG = 5; 


IF Neighborhood = "Blmngtn" then NEIGHBORHOOD = 1; 
IF Neighborhood = "Blueste" then NEIGHBORHOOD = 2; 
IF Neighborhood = "BrDale" then NEIGHBORHOOD = 3; 
IF Neighborhood = "BrkSide" then NEIGHBORHOOD = 4; 
IF Neighborhood = "ClearCr" then NEIGHBORHOOD = 5; 
IF Neighborhood = "CollgCr" then NEIGHBORHOOD = 6; 
IF Neighborhood = "Crawfor" then NEIGHBORHOOD = 7; 
IF Neighborhood = "Edwards" then NEIGHBORHOOD = 8; 
IF Neighborhood = "Gilbert" then NEIGHBORHOOD = 9; 
IF Neighborhood = "Greens" then NEIGHBORHOOD = 10; 
IF Neighborhood = "GrnHill" then NEIGHBORHOOD = 11; 
IF Neighborhood = "IDOTRR" then NEIGHBORHOOD = 12; 
IF Neighborhood = "Landmrk" then NEIGHBORHOOD = 13; 
IF Neighborhood = "MeadowV" then NEIGHBORHOOD = 14; 
IF Neighborhood = "Mitchel" then NEIGHBORHOOD = 15; 
IF Neighborhood = "Names" then NEIGHBORHOOD = 16; 
IF Neighborhood = "NoRidge" then NEIGHBORHOOD = 17; 
IF Neighborhood = "NPkVill" then NEIGHBORHOOD = 18; 
IF Neighborhood = "NridgHt" then NEIGHBORHOOD = 19; 
IF Neighborhood = "NWAmes" then NEIGHBORHOOD = 20; 
IF Neighborhood = "OldTown" then NEIGHBORHOOD =21; 
IF Neighborhood = "SWISU" then NEIGHBORHOOD = 22; 
IF Neighborhood = "Sawyer" then NEIGHBORHOOD = 23; 
IF Neighborhood = "SawyerW" then NEIGHBORHOOD = 24; 
IF Neighborhood = "Somerst" then NEIGHBORHOOD = 25; 
IF Neighborhood = "StoneBr" then NEIGHBORHOOD = 26; 
IF Neighborhood = "Timber" then NEIGHBORHOOD = 27; 
IF Neighborhood = "Veenker" then NEIGHBORHOOD = 28; 


IF Condition_1 = "Artery" then CONDITION_1 = 1; 
ELSE IF Condition_1 = "Adjacent" then CONDITION_1 = 2;
ELSE IF Condition_1 = "Feedr"	then CONDITION_1 = 3;
ELSE IF Condition_1 =  "Norm"	then CONDITION_1 = 4;	
ELSE IF Condition_1 =  "RRNn"	then CONDITION_1 = 5;
ELSE IF Condition_1 =  "RRAn"	then CONDITION_1 = 6;
ELSE IF Condition_1 =  "PosN"	then CONDITION_1 = 7;
ELSE IF Condition_1 =   "PosA"	then CONDITION_1 = 8;
ELSE IF Condition_1 =   "RRNe"	then CONDITION_1 = 9;
ELSE IF Condition_1 =  "RRAe"	then CONDITION_1 = 10;

IF Condition_2 = "Artery" then CONDITION_2 = 1; 
ELSE IF Condition_2 = "Feedr"	then CONDITION_2 = 2;
ELSE IF Condition_2 =  "Norm"	then CONDITION_2 = 3;	
ELSE IF Condition_2 =  "RRNn"	then CONDITION_2 = 4;
ELSE IF Condition_2 =  "RRAn"	then CONDITION_2 = 5;
ELSE IF Condition_2 =  "PosN"	then CONDITION_2 = 6;
ELSE IF Condition_2 =   "PosA"	then CONDITION_2 = 7;
ELSE IF Condition_2 =   "RRNe"	then CONDITION_2 = 8;
ELSE IF Condition_2 =  "RRAe"	then CONDITION_2 = 9;


IF Bldg_Type = "1Fam" then BLDG_TYPE = 1; 

IF House_Style = "1Story" then HOUSE_STYLE= 1; 
ELSE if House_Style = "1.5Fin" then HOUSE_STYLE = 2; 
ELSE IF House_Style = "1.5Unf" then HOUSE_StYLE = 3; 
ELSE IF House_Style = "2Story" then HOUSE_STYLE =  4; 
ELSE IF House_Style = '2.5Fin' then HOUSE_STYLE = 5; 
ELSE IF House_Style = "2.5Unf" then HOUSE_STYLE = 6; 
ELSE IF House_Style = "SFoyer" then HOUSE_STYLE = 7; 
ELSE IF House_Style = "SLvl" then HOUSE_STYLE = 8; 

IF Roof_Style = "Flat" then ROOF_STYLE = 1; 
ELSE if Roof_Style = "Gable" then ROOF_STYLE = 2; 
ELSE if Roof_Style = "Gambrel" then ROOF_STYLE = 3; 
ELSE if Roof_Style = "Hip" then ROOF_STYLE = 4; 
ELSE if Roof_Style = "Mansard" then ROOF_STYLE = 5; 
ELSE if Roof_Style = "Shed" then ROOF_STYLE = 6; 

IF Pool_QC = "Ex" then POOL_QC = 4; 
ELSE IF Pool_QC = "Gd" then POOL_QC = 3; 
ELSE IF Pool_QC = "TA" then POOL_QC = 2;
ELSE IF Pool_QC = "Fa" then POOL_QC = 1;
ELSE IF Pool_QC = "NA" then POOL_QC = 0;

IF Misc_Feature = "Elev" then MISC_FEATURE = 5; 
ELSE IF Misc_Feature = "Gar2" then MISC_FEATURE = 4; 
ELSE IF Misc_Feature = 'Othr' then MISC_FEATURE = 1; 
ELSE IF Misc_Feature = 'Shed' then MISC_FEATURE = 2; 
ELSE IF Misc_Feature = 'TenC' then MISC_FEATURE = 3; 
ELSE IF Misc_Feature = 'NA' then MISC_FEATURE = 0;
ELSE Misc_Feature = MISC_FEATURE; 

IF Garage_Type = "2Types" then GARAGE_TYPE = 1; 
ELSE IF Garage_Type = "Attchd" then GARAGE_TYPE = 2; 
ELSE IF Garage_Type = "Basment" then GARAGE_TYPE = 3; 
ELSE IF Garage_Type = "BuiltIn" then GARAGE_TYPE = 4; 
ELSE IF Garage_Type = "CarPort" then GARAGE_TYPE = 5; 
ELSE IF Garage_Type = "Detchd" then GARAGE_TYPE = 6; 
ELSE IF Garage_Type = "NA" then GARAGE_TYPE = 0;
ELSE Garage_Type = GARAGE_TYPE;

IF Central_Air = "N" then CENTRAL_AIR = 1; 
Else if Central_Air = "Y" then CENTRAL_AIR = 2; 
ELSE Central_Air = CENTRAL_AIR; 


If Heating = "Floor" then HEATING = 6; 
Else if Heating = "GasA" then HEATING = 5; 
Else if Heating = "GasW" then HEATING = 4; 
Else if Heating = "Grav" then HEATING = 3; 
Else if Heating = "OthW" then HEATING = 2; 
Else if Heating = "Wall" then HEATING = 1; 
Else Heating = HEATING; 


*******Ordinal Variables ******; 

If Lot_Shape = "Reg" then LOT_SHAPE = 4; 
Else if Lot_Shape = "IR1" then LOT_SHAPE = 3; 
Else if Lot_Shape = "IR2" then LOT_SHAPE = 2; 
Else if Lot_Shape = "IR3" then LOT_SHAPE = 1; 
Else  Lot_Shape = LOT_SHAPE;

If HeatingQC = "Ex" then HEATING_QC = 5; 
Else if HeatingQC = "Gd" then HEATING_QC = 4; 
Else if HeatingQC = "TA" then HEATING_QC = 3; 
Else if HeatingQC = "Fa" then HEATING_QC = 2; 
Else if HeatingQC = "Po" then HEATING_QC = 1; 
Else HeatingQC = HEATING_QC; 


IF Fence = "GdPrv" then FENCE = 4; 
ELSE IF Fence = "MnPrv" then FENCE = 3; 
ElSE IF Fence = "GdWo" then FENCE = 2; 
ELSE IF Fence = "MnWw" then FENCE = 1; 
ELSE IF Fence = "NA" then FENCE = 0; 
ELSE Fence = FENCE; 


IF Fireplace_Qu = "Ex" then FIREPLACEQU = 5; 
ELSE IF Fireplace_Qu = "Gd" then FIREPLACEQU = 4; 
ELSE IF Fireplace_Qu = "TA" then FIREPLACEQU = 3;
ELSE IF Fireplace_Qu = "Fa" then FIREPLACEQU = 2;
ELSE IF Fireplace_Qu = "Po" then FIREPLACEQU = 1;
ELSE IF Fireplace_Qu = "NA" then FIREPLACEQU = 0;
ELSE Fireplace_Qu = FIREPLACEQU;

If Garage_Yr_Blt = . then GARAGE_YR_BLT = 0; 
else Garage_Yr_Blt = GARAGE_YR_BLT ; 

IF Garage_Qual = "Ex" then GARAGE_QUAL = 5; 
Else IF Garage_Qual = "Gd" then GARAGE_QUAL = 4; 
ELSE IF Garage_Qual = "TA" then GARAGE_QUAL = 3; 
ELSE IF Garage_Qual = "Fa" then GARAGE_QUAL = 2; 
ELSE IF Garage_Qual = "Po" then GARAGE_QUAL = 1; 
ELSE IF Garage_Qual = "NA" then GARAGE_QUAL = 0; 
ELSE Garage_Qual = GARAGE_QUAL; 

IF Garage_Cond = "Ex" then GARAGE_COND = 5; 
Else IF Garage_Cond = "Gd" then GARAGE_COND = 4; 
ELSE IF Garage_Cond = "TA" then GARAGE_COND = 3; 
ELSE IF Garage_Cond = "Fa" then GARAGE_COND = 2; 
ELSE IF Garage_Cond = "Po" then GARAGE_COND = 1; 
ELSE IF Garage_Cond = "NA" then GARAGE_COND = 0; 
ELSE Garage_Cond = GARAGE_COND; 

IF Garage_Finish = "Fin" then GARAGE_FINISH = 3; 
ELSE if Garage_Finish = "RFn" then GARAGE_FINISH = 2; 
Else if Garage_Finish = "Unf" then GARAGE_FINISH = 1; 
ELSE if Garage_Finish = "NA" then GARAGE_FINISH = 0; 
ELSE Garage_Finish = GARAGE_FINISH; 

IF Bsmt_Qual = "Ex" then  BSMT_QUAL = 5; 
ELSE IF Bsmt_Qual = "Gd" then BSMT_QUAL = 4; 
ELSE IF Bsmt_Qual = "TA" then BSMT_QUAL = 3; 
ELSE IF Bsmt_Qual = "FA" then BSMT_QUAL = 2;
ELSE IF Bsmt_Qual = "Po" then BSMT_QUAL = 1;
ELSE If Bsmt_Qual = "NA" then BSMT_QUAL = 0; 
ELSE If Bsmt_Qual = BSMT_QUAL; 

IF Bsmt_Cond = "Ex" then  BSMT_COND = 5; 
ELSE IF Bsmt_Cond = "Gd" then BSMT_COND = 4; 
ELSE IF Bsmt_Cond = "TA" then BSMT_COND = 3; 
ELSE IF Bsmt_Cond = "FA" then BSMT_COND = 2;
ELSE IF Bsmt_Cond = "Po" then BSMT_COND = 1;
ELSE If Bsmt_Cond = "NA" then BSMT_COND = 0; 
ELSE If Bsmt_Cond = BSMT_COND;

IF Bsmt_Exposure = "Gd" then  BSMT_COND = 4; 
ELSE IF Bsmt_Exposure = "Av" then BSMT_COND = 3; 
ELSE IF Bsmt_Exposure = "Mn" then BSMT_COND = 2; 
ELSE IF Bsmt_Exposure = "No" then BSMT_COND = 1;
ELSE If Bsmt_Exposure = "NA" then BSMT_COND = 0; 
ELSE If Bsmt_Exposure = BSMT_COND; 

If BsmtFin_Type_1 = "GLQ" then BSMTFIN_TYPE1 = 7; 
ELSE If BsmtFin_Type_1 = "ALQ" then BSMTFIN_TYPE1 = 6; 
ELSE If BsmtFin_Type_1 = "BLQ" then BSMTFIN_TYPE1 = 5; 
ELSE If BsmtFin_Type_1 = "Rec" then BSMTFIN_TYPE1 = 4; 
ELSE If BsmtFin_Type_1 = "LwQ" then BSMTFIN_TYPE1 = 3; 
ELSE If BsmtFin_Type_1 = "Unf" then BSMTFIN_TYPE1 = 2; 
ELSE If BsmtFin_Type_1 = "NA" then BSMTFIN_TYPE1 = 1; 
ELSE  BsmtFin_Type_1 =  BSMTFIN_TYPE1; 


If BsmtFin_Type_2 = "GLQ" then BSMTFIN_TYPE2 = 7; 
ELSE If BsmtFin_Type_2 = "ALQ" then BSMTFIN_TYPE2 = 6; 
ELSE If BsmtFin_Type_2 = "BLQ" then BSMTFIN_TYPE2 = 5; 
ELSE If BsmtFin_Type_2 = "Rec" then BSMTFIN_TYPE2 = 4; 
ELSE If BsmtFin_Type_2 = "LwQ" then BSMTFIN_TYPE2 = 3; 
ELSE If BsmtFin_Type_2 = "Unf" then BSMTFIN_TYPE2 = 2; 
ELSE If BsmtFin_Type_2 = "NA" then BSMTFIN_TYPE2 = 1; 
ELSE  BsmtFin_Type_2 =  BSMTFIN_TYPE2;

IF Pool_QC = "Ex" then POOL_QC = 4; 
ELSE IF Pool_QC = "Gd" then POOL_QC = 3; 
ELSE IF Pool_QC = "TA" then POOL_QC = 2; 
ELSE IF Pool_QC = "Fa" then POOL_QC = 1; 
ELSE IF Pool_QC = "NA" then POOL_QC = 0; 
ELSE  Pool_QC_ = POOL_QC; 

IF Paved_Drive = "Y" then PAVED_DRIVE = 3; 
ELSE IF Paved_Drive = "P" then PAVED_DRIVE = 2; 
ELSE IF Paved_Drive = "N" then PAVED_DRIVE = 1; 
ELSE Paved_Drive = PAVED_DRIVE; 

IF Functional = "Typ" then FUNCTIONAL = 8; 
ELSE IF Functional = "Min1" then FUNCTIONAL = 7; 
ELSE IF Functional = "Min2" then FUNCTIONAL = 6; 
ELSE IF Functional = "Mod" then FUNCTIONAL = 5; 
ELSE IF Functional = "Maj1" then FUNCTIONAL = 4; 
ELSE IF Functional = "Maj2" then FUNCTIONAL = 3; 
ELSE IF Functional = "Sev" then FUNCTIONAL = 2; 
ELSE IF Functional = "Sal" then FUNCTIONAL = 1; 
ELSE  Functional =  FUNCTIONAL; 

IF Kitchen_Qual = "Ex" then KITCHENQUAL = 5; 
Else if Kitchen_Qual = "Gd" then KITCHENQUAL = 4; 
Else if Kitchen_Qual = "TA" then KITCHENQUAL = 3; 
Else if Kitchen_Qual = "Fa" then KITCHENQUAL = 2; 
Else if Kitchen_Qual = "Po" then KITCHENQUAL = 1; 
Else Kitchen_Qual = KITCHENQUAL; 

IF Electrical = "SBrkr" then ELECTRIC = 5; 
Else if Electrical = "FuseA" then ELECTRIC = 4; 
Else if Electrical = "FuseF" then ELECTRIC = 3; 
Else if Electrical = "FuseP" then ELECTRIC = 2; 
Else if Electrical = "Mix" then ELECTRIC = 1; 
Else Electrical = ELECTRIC; 

run; 
