DATA Beth.Ames_05;  
	SET Beth.Ames0118; 

 

**********NOMINAL VARIABLES******; 

	IF Alley = "NA" THEN ALLEY_ = 0; 
	ELSE IF Alley = 'GRVL' THEN ALLEY_ = 1; 
	ELSE IF Alley = 'PAVE' THEN ALLEY_ = 2; 
	

	***two unique levels ****; 

IF Street = 'Grvl' THEN STREET_ = 1; 
ELSE IF Street = 'Pave' THEN STREET_ = 2;
 
 
****Land Contour four unique levels*****; 

IF Land_Contour = 'Lvl' then LAND_CONTOUR_ = 1; 
ELSE IF Land_Contour = 'Bnk' then LAND_CONTOUR_ = 2; 
ELSE IF Land_Contour = 'HLS' then LAND_CONTOUR_ = 3;
ELSE IF Land_Contour = 'Low' then LAND_CONTOUR_ = 4; 


IF Lot_Config = 'Inside' then LOT_CONFIG_ = 1;  
ELSE IF Lot_Config = 'Corner' then LOT_CONFIG_ = 2;
ELSE IF Lot_Config = 'CulDSac' then LOT_CONFIG_ = 3; 
ELSE IF Lot_Config = 'FR2' then LOT_CONFIG_ = 4; 
ELSE IF Lot_Config = 'FR3' then LOT_CONFIG_ = 5;



IF Neighborhood = "Blmngtn" then NEIGHBORHOOD_ = 1; 
ELSE IF Neighborhood = "Blueste" then NEIGHBORHOOD_ = 2; 
ELSE IF Neighborhood = "BrDale" then NEIGHBORHOOD_ = 3; 
ELSE IF Neighborhood = "BrkSide" then NEIGHBORHOOD_ = 4; 
ELSE IF Neighborhood = "ClearCr" then NEIGHBORHOOD_ = 5; 
ELSE IF Neighborhood = "CollgCr" then NEIGHBORHOOD_ = 6; 
ELSE IF Neighborhood = "Crawfor" then NEIGHBORHOOD_ = 7; 
ELSE IF Neighborhood = "Edwards" then NEIGHBORHOOD_ = 8; 
ELSE IF Neighborhood = "Gilbert" then NEIGHBORHOOD_ = 9; 
ELSE IF Neighborhood = "Greens" then NEIGHBORHOOD_ = 10; 
ELSE IF Neighborhood = "GrnHill" then NEIGHBORHOOD_ = 11; 
ELSE IF Neighborhood = "IDOTRR" then NEIGHBORHOOD_ = 12; 
ELSE IF Neighborhood = "Landmrk" then NEIGHBORHOOD_ = 13; 
ELSE IF Neighborhood = "MeadowV" then NEIGHBORHOOD_ = 14; 
ELSE IF Neighborhood = "Mitchel" then NEIGHBORHOOD_ = 15; 
ELSE IF Neighborhood = "Names" then NEIGHBORHOOD_ = 16; 
ELSE IF Neighborhood = "NoRidge" then NEIGHBORHOOD_ = 17; 
ELSE IF Neighborhood = "NPkVill" then NEIGHBORHOOD_ = 18; 
ELSE IF Neighborhood = "NridgHt" then NEIGHBORHOOD_ = 19; 
ELSE IF Neighborhood = "NWAmes" then NEIGHBORHOOD_ = 20; 
ELSE IF Neighborhood = "OldTown" then NEIGHBORHOOD_ =21; 
ELSE IF Neighborhood = "SWISU" then NEIGHBORHOOD_ = 22; 
ELSE IF Neighborhood = "Sawyer" then NEIGHBORHOOD_ = 23; 
ELSE IF Neighborhood = "SawyerW" then NEIGHBORHOOD_ = 24; 
ELSE IF Neighborhood = "Somerst" then NEIGHBORHOOD_ = 25; 
ELSE IF Neighborhood = "StoneBr" then NEIGHBORHOOD_ = 26; 
ELSE IF Neighborhood = "Timber" then NEIGHBORHOOD_ = 27; 
ELSE IF Neighborhood = "Veenker" then NEIGHBORHOOD_ = 28; 
 


IF Condition_1 = "Artery" then CONDITION_1_ = 1; 
ELSE IF Condition_1 = "Adjacent" then CONDITION_1_ = 2;
ELSE IF Condition_1 = "Feedr"	then CONDITION_1_ = 3;
ELSE IF Condition_1 =  "Norm"	then CONDITION_1_ = 4;	
ELSE IF Condition_1 =  "RRNn"	then CONDITION_1_ = 5;
ELSE IF Condition_1 =  "RRAn"	then CONDITION_1_ = 6;
ELSE IF Condition_1 =  "PosN"	then CONDITION_1_ = 7;
ELSE IF Condition_1 =   "PosA"	then CONDITION_1_ = 8;
ELSE IF Condition_1 =   "RRNe"	then CONDITION_1_ = 9;
ELSE IF Condition_1 =  "RRAe"	then CONDITION_1_ = 10;

IF Condition_2 = "Artery" then CONDITION_2_ = 1; 
ELSE IF Condition_2 = "Feedr"	then CONDITION_2_ = 2;
ELSE IF Condition_2 =  "Norm"	then CONDITION_2_ = 3;	
ELSE IF Condition_2 =  "RRNn"	then CONDITION_2_ = 4;
ELSE IF Condition_2 =  "RRAn"	then CONDITION_2_ = 5;
ELSE IF Condition_2 =  "PosN"	then CONDITION_2_ = 6;
ELSE IF Condition_2 =   "PosA"	then CONDITION_2_ = 7;
ELSE IF Condition_2 =   "RRNe"	then CONDITION_2_ = 8;
ELSE IF Condition_2 =  "RRAe"	then CONDITION_2_ = 9;





IF House_Style = "1Story" then HOUSE_STYLE_ = 1; 
ELSE if House_Style = "1.5Fin" then HOUSE_STYLE_ = 2; 
ELSE IF House_Style = "1.5Unf" then HOUSE_StYLE_ = 3; 
ELSE IF House_Style = "2Story" then HOUSE_STYLE_ =  4; 
ELSE IF House_Style = '2.5Fin' then HOUSE_STYLE_ = 5; 
ELSE IF House_Style = "2.5Unf" then HOUSE_STYLE_ = 6; 
ELSE IF House_Style = "SFoyer" then HOUSE_STYLE_ = 7; 
ELSE IF House_Style = "SLvl" then HOUSE_STYLE_ = 8; 

IF Roof_Style = "Flat" then ROOF_STYLE = 1; 
ELSE if Roof_Style = "Gable" then ROOF_STYLE = 2; 
ELSE if Roof_Style = "Gambrel" then ROOF_STYLE = 3; 
ELSE if Roof_Style = "Hip" then ROOF_STYLE = 4; 
ELSE if Roof_Style = "Mansard" then ROOF_STYLE = 5; 
ELSE if Roof_Style = "Shed" then ROOF_STYLE = 6; 

IF Pool_QC = "Ex" then POOL_QC_ = 4; 
ELSE IF Pool_QC = "Gd" then POOL_QC_ = 3; 
ELSE IF Pool_QC = "TA" then POOL_QC_ = 2;
ELSE IF Pool_QC = "Fa" then POOL_QC_ = 1;
ELSE IF Pool_QC = "NA" then POOL_QC_ = 0;

IF Misc_Feature = "Elev" then MISC_FEATURE_ = 5; 
ELSE IF Misc_Feature = "Gar2" then MISC_FEATURE_ = 4; 
ELSE IF Misc_Feature = 'Othr' then MISC_FEATURE_ = 1; 
ELSE IF Misc_Feature = 'Shed' then MISC_FEATURE_ = 2; 
ELSE IF Misc_Feature = 'TenC' then MISC_FEATURE_ = 3; 
ELSE IF Misc_Feature = 'NA' then MISC_FEATURE_ = 0;


IF Garage_Type = "2Types" then GARAGE_TYPE_ = 1; 
ELSE IF Garage_Type = "Attchd" then GARAGE_TYPE_ = 2; 
ELSE IF Garage_Type = "Basment" then GARAGE_TYPE_ = 3; 
ELSE IF Garage_Type = "BuiltIn" then GARAGE_TYPE_ = 4; 
ELSE IF Garage_Type = "CarPort" then GARAGE_TYPE_ = 5; 
ELSE IF Garage_Type = "Detchd" then GARAGE_TYPE_ = 6; 
ELSE IF Garage_Type = "NA" then GARAGE_TYPE_ = 0;
ELSE GARAGE_TYPE_ = . ;

IF Central_Air = "N" then CENTRAL_AIR_ = 1; 
Else if Central_Air = "Y" then CENTRAL_AIR_ = 2; 
ELSE CENTRAL_AIR_ = .; 


If Heating = "Floor" then HEATING_ = 6; 
Else if Heating = "GasA" then HEATING_ = 5; 
Else if Heating = "GasW" then HEATING_ = 4; 
Else if Heating = "Grav" then HEATING_ = 3; 
Else if Heating = "OthW" then HEATING_ = 2; 
Else if Heating = "Wall" then HEATING_ = 1; 
Else  HEATING_ = .; 


*******Ordinal Variables ******; 

If Lot_Shape = "Reg" then LOT_SHAPE_ = 4; 
Else if Lot_Shape = "IR1" then LOT_SHAPE_ = 3; 
Else if Lot_Shape = "IR2" then LOT_SHAPE_ = 2; 
Else if Lot_Shape = "IR3" then LOT_SHAPE_ = 1; 
Else   LOT_SHAPE_ = .;

If Heating_QC = "Ex" then HEATING_QC_ = 5; 
Else if Heating_QC = "Gd" then HEATING_QC_ = 4; 
Else if Heating_QC = "TA" then HEATING_QC_ = 3; 
Else if Heating_QC = "Fa" then HEATING_QC_ = 2; 
Else if Heating_QC = "Po" then HEATING_QC_ = 1; 
Else HEATING_QC_ = .; 


IF Fence = "GdPrv" then FENCE = 4; 
ELSE IF Fence = "MnPrv" then FENCE = 3; 
ElSE IF Fence = "GdWo" then FENCE = 2; 
ELSE IF Fence = "MnWw" then FENCE = 1; 
ELSE IF Fence = "NA" then FENCE = 0; 



IF Fireplace_Qu = "Ex" then FIREPLACEQU = 5; 
ELSE IF Fireplace_Qu = "Gd" then FIREPLACEQU = 4; 
ELSE IF Fireplace_Qu = "TA" then FIREPLACEQU = 3;
ELSE IF Fireplace_Qu = "Fa" then FIREPLACEQU = 2;
ELSE IF Fireplace_Qu = "Po" then FIREPLACEQU = 1;
ELSE IF Fireplace_Qu = "NA" then FIREPLACEQU = 0;


If Garage_Yr_Blt = . then GARAGE_YR_BLT_ = 0; 
 

IF Garage_Qual = "Ex" then GARAGE_QUAL_ = 5; 
Else IF Garage_Qual = "Gd" then GARAGE_QUAL_ = 4; 
ELSE IF Garage_Qual = "TA" then GARAGE_QUAL_ = 3; 
ELSE IF Garage_Qual = "Fa" then GARAGE_QUAL_ = 2; 
ELSE IF Garage_Qual = "Po" then GARAGE_QUAL_ = 1; 
ELSE IF Garage_Qual = "NA" then GARAGE_QUAL_ = 0; 


IF Garage_Cond = "Ex" then GARAGE_COND_ = 5; 
Else IF Garage_Cond = "Gd" then GARAGE_COND_ = 4; 
ELSE IF Garage_Cond = "TA" then GARAGE_COND_ = 3; 
ELSE IF Garage_Cond = "Fa" then GARAGE_COND_ = 2; 
ELSE IF Garage_Cond = "Po" then GARAGE_COND_ = 1; 
ELSE IF Garage_Cond = "NA" then GARAGE_COND_ = 0; 


IF Garage_Finish = "Fin" then GARAGE_FINISH_ = 3; 
ELSE if Garage_Finish = "RFn" then GARAGE_FINISH_ = 2; 
Else if Garage_Finish = "Unf" then GARAGE_FINISH_ = 1; 
ELSE if Garage_Finish = "NA" then GARAGE_FINISH_ = 0; 


IF Bsmt_Qual = "Ex" then  BSMT_QUAL = 5; 
ELSE IF Bsmt_Qual = "Gd" then BSMT_QUAL_ = 4; 
ELSE IF Bsmt_Qual = "TA" then BSMT_QUAL_ = 3; 
ELSE IF Bsmt_Qual = "FA" then BSMT_QUAL_ = 2;
ELSE IF Bsmt_Qual = "Po" then BSMT_QUAL_ = 1;
ELSE If Bsmt_Qual = "NA" then BSMT_QUAL_ = 0; 
 

IF Bsmt_Cond = "Ex" then  BSMT_COND_ = 5; 
ELSE IF Bsmt_Cond = "Gd" then BSMT_COND_ = 4; 
ELSE IF Bsmt_Cond = "TA" then BSMT_COND_ = 3; 
ELSE IF Bsmt_Cond = "FA" then BSMT_COND_ = 2;
ELSE IF Bsmt_Cond = "Po" then BSMT_COND_ = 1;
ELSE If Bsmt_Cond = "NA" then BSMT_COND_ = 0; 

IF Bsmt_Exposure = "Gd" then  BSMT_EXP_ = 4; 
ELSE IF Bsmt_Exposure = "Av" then BSMT_EXP_ = 3; 
ELSE IF Bsmt_Exposure = "Mn" then BSMT_EXP_ = 2; 
ELSE IF Bsmt_Exposure = "No" then BSMT_EXP_ = 1;
ELSE If Bsmt_Exposure = "NA" then BSMT_EXP_ = 0; 


If BsmtFin_Type_1 = "GLQ" then BSMTFIN_TYPE1_ = 7; 
ELSE If BsmtFin_Type_1 = "ALQ" then BSMTFIN_TYPE1_ = 6; 
ELSE If BsmtFin_Type_1 = "BLQ" then BSMTFIN_TYPE1_ = 5; 
ELSE If BsmtFin_Type_1 = "Rec" then BSMTFIN_TYPE1_ = 4; 
ELSE If BsmtFin_Type_1 = "LwQ" then BSMTFIN_TYPE1_ = 3; 
ELSE If BsmtFin_Type_1 = "Unf" then BSMTFIN_TYPE1_ = 2; 
ELSE If BsmtFin_Type_1 = "NA" then BSMTFIN_TYPE1_ = 1; 
 


If BsmtFin_Type_2 = "GLQ" then BSMTFIN_TYPE2_ = 7; 
ELSE If BsmtFin_Type_2 = "ALQ" then BSMTFIN_TYPE2_ = 6; 
ELSE If BsmtFin_Type_2 = "BLQ" then BSMTFIN_TYPE2_ = 5; 
ELSE If BsmtFin_Type_2 = "Rec" then BSMTFIN_TYPE2_ = 4; 
ELSE If BsmtFin_Type_2 = "LwQ" then BSMTFIN_TYPE2_ = 3; 
ELSE If BsmtFin_Type_2 = "Unf" then BSMTFIN_TYPE2_ = 2; 
ELSE If BsmtFin_Type_2 = "NA" then BSMTFIN_TYPE2_ = 1; 


IF Pool_QC = "Ex" then POOL_QC1 = 4; 
ELSE IF Pool_QC = "Gd" then POOL_QC1 = 3; 
ELSE IF Pool_QC = "TA" then POOL_QC1 = 2; 
ELSE IF Pool_QC = "Fa" then POOL_QC1 = 1; 
ELSE IF Pool_QC = "NA" then POOL_QC1 = 0; 


IF Paved_Drive = "Y" then PAVED_DRIVE1 = 3; 
ELSE IF Paved_Drive = "P" then PAVED_DRIVE1= 2; 
ELSE IF Paved_Drive = "N" then PAVED_DRIVE1 = 1; 
 

IF Functional = "Typ" then FUNCTIONAL1 = 8; 
ELSE IF Functional = "Min1" then FUNCTIONAL1 = 7; 
ELSE IF Functional = "Min2" then FUNCTIONAL1 = 6; 
ELSE IF Functional = "Mod" then FUNCTIONAL1 = 5; 
ELSE IF Functional = "Maj1" then FUNCTIONAL1 = 4; 
ELSE IF Functional = "Maj2" then FUNCTIONAL1 = 3; 
ELSE IF Functional = "Sev" then FUNCTIONAL1 = 2; 
ELSE IF Functional = "Sal" then FUNCTIONAL1 = 1; 


IF Kitchen_Qual = "Ex" then KITCHENQUAL = 5; 
Else if Kitchen_Qual = "Gd" then KITCHENQUAL = 4; 
Else if Kitchen_Qual = "TA" then KITCHENQUAL = 3; 
Else if Kitchen_Qual = "Fa" then KITCHENQUAL = 2; 
Else if Kitchen_Qual = "Po" then KITCHENQUAL = 1; 
 

IF Electrical = "SBrkr" then ELECTRIC = 5; 
Else if Electrical = "FuseA" then ELECTRIC = 4; 
Else if Electrical = "FuseF" then ELECTRIC = 3; 
Else if Electrical = "FuseP" then ELECTRIC = 2; 
Else if Electrical = "Mix" then ELECTRIC = 1; 



run; 
Drop Alley Street Land_Contour Land_Config Neighborhood Condition_1 Con

Drop Alley, Street, Land_Contour, Land_Config, Neighborhood, Condition_1, Condition_2, Bldg_Type, House_Style, Roof_Style, Pool_QC, Misc_Feature, Garage_Type, Central_Air, Heating, Lot_Shape, Heating_QC, Fence, Fireplace_Qu, Garage_Yr_Blt, Garage_Qual, Garage_Cond, Garage_Finish, Bsmt_Qual, Bsmt_Cond, Bsmt_Exposure, BsmtFin_Type_1, BsmtFin_Type_2, Pool_QC, Paved_Drive, Functional, Kitchen_Qual Electrical 
