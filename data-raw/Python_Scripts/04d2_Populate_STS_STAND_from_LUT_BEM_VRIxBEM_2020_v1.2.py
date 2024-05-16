#======================================================================================================================
#                                     Populate STS and STAND from Look Up Table 
#======================================================================================================================
#
# Author(s)         :  Written by CloverPoint (Jeff Kruys); Edited by Shari Willmott to work with BEM v3_2
#
# Date              :  2017-05-23
#                   :  2019-01-27
#  
# Purpose           :  This script populates the structrual stage and stand values in the BEM feature class from the 
#                      data provided by the Lookup Table
#
# Arguments         :  argv(1) = BEM feature class
#                   :  argv(2) = STS Lookup Table csv file
#
# Example Input     :  X:\fullpath\BEM_V2 Y:\fullpath\LUT.csv
#
# Outputs           :  BEM feature class is updated with the fields AGE_CL_STS, AGE_CL_STD, FORESTED_1, FORESTED_2 and 
#                      FORESTED_3 and populates the attributes for these fields based on the VRI data and the Lookup 
#                      Table
#
# Dependencies      :  The csv file must have values for the above fields for each unique EcoSection, BGC and BEU_MC 
#                      combination and the VRI feature class must have VRI_AGE_CL_STS and VRI_AGE_CL_STD fields. If 
#                      not, run the script 04a_Add_LUT_age_classes_to_VRI.py first
#              
# Expected Duration :  Depends on the size of the input BEM feature class
#
# History           :  Script created for BEM Ominceca Moose WHR (Madrone Dosier 15.0242)
#                      Script updated to allow Stand value "B" to be assigned for ecosystem units that have Structural 
#                      Stages 3b and 3c (BEM3.2)
#                   :  2020-09-04 (AE): Script used for the Northeast Moose WHR (Madrone Dosier 19.0461)
#                   :  2021-01-03 (AE): Script used for the Skeena Moose WHR (Madrone Dosier 20.0221)
#                   :  2021-03-01 (JK): Update the assignment of Stand codes to use the value in the STD_VRI field 
#                      first.
#                   :  2021-03-19 (JK): Remove the VRI loverlay step; update logic for assigning STRCT_S# and STAND_A#.
#
#======================================================================================================================

print("Initializing...")
import arcpy, sys, os, time, pdb, operator, csv
dtCalcScriptStart = time.time()
print('Start Time: ' + time.ctime(time.time()))

if len(sys.argv) != 3:
    print("- Two arguments required: WHR (PEM/TEM or BEM) feature class, and Structural Stage LUT CSV file.")
    sys.exit()

#set environment settings
working_fc = sys.argv[1]
working_gdb = os.path.split(working_fc)[0]
whr_master_path = os.path.split(working_gdb)[0]
sts_csv = sys.argv[2]

## ---------------------------------------------------------------------------------------------------------
## Check that the expected arguments were provided and that all required fields exist
## ---------------------------------------------------------------------------------------------------------

working_fc = sys.argv[1]
if not arcpy.Exists(working_fc):
    print("- Specified WHR polygon feature class does not exist.")
    sys.exit()

bem_req_fields = ["TEIS_ID", "STD_VRI", "VRI_AGE_CL_STS", "VRI_AGE_CL_STD", "BGC_ZONE", "BGC_SUBZON", "BGC_VRT", "BGC_PHASE", 
                  "SDEC_1", "SDEC_2", "SDEC_3", "BEUMC_S1", "BEUMC_S2", "BEUMC_S3", "STRCT_S1", "STRCT_S2", "STRCT_S3", 
                  "STAND_A1", "STAND_A2", "STAND_A3"]
bem_existing_fields = [f.name for f in arcpy.ListFields(working_fc)]
bem_missing_fields = []
for f in bem_req_fields:
    if f not in bem_existing_fields:
        bem_missing_fields.append(f)
if len(bem_missing_fields) > 0:
    print("**** Specified BEM polygon feature class is missing required fields: {}".format(str(bem_missing_fields)))
    sys.exit()

lut_req_fields = ["BGC_Label", "BGC_ZONE", "BGC_SUBZON", "BGC_VRT", "BGC_PHASE", "BEU_MC", "REALM", "GROUP", "CLASS", 
                  "KIND", "Forested (Y/N)", "Strct_Climax", "Stand_Climax", "Stand_Age_0-15", "Stand_Age_16-30", 
                  "Stand_Age_31-50", "Stand_Age_51-80", "Stand_Age_80+", "Struct_Age_0-3", "Struct_Age_4-10", 
                  "Struct_Age_11-30", "Struct_Age_31-40", "Struct_Age_41-60", "Struct_Age_61-80", "Struct_Age_81-139", 
                  "Struct_Age_140-249", "Struct_Age_250+", "Snow_Code"]
sts_csv_obj = open(sts_csv, "rU")
csvReader = csv.DictReader(sts_csv_obj)
lut_missing_fields = []
csv_line_total = 0
for data in csvReader:
    csv_line_total += 1
for req_field in lut_req_fields:
    if req_field not in data.keys():
        lut_missing_fields.append(req_field)
if len(lut_missing_fields) > 0:
    print("*** Specified structural stage lookup table missing required field(s): {}".format(str(lut_missing_fields)))
    sys.exit()
del csvReader
sts_csv_obj.close()

## ---------------------------------------------------------------------------------------------------------
## Define routine to format duration value in seconds into hours or minutes
## ---------------------------------------------------------------------------------------------------------

def SanitizeElapsedTime(dtInput):
    if dtInput < 120:
        strElapsedTime = str(int(dtInput)) + ' sec.'
    elif dtInput < 3600:
        strElapsedTime = str(round(dtInput / 60, 1)) + ' min.'
    else:
        strElapsedTime = str(round(dtInput / 3600, 2)) + ' hr.'
    return strElapsedTime

## ---------------------------------------------------------------------------------------------------------
## Read the structural stage lookup table into dictionaries
## ---------------------------------------------------------------------------------------------------------

print("Reading STS lookup table")

forested_dict = {}
stand_climax_dict = {}
sts_climax_dict = {}
sts_age_lookup_dict = {}
stand_age_lookup_dict = {}
line_counter = 0

sts_csv_obj = open(sts_csv, "rU")
csvReader = csv.DictReader(sts_csv_obj)
for data in csvReader:
    line_counter += 1
    curr_bgc_zone          = str(data["BGC_ZONE"]).replace('"','').replace(' ','')
    curr_bgc_subzon        = str(data["BGC_SUBZON"]).replace('"','').replace(' ','')
    curr_bgc_vrt           = str(data["BGC_VRT"]).replace('"','').replace('0','')
    curr_bgc_phase         = str(data["BGC_PHASE"]).replace('"','').replace(' ','')
    curr_beu_mc            = str(data["BEU_MC"]).replace('"','').replace(' ','')
    curr_forested          = str(data["Forested (Y/N)"]).replace('"','').replace(' ','')
    curr_sts_climax        = str(data["Strct_Climax"]).replace('"','').replace(' ','')
    curr_stand_climax      = str(data["Stand_Climax"]).replace('"','').replace(' ','')
    curr_STAND_Age_0_15    = str(data["Stand_Age_0-15"]).replace('"','').replace(' ','')
    curr_STAND_Age_16_30   = str(data["Stand_Age_16-30"]).replace('"','').replace(' ','')
    curr_STAND_Age_31_50   = str(data["Stand_Age_31-50"]).replace('"','').replace(' ','')
    curr_STAND_Age_51_80   = str(data["Stand_Age_51-80"]).replace('"','').replace(' ','')
    curr_STAND_Age_81_9999 = str(data["Stand_Age_80+"]).replace('"','').replace(' ','')
    curr_STS_Age_0_3       = str(data["Struct_Age_0-3"]).replace('"','').replace(' ','')
    curr_STS_Age_4_10      = str(data["Struct_Age_4-10"]).replace('"','').replace(' ','')
    curr_STS_Age_11_30     = str(data["Struct_Age_11-30"]).replace('"','').replace(' ','')
    curr_STS_Age_31_40     = str(data["Struct_Age_31-40"]).replace('"','').replace(' ','')
    curr_STS_Age_41_60     = str(data["Struct_Age_41-60"]).replace('"','').replace(' ','')
    curr_STS_Age_61_80     = str(data["Struct_Age_61-80"]).replace('"','').replace(' ','')          
    curr_STS_Age_81_139    = str(data["Struct_Age_81-139"]).replace('"','').replace(' ','')          
    curr_STS_Age_140_249   = str(data["Struct_Age_140-249"]).replace('"','').replace(' ','')          
    curr_STS_Age_250_9999  = str(data["Struct_Age_250+"]).replace('"','').replace(' ','')          

    curr_bgc_unit = curr_bgc_zone + curr_bgc_subzon + curr_bgc_vrt + curr_bgc_phase
    curr_eco_unit = curr_bgc_unit + "~" + curr_beu_mc

    sts_age_lookup_dict[curr_eco_unit] = {}
    sts_age_lookup_dict[curr_eco_unit][2] = curr_STS_Age_0_3
    sts_age_lookup_dict[curr_eco_unit][7] = curr_STS_Age_4_10
    sts_age_lookup_dict[curr_eco_unit][20] = curr_STS_Age_11_30
    sts_age_lookup_dict[curr_eco_unit][35] = curr_STS_Age_31_40
    sts_age_lookup_dict[curr_eco_unit][50] = curr_STS_Age_41_60
    sts_age_lookup_dict[curr_eco_unit][70] = curr_STS_Age_61_80
    sts_age_lookup_dict[curr_eco_unit][125] = curr_STS_Age_81_139
    sts_age_lookup_dict[curr_eco_unit][195] = curr_STS_Age_140_249
    sts_age_lookup_dict[curr_eco_unit][301] = curr_STS_Age_250_9999

    stand_age_lookup_dict[curr_eco_unit] = {}
    stand_age_lookup_dict[curr_eco_unit][15] = curr_STAND_Age_0_15
    stand_age_lookup_dict[curr_eco_unit][30] = curr_STAND_Age_16_30
    stand_age_lookup_dict[curr_eco_unit][50] = curr_STAND_Age_31_50
    stand_age_lookup_dict[curr_eco_unit][80] = curr_STAND_Age_51_80
    stand_age_lookup_dict[curr_eco_unit][9999] = curr_STAND_Age_81_9999
            
    sts_climax_dict[curr_eco_unit] = curr_sts_climax
    stand_climax_dict[curr_eco_unit] = curr_stand_climax
    forested_dict[curr_eco_unit] = curr_forested

    if line_counter == 100 or line_counter % 1000 == 0 or line_counter == csv_line_total:
        print("    - Read " + str(line_counter) + " lines")

vri_stand_class_dict = {}
vri_stand_class_dict[-1] = -1
vri_stand_class_dict[2] = 15
vri_stand_class_dict[7] = 15
vri_stand_class_dict[20] = 30
vri_stand_class_dict[35] = 50
vri_stand_class_dict[50] = 50
vri_stand_class_dict[70] = 80
vri_stand_class_dict[125] = 9999
vri_stand_class_dict[195] = 9999
vri_stand_class_dict[301] = 9999

## ---------------------------------------------------------------------------------------------------------
## Overlay WHR with VRI and write the age classes and looked-up structural stage and stand codes
## ---------------------------------------------------------------------------------------------------------

for i in range(1, 4):
    new_field = "FORESTED_{}".format(i)
    if new_field not in bem_existing_fields:
        print("Adding {} field to {}".format(new_field, working_fc))
        arcpy.AddField_management(working_fc, new_field, "TEXT", "#", "#", "1")

cursor_fields = ["TEIS_ID", "STD_VRI", "VRI_AGE_CL_STS", "VRI_AGE_CL_STD", "BGC_ZONE", "BGC_SUBZON", "BGC_VRT", "BGC_PHASE", 
                 "SDEC_1", "SDEC_2", "SDEC_3", "BEUMC_S1", "BEUMC_S2", "BEUMC_S3", "STRCT_S1", "STRCT_S2", "STRCT_S3", 
                 "STAND_A1", "STAND_A2", "STAND_A3", "FORESTED_1", "FORESTED_2", "FORESTED_3", "SHAPE@XY"]

print("Updating attribute values in WHR polygons")
row_count = 0
row_total = int(arcpy.GetCount_management(working_fc).getOutput(0))
cfd = {}
x = 0
for cursor_field in cursor_fields:
    cfd[cursor_field] = x
    x += 1
with arcpy.da.UpdateCursor(working_fc, cursor_fields) as cursor:
    for row in cursor:
        row_count += 1
        
        # STRCT_S# is assigned the structural stage code from the Structural Stage Look-up Table using the BEC unit and 
        # first component ecosystem codes and the age value of highest precedence. The age value to be used is 
        # VRI_AGE_CL_STS; if age class value is -1, then, we use null if the STRCT_S1 attribute is null; if there is no 
        # other age information, and the ecosystem is not forested (Forested = N), or a non-productive forest such as 
        # parkland forest (Forested = Y and Climax STS of "7" or "3b" indicating krummholtz), we use the climax 
        # structural stage code for the ecosystem unit according to the Structural Stage Look-up Table.

        # If the value assigned to the STRCT_S# attribute begins with 1-2 or is null, STAND_D# is assigned a null 
        # value; if STRCT_S# begins with 3-7, STAND_D# is assigned the stand composition code from the Structural Stage 
        # Look-up Table using the BEC unit and first component ecosystem codes and the age value of highest precedence. 
        # The age value to be used is VRI_AGE_CL_STAND; if age class value is -1 and ecosystem is not forested 
        # (Forested = N), or a non-productive forest such as parkland forest (Forested = Y and Climax STS of "7" or "3b" 
        # indicating krummholtz), we use the climax stand composition code for the ecosystem unit according to the 
        # Look-up Table.  

        # Updated logic:
        # If the ecosystem unit is found in the LUT:
        #     Assign forested value from LUT to the FORESTED_# field
        #     If value in VRI_AGE_CL_STS is > 0: 
        #         Look up the STS value from the LUT and assign it to STRCT_S#
        #     If VRI_AGE_CL_STS = -1:
        #         If FORESTED_# = 'N' or BGC_SUBZON ends with "p" (parkland):
        #             Look up the climax STS from the LUT and assign it to STRCT_S#
        #         Otherwise:
        #             Assign a blank to STRCT_S#
        #     If STRCT_S# is blank, 1, 2 or 3:
        #         Assign blank to STAND_A#
        #     If STRCT_S# begins with 4 to 7:
        #         If STD_VRI is 'B', 'C' or 'M':
        #             Assign the value in STD_VRI to STAND_A#
        #         If STD_VRI is blank:
        #             If VRI_AGE_CL_STD > 0:
        #                 Look up the STD code from the LUT and assign it to STAND_A#
        #             If VRI_AGE_CL_STD = -1:
        #                 If FORESTED_# = 'N' or BGC_SUBZON ends with "p" (parkland):
        #                     Look up the climax STD code from the LUT and assign it to STAND_A#
        #                 Otherwise:
        #                     Assign a blank to STAND_A#

        for i in range(1, 4):
            if row[cfd["SDEC_{}".format(i)]] > 0:
                eco_unit = "{}{}{}{}~{}".format(str(row[cfd["BGC_ZONE"]]).replace('None',''), 
                                                str(row[cfd["BGC_SUBZON"]]).replace('None',''),
                                                str(row[cfd["BGC_VRT"]]).replace('None','').replace('0',''),
                                                str(row[cfd["BGC_PHASE"]]).replace('None',''),
                                                str(row[cfd["BEUMC_S{}".format(i)]]).replace('None',''))
                try:
                    row[cfd["FORESTED_{}".format(i)]] = forested_dict[eco_unit]
                except:
                    row[cfd["FORESTED_{}".format(i)]] = ''
                if eco_unit in sts_age_lookup_dict.keys():
                    if row[cfd["VRI_AGE_CL_STS"]] > 0:
                        row[cfd["STRCT_S{}".format(i)]] = sts_age_lookup_dict[eco_unit][row[cfd["VRI_AGE_CL_STS"]]]
                    elif forested_dict[eco_unit] == 'N' or str(row[cfd["BGC_SUBZON"]]).replace('None','')[:-1] == 'p':
                        row[cfd["STRCT_S{}".format(i)]] = sts_climax_dict[eco_unit]
                    else:
                        row[cfd["STRCT_S{}".format(i)]] = ''
                    if row[cfd["STRCT_S{}".format(i)]][:1] in ['4', '5', '6', '7']:
                        if row[cfd["STD_VRI"]] in ['B', 'C', 'M']:
                            row[cfd["STAND_A{}".format(i)]] = row[cfd["STD_VRI"]]
                        elif row[cfd["VRI_AGE_CL_STD"]] > 0:
                            row[cfd["STAND_A{}".format(i)]] = stand_age_lookup_dict[eco_unit][row[cfd["VRI_AGE_CL_STD"]]]
                        elif forested_dict[eco_unit] == 'N' or str(row[cfd["BGC_SUBZON"]]).replace('None','')[
                                :-1] == 'p':
                            row[cfd["STAND_A{}".format(i)]] = stand_climax_dict[eco_unit][row[cfd["VRI_AGE_CL_STD"]]]
                        else:
                            row[cfd["STAND_A{}".format(i)]] = ''
                    else:
                        row[cfd["STAND_A{}".format(i)]] = ''
                else:
                    row[cfd["STRCT_S{}".format(i)]] = ''
                    row[cfd["STAND_A{}".format(i)]] = ''
            else:
                row[cfd["STRCT_S{}".format(i)]] = ''
                row[cfd["STAND_A{}".format(i)]] = ''

        cursor.updateRow(row)

        if row_count == 100 or row_count % 10000 == 0 or row_count == row_total:
            print("    - Processed {} of {} rows".format(row_count, row_total))

## ---------------------------------------------------------------------------------------------------------
## Done
## ---------------------------------------------------------------------------------------------------------

dtCalcNow = time.time()
dtCalcScriptElapsed = dtCalcNow - dtCalcScriptStart
print(" ")
print("Script complete after {}".format(SanitizeElapsedTime(dtCalcScriptElapsed)))
