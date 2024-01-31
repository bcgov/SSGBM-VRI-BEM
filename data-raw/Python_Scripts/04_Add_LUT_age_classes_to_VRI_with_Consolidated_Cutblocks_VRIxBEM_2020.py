#==================================================================================================================
#                                      Add LUT Age Classes to VRI with Consolidated Cutblocks.py
#==================================================================================================================
#
# Author(s)         :  Written by CloverPoint (Jeff Kruys) 
#
# Date              :  2016-03-14
#
# Purpose           :  This script adds the Lookup Table age classes to the VRI feature class
#                                   
# Arguments         :  argv(1) = VRI feature class
#
# Recommendations   :  LOOK FOR HARVEST AGE, RIGHT NOW IT IS 2020!!
#
# Example Input     :  X:\fullpath\VRIxBEM
#
# Outputs           :  Updated Age Class fields in the VRIxBEM feature class VRI_AGE_CL_STS and VRI_AGE_CL_STD added to user-specified VRI feature class
#
# Dependencies      :  VRI feature class must contain the following fields: "BCLCS_LEVEL_1", "PROJ_AGE_CLASS_CD_1", 
#                      "HARVEST_DATE", "FID_Consolidated_Cutblock_Clip","HARVESTYR_CC" (these last two fields are created
#                      through an intermediate step using the Consolidated Cut Blocks data and the VRI. It is not part of
#                      this script at this point.) Make sure to double check the harvest year values for the consolidated 
#                      cutblock data and update the script at line 100 and line 110 (see comments). Additional documentation
#                      for how this script populates the VRI_AGE_CL_STS and VRI_AGE_CL_STD fields can be found in the
#                      BEM 3.1 Arcview\Documentation\GIS Methods for BEI Update for Omineca Study Area.docx
#
# Expected Duration :  Depends on the size of the input BEM feature class and 
#
# History           :  2016-03-14 (JK): Script created for the BEI Omineca Moose WHR (Madrone Dosier 15.0242)
# History           :  2020-09-04 (AE): Script used for the Northeast Moose WHR (Madrone Dosier 19.0461)
# History           :  2021-01-03 (AE): Script used for the Skeena Moose WHR (Madrone Dosier 20.0221)
#
#==================================================================================================================

print "Initializing..."
import arcpy, sys, os, time, pdb
dtCalcScriptStart = time.time()
print 'Start Time: ' + time.ctime(time.time())

if len(sys.argv) <> 2:
    print "- One argument required: full path to VRI feature class."
    sys.exit()

#set environment settings
working_fc = sys.argv[1]
working_gdb = os.path.split(working_fc)[0]
whr_master_path = os.path.split(working_gdb)[0]


## ---------------------------------------------------------------------------------------------------------
## Check that the expected arguments were provided
## ---------------------------------------------------------------------------------------------------------

vri_fc = sys.argv[1]
if not arcpy.Exists(vri_fc):
    print "- Specified VRI polygon feature class does not exist."
    sys.exit()

existing_vri_field_list = [f.name for f in arcpy.ListFields(vri_fc)]

if "BCLCS_LEVEL_1" not in existing_vri_field_list or "PROJ_AGE_CLASS_CD_1" not in existing_vri_field_list or "HARVEST_DATE" not in existing_vri_field_list:
    print "- Specified polygon feature class is not in specified VRI format."
    sys.exit()

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
## Do the stuff
## ---------------------------------------------------------------------------------------------------------

if "VRI_AGE_CL_STS" not in existing_vri_field_list:
    print "    - Adding field VRI_AGE_CL_STS to VRI feature class"
    arcpy.AddField_management(vri_fc,"VRI_AGE_CL_STS","SHORT")
if "VRI_AGE_CL_STD" not in existing_vri_field_list:
    print "    - Adding field VRI_AGE_CL_STD to VRI feature class"
    arcpy.AddField_management(vri_fc,"VRI_AGE_CL_STD","SHORT")

cursor_fields = ["PROJ_AGE_1","VRI_AGE_CL_STS","VRI_AGE_CL_STD","HARVEST_DATE","FID_Cut_Block_all_BC","HARVESTYR_CC"]
##                    0                1               2                3                 4                 5

row_count = 0
with arcpy.da.UpdateCursor(vri_fc,cursor_fields) as cursor:
    for row in cursor:
        row_count += 1
        proj_age = row[0]
        fid_cc = row[4]
        cc_harv_year = row[5]

        if not proj_age >= 0:
            harv_year_string = str(row[3])[:4]
            try:
                harv_year_int = int(harv_year_string)  ## if harvest year is null, causes error here and skips to except clause below
# change the year in the calculation below to match that for the most recent harvest year from the consolidated cutblock data
# i.e. if the CC data has a harvest year for 2020 then the year below must match 
                proj_age = 2020 - harv_year_int
            except: ## no harvest year value. can't evaluate age class.
                pass

        if fid_cc != -1:
            harv_year_string = str(row[5])[:4]
            try:
                harv_year_int = int(harv_year_string)  ## if harvest year is null, causes error here and skips to except clause below
# change the year in the calculation below to match that for the most recent harvest year from the consolidated cutblock data
# i.e. if the CC data has a harvest year for 2020 then the year below must match 
                proj_age = 2020 - harv_year_int
            except: ## no harvest year value. can't evaluate age class.
                pass

        ## populate VRI_AGE_CL_STS (age class for looking up structural stage from the LUT)
        if 0 <= proj_age <= 3:
            row[1] = 2
        elif 4 <= proj_age <= 10:
            row[1] = 7
        elif 11 <= proj_age <= 30:
            row[1] = 20
        elif 31 <= proj_age <= 40:
            row[1] = 35
        elif 41 <= proj_age <= 60:
            row[1] = 50
        elif 61 <= proj_age <= 80:
            row[1] = 70
        elif 81 <= proj_age <= 140:
            row[1] = 125
        elif 141 <= proj_age <= 249:
            row[1] = 195        
        elif 250 <= proj_age:
            row[1] = 301
        else:
            row[1] = -1

        ## populate VRI_AGE_CL_STD (age class for looking up stand composition from the LUT)
        if 0 <= proj_age <= 15:
            row[2] = 15
        elif 16 <= proj_age <= 30:
            row[2] = 30
        elif 31 <= proj_age <= 50:
            row[2] = 50
        elif 51 <= proj_age <= 80:
            row[2] = 80
        elif 81 <= proj_age:
            row[2] = 9999
        else:
            row[2] = -1

        cursor.updateRow(row)
        if row_count == 100 or row_count % 100000 == 0:
                print "            - Processed " + str(row_count) + " rows"
    print "            - Processed " + str(row_count) + " rows"

## ---------------------------------------------------------------------------------------------------------
## Done
## ---------------------------------------------------------------------------------------------------------

dtCalcNow = time.time()
dtCalcScriptElapsed = dtCalcNow - dtCalcScriptStart
print " "
print "- Script complete after " + SanitizeElapsedTime(dtCalcScriptElapsed)
