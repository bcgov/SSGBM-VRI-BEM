#=======================================================================================================================================================
#                                     Create Unique Ecosystems List for TEIS products (BEM/VRIxBEM)
#=======================================================================================================================================================
#
# Author(s)         :  Written by CloverPoint (Jeff Kruys) adapted to BEM by Island GIS (Shari Willmott)
#
# Date              :  2016-02-11
#
# Purpose           :  This script generates a list of unique BGC label and habitat combinations for the Look up Table starting point
#              
# Arguments         :  argv(1) = BEM feature class
#
# Example Input     :  X:\fullpath\BEM_V2 
#
# Outputs           :  CSV file called WHR_Lookup_Table_Template_datecreated saved in the project folder\Supporting_Data
#
# Dependencies      :  BEM feature class must have the following fields - SDEC_1, SDEC_2, SDEC_3, BEUMC_1, BEUMC_2, BEUMC_3
#              
# Expected Duration :  Depends on the size of the input BEM feature class.
#
# History           :  Script created for previous WHR projects. Adpated to run on BEM fields for BEM Ominceca Moose WHR (Madrone Dosier 15.0242)
# History           :  2020-12-29 (AE): Script used for the Skeena Moose WHR (Madrone Dosier 20.0221)
#
#========================================================================================================================================================

print "Initializing..."
import arcpy, sys, os, time, operator, pdb
dtCalcScriptStart = time.time()
print 'Start Time: ' + time.ctime(time.time())

if len(sys.argv) <> 2:
    print "- One argument required: full path to BEMv2 feature class."
    sys.exit()

#set environment settings
working_fc = sys.argv[1]
working_gdb = os.path.split(working_fc)[0]
whr_master_path = os.path.split(working_gdb)[0]
scratch_gdb = whr_master_path + r'\Scratch\Scratch.gdb'
out_csv = whr_master_path + r'\Supporting_Data\WHR_Lookup_Table_Template_' + time.strftime("%Y%m%d") + '.csv'

## ---------------------------------------------------------------------------------------------------------
## Check that the expected arguments were provided
## ---------------------------------------------------------------------------------------------------------

if not arcpy.Exists(working_fc):
    print "- Specified BEMv2 polygon feature class does not exist."
    sys.exit()

existing_working_fc_field_list = [f.name for f in arcpy.ListFields(working_fc)]

if "SDEC_1" not in existing_working_fc_field_list or "BGC_ZONE" not in existing_working_fc_field_list or "BEUMC_S1" not in existing_working_fc_field_list:
    print "- Specified polygon feature class is not BEMv2."
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
## Build dictionary from PEM/TEM data of all the unique combinations of ecosystem attributes
## and the total area occupied by each (proportioned by decile value)
## ---------------------------------------------------------------------------------------------------------

cursor_fields = ["BGC_ZONE","BGC_SUBZON","BGC_VRT","BGC_PHASE","SDEC_1","SDEC_2","SDEC_3","BEUMC_S1","BEUMC_S2","BEUMC_S3"]
cfd = {}
x = 0
for cursor_field in cursor_fields:
    cfd[cursor_field] = x
    x += 1
unique_eco_dict = {}
print "    - Reading PEM/TEM feature class into dictionary"
row_count = 0
with arcpy.da.SearchCursor(working_fc,cursor_fields) as cursor:
    for row in cursor:
        row_count += 1
        if str(row[cfd["SDEC_1"]]) in ['1','2','3','4','5','6','7','8','9','10']:
            key = str(row[cfd["BGC_ZONE"]]).replace('None','').replace(' ','') 
            key += '~' + str(row[cfd["BGC_SUBZON"]]).replace('None','').replace(' ','')
            key += '~' + str(row[cfd["BGC_VRT"]]).replace('None','').replace('0','')
            key += '~' + str(row[cfd["BGC_PHASE"]]).replace('None','').replace(' ','')
            key += '~' + str(row[cfd["BEUMC_S1"]]).replace('None','').replace(' ','')
            try:
                unique_eco_dict[key] += 1
            except:
                unique_eco_dict[key] = 1

        if str(row[cfd["SDEC_2"]]) in ['1','2','3','4','5','6','7','8','9','10']:
            key = str(row[cfd["BGC_ZONE"]]).replace('None','').replace(' ','') 
            key += '~' + str(row[cfd["BGC_SUBZON"]]).replace('None','').replace(' ','')
            key += '~' + str(row[cfd["BGC_VRT"]]).replace('None','').replace('0','')
            key += '~' + str(row[cfd["BGC_PHASE"]]).replace('None','').replace(' ','')
            key += '~' + str(row[cfd["BEUMC_S2"]]).replace('None','').replace(' ','')
            try:
                unique_eco_dict[key] += 1
            except:
                unique_eco_dict[key] = 1

        if str(row[cfd["SDEC_3"]]) in ['1','2','3','4','5','6','7','8','9','10']:
            key = str(row[cfd["BGC_ZONE"]]).replace('None','').replace(' ','') 
            key += '~' + str(row[cfd["BGC_SUBZON"]]).replace('None','').replace(' ','')
            key += '~' + str(row[cfd["BGC_VRT"]]).replace('None','').replace('0','')
            key += '~' + str(row[cfd["BGC_PHASE"]]).replace('None','').replace(' ','')
            key += '~' + str(row[cfd["BEUMC_S3"]]).replace('None','').replace(' ','')
            try:
                unique_eco_dict[key] += 1
            except:
                unique_eco_dict[key] = 1

        if row_count % 100000 == 0:
            print "        - Read " + str(row_count) + " records (" + str(len(unique_eco_dict.keys())) + " unique ecosystems found)"
    print "        - Read " + str(row_count) + " records (" + str(len(unique_eco_dict.keys())) + " unique ecosystems found)"

## ---------------------------------------------------------------------------------------------------------
## Write the CSV file (the table of unique ecosystems) to be populated with structural stage and stand
## composition codes by the biologist
## ---------------------------------------------------------------------------------------------------------

print "    - Writing output file " + out_csv

header_line = "FREQ,BGC_Label,BGC_ZONE,BGC_SUBZON,BGC_VRT,BGC_PHASE,BEU_MC,REALM,GROUP,CLASS,KIND,Forested (Y/N),Strct_Climax,Stand_Climax,Stand_Age_0-15,Stand_Age_16-30,Stand_Age_31-50,Stand_Age_51-80,Stand_Age_80+,Struct_Age_0-3,Struct_Age_4-10,Struct_Age_11-30,Struct_Age_31-40,Struct_Age_41-60,Struct_Age_61-80,Struct_Age_81-139,Struct_Age_140-249,Struct_Age_250+,Snow_Code\n"
outputfile = open(out_csv,"w")
outputfile.write(header_line)
for eco in sorted(set(unique_eco_dict.keys())):
    item_list = eco.split('~')
    ## for SITEMC_S, write the values as ="value" so that when the csv file is opened in Excel, it interprets numeric values as text
    output_line = str(unique_eco_dict[eco]) + ',,' + item_list[0] + ',' + item_list[1] + ',' + item_list[2] + ',' + item_list[3] + ',' + item_list[4] + ',,,,,,,,,,,,,,,,,,,\n'
    outputfile.write(output_line)
outputfile.close()

## ---------------------------------------------------------------------------------------------------------
## Done
## ---------------------------------------------------------------------------------------------------------

dtCalcNow = time.time()
dtCalcScriptElapsed = dtCalcNow - dtCalcScriptStart
print " "
print "- Script complete after " + SanitizeElapsedTime(dtCalcScriptElapsed)
print " "
print " **** Output file " + out_csv + " must now be manually populated in Excel."
