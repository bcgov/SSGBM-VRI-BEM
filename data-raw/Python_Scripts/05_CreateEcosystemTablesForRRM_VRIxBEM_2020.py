#===================================================================================================================================================
#                                      Creates an Ecosystem Table for RRM input
#===================================================================================================================================================
#
# Author(s)         :  Written by CloverPoint (Jeff Kruys) 
#
# Date              :  2016-03-22
#
# Purpose           :  This script generates the unique list of ecosystems and determines all possible combinations of STS and STAND codes 
#                      that could apply to the ecosystem at any age value, and expand to include all existing and "potential" unique ecosystems
#              
# Arguments         :  argv(1) = BEM feature class
#                   :  argv(2) = Lookup Table csv file                  
#
# Example Input     :  X:\fullpath\BEM_V2 X:\fullpath\LookupTable.csv 
#
# Outputs           :  An .xlsx file (the table of unique ecosystems) to be pasted to a worksheet in the RRM spreadsheet tool
#
# Dependencies      :  The TEIS_ID field in the input BEM feature class needs to be populated with unique values.
#              
# Expected Duration :  Depends on the size of the input BEM feature class
#
# History           :  Script created for previous WHR projects. Adpated to run on BEM fields for BEM Ominceca Moose WHR (Madrone Dosier 15.0242)
#                   :  2020-09-04 (AE): Script used for the Northeast Moose WHR (Madrone Dosier 19.0461)
#                   :  2021-01-03 (AE): Script used for the Skeena Moose WHR (Madrone Dosier 20.0221)
#                   :  2021-02-17 (AE): Added CROWN_MOOSE to the script 
#==================================================================================================================================================


print "Initializing..."
import arcpy, sys, os, time, operator, csv, xlsxwriter, pdb
dtCalcScriptStart = time.time()
print 'Start Time: ' + time.ctime(time.time())

if len(sys.argv) <> 3:
    print "- Two arguments required: WHR PEM/TEM/BEM feature class, and Structural Stage lookup table CSV file."
    sys.exit()

#set environment settings
working_fc = sys.argv[1]
working_gdb = os.path.split(working_fc)[0]
whr_master_path = os.path.split(working_gdb)[0]
out_xlsx = whr_master_path + r'\Supporting_Data\WHR_RRM_Input_Table_BEMV2_' + time.strftime("%Y%m%d") + '.xlsx'

## ---------------------------------------------------------------------------------------------------------
## Check that the expected arguments were provided
## ---------------------------------------------------------------------------------------------------------

working_fc = sys.argv[1]
if not arcpy.Exists(working_fc):
    print "- Specified WHR feature class does not exist."
    sys.exit()

sts_csv = sys.argv[2]
if not os.path.exists(sts_csv):
    print "- Specified Structural Stage lookup table CSV file does not exist."
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

cursor_fields = ["TEIS_ID","BAPID","ECO_SEC","BGC_ZONE","BGC_SUBZON","BGC_VRT","BGC_PHASE","SDEC_1","SDEC_2","SDEC_3","BEUMC_S1","BEUMC_S2","BEUMC_S3","SITEMC_S1","SITEMC_S2","SITEMC_S3","STRCT_S1","STRCT_S2","STRCT_S3","STAND_A1","STAND_A2","STAND_A3","CROWN_MOOSE_1","CROWN_MOOSE_2","CROWN_MOOSE_3","SLOPE_MOD","SITE_M3A","SNOW_CODE","ABOVE_ELEV_THOLD","Shape_Area"]
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
        if str(row[cfd["SDEC_1"]]) in ['1','2','3','4','5','6','7','8','9','10'] and not (str(row[cfd["BEUMC_S1"]]) in ['',' ','None']):
            key = str(row[cfd["ECO_SEC"]]).replace('None','').replace(' ','') 
            key += '~' + str(row[cfd["BGC_ZONE"]]).replace('None','').replace(' ','') 
            key += '~' + str(row[cfd["BGC_SUBZON"]]).replace('None','').replace(' ','')
            key += '~' + str(row[cfd["BGC_VRT"]]).replace('None','').replace('0','')
            key += '~' + str(row[cfd["BGC_PHASE"]]).replace('None','').replace(' ','')
            key += '~' + str(row[cfd["BEUMC_S1"]]).replace('None','').replace(' ','')
            key += '~' + str(row[cfd["SLOPE_MOD"]]).replace('None','').replace(' ','')
            key += '~' + str(row[cfd["SITE_M3A"]]).replace('None','').replace(' ','')
            key += '~' + str(row[cfd["SNOW_CODE"]]).replace('None','').replace(' ','')
            key += '~' + str(row[cfd["ABOVE_ELEV_THOLD"]]).replace('None','').replace(' ','')
            key += '~' + str(row[cfd["CROWN_MOOSE_1"]]).replace('None','').replace(' ','')
            key += '~' + str(row[cfd["STRCT_S1"]]).replace('None','').replace(' ','')
            key += '~' + str(row[cfd["STAND_A1"]]).replace('None','').replace(' ','')
            try:
                unique_eco_dict[key] += row[cfd["Shape_Area"]] * row[cfd["SDEC_1"]] / 10
            except:
                unique_eco_dict[key] = row[cfd["Shape_Area"]] * row[cfd["SDEC_1"]] / 10

        if str(row[cfd["SDEC_2"]]) in ['1','2','3','4','5','6','7','8','9','10'] and not (str(row[cfd["BEUMC_S2"]]) in ['',' ','None']):
            key = str(row[cfd["ECO_SEC"]]).replace('None','').replace(' ','') 
            key += '~' + str(row[cfd["BGC_ZONE"]]).replace('None','').replace(' ','') 
            key += '~' + str(row[cfd["BGC_SUBZON"]]).replace('None','').replace(' ','')
            key += '~' + str(row[cfd["BGC_VRT"]]).replace('None','').replace('0','')
            key += '~' + str(row[cfd["BGC_PHASE"]]).replace('None','').replace(' ','')
            key += '~' + str(row[cfd["BEUMC_S2"]]).replace('None','').replace(' ','')
            key += '~' + str(row[cfd["SLOPE_MOD"]]).replace('None','').replace(' ','')
            key += '~' + str(row[cfd["SITE_M3A"]]).replace('None','').replace(' ','')
            key += '~' + str(row[cfd["SNOW_CODE"]]).replace('None','').replace(' ','')
            key += '~' + str(row[cfd["ABOVE_ELEV_THOLD"]]).replace('None','').replace(' ','')
            key += '~' + str(row[cfd["CROWN_MOOSE_2"]]).replace('None','').replace(' ','')
            key += '~' + str(row[cfd["STRCT_S2"]]).replace('None','').replace(' ','')
            key += '~' + str(row[cfd["STAND_A2"]]).replace('None','').replace(' ','')
            try:
                unique_eco_dict[key] += row[cfd["Shape_Area"]] * row[cfd["SDEC_2"]] / 10
            except:
                unique_eco_dict[key] = row[cfd["Shape_Area"]] * row[cfd["SDEC_2"]] / 10

        if str(row[cfd["SDEC_3"]]) in ['1','2','3','4','5','6','7','8','9','10'] and not (str(row[cfd["BEUMC_S3"]]) in ['',' ','None']):
            key = str(row[cfd["ECO_SEC"]]).replace('None','').replace(' ','') 
            key += '~' + str(row[cfd["BGC_ZONE"]]).replace('None','').replace(' ','') 
            key += '~' + str(row[cfd["BGC_SUBZON"]]).replace('None','').replace(' ','')
            key += '~' + str(row[cfd["BGC_VRT"]]).replace('None','').replace('0','')
            key += '~' + str(row[cfd["BGC_PHASE"]]).replace('None','').replace(' ','')
            key += '~' + str(row[cfd["BEUMC_S3"]]).replace('None','').replace(' ','')
            key += '~' + str(row[cfd["SLOPE_MOD"]]).replace('None','').replace(' ','')
            key += '~' + str(row[cfd["SITE_M3A"]]).replace('None','').replace(' ','')
            key += '~' + str(row[cfd["SNOW_CODE"]]).replace('None','').replace(' ','')
            key += '~' + str(row[cfd["ABOVE_ELEV_THOLD"]]).replace('None','').replace(' ','')
            key += '~' + str(row[cfd["CROWN_MOOSE_3"]]).replace('None','').replace(' ','')
            key += '~' + str(row[cfd["STRCT_S3"]]).replace('None','').replace(' ','')
            key += '~' + str(row[cfd["STAND_A3"]]).replace('None','').replace(' ','')
            try:
                unique_eco_dict[key] += row[cfd["Shape_Area"]] * row[cfd["SDEC_3"]] / 10
            except:
                unique_eco_dict[key] = row[cfd["Shape_Area"]] * row[cfd["SDEC_3"]] / 10

        if row_count % 100000 == 0:
            print "        - Read " + str(row_count) + " records (" + str(len(unique_eco_dict.keys())) + " unique ecosystems found)"
    print "        - Read " + str(row_count) + " records (" + str(len(unique_eco_dict.keys())) + " unique ecosystems found)"

## ---------------------------------------------------------------------------------------------------------
## Read the structural stage lookup table into dictionaries
## ---------------------------------------------------------------------------------------------------------

print "    - Reading STS lookup table"
# required_csv_fields = ["FREQ", "BGC_ZONE", "BGC_SUBZON", "BGC_VRT", "BGC_PHASE", "SITE_S", "SITEMC_S", "Operable(Y/N)", "Default to original STS (Y/N)", "SS_Climax", "Scomp_Climax", "SComp_Age_1-15", "SComp_Age_16-30", "SComp_Age_31-50", "SComp_Age_51-80", "SComp_Age_>80", "SS_Age_0-1", "SS_Age_2-4", "SS_Age_5-10", "SS_Age_11-20", "SS_Age_21-40", "SS_Age_41-60", "SS_Age_61-80", "SS_Age_81-100", "SS_Age_101-120", "SS_Age_121-140", "SS_Age_141-250", "SS_Age_251-399", "SS_Age_400+"]
# required_csv_fields = ["BGC_ZONE", "BGC_SUBZON", "BGC_VRT", "BGC_PHASE", "SITE_S", "SITEMC_S", "Forested (Y/N)", "SS_Climax", "Scomp_Climax", "SComp_Age_1-15", "SComp_Age_16-30", "SComp_Age_31-50", "SComp_Age_51-80", "SComp_Age_>80", "SS_Age_0-1", "SS_Age_2-4", "SS_Age_5-10", "SS_Age_11-20", "SS_Age_21-40", "SS_Age_41-60", "SS_Age_61-80", "SS_Age_81-100", "SS_Age_101-120", "SS_Age_121-140", "SS_Age_141-250", "SS_Age_251-399", "SS_Age_400+"]
# required_csv_fields = ["BGC_Label","BGC_ZONE","BGC_SUBZON","BGC_VRT","BGC_PHASE","BEU_MC","REALM","GROUP","CLASS","KIND","Forested (Y/N)","Strct_Climax","Stand_Climax","Stand_Age_1-15","Stand_Age_16-30","Stand_Age_31-50","Stand_Age_51-80","Stand_Age_>80","Struct_Age_0-1","Struct_Age_2-4","Struct_Age_5-10","Struct_Age_11-20","Struct_Age_21-40","Struct_Age_41-60","Struct_Age_61-80","Struct_Age_81-100","Struct_Age_101-120","Struct_Age_121-140","Struct_Age_141-250","Struct_Age_251-399","Struct_Age_400+","Snow_Code"]
required_csv_fields = ["BGC_Label","BGC_ZONE","BGC_SUBZON","BGC_VRT","BGC_PHASE","BEU_MC","REALM","GROUP","CLASS","KIND","Forested (Y/N)","Strct_Climax","Stand_Climax","Stand_Age_0-15","Stand_Age_16-30","Stand_Age_31-50","Stand_Age_51-80","Stand_Age_80+","Struct_Age_0-3","Struct_Age_4-10","Struct_Age_11-30","Struct_Age_31-40","Struct_Age_41-60","Struct_Age_61-80","Struct_Age_81-139","Struct_Age_140-249","Struct_Age_250+","Snow_Code"]
forested_dict = {}
stand_climax_dict = {}
sts_climax_dict = {}
sts_age_lookup_dict = {}
stand_age_lookup_dict = {}
sts_climax_dict = {}
# def_sts_dict = {}
# solar_s_dict = {}
# solar_w_dict = {}
line_counter = 0
sts_csv_obj = open(sts_csv, "rU")
csvReader = csv.DictReader(sts_csv_obj)
missing_fields = []
for data in csvReader:
    pass  ## just need to get one row of the csv file into "data", to test the existence of required fields - need to think of a clever way to do this without looping through the whole csv file
for req_field in required_csv_fields:
    if req_field not in data:
        missing_fields.append(req_field)
if len(missing_fields) > 0:
    print " *** Structural stage lookup table missing required field(s): " + str(missing_fields)
    sys.exit(0)
del csvReader
sts_csv_obj.close()

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

    # curr_eco_unit = curr_bgc_zone + curr_bgc_subzon + curr_bgc_vrt + curr_bgc_phase + "~" + curr_sites + "~" + curr_sitemc
    curr_eco_unit = curr_bgc_zone + curr_bgc_subzon + curr_bgc_vrt + curr_bgc_phase + "~" + curr_beu_mc
    # operable_dict[curr_eco_unit]       = curr_operable
    forested_dict[curr_eco_unit]       = curr_forested
    stand_climax_dict[curr_eco_unit]   = curr_stand_climax
    sts_climax_dict[curr_eco_unit]     = curr_sts_climax
    # def_sts_dict[curr_eco_unit]        = curr_def_sts


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
            
    if line_counter == 100 or line_counter % 1000 == 0:
        print "        - Read " + str(line_counter) + " lines"
print "        - Read " + str(line_counter) + " lines"

## ---------------------------------------------------------------------------------------------------------
## For each unique ecosystem, determine all possible combinations of STS and STAND codes that could apply
## to the ecosystem at any age value, and expand the previous dictionary of unique ecosytems to include
## all existing and "potential" unique ecosystems
## ---------------------------------------------------------------------------------------------------------

print "    - Expanding unique ecosystem list to include all possible structural stage and stand codes"
expanded_eco_dict = {}
unique_eco_count = 0
for unique_eco in unique_eco_dict:
    unique_eco_count += 1

    expanded_eco_dict[unique_eco] = unique_eco_dict[unique_eco]   ## always add the original ecosystem to the list of expanded ecosystems with its actual area value
    
    item_list = unique_eco.split('~')
    key = item_list[1] + item_list[2] + item_list[3] + item_list[4] + '~' + item_list[5]
    if key in sts_age_lookup_dict and key in stand_age_lookup_dict:  ## this if statement shouldn't be necessary if the STS lookup table was created and populated correctly
        unique_eco_root = item_list[0] + '~' + item_list[1] + '~' + item_list[2] + '~' + item_list[3] + '~' + item_list[4] + '~' + item_list[5] + '~' + item_list[6] + '~' + item_list[7] + '~' + item_list[8] + '~' + item_list[9] + '~' + item_list[10]
        #                    ECO_SEC              BGC_ZONE            BGC_SUBZON           BGC_VRT              BGC_PHASE           BEUMC_S1              SLOPE_MOD              SITE_M3A          SNOW_CODE           ABOVE_ELEV_THOLD     CROWN_MOOSE

        new_unique_eco = {}
        new_unique_eco[3] = unique_eco_root + '~' + sts_age_lookup_dict[key][2] + '~' + stand_age_lookup_dict[key][15]
        new_unique_eco[10] = unique_eco_root + '~' + sts_age_lookup_dict[key][7] + '~' + stand_age_lookup_dict[key][15]
        new_unique_eco[15] = unique_eco_root + '~' + sts_age_lookup_dict[key][20] + '~' + stand_age_lookup_dict[key][15]
        new_unique_eco[30] = unique_eco_root + '~' + sts_age_lookup_dict[key][20] + '~' + stand_age_lookup_dict[key][30]
        new_unique_eco[40] = unique_eco_root + '~' + sts_age_lookup_dict[key][35] + '~' + stand_age_lookup_dict[key][50]
        new_unique_eco[50] = unique_eco_root + '~' + sts_age_lookup_dict[key][50] + '~' + stand_age_lookup_dict[key][50]
        new_unique_eco[60] = unique_eco_root + '~' + sts_age_lookup_dict[key][50] + '~' + stand_age_lookup_dict[key][80]
        new_unique_eco[80] = unique_eco_root + '~' + sts_age_lookup_dict[key][70] + '~' + stand_age_lookup_dict[key][80]
        new_unique_eco[140] = unique_eco_root + '~' + sts_age_lookup_dict[key][125] + '~' + stand_age_lookup_dict[key][9999]
        new_unique_eco[249] = unique_eco_root + '~' + sts_age_lookup_dict[key][195] + '~' + stand_age_lookup_dict[key][9999]
        new_unique_eco[9999] = unique_eco_root + '~' + sts_age_lookup_dict[key][301] + '~' + stand_age_lookup_dict[key][9999]
        for age in [3,10,15,30,40,50,60,80,140,249,9999]:
            ## get rid of any projections to ecosystem units with STS=1/2/3 and STAND=B/C/M - remove the stand code (even though the STS LUT might say that an STS of 1/2/3 and a STAND code of B/C/M co-exist at certain ages)
            if new_unique_eco[age].split('~')[-2][:1] not in ['4','5','6','7'] and new_unique_eco[age].split('~')[-1] in ['B','C','M']:
                new_unique_eco[age] = new_unique_eco[age][:-1]
            if new_unique_eco[age] != unique_eco:
                expanded_eco_dict[new_unique_eco[age]] = 0     ## we may end up "adding" a particular new ecosystem/sts/stand combo to the dictionary multiple times, but it will always be 0 (area) so it doesn't matter

## ---------------------------------------------------------------------------------------------------------
## Write the .xlsx file (the table of unique ecosystems) to be pasted to a worksheet in the RRM spreadsheet 
## tool 
## ---------------------------------------------------------------------------------------------------------

print "    - Writing output file " + out_xlsx

workbook = xlsxwriter.Workbook(out_xlsx)
sheet1 = workbook.add_worksheet("Sheet1")

header_list = ["Eco_sec","Bgc_zone","Bgc_subzon","Bgc_vrt","Bgc_phase","Beumc","Slope_mod","Site_m3a","Snow_code","Above_Elev_Thold","Crown_Moose","Strct_d","Stand_d","Forested","Hectares"]
col = 0
for header in header_list:
    sheet1.write(0,col,header)
    col += 1
row = 0
for eco in sorted(set(expanded_eco_dict.keys())):
    ## now write the row to the worksheet
    item_list = eco.split('~')   ## again, item_list looks like [ecosection, zone, subzone, variant, phase, mapcode, vri cc, slope mod, solar code, snowpack, above 1200m, STS, stand]
    forested_dict_key = item_list[1] + item_list[2] + item_list[3] + item_list[4] + "~" + item_list[5]  ## key for forested_dict needs to look like: curr_bgc_zone + curr_bgc_subzon + curr_bgc_vrt + curr_bgc_phase + "~" + curr_beu_mc
    try:
        if forested_dict[forested_dict_key] not in ['',' ','None']:
            ## Only write an ecosystem unit to the xls file if has forested Y or N (i.e. if the ecosystem unit is in the STS LUT)
            ## That way, ecosystem units not in the STS LUT won't be listed in the RRM input or output tables and won't receive any ratings
            ## (rather than potentially being assigned a Zero rating by RRM, which Tania doesn't want)
            row += 1
            col = 0
            for item in item_list:
                sheet1.write(row,col,item)
                col += 1
            ## now write the Forested value in the next column (the column number being the same as the length of the list of items)
            sheet1.write(row,len(item_list),forested_dict[forested_dict_key])
            ## now write the hectare value in the last column (the column number being the same as the length of the list of items, plus one, because of Forested)
            sheet1.write(row,len(item_list)+1,round(expanded_eco_dict[eco] / 10000,1))
    except:
        pass

workbook.close()

## ---------------------------------------------------------------------------------------------------------
## Done
## ---------------------------------------------------------------------------------------------------------

dtCalcNow = time.time()
dtCalcScriptElapsed = dtCalcNow - dtCalcScriptStart
print " "
print "- Script complete after " + SanitizeElapsedTime(dtCalcScriptElapsed)
print " "
print " **** Output file " + out_xlsx + " can now be used as input in the Excel RRM tool."
