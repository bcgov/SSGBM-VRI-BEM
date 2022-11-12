#===================================================================================================================================================
#                                      Attaches the RRM Output to the BEM Feature Class (new and old suitability ratings)
#===================================================================================================================================================
#
# Author(s)         :  Written by CloverPoint (Jeff Kruys) 
#
# Date              :  2016-03-22
#                   :  2019-01-30
#
# Purpose           :  This script attaches the RRM output csv file to the spatial data and caluclates the 6 class WHR values for capability and suitablilty.
#                      It calculates the suitability ratings in the original method based on one dominant structural stage for each BEM polygon, but no longer
#                      the new method to account for the distribution of all VRI age classes and their associated habitat values for each BEM polygon.
#              
# Arguments         :  argv(1) = BEM feature class
#                   :  argv(2) = Lookup Table csv file
#                   :  argv(3) = RRM output csv file
#                   :  argv(4) = RRM field relation table csv file
#
# Example Input     :  X:\fullpath\BEM_V2 X:\fullpath\LookupTable.csv X:\fullpath\RRM_Output.csv X:\fullpath\RRM_Field_Relation_Table.csv
#
# Outputs           :  The input feature class has the CAP/SUIT ratings fields added based on the RRM output
#
# Dependencies      :  None
#              
# Expected Duration :  Depends on the size of the input BEM feature class. 
#
# History           :  Script created for BEM Ominceca Moose WHR (Madrone Dosier 15.0242)
#                   :  January 30, 2019 - Script updated to check for necessary fields for the RRM tool scripts (i.e. Above_1200m)
#                   :  September 9, 2020 (AE) - Script ran for VRIxBEM Northeast (Dosier 19.0461) 
#                   :  May 6, 2021 (JK) - Added code to cancel suitability ratings for exceptional cases; removed the VRI overlay step.
#
# Pro-Tips          :  1) Check that all fields specified in the field relation csv table are actually present in the input BEM feature class
#                   :  2) Check for spaces in the feature class fields ("" is okay " " is not). Look for spaces contained in the missing combinations list.
#                   :  3) Make sure that the BEM and the VRI feature classes perfectly overlap converted all <Null> values to "" in BGC_VRT and BGC_PHASE.
#                   :  4) If the "None" issue comes up, convert all <Null> values to "" in BGC_VRT and BGC_PHASE.
#
#==================================================================================================================================================

print "Initializing..."
import arcpy, sys, os, time, csv, pdb
dtCalcScriptStart = time.time()
print 'Start Time: ' + time.ctime(time.time())

if len(sys.argv) <> 5:
    print "- Four arguments required: PEM/TEM/BEM feature class, the Structural Stage Lookup Table csv file, the RRM output ratings table csv file, and RRM field relation table csv file."
    sys.exit()

python_script = sys.argv[0]
script_path = os.path.split(sys.argv[0])[0]
whr_master_path = os.path.split(script_path)[0]

## ---------------------------------------------------------------------------------------------------------
## Check that the expected arguments were provided
## ---------------------------------------------------------------------------------------------------------

working_fc = sys.argv[1]
if not arcpy.Exists(working_fc):
    print "- Specified PEM/TEM/BEM feature class does not exist."
    sys.exit()

sts_csv = sys.argv[2]
if not os.path.exists(sts_csv):
    print "- Specified Structural Stage lookup table CSV file does not exist."
    sys.exit()

ratings_csv = sys.argv[3]
if not os.path.exists(ratings_csv):
    print "- Specified RRM output ratings table csv file does not exist."
    sys.exit()

rrm_field_mapping_csv = sys.argv[4]
if not os.path.exists(rrm_field_mapping_csv):
    print "- Specified RRM field mapping csv file does not exist."
    sys.exit()

data_gdb = os.path.split(working_fc)[0]

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

print "    - Reading STS lookup table"
# required_csv_fields = ["FREQ", "BGC_ZONE", "BGC_SUBZON", "BGC_VRT", "BGC_PHASE", "SITE_S", "SITEMC_S", "Operable(Y/N)", "Default to original STS (Y/N)", "SS_Climax", "Scomp_Climax", "SComp_Age_1-15", "SComp_Age_16-30", "SComp_Age_31-50", "SComp_Age_51-80", "SComp_Age_>80", "SS_Age_0-1", "SS_Age_2-4", "SS_Age_5-10", "SS_Age_11-20", "SS_Age_21-40", "SS_Age_41-60", "SS_Age_61-80", "SS_Age_81-100", "SS_Age_101-120", "SS_Age_121-140", "SS_Age_141-250", "SS_Age_251-399", "SS_Age_400+"]
# required_csv_fields = ["BGC_ZONE", "BGC_SUBZON", "BGC_VRT", "BGC_PHASE", "SITE_S", "SITEMC_S", "Forested (Y/N)", "SS_Climax", "Scomp_Climax", "SComp_Age_1-15", "SComp_Age_16-30", "SComp_Age_31-50", "SComp_Age_51-80", "SComp_Age_>80", "SS_Age_0-1", "SS_Age_2-4", "SS_Age_5-10", "SS_Age_11-20", "SS_Age_21-40", "SS_Age_41-60", "SS_Age_61-80", "SS_Age_81-100", "SS_Age_101-120", "SS_Age_121-140", "SS_Age_141-250", "SS_Age_251-399", "SS_Age_400+"]
# required_csv_fields = ["BGC_Label","BGC_ZONE","BGC_SUBZON","BGC_VRT","BGC_PHASE","BEU_MC","REALM","GROUP","CLASS","KIND","Forested (Y/N)","ECOTYPE_CL","Strct_Climax","Stand_Climax","Stand_Age_1-15","Stand_Age_16-30","Stand_Age_31-50","Stand_Age_51-80","Stand_Age_>80","Struct_Age_0-1","Struct_Age_2-4","Struct_Age_5-10","Struct_Age_11-20","Struct_Age_21-40","Struct_Age_41-60","Struct_Age_61-80","Struct_Age_81-100","Struct_Age_101-120","Struct_Age_121-140","Struct_Age_141-250","Struct_Age_251-399","Struct_Age_400+","Snow_Code"]
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
## Read the rating table CSV file into a python dictionary
## The dictionary key will be the ecosystem unit, which is a concatenation of values in all fields before the CSV's HECTARES field
## Dictionary values will be python lists of all rating values
## eg. ratings_dict['BAU~ESSF~mv~1~~FF~3a~C~~'] = [3,3,1,1,5,5,5,5,1,1,1,1,2,2,1,1,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6]
## (the rating values will be in the same order as the field names in the header line)
## ---------------------------------------------------------------------------------------------------------

print "    - Reading ratings table file " + ratings_csv
line_counter = 0
for line in open(ratings_csv,'rU'):
    line_counter += 1
    if line_counter == 1:  ## header line of the CSV file
        curr_line = line[:-1]
        line_elements = curr_line.split(',')
        after_hectares = False
        eco_field_list = []
        csv_rating_field_list = []
        for line_element in line_elements:
            if line_element.upper() == "HECTARES":
                after_hectares = True
            elif after_hectares:
                if ":" not in line_element.upper():
                    csv_rating_field_list.append(line_element.replace('(','').replace(')',''))
                    #print "        - Rating field found: " + line_element
            else:
                if line_element.upper() != "FORESTED":    ## we won't include Forested because that is found in the STS LUT, not the PEM data. And there's a 1:1 relation between ecosystem unit and forested.
                    eco_field_list.append(line_element.replace('(','').replace(')',''))
                #print "        - Ecosystem field found: " + line_element
        if not after_hectares:
            print "*** CSV file does not contain a HECTARES attribute; it isn't a ratings table. Exiting..."
            sys.exit()

suit_ratings_dict = {}
cap_ratings_dict = {}
data_counter = 0
ratings_csv_object = open(ratings_csv, "rU")
csvReader = csv.DictReader(ratings_csv_object)
for data in csvReader:
    data_counter += 1

    cap_key = ''
    suit_key = ''
    for eco_field in eco_field_list:
        suit_key = suit_key + data[eco_field] + '~'
        if "STRCT_" not in eco_field.upper() and "STAND_" not in eco_field.upper():
            cap_key = cap_key + data[eco_field] + '~'
    suit_key = suit_key[:-1]   ## drop the last extra ~
    cap_key = cap_key[:-1]     ## drop the last extra ~
    suit_ratings_dict[suit_key] = {}
    try:
        x = cap_ratings_dict[cap_key]
    except:
        cap_ratings_dict[cap_key] = {}

    for rating_field in csv_rating_field_list:
        current_equation = rating_field.split('_')[len(rating_field.split('_')) - 2]
        if data[rating_field] == '':
            data[rating_field] = '9'  ## need blank to be considered a worse rating (higher number) than a real rating, so that a blank value doesn't become "best" of the three ratings
        # if data[rating_field] == 'H':
        #     data[rating_field] = '1'  ## convert H,M,L,N (4-class ratings) to 1-6 (6-class ratings)
        # if data[rating_field] == 'M':
        #     data[rating_field] = '3'
        # if data[rating_field] == 'L':
        #     data[rating_field] = '4'
        # if data[rating_field] == 'N':
        #     data[rating_field] = '6'
        suit_ratings_dict[suit_key][rating_field] = int(data[rating_field])
            
        ## for capability, we want the best rating among all of the ratings for this ecosystem unit among all different structural stages/stands, so
        ## keep the current rating if it's better (lower number) than current best (lowest) rating for this ecosystem unit
        if rating_field not in cap_ratings_dict[cap_key]:
            cap_ratings_dict[cap_key][rating_field] = int(data[rating_field])
        else:
            if int(data[rating_field]) < cap_ratings_dict[cap_key][rating_field]:
                cap_ratings_dict[cap_key][rating_field] = int(data[rating_field])
    if data_counter == 100 or data_counter % 10000 == 0:
        print "        - Read " + str(data_counter) + " lines"
print "        - Read " + str(data_counter) + " lines"
# pdb.set_trace()
del csvReader

## ---------------------------------------------------------------------------------------------------------
## Read the mapping of RRM field names (Bgc_zone, Slope_mod etc.) to PEM/TEM/BEM field names, from the
## RRM field mapping csv file
## ---------------------------------------------------------------------------------------------------------

pem_field_dict = {}
for decile in [1,2,3]:
    pem_field_dict[decile] = {}
data_counter = 0
rrm_field_mapping_csv_object = open(rrm_field_mapping_csv, "rU")
csvReader = csv.DictReader(rrm_field_mapping_csv_object)
rrm_pem_field_names = []
for data in csvReader:
    data_counter += 1
    for decile in [1,2,3]:
        pem_field_dict[decile][data["RRM_Input_Table_Field_Name"]] = data["PEM_Field_"+str(decile)]
        rrm_pem_field_names.append(data["PEM_Field_"+str(decile)])
del csvReader
TLT_field_name_list = [str(f.name) for f in arcpy.ListFields(working_fc)]
rrm_pem_field_names_missing = []
for rrm_pem_field_name in rrm_pem_field_names:
    if rrm_pem_field_name not in TLT_field_name_list:
        rrm_pem_field_names_missing.append(rrm_pem_field_name)

if len(rrm_pem_field_names_missing) > 0:
    print "*** Some field names found in specified RRM field mapping csv file are not found in specified PEM/TEM/BEM feature class: " + str(sorted(set(rrm_pem_field_names_missing))).replace('[','').replace(']','').replace("'","")
    print "*** Please correct the RRM field mapping csv file and re-run script."
    sys.exit()

## ---------------------------------------------------------------------------------------------------------
## From the field names given in the RRM output csv file, derive names of fields to be added to PEM/TEM, and add them
## ---------------------------------------------------------------------------------------------------------

print "    - Processing WHR PEM/TEM feature class " + working_fc
## find out which rating fields do not yet exist in the PEM, and add the ones that don't exist
# fc_cursor_field_list = ['PROJPOLYID','ECO_SEC','BGC_ZONE','BGC_SUBZON','BGC_VRT','BGC_PHASE','SLOPE_MOD','SDEC_1','SITEMC_S1','LU_STS1','LU_STAND1','VRI_CC1','SDEC_2','SITEMC_S2','LU_STS2','LU_STAND2','VRI_CC2','SDEC_3','SITEMC_S3','LU_STS3','LU_STAND3','VRI_CC3']
fc_cursor_field_list = []
for decile in [1,2,3]:
    for csv_field in pem_field_dict[decile].keys():
        fc_cursor_field_list.append(pem_field_dict[decile][csv_field])
fc_cursor_field_list = sorted(set(fc_cursor_field_list))
fc_cursor_field_list.append('TEIS_ID')
fc_cursor_field_list.append('SHAPE@AREA')
fc_cursor_field_list.append('BGC_ZONE')
fc_cursor_field_list.append('BGC_SUBZON')
fc_cursor_field_list.append('BGC_VRT')
fc_cursor_field_list.append('BGC_PHASE')
fc_cursor_field_list.append('SDEC_1')
fc_cursor_field_list.append('SDEC_2')
fc_cursor_field_list.append('SDEC_3')
fc_cursor_field_list.append('BEUMC_S1')
fc_cursor_field_list.append('BEUMC_S2')
fc_cursor_field_list.append('BEUMC_S3')
fc_cursor_field_list.append('FORESTED_1')
fc_cursor_field_list.append('FORESTED_2')
fc_cursor_field_list.append('FORESTED_3')
fc_cursor_field_list.append('VRI_AGE_CL_STS')
fc_fields = [f.name for f in arcpy.ListFields(working_fc)]
if 'ABOVE_ELEV_THOLD' in fc_fields:
    fc_cursor_field_list.append('ABOVE_ELEV_THOLD')
age_class_list = ["2","7","20","35","50","70","125","195","301","NA"]

for rating_field in csv_rating_field_list:
    ## example rating field from the csv file (RRM output): MURAR_SFD_6C (species_lifereq_classification)
    pem_ratings_field_list = [rating_field.split('_')[0] + '_' + rating_field.split('_')[1] + '_CAP_' + rating_field.split('_')[2] + '_1']
    pem_ratings_field_list.append(rating_field.split('_')[0] + '_' + rating_field.split('_')[1] + '_CAP_' + rating_field.split('_')[2] + '_2')
    pem_ratings_field_list.append(rating_field.split('_')[0] + '_' + rating_field.split('_')[1] + '_CAP_' + rating_field.split('_')[2] + '_3')
    pem_ratings_field_list.append(rating_field.split('_')[0] + '_' + rating_field.split('_')[1] + '_CAP_' + rating_field.split('_')[2] + '_HV')
    pem_ratings_field_list.append(rating_field.split('_')[0] + '_' + rating_field.split('_')[1] + '_CAP_' + rating_field.split('_')[2] + '_WA')
    pem_ratings_field_list.append(rating_field.split('_')[0] + '_' + rating_field.split('_')[1] + '_SU_' + rating_field.split('_')[2] + '_1')
    pem_ratings_field_list.append(rating_field.split('_')[0] + '_' + rating_field.split('_')[1] + '_SU_' + rating_field.split('_')[2] + '_2')
    pem_ratings_field_list.append(rating_field.split('_')[0] + '_' + rating_field.split('_')[1] + '_SU_' + rating_field.split('_')[2] + '_3')
    pem_ratings_field_list.append(rating_field.split('_')[0] + '_' + rating_field.split('_')[1] + '_SU_' + rating_field.split('_')[2] + '_WA')
    pem_ratings_field_list.append(rating_field.split('_')[0] + '_' + rating_field.split('_')[1] + '_SU_' + rating_field.split('_')[2] + '_HV')
    for pem_ratings_field in pem_ratings_field_list:
        if pem_ratings_field not in TLT_field_name_list:
            arcpy.AddMessage("        - Adding field " + pem_ratings_field + " to long table")
            if pem_ratings_field[-5:] == "_CALC":
                arcpy.AddField_management(working_fc,pem_ratings_field,'TEXT','#','#','400')
            else:
                arcpy.AddField_management(working_fc,pem_ratings_field,'SHORT')
        fc_cursor_field_list.append(pem_ratings_field)

cf_dict = {}
x = 0
for cf in fc_cursor_field_list:
    cf_dict[cf] = x
    x += 1
# print cf_dict.keys()
## ---------------------------------------------------------------------------------------------------------
## For each PEM/TEM polygon, calculate values for the ratings fields added above
## ---------------------------------------------------------------------------------------------------------

age_class_list = ["2","7","20","35","50","70","125","195","301","NA"]
stand_equiv_age_class_dict = {}
stand_equiv_age_class_dict["2"] = "15"
stand_equiv_age_class_dict["7"] = "15"
stand_equiv_age_class_dict["20"] = "30"
stand_equiv_age_class_dict["35"] = "50"
stand_equiv_age_class_dict["50"] = "50"
stand_equiv_age_class_dict["70"] = "80"
stand_equiv_age_class_dict["125"] = "9999"
stand_equiv_age_class_dict["195"] = "9999"
stand_equiv_age_class_dict["301"] = "9999"
stand_equiv_age_class_dict["NA"] = "NA"

row_counter = 0
with arcpy.da.UpdateCursor(working_fc,fc_cursor_field_list) as cursor:
    for row in cursor:
        row_counter += 1
        sdec_1 = str(row[cf_dict["SDEC_1"]]).replace('None','0')
        sdec_2 = str(row[cf_dict["SDEC_2"]]).replace('None','0')
        sdec_3 = str(row[cf_dict["SDEC_3"]]).replace('None','0')
        curr_bgc_zone    = str(row[cf_dict["BGC_ZONE"]]).replace(' ','')
        curr_bgc_subzon  = str(row[cf_dict["BGC_SUBZON"]]).replace(' ','')
        curr_bgc_vrt     = str(row[cf_dict["BGC_VRT"]]).replace('0','').replace('None','')
        curr_bgc_phase   = str(row[cf_dict["BGC_PHASE"]]).replace(' ','')
        curr_beumc_s1    = str(row[cf_dict["BEUMC_S1"]]).replace('None','')
        curr_beumc_s2    = str(row[cf_dict["BEUMC_S2"]]).replace('None','')
        curr_beumc_s3    = str(row[cf_dict["BEUMC_S3"]]).replace('None','')
        curr_forested_1  = str(row[cf_dict["FORESTED_1"]]).replace('None','')
        curr_forested_2  = str(row[cf_dict["FORESTED_2"]]).replace('None','')
        curr_forested_3  = str(row[cf_dict["FORESTED_3"]]).replace('None','')
        curr_strct_s1    = str(row[cf_dict["STRCT_S1"]]).replace('None','')
        curr_strct_s2    = str(row[cf_dict["STRCT_S2"]]).replace('None','')
        curr_strct_s3    = str(row[cf_dict["STRCT_S3"]]).replace('None','')
        curr_vri_age_cls = str(row[cf_dict["VRI_AGE_CL_STS"]]).replace('None','')
        if 'ABOVE_ELEV_THOLD' in fc_cursor_field_list:
            curr_above_elev = str(row[cf_dict["ABOVE_ELEV_THOLD"]]).replace('None','')
        ## construct the three ecosystem unit lookup strings for this PEM polygon, eg. 'BAU~ESSF~mv~1~~FF~3a~C~~'
        suit_key = {}
        cap_key = {}
        for decile in [1,2,3]:
            suit_key[decile] = ''
            cap_key[decile] = ''
            for rrm_eco_field in eco_field_list:
                suit_key[decile] += str(row[cf_dict[pem_field_dict[decile][rrm_eco_field]]]).replace('None','') + '~'
                ## CAPABILITY ratings disregard structural stage and stand composition values
                if "STRCT_" not in rrm_eco_field.upper() and "STAND_" not in rrm_eco_field.upper():
                    cap_key[decile] += str(row[cf_dict[pem_field_dict[decile][rrm_eco_field]]]).replace('None','') + '~'
            suit_key[decile] = suit_key[decile][:-1]
            cap_key[decile] = cap_key[decile][:-1]


        # ## SUITABILITY ratings are calculated taking the current structural stage into consideration
        # suit_key1 = str(row[cf_dict["ECO_SEC"]]).replace('None','') + "~" + str(row[cf_dict["BGC_ZONE"]]).replace('None','') + "~" + str(row[cf_dict["BGC_SUBZON"]]).replace('None','') + "~" + str(row[cf_dict["BGC_VRT"]]).replace('None','') + "~" + str(row[cf_dict["BGC_PHASE"]]).replace('None','') + "~" + str(row[cf_dict["SITEMC_S1"]]).replace('None','') + "~" + str(row[cf_dict["STRCT_S1"]]).replace('None','') + "~" + str(row[cf_dict["STAND_A1"]]).replace('None','') + "~" + str(row[cf_dict["MPB_CLASS1"]]).replace('None','') + "~" + str(row[cf_dict["SLOPE_MOD"]]).replace('None','')
        # suit_key2 = str(row[cf_dict["ECO_SEC"]]).replace('None','') + "~" + str(row[cf_dict["BGC_ZONE"]]).replace('None','') + "~" + str(row[cf_dict["BGC_SUBZON"]]).replace('None','') + "~" + str(row[cf_dict["BGC_VRT"]]).replace('None','') + "~" + str(row[cf_dict["BGC_PHASE"]]).replace('None','') + "~" + str(row[cf_dict["SITEMC_S2"]]).replace('None','') + "~" + str(row[cf_dict["STRCT_S2"]]).replace('None','') + "~" + str(row[cf_dict["STAND_A2"]]).replace('None','') + "~" + str(row[cf_dict["MPB_CLASS2"]]).replace('None','') + "~" + str(row[cf_dict["SLOPE_MOD"]]).replace('None','')
        # suit_key3 = str(row[cf_dict["ECO_SEC"]]).replace('None','') + "~" + str(row[cf_dict["BGC_ZONE"]]).replace('None','') + "~" + str(row[cf_dict["BGC_SUBZON"]]).replace('None','') + "~" + str(row[cf_dict["BGC_VRT"]]).replace('None','') + "~" + str(row[cf_dict["BGC_PHASE"]]).replace('None','') + "~" + str(row[cf_dict["SITEMC_S3"]]).replace('None','') + "~" + str(row[cf_dict["STRCT_S3"]]).replace('None','') + "~" + str(row[cf_dict["STAND_A3"]]).replace('None','') + "~" + str(row[cf_dict["MPB_CLASS3"]]).replace('None','') + "~" + str(row[cf_dict["SLOPE_MOD"]]).replace('None','')
        # ## CAPABILITY ratings disregard structural stage values
        # cap_key1 = str(row[cf_dict["ECO_SEC"]]).replace('None','') + "~" + str(row[cf_dict["BGC_ZONE"]]).replace('None','') + "~" + str(row[cf_dict["BGC_SUBZON"]]).replace('None','') + "~" + str(row[cf_dict["BGC_VRT"]]).replace('None','') + "~" + str(row[cf_dict["BGC_PHASE"]]).replace('None','') + "~" + str(row[cf_dict["SITEMC_S1"]]).replace('None','') + "~" + str(row[cf_dict["SLOPE_MOD"]]).replace('None','')
        # cap_key2 = str(row[cf_dict["ECO_SEC"]]).replace('None','') + "~" + str(row[cf_dict["BGC_ZONE"]]).replace('None','') + "~" + str(row[cf_dict["BGC_SUBZON"]]).replace('None','') + "~" + str(row[cf_dict["BGC_VRT"]]).replace('None','') + "~" + str(row[cf_dict["BGC_PHASE"]]).replace('None','') + "~" + str(row[cf_dict["SITEMC_S2"]]).replace('None','') + "~" + str(row[cf_dict["SLOPE_MOD"]]).replace('None','')
        # cap_key3 = str(row[cf_dict["ECO_SEC"]]).replace('None','') + "~" + str(row[cf_dict["BGC_ZONE"]]).replace('None','') + "~" + str(row[cf_dict["BGC_SUBZON"]]).replace('None','') + "~" + str(row[cf_dict["BGC_VRT"]]).replace('None','') + "~" + str(row[cf_dict["BGC_PHASE"]]).replace('None','') + "~" + str(row[cf_dict["SITEMC_S3"]]).replace('None','') + "~" + str(row[cf_dict["SLOPE_MOD"]]).replace('None','')

        for rating_field in csv_rating_field_list:
            cap_rating_field_1 = rating_field.split('_')[0] + '_' + rating_field.split('_')[1] + '_CAP_' + rating_field.split('_')[2] + '_1'
            cap_rating_field_2 = rating_field.split('_')[0] + '_' + rating_field.split('_')[1] + '_CAP_' + rating_field.split('_')[2] + '_2'
            cap_rating_field_3 = rating_field.split('_')[0] + '_' + rating_field.split('_')[1] + '_CAP_' + rating_field.split('_')[2] + '_3'
            cap_rating_field_hv = rating_field.split('_')[0] + '_' + rating_field.split('_')[1] + '_CAP_' + rating_field.split('_')[2] + '_HV'
            cap_rating_field_wa = rating_field.split('_')[0] + '_' + rating_field.split('_')[1] + '_CAP_' + rating_field.split('_')[2] + '_WA'
            suit_rating_field_1 = rating_field.split('_')[0] + '_' + rating_field.split('_')[1] + '_SU_' + rating_field.split('_')[2] + '_1'
            suit_rating_field_2 = rating_field.split('_')[0] + '_' + rating_field.split('_')[1] + '_SU_' + rating_field.split('_')[2] + '_2'
            suit_rating_field_3 = rating_field.split('_')[0] + '_' + rating_field.split('_')[1] + '_SU_' + rating_field.split('_')[2] + '_3'
            suit_rating_field_wa = rating_field.split('_')[0] + '_' + rating_field.split('_')[1] + '_SU_' + rating_field.split('_')[2] + '_WA'
            suit_rating_field_hv = rating_field.split('_')[0] + '_' + rating_field.split('_')[1] + '_SU_' + rating_field.split('_')[2] + '_HV'

            ## write the first decile capability ratings
            if cap_key[1] in cap_ratings_dict and sdec_1 in ['1','2','3','4','5','6','7','8','9','10']:
                if cap_ratings_dict[cap_key[1]][rating_field] in [1,2,3,4,5,6]:
                    row[cf_dict[cap_rating_field_1]] = cap_ratings_dict[cap_key[1]][rating_field]
                else:
                    row[cf_dict[cap_rating_field_1]] = None
            else:
                row[cf_dict[cap_rating_field_1]] = None

            ## write the second decile capability ratings
            if cap_key[2] in cap_ratings_dict and sdec_2 in ['1','2','3','4','5','6','7','8','9','10']:
                if cap_ratings_dict[cap_key[2]][rating_field] in [1,2,3,4,5,6]:
                    row[cf_dict[cap_rating_field_2]] = cap_ratings_dict[cap_key[2]][rating_field]
                else:
                    row[cf_dict[cap_rating_field_2]] = None
            else:
                row[cf_dict[cap_rating_field_2]] = None

            ## write the third decile capability ratings
            if cap_key[3] in cap_ratings_dict and sdec_3 in ['1','2','3','4','5','6','7','8','9','10']:
                if cap_ratings_dict[cap_key[3]][rating_field] in [1,2,3,4,5,6]:
                    row[cf_dict[cap_rating_field_3]] = cap_ratings_dict[cap_key[3]][rating_field]
                else:
                    row[cf_dict[cap_rating_field_3]] = None
            else:
                row[cf_dict[cap_rating_field_3]] = None

            ## calculate and write the HIGHEST VALUE (CAPABILITY) ratings (ie. best rating of the three deciles)
            best_rating = 8
            if cap_key[1] in cap_ratings_dict and sdec_1 in ['1','2','3','4','5','6','7','8','9','10']:
                best_rating = cap_ratings_dict[cap_key[1]][rating_field]
            if cap_key[2] in cap_ratings_dict and sdec_2 in ['1','2','3','4','5','6','7','8','9','10']:
               if cap_ratings_dict[cap_key[2]][rating_field] < best_rating:
                  best_rating = cap_ratings_dict[cap_key[2]][rating_field]
            if cap_key[3] in cap_ratings_dict and sdec_3 in ['1','2','3','4','5','6','7','8','9','10']:
              if cap_ratings_dict[cap_key[3]][rating_field] < best_rating:
                  best_rating = cap_ratings_dict[cap_key[3]][rating_field]
            if best_rating < 8:
              row[cf_dict[cap_rating_field_hv]] = best_rating
            else:
              row[cf_dict[cap_rating_field_hv]] = None

            ## calculate and write the WEIGHTED AVERAGE (CAPABILITY) ratings        
            dec_total = 0
            cap_weighted_rating = float(0)
            if cap_key[1] in cap_ratings_dict and sdec_1 in ['1','2','3','4','5','6','7','8','9','10']:
                dec_total += int(sdec_1)
            if cap_key[2] in cap_ratings_dict and sdec_2 in ['1','2','3','4','5','6','7','8','9','10']:
                dec_total += int(sdec_2)
            if cap_key[3] in cap_ratings_dict and sdec_3 in ['1','2','3','4','5','6','7','8','9','10']:
                dec_total += int(sdec_3)

            if cap_key[1] in cap_ratings_dict and sdec_1 in ['1','2','3','4','5','6','7','8','9','10']:
                cap_weighted_rating += float(cap_ratings_dict[cap_key[1]][rating_field] * float(sdec_1) / dec_total)
            if cap_key[2] in cap_ratings_dict and sdec_2 in ['1','2','3','4','5','6','7','8','9','10']:
                cap_weighted_rating += float(cap_ratings_dict[cap_key[2]][rating_field] * float(sdec_2) / dec_total)
            if cap_key[3] in cap_ratings_dict and sdec_3 in ['1','2','3','4','5','6','7','8','9','10']:
                cap_weighted_rating += float(cap_ratings_dict[cap_key[3]][rating_field] * float(sdec_3) / dec_total)
            
            cap_weighted_rating = int(round(cap_weighted_rating,0))
            if cap_weighted_rating in [1,2,3,4,5,6]:
                row[cf_dict[cap_rating_field_wa]] = cap_weighted_rating
            else:
                row[cf_dict[cap_rating_field_wa]] = None
                

            ## write the first decile suitability ratings
            if suit_key[1] in suit_ratings_dict and sdec_1 in ['1','2','3','4','5','6','7','8','9','10']:
                ## do NOT assign a rating if FORESTED_# = "Y" and STRCT_S# = "7a" and VRI_AGE_CLS_STS = -1
                if suit_ratings_dict[suit_key[1]][rating_field] in [1,2,3,4,5,6] and not (curr_forested_1 == "Y" and curr_strct_s1 == "7a" and curr_vri_age_cls == "-1") and not (curr_forested_1 == 'Y' and curr_strct_s1 == '' and curr_above_elev == 'N'):
                    row[cf_dict[suit_rating_field_1]] = suit_ratings_dict[suit_key[1]][rating_field]
                else:
                    row[cf_dict[suit_rating_field_1]] = None
            else:
                row[cf_dict[suit_rating_field_1]] = None

            ## write the second decile suitability ratings
            if suit_key[2] in suit_ratings_dict and sdec_2 in ['1','2','3','4','5','6','7','8','9','10']:
                ## do NOT assign a rating if FORESTED_# = "Y" and STRCT_S# = "7a" and VRI_AGE_CLS_STS = -1
                if suit_ratings_dict[suit_key[2]][rating_field] in [1,2,3,4,5,6] and not (curr_forested_2 == "Y" and curr_strct_s2 == "7a" and curr_vri_age_cls == "-1") and not (curr_forested_2 == 'Y' and curr_strct_s2 == '' and curr_above_elev == 'N'):
                    row[cf_dict[suit_rating_field_2]] = suit_ratings_dict[suit_key[2]][rating_field]
                else:
                    row[cf_dict[suit_rating_field_2]] = None
            else:
                row[cf_dict[suit_rating_field_2]] = None

            ## write the third decile suitability ratings
            if suit_key[3] in suit_ratings_dict and sdec_3 in ['1','2','3','4','5','6','7','8','9','10']:
                ## do NOT assign a rating if FORESTED_# = "Y" and STRCT_S# = "7a" and VRI_AGE_CLS_STS = -1
                if suit_ratings_dict[suit_key[3]][rating_field] in [1,2,3,4,5,6] and not (curr_forested_3 == "Y" and curr_strct_s3 == "7a" and curr_vri_age_cls == "-1") and not (curr_forested_3 == 'Y' and curr_strct_s3 == '' and curr_above_elev == 'N'):
                    row[cf_dict[suit_rating_field_3]] = suit_ratings_dict[suit_key[3]][rating_field]
                else:
                    row[cf_dict[suit_rating_field_3]] = None
            else:
                row[cf_dict[suit_rating_field_3]] = None

            ## calculate and write the WEIGHTED AVERAGE (SUITABILITY) ratings (the old way, just summarizing the simple decile ratings above)
            dec_total = 0
            suit_weighted_rating = float(0)
            if suit_key[1] in suit_ratings_dict and sdec_1 in ['1','2','3','4','5','6','7','8','9','10'] and row[cf_dict[suit_rating_field_1]] is not None:
                dec_total += int(sdec_1)
            if suit_key[2] in suit_ratings_dict and sdec_2 in ['1','2','3','4','5','6','7','8','9','10'] and row[cf_dict[suit_rating_field_2]] is not None:
                dec_total += int(sdec_2)
            if suit_key[3] in suit_ratings_dict and sdec_3 in ['1','2','3','4','5','6','7','8','9','10'] and row[cf_dict[suit_rating_field_3]] is not None:
                dec_total += int(sdec_3)

            if dec_total > 0:
                if suit_key[1] in suit_ratings_dict and sdec_1 in ['1','2','3','4','5','6','7','8','9','10']:
                    suit_weighted_rating += float(suit_ratings_dict[suit_key[1]][rating_field] * float(sdec_1) / dec_total)
                if suit_key[2] in suit_ratings_dict and sdec_2 in ['1','2','3','4','5','6','7','8','9','10']:
                    suit_weighted_rating += float(suit_ratings_dict[suit_key[2]][rating_field] * float(sdec_2) / dec_total)
                if suit_key[3] in suit_ratings_dict and sdec_3 in ['1','2','3','4','5','6','7','8','9','10']:
                    suit_weighted_rating += float(suit_ratings_dict[suit_key[3]][rating_field] * float(sdec_3) / dec_total)
            # else:
            #     print "*** TEIS_ID = " + str(row[cf_dict["TEIS_ID"]]) + " ecosystems not found in RRM output table; check the data and/or create a new RRM input table and re-run RRM."
            #     print "               ecosystems: " + suit_key[1] + ", " + suit_key[2] + ", " + suit_key[3]

            suit_weighted_rating = int(round(suit_weighted_rating,0))
            if suit_weighted_rating in [1,2,3,4,5,6]:
                row[cf_dict[suit_rating_field_wa]] = suit_weighted_rating
            else:
                row[cf_dict[suit_rating_field_wa]] = None


            ## calculate and write the HIGHEST VALUE (SUITABILITY) ratings (ie. best rating of the three deciles)
            best_rating = 8
            if suit_key[1] in suit_ratings_dict and sdec_1 in ['1','2','3','4','5','6','7','8','9','10'] and row[cf_dict[suit_rating_field_1]] is not None:
                best_rating = suit_ratings_dict[suit_key[1]][rating_field]
            if suit_key[2] in suit_ratings_dict and sdec_2 in ['1','2','3','4','5','6','7','8','9','10'] and row[cf_dict[suit_rating_field_2]] is not None:
                if suit_ratings_dict[suit_key[2]][rating_field] < best_rating:
                   best_rating = suit_ratings_dict[suit_key[2]][rating_field]
            if suit_key[3] in suit_ratings_dict and sdec_3 in ['1','2','3','4','5','6','7','8','9','10'] and row[cf_dict[suit_rating_field_3]] is not None:
                if suit_ratings_dict[suit_key[3]][rating_field] < best_rating:
                   best_rating = suit_ratings_dict[suit_key[3]][rating_field]
            if best_rating < 8:
                row[cf_dict[suit_rating_field_hv]] = best_rating
            else:
                row[cf_dict[suit_rating_field_hv]] = None


            # pdb.set_trace()
        cursor.updateRow(row)
        if row_counter == 100 or row_counter % 10000 == 0:
            print "        - Wrote " + str(row_counter) + " records"
print "        - Wrote " + str(row_counter) + " records"
print "READ ME: On successful completion of this Script 6: Verify that suitability ratings were not assigned where VRI_AGE_CL_STS = -1, FORESTED_# = Y and STRCT_S# = 7a."
if 'ABOVE_ELEV_THOLD' in fc_cursor_field_list:
    print "Also verify that suitability ratings were not assigned where FORESTED_# = Y, STRCT_S# = blank and ABOVE_ELEV_THOLD = N."

## ---------------------------------------------------------------------------------------------------------
## Done
## ---------------------------------------------------------------------------------------------------------

dtCalcNow = time.time()
dtCalcScriptElapsed = dtCalcNow - dtCalcScriptStart
print " "
print "- Script complete after " + SanitizeElapsedTime(dtCalcScriptElapsed)
