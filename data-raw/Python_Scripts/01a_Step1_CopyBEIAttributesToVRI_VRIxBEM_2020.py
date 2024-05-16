#===================================================================================================================================================
#                                      01_CopyBEIAttributesToVRI.py (Step 1 of VRIxBEM products)
#===================================================================================================================================================
#
# Author(s)         :  Written by Jeff Kruys (sub to Madrone)
#
# Creation Date     :  2017-10-01
# Last Revision     :  2018-12-07
#                   :  2020-01-19 - updated to recalculate the TEIS_ID field to be equal to the VRI_OBJ_ID field (which was created as the new unique
#                                   ID field after running the clip, explode and eliminate tools which would result in non-unique FEATURE_ID values in
#                                   the VRI). This is to avoid duplicate TEIS_ID values in the output VRI feature class, since we'll be using TEIS_ID
#                                   as a unique ID field in subsequent scripts.
#
# Purpose           :  This script copies all of the attributes in the specified BEI feature class to each polygon in the specified VRI feature class.
#                      The script runs the Intersect tool to determine which BEI polygon occupies the largest portion of each VRI polygon.
#
# Arguments         :  argv(1) = VRI feature class
#                   :  argv(2) = BEI feature class
#
#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!
#                   :  Adjust Line 69 in order to change the eliminate polygon size
#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!
#
# Example Input     :  X:\fullpath\03_CopyBEIAttributesToVRI.py Y:\fullpath\VRI Z:\fullpath\BEI
#
# Outputs           :  The output feature class will be a copy of the VRI polygons clipped to the BEI feature class, then
#                   :  exploded to singlepart, and its slivers will be eliminated. Then its attribute table will have all of the 
#                   :  BEI attribute fields added to the existing VRI attributes. The BEI attribute fields will be populated 
#                   :  by copying the BEI values of the majority area BEI polygon within each VRI polygon.
#
# Dependencies      :  None
#              
# Expected Duration :  ~ 5 hrs. if ran using command prompt
#
# History           :  2017-10-01: Script created for BEM Ominceca Moose WHR
#
#                   :  2017-11-06: Use VRI field FEATURE_ID as unique polygon ID field, rather than POLYGON_ID which contains non-unique values.
#                                  In the output feature class, overwrite values in TEIS_ID with the values in FEATURE_ID, as the values in
#                                  the TEIS_ID field will be used in subsequent scripts and must contain unique values.
#
#                   :  2018-12-01: Now includes an explode of multi part features & eliminates slivers generated (adjust minimum polygon size on line 69)
#
# Pro Tips          : Make sure that the BEM and VRI have the same extent (full overlap coverage)
#                   : Error may occur if both feature classes contain the Area_ha field (delete from BEM and re-add later)
#                   : Move final output in to a new geodatabase to continue the scripting process
#                   : ALWAYS RUN IN CMD

# Next Steps:       : Copy over the appropriate BGC fields and double check on screen against the current BEC version
#                   : Run 02_Script to create a unique list of BEC/Ecosystem combinations (02_Create_LookupTableTemplate_####.py)
#                   : Run 03_DEM Scripts to populate Slope, Aspect, and Elevation (03ab_Add_AspectandSlope_####.py & 03c_Add_ELEV_Attributes_####.py)
#
#==================================================================================================================================================

print "Initializing..."
import arcpy, sys, os, time, csv, pdb
dtCalcScriptStart = time.time()
print 'Start Time: ' + time.ctime(time.time())

if len(sys.argv) <> 3:
    print "- Two arguments required: VRI feature class, and BEI feature class. They must both already be clipped to the same AOI boundary."
    sys.exit()

python_script = sys.argv[0]
script_path = os.path.split(sys.argv[0])[0]
whr_master_path = os.path.split(script_path)[0]

## ---------------------------------------------------------------------------------------------------------
## Check that the expected arguments were provided
## ---------------------------------------------------------------------------------------------------------

vri_fc = sys.argv[1]
if not arcpy.Exists(vri_fc):
    print "- Specified VRI polygon feature class does not exist."
    sys.exit()

bei_fc = sys.argv[2]
if not arcpy.Exists(bei_fc):
    print "- Specified BEI feature class does not exist."
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

## don't proceed if the VRI feature class already contains a TEIS_ID field (the unique ID field of the BEI feature class)
## because then the output of the Intersect would attach the VRI and BEI tables together and name the real TEIS_ID field to TEIS_ID_1
## or TEIS_ID_2 if TEIS_ID_1 already exists, and blah blah... it just gets confusing.
vf = [f.name for f in arcpy.ListFields(vri_fc)]
if "TEIS_ID" in vf:
    print "**** VRI feature class already contains a TEIS_ID field"
    print "**** Please remove all BEI fields from the VRI feature class, then re-run this script."
    sys.exit()

## check that the TEIS_ID field in the BEM feature class contains unique values
print "    - Checking that TEIS_ID field in BEM feature class contains all unique values"
teis_id_set = set()
for row in arcpy.da.SearchCursor(bei_fc,["TEIS_ID"]):
    if row[0] not in teis_id_set:
        teis_id_set.add(row[0])
    else:
        print "**** Duplicate value " + str(row[0]) + " found in TEIS_ID field of BEI feature class. Re-populate"
        print "     the TEIS_ID field with unique values and run this script again."
        sys.exit()

vri_sliv_fc = vri_fc + "_" + time.strftime("%Y%m%d_%H%M%S")
print "    - Exploding clipped VRI feature class to singlepart, output named " + vri_sliv_fc
arcpy.MultipartToSinglepart_management(vri_fc,vri_sliv_fc)

#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!
expression = '"Shape_Area" < 1000'   ### all polygons smaller than this number of square meters will be eliminated
#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!

temp_layer = "temp_layer"
arcpy.MakeFeatureLayer_management(vri_sliv_fc, temp_layer)
arcpy.SelectLayerByAttribute_management(temp_layer, "NEW_SELECTION", expression)
vri_sliv_fc = vri_fc + "_" + time.strftime("%Y%m%d_%H%M%S")
print "    - Eliminating polygons < 1000sq.m. from singlepart VRI, output named " + vri_sliv_fc
arcpy.Eliminate_management(temp_layer, vri_sliv_fc, "AREA")

## ---------------------------------------------------------------------------------------------------------
## Add a new unique ID field to VRI and populate it with unique values
## (the previously established ID field may now be non-unique after the clip and eliminate processes)
## ---------------------------------------------------------------------------------------------------------

print "    - Adding field VRI_OBJ_ID to " + vri_sliv_fc
arcpy.AddField_management(vri_sliv_fc,"VRI_OBJ_ID","LONG")
print "    - Populating VRI_OBJ_ID field"
arcpy.CalculateField_management(vri_sliv_fc,"VRI_OBJ_ID","!OBJECTID!","PYTHON")

## ---------------------------------------------------------------------------------------------------------
## Run the Tabulate intersection and read the output table into a dictionary
## ---------------------------------------------------------------------------------------------------------

print "    - Overlaying clipped VRI and BEI feature classes using the Tabulate Intersection tool"
vri_x_bei_tbl = vri_fc + "_x_bei"
if arcpy.Exists(vri_x_bei_tbl):
    arcpy.Delete_management(vri_x_bei_tbl)
#arcpy.Intersect_analysis([vri_sliv_fc,bei_fc],vri_x_bei_tbl)
arcpy.TabulateIntersection_analysis(in_zone_features=vri_sliv_fc, zone_fields="VRI_OBJ_ID", in_class_features=bei_fc, 
                                    out_table=vri_x_bei_tbl, class_fields="TEIS_ID", sum_fields="SHAPE_Area")


print "    - Reading the Tabulate Intersection output table to find the largest BEI fragment for each VRI polygon"
vri_x_bei_dict = {}
vri_x_bei_id_list = []
cf = ["VRI_OBJ_ID","TEIS_ID","PERCENTAGE"]
for row in arcpy.da.SearchCursor(vri_x_bei_tbl,cf):
    ## current polygon fragment has both a VRI ID and a BEI ID.
    ## If the current VRI ID is one we've already encountered, then check if this BEI fragment is bigger than the previous
    ## biggest BEI fragment for this VRI ID, and if so, overwrite it, so this will be stored as the latest biggest BEI fragment.
    if row[1] != None:
        try:
            if row[2] > vri_x_bei_dict[row[0]][1]:
                vri_x_bei_dict[row[0]] = [row[1],row[2]]
        except:  ## this VRI ID wasn't in the dictionary yet. So the current BEI fragment is the biggest so far.
            vri_x_bei_dict[row[0]] = [row[1],row[2]]
        vri_x_bei_id_list.append(row[0])
vri_x_bei_id_list = sorted(set(vri_x_bei_id_list))

print "    - Reading VRI polygon IDs"
vri_id_list = []
for row in arcpy.da.SearchCursor(vri_sliv_fc,["VRI_OBJ_ID"]):
    vri_id_list.append(row[0])
vri_id_list = sorted(set(vri_id_list))

if vri_id_list != vri_x_bei_id_list:
    print "***** Some VRI polygon IDs were not found in the overlay dataset"
    print "***** Please use a BEI polygon feature class that completely overlaps the VRI polygon feature class."

## now we need to read in all of the BEI attribute values into a dictionary
## e.g. bei_dict[6947]["SDEC_1"] = 8
print "    - Reading the attributes of the BEI feature class"
bei_dict = {}
cf = [f.name for f in arcpy.ListFields(bei_fc) if not f.required]
for row in arcpy.da.SearchCursor(bei_fc,cf):
    bei_dict[row[cf.index("TEIS_ID")]] = {}
    for f in cf:
        if f != "TEIS_ID":
            bei_dict[row[cf.index("TEIS_ID")]][f] = row[cf.index(f)]

print "    - Adding BEI fields to VRI feature class attribute table"
vf = [f.name for f in arcpy.ListFields(vri_sliv_fc)]
for bf in arcpy.ListFields(bei_fc):
    if not bf.required:
        if bf.name not in vf:
            if bf.type == "Integer":
                bftype = "LONG"
            elif bf.type == "String":
                bftype = "TEXT"
            elif bf.type == "SmallInteger":
                bftype = "SHORT"
            else:
                bftype = bf.type.upper()
            if bftype == "TEXT":
                print "        - Adding field " + bf.name + ", type " + bftype + ", length " + str(bf.length)
            else:
                print "        - Adding field " + bf.name + ", type " + bftype
            arcpy.AddField_management(vri_sliv_fc,bf.name,bftype,bf.precision,bf.scale,bf.length,bf.aliasName,bf.isNullable,bf.required,bf.domain)

print "    - Populating BEI fields in VRI feature class attribute table"
cf = [f.name for f in arcpy.ListFields(vri_sliv_fc) if not f.required]
row_count = 0
row_update_count = 0
with arcpy.da.UpdateCursor(vri_sliv_fc,cf) as cursor:
    for row in cursor:
        row_count += 1
        row_updated = False
        try:
            ## vri_x_bei_dict[row[cf.index("VRI_OBJ_ID")]] gives us the BEI polygon ID (TEIS_ID) that was the biggest BEI fragment in the current VRI polygon
            ## (actually a list of two items, the TEIS_ID and area, so we'll stick a [0] so we just get the first item of that list, i.e. the TEIS_ID)
            ## then bei_dict[a TEIS_ID value] is a dictionary of field names and their attributes for this BEI TEIS_ID
            for bf in bei_dict[vri_x_bei_dict[row[cf.index("VRI_OBJ_ID")]][0]]:
                row[cf.index(bf)] = bei_dict[vri_x_bei_dict[row[cf.index("VRI_OBJ_ID")]][0]][bf]
            ## Previous code copies the TEIS_ID from the input BEI, but this results in non-unique values in the VRI. Subsequent scripts require TEIS_ID
            ## to have unique values. Instead, we will copy the VRI_OBJ_ID (VRI unique ID) values to TEIS_ID.
            ## row[cf.index("TEIS_ID")] = vri_x_bei_dict[row[cf.index("VRI_OBJ_ID")]][0]
            row[cf.index("TEIS_ID")] = str(row[cf.index("VRI_OBJ_ID")])
            row_updated = True
        except:
            print "VRI_OBJ_ID " + str(row[cf.index("VRI_OBJ_ID")]) + " not found in overlay dataset; VRI polygon isn't overlapped by any BEI polygon"
        if row_updated:
            cursor.updateRow(row)
            row_update_count += 1
        if row_count % 100000 == 0:
            print "        - Processed " + str(row_count) + " records (updated " + str(row_update_count) + " records)"
print "        - Processed " + str(row_count) + " records (updated " + str(row_update_count) + " records)"

# ---------------------------------------------------------------------------------------------------------
# Done. The Feature Class with the date and time is the final output. Polygon count should match the Input VRI
# ---------------------------------------------------------------------------------------------------------

dtCalcNow = time.time()
dtCalcScriptElapsed = dtCalcNow - dtCalcScriptStart
print " "
print "- Script complete after " + SanitizeElapsedTime(dtCalcScriptElapsed)
print "- New VRI feature class with added BEI attributes is named " + vri_sliv_fc
