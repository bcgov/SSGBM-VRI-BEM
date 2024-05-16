#======================================================================================================================================================
#                                     Add Aspect and Slope to TEIS product (**requires Spatial Analyst Extension**)
#======================================================================================================================================================
#
# Author(s)         :  Written by CloverPoint (Jeff Kruys)
#
# Date              :  2016-02-17
#                   :  2018-12-18
#
# Purpose           :  This script overlays the user-specified polygon feature class with slope and aspect rasters to
#                      add mean aspect, mean slope and slope modifers to the polygon attribute table.
#              
# Arguments         :  argv(1) = BEM feature class
#                   :  argv(2) = Aspect Raster                   
#                   :  argv(3) = Slope Raster
#
# Example Input     :  X:\fullpath\BEM_V2 X:\fullpath\aspect_raster X:\fullpath\slope_raster  
#
# Outputs           :  MEAN_ASP, MEAN_SLOPE, SLOPE_MOD attributes added to user-specified BEM feature class 
#
# Dependencies      :  The TEIS_ID field in the input BEM feature class needs to be populated with unique values.
#              
# Expected Duration :  ~ 1.0 hr if running in command prompt
#
# History           :  2016-02-17     : Script created for previous WHR projects. Adpated to run on BEM fields for BEM Ominceca Moose WHR (Madrone Dosier 15.0242)
#                   :  2018-12-19     : Script updated to use the zonal statistics tool. This cuts down on processing time significantly 
#                   :  2021-01-16     : Added code to assign a "j" to SLOPE_MOD if the MEAN_SLOPE is between 10 and 25 (or 10 and 35 for CWH & MH zones)
#
# Pro Tips          : Runs best in command prompt
#                   : Use ArcPy 64bit version
#
# Next Steps:       : Run the elevation script and QA on screen for completion
#
#======================================================================================================================================================


print "Initializing..."

#import system modules
import arcpy, sys, os, time, math, operator, csv, pdb
from bisect import bisect_left
arcpy.CheckOutExtension("spatial")
dtCalcScriptStart = time.time()
print 'Start Time: ' + time.ctime(time.time())

if len(sys.argv) <> 4:
    print "- Three arguments required: the BEM feature class, the Aspect raster and the Slope raster."
    sys.exit()

#set environment settings and check that the expected arguments were provided
working_fc = sys.argv[1]
aspect_raster = sys.argv[2]
slope_raster = sys.argv[3]

working_gdb = os.path.split(working_fc)[0]
whr_master_path = os.path.split(working_gdb)[0]
scratch_gdb = whr_master_path + r'\Scratch\Scratch.gdb'

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
## Define routine to determine the closest value in a list to a specified value. Used for finding the 
## nearest raster cell centroid to a polygon centroid when no raster cell centroids lie in the polygon.
## ---------------------------------------------------------------------------------------------------------

def takeClosest(myList, myNumber):
    #Assumes myList is sorted. Returns closest value to myNumber.
    #If two numbers are equally close, return the smallest number.
    #code taken from http://stackoverflow.com/questions/12141150/from-list-of-integers-get-number-closest-to-some-value
    pos = bisect_left(myList, myNumber)
    if pos == 0:
        return myList[0]
    if pos == len(myList):
        return myList[-1]
    before = myList[pos - 1]
    after = myList[pos]
    if after - myNumber < myNumber - before:
       return after
    else:
       return before

## ---------------------------------------------------------------------------------------------------------
## Add mean aspect, mean slope and slope  modifiers:
##   - clip the aspect/slope rasters to the study area
##   - convert clipped rasters to points
##   - overlay the points with the polygons so each point is assigned the ID of the polygon it lies in
##   - read the overlay datasets (xy of point, aspect, slope, polygon ID) into a dictionary
##   - for each polygon, calculate average aspect and average slope of all the points within the polygon (or take the
##        aspect or slope of the point nearest to the polygon's centroid)
##   - add fields MEAN_ASP, MEAN_SLOPE and SLOPE_MOD to BEM, and write aspect / slope values and modifier codes
## ---------------------------------------------------------------------------------------------------------

# Create index on TEIS_ID field if it doesn't exist
attr_index_list = arcpy.ListIndexes(working_fc)
teis_id_field_is_indexed = False
for attr_index in attr_index_list:
    for idx_field in attr_index.fields:
        if idx_field.name == "TEIS_ID":
            teis_id_field_is_indexed = True
if teis_id_field_is_indexed:
    print "    - Attribute index already exists on TEIS_ID field in " + working_fc
else:
    print "    - Adding attribute index for TEIS_ID field in " + working_fc
    arcpy.AddIndex_management(working_fc, "TEIS_ID", "TEIS_ID_idx", "NON_UNIQUE", "NON_ASCENDING")

# Add fields if they don't exist
whr_fields = [f.name for f in arcpy.ListFields(working_fc)]
if "MEAN_ASP" not in whr_fields:
    print "    - Adding MEAN_ASP field to " + working_fc
    arcpy.AddField_management(working_fc,"MEAN_ASP","SHORT")
if "MEAN_SLOPE" not in whr_fields:
    print "    - Adding MEAN_SLOPE field to " + working_fc
    arcpy.AddField_management(working_fc,"MEAN_SLOPE","SHORT")
if "SLOPE_MOD" not in whr_fields:
    print "    - Adding SLOPE_MOD field to " + working_fc
    arcpy.AddField_management(working_fc,"SLOPE_MOD","TEXT","#","#","1")

# New faster way to calculate slope values, using the zonal statistics table tool which calculates the simple
# mean of raster cell values within each polygon.

# Can't use this for aspect, because we need the "circular average", so we need to extract all the individual raster
# cell values within each polygon to calculate that. 

print "    - Calculating MEAN_SLOPE values using zonal statistics"
zonal_stats_tbl = scratch_gdb + r"\zonal_stats_tbl"

print "        - Creating zonal statistics table " + zonal_stats_tbl
if arcpy.Exists(zonal_stats_tbl):
    arcpy.Delete_management(zonal_stats_tbl)
arcpy.gp.ZonalStatisticsAsTable_sa(working_fc, "TEIS_ID", slope_raster, zonal_stats_tbl, "DATA", "MEAN")

print "        - Reading zonal statistics table to dictionary"
row_count = 0
row_total = int(arcpy.GetCount_management(zonal_stats_tbl).getOutput(0))
slope_dict = {}
for row in arcpy.da.SearchCursor(zonal_stats_tbl,["TEIS_ID","MEAN"]):
    row_count += 1
    slope_dict[row[0]] = row[1]
    if row_count % 100000 == 0:
        print "            - Processed " + str(row_count) + " of " + str(row_total) + " rows"
print "            - Processed " + str(row_count) + " of " + str(row_total) + " rows"

row_count = 0
slope_null_count = 0
row_total = int(arcpy.GetCount_management(working_fc).getOutput(0))
print "        - Writing slope values to MEAN_SLOPE field in " + working_fc
with arcpy.da.UpdateCursor(working_fc,["TEIS_ID","MEAN_SLOPE","SHAPE@XY"]) as cursor:
    for row in cursor:
        row_count += 1
        try:
            row[1] = int(round(slope_dict[row[0]],0))
            cursor.updateRow(row)
        except:
            try:
                # if the current polygon had no entry in the dictionary, it means there were no raster cell centroids in it
                # so we will try to pull the raster value at the xy coordinates of the polygon's centroid.
                xy = str(row[2][0]) + " " + str(row[2][1])
                row[1] = int(arcpy.GetCellValue_management(slope_raster,xy).getOutput(0))
                cursor.updateRow(row)
            except:
                # the polygon lies outside of the raster extents.
                row[1] = None
                cursor.updateRow(row)
                slope_null_count += 1
        if row_count % 100000 == 0:
            print "            - Processed " + str(row_count) + " of " + str(row_total) + " rows"
    print "            - Processed " + str(row_count) + " of " + str(row_total) + " rows"
    if slope_null_count == 0:
        print "    **** There were " + str(slope_null_count) + " polygon(s) for which a MEAN_SLOPE value could not be calculated. That's good!"
    else:
        print "    **** WARNING: There were " + str(slope_null_count) + " polygon(s) for which a MEAN_SLOPE value could not be calculated. These polygons probably lie outside of the extents of the slope raster."
arcpy.Delete_management(zonal_stats_tbl)
del slope_dict

print "    - Calculating MEAN_ASP values in " + working_fc

tabulate_area_tbl = scratch_gdb + r'\tabulate_area_tbl'
if arcpy.Exists(tabulate_area_tbl):
    arcpy.Delete_management(tabulate_area_tbl)
print "        - Creating Tabulate Area table " + tabulate_area_tbl
arcpy.gp.TabulateArea_sa(working_fc, "TEIS_ID", aspect_raster, "Value", tabulate_area_tbl)

cellsizex = int(arcpy.GetRasterProperties_management(aspect_raster, "CELLSIZEX").getOutput(0))
cellsizey = int(arcpy.GetRasterProperties_management(aspect_raster, "CELLSIZEY").getOutput(0))
raster_cell_area = cellsizex * cellsizey
print "        - Aspect raster cell size " + str(cellsizex) + "m x " + str(cellsizey) + "m"
print "        - Reading Tabulate Area table and calculating mean aspects for each polygon"
tabulate_area_fields = [f.name for f in arcpy.ListFields(tabulate_area_tbl)]
cursor_fields = ["TEIS_ID"]
for aspect_value in range(0,360):
    if "VALUE_" + str(aspect_value) in tabulate_area_fields:
        cursor_fields.append("VALUE_" + str(aspect_value))
aspect_dict = {}
row_count = 0
row_total = int(arcpy.GetCount_management(tabulate_area_tbl).getOutput(0))
for row in arcpy.da.SearchCursor(tabulate_area_tbl, cursor_fields):
    row_count += 1
    curr_teis_id = row[cursor_fields.index("TEIS_ID")]
    sin_sum = 0
    cos_sum = 0
    avg_aspect = 0
    # calculate the "circular average" from the values in the Tabulate Area table for each row (table contains one row for each TEIS_ID)
    for aspect_value_field in [f for f in cursor_fields if f != "TEIS_ID"]:
        if row[cursor_fields.index(aspect_value_field)] > 0:
            aspect_value = int(aspect_value_field.split("_")[1])
            points_with_curr_aspect = int(row[cursor_fields.index("VALUE_" + str(aspect_value))] / raster_cell_area)
            for point in range(1,points_with_curr_aspect+1):
                sin_sum += math.sin(float(aspect_value) * math.pi / 180)
                cos_sum += math.cos(float(aspect_value) * math.pi / 180)
    if sin_sum > 0 and cos_sum > 0:
        avg_aspect = math.atan(sin_sum / cos_sum) * 180 / math.pi
    elif cos_sum < 0:
        avg_aspect = (math.atan(sin_sum / cos_sum) * 180 / math.pi) + 180
    elif sin_sum < 0 and cos_sum > 0:
        avg_aspect = (math.atan(sin_sum / cos_sum) * 180 / math.pi) + 360
    aspect_dict[curr_teis_id] = int(avg_aspect)
    if row_count % 100000 == 0:
        print "            - Processed " + str(row_count) + " of " + str(row_total) + " rows"
print "            - Processed " + str(row_count) + " of " + str(row_total) + " rows"

print "        - Writing values to MEAN_ASP field in " + working_fc
row_count = 0
aspect_null_count = 0
row_total = int(arcpy.GetCount_management(working_fc).getOutput(0))
with arcpy.da.UpdateCursor(working_fc,["TEIS_ID","MEAN_ASP","SHAPE@XY"]) as cursor:
    for row in cursor:
        row_count += 1
        try:
            ## calculate average value of the raster points that lie within the polygon; for aspect, this is the "circular average"
            row[1] = aspect_dict[row[0]]
            cursor.updateRow(row)
        except:
            try:
                # if the current polygon had no entry in the dictionary, it means there were no raster cell centroids in it
                # so we will try to pull the raster value at the xy coordinates of the polygon's centroid.
                xy = str(row[2][0]) + " " + str(row[2][1])
                row[1] = int(arcpy.GetCellValue_management(aspect_raster,xy).getOutput(0))
                cursor.updateRow(row)
            except:
                # the polygon lies outside of the raster extents.
                row[1] = None
                cursor.updateRow(row)
                aspect_null_count += 1
        if row_count % 100000 == 0:
            print "            - Processed " + str(row_count) + " of " + str(row_total) + " rows"
    print "            - Processed " + str(row_count) + " of " + str(row_total) + " rows"
    if aspect_null_count == 0:
        print "    **** There were " + str(aspect_null_count) + " polygon(s) for which a MEAN_ASP value could not be calculated. That's good!"
    else:
        print "    **** WARNING: There were " + str(aspect_null_count) + " polygon(s) for which a MEAN_ASP value could not be calculated. These polygons probably lie outside of the extents of the aspect raster."

del aspect_dict
arcpy.Delete_management(tabulate_area_tbl)

print "    - Populating the SLOPE_MOD field"

## "k" (cool aspect) if aspect is 285-359 or 0-134 AND slope is 25%-100% (except in CWH and MH zones, it's 35%-100%)
## "q" (very steep cool aspect) if aspect is 285-359 or 0-134 AND slope > 100%
## "w" (warm aspect) if aspect is 135-284 AND slope is 25%-100% (except in CWH and MH zones, it's 35%-100%)
## "z" (very steep warm aspect) if aspect is 135-284 AND slope > 100%

mapcodes_no_slope_mods = ["LL","LS","LA","La","OW","Pd","PD","RE","RI","Ri","Wa","WE","Wm","Ww","Ws","WL"]
row_count = 0
row_total = int(arcpy.GetCount_management(working_fc).getOutput(0))
with arcpy.da.UpdateCursor(working_fc,["TEIS_ID","BGC_ZONE","MEAN_SLOPE","MEAN_ASP","SLOPE_MOD","BEUMC_S1","BEUMC_S2","BEUMC_S3"]) as cursor:
    for row in cursor:
        row_count += 1
        if row[2] != None and row[3] != None and str(row[5])[:2] not in mapcodes_no_slope_mods and str(row[6])[:2] not in mapcodes_no_slope_mods and str(row[7])[:2] not in mapcodes_no_slope_mods:
            if str(row[1]) in ["CWH","MH"]:
                if (285 <= row[3] <= 360 or 0 <= row[3] <= 134):
                    if 10 <= row[2] < 35:
                        row[4] = 'j'
                    elif 35 <= row[2] <= 100:
                        row[4] = 'k'
                    elif row[2] > 100:
                        row[4] = 'q'
                    else:
                        row[4] = ''
                elif 135 <= row[3] <= 284:
                    if 10 <= row[2] < 35:
                        row[4] = 'j'
                    elif 35 <= row[2] <= 100:
                        row[4] = 'w'
                    elif row[2] > 100:
                        row[4] = 'z'
                    else:
                        row[4] = ''
                else:
                    row[4] = ''
            else:
                if (285 <= row[3] <= 360 or 0 <= row[3] <= 134):
                    if 10 <= row[2] < 25:
                        row[4] = 'j'
                    elif 25 <= row[2] <= 100:
                        row[4] = 'k'
                    elif row[2] > 100:
                        row[4] = 'q'
                    else:
                        row[4] = ''
                elif 135 <= row[3] <= 284:
                    if 10 <= row[2] < 25:
                        row[4] = 'j'
                    elif 25 <= row[2] <= 100:
                        row[4] = 'w'
                    elif row[2] > 100:
                        row[4] = 'z'
                    else:
                        row[4] = ''
                else:
                    row[4] = ''
        else:
            row[4] = ''
        cursor.updateRow(row)
        if row_count % 100000 == 0:
            print "        - Processed " + str(row_count) + " of " + str(row_total) + " rows"
    print "        - Processed " + str(row_count) + " of " + str(row_total) + " rows"

## ---------------------------------------------------------------------------------------------------------
## Done
## ---------------------------------------------------------------------------------------------------------

dtCalcNow = time.time()
dtCalcScriptElapsed = dtCalcNow - dtCalcScriptStart
print " "
print "- Script complete after " + SanitizeElapsedTime(dtCalcScriptElapsed)
