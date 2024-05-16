#==================================================================================================================
#                                      Add_ELEV_Attributes.py (**requires Spatial Analyst Extension**)
#==================================================================================================================
#
# Author(s)         :  Written by CloverPoint (Jeff Kruys)
#
# Date              :  2016-02-23
#                   :  2018-12-19
#
# Purpose           :  This script overlays the user-specified polygon feature class and DEM layer to
#                      add elevation attributes to the polygon attribute table.
#              
# Arguments         :  argv(1) = BEM feature class
#                   :  argv(2) = DEM raster
#                   :  argv(3) = elevation threshold value in meters (i.e. 1400)
#
# Example Input     :  X:\fullpath\BEM_V2 X:\fullpath\DEM
#
# Outputs           :  Elevation attribute added to user-specified BEM feature class
#
# Dependencies      :  The TEIS_ID field in the input BEM feature class needs to be populated with unique values.
#              
# Expected Duration :  Depends on the size of the input BEM feature class and teh size of DEM raster (100m vrs 25m).
#
# History           :  2012-03-27 (JK): Script created; 2016-02-23 (SW): Script modified
#                   :  2018-12-19     : Script updated to use the zonal statistics tool. This cuts down on processing time significantly
#
#==================================================================================================================

print "Initializing..."

#import system modules
import arcpy, sys, os, time, math, operator, csv, pdb
from bisect import bisect_left
arcpy.CheckOutExtension("spatial")
dtCalcScriptStart = time.time()
print 'Start Time: ' + time.ctime(time.time())

if len(sys.argv) <> 4:
    print "- Three arguments required: the BEM feature class, the Elevation (DEM) raster, and an elevation threshold value in meters."
    sys.exit()

#set environment settings and check that the expected arguments were provided
working_fc = sys.argv[1]
elev_raster = sys.argv[2]
try:
    elev_thold = int(sys.argv[3])
except:
    arcpy.AddMessage("*** Elevation threshold value should be a numeric value in meters. Exiting.")
    sys.exit()

working_gdb = os.path.split(working_fc)[0]
whr_master_path = os.path.split(working_gdb)[0]
scratch_gdb = whr_master_path + r'\Scratch\Scratch.gdb'

if not arcpy.Exists(working_fc):
    arcpy.AddMessage("*** Specified BEM feature class doesn't exist. Exiting.")
    sys.exit()

working_fc_fields = [str(f.name) for f in arcpy.ListFields(working_fc)]
if "TEIS_ID" not in working_fc_fields:
    arcpy.AddMessage("*** Specified BEM feature class doesn't contain a TEIS_ID field. Exiting.")
    sys.exit()

if not arcpy.Exists(elev_raster):
    arcpy.AddMessage("*** Specified DEM raster doesn't exist. Exiting.")
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
## Add ELEV:
##   - create zonal statistics table from overlay of polygons and DEM
##   - read the table (polygon ID, mean elevation) into a dictionary
##   - add field ELEV to polygon attribute table and write elevation values 
## ---------------------------------------------------------------------------------------------------------

print "    - Calculating ELEV values using zonal statistics"
zonal_stats_tbl = scratch_gdb + r"\zonal_stats_tbl"

print "        - Creating zonal statistics table " + zonal_stats_tbl
if arcpy.Exists(zonal_stats_tbl):
    arcpy.Delete_management(zonal_stats_tbl)
arcpy.gp.ZonalStatisticsAsTable_sa(working_fc, "TEIS_ID", elev_raster, zonal_stats_tbl, "DATA", "MEAN")

print "        - Reading zonal statistics table to dictionary"
row_count = 0
row_total = int(arcpy.GetCount_management(zonal_stats_tbl).getOutput(0))
elev_dict = {}
for row in arcpy.da.SearchCursor(zonal_stats_tbl,["TEIS_ID","MEAN"]):
    row_count += 1
    elev_dict[row[0]] = row[1]
    if row_count % 100000 == 0:
        print "            - Processed " + str(row_count) + " of " + str(row_total) + " rows"
print "            - Processed " + str(row_count) + " of " + str(row_total) + " rows"

whr_fields = [f.name for f in arcpy.ListFields(working_fc)]
if "ELEV" not in whr_fields:
    print "        - Adding ELEV field to " + working_fc
    arcpy.AddField_management(working_fc,"ELEV","LONG")
if "ABOVE_ELEV_THOLD" not in whr_fields:
    print "        - Adding ABOVE_ELEV_THOLD field to " + working_fc
    arcpy.AddField_management(working_fc,"ABOVE_ELEV_THOLD","TEXT","#","#","1")
    
row_count = 0
elev_null_count = 0
row_total = int(arcpy.GetCount_management(working_fc).getOutput(0))
print "        - Writing mean elevation values to ELEV field in " + working_fc
with arcpy.da.UpdateCursor(working_fc,["TEIS_ID","ELEV","SHAPE@XY","ABOVE_ELEV_THOLD"]) as cursor:
    for row in cursor:
        row_count += 1
        try:
            row[1] = int(round(elev_dict[row[0]],0))
            if row[1] > elev_thold:
                row[3] = "Y"
            else:
                row[3] = "N"
            cursor.updateRow(row)
        except:
            try:
                # if the current polygon had no entry in the dictionary, it means there were no raster cell centroids in it
                # so we will try to pull the raster value at the xy coordinates of the polygon's centroid.
                xy = str(row[2][0]) + " " + str(row[2][1])
                row[1] = int(arcpy.GetCellValue_management(elev_raster,xy).getOutput(0))
                if row[1] > elev_thold:
                    row[3] = "Y"
                else:
                    row[3] = "N"
                cursor.updateRow(row)
            except:
                # the polygon lies outside of the raster extents.
                row[1] = None
                row[3] = ""
                cursor.updateRow(row)
                elev_null_count += 1
        if row_count % 100000 == 0:
            print "            - Processed " + str(row_count) + " of " + str(row_total) + " rows"
    print "            - Processed " + str(row_count) + " of " + str(row_total) + " rows"
    if elev_null_count == 0:
        print "    **** There were " + str(elev_null_count) + " polygon(s) for which a ELEV value could not be calculated. That's good!"
    else:
        print "    **** WARNING: There were " + str(elev_null_count) + " polygon(s) for which a ELEV value could not be calculated. These polygons probably lie outside of the extents of the elevation raster."
arcpy.Delete_management(zonal_stats_tbl)
del elev_dict

dtCalcNow = time.time()
dtCalcScriptElapsed = dtCalcNow - dtCalcScriptStart
print " "
print "- Script complete after " + SanitizeElapsedTime(dtCalcScriptElapsed)
