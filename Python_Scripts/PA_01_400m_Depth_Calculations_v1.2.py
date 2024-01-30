"""

Original Author:
Madrone (Jeff Kruys)

Created on:
2020-01-30

Purpose:
This script creates the 400m Depth (Effective Winter Habitat) raster layer, using the shelter
polygons and buffered linear feature polygons feature classes as input.

Usage:
400m_Depth_Calculations.py spo lfb [-h] [-l] [-ld]

Positional Arguments:
   spo              Shelter polygon feature class (VECTOR) with HEM fields - W_Shleter_1 is required
   lfb              Linear distrubance feature class (VECTOR) 

Optional Arguments:
  -h, --help       show this help message and exit
  -l, --level      log level messages to display; Default: 30 - WARN
  -ld, --log_dir   path to directory for output log file

Output Location and next steps:
Output 400m_

Example Input in CMD:

C:\Python27\ArcGISx6410.7\python.exe
E:\Projects_OLD\18.0512_VRIxBEM\Scripts\PA_01_400m_Depth_Calculations.py
Y:\19.0461_Peace_VRI_BEM\VRIxBEM_Peace_Step5.gdb\Northeast_VRIxBEM_working
E:\Projects_OLD\18.0512_VRIxBEM\19.0461_Northeast_Disturbances\All_Disturbances.gdb\ALL_DISTURBANCE

History
2020-01-30 (JK): Created script 400m_Depth_Calculations.
2020-05-04 (JK): Added Dissolve steps 1b and 1e
2020-06-23 (JK): Replaced the Step 1c Union step with an Erase; add steps to create an AOI zero raster and mosaic it
                 with the Step 2e raster to fill in gaps caused by Erased polygons from Step 1c
2020-06-24 (JK): Added code to process the Step 1c Erase in chunks of 100,000 polygons at a time
2020-06-26 (JK): JK notes that for ease of script converting the distrubance to a raster (16 hrs.) and then
                 back to polygon increases the efficency of this script a great deal (<2 hrs. vs. 24 hrs.)  
"""

import logging
import time
import os
import sys
import ctypes

from argparse import ArgumentParser
from argparse import RawTextHelpFormatter


def main(shelter_fc, linear_features_fc):
    logging.info("Initializing...")

    logging.info('Start Time: ' + time.ctime(time.time()))
    dtCalcScriptStart = time.time()

    class MEMORYSTATUSEX(ctypes.Structure):
        _fields_ = [
            ("dwLength", ctypes.c_ulong),
            ("dwMemoryLoad", ctypes.c_ulong),
            ("ullTotalPhys", ctypes.c_ulonglong),
            ("ullAvailPhys", ctypes.c_ulonglong),
            ("ullTotalPageFile", ctypes.c_ulonglong),
            ("ullAvailPageFile", ctypes.c_ulonglong),
            ("ullTotalVirtual", ctypes.c_ulonglong),
            ("ullAvailVirtual", ctypes.c_ulonglong),
            ("sullAvailExtendedVirtual", ctypes.c_ulonglong),
        ]

        def __init__(self):
            # have to initialize this to the size of MEMORYSTATUSEX
            self.dwLength = ctypes.sizeof(self)
            super(MEMORYSTATUSEX, self).__init__()



    try:
        arcpy.CheckOutExtension("Spatial")
    except:
        logging.error("*** Unable to use Spatial Analyst extension. Exiting script.")
        sys.exit()

    python_script = sys.argv[0]
    script_path = os.path.split(sys.argv[0])[0]

    # ---------------------------------------------------------------------------------------------------------
    #  Function to construct a time string from a number (of seconds)
    # ---------------------------------------------------------------------------------------------------------

    def SanitizeElapsedTime(dtInput):
        if dtInput < 120:
            strElapsedTime = str(int(dtInput)) + ' sec.'
        elif dtInput < 3600:
            strElapsedTime = str(round(dtInput / 60, 1)) + ' min.'
        else:
            strElapsedTime = str(round(dtInput / 3600, 2)) + ' hr.'
        return strElapsedTime

    # ---------------------------------------------------------------------------------------------------------
    # Check that input feature classes exist
    # ---------------------------------------------------------------------------------------------------------

    for fc in [shelter_fc, linear_features_fc]:
        if not arcpy.Exists(fc):
            logging.error("**** Specified input feature class " + fc + " does not exist. Exiting script.")
            sys.exit()
    if "W_Shelter_1" not in [f.name for f in arcpy.ListFields(shelter_fc)]:
        logging.error("**** Specified shelter polygon feature class does not contain required field W_Shelter_1. Exiting script.")
        sys.exit()

    # ---------------------------------------------------------------------------------------------------------
    # Create Scratch folder and FGDB if they don't already exist
    # ---------------------------------------------------------------------------------------------------------

    input_fgdb = os.path.split(shelter_fc)[0]
    input_folder = os.path.split(input_fgdb)[0]
    scratch_folder = os.path.join(input_folder, "Scratch")
    if not os.path.exists(scratch_folder):
        logging.info("Creating Scratch folder " + scratch_folder)
        os.makedirs(scratch_folder)
    scratch_fgdb = os.path.join(scratch_folder, "Scratch.gdb")
    if not os.path.exists(scratch_fgdb):
        logging.info("Creating Scratch FGDB " + scratch_fgdb)
        arcpy.CreateFileGDB_management(scratch_folder, "Scratch")

    # ---------------------------------------------------------------------------------------------------------
    # Create raster with HaBC specs, and with random pixel values, to act as the snap raster
    # ---------------------------------------------------------------------------------------------------------

    habc_raster = os.path.join(scratch_fgdb, 'HaBC_Raster')
    logging.info("Creating random value raster conforming to HaBC specifications: " + habc_raster)
    if arcpy.Exists(habc_raster):
        arcpy.Delete_management(habc_raster)
    cell_resolution = "100"
    ## Full HaBC extents would be "159587.5 173787.5 1881187.5 1748187.5" but we will use much smaller extents
    ## to save time and effort. Raster will only be used as a snap raster for the other rasters created below.
    arcpy.CreateRandomRaster_management(scratch_fgdb, 'HaBC_Raster', "UNIFORM 0.0 1.0",
                                        "1020387.5 960987.5 1030387.5 970987.5", cell_resolution)
    arcpy.DefineProjection_management(habc_raster,
                                      "PROJCS['NAD_1983_BC_Environment_Albers',"
                                      "GEOGCS['GCS_North_American_1983',"
                                      "DATUM['D_North_American_1983',"
                                      "SPHEROID['GRS_1980',6378137.0,298.257222101]],"
                                      "PRIMEM['Greenwich',0.0],"
                                      "UNIT['Degree',0.0174532925199433]],"
                                      "PROJECTION['Albers'],"
                                      "PARAMETER['False_Easting',1000000.0],"
                                      "PARAMETER['False_Northing',0.0],"
                                      "PARAMETER['Central_Meridian',-126.0],"
                                      "PARAMETER['Standard_Parallel_1',50.0],"
                                      "PARAMETER['Standard_Parallel_2',58.5],"
                                      "PARAMETER['Latitude_Of_Origin',45.0],"
                                      "UNIT['Meter',1.0]]")
    arcpy.env.snapRaster = habc_raster

    # ---------------------------------------------------------------------------------------------------------
    # Steps 1a to 1d
    # ---------------------------------------------------------------------------------------------------------

    if os.path.split(shelter_fc)[1][:7] == "Step1a_":
        shelter_singlepart_fc = scratch_fgdb + os.sep + os.path.split(shelter_fc)[1] + "_singlepart"
    else:
        shelter_singlepart_fc = scratch_fgdb + os.sep + "Step1a_" + os.path.split(shelter_fc)[1] + "_singlepart"
    logging.info("Step 1a: Multipart to Singlepart")
    if arcpy.Exists(shelter_singlepart_fc):
        arcpy.Delete_management(shelter_singlepart_fc)
    arcpy.MultipartToSinglepart_management(shelter_fc, shelter_singlepart_fc)

    shelter_singlepart_dissolve_fc = scratch_fgdb + os.sep + "Step1b_" + \
                                     os.path.split(shelter_singlepart_fc)[1][7:] + "_dissolve"
    logging.info("Step 1b: Dissolve on W_Shelter_1 attribute")
    if arcpy.Exists(shelter_singlepart_dissolve_fc):
        arcpy.Delete_management(shelter_singlepart_dissolve_fc)
    shelter_singlepart_fl_1 = "shelter_singlepart_fl_1"
    where_clause = '"W_Shelter_1" = 1'
    logging.info("    - Selecting polygons where W_Shelter_1 = 1")
    arcpy.MakeFeatureLayer_management(shelter_singlepart_fc, shelter_singlepart_fl_1, where_clause)
    logging.info("    - Dissolving polygons where W_Shelter_1 = 1")
    arcpy.Dissolve_management(shelter_singlepart_fl_1, shelter_singlepart_dissolve_fc, 
                              "W_Shelter_1", "#", "SINGLE_PART")
    shelter_singlepart_fl_0 = "shelter_singlepart_fl_0"
    where_clause = '"W_Shelter_1" <> 1'
    logging.info("    - Selecting polygons where W_Shelter_1 != 1")
    arcpy.MakeFeatureLayer_management(shelter_singlepart_fc, shelter_singlepart_fl_0, where_clause)
    logging.info("    - Appending polygons where W_Shelter_1 != 1 to dissolved feature class")
    arcpy.Append_management(shelter_singlepart_fl_0, shelter_singlepart_dissolve_fc, "NO_TEST")
    ## This dissolved feature class will be used for Step 3e

    field_list = [f.name for f in arcpy.ListFields(shelter_singlepart_dissolve_fc)]
    if "ZeroField" not in field_list:
        logging.info("    - Adding ZeroField field")
        arcpy.AddField_management(shelter_singlepart_dissolve_fc, "ZeroField", "SHORT")
    logging.info("    - Populating ZeroField field with zeros")
    with arcpy.da.UpdateCursor(shelter_singlepart_dissolve_fc, ["ZeroField"]) as cursor:
        for row in cursor:
            row[0] = 0
            cursor.updateRow(row)
    aoi_zero_r = scratch_fgdb + os.sep + "AOI_zero_raster"
    logging.info("    - Converting to AOI raster with all cells set to value zero")
    if arcpy.Exists(aoi_zero_r):
        arcpy.Delete_management(aoi_zero_r)
    arcpy.PolygonToRaster_conversion(shelter_singlepart_dissolve_fc, "ZeroField", aoi_zero_r, "CELL_CENTER", 
                                     "#", 100)
    # This AOI zero raster will be used to fill in gaps in final rasters due to erased linear buffer polygons

    ## Old Step 1c used Union, but this crashes on large input datasets
    # shelter_u_linear_fc = scratch_fgdb + os.sep + "Step1c_" + os.path.split(shelter_singlepart_fc)[1][7:] + "_u_linear"
    # if arcpy.Exists(shelter_u_linear_fc):
    #     arcpy.Delete_management(shelter_u_linear_fc)
    # logging.info(
    #     "- Step 1c: Union with linear features and update shelter values to 0 for areas within linear feature buffers")
    # arcpy.Union_analysis([shelter_singlepart_fc, linear_features_fc], shelter_u_linear_fc)
    # logging.info("    - Updating W_Shelter_1 field values to 0 for polygons within linear feature buffer areas")
    # with arcpy.da.UpdateCursor(shelter_u_linear_fc, ["W_Shelter_1", "FID_" + 
    #                            os.path.split(linear_features_fc)[1]]) as cursor:
    #     for row in cursor:
    #         if row[1] != -1:
    #             row[0] = 0
    #             cursor.updateRow(row)

    ## Instead of Union, using Erase
    shelter_u_linear_fc = scratch_fgdb + os.sep + "Step1c_" + os.path.split(shelter_singlepart_fc)[1][7:] + "_e_linear"
    if arcpy.Exists(shelter_u_linear_fc):
        arcpy.Delete_management(shelter_u_linear_fc)
    logging.info("Step 1c: Erasing linear feature buffers from input polygon feature class")
    oid_field = str([f.name for f in arcpy.ListFields(shelter_singlepart_fc) if f.type == "OID"][0])
    largest_oid = 0
    for row in arcpy.da.SearchCursor(shelter_singlepart_fc, [oid_field]):
        if row[0] > largest_oid:
            largest_oid = row[0]
    chunks = []
    small = 1
    big = 100000
    while big <= largest_oid:
        chunks.append([small, big])
        small += 100000
        big += 100000
    chunks.append([small, largest_oid])
    chunk_count = 0
    fcs_to_append = []
    for chunk in chunks:
        chunk_count += 1
        fl = "fl_" + str(chunk_count)
        where_clause = oid_field + " >= " + str(chunk[0]) + " AND " + oid_field + " <= " + str(chunk[1])
        arcpy.MakeFeatureLayer_management(shelter_singlepart_fc, fl, where_clause)
        shelter_u_linear_fc_chunk = shelter_u_linear_fc + "_chunk_" + str(chunk_count)
        if arcpy.Exists(shelter_u_linear_fc_chunk):
            arcpy.Delete_management(shelter_u_linear_fc_chunk)
        logging.info("    - Erasing chunk " + str(chunk_count) + " of " + str(len(chunks)) \
                     + " (OBJECTID " + str(chunk[0]) + " to " + str(chunk[1]) + ")")
        arcpy.Erase_analysis(fl, linear_features_fc, shelter_u_linear_fc_chunk)
        shelter_u_linear_fc_chunk_sp = shelter_u_linear_fc + "_chunk_" + str(chunk_count) + "_singlepart"
        if arcpy.Exists(shelter_u_linear_fc_chunk_sp):
            arcpy.Delete_management(shelter_u_linear_fc_chunk_sp)
        logging.info("    - Multipart to singlepart")
        arcpy.MultipartToSinglepart_management(shelter_u_linear_fc_chunk, shelter_u_linear_fc_chunk_sp)
        if chunk_count == 1:
            fc_to_be_appended = shelter_u_linear_fc_chunk_sp
        else:
            fcs_to_append.append(shelter_u_linear_fc_chunk_sp)
    arcpy.Copy_management(fc_to_be_appended, shelter_u_linear_fc)
    if len(fcs_to_append) > 0:
        logging.info("    - Appending outputs together")
        arcpy.Append_management(fcs_to_append, shelter_u_linear_fc, "TEST")

    shelter_u_linear_singlepart_fc = scratch_fgdb + os.sep + "Step1d_" + os.path.split(shelter_u_linear_fc)[1][7:] + \
                                     "_singlepart"
    logging.info("Step 1d: Multipart to Singlepart")
    if arcpy.Exists(shelter_u_linear_singlepart_fc):
        arcpy.Delete_management(shelter_u_linear_singlepart_fc)
    arcpy.MultipartToSinglepart_management(shelter_u_linear_fc, shelter_u_linear_singlepart_fc)

    shelter_u_linear_singlepart_dissolve_fc = scratch_fgdb + os.sep + "Step1e_" + \
                                     os.path.split(shelter_u_linear_fc)[1][7:] + "_dissolve"
    logging.info("Step 1e: Dissolve on W_Shelter_1 attribute")
    if arcpy.Exists(shelter_u_linear_singlepart_dissolve_fc):
        arcpy.Delete_management(shelter_u_linear_singlepart_dissolve_fc)
    shelter_u_linear_singlepart_fl_1 = "shelter_u_linear_singlepart_fl_1"
    where_clause = '"W_Shelter_1" = 1'
    logging.info("    - Selecting polygons where W_Shelter_1 = 1")
    arcpy.MakeFeatureLayer_management(shelter_u_linear_singlepart_fc, shelter_u_linear_singlepart_fl_1, where_clause)
    logging.info("    - Dissolving polygons where W_Shelter_1 = 1")
    arcpy.Dissolve_management(shelter_u_linear_singlepart_fl_1, shelter_u_linear_singlepart_dissolve_fc, 
                              "W_Shelter_1", "#", "SINGLE_PART")
    shelter_u_linear_singlepart_fl_0 = "shelter_u_linear_singlepart_fl_0"
    where_clause = '"W_Shelter_1" <> 1'
    logging.info("    - Selecting polygons where W_Shelter_1 != 1")
    arcpy.MakeFeatureLayer_management(shelter_u_linear_singlepart_fc, shelter_u_linear_singlepart_fl_0, where_clause)
    logging.info("    - Appending polygons where W_Shelter_1 != 1 to dissolved feature class")
    arcpy.Append_management(shelter_u_linear_singlepart_fl_0, shelter_u_linear_singlepart_dissolve_fc, "NO_TEST")
    ## This dissolved feature class will be used for Step 2d

    shelter_u_linear_r = scratch_fgdb + os.sep + "Step1f_" + \
                         os.path.split(shelter_u_linear_singlepart_dissolve_fc)[1][7:] + "_raster"
    logging.info("Step 1f: Convert to raster")
    if arcpy.Exists(shelter_u_linear_r):
        arcpy.Delete_management(shelter_u_linear_r)
    arcpy.PolygonToRaster_conversion(shelter_u_linear_singlepart_fc, "W_Shelter_1", shelter_u_linear_r, "CELL_CENTER", 
                                     "#", 100)

    # ---------------------------------------------------------------------------------------------------------
    # Steps 2a to 2e
    # ---------------------------------------------------------------------------------------------------------

    focal_stats_r = scratch_fgdb + os.sep + "Step2a_" + os.path.split(shelter_u_linear_r)[1][7:] + "_fstats"
    logging.info("Step 2a: Focal Statistics")
    if arcpy.Exists(focal_stats_r):
        arcpy.Delete_management(focal_stats_r)
    arcpy.gp.FocalStatistics_sa(shelter_u_linear_r, focal_stats_r, "Rectangle 4 4 CELL", "SUM", "DATA")

    focal_stats_16_r = scratch_fgdb + os.sep + "Step2b_" + os.path.split(focal_stats_r)[1][7:] + "_16"
    logging.info("Step 2b: Reclassify raster to keep only cells with value 16")
    if arcpy.Exists(focal_stats_16_r):
        arcpy.Delete_management(focal_stats_16_r)
    arcpy.gp.Reclassify_sa(focal_stats_r, "Value", "0 15 NODATA;16 16", focal_stats_16_r, "NODATA")

    focal_stats_16_fc = scratch_fgdb + os.sep + "Step2c_" + os.path.split(focal_stats_16_r)[1][7:] + "_pts"
    logging.info("Step 2c: Convert raster to point feature class")
    if arcpy.Exists(focal_stats_16_fc):
        arcpy.Delete_management(focal_stats_16_fc)
    arcpy.gp.RasterToPoint_conversion(focal_stats_16_r, focal_stats_16_fc)

    shelter_u_linear_copy_fc = scratch_fgdb + os.sep + "Step2d_" + \
                               os.path.split(shelter_u_linear_singlepart_dissolve_fc)[1][7:] + "_updated"
    logging.info("Step 2d: Select polygons that intersect points and recalculate W_Shelter_1 field value to 1")
    if arcpy.Exists(shelter_u_linear_copy_fc):
        arcpy.Delete_management(shelter_u_linear_copy_fc)
    arcpy.Copy_management(shelter_u_linear_singlepart_dissolve_fc, shelter_u_linear_copy_fc)
    logging.info("    - Resetting all polygons to a W_Shelter_1 field value of 0")
    with arcpy.da.UpdateCursor(shelter_u_linear_copy_fc,["W_Shelter_1"]) as cursor:
        for row in cursor:
            if row[0] != 0:
                row[0] = 0
                cursor.updateRow(row)
    shelter_u_linear_copy_fl = "shelter_u_linear_copy_fl"
    arcpy.MakeFeatureLayer_management(shelter_u_linear_copy_fc, shelter_u_linear_copy_fl)
    arcpy.SelectLayerByLocation_management(shelter_u_linear_copy_fl, "INTERSECT", focal_stats_16_fc)
    select_count = int(arcpy.GetCount_management(shelter_u_linear_copy_fl).getOutput(0))
    total_count = int(arcpy.GetCount_management(shelter_u_linear_copy_fc).getOutput(0))
    logging.info("    - " + str(select_count) + " of " + str(total_count) + 
                 " selected to be assigned a W_Shelter_1 value of 1")
    if select_count > 0:
        with arcpy.da.UpdateCursor(shelter_u_linear_copy_fl,["W_Shelter_1"]) as cursor:
            for row in cursor:
                row[0] = 1
                cursor.updateRow(row)

    logging.info("Step 2e: Convert to final raster")
    shelter_with_gaps_r = scratch_fgdb + os.sep + "Step2e_Shelter_1_400m_depth_EWH_with_linear_buffer_gaps"
    logging.info("    - Converting to raster with linear buffer area gaps")
    if arcpy.Exists(shelter_with_gaps_r):
        arcpy.Delete_management(shelter_with_gaps_r)
    arcpy.PolygonToRaster_conversion(shelter_u_linear_copy_fc, "W_Shelter_1", shelter_with_gaps_r, "CELL_CENTER", "#", 
                                     100)
    working_fgdb = os.path.split(shelter_fc)[0]
    final_shelter_r = working_fgdb + os.sep + "Shelter_1_400m_depth_EWH"
    logging.info("    - Mosaic with AOI to fill linear buffer gaps to final raster " + final_shelter_r)
    if arcpy.Exists(final_shelter_r):
        arcpy.Delete_management(final_shelter_r)
    arcpy.MosaicToNewRaster_management([shelter_with_gaps_r, aoi_zero_r], working_fgdb, "Shelter_1_400m_depth_EWH", 
                                       "#", "#", "100", "1", "SUM")
    logging.info("    - Building raster attribute table")
    arcpy.BuildRasterAttributeTable_management(final_shelter_r, "Overwrite")

    # ---------------------------------------------------------------------------------------------------------
    # Steps 3a to 3e, repetition of previous steps without introducing linear features to shelter polygons
    # ---------------------------------------------------------------------------------------------------------

    shelter_r = scratch_fgdb + os.sep + "Step3a_" + os.path.split(shelter_singlepart_fc)[1][7:] + "_raster"
    logging.info("Step 3a: Convert shelter polygons to raster")
    if arcpy.Exists(shelter_r):
        arcpy.Delete_management(shelter_r)
    arcpy.PolygonToRaster_conversion(shelter_singlepart_fc, "W_Shelter_1", shelter_r, "CELL_CENTER", 
                                     "#", 100)

    focal_stats_r = scratch_fgdb + os.sep + "Step3b_" + os.path.split(shelter_r)[1][7:] + "_fstats"
    logging.info("Step 3b: Focal Statistics")
    if arcpy.Exists(focal_stats_r):
        arcpy.Delete_management(focal_stats_r)
    arcpy.gp.FocalStatistics_sa(shelter_r, focal_stats_r, "Rectangle 4 4 CELL", "SUM", "DATA")

    focal_stats_16_r = scratch_fgdb + os.sep + "Step3c_" + os.path.split(focal_stats_r)[1][7:] + "_16"
    logging.info("Step 3c: Reclassify raster to keep only cells with value 16")
    if arcpy.Exists(focal_stats_16_r):
        arcpy.Delete_management(focal_stats_16_r)
    arcpy.gp.Reclassify_sa(focal_stats_r, "Value", "0 15 NODATA;16 16", focal_stats_16_r, "NODATA")

    focal_stats_16_fc = scratch_fgdb + os.sep + "Step3d_" + os.path.split(focal_stats_16_r)[1][7:] + "_pts"
    logging.info("Step 3d: Convert raster to point feature class")
    if arcpy.Exists(focal_stats_16_fc):
        arcpy.Delete_management(focal_stats_16_fc)
    arcpy.gp.RasterToPoint_conversion(focal_stats_16_r, focal_stats_16_fc)

    shelter_copy_fc = scratch_fgdb + os.sep + "Step3e_" + \
                               os.path.split(shelter_singlepart_dissolve_fc)[1] + "_updated"
    logging.info("Step 3e: Select polygons that intersect points and recalculate W_Shelter_1 field value to 1")
    logging.info("Creating copy of shelter polygon feature class: " + shelter_copy_fc)
    if arcpy.Exists(shelter_copy_fc):
        arcpy.Delete_management(shelter_copy_fc)
    arcpy.Copy_management(shelter_singlepart_dissolve_fc, shelter_copy_fc)
    logging.info("    - Resetting all polygons to a W_Shelter_1 field value of 0")
    with arcpy.da.UpdateCursor(shelter_copy_fc,["W_Shelter_1"]) as cursor:
        for row in cursor:
            if row[0] != 0:
                row[0] = 0
                cursor.updateRow(row)
    shelter_copy_fl = "shelter_copy_fl"
    arcpy.MakeFeatureLayer_management(shelter_copy_fc, shelter_copy_fl)
    arcpy.SelectLayerByLocation_management(shelter_copy_fl, "INTERSECT", focal_stats_16_fc)
    select_count = int(arcpy.GetCount_management(shelter_copy_fl).getOutput(0))
    total_count = int(arcpy.GetCount_management(shelter_copy_fc).getOutput(0))
    logging.info("    - " + str(select_count) + " of " + str(total_count) + 
                 " selected to be assigned a W_Shelter_1 value of 1")
    if select_count > 0:
        with arcpy.da.UpdateCursor(shelter_copy_fl,["W_Shelter_1"]) as cursor:
            for row in cursor:
                row[0] = 1
                cursor.updateRow(row)

    final_shelter_r = working_fgdb + os.sep + "Shelter_1_400m_depth_EWH_intact"
    logging.info("Step 3f: Convert to final raster " + final_shelter_r)
    if arcpy.Exists(final_shelter_r):
        arcpy.Delete_management(final_shelter_r)
    arcpy.PolygonToRaster_conversion(shelter_copy_fc, "W_Shelter_1", final_shelter_r, "CELL_CENTER", "#", 100)

    # ---------------------------------------------------------------------------------------------------------
    # Done
    # ---------------------------------------------------------------------------------------------------------

    dtCalcNow = time.time()
    dtCalcScriptElapsed = dtCalcNow - dtCalcScriptStart
    logging.info("Script complete after " + SanitizeElapsedTime(dtCalcScriptElapsed))

if __name__ == '__main__':
    try:
        # Parse arguments
        parser = ArgumentParser(description='This script produces the 400m Depth (Effective Winter Habitat) raster, '
                                            'using the specified shelter polygon and buffered linear features '
                                            'feature classes as input.',
                                formatter_class=RawTextHelpFormatter)
        parser.add_argument('spo', help='Shelter polygons dissolved')
        parser.add_argument('lfb', help='Linear features buffered')
        parser.add_argument('-l', '--level', type=int,
                            help='Log level\nValues: 10-DEBUG, 20-INFO, 30-WARN(default), 40-ERROR, 50-CRITICAL')
        parser.add_argument('-ld', '--log_dir', help='Path to log directory')
        args = parser.parse_args()

        # Set up logger
        if args.level is not None and args.level not in [10, 20, 30, 40, 50]:
            raise ValueError('Invalid log level')
        elif args.level is None:
            args.level = 20

        logging.basicConfig(format='%(asctime)s - %(levelname)s - %(message)s', level=args.level)

        # Import arcpy
        import arcpy

        # Start the script
        main(args.spo, args.lfb)

    except Exception as e:
        logging.exception('Unexpected exception. Program terminating.')
else:
    import arcpy
