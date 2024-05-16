"""

Original Author:
Madrone (Jeff Kruys)

Created on:
2020-01-30

Purpose:
This script creates the Creating the Dash distance (100m from Shelter/Security 
to Dynamic forage) raster.

Usage:
Dash_Distance.py ifc a6r [-h] [-l] [-ld]

Positional Arguments:
   ifc              Input Vector HEM feature class containting "Security_1" field
   a6r              AOI_WMUs_6 raster

Optional Arguments:
  -h, --help       show this help message and exit
  -l, --level      log level messages to display; Default: 30 - WARN
  -ld, --log_dir   path to directory for output log file

Example Input:
X:\fullpath\Dash_Distance.py Y:\fullpath\ifc Z:\fullpath\a6r

History
2020-01-30 (JK): Created script Dash_Distance.
2020-02-18 (JK): Added code to create Hectares BC snap raster
"""

import logging
import time
import os
import sys
import ctypes

from argparse import ArgumentParser
from argparse import RawTextHelpFormatter


def main(input_fc, aoi6_r):
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

    for fc in [input_fc]:
        if not arcpy.Exists(fc):
            logging.error("**** Specified input feature class " + fc + " does not exist. Exiting script.")
            sys.exit()
    exit_script = False
    for required_field in ["W_Shelter_1", "Dynamic_WFD_All", "Security_1"]:
        if required_field not in [f.name for f in arcpy.ListFields(input_fc)]:
            logging.error("**** Specified shelter polygon feature class does not contain required field " + \
                          required_field + ". Exiting script.")
            exit_script = True
    if exit_script:
        sys.exit()

    # ---------------------------------------------------------------------------------------------------------
    # Create Scratch folder and FGDB if they don't already exist
    # ---------------------------------------------------------------------------------------------------------

    input_fgdb = os.path.split(input_fc)[0]
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
    # Buffer the polygons with Security_1 = 1 by 100m and convert to raster
    # ---------------------------------------------------------------------------------------------------------

    security_fl = "security_fl"
    where_clause = '"Security_1" = 1'
    logging.info("Selecting polygons where Security_1 = 1")
    arcpy.MakeFeatureLayer_management(input_fc, security_fl, where_clause)
    
    buff_fc = os.path.join(scratch_fgdb, "Buffer_to_Select_Dynamic_Forage_100m")
    if arcpy.Exists(buff_fc):
        arcpy.Delete_management(buff_fc)
    logging.info("Buffering by 100m")
    arcpy.Buffer_analysis(security_fl, buff_fc, "100 Meters")
    logging.info("Adding 'RasterValue' field and assigning a value of 4 to this field to all buffer polygons")
    arcpy.AddField_management(buff_fc, "RasterValue", "SHORT")
    with arcpy.da.UpdateCursor(buff_fc, ["RasterValue"]) as cursor:
        for row in cursor:
            row[0] = 4
            cursor.updateRow(row)

    buff_r = buff_fc + "_raster"
    if arcpy.Exists(buff_r):
        arcpy.Delete_management(buff_r)
    logging.info("Converting buffer polygons to raster: " + buff_r)
    arcpy.PolygonToRaster_conversion(buff_fc, "RasterValue", buff_r, "CELL_CENTER", "#", 100)

    # ---------------------------------------------------------------------------------------------------------
    # Select the polygons with Dynamic_WFD_All = 1 and convert to raster
    # ---------------------------------------------------------------------------------------------------------

    dynamic_forage_fl = "dynamic_forage_fl"
    where_clause = '"Dynamic_WFD_All" = 1'
    logging.info("Selecting polygons where Dynamic_WFD_All = 1")
    arcpy.MakeFeatureLayer_management(input_fc, dynamic_forage_fl, where_clause)
    
    dynamic_forage_r = os.path.join(scratch_fgdb, "Dynamic_WFD_All_meets_site_cond")
    if arcpy.Exists(dynamic_forage_r):
        arcpy.Delete_management(dynamic_forage_r)
    logging.info("Converting selected polygons to raster: " + dynamic_forage_r)
    arcpy.PolygonToRaster_conversion(dynamic_forage_fl, "Dynamic_WFD_All", dynamic_forage_r, "CELL_CENTER", "#", 100)

    # ---------------------------------------------------------------------------------------------------------
    # Merge the above two rasters and the AOI_WMUs_6 raster to a new raster with overlapping cells assigned
    # the sum of the three input cells' values
    # ---------------------------------------------------------------------------------------------------------

    merged_r_name = "Dyn_Forage_100m_Dash_Security_temp"
    merged_r = os.path.join(scratch_fgdb, merged_r_name)
    if arcpy.Exists(merged_r):
        arcpy.Delete_management(merged_r)
    # logging.info("Merging the two new rasters with the AOI_WMUs_6 raster with overlapping cell values summed")
    # arcpy.MosaicToNewRaster_management([buff_r, dynamic_forage_r, aoi6_r], scratch_fgdb, merged_r_name, "#", "#",
    #                                    "#", 1, "SUM")
    logging.info("Calculating weighted sum of the two new rasters with the AOI_WMUs_6 raster")
    arcpy.gp.WeightedSum_sa(buff_r + " Value 1;" + dynamic_forage_r + " Value 1;" + aoi6_r + " Value 1", merged_r)

    # ---------------------------------------------------------------------------------------------------------
    # Reclassify raster so cells with value 11 are assigned a 1 and all others assigned a 0
    # ---------------------------------------------------------------------------------------------------------

    merged_reclass_r = os.path.join(os.path.split(input_fc)[0], "Dyn_Forage_100m_Dash_Security")
    if arcpy.Exists(merged_reclass_r):
        arcpy.Delete_management(merged_reclass_r)
    logging.info("Reclassifying the raster to convert 11's to 1's and all other values to 0")
    arcpy.gp.Reclassify_sa(merged_r, "Value", "0 10 0;11 1", merged_reclass_r, "NODATA")
    logging.info("Final output raster: " + merged_reclass_r)

    # ---------------------------------------------------------------------------------------------------------
    # Done
    # ---------------------------------------------------------------------------------------------------------

    dtCalcNow = time.time()
    dtCalcScriptElapsed = dtCalcNow - dtCalcScriptStart
    logging.info("- Script complete after " + SanitizeElapsedTime(dtCalcScriptElapsed))

if __name__ == '__main__':
    try:
        # Parse arguments
        parser = ArgumentParser(description='This script creates the Creating the Dash distance (100m from '
                                            'Shelter/Security to Dynamic forage) raster. ',
                                formatter_class=RawTextHelpFormatter)
        parser.add_argument('ifc', help='Input polygons')
        parser.add_argument('a6r', help='AOI_WMUs_6 raster')
        parser.add_argument('-l', '--level', type=int,
                            help='Log level\nValues: 10-DEBUG, 20-INFO(default), 30-WARN, 40-ERROR, 50-CRITICAL')
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
        main(args.ifc, args.a6r)

    except Exception as e:
        logging.exception('Unexpected exception. Program terminating.')
else:
    import arcpy
