"""

Original Author:
Madrone (Jeff Kruys)

Created on:
2021-02-10

Purpose:
This script overlays a BEM and VRI feature classes and determines the area-dominant
CROWN_BEAR and CROWN_MOOSE values for each BEM polygon.

Usage:
Add_CROWN_Fields_To_BEM.py bfc vfc [-h] [-l] [-ld]

Positional Arguments:
   bfc              BEM feature class
   vfc              VRI feature class that contains CROWN_BEAR and CROWN_MOOSE fields
   
Optional Arguments:
  -h, --help       show this help message and exit
  -l, --level      log level messages to display; Default: 20 - INFO
  -ld, --log_dir   path to directory for output log file

Example Input:
X:\fullpath\Add_CROWN_Fields_To_BEM.py Y:\fullpath\bfc Z:\fullpath\vfc

History
2021-02-10 (JK): Created script, based on the old script Add_STD_VRI_Field_To_BEM.py.
2021-02-18 (JK): Updated to handle CROWN_BEAR and CROWN_MOOSE fields instead of CROWN_C.
2021-02-24 (JK): Removed the STD_VRI from this script, which will be handled by a separate script,
                 Add_STD_VRI_Field_To_BEM.py. Renamed this script Add_CROWN_Fields_To_BEM.py.

"""

import logging
import time
import os
import sys
import ctypes
import pdb
import operator

from argparse import ArgumentParser
from argparse import RawTextHelpFormatter

def main(bem_fc, vri_fc):
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
    # Check that input feature classes exist and contain required fields
    # ---------------------------------------------------------------------------------------------------------

    if not arcpy.Exists(bem_fc):
        logging.error("**** Specified BEM feature class {} does not exist. Exiting script.".format(bem_fc))
        sys.exit()

    teis_id_field_found = False
    for f in arcpy.ListFields(bem_fc):
        if f.name == "TEIS_ID":
            teis_id_field_found = True
            if f.type not in ["Integer", "SmallInteger"]:
                logging.error("**** TEIS_ID field in specified BEM feature class is not a numeric field. "
                              "Exiting script.")
                sys.exit()
    if not teis_id_field_found:
        logging.error("**** Specified BEM feature class does not have a TEIS_ID field. Exiting script.")
        sys.exit()

    if not arcpy.Exists(vri_fc):
        logging.error("**** Specified VRI feature class does not exist. Exiting script.")
        sys.exit()

    req_vri_fields = ["CROWN_BEAR", "CROWN_MOOSE"]
    existing_vri_fields = [f.name for f in arcpy.ListFields(vri_fc)]
    missing_fields = []
    for req_vri_field in req_vri_fields:
        if req_vri_field not in existing_vri_fields:
            missing_fields.append(req_vri_field)
    if len(missing_fields) > 0:
        logging.error("**** Specified VRI feature class is missing required fields {}. Exiting script.".format(
                str(missing_fields)))
        sys.exit()

    req_bem_fields = ["STRCT_S1", "STRCT_S2", "STRCT_S3", "FORESTED_1", "FORESTED_2", "FORESTED_3"]
    existing_bem_fields = [f.name for f in arcpy.ListFields(bem_fc)]
    missing_fields = []
    for req_bem_field in req_bem_fields:
        if req_bem_field not in existing_bem_fields:
            missing_fields.append(req_bem_field)
    if len(missing_fields) > 0:
        logging.error("**** Specified BEM feature class is missing required fields {}. Exiting script.".format(
                str(missing_fields)))
        sys.exit()

    # ---------------------------------------------------------------------------------------------------------
    # Check that TEIS_ID field contains unique values
    # ---------------------------------------------------------------------------------------------------------

    logging.info("Checking that BEM TEIS_ID field contains unique values")
    teis_id_count_dict = {}
    row_count = 0
    total_count = int(arcpy.GetCount_management(bem_fc).getOutput(0))
    dupe_teis_id_found = False
    for row in arcpy.da.SearchCursor(bem_fc, ["TEIS_ID"]):
        row_count += 1
        try:
            teis_id_count_dict[row[0]] += 1
        except:
            teis_id_count_dict[row[0]] = 1
        if teis_id_count_dict[row[0]] > 1:
            dupe_teis_id_found = True
        if row_count % 100000 == 0 or row_count == total_count:
            logging.info("    - Read " + str(row_count) + " of " + str(total_count) + " rows")
    if dupe_teis_id_found:
        logging.info("    - Duplicate TEIS_ID values found. Repopulating TEIS_ID field with OBJECTID values.")
        row_count = 0
        with arcpy.da.UpdateCursor(bem_fc, ["TEIS_ID", "OID@"]) as cursor:
            for row in cursor:
                row_count += 1
                row[0] = row[1]
                cursor.updateRow(row)
                if row_count % 100000 == 0 or row_count == total_count:
                    logging.info("    - Updated " + str(row_count) + " of " + str(total_count) + " rows")
    else:
        logging.info("    - TEIS_ID field contains all unique values.")

    # ---------------------------------------------------------------------------------------------------------
    # Calculate CROWN_BEAR and CROWN_MOOSE values for each BEM polygon.
    # ---------------------------------------------------------------------------------------------------------

    tab_int_tbl = bem_fc + "_tab_int_vri"
    vri_fields = ["CROWN_BEAR", "CROWN_MOOSE"]
    logging.info("Creating Tabulate Intersection table " + tab_int_tbl)
    if arcpy.Exists(tab_int_tbl):
        arcpy.Delete_management(tab_int_tbl)
    arcpy.TabulateIntersection_analysis(in_zone_features = bem_fc, zone_fields = "TEIS_ID", in_class_features = vri_fc, 
                                        out_table = tab_int_tbl, class_fields = vri_fields, sum_fields = "", 
                                        xy_tolerance = "-1 Unknown", out_units = "UNKNOWN")

    row_total = int(arcpy.GetCount_management(tab_int_tbl).getOutput(0))
    tabulate_intersection_succeeded = False

    crown_b_dict = {}
    crown_m_dict = {}
    if row_total > 0: ## sometimes the TabulateIntersection tool results in an empty output table for no reason
        logging.info("Reading Tabulate Intersection table to dictionary")
        row_count = 0
        for row in arcpy.da.SearchCursor(tab_int_tbl,["TEIS_ID", "CROWN_BEAR", "CROWN_MOOSE", "AREA"]):
            row_count += 1
            try:
                crown_b_dict[row[0]][row[1]] += row[3]
            except:
                try:
                    crown_b_dict[row[0]][row[1]] = row[3]
                except:
                    crown_b_dict[row[0]] = {}
                    crown_b_dict[row[0]][row[1]] = row[3]
            try:
                crown_m_dict[row[0]][row[2]] += row[3]
            except:
                try:
                    crown_m_dict[row[0]][row[2]] = row[3]
                except:
                    crown_m_dict[row[0]] = {}
                    crown_m_dict[row[0]][row[2]] = row[3]

            if row_count % 100000 == 0 or row_count == row_total:
                logging.info("    Read {} of {} rows".format(row_count, row_total))
        tabulate_intersection_succeeded = True

    else: ## if output table was empty, run an Intersect instead
        logging.error("**** Tabulate Intersection output table is empty")
        logging.info("Running an Intersect of BEM and age feature classes")
        intersect_fc = bem_fc + "_int_vri"
        if arcpy.Exists(intersect_fc):
            arcpy.Delete_management(intersect_fc)
        arcpy.Intersect_analysis(in_features = vri_fc + " #;" + bem_fc + " #", out_feature_class = intersect_fc, 
                                 join_attributes = "ALL", cluster_tolerance = "-1 Unknown", output_type = "INPUT")
        row_total = int(arcpy.GetCount_management(intersect_fc).getOutput(0))
        if row_total > 0:
            logging.info("Reading Intersect output feature class table to dictionary")
            row_count = 0
            for row in arcpy.da.SearchCursor(intersect_fc, ["TEIS_ID", "CROWN_BEAR", "CROWN_MOOSE", "SHAPE@AREA"]):
                row_count += 1
                try:
                    crown_b_dict[row[0]][row[1]] += row[3]
                except:
                    try:
                        crown_b_dict[row[0]][row[1]] = row[3]
                    except:
                        crown_b_dict[row[0]] = {}
                        crown_b_dict[row[0]][row[1]] = row[3]
                try:
                    crown_m_dict[row[0]][row[2]] += row[3]
                except:
                    try:
                        crown_m_dict[row[0]][row[2]] = row[3]
                    except:
                        crown_m_dict[row[0]] = {}
                        crown_m_dict[row[0]][row[2]] = row[3]
                if row_count % 100000 == 0 or row_count == row_total:
                    logging.info("    Read {} of {} rows".format(row_count, row_total))
        else:
            arcpy.Delete_management(intersect_fc)
            logging.error("Intersection is empty; VRI and PEM/TEM feature classes do not overlap. Exiting.")
            sys.exit()

    bem_fields = [f.name for f in arcpy.ListFields(bem_fc)]
    for i in range(1, 4):
        if "CROWN_BEAR_{}".format(i) not in bem_fields:
            logging.info("Adding new field CROWN_BEAR_{} to BEM feature class.".format(i))
            arcpy.AddField_management(bem_fc, "CROWN_BEAR_{}".format(i), "SHORT")
        else:
            logging.info("Existing values will be overwritten in CROWN_BEAR_{} field in BEM.".format(i))
    for i in range(1, 4):
        if "CROWN_MOOSE_{}".format(i) not in bem_fields:
            logging.info("Adding new field CROWN_MOOSE_{} to BEM feature class.".format(i))
            arcpy.AddField_management(bem_fc, "CROWN_MOOSE_{}".format(i), "TEXT", "#", "#", 2)
        else:
            logging.info("Existing values will be overwritten in CROWN_MOOSE_{} field in BEM.".format(i))

    row_count = 0
    no_crown_b_count = 0
    no_crown_m_count = 0
    cfl = ["TEIS_ID", "CROWN_BEAR_1", "CROWN_BEAR_2", "CROWN_BEAR_3", "CROWN_MOOSE_1", "CROWN_MOOSE_2", 
           "CROWN_MOOSE_3", "STRCT_S1", "STRCT_S2", "STRCT_S3", "FORESTED_1", "FORESTED_2", "FORESTED_3"]
    row_total = int(arcpy.GetCount_management(bem_fc).getOutput(0))
    logging.info("Writing CROWN_BEAR_# and CROWN_MOOSE_# values to BEM")
    with arcpy.da.UpdateCursor(bem_fc, cfl) as cursor:
        for row in cursor:
            row_count += 1
            try:
                biggest_crown_b = max(crown_b_dict[row[0]].iteritems(), key=operator.itemgetter(1))[0]    
                ## see http://stackoverflow.com/questions/268272/getting-key-with-maximum-value-in-dictionary
                for i in range(1, 4):
                    if row[cfl.index("FORESTED_{}".format(i))] == "Y" and str(
                            row[cfl.index("STRCT_S{}".format(i))])[:1] in ["4", "5", "6", "7"]:
                        row[cfl.index("CROWN_BEAR_{}".format(i))] = biggest_crown_b
                    else:
                        row[cfl.index("CROWN_BEAR_{}".format(i))] = None
            except:
                # if the current polygon had no entry in the dictionary, then there is no
                # age class info for the polygon, so assign it values of -1.
                for i in range(1, 4):
                    row[cfl.index("CROWN_BEAR_{}".format(i))] = None
                    no_crown_b_count += 1
            try:
                biggest_crown_m = max(crown_m_dict[row[0]].iteritems(), key=operator.itemgetter(1))[0]    
                ## see http://stackoverflow.com/questions/268272/getting-key-with-maximum-value-in-dictionary
                for i in range(1, 4):
                    if row[cfl.index("FORESTED_{}".format(i))] == "Y" and str(
                            row[cfl.index("STRCT_S{}".format(i))])[:1] in ["4", "5", "6", "7"]:
                        row[cfl.index("CROWN_MOOSE_{}".format(i))] = biggest_crown_m
                    else:
                        row[cfl.index("CROWN_MOOSE_{}".format(i))] = None
            except:
                # if the current polygon had no entry in the dictionary, then there is no
                # age class info for the polygon, so assign it values of -1.
                for i in range(1, 4):
                    row[cfl.index("CROWN_MOOSE_{}".format(i))] = ""
                    no_crown_m_count += 1
            cursor.updateRow(row)
            if row_count % 100000 == 0 or row_count == row_total:
                logging.info("    Processed {} of {} rows".format(row_count, row_total))
        if no_crown_b_count == 0 and no_crown_m_count == 0:
            logging.info("All {} BEM polygon(s) overlapped with a VRI polygon. That's good!".format(row_total))
        else:
            logging.info("**** WARNING: There were {} polygon(s) for which stand and/or crown classes could "
                         "not be calculated. These polygons probably don't overlap with any polygons in the age "
                         "feature class.".format(max(no_crown_b_count, no_crown_m_count)))
    # arcpy.Delete_management(tab_int_tbl)

    # ---------------------------------------------------------------------------------------------------------
    # Done
    # ---------------------------------------------------------------------------------------------------------

    dtCalcNow = time.time()
    dtCalcScriptElapsed = dtCalcNow - dtCalcScriptStart
    logging.info("Script complete after " + SanitizeElapsedTime(dtCalcScriptElapsed))

if __name__ == '__main__':
    try:
        # Parse arguments
        parser = ArgumentParser(description='This script copies the CROWN_BEAR and CROWN_MOOSE fields from a VRI '
                                            'feature class to a BEM feature class based on overlap area ',
                                formatter_class=RawTextHelpFormatter)
        parser.add_argument('bfc', help='BEM feature class')
        parser.add_argument('vfc', help='VRI feature class')
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
        main(args.bfc, args.vfc)

    except Exception as e:
        logging.exception('Unexpected exception. Program terminating.')
else:
    import arcpy
