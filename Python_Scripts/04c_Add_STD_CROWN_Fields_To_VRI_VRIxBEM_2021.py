"""

Original Author:
Madrone (Jeff Kruys)

Created on:
2021-01-28

Purpose:
This script takes an input VRI feature class and adds two new field, STD_VRI,
CROWN_BEAR and CROWN_MOOSE. It populates the STD_VRI field "C", "B" or "M" based 
on the existing values in the SPECIES_CD_# and SPECIES_PCT_# fields, and populates 
the CROWN_BEAR field with codes 1 to 4, and CROWN_MOOSE with codes H, M, L, VL and N
based on the existing values in the CROWN_CLOSURE field.

Usage:
Add_STD_CROWN_Fields_To_VRI.py vfc [-h] [-l] [-ld]

Positional Arguments:
   vfc              Input VRI feature class
   
Optional Arguments:
  -h, --help       show this help message and exit
  -l, --level      log level messages to display; Default: 20 - INFO
  -ld, --log_dir   path to directory for output log file

Example Input:
X:\fullpath\Add_STD_CROWN_Fields_To_VRI.py Y:\fullpath\vfc


History
2021-02-10 (JK): Created script, based on the old script Add_Stand_Field_To_VRI.py.
2021-02-18 (JK): Added code to populate two fields CROWN_BEAR and CROWN_MOOSE rather
                 than one field CROWN_C.
"""

import logging
import time
import os
import sys
import ctypes
import pdb

from argparse import ArgumentParser
from argparse import RawTextHelpFormatter

def main(vri_fc):
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
    # Check that input feature class exists and contains all required fields
    # ---------------------------------------------------------------------------------------------------------

    if not arcpy.Exists(vri_fc):
        logging.error("**** Specified input feature class " + vri_fc + " does not exist. Exiting script.")
        sys.exit()
    
    existing_fields = [f.name for f in arcpy.ListFields(vri_fc)]
    required_fields = ["SPECIES_CD_1", "SPECIES_CD_2", "SPECIES_CD_3", "SPECIES_CD_4", "SPECIES_CD_5", "SPECIES_CD_6",
                       "SPECIES_PCT_1", "SPECIES_PCT_2", "SPECIES_PCT_3", "SPECIES_PCT_4", "SPECIES_PCT_5", 
                       "SPECIES_PCT_6", "CROWN_CLOSURE"]
    missing_fields = []
    for required_field in required_fields:
        if required_field not in existing_fields:
            missing_fields.append(required_field)
    if len(missing_fields) > 0:
        logging.error("**** Specified input feature class is missing required fields: " 
                      + str(missing_fields).replace("[", "").replace("]", "").replace("'", ""))

    # ---------------------------------------------------------------------------------------------------------
    # Create populate STD_VRI field
    # ---------------------------------------------------------------------------------------------------------

    if "STD_VRI" not in existing_fields:
        logging.info("Adding field STD_VRI")
        arcpy.AddField_management(vri_fc, "STD_VRI", "TEXT", "#", "#", 1)
    else:
        for f in arcpy.ListFields(vri_fc):
            if f.name == "STD_VRI" and f.type != "String":
                logging.error("STD_VRI field already exists and it the wrong type ({}). "
                              "Remove or rename the STD_VRI field, then run this script again.".format(f.type))
                sys.exit()
        logging.info("Existing STD_VRI field will be overwritten.")

    if "CROWN_BEAR" not in existing_fields:
        logging.info("Adding field CROWN_BEAR")
        arcpy.AddField_management(vri_fc, "CROWN_BEAR", "SHORT")
    else:
        for f in arcpy.ListFields(vri_fc):
            if f.name == "CROWN_BEAR" and f.type != "SmallInteger":
                logging.error("CROWN_BEAR field already exists and it the wrong type ({}). "
                              "Remove or rename the CROWN_BEAR field, then run this script again.".format(f.type))
                sys.exit()
        logging.info("Existing CROWN_BEAR field will be overwritten.")

    if "CROWN_MOOSE" not in existing_fields:
        logging.info("Adding field CROWN_MOOSE")
        arcpy.AddField_management(vri_fc, "CROWN_MOOSE", "TEXT", "#", "#", 2)
    else:
        for f in arcpy.ListFields(vri_fc):
            if f.name == "CROWN_MOOSE" and f.type != "String":
                logging.error("CROWN_MOOSE field already exists and it the wrong type ({}). "
                              "Remove or rename the CROWN_MOOSE field, then run this script again.".format(f.type))
                sys.exit()
            if f.name == "CROWN_MOOSE" and f.type == "String" and f.length < 2:
                logging.error("CROWN_MOOSE field already exists and it the wrong length ({}). "
                              "Remove or rename the CROWN_MOOSE field, then run this script again.".format(f.length))
                sys.exit()
        logging.info("Existing CROWN_MOOSE field will be overwritten.")

    b_species = ["D", "DR", "U", "UP", "A", "AC", "ACB", "ACT", "AX", "AT", "R", "RA", "E", "EA", "EXP", "EP", "EW", 
                 "GP", "MB", "MB", "MV", "Q", "QG", "XH", "V", "VB", "VP", "W", "WS", "WA", "WB", "WD", "WP", "WT"]

    logging.info("Populating the STD_VRI, CROWN_BEAR and CROWN_MOOSE fields")
    cfl = ["SPECIES_CD_1", "SPECIES_CD_2", "SPECIES_CD_3", "SPECIES_CD_4", "SPECIES_CD_5", "SPECIES_CD_6",
           "SPECIES_PCT_1", "SPECIES_PCT_2", "SPECIES_PCT_3", "SPECIES_PCT_4", "SPECIES_PCT_5", "SPECIES_PCT_6", 
           "STD_VRI", "CROWN_CLOSURE", "CROWN_BEAR", "CROWN_MOOSE"]
    row_count = 0
    row_total = int(arcpy.GetCount_management(vri_fc).getOutput(0))
    with arcpy.da.UpdateCursor(vri_fc, cfl) as cursor:
        for row in cursor:
            row_count += 1
            all_pct_null = True
            b_pct = 0
            for x in ["1", "2", "3", "4", "5", "6"]:
                if str(row[cfl.index("SPECIES_CD_" + x)]).upper() in b_species:
                    if row[cfl.index("SPECIES_PCT_" + x)] > 0:
                        b_pct += row[cfl.index("SPECIES_PCT_" + x)]
                if row[cfl.index("SPECIES_PCT_" + x)] > 0:
                    all_pct_null = False
            
            if all_pct_null:
                row[cfl.index("STD_VRI")] = ""
            elif b_pct < 25:
                row[cfl.index("STD_VRI")] = "C"
            elif b_pct < 75:
                row[cfl.index("STD_VRI")] = "M"
            else:
                row[cfl.index("STD_VRI")] = "B"
            
            if 0 <= row[cfl.index("CROWN_CLOSURE")] <= 20:
                row[cfl.index("CROWN_BEAR")] = 1
            elif 20 < row[cfl.index("CROWN_CLOSURE")] <= 40:
                row[cfl.index("CROWN_BEAR")] = 2
            elif 40 < row[cfl.index("CROWN_CLOSURE")] <= 60:
                row[cfl.index("CROWN_BEAR")] = 3
            elif 60 < row[cfl.index("CROWN_CLOSURE")]:
                row[cfl.index("CROWN_BEAR")] = 4
            else:
                row[cfl.index("CROWN_BEAR")] = None

            if row[cfl.index("CROWN_CLOSURE")] == 0:
                row[cfl.index("CROWN_MOOSE")] = "N"
            elif 1 <= row[cfl.index("CROWN_CLOSURE")] <= 9:
                row[cfl.index("CROWN_MOOSE")] = "VL"
            elif 10 <= row[cfl.index("CROWN_CLOSURE")] <= 25:
                row[cfl.index("CROWN_MOOSE")] = "L"
            elif 26 <= row[cfl.index("CROWN_CLOSURE")] <= 60:
                row[cfl.index("CROWN_MOOSE")] = "M"
            elif 61 <= row[cfl.index("CROWN_CLOSURE")]:
                row[cfl.index("CROWN_MOOSE")] = "H"
            else:
                row[cfl.index("CROWN_MOOSE")] = ""

            cursor.updateRow(row)

            if row_count % 100000 == 0 or row_count == row_total:
                logging.info("    Processed {} of {} rows.".format(row_count, row_total))

    # ---------------------------------------------------------------------------------------------------------
    # Done
    # ---------------------------------------------------------------------------------------------------------

    dtCalcNow = time.time()
    dtCalcScriptElapsed = dtCalcNow - dtCalcScriptStart
    logging.info("Script complete after {}".format(SanitizeElapsedTime(dtCalcScriptElapsed)))

if __name__ == '__main__':
    try:
        # Parse arguments
        parser = ArgumentParser(description='This script adds a new field STD_VRI to a specified VRI '
                                            'feature class and populates it based on the SPECIES fields.',
                                formatter_class=RawTextHelpFormatter)
        parser.add_argument('vfc', help='Input VRI feature class')
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
        main(args.vfc)

    except Exception as e:
        logging.exception('Unexpected exception. Program terminating.')
else:
    import arcpy
