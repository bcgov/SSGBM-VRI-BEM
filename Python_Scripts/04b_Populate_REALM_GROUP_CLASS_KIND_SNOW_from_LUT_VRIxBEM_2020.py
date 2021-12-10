"""

Original Author:
Madrone (Jeff Kruys)

Created on:
2016-03-15

Purpose:
This script populates the TEIS fields, REALM, GROUP, CLASS, and KIND as well 
as SNOW_CODE from the data provided by the Lookup Table

Usage:
04b_Populate_REALM_GROUP_CLASS_KIND_SNOW_from_LUT_RSEA.py vfc [-h] [-l] [-ld]

Positional Arguments:
   bfc              Input BEM polygon feature class
   lut              Lookup table CSV file

Optional Arguments:
  -h, --help       show this help message and exit
  -l, --level      log level messages to display; Default: 20 - INFO
  -ld, --log_dir   path to directory for output log file

Example Input:
X:\fullpath\04b_Populate_REALM_GROUP_CLASS_KIND_SNOW_from_LUT_RSEA.py Y:\fullpath\vfc Z:\fullpath\lut

Outputs:  
BEM feature class is updated with the attributes for the REALM, GROUP, CLASS, 
KIND and SNOW_CODE fields

Dependencies:
The csv file must have values for the above fields for each unique EcoSection, 
BGC and BEU_MC combination

Expected Duration :  Depends on the size of the input BEM feature class

History
2016-03-15 (JK): Script created for BEM Ominceca Moose WHR (Madrone Dosier 15.0242)
2020-03-10 (JK): Updated to PEP-8 Python style standard
2020-09-04 (AE): Script used for the Northeast Moose WHR (Madrone Dosier 19.0461)
#
Make sure that Phase doesnt have NULLs in it. Use "" instead.
"""

import logging
import time
import os
import sys
import ctypes
import math
import operator
import csv

from argparse import ArgumentParser
from argparse import RawTextHelpFormatter


def main(working_fc, sts_csv):
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

    ## ---------------------------------------------------------------------------------------------------------
    ## Check that the expected arguments were provided
    ## ---------------------------------------------------------------------------------------------------------

    if not arcpy.Exists(working_fc):
        logging.error("- Specified WHR feature class does not exist.")
        sys.exit()

    if not os.path.exists(sts_csv):
        logging.error("- Specified Lookup table CSV file does not exist.")
        sys.exit()

    ## ---------------------------------------------------------------------------------------------------------
    ## Read the structural stage lookup table into dictionaries
    ## ---------------------------------------------------------------------------------------------------------

    logging.info("    - Reading STS lookup table")
    required_csv_fields = ["BGC_Label", "BGC_ZONE", "BGC_SUBZON", "BGC_VRT", "BGC_PHASE", "BEU_MC", "REALM", 
                           "GROUP", "CLASS", "KIND", "Forested (Y/N)", "Strct_Climax", "Stand_Climax", 
                           "Stand_Age_0-15", "Stand_Age_16-30", "Stand_Age_31-50", "Stand_Age_51-80", "Stand_Age_80+", 
                           "Struct_Age_0-3", "Struct_Age_4-10", "Struct_Age_11-30", "Struct_Age_31-40", 
                           "Struct_Age_41-60", "Struct_Age_61-80", "Struct_Age_81-139", "Struct_Age_140-249", 
                           "Struct_Age_250+", "Snow_Code"]

    realm_dict = {}
    group_dict = {}
    class_dict = {}
    kind_dict = {}
    snow_dict = {}
    line_counter = 0
    sts_csv_obj = open(sts_csv, "rU")

    csvReader = csv.DictReader(sts_csv_obj)
    missing_fields = []
    for data in csvReader:
        pass  
        ## just need to get one row of the csv file into "data", to test the existence of required fields - need to 
        ## think of a clever way to do this without looping through the whole csv file
    for req_field in required_csv_fields:
        if req_field not in data:
            missing_fields.append(req_field)
    if len(missing_fields) > 0:
        logging.info(" *** Structural stage lookup table missing required field(s): " + str(missing_fields))
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
        curr_realm             = str(data["REALM"]).replace('"','').replace(' ','')
        curr_group             = str(data["GROUP"]).replace('"','').replace(' ','')
        curr_class             = str(data["CLASS"]).replace('"','').replace(' ','')
        curr_kind              = str(data["KIND"]).replace('"','').replace(' ','')
        curr_snow              = str(data["Snow_Code"]).replace('"','').replace(' ','')

        curr_bgc_unit = curr_bgc_zone + curr_bgc_subzon + curr_bgc_vrt + curr_bgc_phase
        curr_eco_unit = curr_bgc_unit + "~" + curr_beu_mc

        realm_dict[curr_eco_unit]       = curr_realm
        group_dict[curr_eco_unit]       = curr_group
        class_dict[curr_eco_unit]       = curr_class
        kind_dict[curr_eco_unit]        = curr_kind
        snow_dict[curr_bgc_unit]        = curr_snow
                
        if line_counter == 100 or line_counter % 1000 == 0:
            logging.info("        - Read " + str(line_counter) + " lines")
    logging.info("        - Read " + str(line_counter) + " lines")


    ## ---------------------------------------------------------------------------------------------------------
    ## Remove Null values from BGC_PHASE field.
    ## ---------------------------------------------------------------------------------------------------------

    logging.info("    - Replacing Null values with empty string in BGC_PHASE field")
    with arcpy.da.UpdateCursor(working_fc, ["BGC_PHASE"]) as cursor:
        for row in cursor:
            if row[0] == None:
                row[0] = ""

    ## ---------------------------------------------------------------------------------------------------------
    ## Update the REALM_1, REALM_2 and REALM_3 fields in the specified BEM feature class
    ## ---------------------------------------------------------------------------------------------------------

    logging.info("    - Populating fields in BEM feature class")
    cfl = ["BGC_ZONE", "BGC_SUBZON", "BGC_VRT", "BGC_PHASE", "SDEC_1", "BEUMC_S1", "SDEC_2", "BEUMC_S2", "SDEC_3", 
           "BEUMC_S3", "REALM_1", "REALM_2", "REALM_3", "GROUP_1", "GROUP_2", "GROUP_3", "CLASS_1", "CLASS_2", 
           "CLASS_3", "KIND_1", "KIND_2", "KIND_3", "SNOW_CODE"]

    row_count = 0
    with arcpy.da.UpdateCursor(working_fc,cfl) as cursor:
        for row in cursor:
            row_count += 1

            bgc_unit = str(row[cfl.index("BGC_ZONE")]) + str(row[cfl.index("BGC_SUBZON")]) \
                       + str(row[cfl.index("BGC_VRT")]).replace("None","") + str(row[cfl.index("BGC_PHASE")])

            if row[cfl.index("SDEC_1")] not in [0,None]:
                eco_unit_1 = bgc_unit + "~" + str(row[cfl.index("BEUMC_S1")]).replace("None","")
            else:
                eco_unit_1 = ""
            if row[cfl.index("SDEC_2")] not in [0,None]:
                eco_unit_2 = bgc_unit + "~" + str(row[cfl.index("BEUMC_S2")]).replace("None","")
            else:
                eco_unit_2 = ""
            if row[cfl.index("SDEC_3")] not in [0,None]:
                eco_unit_3 = bgc_unit + "~" + str(row[cfl.index("BEUMC_S3")]).replace("None","")
            else:
                eco_unit_3 = ""

            try:
                row[cfl.index("REALM_1")] = realm_dict[eco_unit_1]
                row[cfl.index("GROUP_1")] = group_dict[eco_unit_1]
                row[cfl.index("CLASS_1")] = class_dict[eco_unit_1]
                row[cfl.index("KIND_1")]  = kind_dict[eco_unit_1]
            except:
                row[cfl.index("REALM_1")] = ""
                row[cfl.index("GROUP_1")] = ""
                row[cfl.index("CLASS_1")] = ""
                row[cfl.index("KIND_1")]  = ""
            try:
                row[cfl.index("REALM_2")] = realm_dict[eco_unit_2]
                row[cfl.index("GROUP_2")] = group_dict[eco_unit_2]
                row[cfl.index("CLASS_2")] = class_dict[eco_unit_2]
                row[cfl.index("KIND_2")]  = kind_dict[eco_unit_2]
            except:
                row[cfl.index("REALM_2")] = ""
                row[cfl.index("GROUP_2")] = ""
                row[cfl.index("CLASS_2")] = ""
                row[cfl.index("KIND_2")]  = ""
            try:
                row[cfl.index("REALM_3")] = realm_dict[eco_unit_3]
                row[cfl.index("GROUP_3")] = group_dict[eco_unit_3]
                row[cfl.index("CLASS_3")] = class_dict[eco_unit_3]
                row[cfl.index("KIND_3")]  = kind_dict[eco_unit_3]
            except:
                row[cfl.index("REALM_3")] = ""
                row[cfl.index("GROUP_3")] = ""
                row[cfl.index("CLASS_3")] = ""
                row[cfl.index("KIND_3")]  = ""

            try:
                row[cfl.index("SNOW_CODE")] = int(snow_dict[bgc_unit])
            except:
                row[cfl.index("SNOW_CODE")] = None

            try:
                cursor.updateRow(row)
            except:
                logging.info("Error writing row " + str(row_count) + ": " + str(row))
                sys.exit()
                
            if row_count == 100 or row_count % 100000 == 0:
                    logging.info("            - Processed " + str(row_count) + " rows")
        logging.info("            - Processed " + str(row_count) + " rows")

    # ---------------------------------------------------------------------------------------------------------
    # Done
    # ---------------------------------------------------------------------------------------------------------

    dtCalcNow = time.time()
    dtCalcScriptElapsed = dtCalcNow - dtCalcScriptStart
    logging.info("- Script complete after " + SanitizeElapsedTime(dtCalcScriptElapsed))

if __name__ == '__main__':
    try:
        # Parse arguments
        parser = ArgumentParser(description='This script generates a list of unique BGC label and habitat '
                                            'combinations for the Look up Table starting point.',
                                formatter_class=RawTextHelpFormatter)
        parser.add_argument('bfc', help='Input BEM polygon feature class')
        parser.add_argument('lut', help='Lookup table CSV file')
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
        main(args.bfc, args.lut)

    except Exception as e:
        logging.exception('Unexpected exception. Program terminating.')
else:
    import arcpy
