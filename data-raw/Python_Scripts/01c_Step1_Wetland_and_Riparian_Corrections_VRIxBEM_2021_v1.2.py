"""

Original Author:
Madrone (Jeff Kruys)

Created on:
2020-09-27

Purpose:
This script updates BEM attributes based on intersection with 
Wetland polygons.

Usage:
Overlay_Wetlands_BEM.py ifc rfc buc [-h] [-l] [-ld]

Positional Arguments:
   bfc              BEM polygon feature class
   wfc              Wetlands polygon feature class [FWA_Wetlands[poly]]
   buc              BEU wetland update table CSV file

Optional Arguments:
  -h, --help       show this help message and exit
  -l, --level      log level messages to display; Default: 20 - INFO
  -ld, --log_dir   path to directory for output log file

Example Input:
X:\fullpath\01c_Step1_Wetland_and_Riparian_Corrections_VRIxBEM.py Y:\fullpath\bfc Z:\fullpath\wfc W:\fullpath\buc.csv

History
2020-09-27 (JK): Created script Overlay_Wetlands_BEM.
2020-10-28 (JK): Added code to read in BEU update table and calculate updates
                 to SDEC_# and BEUMC_S# fields based on wetland overlap. Updates
                 are only written to a comment field; fields containing the 
                 original data are not overwritten.
2020-11-16 (JK): Added code to overwrite the ecosystem unit field values in 
                 the input feature class with the updated values.
2021-01-27 (JK): Renamed script to 01c_Step1_Wetland_and_Riparian_Corrections_VRIxBEM.py.
                 Added code to update BEUMC_S1 to the riparian mapcode that 
                 corresponds with its BGC_ZONE when MEAN_SLOPE < 10, floodplain
                 modifier 'a' is assigned in SITE_M3A, and BEUMC_S1 that is not
                 unvegetated.
2021-03-22 (AE): Updated to ignore and water BEUs that are 50% or over and added "BA" as a water BEU.  
2021-03-22 (JK): Updated to also ignore (i.e. not add WL components to) polygons with BCLCS_LEVEL_4 = TB, TC or TM
2021-03-23 (JK): Updated to add SMPL_TYPE field to the BEM feature class if it doesn't already exist, and
                 to skip updating a record if the SMPL_TYPE field contains any non-null value.
2021-05-10 (AJ): Updated to add "RI" to the list that wetlands should not be applied to. 
"""

import logging
import time
import os
import sys
import ctypes
import csv
import pdb

from argparse import ArgumentParser
from argparse import RawTextHelpFormatter


def main(bem_fc, wl_fc, beu_csv):
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
    # Check that input feature class exists and contains the required fields
    # ---------------------------------------------------------------------------------------------------------

    if not arcpy.Exists(bem_fc):
        logging.error("**** Specified BEM feature class " + bem_fc + " does not exist. Exiting script.")
        sys.exit()
    if not arcpy.Exists(wl_fc):
        logging.error("**** Specified wetlands feature class " + wl_fc + " does not exist. Exiting script.")
        sys.exit()
    if not arcpy.Exists(beu_csv):
        logging.error("**** Specified BEU update table CSV file " + beu_csv + " does not exist. Exiting script.")
        sys.exit()
    req_f = ["TEIS_ID", "SDEC_1", "BEUMC_S1", "REALM_1", "GROUP_1", "CLASS_1", "KIND_1", "SITE_S1", 
            "SITEAM_S1A", "SITEAM_S1B", "SITEAM_S1C", "SITEAM_S1D", "SITEMC_S1", "SITE_M1A", "SITE_M1B", 
            "STRCT_S1", "STRCT_M1", "STAND_A1", "SERAL_1", "TREE_C1", "SHRUB_C1", "DISTCLS_1", 
            "DISTSCLS_1", "DISSSCLS_1", "SECL_1", "SESUBCL_1", "COND_1", "VIAB_1", "SDEC_2", "BEUMC_S2", 
            "REALM_2", "GROUP_2", "CLASS_2", "KIND_2", "SITE_S2", "SITEAM_S2A", "SITEAM_S2B", "SITEAM_S2C", 
            "SITEAM_S2D", "SITEMC_S2", "SITE_M2A", "SITE_M2B", "STRCT_S2", "STRCT_M2", "STAND_A2", 
            "SERAL_2", "TREE_C2", "SHRUB_C2", "DISTCLS_2", "DISTSCLS_2", "DISSSCLS_2", "SECL_2", 
            "SESUBCL_2", "COND_2", "VIAB_2", "SDEC_3", "BEUMC_S3", "REALM_3", "GROUP_3", "CLASS_3", 
            "KIND_3", "SITE_S3", "SITEAM_S3A", "SITEAM_S3B", "SITEAM_S3C", "SITEAM_S3D", "SITEMC_S3", 
            "SITE_M3A", "SITE_M3B", "STRCT_S3", "STRCT_M3", "STAND_A3", "SERAL_3", "TREE_C3", "SHRUB_C3", 
            "DISTCLS_3", "DISTSCLS_3", "DISSSCLS_3", "SECL_3", "SESUBCL_3", "COND_3", "VIAB_3", "BGC_ZONE",
            "MEAN_SLOPE", "BCLCS_LEVEL_4"]
    existing_fields = [f.name for f in arcpy.ListFields(bem_fc) if not f.required]
    missing_fields = []
    for required_field in req_f:
        if required_field not in existing_fields:
            missing_fields.append(required_field)
    if len(missing_fields) > 0:
        logging.error("**** Input feature class is missing the following required fields: " + str(missing_fields))
        sys.exit()

    logging.info("Checking that TEIS_ID field contains unique values")
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
        total_count = int(arcpy.GetCount_management(bem_fc).getOutput(0))
        with arcpy.da.UpdateCursor(bem_fc, ["TEIS_ID", "OID@"]) as cursor:
            for row in cursor:
                row_count += 1
                row[0] = row[1]
                cursor.updateRow(row)
                if row_count % 100000 == 0 or row_count == total_count:
                    logging.info("    - Updated " + str(row_count) + " of " + str(total_count) + " rows")
    else:
        logging.info("    - TEIS_ID field contains all unique values.")

    if "Lbl_edit_wl" not in existing_fields:
        logging.info("Adding new field 'Lbl_edit_wl'")
        arcpy.AddField_management(bem_fc, "Lbl_edit_wl", "TEXT", "#", "#", "1000")
    req_f.append("Lbl_edit_wl")

    # ---------------------------------------------------------------------------------------------------------
    # Add SMPL_TYPE field if it doesn't already exist
    # ---------------------------------------------------------------------------------------------------------

    if "SMPL_TYPE" not in [f.name for f in arcpy.ListFields(bem_fc)]:
        logging.info("Adding field SMPL_TYPE to input BEM feature class")
        arcpy.AddField_management(bem_fc, "SMPL_TYPE", "TEXT", "#", "#", 1, "Field Check of Polygon")
    req_f.append("SMPL_TYPE")
   
    # ---------------------------------------------------------------------------------------------------------
    # Make note of which polygons have SITE_M3A == 'a' for later (before the 'a' is potentially moved to
    # SITE_M1A or SITE_M2A)
    # ---------------------------------------------------------------------------------------------------------

    logging.info("Reading BEM attributes to find polygons with SITE_M3A = 'a'")
    site_m3a_is_a_dict = {}
    a_count = 0
    total_count = int(arcpy.GetCount_management(bem_fc).getOutput(0))
    for row in arcpy.da.SearchCursor(bem_fc, ["TEIS_ID", "SITE_M3A"]):
        if row[1] == "a":
            site_m3a_is_a_dict[row[0]] = True
            a_count += 1
        else:
            site_m3a_is_a_dict[row[0]] = False
    logging.info("    Found " + str(a_count) + " of " + str(total_count) + " polygons with SITE_M3A = 'a'")

    # ---------------------------------------------------------------------------------------------------------
    # Read the table of BEU decile and mapcode updates according to wetland overlap percentage
    # ---------------------------------------------------------------------------------------------------------

    logging.info("Reading BEU wetland update table CSV file")
    beu_update_dict = {}
    data_counter = 0
    beu_update_csv_object = open(beu_csv, "rU")
    csvReader = csv.DictReader(beu_update_csv_object)
    csv_fields_checked = False
    for data in csvReader:
        if not csv_fields_checked:
            try:
                y = data["Code_Orig"]
                for x in [0, 1, 2, 3, 4, 5, 6, 7, 8, 10]:
                    y = data["Code_WL" + str(x)]
            except:
                logging.error("Specified CSV file missing required field(s) 'Code_Orig', 'Code_WL0' through "
                              "'Code_WL8', 'Code_WL10'")
                logging.error("Exiting script.")
                sys.exit()
            csv_fields_checked = True

        ## Read the codes into a dictionary.
        ## The codes look like 7212, where the 7, 2, 1 are the three decile values, and the 2 indicates
        ## that the 2nd decile mapcode is WL (or 0 if none of them is WL). 
        ## If first decile value is 10, code would be 10000 or 10001.
        data_counter += 1
        beu_update_dict[data["Code_Orig"]] = {}
        for x in [0, 1, 2, 3, 4, 5, 6, 7, 8, 10]:  ## the proportion of wetland overlap as a decile value
            beu_update_dict[data["Code_Orig"]][x] = data["Code_WL" + str(x)]

    logging.info("    - Read " + str(data_counter) + " lines from CSV file")
    del csvReader

    # ---------------------------------------------------------------------------------------------------------
    # Run the overlay to calculate wetland overlap percentage in each polygon
    # ---------------------------------------------------------------------------------------------------------

    logging.info("Overlaying BEM polygons with wetland polygons to table")
    intersect_tbl = bem_fc + "_tabint_wetlands"
    if arcpy.Exists(intersect_tbl):
        arcpy.Delete_management(intersect_tbl)
    arcpy.TabulateIntersection_analysis(bem_fc, "TEIS_ID", wl_fc, intersect_tbl)
    
    bem_wl_dict = {}
    logging.info("Reading overlay table to determine wetland area of each BEM polygon")
    for row in arcpy.da.SearchCursor(intersect_tbl, ["TEIS_ID", "PERCENTAGE"]): ## check these field names
        bem_wl_dict[row[0]] = int(round(row[1], 0))
    arcpy.Delete_management(intersect_tbl)

    logging.info("Updating BEM feature class attributes to reflect actual wetland presence in each polygon")
    row_count = 0
    total_count = int(arcpy.GetCount_management(bem_fc).getOutput(0))
    eco_unit_string_field_list = {}
    eco_unit_integer_field_list = {}
    for dec in ["1", "2", "3"]:
        eco_unit_string_field_list[dec] = []
        eco_unit_string_field_list[dec].append("REALM_" + dec)
        eco_unit_string_field_list[dec].append("GROUP_" + dec)
        eco_unit_string_field_list[dec].append("CLASS_" + dec)
        eco_unit_string_field_list[dec].append("KIND_" + dec)
        eco_unit_string_field_list[dec].append("SITE_S" + dec)
        eco_unit_string_field_list[dec].append("SITEAM_S" + dec + "A")
        eco_unit_string_field_list[dec].append("SITEAM_S" + dec + "B")
        eco_unit_string_field_list[dec].append("SITEAM_S" + dec + "C")
        eco_unit_string_field_list[dec].append("SITEAM_S" + dec + "D")
        eco_unit_string_field_list[dec].append("SITEMC_S" + dec)
        eco_unit_string_field_list[dec].append("SITE_M" + dec + "A")
        eco_unit_string_field_list[dec].append("SITE_M" + dec + "B")
        eco_unit_string_field_list[dec].append("STRCT_S" + dec)
        eco_unit_string_field_list[dec].append("STRCT_M" + dec)
        eco_unit_string_field_list[dec].append("STAND_A" + dec)
        eco_unit_string_field_list[dec].append("SERAL_" + dec)
        eco_unit_string_field_list[dec].append("DISTCLS_" + dec)
        eco_unit_string_field_list[dec].append("DISTSCLS_" + dec)
        eco_unit_string_field_list[dec].append("DISSSCLS_" + dec)
        eco_unit_string_field_list[dec].append("SECL_" + dec)
        eco_unit_string_field_list[dec].append("SESUBCL_" + dec)
        eco_unit_string_field_list[dec].append("COND_" + dec)
        eco_unit_string_field_list[dec].append("VIAB_" + dec)
        eco_unit_integer_field_list[dec] = []
        eco_unit_integer_field_list[dec].append("TREE_C" + dec)
        eco_unit_integer_field_list[dec].append("SHRUB_C" + dec)

    with arcpy.da.UpdateCursor(bem_fc, req_f) as cursor:
        for row in cursor:
            row_count += 1
            if row[req_f.index("SMPL_TYPE")] in ["", "None", None]:
                teis_id = row[req_f.index("TEIS_ID")]
                sdec_1 = row[req_f.index("SDEC_1")]
                beumc_s1 = row[req_f.index("BEUMC_S1")]
                sdec_2 = row[req_f.index("SDEC_2")]
                beumc_s2 = row[req_f.index("BEUMC_S2")]
                sdec_3 = row[req_f.index("SDEC_3")]
                beumc_s3 = row[req_f.index("BEUMC_S3")]
                bclcs_level_4 = row[req_f.index("BCLCS_LEVEL_4")]
                eco_unit = {}
                for dec in ["1", "2", "3"]:
                    eco_unit[dec] = []
                    for eco_unit_string_field in eco_unit_string_field_list[dec]:
                        eco_unit[dec].append(row[req_f.index(eco_unit_string_field)])
                    for eco_unit_integer_field in eco_unit_integer_field_list[dec]:
                        eco_unit[dec].append(row[req_f.index(eco_unit_integer_field)])
                wl_pct = 0
                try:
                    wl_pct = bem_wl_dict[teis_id]
                    row[req_f.index("Lbl_edit_wl")] = str(wl_pct) + "% of polygon occupied by wetland."
                except:
                    row[req_f.index("Lbl_edit_wl")] = "No wetland."
                row[req_f.index("Lbl_edit_wl")] += " Current BEU: "
                row[req_f.index("Lbl_edit_wl")] += str(sdec_1)
                row[req_f.index("Lbl_edit_wl")] += " " + str(beumc_s1)
                if sdec_2 > 0:
                    row[req_f.index("Lbl_edit_wl")] += ", " + str(sdec_2)
                    row[req_f.index("Lbl_edit_wl")] += " " + str(beumc_s2)
                    if sdec_3 > 0:
                        row[req_f.index("Lbl_edit_wl")] += ", " + str(sdec_3)
                        row[req_f.index("Lbl_edit_wl")] += " " + str(beumc_s3)

                curr_beu_code = str(sdec_1) + str(sdec_2).replace("None", "0") + str(sdec_3).replace("None", "0")
                if beumc_s1 == "WL":
                    curr_beu_code += "1"
                elif beumc_s2 == "WL":
                    curr_beu_code += "2"
                elif beumc_s3 == "WL":
                    curr_beu_code += "3"
                else:
                    curr_beu_code += "0"
                row[req_f.index("Lbl_edit_wl")] += " (" + str(curr_beu_code) + ")"
                # If curr_beu_code is 4 digits: 1 digit for each decile, with the 4th digit:
                #     0 if none of the 3 components is WL
                #     1 if the 1st component is WL
                #     2 if the 2nd component is WL
                #     3 if the 3rd component is WL
                # If curr_beu_code is 5 digits: the first decile value must 10, the 2nd and 3rd are 0's,
                # and the 5th digit is:
                #     0 if the first and only component is not WL
                #     1 if the first and only component is WL
                
                new_sdec_1 = sdec_1
                new_beumc_s1 = beumc_s1
                new_sdec_2 = sdec_2
                new_beumc_s2 = beumc_s2
                new_sdec_3 = sdec_3
                new_beumc_s3 = beumc_s3
                
                new_eco_unit = {}
                new_eco_unit["1"] = []
                new_eco_unit["2"] = []
                new_eco_unit["3"] = []

                # Phase 2:
                # 1. Remove all 'WL' mapcodes from Dec3 - these were added during the BEM process and no longer apply at
                # the VRI scale
                #     a. How to re-adjust the deciles... ? Add dec 3 to dec 1.
                if sdec_3 > 0 and beumc_s3 == "WL":
                    new_sdec_1 += new_sdec_3
                    new_sdec_3 = None
                    new_beumc_s3 = ""
                    for eco_unit_string_field in eco_unit_string_field_list["3"]:
                        new_eco_unit["3"].append("")
                    for eco_unit_integer_field in eco_unit_integer_field_list["3"]:
                        new_eco_unit["3"].append(None)

                # 2. Ignore any pure decile that are listed in table 1 that indicate a WL overlap.
                # Changed to ignore any water BEU with a decile more of equal to 50% and to ignore WL is a pure decile. 
                # Changed to ignore polygon with BCLCS_LEVEL_4 = TB, TC or TM (which would have resulted in removing
                # any WL component in script 01b)
                if not (sdec_1 >= 5 and beumc_s1 in ["BB", "CB", "CR", "ER", "RI", "PB", "PR", "RR", "RS", "SK", "SR", "TF", 
                        "WG", "WR", "YB", "YS", "BG", "FE", "MR", "OW", "SH", "SW", "BA", "LS", "LL"]) and not (
                        sdec_1 == 10 and beumc_s1 == "WL") and not wl_pct < 8 and not bclcs_level_4 in ["TB", "TC", 
                        "TM"]:
                    if curr_beu_code in beu_update_dict.keys():
                        if 8 <= wl_pct < 14:
                            wl_dec_value = 1
                        elif 15 <= wl_pct < 25:
                            wl_dec_value = 2
                        elif 25 <= wl_pct < 35:
                            wl_dec_value = 3
                        elif 35 <= wl_pct < 45:
                            wl_dec_value = 4
                        elif 45 <= wl_pct < 55:
                            wl_dec_value = 5
                        elif 55 <= wl_pct < 65:
                            wl_dec_value = 6
                        elif 65 <= wl_pct < 75:
                            wl_dec_value = 7
                        elif 75 <= wl_pct < 80:
                            wl_dec_value = 8
                        elif wl_pct >= 80:
                            wl_dec_value = 10
                        new_beu_code = beu_update_dict[curr_beu_code][wl_dec_value]
                        if new_beu_code != curr_beu_code:
                            if len(str(new_beu_code)) == 5:
                                ## A 5-digit code means the first decile is a 10, then 0, 0, and 1 if it's WL or 0 if not
                                new_sdec_1 = 10
                                if str(new_beu_code)[-1:] == "1":
                                    new_beumc_s1 = "WL"
                                    new_eco_unit["1"] = []
                                    for eco_unit_string_field in eco_unit_string_field_list["1"]:
                                        new_eco_unit["1"].append("")
                                    for eco_unit_integer_field in eco_unit_integer_field_list["1"]:
                                        new_eco_unit["1"].append(None)
                                else:
                                    new_beumc_s1 = beumc_s1
                                    new_eco_unit["1"] = eco_unit["1"]
                                new_sdec_2 = 0
                                new_beumc_s2 = ""
                                new_eco_unit["2"] = []
                                for eco_unit_string_field in eco_unit_string_field_list["2"]:
                                    new_eco_unit["2"].append("")
                                for eco_unit_integer_field in eco_unit_integer_field_list["2"]:
                                    new_eco_unit["2"].append(None)
                                new_sdec_3 = 0
                                new_beumc_s3 = ""
                                new_eco_unit["3"] = []
                                for eco_unit_string_field in eco_unit_string_field_list["3"]:
                                    new_eco_unit["3"].append("")
                                for eco_unit_integer_field in eco_unit_integer_field_list["3"]:
                                    new_eco_unit["3"].append(None)
                            else:
                                ## Code is 4 digits: 1 digit for each decile value, then 0 or 1
                                new_sdec_1 = int(str(new_beu_code)[:1])
                                new_sdec_2 = int(str(new_beu_code)[1:2])
                                new_sdec_3 = int(str(new_beu_code)[2:3])
                                if str(new_beu_code)[3:4] == "0":
                                    # New 4th digit is zero, no WL code
                                    if str(curr_beu_code)[3:4] == "0":  ## No WL code in any "old" decile
                                        # Old 4th digit was 0, no changes
                                        new_beumc_s1 = beumc_s1
                                        new_eco_unit["1"] = eco_unit["1"]
                                        new_beumc_s2 = beumc_s2
                                        new_eco_unit["2"] = eco_unit["2"]
                                        new_beumc_s3 = beumc_s3
                                        new_eco_unit["3"] = eco_unit["3"]
                                    elif str(curr_beu_code)[3:4] == "1":
                                        # Old 4th digit was 1, removing WL from 1st component
                                        new_beumc_s1 = beumc_s2
                                        new_eco_unit["1"] = eco_unit["2"]
                                        new_beumc_s2 = beumc_s3
                                        new_eco_unit["2"] = eco_unit["3"]
                                        new_beumc_s3 = ""
                                        new_eco_unit["3"] = []
                                        for eco_unit_string_field in eco_unit_string_field_list["3"]:
                                            new_eco_unit["3"].append("")
                                        for eco_unit_integer_field in eco_unit_integer_field_list["3"]:
                                            new_eco_unit["3"].append(None)
                                    elif str(curr_beu_code)[3:4] == "2":
                                        # Old 4th digit was 2, removing WL from 2nd component
                                        new_beumc_s1 = beumc_s1
                                        new_eco_unit["1"] = eco_unit["1"]
                                        new_beumc_s2 = beumc_s3
                                        new_eco_unit["2"] = eco_unit["3"]
                                        new_beumc_s3 = ""
                                        new_eco_unit["3"] = []
                                        for eco_unit_string_field in eco_unit_string_field_list["3"]:
                                            new_eco_unit["3"].append("")
                                        for eco_unit_integer_field in eco_unit_integer_field_list["3"]:
                                            new_eco_unit["3"].append(None)
                                    elif str(curr_beu_code)[3:4] == "3":
                                        # Old 4th digit was 3, removing WL from 3rd component
                                        new_beumc_s1 = beumc_s1
                                        new_eco_unit["1"] = eco_unit["1"]
                                        new_beumc_s2 = beumc_s2
                                        new_eco_unit["2"] = eco_unit["2"]
                                        new_beumc_s3 = ""
                                        new_eco_unit["3"] = []
                                        for eco_unit_string_field in eco_unit_string_field_list["3"]:
                                            new_eco_unit["3"].append("")
                                        for eco_unit_integer_field in eco_unit_integer_field_list["3"]:
                                            new_eco_unit["3"].append(None)
                                elif str(new_beu_code)[3:4] == "1":  ## "WL" belongs in 1st decile
                                    # New 4th digit is 1; WL goes in 1st component
                                    if str(curr_beu_code)[3:4] == "0":
                                        # Old 4th digit was 0; adding WL to 1st component and shifting 1st/2nd to 2nd/3rd
                                        new_beumc_s1 = "WL"
                                        new_eco_unit["1"] = []
                                        for eco_unit_string_field in eco_unit_string_field_list["1"]:
                                            # When adding a new WL component, also make REALM_# = "W",
                                            # GROUP_# = "W" and KIND_# = "U"
                                            if eco_unit_string_field[:5] in ["REALM", "GROUP"]:
                                                new_eco_unit["1"].append("W")
                                            elif eco_unit_string_field[:4] == "KIND":
                                                new_eco_unit["1"].append("U")
                                            else:
                                                new_eco_unit["1"].append("")
                                        for eco_unit_integer_field in eco_unit_integer_field_list["1"]:
                                            new_eco_unit["1"].append(None)
                                        new_beumc_s2 = beumc_s1
                                        new_eco_unit["2"] = eco_unit["1"]
                                        new_beumc_s3 = beumc_s2
                                        new_eco_unit["3"] = eco_unit["2"]
                                    elif str(curr_beu_code)[3:4] == "1":
                                        # Old 4th digit was 1, keeping WL in 1st component
                                        new_beumc_s1 = "WL"
                                        new_eco_unit["1"] = eco_unit["1"]
                                        new_beumc_s2 = beumc_s2
                                        new_eco_unit["2"] = eco_unit["2"]
                                        new_beumc_s3 = beumc_s3
                                        new_eco_unit["3"] = eco_unit["3"]
                                    elif str(curr_beu_code)[3:4] == "2":
                                        # Old 4th digit was 2, moving WL from 2nd to 1st component and moving old 1st 
                                        # to 2nd
                                        new_beumc_s1 = "WL"
                                        new_eco_unit["1"] = eco_unit["2"]
                                        new_beumc_s2 = beumc_s1
                                        new_eco_unit["2"] = eco_unit["1"]
                                        new_beumc_s3 = beumc_s3
                                        new_eco_unit["3"] = eco_unit["3"]
                                    elif str(curr_beu_code)[3:4] == "3":
                                        # Old 4th digit was 3, moving WL from 3rd to 1st component and moving old 1st/2nd 
                                        # to 2nd/3rd
                                        new_beumc_s1 = "WL"
                                        new_eco_unit["1"] = eco_unit["3"]
                                        new_beumc_s2 = beumc_s1
                                        new_eco_unit["2"] = eco_unit["1"]
                                        new_beumc_s3 = beumc_s2
                                        new_eco_unit["3"] = eco_unit["2"]
                                elif str(new_beu_code)[3:4] == "2":
                                    # New 4th digit is 2, WL goes in 2nd component
                                    if str(curr_beu_code)[3:4] == "0":
                                        # Old 4th digit was 0, adding WL to 2nd component and shifting old 2nd to 3rd
                                        new_beumc_s1 = beumc_s1
                                        new_eco_unit["1"] = eco_unit["1"]
                                        new_beumc_s2 = "WL"
                                        new_eco_unit["2"] = []
                                        for eco_unit_string_field in eco_unit_string_field_list["2"]:
                                            # When adding a new WL component, also make REALM_# = "W",
                                            # GROUP_# = "W" and KIND_# = "U"
                                            if eco_unit_string_field[:5] in ["REALM", "GROUP"]:
                                                new_eco_unit["2"].append("W")
                                            elif eco_unit_string_field[:4] == "KIND":
                                                new_eco_unit["2"].append("U")
                                            else:
                                                new_eco_unit["2"].append("")
                                        for eco_unit_integer_field in eco_unit_integer_field_list["2"]:
                                            new_eco_unit["2"].append(None)
                                        new_beumc_s3 = beumc_s2
                                        new_eco_unit["3"] = eco_unit["2"]
                                    elif str(curr_beu_code)[3:4] == "1":
                                        # Old 4th digit was 1; moving WL from 1st to 2nd component and shifting old 2nd 
                                        # to 1st
                                        new_beumc_s1 = beumc_s2
                                        new_eco_unit["1"] = eco_unit["2"]
                                        new_beumc_s2 = "WL"
                                        new_eco_unit["2"] = eco_unit["1"]
                                        new_beumc_s3 = beumc_s3
                                        new_eco_unit["3"] = eco_unit["3"]
                                    elif str(curr_beu_code)[3:4] == "2":
                                        # Old 4th digit was 2; keeping WL in 2nd component
                                        new_beumc_s1 = beumc_s1
                                        new_eco_unit["1"] = eco_unit["1"]
                                        new_beumc_s2 = "WL"
                                        new_eco_unit["2"] = eco_unit["2"]
                                        new_beumc_s3 = beumc_s3
                                        new_eco_unit["3"] = eco_unit["3"]
                                    elif str(curr_beu_code)[3:4] == "3":
                                        # Old 4th digit was 3; moving WL from 3rd to 2nd component and shifting old 2nd 
                                        # to 3rd
                                        new_beumc_s1 = beumc_s1
                                        new_eco_unit["1"] = eco_unit["1"]
                                        new_beumc_s2 = "WL"
                                        new_eco_unit["2"] = eco_unit["3"]
                                        new_beumc_s3 = beumc_s2
                                        new_eco_unit["3"] = eco_unit["2"]
                                elif str(new_beu_code)[3:4] == "3":
                                    # New 4th digit is 3; WL goes in 3rd component
                                    if str(curr_beu_code)[3:4] == "0":
                                        # Old 4th digit was 0; adding WL to 3rd component and removing old 3rd
                                        new_beumc_s1 = beumc_s1
                                        new_eco_unit["1"] = eco_unit["1"]
                                        new_beumc_s2 = beumc_s2
                                        new_eco_unit["2"] = eco_unit["2"]
                                        new_beumc_s3 = "WL"
                                        new_eco_unit["3"] = []
                                        for eco_unit_string_field in eco_unit_string_field_list["3"]:
                                            # When adding a new WL component, also make REALM_# = "W",
                                            # GROUP_# = "W" and KIND_# = "U"
                                            if eco_unit_string_field[:5] in ["REALM", "GROUP"]:
                                                new_eco_unit["3"].append("W")
                                            elif eco_unit_string_field[:4] == "KIND":
                                                new_eco_unit["3"].append("U")
                                            else:
                                                new_eco_unit["3"].append("")
                                        for eco_unit_integer_field in eco_unit_integer_field_list["3"]:
                                            new_eco_unit["3"].append(None)
                                    elif str(curr_beu_code)[3:4] == "1":
                                        # Old 4th digit was 1; moving WL from 1st to 3rd component and shifting 2nd/3rd 
                                        # to 1st/2nd
                                        new_beumc_s1 = beumc_s2
                                        new_eco_unit["1"] = eco_unit["2"]
                                        new_beumc_s2 = beumc_s3
                                        new_eco_unit["2"] = eco_unit["3"]
                                        new_beumc_s3 = "WL"
                                        new_eco_unit["3"] = eco_unit["1"]
                                    elif str(curr_beu_code)[3:4] == "2":
                                        # Old 4th digit was 2; moving WL from 2nd to 3rd component and shifting old 3rd 
                                        # to 2nd
                                        new_beumc_s1 = beumc_s1
                                        new_eco_unit["1"] = eco_unit["1"]
                                        new_beumc_s2 = beumc_s3
                                        new_eco_unit["2"] = eco_unit["3"]
                                        new_beumc_s3 = "WL"
                                        new_eco_unit["3"] = eco_unit["2"]
                                    elif str(curr_beu_code)[3:4] == "3":
                                        # Old 4th digit was 3; keeping WL in 3rd component
                                        new_beumc_s1 = beumc_s1
                                        new_eco_unit["1"] = eco_unit["1"]
                                        new_beumc_s2 = beumc_s2
                                        new_eco_unit["2"] = eco_unit["2"]
                                        new_beumc_s3 = "WL"
                                        new_eco_unit["3"] = eco_unit["3"]

                        else:
                            ## No changes, wetland component decile value was already correct
                            new_beumc_s1 = beumc_s1
                            new_eco_unit["1"] = eco_unit["1"]
                            new_beumc_s2 = beumc_s2
                            new_eco_unit["2"] = eco_unit["2"]
                            new_beumc_s3 = beumc_s3
                            new_eco_unit["3"] = eco_unit["3"]

                        row[req_f.index("Lbl_edit_wl")] += "; Updated BEU: "
                        row[req_f.index("Lbl_edit_wl")] += str(new_sdec_1)
                        row[req_f.index("Lbl_edit_wl")] += " " + str(new_beumc_s1)
                        if new_sdec_2 > 0:
                            row[req_f.index("Lbl_edit_wl")] += ", " + str(new_sdec_2)
                            row[req_f.index("Lbl_edit_wl")] += " " + str(new_beumc_s2)
                            if new_sdec_3 > 0:
                                row[req_f.index("Lbl_edit_wl")] += ", " + str(new_sdec_3)
                                row[req_f.index("Lbl_edit_wl")] += " " + str(new_beumc_s3)
                        # Write the contents of new_sdec_#, new_beumc_s# and new_eco_unit["#"]
                        # to the ecosystem unit fields
                        row[req_f.index("SDEC_1")] = new_sdec_1
                        row[req_f.index("BEUMC_S1")] = new_beumc_s1
                        row[req_f.index("SDEC_2")] = new_sdec_2
                        row[req_f.index("BEUMC_S2")] = new_beumc_s2
                        row[req_f.index("SDEC_3")] = new_sdec_3
                        row[req_f.index("BEUMC_S3")] = new_beumc_s3
                        for dec in ["1", "2", "3"]:
                            count = 0
                            for eco_unit_string_field in eco_unit_string_field_list[dec]:
                                row[req_f.index(eco_unit_string_field)] = new_eco_unit[dec][count]
                                count += 1
                            for eco_unit_integer_field in eco_unit_integer_field_list[dec]:
                                row[req_f.index(eco_unit_integer_field)] = new_eco_unit[dec][count]
                                count += 1
                        row[req_f.index("Lbl_edit_wl")] += " (" + str(new_beu_code) + ")"
                        # print "SDEC_# BEUMC_S# " + str(eco_unit_string_field_list["3"]).replace("3","#") + str(eco_unit_integer_field_list["3"]).replace("3","#")
                        # print "TEID_ID " + str(teis_id)
                        # print "Old 1: " + str(sdec_1) + " " + str(beumc_s1) + " " + str(eco_unit["1"])
                        # print "Old 2: " + str(sdec_2) + " " + str(beumc_s2) + " " + str(eco_unit["2"])
                        # print "Old 3: " + str(sdec_3) + " " + str(beumc_s3) + " " + str(eco_unit["3"])
                        # print "New 1: " + str(new_sdec_1) + " " + str(new_beumc_s1) + " " + str(new_eco_unit["1"])
                        # print "New 2: " + str(new_sdec_2) + " " + str(new_beumc_s2) + " " + str(new_eco_unit["2"])
                        # print "New 3: " + str(new_sdec_3) + " " + str(new_beumc_s3) + " " + str(new_eco_unit["3"])
                        # pdb.set_trace()
                    else:
                        row[req_f.index("Lbl_edit_wl")] += "; *** INVALID CODE"
                else:
                    row[req_f.index("Lbl_edit_wl")] += "; No update needed."

                cursor.updateRow(row)

            if row_count % 100000 == 0 or row_count == total_count:
                logging.info("    - Processed " + str(row_count) + " of " + str(total_count) + " rows")

    # ---------------------------------------------------------------------------------------------------------
    # For polygons that have Mean_Slope less than 10, and the floodplain modifier 'a' assigned, and BEUMC_1 that 
    # is not unvegetated, update to SDEC = 10 and BEUMC_S1 = riparian map code that corresponds with its BGC_Zone.
    # ---------------------------------------------------------------------------------------------------------

    non_veg = ["RI", "WL", "BB", "UR", "OW", "LS", "LL", "RE", "CL", "GB", "GL", "GP", "MI", "RO", "TA", "TC", "TR", 
               "UV", "BG", "CB", "FE", "MR", "PB", "RS", "SH", "SK", "SW", "WG", "TF", "YB", "YS", "AU", "AV", "ES", 
               "IM", "ME", "OV", "RM", "SC", "SM", "ST"]
    riparian_mapcode_dict = {}
    riparian_mapcode_dict["CDF"] = "CR"
    riparian_mapcode_dict["BWBS"] = "PR"
    riparian_mapcode_dict["SWB"] = "PR"
    riparian_mapcode_dict["ESSF"] = "ER"
    riparian_mapcode_dict["ICH"] = "RR"
    riparian_mapcode_dict["CWH"] = "SR"
    riparian_mapcode_dict["SBPS"] = "WR"
    riparian_mapcode_dict["SBS"] = "WR"

    logging.info("Updating floodplain polygons to riparian mapcode appropriate for BGC_ZONE")
    row_count = 0
    update_count = 0
    with arcpy.da.UpdateCursor(bem_fc, req_f) as cursor:
        for row in cursor:
            row_count += 1
            if site_m3a_is_a_dict[row[req_f.index("TEIS_ID")]]:
                if row[req_f.index("MEAN_SLOPE")] < 10:
                    if row[req_f.index("BEUMC_S1")] not in non_veg:
                        if row[req_f.index("BGC_ZONE")] in riparian_mapcode_dict.keys():
                            row[req_f.index("SDEC_1")] = 10
                            row[req_f.index("SDEC_2")] = 0
                            row[req_f.index("SDEC_3")] = 0
                            row[req_f.index("BEUMC_S1")] = riparian_mapcode_dict[row[req_f.index("BGC_ZONE")]]
                            row[req_f.index("BEUMC_S2")] = ""
                            row[req_f.index("BEUMC_S3")] = ""
                            for x in ["2", "3"]:
                                for field in eco_unit_string_field_list[x]:
                                    row[req_f.index(field)] = ""
                                for field in eco_unit_integer_field_list[x]:
                                        row[req_f.index(field)] = None
                            row[req_f.index("Lbl_edit_wl")] = "Updated to 10 " \
                                    + riparian_mapcode_dict[row[req_f.index("BGC_ZONE")]] + " because SITE_M3A = 'a'," \
                                    + " Slope < 10, and BGC_ZONE = '" + row[req_f.index("BGC_ZONE")] + "'."
                            update_count += 1
                row[req_f.index("SITE_M3A")] = "a"
                cursor.updateRow(row)
            if row_count % 100000 == 0 or row_count == total_count:
                logging.info("    - Processed " + str(row_count) + " of " + str(total_count) + " rows; "
                             "updated " + str(update_count) + " rows.")

    # ---------------------------------------------------------------------------------------------------------
    # Done
    # ---------------------------------------------------------------------------------------------------------

    dtCalcNow = time.time()
    dtCalcScriptElapsed = dtCalcNow - dtCalcScriptStart
    logging.info("Script complete after " + SanitizeElapsedTime(dtCalcScriptElapsed))

if __name__ == '__main__':
    try:
        # Parse arguments
        parser = ArgumentParser(description='This script updates BEM attributes based on VRI attributes, '
                                            'according to the document "Corrections to the BEM map codes". ',
                                formatter_class=RawTextHelpFormatter)
        parser.add_argument('bfc', help='BEM polygon feature class')
        parser.add_argument('wfc', help='Wetlands polygon feature class')
        parser.add_argument('buc', help='BEU update table CSV file')
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
        main(args.bfc, args.wfc, args.buc)

    except Exception as e:
        logging.exception('Unexpected exception. Program terminating.')
else:
    import arcpy
