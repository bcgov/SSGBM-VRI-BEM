"""

Original Author:
Madrone (Jeff Kruys)

Created on:
2020-02-20

Purpose:
This script updates BEM attributes based on VRI attributes, 
according to the document "Corrections to the BEM map codes".

Usage:
BEM_Corrections.py ifc rfc csv [-h] [-l] [-ld]

Positional Arguments:
   ifc              Input polygon feature class
   rfc              Rivers polygon feature class [FWA_Rivers[poly]]
   csv              CSV of allowed BEC and BEM Code Combos

Optional Arguments:
  -h, --help       show this help message and exit
  -l, --level      log level messages to display; Default: 20 - INFO
  -ld, --log_dir   path to directory for output log file

Example Input:
X:\fullpath\BEM_Corrections.py Y:\fullpath\ifc Z:\fullpath\rfc W:\fullpath\csv

History
2020-02-20 (JK): Created script BEM_Corrections.
2020-05-09 (JK): Added section to validate subzone/mapcode combinations
                 from a supplied CSV file.
2020-07-30 (AC): Testing
2020-08-07 (AC): Testing BWBS
2020-08-31 (JK): Updated new section from May 9 2020 to report invalid and 
                 missing subzone/mapcode combinations in Lbl_edit field
                 (not just the corrections that were made).
2020-09-03 (JK): Updated new section from May 9 2020 to create output CSV
                 file with list of subzone/mapcode combos found in data
                 but missing from input CSV file
2020-09-14 (AJ): Updated to remore requirment of Static_Sb_Bog as this is not added until the HEM process (if even 
                 required)
2020-09-18 (AJ): Updated to correct for blanket 10 WL edits
2020-09-22 (AJ): Edits to include CL - cliff based on steep slopes, re-arranged order of BB - bog corrections - after 
                 WL corrections, LS, OW adjustments (<=2 ha)
2020-11-26 (AE): Added the remove duplicate lables to the bottom of the script and left the one at the top. 
2021-01-27 (JK): Added new code to:
                 - Prompt user for whether to clear all SITE_M#A fields
                 - Clear Lbl_edit, SITE_M#A fields and recalculate Area_Ha and DEC_Total fields
                 - Update BEUMC_1 to 'UV' based on LAND_COVER_CLASS_CD_1 and EST_COVER_PCT_1 values
                 - Update STAND_A1 based on SPECIES_CD_1, SPECIES_PCT_1 and existing STAND_A1 values
2021-03-23 (JK): Added new code to add new field SMPL_TYPE if it doesn't already exist, and to skip processing any
                 record if the SMPL_TYPE field contains any non-null value.
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


def main(input_fc, rivers_fc, bec_beu_csv):
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
    # Prompt user for whether they want to clear existing values in SITE_M#A fields
    # ---------------------------------------------------------------------------------------------------------

    clear_site_ma = ""
    while clear_site_ma not in ["Y", "N", "y", "n"]:
        clear_site_ma = raw_input("Do you wish to clear the existing values in SITE_M1A, SITE_M2A and SITE_M3A? "
                                  "(Y/N): ")
    if clear_site_ma in ["Y", "y"]:
        logging.info("Clearing all fields SITE_M1A, SITE_M2A and SITE_M3A.")
    else:
        logging.info("Clearing only SITE_M3A.")

    # ---------------------------------------------------------------------------------------------------------
    # Check that input feature class exists and contains the required fields
    # ---------------------------------------------------------------------------------------------------------

    if not arcpy.Exists(input_fc):
        logging.error("**** Specified input feature class " + input_fc + " does not exist. Exiting script.")
        sys.exit()
    if not arcpy.Exists(rivers_fc):
        logging.error("**** Specified rivers feature class " + rivers_fc + " does not exist. Exiting script.")
        sys.exit()
    required_fields = ["SDEC_1", "BEUMC_S1", "REALM_1", "GROUP_1", "CLASS_1", "KIND_1", "SITE_S1", "SITEAM_S1A", 
                       "SITEAM_S1B", "SITEAM_S1C", "SITEAM_S1D", "SITEMC_S1", "SITE_M1A", "SITE_M1B", "STRCT_S1", 
                       "STRCT_M1", "STAND_A1", "SERAL_1", "TREE_C1", "SHRUB_C1", "DISTCLS_1", "DISTSCLS_1", 
                       "DISSSCLS_1", "SECL_1", "SESUBCL_1", "COND_1", "VIAB_1", "SDEC_2", "BEUMC_S2", "REALM_2", 
                       "GROUP_2", "CLASS_2", "KIND_2", "SITE_S2", "SITEAM_S2A", "SITEAM_S2B", "SITEAM_S2C", 
                       "SITEAM_S2D", "SITEMC_S2", "SITE_M2A", "SITE_M2B", "STRCT_S2", "STRCT_M2", "STAND_A2", 
                       "SERAL_2", "TREE_C2", "SHRUB_C2", "DISTCLS_2", "DISTSCLS_2", "DISSSCLS_2", "SECL_2", 
                       "SESUBCL_2", "COND_2", "VIAB_2", "SDEC_3", "BEUMC_S3", "REALM_3", "GROUP_3", "CLASS_3", 
                       "KIND_3", "SITE_S3", "SITEAM_S3A", "SITEAM_S3B", "SITEAM_S3C", "SITEAM_S3D", "SITEMC_S3", 
                       "SITE_M3A", "SITE_M3B", "STRCT_S3", "STRCT_M3", "STAND_A3", "SERAL_3", "TREE_C3", "SHRUB_C3", 
                       "DISTCLS_3", "DISTSCLS_3", "DISSSCLS_3", "SECL_3", "SESUBCL_3", "COND_3", "VIAB_3", "SLOPE_MOD",
                       "FORESTED_1", "FORESTED_2", "FORESTED_3", "BCLCS_LEVEL_1", "BCLCS_LEVEL_2", "BCLCS_LEVEL_3", 
                       "BCLCS_LEVEL_4", "BCLCS_LEVEL_5", "SPECIES_CD_1", "AGE_CL_STS", "LAND_COVER_CLASS_CD_1", 
                       "EST_COVERAGE_PCT_1", "LINE_5_VEGETATION_COVER", "Area_Ha", "BGC_ZONE", "BGC_SUBZON", 
                       "SPECIES_PCT_1"]
    existing_fields = [f.name for f in arcpy.ListFields(input_fc) if not f.required]
    missing_fields = []
    for required_field in required_fields:
        if required_field not in existing_fields:
            missing_fields.append(required_field)
    if len(missing_fields) > 0:
        logging.error("**** Input feature class is missing the following required fields: " + str(missing_fields))
        sys.exit()

    if "Lbl_edit" not in existing_fields:
        logging.info("Adding new field 'Lbl_edit'")
        arcpy.AddField_management(input_fc, "Lbl_edit", "TEXT", "#", "#", "1000")
    required_fields.append("Lbl_edit")

    if "DEC_Total" not in existing_fields:
        logging.info("Adding new field 'DEC_Total'")
        arcpy.AddField_management(input_fc, "DEC_Total", "SHORT")
    required_fields.append("DEC_Total")

    # ---------------------------------------------------------------------------------------------------------
    # Read the CSV table of acceptable/unacceptable combinations of BEC unit and BEU mapcodes
    # ---------------------------------------------------------------------------------------------------------

    bec_beu_allowed_dict = {}
    bec_beu_error_dict = {}
    data_counter = 0
    bec_beu_csv_object = open(bec_beu_csv, "rU")
    csvReader = csv.DictReader(bec_beu_csv_object)
    csv_fields_checked = False
    for data in csvReader:
        if not csv_fields_checked:
            try:
                w = data["BGC Subzone"]
                x = data["BEU_#"]
                y = data["Script rule"]
                z = data["Change to BEU ="]
            except:
                logging.error("Specified CSV file missing required field(s) 'BGC Subzone', 'BEU_#', 'Script Rule'")
                logging.error("Exiting script.")
                sys.exit()
            csv_fields_checked = True

        data_counter += 1
        if data["Script rule"] == "Error":
            try:
                bec_beu_error_dict[data["BGC Subzone"]][data["BEU_#"]] = data["Change to BEU ="]
            except:
                bec_beu_error_dict[data["BGC Subzone"]] = {}
                bec_beu_error_dict[data["BGC Subzone"]][data["BEU_#"]] = data["Change to BEU ="]
        else:
            try:
                bec_beu_allowed_dict[data["BGC Subzone"]].append(data["BEU_#"])
            except:
                bec_beu_allowed_dict[data["BGC Subzone"]] = [data["BEU_#"]]

    del csvReader

    # ---------------------------------------------------------------------------------------------------------
    # Perform corrections, record by record
    # ---------------------------------------------------------------------------------------------------------

    if clear_site_ma in ["N", "n"]:
        logging.info("Recalculating 'Lbl_edit', 'Site_M3A' and 'Area_Ha' fields")
        with arcpy.da.UpdateCursor(input_fc, ["Lbl_edit", "Area_Ha", "Site_M3A", "SHAPE@AREA"]) as cursor:
            for row in cursor:
                row[0] = ""
                row[1] = round(row[3] / 10000, 2)
                row[2] = ""
                cursor.updateRow(row)
    elif clear_site_ma in ["Y", "y"]:
        logging.info("Recalculating 'Lbl_edit', 'Site_M1A', 'Site_M2A', 'Site_M3A' and 'Area_Ha' fields")
        with arcpy.da.UpdateCursor(input_fc, ["Lbl_edit", "Area_Ha", "Site_M1A", "Site_M2A", "Site_M3A", 
                                              "SHAPE@AREA"]) as cursor:
            for row in cursor:
                row[0] = ""
                row[1] = round(row[5] / 10000, 2)
                row[2] = ""
                row[3] = ""
                row[4] = ""
                cursor.updateRow(row)

    if "SMPL_TYPE" not in [f.name for f in arcpy.ListFields(input_fc)]:
        logging.info("Adding field SMPL_TYPE to input feature class")
        arcpy.AddField_management(input_fc, "SMPL_TYPE", "TEXT", "#", "#", 1, "Field Check of Polygon")
    required_fields.append("SMPL_TYPE")

    logging.info("Updating attribute table with corrections")
    row_count = 0
    update_count = 0
    total_count = int(arcpy.GetCount_management(input_fc).getOutput(0))
    cf = required_fields
    unlisted_combos = []
    with arcpy.da.UpdateCursor(input_fc, cf) as cursor:
        for row in cursor:

            row_count += 1
            if row[cf.index("SMPL_TYPE")] in ["", "None", None]:
                row_updated = False
                blank_remaining_eco_fields = False
                preserve_strct_s1_field = False
                correction_string = ''

                eco_fields = ["BEUMC_S1", "REALM_1", "GROUP_1", "CLASS_1", "KIND_1", "SITE_S1", "SITEAM_S1A", 
                              "SITEAM_S1B", "SITEAM_S1C", "SITEAM_S1D", "SITEMC_S1", "SITE_M1A", "SITEAM_S1D", 
                              "SITEMC_S1", "SITE_M1A", "SITE_M1B", "STRCT_S1", "STRCT_M1", "STAND_A1", "SERAL_1", 
                              "TREE_C1", "SHRUB_C1", "DISTCLS_1", "DISTSCLS_1", "DISSSCLS_1", "SECL_1", 
                              "SESUBCL_1", "COND_1", "VIAB_1", "FORESTED_1"]

                # Remove duplicate labels (in BEM may have had two of the same forested unit; one associated with one set 
                # of site conditions and the other representing different conditions).  Site modifiers are NOT updated in 
                # this product.  Therefore, duplicate labels were removed where the following query applied:
                # Where (BEUMC_S1 =  BEUMC_S2) combine as one decile 

                if row[cf.index("BEUMC_S1")] == row[cf.index("BEUMC_S2")]:
                    if row[cf.index("SDEC_1")] > 0 and row[cf.index("SDEC_2")] > 0: 
                        row[cf.index("SDEC_1")] = row[cf.index("SDEC_1")] + row[cf.index("SDEC_2")]
                    row[cf.index("SDEC_2")] = row[cf.index("SDEC_3")]
                    row[cf.index("SDEC_3")] = None
                    for eco_field in eco_fields:
                        row[cf.index(eco_field.replace("1", "2"))] = row[cf.index(eco_field.replace("1", "3"))]
                        if eco_field[:4] in ["TREE", "SHRU"]:
                            row[cf.index(eco_field.replace("1", "3"))] = None
                        else:
                            row[cf.index(eco_field.replace("1", "3"))] = ""
                    row_updated = True
                    correction_string = "Combined components 1 and 2 with same BEUMC_S# code into single component 1"

                # BEU_MC = OW 
                # where BCLCS_LEVEL_1 = 'N' AND BCLCS_LEVEL_5 = 'LA' AND Area_Ha <=10
                # -shallow open water typically associated with floating vegetation
                # -For LIW for moose, LS is rated 0.05, and OW is rated 0.25 because of its common association with a 
                # shrub fringe

                elif row[cf.index("BCLCS_LEVEL_1")] == 'N' and row[cf.index("BCLCS_LEVEL_5")] == 'LA' and \
                            row[cf.index("Area_Ha")] <= 2:
                    row[cf.index("SDEC_1")] = 10
                    row[cf.index("BEUMC_S1")] = "OW"
                    blank_remaining_eco_fields = True
                    row_updated = True
                    correction_string = "Updated to 10 OW because BCLCS_LEVEL_1 = 'N', BCLCS_LEVEL_5 = 'LA', Area <= 10 ha"

                # BEU_MC = LS 
                # where BCLCS_LEVEL_1 = 'N' AND BCLCS_LEVEL_5 = 'LA' AND Area_Ha >10 ha AND Area_Ha <=60 
                # -LA (>10 ha and < or equal to 60 ha) = LS - small lake 

                elif row[cf.index("BCLCS_LEVEL_1")] == 'N' and row[cf.index("BCLCS_LEVEL_5")] == 'LA' and \
                            2 < row[cf.index("Area_Ha")] <= 60:
                    row[cf.index("SDEC_1")] = 10
                    row[cf.index("BEUMC_S1")] = "LS"
                    blank_remaining_eco_fields = True
                    row_updated = True
                    correction_string = "Updated to 10 LS because BCLCS_LEVEL_1 = 'N', BCLCS_LEVEL_5 = 'LA', Area <= 60 ha"

                # BEU_MC = LL 
                # where BCLCS_LEVEL_1 = 'N' AND BCLCS_LEVEL_5 = 'LA' AND Area_Ha >60 ha 
                # -LA (>60 ha) = BEM code LL (large lake)

                elif row[cf.index("BCLCS_LEVEL_1")] == 'N' and row[cf.index("BCLCS_LEVEL_5")] == 'LA' and \
                            row[cf.index("Area_Ha")] > 60:
                    row[cf.index("SDEC_1")] = 10
                    row[cf.index("BEUMC_S1")] = "LL"
                    blank_remaining_eco_fields = True
                    row_updated = True
                    correction_string = "Updated to 10 LL because BCLCS_LEVEL_1 = 'N', BCLCS_LEVEL_5 = 'LA', Area > 60 ha"

                # BEU_MC = RE
                # where BCLCS_LEVEL_1 = 'N' AND BCLCS_LEVEL_5 = 'RE'
                # -RE = BEM code for reservoir

                elif row[cf.index("BCLCS_LEVEL_1")] == 'N' and row[cf.index("BCLCS_LEVEL_5")] == 'RE':
                    row[cf.index("SDEC_1")] = 10
                    row[cf.index("BEUMC_S1")] = "RE"
                    blank_remaining_eco_fields = True
                    row_updated = True
                    correction_string = "Updated to 10 RE because BCLCS_LEVEL_1 = 'N', BCLCS_LEVEL_5 = 'RE'"

                # Rivers
                # BEU_MC = FP 
                # where BCLCS_LEVEL_1 = 'N' AND (BCLCS_LEVEL_5 = 'RI' OR BCLCS_LEVEL_5 = 'RS')
                # There are two VRI codes (labels) that apply to rivers; river and river sediments:
                # -RI = River
                # -RS = River Sediments
                # The default applied was to assign 'FP' (Fast Perennial Stream) to BEU_MC where rivers were identified by 
                # this query.

                elif row[cf.index("BCLCS_LEVEL_1")] == 'N' and row[cf.index("BCLCS_LEVEL_5")] in ['RI', 'RS']:
                    row[cf.index("SDEC_1")] = 10
                    row[cf.index("BEUMC_S1")] = "RI"
                    blank_remaining_eco_fields = True
                    row_updated = True
                    correction_string = "Updated to 10 RI because BCLCS_LEVEL_1 = 'N', BCLCS_LEVEL_5 = 'RI' or 'RS'"

                # BEU_MC = WL where BCLCS_LEVEL_4 = 'ST' OR BCLCS_LEVEL_4 = 'SL' OR BCLCS_LEVEL_4 = 'HE' OR 
                # BCLCS_LEVEL_4 = 'HF' OR BCLCS_LEVEL_4 = 'HG'
                # 3b for ST, 3a for SL, 2b for HE  
                # Other BEM wetland codes that could be applied following photo or field confirmation include:  
                # BG = Sphagnum bog; FE = Sedge fen; SC = Shrub-carr; SW = shrub swamp; ME = meadow; MR = marsh; 
                # MS = montane shrub/grassland; SH = Shrub fen; SK = spruce-swamp

    #           elif row[cf.index("BCLCS_LEVEL_4")] in ['ST', 'SL', 'HE', 'HF', 'HG']:
    #              row[cf.index("SDEC_1")] = 10
    #              row[cf.index("BEUMC_S1")] = "WL"
    #                if row[cf.index("BCLCS_LEVEL_4")] == 'ST':
    #                    row[cf.index("STRCT_S1")] = '3b'
    #                    preserve_strct_s1_field = True
    #                    correction_string = "Updated to 10 WL, STRCT_S1 3b because BCLCS_LEVEL_4 = 'ST'"
    #                elif row[cf.index("BCLCS_LEVEL_4")] == 'SL':
    #                    row[cf.index("STRCT_S1")] = '3a'
    #                    preserve_strct_s1_field = True
    #                    correction_string = "Updated to 10 WL, STRCT_S1 3a because BCLCS_LEVEL_4 = 'SL'"
    #                elif row[cf.index("BCLCS_LEVEL_4")] == 'HE':
    #                    row[cf.index("STRCT_S1")] = '2b'
    #                    preserve_strct_s1_field = True
    #                    correction_string = "Updated to 10 WL, STRCT_S1 2b because BCLCS_LEVEL_4 = 'HE'"
    #                else:
    #                    correction_string = "Updated to 10 WL because BCLCS_LEVEL_4 = 'HF' or 'HG'"
    #                blank_remaining_eco_fields = True
    #                row_updated = True

                # BEU_MC = WL where (BCLCS_LEVEL_1 = 'V' AND BCLCS_LEVEL_2 ='N' AND BCLCS_LEVEL_3 = 'W' AND AGE_CL_STS =-1)

                elif row[cf.index("BCLCS_LEVEL_1")] == 'V' and row[cf.index("BCLCS_LEVEL_2")] == 'N' and \
                            row[cf.index("BCLCS_LEVEL_3")] == 'W' and row[cf.index("AGE_CL_STS")] == -1:
                    row[cf.index("SDEC_1")] = 10
                    row[cf.index("BEUMC_S1")] = "WL"
                    blank_remaining_eco_fields = True
                    correction_string = "Updated to 10 WL because BCLCS_LEVEL_1/2/3 = 'V'/'N'/'W' and AGE_CL_STS = -1"
                    row_updated = True

                # Forested - NOT WL
                # where (BCLCS_LEVEL_4 = 'TB' OR BCLCS_LEVEL_4 = 'TC' OR BCLCS_LEVEL_4 = 'TM') AND (BEU_MC = WL)
                # Where the following query results = should not contain a wetland (WL) label component.  
                # Remove WL decile component and update value in Decile 

                elif row[cf.index("BCLCS_LEVEL_4")] in ['TB', 'TC', 'TM']:
                    # Remove any component that has WL (wetland) code.
                    # Assumption here is that only one of the three components is WL. If 2 or 3 components
                    # are WL, then this script will only remove the one that's in the largest component.
                    if row[cf.index("BEUMC_S1")] == 'WL':
                        if row[cf.index("SDEC_2")] > 0:
                            row[cf.index("SDEC_1")] += row[cf.index("SDEC_2")]
                            row[cf.index("SDEC_2")] = row[cf.index("SDEC_3")]
                            row[cf.index("SDEC_3")] = None
                            # move component 2 to component 1, move component 3 to 2, and make 3 blank
                            for eco_field in eco_fields:
                                row[cf.index(eco_field)] = row[cf.index(eco_field.replace("1", "2"))]
                                row[cf.index(eco_field.replace("1", "2"))] = row[cf.index(eco_field.replace("1", "3"))]
                                if eco_field[:4] in ["TREE", "SHRU"]:
                                    row[cf.index(eco_field.replace("1", "3"))] = None
                                else:
                                    row[cf.index(eco_field.replace("1", "3"))] = ""
                            correction_string = "Removed WL in component 1 because BCLCS_LEVEL_4 = 'TB', 'TC' or 'TM'"
                            row_updated = True
                        else:
                            # component 1 was a 10 WL, so just blank it out
                            row_updated = True
                            correction_string = "**** Warning: Polygon is pure WL, but BCLCS_LEVEL_4 = 'TB', 'TC' or 'TM'"

                    elif row[cf.index("BEUMC_S2")] == 'WL':
                        if row[cf.index("SDEC_3")] > 0:
                            # move component 3 to component 2, and make 3 blank
                            row[cf.index("SDEC_2")] += row[cf.index("SDEC_3")]
                            row[cf.index("SDEC_3")] = None
                            for eco_field in eco_fields:
                                row[cf.index(eco_field.replace("1", "2"))] = row[cf.index(eco_field.replace("1", "3"))]
                                if eco_field[:4] in ["TREE", "SHRU"]:
                                    row[cf.index(eco_field.replace("1", "3"))] = None
                                else:
                                    row[cf.index(eco_field.replace("1", "3"))] = ""
                            row_updated = True
                            correction_string = "Removed WL in component 2 because BCLCS_LEVEL_4 = 'TB', 'TC' or 'TM'"
                        else:
                            # add sdec_2 value to sdec_1, and make all of component 2 blank
                            row[cf.index("SDEC_1")] += row[cf.index("SDEC_2")]
                            row[cf.index("SDEC_2")] = None
                            for eco_field in eco_fields:
                                if eco_field[:4] in ["TREE", "SHRU"]:
                                    row[cf.index(eco_field.replace("1", "2"))] = None
                                else:
                                    row[cf.index(eco_field.replace("1", "2"))] = ""
                            row_updated = True
                            correction_string = "Removed WL in component 2 because BCLCS_LEVEL_4 = 'TB', 'TC' or 'TM'"

                    elif row[cf.index("BEUMC_S3")] == 'WL':
                        row[cf.index("SDEC_2")] += row[cf.index("SDEC_3")]
                        row[cf.index("SDEC_3")] = None
                        for eco_field in eco_fields:
                            if eco_field[:4] in ["TREE", "SHRU"]:
                                row[cf.index(eco_field.replace("1", "3"))] = None
                            else:
                                row[cf.index(eco_field.replace("1", "3"))] = ""
                        row_updated = True
                        correction_string = "Removed WL in component 3 because BCLCS_LEVEL_4 = 'TB', 'TC' or 'TM'"

                # Wetlands
                # Black Spruce Bogs
                # BEU_MC = BB
                # Results of VRI queries completed in Step 2 were applied to update BEM labels.
                # Query of SPECIES_CD_1 = 'SB'
                # - Checks that leading tree species is SB: SPECIES_CD_1 = 'SB'
                # Query of other wetland forage identified assigned "WL" a generic BEM and PEM wetland code.

                elif row[cf.index("SPECIES_CD_1")] == 'SB' and row[cf.index("SPECIES_PCT_1")] >= 90:
                    row[cf.index("SDEC_1")] = 10
                    row[cf.index("BEUMC_S1")] = "BB"
                    blank_remaining_eco_fields = True
                    row_updated = True
                    correction_string = "Updated to 10 BB because SPECIES_CD_1 = 'SB'"
                    
                # Anthropogenic and Non- vegetated

                elif row[cf.index("BCLCS_LEVEL_5")] == "AP":
                    row[cf.index("SDEC_1")] = 10
                    row[cf.index("BEUMC_S1")] = "UR"
                    blank_remaining_eco_fields = True
                    row_updated = True
                    correction_string = "Updated to 10 UR because BCLCS_LEVEL_5 = 'AP'"

                elif row[cf.index("BCLCS_LEVEL_5")] == "BU":
                    row[cf.index("DISTCLS_1")] = "F"
                    row_updated = True
                    correction_string = "Updated to DISTCLS_1 'F' because BCLCS_LEVEL_5 = 'BU'"
                    
    # if slope mod is a q or z = cliff
                elif row[cf.index("SLOPE_MOD")] in ["q", "z"]:
                    row[cf.index("SDEC_1")] = 10
                    row[cf.index("BEUMC_S1")] = "CL"
                    blank_remaining_eco_fields = True
                    row_updated = True
                    correction_string = "Updated to 10 CL because Slope Mod is q or z"

    # TO BE ADDED
    # if  If Site Mod 3A = "a" and STS_AGE_CL >= 1 then assign 10 PR, ER OR WR as per which
    # BEC zone those units are allowed (ER can only go in the ESSF, etc.).  In common language
    # I'm saying that forested units adjacent to floodplains should be one of these riparian
    # forest types as the dominant forest ecosystem.                 

                    
                elif row[cf.index("BCLCS_LEVEL_5")] == "GB":
                    row[cf.index("SDEC_1")] = 10
                    row[cf.index("BEUMC_S1")] = "GB"
                    blank_remaining_eco_fields = True
                    row_updated = True
                    correction_string = "Updated to 10 GB because BCLCS_LEVEL_5 = 'GB'"

                elif row[cf.index("BCLCS_LEVEL_5")] in ["GL", "PN"]:
                    row[cf.index("SDEC_1")] = 10
                    row[cf.index("BEUMC_S1")] = "GL"
                    blank_remaining_eco_fields = True
                    row_updated = True
                    correction_string = "Updated to 10 GL because BCLCS_LEVEL_5 = 'GL' or 'PN'"

                elif row[cf.index("BCLCS_LEVEL_5")] == "GP":
                    row[cf.index("SDEC_1")] = 10
                    row[cf.index("BEUMC_S1")] = "GP"
                    blank_remaining_eco_fields = True
                    row_updated = True
                    correction_string = "Updated to 10 GP because BCLCS_LEVEL_5 = 'GP'"

                elif row[cf.index("BCLCS_LEVEL_5")] == "LL":
                    row[cf.index("SDEC_1")] = 10
                    row[cf.index("BEUMC_S1")] = "LL"
                    blank_remaining_eco_fields = True
                    row_updated = True
                    correction_string = "Updated to 10 LL because BCLCS_LEVEL_5 = 'LL'"

                elif row[cf.index("BCLCS_LEVEL_5")] in ["MI", "TZ", "MZ"]:
                    row[cf.index("SDEC_1")] = 10
                    row[cf.index("BEUMC_S1")] = "MI"
                    blank_remaining_eco_fields = True
                    row_updated = True
                    correction_string = "Updated to 10 MI because BCLCS_LEVEL_5 = 'MI', 'TZ' or 'MZ'"

                elif row[cf.index("BCLCS_LEVEL_5")] in ["RO", "BR", "BI"]:
                    row[cf.index("SDEC_1")] = 10
                    row[cf.index("BEUMC_S1")] = "RO"
                    blank_remaining_eco_fields = True
                    row_updated = True
                    correction_string = "Updated to 10 RO because BCLCS_LEVEL_5 = 'RO', 'BR' or 'BI'"

                elif row[cf.index("BCLCS_LEVEL_5")] == "TA":
                    row[cf.index("SDEC_1")] = 10
                    row[cf.index("BEUMC_S1")] = "TA"
                    blank_remaining_eco_fields = True
                    row_updated = True
                    correction_string = "Updated to 10 TA because BCLCS_LEVEL_5 = 'TA'"

                elif row[cf.index("BCLCS_LEVEL_5")] in ["TC", "RN", "RZ"]:
                    row[cf.index("SDEC_1")] = 10
                    row[cf.index("BEUMC_S1")] = "TC"
                    blank_remaining_eco_fields = True
                    row_updated = True
                    correction_string = "Updated to 10 TC because BCLCS_LEVEL_5 = 'TC', 'RN' or 'RZ'"

                elif row[cf.index("BCLCS_LEVEL_5")] == "TR":
                    row[cf.index("SDEC_1")] = 10
                    row[cf.index("BEUMC_S1")] = "TR"
                    blank_remaining_eco_fields = True
                    row_updated = True
                    correction_string = "Updated to 10 TR because BCLCS_LEVEL_5 = 'TR'"

                elif row[cf.index("BCLCS_LEVEL_5")] in ["UV", "RS", "MU", "ES", "CB", "MN", "RM"]:
                    row[cf.index("SDEC_1")] = 10
                    row[cf.index("BEUMC_S1")] = "UV"
                    blank_remaining_eco_fields = True
                    row_updated = True
                    correction_string = "Updated to 10 UV because BCLCS_LEVEL_5 = 'UV', 'RS', 'MU', 'ES', 'CB', 'MN' " \
                                        + "or 'RM'"

                elif row[cf.index("LAND_COVER_CLASS_CD_1")] in ["UV", "RS", "MU", "ES", "CB", "MN", "RM"] \
                        and row[cf.index("EST_COVERAGE_PCT_1")] >= 95:
                    row[cf.index("SDEC_1")] = 10
                    row[cf.index("BEUMC_S1")] = "UV"
                    blank_remaining_eco_fields = True
                    row_updated = True
                    correction_string = "Updated to 10 UV because LAND_COVER_CLASS_CD_1 = 'UV', 'RS', 'MU', 'ES', 'CB', " \
                                        + "'MN' or 'RM' and EST_COVERAGE_PCT_1 >= 95"

                elif row[cf.index("BCLCS_LEVEL_5")] == "UR":
                    row[cf.index("SDEC_1")] = 10
                    row[cf.index("BEUMC_S1")] = "UR"
                    blank_remaining_eco_fields = True
                    row_updated = True
                    correction_string = "Updated to 10 UR because BCLCS_LEVEL_5 = 'UR'"

                # CF  
                # Identify Cultivated Field (CF) labels for BEM via VRI.  Cultivated field (CF) is a label assigned to 
                # many areas that may actually be forested broadleaf/hardwood Query = Tree species 1 % >98% and 
                # BCLCS level 4 - TB (treed-broadleaf). If VRI indicates a leading tree species and projected age, the 
                # site is kept as a forested site.
                #
                # (BEUMC_S1 ='CF' AND VRI_AGE_CL_STS >-1) OR
                # (BEUMC_S2 ='CF' AND VRI_AGE_CL_STS >-1)
                # VRI_AGE_CL_STS = -1 AND (LINE_5_VEGETATION_COVER ='C' OR LINE_5_VEGETATION_COVER = 'OR')
                # BEUMC_S2 ='CF' AND BCLCS_LEVEL_2 ='T' AND VRI_AGE_CL_STS >=2
                # BEUMC_S2 ='CF' AND BCLCS_LEVEL_2 ='N' AND VRI_AGE_CL_STS >=2

                # --------------------------------------------------------------------------------------------------------
                # ******* NEED FURTHER EXPLANATION OF THE ABOVE *******
                # --------------------------------------------------------------------------------------------------------

                # Roads in VRI Veg "rz" component (leading factor is road in the below cases)
                # For polygons that are indicated as "NOT Treed" (N), the following cases had their BEM labels updated 
                # to "TC" (transportation corridor).  A number of these sites were identified as Static Upland shrub 
                # forage potential.  They will be removed from the effective habitat model when the road buffers/layer is 
                # applied.

                elif row[cf.index("BCLCS_LEVEL_2")] == "T" and row[cf.index("LINE_5_VEGETATION_COVER")] \
                        in ['rz', 'rz,by', 'rz,by,he', 'rz,by,he,sl', 'rz,by,sl', 'rz,by,sl,he', 'rz,by,st', 'rz,he', 
                            'rz,by,sl,he', 'rz,by,st', 'rz,he', 'rz,he,by', 'rz,he,by,sl', 'rz,he,sl', 'rz,he,sl,by', 
                            'rz,he,st', 'rz,he,st,by', 'rz,hf,by', 'rz,hf,sl,by', 'rz,hg', 'rz,hg,sl', 'rz,sl', 
                            'rz,sl,by', 'rz,sl,by,he', 'rz,sl,he', 'rz,sl,he,by', 'rz,sl,hf', 'rz,sl,hf,by', 'rz,sl,hg', 
                            'rz,st', 'rz,st,he', 'rz,st,he,by', 'rz,st,hf', 'rz,st,hg']:
                    if row[cf.index("SDEC_1")] == 10:
                        row[cf.index("SDEC_1")] = 8
                        row[cf.index("SDEC_2")] = 2
                        row[cf.index("BEUMC_S2")] = "TC"
                        row_updated = True
                        correction_string = "Added 2nd component 2 TC because BCLCS_LEVEL_2 = 'T' and " + \
                                            "LINE_5_VEGETATION_COVER begins with 'rz'"

                # Update STAND_A1 based on SPECIES_CD_1 and SPECIES_PCT_1 and existing STAND_A1
                if row[cf.index("SPECIES_CD_1")] in ["AC", "ACB", "ACT", "AT", "EP"]:
                    if row[cf.index("SPECIES_PCT_1")] >= 75 and row[cf.index("STAND_A1")] in ["C", "M"]:
                        row[cf.index("STAND_A1")] = "B"
                        row_updated = True
                        correction_string_add = "Updated STAND_A1 to 'B' because SPECIES_CD_1 = '" + row[
                                cf.index("SPECIES_CD_1")] + "' and SPECIES_PCT_1 >= 75 and STAND_A1 was 'C' or 'M'"
                        if correction_string == "":
                            correction_string = correction_string_add
                        else:
                            correction_string += "; " + correction_string_add
                    elif 50 <= row[cf.index("SPECIES_PCT_1")] < 75 and row[cf.index("STAND_A1")] in ["C", "B"]:
                        row[cf.index("STAND_A1")] = "M"
                        row_updated = True
                        correction_string_add = "Updated STAND_A1 to 'M' because SPECIES_CD_1 = '" + row[
                                cf.index("SPECIES_CD_1")] + "' and SPECIES_PCT_1 >= 50 and < 75 and STAND_A1 was 'C' or 'B'"
                        if correction_string == "":
                            correction_string = correction_string_add
                        else:
                            correction_string += "; " + correction_string_add
                elif row[cf.index("SPECIES_CD_1")] in ["B", "BB", "BL", "CW", "FD", "FDI", "HM", "HW", "PA", "PL", "PLI", 
                        "S", "SB", "SE", "SS", "SW", "SX", "SXW"] and row[cf.index("SPECIES_PCT_1")] >= 75 \
                        and row[cf.index("STAND_A1")] == "M":
                    row[cf.index("STAND_A1")] = "C"
                    row_updated = True
                    correction_string_add = "Updated STAND_A1 to 'C' because SPECIES_CD_1 = '" + row[
                            cf.index("SPECIES_CD_1")] + "' and SPECIES_PCT_1 >= 75 and STAND_A1 was 'M'"
                    if correction_string == "":
                        correction_string = correction_string_add
                    else:
                        correction_string += "; " + correction_string_add

                if blank_remaining_eco_fields:
                    ## Blank everything but SDEC_1 and BEUMC_S1
                    for eco_field in eco_fields:
                        if eco_field[:4] in ["TREE", "SHRU"]:
                            row[cf.index(eco_field)] = None
                            row[cf.index(eco_field.replace("1", "2"))] = None
                            row[cf.index(eco_field.replace("1", "3"))] = None
                        else:                        
                            if eco_field != "BEUMC_S1" and not (preserve_strct_s1_field and eco_field == "STRCT_S1"):
                                row[cf.index(eco_field)] = ""
                            row[cf.index(eco_field.replace("1", "2"))] = ""
                            row[cf.index(eco_field.replace("1", "3"))] = ""
                        row[cf.index("SDEC_2")] = None
                        row[cf.index("SDEC_3")] = None

                decile_total = 0
                if row[cf.index("SDEC_1")] > 0:
                    decile_total += row[cf.index("SDEC_1")]
                if row[cf.index("SDEC_2")] > 0:
                    decile_total += row[cf.index("SDEC_2")]
                if row[cf.index("SDEC_3")] > 0:
                    decile_total += row[cf.index("SDEC_3")]
                if decile_total != 10:
                    row_updated = True
                    if correction_string == '':
                        correction_string = "**** DECILE TOTAL " + str(row[cf.index("SDEC_1")]).replace("None","0") + \
                                            "+" + str(row[cf.index("SDEC_2")]).replace("None","0") + "+" + \
                                            str(row[cf.index("SDEC_3")]).replace("None","0") + "=" + str(decile_total)
                    else:
                        correction_string += "; **** DECILE TOTAL " + str(row[cf.index("SDEC_1")]).replace("None","0") + \
                                             "+" + str(row[cf.index("SDEC_2")]).replace("None","0") + "+" + \
                                             str(row[cf.index("SDEC_3")]).replace("None","0") + "=" + str(decile_total)

                # Added May 9, 2020: Check combinations of BGC Subzone and BEU Mapcode against specified CSV table
                # Updated August 31, 2020: Report invalid and missing combiations in Lbl_edit field.
                # Updated Sept. 3, 2020: Create an output CSV file listing all combinations found in the data but
                # not found in the input CSV file.
                subzone = str(row[cf.index("BGC_ZONE")]).replace("None", "") \
                          + str(row[cf.index("BGC_SUBZON")]).replace("None", "")
                for decile in ["1", "2", "3"]:
                    if row[cf.index("SDEC_" + decile)] > 0:
                        beu = str(row[cf.index("BEUMC_S" + decile)])
                        if subzone in bec_beu_allowed_dict:
                            if beu not in bec_beu_allowed_dict[subzone]:
                                try:
                                    if len(bec_beu_error_dict[subzone][beu]) == 2:
                                        row[cf.index("BEUMC_S" + decile)] = bec_beu_error_dict[subzone][beu]
                                        if correction_string != "":
                                            correction_string += "; "
                                        correction_string += subzone + " " + beu + " corrected to " \
                                                             + bec_beu_error_dict[subzone][beu] + " in decile " + decile
                                    else:
                                        if correction_string != "":
                                            correction_string += "; "
                                        correction_string += subzone + " " + beu + " in decile " + decile \
                                                             + " is invalid combination (mapper needs to assess)"
                                except:
                                    if correction_string != "":
                                        correction_string += "; "
                                    correction_string += subzone + " " + beu + " in decile " + decile \
                                                         + " combination is not listed"
                                    unlisted_combos.append(subzone + "," + beu)
                                row_updated = True

                        # else:
                        #     pdb.set_trace()
                            # pass

                if row_updated:
                    row[cf.index("Lbl_edit")] = correction_string[:1000]
                
                row[cf.index("DEC_Total")] = 0
                if row[cf.index("SDEC_1")] > 0:
                    row[cf.index("DEC_Total")] += row[cf.index("SDEC_1")]
                if row[cf.index("SDEC_2")] > 0:
                    row[cf.index("DEC_Total")] += row[cf.index("SDEC_2")]
                if row[cf.index("SDEC_3")] > 0:
                    row[cf.index("DEC_Total")] += row[cf.index("SDEC_3")]

                cursor.updateRow(row)
                update_count += 1

            if row_count % 100000 == 0 or row_count == total_count:
                logging.info("    - Processed " + str(row_count) + " of " + str(total_count) + " rows; updated " + \
                             str(update_count) + " rows")

    ## Write the output CSV file any subzone/mapcode combos found in the data that were not in the input CSV
    if len(unlisted_combos) > 0:
        out_csv = bec_beu_csv[:-4] + "_unlisted_" + time.strftime('%Y%m%d_%H%M%S') + ".csv"
        logging.info("Writing unlisted combinations of subzone and mapcode in CSV file " + out_csv)
        out_csv_object = open(out_csv, "w")
        out_csv_object.write("BGC Subzone,BEU_#,Script rule,Change to BEU =\n")
        for unlisted_combo in sorted(set(unlisted_combos)):
            out_csv_object.write(unlisted_combo + ",,\n")

            # Remove duplicate labels (in BEM may have had two of the same forested unit; one associated with one set 
            # of site conditions and the other representing different conditions).  Site modifiers are NOT updated in 
            # this product.  Therefore, duplicate labels were removed where the following query applied:
            # Where (BEUMC_S1 =  BEUMC_S2) combine as one decile 

            if row[cf.index("BEUMC_S1")] == row[cf.index("BEUMC_S2")]:
                if row[cf.index("SDEC_1")] > 0 and row[cf.index("SDEC_2")] > 0: 
                    row[cf.index("SDEC_1")] = row[cf.index("SDEC_1")] + row[cf.index("SDEC_2")]
                row[cf.index("SDEC_2")] = row[cf.index("SDEC_3")]
                row[cf.index("SDEC_3")] = None
                for eco_field in eco_fields:
                    row[cf.index(eco_field.replace("1", "2"))] = row[cf.index(eco_field.replace("1", "3"))]
                    if eco_field[:4] in ["TREE", "SHRU"]:
                        row[cf.index(eco_field.replace("1", "3"))] = None
                    else:
                        row[cf.index(eco_field.replace("1", "3"))] = ""
                row_updated = True
                correction_string = "Combined components 1 and 2 with same BEUMC_S# code into single component 1"
    # ---------------------------------------------------------------------------------------------------------
    # Select polygons that intersect any river and update them with SITE_M3A = 'a'
    # ---------------------------------------------------------------------------------------------------------

    logging.info("Selecting polygons that intersect rivers")
    input_fl = "input_fl"
    arcpy.MakeFeatureLayer_management(input_fc, input_fl)
    arcpy.SelectLayerByLocation_management(input_fl, "INTERSECT", rivers_fc)
    row_count = 0
    total_count = int(arcpy.GetCount_management(input_fl).getOutput(0))
    logging.info("Updating " + str(total_count) + " selected polygons that intersect rivers")
    
    with arcpy.da.UpdateCursor(input_fl, ["SITE_M3A", "Lbl_edit"]) as cursor:
        for row in cursor:
            row_count += 1
            old_site_m3a = ""
            if row[0] not in ["", " ", None]:
                old_site_m3a = row[0]
            row[0] = "a"
            if row[1] not in ["", " ", None]:
                row[1] += "; Updated SITE_M3A from '" + old_site_m3a + "' to 'a' because polygon is adjacent to river"
            else:
                row[1] = "Updated SITE_M3A from '" + old_site_m3a + "' to 'a' because polygon is adjacent to river"
            row[1] = row[1][:1000]
            cursor.updateRow(row)

            if row_count % 100000 == 0 or row_count == total_count:
                logging.info("    - Processed " + str(row_count) + " of " + str(total_count) + " rows")

    # ---------------------------------------------------------------------------------------------------------
    # Done
    # ---------------------------------------------------------------------------------------------------------

    dtCalcNow = time.time()
    dtCalcScriptElapsed = dtCalcNow - dtCalcScriptStart
    logging.info("- Script complete after " + SanitizeElapsedTime(dtCalcScriptElapsed))

if __name__ == '__main__':
    try:
        # Parse arguments
        parser = ArgumentParser(description='This script updates BEM attributes based on VRI attributes, '
                                            'according to the document "Corrections to the BEM map codes". ',
                                formatter_class=RawTextHelpFormatter)
        parser.add_argument('ifc', help='Input polygon feature class')
        parser.add_argument('riv', help='Rivers polygon feature class')
        parser.add_argument('bbc', help='BEC BEU CSV file')
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
        main(args.ifc, args.riv, args.bbc)

    except Exception as e:
        logging.exception('Unexpected exception. Program terminating.')
else:
    import arcpy
