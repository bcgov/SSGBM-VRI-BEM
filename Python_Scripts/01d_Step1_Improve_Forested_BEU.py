r"""

Original Author:
Madrone (Jeff Kruys)

Created on:
2021-01-18

Purpose:
This script takes a specified input table or feature class and an .xlsx file
containing rules for assigning BEU mapcodes to VRIxBEM polygons. Excel file must
contain columns describing the rules to be applied and values to be assigned.
Expected columns headers are RULE#, INPUTS, followed by a number of fields
that are found in the VRIxBEM attribute table, OUTPUTS, and a number of fields
- which may or may not need to be added to the VRI attribute table - to be 
populated as specified.

Usage:
Improve_Forested_BEU.py vfc ixl [-h] [-l] [-ld]

Positional Arguments:
   vfc              BEMVRI feature class
   ixl              Rules for Improving Forested BEUs Excel file
   
Optional Arguments:
  -h, --help       show this help message and exit
  -l, --level      log level messages to display; Default: 20 - INFO
  -ld, --log_dir   path to directory for output log file

Example Input:
X:\fullpath\Improve_Forested_BEU.py Y:\fullpath\vfc Z:\fullpath\ixl.xlsx


History
2021-03-24 (JK): Created script.
2021-04-22 (JK): Updated to handle SPECIES_CD_# and SPECIES_PCT_# fields
                 as a special case. The input Excel table may contain columns
                 with SPECIES_CD_1, SPECIES_PCT_1 etc. headers, and the values
                 in these columns will be searched in each of the 6 pairs of 
                 SPECIES_CD_# and SPECIES_PCT_# fields in the spatial data when
                 looking for polygon attribute values that match the rules.
                 Also the comment written to the BEU_Comment field will contain
                 a list of all 6 species and percentage values, regardless of
                 how many of these pairs of fields are found in the Excel 
                 table.
2021-04-23 (JK): Also updated so the BEU_Comment field will contain a list of
                 all 6 species and percentage values in the "No match" comments.
                 Added code to handle special headers in Excel table, 
                 TREE_RL_SP_CD_# and TREE_RL_SP_PCT_#, whose values will be 
                 searched for matching values in all 6 of the SPECIES_CD_# and 
                 SPECIES_PCT_# fields in the VRIxBEM data.
2022-02-01 (JK): Updates to handle cases where 0% for a species is specified, 
                 where a species percent must be greater than another species 
                 percent, and where CONTAINS or DOES NOT CONTAIN is specified.
                 Updated to PEP8 script code standards.
2022-03-08 (JK): Update for new sheet name "Combined_Rules_for_Script". Renamed
                 script to 02h_improve_forested_beu.py.
2022-03-10 (JK): Display update every 10000 records in UpdateCursor loop 
                 instead of every 100000. Add field names to comment string.
2022-03-11 (JK): Updated with standard logger and check for 64-bit python 2.x.
                 Fixed minor issues.
2022-03-12 (JK): Added code to skip updates to polygons if there is any non-
                 null value in POLY_COMM or SMPL_TYPE. Stop checking validity
                 of column headers to the left of INPUTS in the BEU Rules
                 table.
2022-03-13 (JK): Added a check that the input is a polygon feature class.
2022-03-14 (JK): Updated the comments section.
2022-04-08 (JK): Ensure that the string written to BEUMC_Comment is 200
                 characters or less.
"""

import logging
import time
import os
import sys
import ctypes
import subprocess

from argparse import ArgumentParser
from argparse import RawTextHelpFormatter


def main(vri_fc, input_xlsx):
    logger.info("Initializing...")

    logger.info("Start Time: {}".format(time.ctime(time.time())))
    dt_calc_script_start = time.time()

    # ---------------------------------------------------------------------------------------------------------
    #  Check ArcGIS license/extension availability
    # ---------------------------------------------------------------------------------------------------------

    prod_info = arcpy.ProductInfo()
    prod_name_dict = {"ArcView": "Basic", "ArcEditor": "Standard", "ArcInfo": "Advanced"}
    if prod_info in prod_name_dict:
        logger.info("ArcGIS Desktop {} ({}) license detected".format(prod_info, prod_name_dict[prod_info]))
    else:
        logger.error("ArcView (Basic) license or higher required for this script. Exiting script.")
        sys.exit()

    # ---------------------------------------------------------------------------------------------------------
    # After defining this MEMORYSTATUSEX class, use it to access memory usage numbers, e.g.:
    #
    # stat = MEMORYSTATUSEX()
    # logger.info("Memory load: {}%".format(stat.dwMemoryLoad))
    # ---------------------------------------------------------------------------------------------------------

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

    stat = MEMORYSTATUSEX()
    logger.info("Current memory load: {}%".format(stat.dwMemoryLoad))

    # ---------------------------------------------------------------------------------------------------------
    #  Function to construct a time string from a number (of seconds)
    # ---------------------------------------------------------------------------------------------------------

    def sanitize_elapsed_time(dt_input):
        if dt_input < 120:
            str_elapsed_time = "{} sec.".format(int(dt_input))
        elif dt_input < 3600:
            str_elapsed_time = "{} min.".format(round(dt_input / 60, 1))
        else:
            str_elapsed_time = "{} hr.".format(round(dt_input / 3600, 2))
        return str_elapsed_time

    # ---------------------------------------------------------------------------------------------------------
    #  Function to test if schema locked before writing; inform user and wait 10 sec if locked
    # ---------------------------------------------------------------------------------------------------------

    def check_schema_lock(fc_or_tbl):
        while not arcpy.TestSchemaLock(fc_or_tbl):
            logger.info('**** Close the application(s) that are keeping dataset {} locked. '
                        'Trying again in 10 seconds.'.format(fc_or_tbl))
            time.sleep(10)

    # ---------------------------------------------------------------------------------------------------------
    # Check that input feature classes exist
    # ---------------------------------------------------------------------------------------------------------

    if not arcpy.Exists(vri_fc):
        logger.error("**** Specified input BEMVRI feature class {} does not exist. Exiting script.".format(
                vri_fc))
        sys.exit()

    fc_desc = arcpy.Describe(vri_fc)
    fc_datatype = fc_desc.dataType
    if fc_datatype != "FeatureClass":
        logger.error("Specified TEM/PEM/BEM feature class is a {}, not a feature class. Exiting "
                     "script.".format(fc_datatype))
        sys.exit()
    fc_shapetype = fc_desc.shapeType
    if fc_shapetype != "Polygon":
        logger.error("Specified TEM/PEM/BEM feature class is a {} feature class, not polygon. Exiting "
                     "script.".format(fc_shapetype))
        sys.exit()

    if not os.path.isfile(input_xlsx):
        logger.error("**** Specified input Excel file {} does not exist. Exiting script.".format(input_xlsx))
        sys.exit()

    # ---------------------------------------------------------------------------------------------------------
    # Check that the fields listed in the Excel file all exist in the table or feature class
    # ---------------------------------------------------------------------------------------------------------

    valid_headers = [f.name for f in arcpy.ListFields(vri_fc) if not f.required]
    for header in ['RULE#', 'INPUTS', 'OUTPUTS']:
        valid_headers.append(header)

    # Construct a list of the first column references in an Excel sheet - "A" through "Z", then "AA" through "AZ",
    # "BA" through "BZ", etc. up to "ZA" through "ZZ". (Makes 702 columns, Should be enough for any output table)
    alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    columns = []
    for letter in alphabet:
        columns.append(letter)
    for letter1 in alphabet:
        for letter2 in alphabet:
            columns.append(letter1 + letter2)

    logger.info("Checking fields in input Excel file")
    wb = openpyxl.load_workbook(input_xlsx)
    try:
        sheet = wb.get_sheet_by_name("Combined_Rules_for_Script")
    except:
        logger.error("Input Excel file missing required worksheet 'Rules_for_Script'. Exiting script.".format(header))
        sys.exit()

    xlsx_field_list = []
    # Header to column dictionary: htc_dict["BGC_ZONE"] = "A"
    htc_dict = {}
    inputs_found = False
    valid_chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz1234567890_"
    for column in columns:
        try:
            header = str(sheet["{}1".format(column)].value)
        except:
            logger.error("Invalid character found in Excel header {}. Exiting script.".format(header))
            sys.exit()
        if header == "INPUTS":
            inputs_found = True
        # The columns to the left of the "INPUTS" column are just for comments etc., so ignore them
        if inputs_found:
            if header not in valid_headers:
                for char in header:
                    if char not in valid_chars:
                        logger.error("Invalid character '{}' found in Excel header {}. Exiting script.".format(char,
                                                                                                               header))
                        sys.exit()
                if len(header) > 32:
                    logger.error("Invalid Excel header {} (longer than 32 characters). Exiting script.".format(header))
                    sys.exit()
                if header[:1] in "0123456789":
                    logger.error("Invalid Excel header {} (must not begin with number). Exiting script.".format(header))
                    sys.exit()
        if header not in ["", "None"]:
            if header[:14] == "TREE_RL_SP_CD_" or header[:15] == "TREE_RL_SP_PCT_":
                xlsx_field_list.append(header.replace("TREE_RL_SP_", "SPECIES_"))
                htc_dict[header.replace("TREE_RL_SP_", "SPECIES_")] = column
            else:
                xlsx_field_list.append(header)
                htc_dict[header] = column
        else:
            break

    for f in ['INPUTS', 'OUTPUTS']:
        if f not in xlsx_field_list:
            logger.error("Input Excel missing required field {}. Exiting script.".format(f))
            sys.exit()

    # if len(xlsx_field_list) != len(set(xlsx_field_list)):
    #     logger.error("Duplicate field names found in the input Excel file. Exiting script.")
    missing_fields = []
    fields_to_add = []
    existing_vri_fields = [f.name for f in arcpy.ListFields(vri_fc) if not f.required]
    inputs_found = False
    outputs_found = False
    xlsx_input_fields = []
    xlsx_output_fields = []
    for xlsx_field in xlsx_field_list:
        if xlsx_field == "INPUTS":
            inputs_found = True
        elif xlsx_field == "OUTPUTS":
            outputs_found = True
        else:
            if xlsx_field not in existing_vri_fields:
                if outputs_found:
                    fields_to_add.append(xlsx_field)
                elif inputs_found:
                    missing_fields.append(xlsx_field)
            if outputs_found:
                xlsx_output_fields.append(xlsx_field)
            elif inputs_found:
                xlsx_input_fields.append(xlsx_field)

    if len(missing_fields) > 0:
        logger.error("Fields in Excel file missing from VRI feature class: {}. Exiting script.".format(
                str(missing_fields).replace("[", "").replace("]", "").replace("'", "")))
        sys.exit()
    if len(xlsx_input_fields) == 0:
        logger.error("No fields found between INPUTS and OUTPUTS fields in Excel file. Exiting script.")
        sys.exit()
    if len(xlsx_output_fields) == 0:
        logger.error("No fields found to the right of the OUTPUTS field in Excel file. Exiting script.")
        sys.exit()
    if len(fields_to_add) > 0:
        logger.info("Fields found to the right of the OUTPUTS field will be added to the input VRI feature "
                    "class: {}".format(", ".join(fields_to_add)))

    # ---------------------------------------------------------------------------------------------------------
    # Read the Excel file into a dictionary. Dictionary key will be a stringified list of values in the fields
    # before FREQ. Values will be a dictionary of field names and values after FREQ.
    # ---------------------------------------------------------------------------------------------------------

    logger.info("Reading input Excel file")

    tbl_field_dict = {}
    for field in [f for f in arcpy.ListFields(vri_fc)]:
        tbl_field_dict[field.name] = {"TYPE": field.type, "LENGTH": field.length}

    value_type_count_dict = {}
    for field in fields_to_add:
        value_type_count_dict[field] = {"BLANK": 0, "LONG": 0, "SHORT": 0, "DOUBLE": 0, "TEXT": 0, "LONGEST": 0}

    row = 1
    row_has_data = True
    rules_dict = {}
    rule_comment_dict = {}
    while row_has_data:
        row += 1
        row_has_data = False
        rules_dict[row] = {}
        rules_dict[row]["COUNT"] = 0
        rule_comment_dict[row] = "(Rule: "

        for field_name in xlsx_input_fields:
            cell_value = sheet["{}{}".format(htc_dict[field_name], row)].value
            if cell_value not in ["", None]:
                row_has_data = True
                if field_name in ["TREE_RL_SP_CD_1", "SPECIES_CD_1"]:
                    rule_comment_dict[row] += "Species: " + str(cell_value.replace(" ", "").replace(",", "/")) + " "
                elif ((field_name[:14] == "TREE_RL_SP_CD_" or field_name[:11] == "SPECIES_CD_") 
                      and field_name[-1:] != "1"):
                    rule_comment_dict[row] += str(cell_value.replace(" ", "").replace(",", "/")) + " "
                elif field_name[:15] == "TREE_RL_SP_PCT_" or field_name[:12] == "SPECIES_PCT_":
                    rule_comment_dict[row] += str(cell_value) + "%, "
                else:
                    rule_comment_dict[row] += field_name + ": " + str(cell_value) + ", "
                # Values for numeric fields must be expressed as a range, e.g. "25-90"
                # If you want say "< 25", use "0-24.9"; ">= 25" should be "25-100"
                if tbl_field_dict[field_name]["TYPE"] in ["Integer", "SmallInteger", "Single", "Double"]:
                    if str(cell_value).count("-") == 1 and str(cell_value)[:1] != "-":
                        for i in [0, 1]:
                            try:
                                z = float(str(cell_value).split('-')[i])
                            except:
                                logger.error("Invalid value found in row {} of Excel file. Value in {} field is "
                                             "'{}' but needs to be a numeric value, or a range of values "
                                             "specified as min-max, e.g. 10-30. Exiting "
                                             "script.".format(row, field_name, str(cell_value).split('-')[i]))
                                sys.exit()
                        value_tuple = (float(str(cell_value).split('-')[0]), float(str(cell_value).split('-')[1]))
                        rules_dict[row][field_name] = value_tuple
                    else:
                        try:
                            z = float(cell_value)
                            # This will incur an error if the item is not numeric.
                        except:
                            logger.error("Invalid value found in row {} of Excel file. Value in {} field is "
                                         "'{}' but needs to be a numeric value, or a range of values "
                                         "specified as min-max, e.g. 10-30. Exiting "
                                         "script.".format(row, field_name, cell_value))
                            sys.exit()
                        value_tuple = (float(str(cell_value)), float(str(cell_value)))
                        rules_dict[row][field_name] = value_tuple

                elif tbl_field_dict[field_name.replace("#", "1")]["TYPE"] == "String":
                    # Values can be a list of strings separated by commas.
                    # They can also be two such lists of tree species, separated by a > (greater than sign).
                    # to indicate that the total percent of the first list of species must be greater than
                    # the total percent of the second list of species.
                    if ">" in str(cell_value):
                        str_list = []
                        cell_value1 = cell_value.split(">")[0]
                        for str_value1 in str(cell_value1).replace(" ", "").split(","):
                            if len(str_value1.replace("BLANK", "")) <= tbl_field_dict[field_name]["LENGTH"]:
                                str_list.append(">" + str_value1.replace("BLANK", ""))
                            else:
                                logger.error("Invalid value found in row {} of Excel file. Value '{}' in {} field "
                                             "needs to have length {} or less. Exiting "
                                             "script.".format(row, str_value1, field_name,
                                                              tbl_field_dict[field_name]["LENGTH"]))
                                sys.exit()
                        cell_value2 = cell_value.split(">")[1]
                        for str_value2 in str(cell_value2).replace(" ", "").split(","):
                            if len(str_value2.replace("BLANK", "")) <= tbl_field_dict[field_name]["LENGTH"]:
                                str_list.append("<" + str_value2.replace("BLANK", ""))
                            else:
                                logger.error("Invalid value found in row {} of Excel file. Value '{}' in {} field "
                                             "needs to have length {} or less. Exiting "
                                             "script.".format(row, str_value2, field_name,
                                                              tbl_field_dict[field_name]["LENGTH"]))
                                sys.exit()
                        # If cell value was "PL,PLI>SW,SX,SE,S,SXW", then str_list now looks like 
                        # [">PL", ">PLI", "<SW", "<SX", "<SE", "<S", "<SXW"]
                        rules_dict[row][field_name] = str_list
                    elif ',' in str(cell_value):
                        str_list = []
                        for str_value in str(cell_value).replace(" ", "").split(","):
                            if len(str_value.replace("BLANK", "")) <= tbl_field_dict[field_name]["LENGTH"]:
                                str_list.append(str_value.replace("BLANK", ""))
                            else:
                                logger.error("Invalid value found in row {} of Excel file. Value '{}' in {} field "
                                             "needs to have length {} or less. Exiting "
                                             "script.".format(row, str_value, field_name,
                                                              tbl_field_dict[field_name]["LENGTH"]))
                                sys.exit()
                        rules_dict[row][field_name] = str_list
                    elif len(str(cell_value).replace("None", "").replace("BLANK", "").lstrip("CONTAINS ").lstrip(
                            "DOES NOT CONTAIN ")) <= tbl_field_dict[field_name]["LENGTH"]:
                        rules_dict[row][field_name] = str(cell_value).replace("None", "").replace("BLANK", "")
                    else:
                        logger.error("Invalid value found in row {} of Excel file. Value in {} field is '{}' but "
                                     "needs to have length {} or less. Only string or numeric fields are permitted. "
                                     "Exiting script.".format(row, field_name, cell_value,
                                                              tbl_field_dict[field_name]["LENGTH"]))
                        sys.exit()
                else:
                    logger.error("Invalid field type found in row {} of Excel file. Value in {} field is '{}' but "
                                 "input table or feature class field {} has type {}. Exiting "
                                 "script.".format(row, field_name, cell_value, field_name,
                                                  tbl_field_dict[field_name]["TYPE"]))
                    sys.exit()

        for field_name in xlsx_output_fields:
            cell_value = sheet["{}{}".format(htc_dict[field_name], row)].value
            if field_name in fields_to_add:
                rules_dict[row]["OUTPUT_{}".format(field_name)] = cell_value
                # Keep the value regardless of value type (string, numeric etc.)
                # Now test the values and keep track of how many values of each type are found in 
                # each field_to_add CSV column. Later we will use this info to determine what field
                # type to create each new field as.
                if cell_value in ["", None]:
                    value_type_count_dict[field_name]["BLANK"] += 1
                else:
                    try:
                        z = float(cell_value)
                        # This will incur an error if the item is not numeric.
                        if z == cell_value:
                            if -32768 <= z <= 32767:
                                value_type_count_dict[field_name]["SHORT"] += 1
                            elif -2147483648 <= z <= 2147483647:
                                value_type_count_dict[field_name]["LONG"] += 1
                            else:
                                value_type_count_dict[field_name]["DOUBLE"] += 1
                        else:
                            value_type_count_dict[field_name]["DOUBLE"] += 1
                    except:
                        value_type_count_dict[field_name]["TEXT"] += 1
                if len(str(cell_value).replace("None", "")) > value_type_count_dict[field_name]["LONGEST"]:
                    value_type_count_dict[field_name]["LONGEST"] = len(str(cell_value).replace("None", ""))
              
            else:
                # Values from fields to the right of OUTPUTS are going to be written to existing fields in the
                # input feature class or table, so we need to check that the values in the Excel file are 
                # compatible with the field types in the feature class.
                if tbl_field_dict[field_name]["TYPE"] in ["Integer", "SmallInteger", "Single", "Double"]:
                    if cell_value in ["", None]:
                        rules_dict[row]["OUTPUT_{}".format(field_name)] = None
                    else:
                        try:
                            z = float(cell_value)
                            # This will incur an error if the item is not numeric.
                            if z == cell_value:
                                # Value in CSV is an integer. 
                                if (tbl_field_dict[field_name]["TYPE"] == "SmallInteger" and 
                                        (z < -32768 or z > 32767)):
                                    logger.error("Invalid value found in row {} of Excel file. Value in {} field is "
                                                 "'{}' but needs to be between -32,768 and 32767. Exiting "
                                                 "script.".format(row, field_name, cell_value))
                                    sys.exit()
                                elif (tbl_field_dict[field_name]["TYPE"] == "Integer" and 
                                        (z < -2147483648 or z > 2147483647)):
                                    logger.error("Invalid value found in row {} of Excel file. Value in {} field is "
                                                 "'{}' but needs to be between -2,147,483,648 and 2,147,483,647. "
                                                 "Exiting script.".format(row, field_name, cell_value))
                                    sys.exit()
                                # Single and Double fields can handle pretty much any number.
                            else:
                                if tbl_field_dict[field_name]["TYPE"] in ["Integer", "SmallInteger"]:
                                    logger.error("Invalid value found in row {} of Excel file. Value in {} field is "
                                                 "'{}' but needs to an integer value for this field. Exiting "
                                                 "script.".format(row, field_name, cell_value))
                                    sys.exit()
                            rules_dict[row]["OUTPUT_{}".format(field_name)] = cell_value
                        except:
                            logger.error("Invalid value found in row {} of Excel file. Value in {} field is '{}' but "
                                         "needs to be a numeric value. Exiting script.".format(row, field_name,
                                                                                               cell_value))
                            sys.exit()
                elif tbl_field_dict[field_name]["TYPE"] == "String":
                    field_len = tbl_field_dict[field_name]["LENGTH"]
                    if len(str(cell_value).replace("None", "")) <= field_len:
                        rules_dict[row]["OUTPUT_{}".format(field_name)] = str(cell_value).replace("None", "")
                    else:
                        logger.error("Invalid value found in row {} of Excel file. Value in {} field is '{}' but "
                                     "needs to have length {} or less. Exiting "
                                     "script.".format(row, field_name, str(cell_value).replace("None", ""), field_len))
                        sys.exit()
                else:
                    field_type = tbl_field_dict[field_name]["LENGTH"]
                    logger.error("Invalid field type found in Excel file. Input table or feature class field {} has "
                                 "type {}. Only string or numeric fields are permitted. Exiting "
                                 "script.".format(field_name, field_type))
                    sys.exit()
    
        rule_comment_dict[row] = rule_comment_dict[row].rstrip(", ") + ")"

    # Remove the last rule, which represents the first all-blank row of the Excel table
    del rules_dict[row]
    del rule_comment_dict[row]

    # ---------------------------------------------------------------------------------------------------------
    # Add new fields to table or feature class, if any new fields found in Excel file to the right of the OUTPUTS 
    # field.
    # ---------------------------------------------------------------------------------------------------------

    for field_name in fields_to_add:
        if value_type_count_dict[field_name]["TEXT"] > 0:
            length = value_type_count_dict[field_name]["LONGEST"]
            logger.info("Adding new field {} of type TEXT, length {}".format(field_name, length))
            check_schema_lock(vri_fc)
            arcpy.AddField_management(vri_fc, field_name, "TEXT", "#", "#", length)
        elif value_type_count_dict[field_name]["DOUBLE"] > 0:
            logger.info("Adding new field {} of type DOUBLE".format(field_name))
            check_schema_lock(vri_fc)
            arcpy.AddField_management(vri_fc, field_name, "DOUBLE")
        elif value_type_count_dict[field_name]["LONG"] > 0:
            logger.info("Adding new field {} of type LONG".format(field_name))
            check_schema_lock(vri_fc)
            arcpy.AddField_management(vri_fc, field_name, "LONG")
        elif value_type_count_dict[field_name]["SHORT"] > 0:
            logger.info("Adding new field {} of type SHORT".format(field_name))
            check_schema_lock(vri_fc)
            arcpy.AddField_management(vri_fc, field_name, "SHORT")
        else:
            logger.error("Excel field {} contains only blank values. Exiting script.".format(field_name))
            sys.exit()

    if "BEUMC_Comment" not in [f.name for f in arcpy.ListFields(vri_fc)]:
        logger.info("Adding new field BEUMC_Comment of type TEXT, length 200")
        check_schema_lock(vri_fc)
        arcpy.AddField_management(vri_fc, "BEUMC_Comment", "TEXT", "#", "#", 200)

    if "SMPL_TYPE" not in [f.name for f in arcpy.ListFields(vri_fc)]:
        logger.info("Adding new field SMPL_TYPE of type TEXT, length 1")
        check_schema_lock(vri_fc)
        arcpy.AddField_management(vri_fc, "SMPL_TYPE", "TEXT", "#", "#", 1)

    # ---------------------------------------------------------------------------------------------------------
    # Write the matching values from the Excel fields to the right of FREQ to the fields in the input table or 
    # feature class.
    # ---------------------------------------------------------------------------------------------------------

    logger.info("Overwriting values in input VRI feature class fields {}, BEUMC_Comment".format( 
                 str(xlsx_output_fields).replace("[", "").replace("]", "").replace("'", "")))

    cfl = []

    for field_name in xlsx_input_fields:
        cfl.append(field_name)

    for field_name in xlsx_output_fields:
        cfl.append(field_name)

    exi_f = [f.name for f in arcpy.ListFields(vri_fc)]
    cfl.append("BEUMC_Comment")
    cfl.append("SMPL_TYPE")
    if "POLY_COMM" in exi_f:
        cfl.append("POLY_COMM")
    for i in range(1, 7):
        if "SPECIES_CD_{}".format(i) not in cfl:
            cfl.append("SPECIES_CD_{}".format(i))
        if "SPECIES_PCT_{}".format(i) not in cfl:
            cfl.append("SPECIES_PCT_{}".format(i))

    cfl_dict = {}
    for field in [f for f in arcpy.ListFields(vri_fc)]:
        cfl_dict[field.name] = {"TYPE": field.type, "LENGTH": field.length}

    total_count = int(arcpy.GetCount_management(vri_fc).getOutput(0))
    row_count = 0
    no_match_count = 0
    check_schema_lock(vri_fc)
    with arcpy.da.UpdateCursor(vri_fc, cfl) as cursor:
        for row in cursor:
            row_count += 1
            if "POLY_COMM" in cfl:
                poly_comm = row[cfl.index("POLY_COMM")]
            else:
                poly_comm = None
            if row[cfl.index("SMPL_TYPE")] in [None, "", "None"] and poly_comm in [None, "", "None"]:
                row_updated = False
                for i in rules_dict.keys():
                    its_a_match = True
                    if not row_updated:
                        species_pct_dict = {}
                        species_gt_dict = {}
                        # First build a dictionary of all species/percent pairs found in the table. We have to check
                        # all six of the SPECIES_CD_# and SPECIES_PCT_# fields for each species and percent range.
                        for j in range(1, 7):
                            if ("SPECIES_CD_{}".format(j) in rules_dict[i].keys() and "SPECIES_PCT_{}".format(j)
                                    in rules_dict[i].keys()):
                                species_pct_dict[j] = {}
                                if type(rules_dict[i]["SPECIES_CD_{}".format(j)]) == list:
                                    for species in rules_dict[i]["SPECIES_CD_{}".format(j)]:
                                        species_pct_dict[j][species] = rules_dict[i]["SPECIES_PCT_{}".format(j)]
                                else:
                                    species_pct_dict[j][rules_dict[i]["SPECIES_CD_{}".format(j)]] = rules_dict[i][
                                            "SPECIES_PCT_{}".format(j)]
                            elif "SPECIES_CD_{}".format(j) in rules_dict[i].keys():
                                species_gt_dict[j] = rules_dict[i]["SPECIES_CD_{}".format(j)]
                        species_pct_sum_dict = {}
                        for j in species_pct_dict.keys():
                            species_pct_sum_dict[j] = 0
                            for species in species_pct_dict[j].keys():
                                lower_limit = 0
                                upper_limit = 0
                                if type(species_pct_dict[j][species]) in [int, float, str]:
                                    lower_limit = species_pct_dict[j][species]
                                    upper_limit = species_pct_dict[j][species]
                                elif type(species_pct_dict[j][species]) == tuple:
                                    lower_limit = species_pct_dict[j][species][0]
                                    upper_limit = species_pct_dict[j][species][1]
                                for k in range(1, 7):
                                    if row[cfl.index("SPECIES_CD_{}".format(k))] == species:
                                        if row[cfl.index("SPECIES_PCT_{}".format(k))] is not None:
                                            species_pct_sum_dict[j] += row[cfl.index("SPECIES_PCT_{}".format(k))]
                            if not lower_limit <= species_pct_sum_dict[j] <= upper_limit:
                                its_a_match = False

                        # If there were any species lists like "PL,PLI>SX,SE,SXW" i.e. "total pine must be greater than
                        # total spruce" - these should be in a TREE_RL_SP_CD_# column in the input table, with nothing
                        # in the TREE_RL_SP_PCT_# column
                        gt_total = 0
                        lt_total = 0
                        for j in range(1, 7):
                            if j in species_gt_dict:
                                for k in range(1, 7):
                                    for species in species_gt_dict[j]:
                                        if row[cfl.index("SPECIES_CD_{}".format(k))] == species[1:]:
                                            if species[:1] == "<":
                                                if row[cfl.index("SPECIES_PCT_{}".format(k))] > 0:
                                                    lt_total += row[cfl.index("SPECIES_PCT_{}".format(k))]
                                            elif species[:1] == ">":
                                                if row[cfl.index("SPECIES_PCT_{}".format(k))] > 0:
                                                    gt_total += row[cfl.index("SPECIES_PCT_{}".format(k))]
                        if lt_total > gt_total:
                            its_a_match = False

                        for field_name in rules_dict[i].keys():
                            if field_name[:7] != "OUTPUT_" and field_name != "COUNT" and field_name[:8] != "SPECIES_":
                                if type(rules_dict[i][field_name]) in [int, float, str]:
                                    if rules_dict[i][field_name][:9] == "CONTAINS ":
                                        if rules_dict[i][field_name][9:] not in row[cfl.index(field_name)]:
                                            its_a_match = False
                                    elif rules_dict[i][field_name][:17] == "DOES NOT CONTAIN ":
                                        if rules_dict[i][field_name][17:] in row[cfl.index(field_name)]:
                                            its_a_match = False
                                    elif row[cfl.index(field_name)] != rules_dict[i][field_name]:
                                        its_a_match = False
                                elif type(rules_dict[i][field_name]) == tuple:
                                    if (row[cfl.index(field_name)] < rules_dict[i][field_name][0] or 
                                            row[cfl.index(field_name)] > rules_dict[i][field_name][1]):
                                        its_a_match = False
                                elif type(rules_dict[i][field_name]) == list:
                                    if row[cfl.index(field_name)] not in rules_dict[i][field_name]:
                                        its_a_match = False
                        if its_a_match:
                            for field_name in xlsx_output_fields:
                                value = rules_dict[i]["OUTPUT_{}".format(field_name)]
                                if value in ["", None]:
                                    if cfl_dict[field_name]["TYPE"] == "String": 
                                        row[cfl.index(field_name)] = ""
                                    elif cfl_dict[field_name]["TYPE"] in ["Integer", "SmallInteger", "Single", 
                                                                          "Double"]:
                                        row[cfl.index(field_name)] = None
                                else:
                                    if cfl_dict[field_name]["TYPE"] == "String": 
                                        row[cfl.index(field_name)] = str(value).replace("None", "")
                                    elif cfl_dict[field_name]["TYPE"] in ["Integer", "SmallInteger"]:
                                        row[cfl.index(field_name)] = int(value)
                                    elif cfl_dict[field_name]["TYPE"] in ["Single", "Double"]:
                                        row[cfl.index(field_name)] = float(value)

                            comment_str = "Matched with rule on row {} of Excel table: ".format(i)
                            species_found = False
                            for field_name in xlsx_input_fields:
                                if field_name == "SPECIES_CD_1":
                                    for j in range(1, 7):
                                        if row[cfl.index("SPECIES_CD_{}".format(j))] not in ["", None]:
                                            if j == 1:
                                                comment_str += "Species "
                                                species_found = True
                                            comment_str += "{} ".format(row[cfl.index("SPECIES_CD_{}".format(j))])
                                        if row[cfl.index("SPECIES_PCT_{}".format(j))] is not None:
                                            comment_str += "{}%, ".format(row[cfl.index("SPECIES_PCT_{}".format(j))])
                                    if not species_found:
                                        comment_str += "No species, "
                                elif field_name[:11] == "SPECIES_CD_" or field_name[:12] == "SPECIES_PCT_":
                                    pass
                                elif row[cfl.index(field_name)] == "":
                                    comment_str += "{} blank, ".format(field_name)
                                else:
                                    comment_str += "{} {}, ".format(field_name, row[cfl.index(field_name)])
                            comment_str = comment_str.rstrip(", ").replace("None", "null")
                            comment_str += " " + rule_comment_dict[i]
                            row[cfl.index("BEUMC_Comment")] = comment_str[:200]
                            cursor.updateRow(row)
                            row_updated = True
                            rules_dict[i]["COUNT"] += 1
                if not row_updated:
                    for field_name in xlsx_output_fields:
                        if cfl_dict[field_name]["TYPE"] == "String": 
                            row[cfl.index(field_name)] = ""
                        elif cfl_dict[field_name]["TYPE"] in ["Integer", "SmallInteger", "Single", "Double"]:
                            row[cfl.index(field_name)] = None
                    comment_str = "No match: "
                    species_found = False
                    for field_name in xlsx_input_fields:
                        if field_name == "SPECIES_CD_1":
                            for j in range(1, 7):
                                if row[cfl.index("SPECIES_CD_{}".format(j))] not in ["", None]:
                                    if j == 1:
                                        comment_str += "Species "
                                        species_found = True
                                    comment_str += "{} ".format(row[cfl.index("SPECIES_CD_{}".format(j))])
                                if row[cfl.index("SPECIES_PCT_{}".format(j))] is not None:
                                    comment_str += "{}%, ".format(row[cfl.index("SPECIES_PCT_{}".format(j))])
                            if not species_found:
                                comment_str += "No species, "

                        elif field_name[:11] == "SPECIES_CD_" or field_name[:12] == "SPECIES_PCT_":
                            pass
                        elif row[cfl.index(field_name)] == "":
                            comment_str += "{} blank, ".format(field_name)
                        else:
                            comment_str += "{} {}, ".format(field_name, row[cfl.index(field_name)])
                    row[cfl.index("BEUMC_Comment")] = comment_str.rstrip(", ").replace("None", "null")
                    no_match_count += 1

            else:
                for field_name in xlsx_output_fields:
                    if cfl_dict[field_name]["TYPE"] == "String":
                        row[cfl.index(field_name)] = ""
                    elif cfl_dict[field_name]["TYPE"] in ["Integer", "SmallInteger", "Single", "Double"]:
                        row[cfl.index(field_name)] = None
                row[cfl.index("BEUMC_Comment")] = "No update due to a value found in the SMPL_TYPE or POLY_COMM field."
                no_match_count += 1

            row[cfl.index("BEUMC_Comment")] = row[cfl.index("BEUMC_Comment")][:200]

            cursor.updateRow(row)

            if row_count % 10000 == 0 or row_count == total_count:
                logger.info("    Processed {} of {} rows; {} matches, {} "
                            "non-matches".format(row_count, total_count, row_count - no_match_count, no_match_count))

    for i in sorted(set(rules_dict.keys())):
        logger.info("Found {} records matching Excel table row {}".format(rules_dict[i]["COUNT"], i))

    # ---------------------------------------------------------------------------------------------------------
    # Done
    # ---------------------------------------------------------------------------------------------------------

    dt_calc_now = time.time()
    dt_calc_script_elapsed = dt_calc_now - dt_calc_script_start
    logger.info('Script complete after {}'.format(sanitize_elapsed_time(dt_calc_script_elapsed)))


if __name__ == '__main__':
    try:
        # Parse arguments
        parser = ArgumentParser(description='This script reads an Excel file containing all unique data '
                                            'combinations found in the input table plus user-added columns '
                                            'containing data to be overwritten to the input table.',
                                formatter_class=RawTextHelpFormatter)
        parser.add_argument('vfc', help='BEMVRI feature class')
        parser.add_argument('ixl', help='Rules for Improving Forested BEUs Excel file')
        parser.add_argument('-l', '--level', type=int,
                            help='Log level\nValues: 10-DEBUG, 20-INFO(default), 30-WARN, 40-ERROR, 50-CRITICAL')
        parser.add_argument('-ld', '--log_dir', help='Path to log directory')
        try:
            args = parser.parse_args()
        except:
            parser.print_help()
            sys.exit(0)

        # Set up logger
        if args.level is not None and args.level not in [10, 20, 30, 40, 50]:
            raise ValueError('Invalid log level')
        elif args.level is None:
            args.level = 20

        log_name = 'main_logger'
        logger = logging.getLogger(log_name)

        log_fmt = logging.Formatter('%(asctime)s - %(levelname)s - %(message)s')
        log_file_base_name = os.path.basename(sys.argv[0])
        log_file_extension = 'log'
        timestamp = time.strftime('%Y-%m-%d_%H-%M-%S')
        log_file = "{}_{}.{}".format(timestamp, log_file_base_name, log_file_extension)

        logger.setLevel(args.level)

        sh = logging.StreamHandler()
        sh.setLevel(args.level)
        sh.setFormatter(log_fmt)
        logger.addHandler(sh)

        if args.log_dir:
            try:
                os.makedirs(args.log_dir)
            except OSError:
                pass

            fh = logging.FileHandler(os.path.join(args.log_dir, log_file))
            fh.setLevel(args.level)
            fh.setFormatter(log_fmt)
            logger.addHandler(fh)

        # Import arcpy
        try:
            import arcpy
        except:
            msg = 'Python module arcpy not installed. Exiting script.'
            logger.error(msg)
            sys.exit(msg)

        if sys.maxsize <= 2**32:
            logger.error('**** 32-bit python.exe detected. Script must be run in 64-bit python.exe. Exiting script.')
            sys.exit()

        if sys.version_info[0] != 2:
            logger.error('**** Python version {}.{}.{} detected. Script must be run in 64-bit Python 2.x. Exiting '
                         'script.'.format(sys.version_info[0], sys.version_info[1], sys.version_info[2]))
            sys.exit()

        logger.info('Running in 64-bit Python version {}.{}.{}'.format(sys.version_info[0], sys.version_info[1], 
                                                                       sys.version_info[2]))

        try:
            import openpyxl
        except:
            logger.info("Attempting to install openpyxl module")
            try:
                subprocess.check_call([sys.executable, "-m", "pip", "install", "openpyxl"])
                import openpyxl
            except:
                logger.error('Failed to install Python module openpyxl.')
                logger.error('To install it, enter the following commands at the Windows Command Prompt:')
                logger.error('    {}'.format(os.path.split(sys.executable)[0])[:2])
                logger.error(r'    cd {}\Scripts'.format(os.path.split(sys.executable)[0]))
                logger.error('    pip install openpyxl')
                sys.exit()

        # Start the script
        main(args.vfc, args.ixl)

    except Exception as e:
        logging.exception('Unexpected exception. Program terminating.')

else:
    import arcpy
