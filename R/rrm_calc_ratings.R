#' Calculate WHR ratings based on RRM
#' @details Calculates WHR ratings based on an RRM Excel file
#' that contains RSI, AVE and RRT worksheets in their expected
#' formats. Worksheet names must contain prefix RSI_, AVE_ or RRT_.
#' The RRT_ worksheet will be modified with added columns containing
#' the calculated ratings.
#' @param rrm_input A data.table.
#' @param template A character. Path to an RRM Excel file.
#' @param rsi_source A data.table.
#' @param output A character. Either `value/rating` or `full` for the full
#' details with all variables from the formula sheet.
#' @return A data.table matching RRT sheet with two additional columns for each
#' formula. Attribute `missing_lines` contains a list of missing lines for each
#' component sheet.
rrm_calc_ratings <- function(rrm_input, template, rsi_source, output = c("value/rating", "full")) {

  output <- match.arg(output)

  data.table::setnames(
    rsi_source,
    c(tools::toTitleCase(tolower(names(rsi_source)))[-ncol(rsi_source)], tail(names(rsi_source), 1))
  )

  logger::log_info("Initializing...")
  # Time already in log
  # logger::log_info("Start Time: ", format(Sys.time()))

  dt_calc_script_start <- Sys.time()

  # ---------------------------------------------------------------------------------------------------------
  # Check that input feature class exists and contains the required worksheets
  # ---------------------------------------------------------------------------------------------------------

  # Already checked by readxl functions via `readxl:::check_file`, redundant
  if (!file.exists(template)) {
    logger::log_error("File %s does not exist." |> sprintf(template))
    return()
  }

  logger::log_info("Loading file.")
  sheet_names <- readxl::excel_sheets(path = template)

  if (!any(rrt_idx <- grepl("^RRT_", sheet_names))) {
    logger::log_error("File does not contain an RRT sheet.")
    return()
  }

  if (!any(grepl("^RSI_", sheet_names)) && !any(grepl("^AVE_", sheet_names))) {
    logger::log_error("File does not contain any RSI or AVE sheets.")
    return()
  }

  # ---------------------------------------------------------------------------------------------------------
  # Read the RRT worksheet to determine if is has the necessary info: A "Hectares" header and a valid formula
  # header.
  # ---------------------------------------------------------------------------------------------------------

  if (sum(rrt_idx) > 1L) {
    logger::log_error("Multiple RRT sheets found. Expected only one.")
    return()
  }

  logger::log_info(
    "Checking headers in RRT worksheet %s." |>
      sprintf(sheet_names[rrt_idx])
  )
  rrt_sheet <- readxl::read_excel(
    path = template,
    sheet = which(rrt_idx),
    n_max = 1
  )
  rrt_col <- names(rrt_sheet)

  # Read the headers in row 1, look for one header named "Hectares"
  if (!"Hectares" %in% rrt_col) {
    logger::log_error(
      "No 'Hectares' column found in worksheet %s." |>
        sprintf(sheet_names[rrt_idx])
    )
    return()
  } else {
    logger::log_info(
      "'Hectares' found in column %s." |>
        sprintf(
          which(rrt_idx) |> cellranger::num_to_letter()
        )
    )
  }

  # All non-null headers to the right of Hectares are potential formula columns
  # Header is considered a formula if it contains a * (multiplication symbol).
  # Strings on either side of each * should be names of RSI or AVE worksheets.
  # Keep a list of "ignore" columns that have a non-null header that isn't a valid formula. We'll leave these
  # columns alone.

  formula_columns <- rrt_col[-seq_len(which(rrt_col == "Hectares"))]
  all_data_column_headers = setdiff(rrt_col, formula_columns)
  formulas <- grep("[^*]+\\*[^*]+", formula_columns, value = TRUE) |>
    setNames(nm = _)
  ignore_columns <- grep("[^*]+\\*[^*]+", formula_columns, value = TRUE, invert = TRUE)
  rsi_ave_sheets <- grep("^AVE_|^RSI_", sheet_names, value = TRUE)

  if (!length(formulas)) {
    logger::log_error(
      "No formula columns after 'Hectares' column in sheet %s." |>
        sprintf(sheet_names[rrt_idx])
    )
    return()
  }

  if (length(ignore_columns)) {
    logger::log_info(
      "Invalid %s sheet header formula. Ignoring column(s): [%s]." |>
        sprintf(
          sheet_names[rrt_idx],
          ignore_columns |>
            match(rrt_col) |>
            cellranger::num_to_letter() |>
            paste0(collapse = ", ")
        )
    )
  }

  formulas_components <- strsplit(formulas, ":", fixed = TRUE) |>
    lapply(tail, n = 1) |>
    lapply(strsplit, "*", fixed = TRUE) |>
    lapply(unlist)

  for (i in seq_along(formulas_components)) {
    components <- formulas_components[[i]]
    formula_ref <- names(formulas_components)[i]
    column_ref <- formula_ref |>
      match(rrt_col) |>
      cellranger::num_to_letter()
    if (any(missing_component <- !components %in% rsi_ave_sheets)) {
      logger::log_error(
        "Formula in column %s contains component(s) '%s' without a matching RSI or AVE sheet name." |>
          sprintf(column_ref, components[missing_component] |> paste0(collapse = ", "))
      )
      return()
    } else {
      logger::log_info(
        "Found valid formula in column %s: %s" |>
          sprintf(column_ref, formula_ref)
      )
    }
  }

  # ---------------------------------------------------------------------------------------------------------
  # Read each RSI and AVE table to determine which ones are IAV-assigning and which are not.
  # Basically, if a table has IAV fields to the right of the non-IAV fields, then it is IAV-assigning.
  # If a table has an IAV field in column A, or no IAV fields at all, then it is not IAV-assigning.
  # ---------------------------------------------------------------------------------------------------------

  rsi_ave_sheets_with_assigned_iav <- all_assigned_iav_headers <- character()
  data <- all_data_iav_headers <- list()
  for (sht in rsi_ave_sheets) {

    logger::log_info("Reading sheet name %s." |> sprintf(sht))

    first_row <- readxl::read_excel(
      path = template,
      sheet = sht,
      n_max = 1
    )

    rsi_ave_col <- substr(sht, 1,3)

    if (!rsi_ave_col %in% names(first_row)) {
      logger::log_error(
        "No %s column found in sheet %s." |>
          sprintf(rsi_ave_col, sht)
      )
      return()
    }

    empty_pos <- which(names(first_row) %in% "")
    if (length(empty_pos) && any(empty_pos < match(rsi_ave_col, names(first_row)))) {
      logger::log_error(
        "Sheet %s contains blank header(s) before the %s column." |>
          sprintf(sht, rsi_ave_col)
      )
      return()
    }

    rsi_ave_pos <- which(names(first_row) %in% rsi_ave_col)
    iav_pos <- grepl("^IAV", names(first_row), ignore.case = TRUE) |> which()
    data_pos <- setdiff(seq_len(rsi_ave_pos - 1L), c(empty_pos, iav_pos))

    if (any(missing_data_columns <- !names(first_row)[data_pos] %in% all_data_column_headers)) {
      logger::log_error(
        "Sheet %s, column header(s) do(es) not exist in RRT table : [%s]." |>
          sprintf(
            sht,
            paste0(
              names(first_row)[data_pos][missing_data_columns],
              collapse = ", "
            )
          )
      )
      return()
    }

    first_col <- readxl::read_excel(
      path = template,
      sheet = sht,
      range = readxl::cell_cols("A")
    )

    last_nonblank_row <- which(first_col[[1]] %in% "#") |> head(1)
    if (sht %in% "RSI_BGC_BEU") last_nonblank_row <- 1L

    if (!length(last_nonblank_row)) {
      logger::log_error(
        "Sheet %s contains no # at the end of the data in column A." |>
          sprintf(sht)
      )
      return()
    }

    logger::log_info(
      "Data found in row(s) 1 to %s, column(s): [%s]." |>
        sprintf(
          last_nonblank_row,
          paste0(names(first_row)[data_pos], collapse = ", ")
        )
    )

    # IAV values are "assigned" if the IAV columns come after the data columns.
    # IAV values are "referenced" if the IAV columns come before the data columns, and then they can be
    # treated like regular data columns.
    if (length(iav_pos) && !(1 %in% iav_pos)) {
      rsi_ave_sheets_with_assigned_iav <- c(rsi_ave_sheets_with_assigned_iav, sht)
      all_assigned_iav_headers <- c(all_assigned_iav_headers, names(first_row)[iav_pos])
      data[["iav"]][[sht]][["key"]] <- names(first_row)[data_pos]
      data[["iav"]][[sht]][["range"]] <- readxl::cell_limits(
        ul = c(1L,1L),
        lr = c(last_nonblank_row, rsi_ave_pos - 1L)
      )
    }

    # Store data range for further use in the next code blocks
    data[["rsi_ave"]][[sht]][["key"]] <- names(first_row)[seq_len(rsi_ave_pos - 1L)]
    data[["rsi_ave"]][[sht]][["range"]] <- readxl::cell_limits(
      ul = c(1L,1L),
      lr = c(last_nonblank_row, rsi_ave_pos)
    )

  }

  logger::log_info(
    "Found the following RSI/AVE worksheets with IAV values assigned : [%s]." |>
      sprintf(paste0(rsi_ave_sheets_with_assigned_iav, collapse = ", "))
  )

  logger::log_info(
    "Found the following RSI/AVE worksheets: [%s]" |>
      sprintf(paste0(rsi_ave_sheets, collapse = ", "))
  )

  # ---------------------------------------------------------------------------------------------------------
  # Read each of the IAV-assigning RSI and AVE tables into data.table for looking up IAV field values.
  # ---------------------------------------------------------------------------------------------------------

  # Data.table keys will be the stringified lists of non-IAV values found in each row.
  # e.g. Bgc_zone  Bgc_subzon  Bgc_vrt  Bgc_phase  Sitemc_s  Iav_strct_s  Iav_Slope_mod  Iav_Snow  Iav_Solar  RSI
  #      CDF       mm                              AS                                    1         1          0.50
  # Key would be the string "['CDF', 'mm', '', '', 'AS']"
  # rsi_ave_iav_list_dict["['CDF', 'mm', '', '', 'AS']"] = {"Iav_strct_s": "", "Iav_Slope_mod": "",
  #                                                         "Iav_Snow": "1", "Iav_Solar": "1"}

  for (sht in names(data[["iav"]])) {

    use_init <- sht %in% "RSI_BGC_BEU"

    logger::log_info(
      "Reading sheet named %s to assign IAV values to ecosystem units" |>
        sprintf(sht)
    )

    k <- data[["iav"]][[sht]][["key"]]
    r <- data[["iav"]][[sht]][["range"]]

    data[["iav"]][[sht]] <- readxl::read_excel(path = template, sheet = sht, range = r) |>
      data.table::setDT(key = k)

    if (use_init) {
      data[["iav"]][[sht]] <- data.table::rbindlist(
        list(data[["iav"]][[sht]], rsi_source[,intersect(names(rsi_source), names(data[["iav"]][[sht]])), with = FALSE]),
        use.names = TRUE,
        fill = TRUE
      ) |>
        data.table::setDT(key = k)
    }

    dup_rows <- duplicated(data[["iav"]][[sht]][, k, with = FALSE])

    if (any(dup_rows)) {
      logger::log_error(
        "Duplicated rows found in sheet %s: [%s]." |>
          sprintf(sht, paste0(which(dup_rows) + 1L, collapse = ", "))
      )
      return()
    }

  }

  # ---------------------------------------------------------------------------------------------------------
  # Read each of the RSI and AVE tables into data.table for looking up RSI or AVE values.
  # ---------------------------------------------------------------------------------------------------------

  # Data.table keys will be the stringified lists of all values found in each row.
  # e.g. Bgc_zone  Bgc_subzon  Bgc_vrt  Bgc_phase  Sitemc_s  Iav_strct_s  Iav_Slope_mod  Iav_Snow  Iav_Solar  RSI
  #      CDF       mm                              AS                                    1         1          0.50
  # Key would be the string "['CDF', 'mm', '', '', 'AS', '', '', '1', '1']"
  # rsi_ave_list_dict["['CDF', 'mm', '', '', 'AS', '', '', '1', '1']"] = 0.50

  for (sht in rsi_ave_sheets) {

    use_init <- sht %in% "RSI_BGC_BEU"

    logger::log_info(
      "Reading worksheet name %s to assign RSI/AVE values to ecosystem units" |>
        sprintf(sht)
    )

    k <- data[["rsi_ave"]][[sht]][["key"]]
    r <- data[["rsi_ave"]][[sht]][["range"]]
    data[["rsi_ave"]][[sht]] <- readxl::read_excel(path = template, sheet = sht, range = r) |>
      data.table::setDT(key = k)

    if (use_init) {
      data[["rsi_ave"]][[sht]] <- data.table::rbindlist(
        list(data[["rsi_ave"]][[sht]], rsi_source[,intersect(names(rsi_source), names(data[["rsi_ave"]][[sht]])), with = FALSE]),
        use.names = TRUE,
        fill = TRUE
      ) |>
        data.table::setDT(key = k)
    }

    iav_columns <- grep(pattern = "^IAV", x = k, ignore.case = TRUE, value = TRUE)
    if (any(missing_iav <- which(!iav_columns %in% all_assigned_iav_headers))) {
      logger::log_error(
        "In sheet %s, IAV field(s) named %s not found in any RSI/AVE sheets where IAV values assigned." |>
          sprintf(sht, paste0(iav_columns[missing_iav], collapse = ", "))
      )
      return()
    }

    dup_rows <- duplicated(data[["rsi_ave"]][[sht]][, k, with = FALSE])

    if (any(dup_rows)) {
      logger::log_error(
        "Duplicated rows found in sheet %s: [%s]." |>
          sprintf(sht, paste0(which(dup_rows) + 1L, collapse = ", "))
      )
      return()
    }

    # RSI/AVE values should always be numeric values, or blank, so we'll test them.
    val_pos <- length(data[["rsi_ave"]][[sht]])
    if (!is.numeric(val <- data[["rsi_ave"]][[sht]][[val_pos]])) {
      invalid <- !val %in% c("", NA) & as.numeric(val) %in% NA
      if (any(invalid)) {
        logger::log_error(
          "Values in RSI or AVE columns must be numeric or blank. Invalid value types found in sheet %s, rows : []." |>
            sprintf(
              sht,
              paste0(
                cellranger::num_to_letter(val_pos),
                which(invalid) + 1L,
                collapse = ", "
              )
            )
        )
        return()
      }
      # But if the RSI/AVE value is blank, assign a NA which will act as a flag later to not calculate.
      data.table::set(data[["rsi_ave"]][[sht]], j = val_pos, value = as.numeric(data[["rsi_ave"]][[sht]][[val_pos]]))
    }

    # Rename RSI/AVE to sheet name
    data.table::setnames(data[["rsi_ave"]][[sht]], names(data[["rsi_ave"]][[sht]])[val_pos], sht)

  }

  # ---------------------------------------------------------------------------------------------------------
  # Save a backup of the Excel file
  # ---------------------------------------------------------------------------------------------------------

  # bak_xlsx <- "%s_BACKUP_%s.xlsx" |>
  #   sprintf(
  #     tools::file_path_sans_ext(template),
  #     format(Sys.time(), "%Y%m%d_%H%M%S")
  #   )
  # logger::log_info("Creating backup Excel file %s." |> sprintf(bak_xlsx))
  # file.copy(from = template, to = bak_xlsx)

  # ---------------------------------------------------------------------------------------------------------
  # Read the RRT table(s) and calculate formula results and ratings for each row.
  # ---------------------------------------------------------------------------------------------------------

  logger::log_info("Reading RRT sheet name %s." |> sprintf(sheet_names[rrt_idx]))

  rrt_sheet <- readxl::read_excel(
    path = template,
    sheet = which(rrt_idx),
  ) |> data.table::setDT()

  data.table::setnames(rrm_input, match_labels(rrm_input, rrt_sheet))

  rrt_sheet <- data.table::rbindlist(
    list(rrt_sheet, rrm_input[,intersect(names(rrm_input), names(rrt_sheet)), with = FALSE]),
    use.names = TRUE,
    fill = TRUE
  )

  # Now read each RRT table and assign all of those IAV values from the IAV-assigning RSI/AVE table to each
  # RRT row.

  missing_lines <- list()
  for (sht in names(data[["iav"]])) {
    iav <- data[["iav"]][[sht]]
    # Making sure keys have the same type
    k <- data.table::key(iav)
    for (col in k) {
      if (!inherits(rrt_sheet[[col]], class(iav[[col]]))) {
        data.table::set(rrt_sheet, j = col, value = as(rrt_sheet[[col]], class(iav[[col]])))
      }
    }
    rrt_sheet <- iav[rrt_sheet, on = k]
    miss <- rrt_sheet[!iav, on = k]
    if (nrow(miss)) {
      missing_lines[["iav"]][[sht]] <- miss
    }
  }

  # Now for each RSI or AVE table, look up the RSI/AVE values.

  for (sht in names(data[["rsi_ave"]])) {
    rsi_ave <- data[["rsi_ave"]][[sht]]
    # Making sure keys have the same type
    k <- data.table::key(rsi_ave)
    for (col in k) {
      if (!inherits(rrt_sheet[[col]], class(rsi_ave[[col]]))) {
        data.table::set(rrt_sheet, j = col, value = as(rrt_sheet[[col]], class(rsi_ave[[col]])))
      }
    }
    rrt_sheet <- rsi_ave[rrt_sheet, on = k]
    miss <- rrt_sheet[!rsi_ave, on = k]
    if (nrow(miss)) {
      missing_lines[["rsi_ave"]][[sht]] <- miss
    }
  }

  rating <- function(x) {
    b <- c(-Inf, 0, 0.05, 0.25, 0.5, 0.75, Inf)
    cut(signif(x,digits=3), breaks = b,labels = c(6,5,4,3,2,1), right = TRUE) |> as.character() |> as.integer()
  }

  res <- list()
  for (fml in formulas) {
    rrt_sheet[, paste(
      c("RESULT", "RATING"),
      "FROM COLUMN",
      match(fml, rrt_col) |>
        cellranger::num_to_letter(),
      format(Sys.time(),"(%Y-%m-%d %H:%M:%S)")
    ) := {
      x <- eval(parse(text = fml))
      r <- rating(x)
      logger::log_info(
        "Processed %s rows. Found %s calculable ratings." |>
          sprintf(length(r), sum(!is.na(r)))
      )
      res[[fml]] <<- list("VALUE" = x, "RATING" = r)
      list(x, r)
    }]
  }

  # # Save back to xlsx file (writing back is a more CPU expensive operation in R)
  # wb <- openxlsx::loadWorkbook(template)
  #
  # openxlsx::writeData(
  #   wb = wb,
  #   sheet = which(rrt_idx),
  #   startCol = length(rrt_col) + 1L,
  #   keepNA = TRUE,
  #   na.string = "",
  #   x = rrt_sheet[,(length(rrt_sheet)-length(formulas)*2+1):length(rrt_sheet), with = FALSE]
  # )
  #
  # openxlsx::saveWorkbook(wb, template, overwrite = TRUE)

  # ---------------------------------------------------------------------------------------------------------
  # Add new rows to the RSI/AVE tables for missing combinations from the RRT table, if needed.
  # ---------------------------------------------------------------------------------------------------------

  # If there were any RRT rows with no matching rows in an IAV-assigning RSI/AVE worksheet, we will add new rows
  # to that RSI/AVE worksheet and then finish updating the spreadsheet.
  # Then the user must go into Excel and add new IAV values to these new rows in the IAV-assigning RSI/AVE worksheet.
  # The IAV-assigning RSI/AVE worksheets must be completed before we can add new rows to any IAV-referencing RSI/AVE
  # worksheets. Otherwise we would end up adding unnecessary new rows to the IAV-referencing worksheets with empty
  # strings in the IAV fields.

  # Use missing_lines, no R lib to insert rows in excel files at the moment

  dt_calc_script_elapsed <- Sys.time() - dt_calc_script_start
  logger::log_info("Script complete after %s" |> sprintf(format(dt_calc_script_elapsed)))

  structure(
    if (output == "full") rrt_sheet else {res},
    missing_lines = if (length(missing_lines)) {missing_lines},
    class = c("rrm_calc_ratings", class(rrt_sheet))
  )

}

#' Extract missing lines from the result of `rrm_calc_ratings`.
#' @param x Result of `rrm_calc_ratings`.
#' @return A list of missing lines by sheet or `NULL` if none.
#' @rdname rrm_calc_ratings
rrm_missing_lines <- function(x) {
  if (inherits(x, "rrm_calc_ratings")) {
    attr(x, "missing_lines")
  }
}
