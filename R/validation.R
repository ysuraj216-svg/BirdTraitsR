#===============================================================================
# BirdTraitsR
# Data Validation Functions
#
# Validate bird ecology datasets before analysis.
#===============================================================================

#------------------------------------------------------------------------------
# Validate Species Column
#------------------------------------------------------------------------------

#' Validate Species Names
#'
#' Checks that the species column exists, contains character or factor
#' values, and is not entirely missing.
#'
#' @param data Data frame.
#' @param species Column name containing species names.
#'
#' @return Logical. Returns TRUE if valid, stops with error message if not.
#'
#' @examples
#' birds <- data.frame(
#'   species = c("Corvus corone", "Pica pica", "Corvus corone"),
#'   mass = c(500, 450, 500)
#' )
#'
#' bt_validate_species(birds, "species")
#'
#' @export

bt_validate_species <- function(
    data,
    species = "species"
){
  
  bt_check_dataframe(data)
  
  if(!species %in% names(data)){
    stop(
      paste0("Column '", species, "' not found."),
      call. = FALSE
    )
  }
  
  if(!is.character(data[[species]]) && !is.factor(data[[species]])){
    stop(
      "Species column must be character or factor.",
      call. = FALSE
    )
  }
  
  if(all(is.na(data[[species]]))){
    stop(
      "Species column contains only missing values.",
      call. = FALSE
    )
  }
  
  invisible(TRUE)
  
}

#------------------------------------------------------------------------------
# Validate Date Column
#------------------------------------------------------------------------------

#' Validate Date Column
#'
#' Checks that date column is properly formatted and contains valid dates.
#'
#' @param data Data frame.
#' @param date Column name containing dates.
#'
#' @return Logical. Returns TRUE if valid, stops with error message if not.
#'
#' @examples
#' birds <- data.frame(
#'   species = c("A", "B", "C"),
#'   survey_date = as.Date(c("2023-01-01", "2023-01-02", "2023-01-03"))
#' )
#'
#' bt_validate_dates(birds, "survey_date")
#'
#' @export

bt_validate_dates <- function(
    data,
    date
){
  
  bt_check_dataframe(data)
  
  if(!date %in% names(data)){
    stop(
      paste0("Column '", date, "' not found."),
      call. = FALSE
    )
  }
  
  if(!inherits(data[[date]], "Date") && 
     !inherits(data[[date]], "POSIXct")){
    stop(
      "Date column must be Date or POSIXct class.",
      call. = FALSE
    )
  }
  
  if(all(is.na(data[[date]]))){
    stop(
      "Date column contains only missing values.",
      call. = FALSE
    )
  }
  
  invisible(TRUE)
  
}

#------------------------------------------------------------------------------
# Validate Coordinates
#------------------------------------------------------------------------------

#' Validate Geographic Coordinates
#'
#' Checks that latitude and longitude columns are valid and within
#' acceptable ranges.
#'
#' @param data Data frame.
#' @param latitude Latitude column name.
#' @param longitude Longitude column name.
#'
#' @return Logical. Returns TRUE if valid, stops with error message if not.
#'
#' @examples
#' birds <- data.frame(
#'   species = c("A", "B", "C"),
#'   latitude = c(51.5, 52.0, 51.8),
#'   longitude = c(-0.1, -0.5, -0.2)
#' )
#'
#' bt_validate_coordinates(birds, "latitude", "longitude")
#'
#' @export

bt_validate_coordinates <- function(
    data,
    latitude = "latitude",
    longitude = "longitude"
){
  
  bt_check_dataframe(data)
  
  if(!latitude %in% names(data)){
    stop(
      paste0("Column '", latitude, "' not found."),
      call. = FALSE
    )
  }
  
  if(!longitude %in% names(data)){
    stop(
      paste0("Column '", longitude, "' not found."),
      call. = FALSE
    )
  }
  
  if(!is.numeric(data[[latitude]])){
    stop(
      "Latitude column must be numeric.",
      call. = FALSE
    )
  }
  
  if(!is.numeric(data[[longitude]])){
    stop(
      "Longitude column must be numeric.",
      call. = FALSE
    )
  }
  
  if(any(data[[latitude]] < -90 | data[[latitude]] > 90, na.rm = TRUE)){
    stop(
      "Latitude values must be between -90 and 90.",
      call. = FALSE
    )
  }
  
  if(any(data[[longitude]] < -180 | data[[longitude]] > 180, na.rm = TRUE)){
    stop(
      "Longitude values must be between -180 and 180.",
      call. = FALSE
    )
  }
  
  invisible(TRUE)
  
}

#------------------------------------------------------------------------------
# Validate IUCN Status
#------------------------------------------------------------------------------

#' Validate IUCN Conservation Status
#'
#' Checks that IUCN status column contains only valid categories.
#'
#' @param data Data frame.
#' @param iucn Column name containing IUCN status.
#'
#' @return Logical. Returns TRUE if valid, stops with error message if not.
#'
#' @examples
#' birds <- data.frame(
#'   species = c("A", "B", "C"),
#'   iucn = c("LC", "VU", "EN")
#' )
#'
#' bt_validate_iucn(birds, "iucn")
#'
#' @export

bt_validate_iucn <- function(
    data,
    iucn = "iucn_status"
){
  
  bt_check_dataframe(data)
  
  if(!iucn %in% names(data)){
    stop(
      paste0("Column '", iucn, "' not found."),
      call. = FALSE
    )
  }
  
  valid_status <- c(
    "EX", "EW",          # Extinct
    "CR", "EN", "VU",    # Threatened
    "NT", "LC",          # Not threatened
    "DD",                # Data deficient
    "NE"                 # Not evaluated
  )
  
  invalid <- data[[iucn]][!data[[iucn]] %in% valid_status & !is.na(data[[iucn]])]
  
  if(length(invalid) > 0){
    stop(
      paste0(
        "Invalid IUCN status values found: ",
        paste(unique(invalid), collapse = ", "),
        ". Valid values: ", paste(valid_status, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  
  invisible(TRUE)
  
}

#------------------------------------------------------------------------------
# Validate Complete Dataset
#------------------------------------------------------------------------------

#' Validate Complete Dataset
#'
#' Runs comprehensive validation checks on the entire dataset including
#' required columns, data types, and value ranges.
#'
#' @param data Data frame.
#' @param required_columns Character vector of required column names.
#'
#' @return List with validation results:
#'   - $valid: Logical. TRUE if all checks pass.
#'   - $errors: Character vector of error messages.
#'   - $warnings: Character vector of warning messages.
#'   - $summary: Data frame with basic dataset statistics.
#'
#' @examples
#' birds <- data.frame(
#'   species = c("A", "B", "C"),
#'   mass = c(20, 25, 30),
#'   survey_date = as.Date(c("2023-01-01", "2023-01-02", "2023-01-03"))
#' )
#'
#' result <- bt_validate_dataset(
#'   birds,
#'   required_columns = c("species", "mass", "survey_date")
#' )
#'
#' @export

bt_validate_dataset <- function(
    data,
    required_columns = NULL
){
  
  bt_check_dataframe(data)
  
  errors <- c()
  warnings_list <- c()
  
  # Check required columns
  if(!is.null(required_columns)){
    missing <- setdiff(required_columns, names(data))
    if(length(missing) > 0){
      errors <- c(errors, paste0(
        "Missing required columns: ",
        paste(missing, collapse = ", ")
      ))
    }
  }
  
  # Check for completely empty rows
  empty_rows <- which(rowSums(is.na(data)) == ncol(data))
  if(length(empty_rows) > 0){
    warnings_list <- c(warnings_list, paste0(
      "Found ", length(empty_rows), " completely empty rows"
    ))
  }
  
  # Check for completely empty columns
  empty_cols <- which(colSums(is.na(data)) == nrow(data))
  if(length(empty_cols) > 0){
    warnings_list <- c(warnings_list, paste0(
      "Found ", length(empty_cols), " completely empty columns"
    ))
  }
  
  # Check for high missing data
  missing_pct <- colSums(is.na(data)) / nrow(data) * 100
  high_missing <- names(data)[missing_pct > 50]
  if(length(high_missing) > 0){
    warnings_list <- c(warnings_list, paste0(
      "Columns with >50% missing data: ",
      paste(high_missing, collapse = ", ")
    ))
  }
  
  # Create summary
  summary_df <- data.frame(
    Metric = c("Rows", "Columns", "Complete Cases", "Missing Cells %"),
    Value = c(
      nrow(data),
      ncol(data),
      nrow(stats::na.omit(data)),
      round(sum(is.na(data)) / (nrow(data) * ncol(data)) * 100, 2)
    )
  )
  
  list(
    valid = length(errors) == 0,
    errors = errors,
    warnings = warnings_list,
    summary = summary_df
  )
  
}