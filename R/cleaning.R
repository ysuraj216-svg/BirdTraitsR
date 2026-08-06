#===============================================================================
# BirdTraitsR
# Data Cleaning Functions
#
# Core functions for cleaning bird trait and ecological datasets.
#===============================================================================

#------------------------------------------------------------------------------
# Clean Dataset
#------------------------------------------------------------------------------

#' Clean Bird Dataset
#'
#' Cleans bird ecological datasets by:
#'
#' * removing empty rows
#' * trimming whitespace
#' * converting empty strings to NA
#' * optionally replacing NA values
#'
#' @param data A data frame.
#' @param replace_na Logical. Replace NA values?
#' @param na_value Value used when replacing missing values.
#'
#' @return A cleaned data frame.
#'
#' @examples
#' birds <- data.frame(
#'   species = c(" Robin", "Sparrow ", ""),
#'   mass = c(20, NA, NA),
#'   stringsAsFactors = FALSE
#' )
#'
#' birds <- bt_clean_data(birds)
#'
#' @export

bt_clean_data <- function(
    data,
    replace_na = FALSE,
    na_value = "Unknown"
){
  
  bt_check_dataframe(data)
  
  data <- bt_remove_empty(data)
  
  data <- bt_clean_strings(data)
  
  if(replace_na){
    
    data <- bt_standardize_missing(
      data,
      value = na_value
    )
    
  }
  
  rownames(data) <- NULL
  
  data
  
}

#------------------------------------------------------------------------------
# Remove Empty Rows
#------------------------------------------------------------------------------

#' Remove Empty Rows
#'
#' Removes rows containing only missing values.
#'
#' @param data Data frame.
#'
#' @return Cleaned data frame.
#'
#' @export

bt_remove_empty <- function(data){
  
  bt_check_dataframe(data)
  
  data[!apply(is.na(data),1,all), , drop = FALSE]
  
}

#------------------------------------------------------------------------------
# Trim Character Columns
#------------------------------------------------------------------------------

#' Clean Character Strings
#'
#' Removes leading/trailing spaces and converts empty strings to NA.
#'
#' @param data Data frame.
#'
#' @return Cleaned data frame.
#'
#' @export

bt_clean_strings <- function(data){
  
  bt_check_dataframe(data)
  
  character_cols <- sapply(data,is.character)
  
  data[character_cols] <-
    
    lapply(
      
      data[character_cols],
      
      function(x){
        
        x <- trimws(x)
        
        x[x==""] <- NA
        
        x
        
      }
      
    )
  
  data
  
}

#------------------------------------------------------------------------------
# Replace Missing Values
#------------------------------------------------------------------------------

#' Replace Missing Values
#'
#' Replaces missing values in character columns.
#'
#' @param data Data frame.
#' @param value Replacement value.
#'
#' @return Cleaned data frame.
#'
#' @export

bt_standardize_missing <- function(
    data,
    value = "Unknown"
){
  
  bt_check_dataframe(data)
  
  character_cols <- sapply(data,is.character)
  
  data[character_cols] <-
    
    lapply(
      
      data[character_cols],
      
      function(x){
        
        x[is.na(x)] <- value
        
        x
        
      }
      
    )
  
  data
  
}

#------------------------------------------------------------------------------
# Clean Column Names
#------------------------------------------------------------------------------

#' Clean Column Names
#'
#' Standardizes column names to snake_case.
#'
#' @param data Data frame.
#'
#' @return Data frame.
#'
#' @export

bt_clean_names <- function(data){
  
  bt_check_dataframe(data)
  
  names(data) <-
    gsub(
      " ",
      "_",
      tolower(trimws(names(data)))
    )
  
  data
  
}
