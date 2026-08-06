#===============================================================================
# BirdTraitsR
# Dataset Summary Functions
#
# Quick summaries of bird ecology datasets.
#===============================================================================

#------------------------------------------------------------------------------
# Dataset Summary
#------------------------------------------------------------------------------

#' Bird Dataset Summary
#'
#' Produces a concise summary of a bird dataset.
#'
#' @param data A data frame.
#' @param species_col Species column.
#' @param family_col Family column.
#' @param order_col Order column.
#'
#' @return BirdTraitsSummary object.
#'
#' @examples
#' birds <- data.frame(
#'   species=c("A","B","C","A"),
#'   family=c("F1","F1","F2","F1"),
#'   order=c("O1","O1","O2","O1"),
#'   stringsAsFactors=FALSE
#' )
#'
#' bt_dataset_summary(birds)
#'
#' @export

bt_dataset_summary <- function(
    data,
    species_col="species",
    family_col="family",
    order_col="order"
){
  
  bt_check_dataframe(data)
  
  result <- list(
    
    records = nrow(data),
    
    variables = ncol(data),
    
    species =
      if(species_col %in% names(data))
        length(unique(data[[species_col]]))
    else
      NA,
    
    families =
      if(family_col %in% names(data))
        length(unique(data[[family_col]]))
    else
      NA,
    
    orders =
      if(order_col %in% names(data))
        length(unique(data[[order_col]]))
    else
      NA,
    
    missing =
      sum(is.na(data))
    
  )
  
  class(result) <- "BirdTraitsSummary"
  
  result
  
}
#------------------------------------------------------------------------------
# Print Method
#------------------------------------------------------------------------------

#' @export

print.BirdTraitsSummary <- function(x,...){
  
  cat("\n")
  
  cat("=====================================\n")
  cat(" BirdTraitsR Dataset Summary\n")
  cat("=====================================\n\n")
  
  cat("Records        :",x$records,"\n")
  cat("Variables      :",x$variables,"\n")
  cat("Species        :",x$species,"\n")
  cat("Families       :",x$families,"\n")
  cat("Orders         :",x$orders,"\n")
  cat("Missing Values :",x$missing,"\n")
  
  invisible(x)
  
}
#===============================================================================
# BirdTraitsR
# Dataset Summary Functions (Part 2)
#
# Column summaries and dataset inspection tools.
#===============================================================================

#------------------------------------------------------------------------------
# Column Summary
#------------------------------------------------------------------------------

#' Column Summary
#'
#' Returns a summary of a single column.
#'
#' @param data A data frame.
#' @param column Column name.
#'
#' @return List.
#'
#' @examples
#' birds <- data.frame(
#'   species = c("A","B","C","A"),
#'   stringsAsFactors = FALSE
#' )
#'
#' bt_column_summary(birds, "species")
#'
#' @export

bt_column_summary <- function(data, column){
  
  bt_check_dataframe(data)
  
  if(!column %in% names(data)){
    stop(
      paste0("Column '", column, "' not found."),
      call. = FALSE
    )
  }
  
  x <- data[[column]]
  
  list(
    
    Column = column,
    
    Class = class(x),
    
    Length = length(x),
    
    Missing = sum(is.na(x)),
    
    Unique = length(unique(x))
    
  )
  
}


#------------------------------------------------------------------------------
# Numeric Summary
#------------------------------------------------------------------------------

#' Numeric Summary
#'
#' Returns descriptive statistics for a numeric column.
#'
#' @param data A data frame.
#' @param column Numeric column.
#'
#' @return Data frame.
#'
#' @examples
#' birds <- data.frame(
#'   mass = c(20,25,18,30,27)
#' )
#'
#' bt_numeric_summary(birds, "mass")
#'
#' @export

bt_numeric_summary <- function(data, column){
  
  bt_check_dataframe(data)
  
  if(!column %in% names(data)){
    stop(
      paste0("Column '", column, "' not found."),
      call. = FALSE
    )
  }
  
  x <- data[[column]]
  
  if(!is.numeric(x)){
    stop(
      "Selected column is not numeric.",
      call. = FALSE
    )
  }
  
  data.frame(
    
    Minimum = min(x, na.rm = TRUE),
    
    Maximum = max(x, na.rm = TRUE),
    
    Mean = mean(x, na.rm = TRUE),
    
    
    Median = stats::median(x, na.rm = TRUE),
    
    SD = stats::sd(x, na.rm = TRUE),
    
    Missing = sum(is.na(x))
    
  )
  
}


#------------------------------------------------------------------------------
# Character Summary
#------------------------------------------------------------------------------

#' Character Summary
#'
#' Returns summary information for a character or factor column.
#'
#' @param data A data frame.
#' @param column Character column.
#'
#' @return List.
#'
#' @examples
#' birds <- data.frame(
#'   diet = c(
#'     "Insectivore",
#'     "Omnivore",
#'     "Omnivore",
#'     "Carnivore"
#'   ),
#'   stringsAsFactors = FALSE
#' )
#'
#' bt_character_summary(birds, "diet")
#'
#' @export

bt_character_summary <- function(data, column){
  
  bt_check_dataframe(data)
  
  if(!column %in% names(data)){
    stop(
      paste0("Column '", column, "' not found."),
      call. = FALSE
    )
  }
  
  x <- data[[column]]
  
  if(!(is.character(x) || is.factor(x))){
    stop(
      "Selected column must be character or factor.",
      call. = FALSE
    )
  }
  
  freq <- table(x, useNA = "ifany")
  
  mode_value <- names(freq)[which.max(freq)]
  
  list(
    
    Column = column,
    
    Class = class(x),
    
    Categories = length(unique(x)),
    
    Missing = sum(is.na(x)),
    
    Most_Frequent = mode_value,
    
    Frequency = max(freq)
    
  )
  
}
#===============================================================================
# BirdTraitsR
# Dataset Summary Functions (Part 3)
#
# Dataset structure inspection
#===============================================================================

#------------------------------------------------------------------------------
# Column Classes
#------------------------------------------------------------------------------

#' Column Classes
#'
#' Returns the class of every column in the dataset.
#'
#' @param data A data frame.
#'
#' @return Data frame.
#'
#' @examples
#' birds <- data.frame(
#'   species = c("A","B"),
#'   mass = c(20,25),
#'   stringsAsFactors = FALSE
#' )
#'
#' bt_column_classes(birds)
#'
#' @export

bt_column_classes <- function(data){
  
  bt_check_dataframe(data)
  
  data.frame(
    
    Column = names(data),
    
    Class = vapply(
      data,
      function(x) class(x)[1],
      character(1)
    ),
    
    stringsAsFactors = FALSE
    
  )
  
}


#------------------------------------------------------------------------------
# Numeric Columns
#------------------------------------------------------------------------------

#' Numeric Columns
#'
#' Returns the names of numeric columns.
#'
#' @param data A data frame.
#'
#' @return Character vector.
#'
#' @examples
#' birds <- data.frame(
#'   mass = c(20,25),
#'   wing = c(10,11)
#' )
#'
#' bt_numeric_columns(birds)
#'
#' @export

bt_numeric_columns <- function(data){
  
  bt_check_dataframe(data)
  
  names(data)[
    vapply(data, is.numeric, logical(1))
  ]
  
}


#------------------------------------------------------------------------------
# Character Columns
#------------------------------------------------------------------------------

#' Character Columns
#'
#' Returns the names of character columns.
#'
#' @param data A data frame.
#'
#' @return Character vector.
#'
#' @examples
#' birds <- data.frame(
#'   species = c("A","B"),
#'   diet = c("Omnivore","Carnivore"),
#'   stringsAsFactors = FALSE
#' )
#'
#' bt_character_columns(birds)
#'
#' @export

bt_character_columns <- function(data){
  
  bt_check_dataframe(data)
  
  names(data)[
    vapply(data, is.character, logical(1))
  ]
  
}


#------------------------------------------------------------------------------
# Factor Columns
#------------------------------------------------------------------------------

#' Factor Columns
#'
#' Returns the names of factor columns.
#'
#' @param data A data frame.
#'
#' @return Character vector.
#'
#' @examples
#' birds <- data.frame(
#'   status = factor(c("LC","VU"))
#' )
#'
#' bt_factor_columns(birds)
#'
#' @export

bt_factor_columns <- function(data){
  
  bt_check_dataframe(data)
  
  names(data)[
    vapply(data, is.factor, logical(1))
  ]
  
}


#------------------------------------------------------------------------------
# Logical Columns
#------------------------------------------------------------------------------

#' Logical Columns
#'
#' Returns the names of logical columns.
#'
#' @param data A data frame.
#'
#' @return Character vector.
#'
#' @examples
#' birds <- data.frame(
#'   migratory = c(TRUE,FALSE)
#' )
#'
#' bt_logical_columns(birds)
#'
#' @export

bt_logical_columns <- function(data){
  
  bt_check_dataframe(data)
  
  names(data)[
    vapply(data, is.logical, logical(1))
  ]
  
}


#------------------------------------------------------------------------------
# Date Columns
#------------------------------------------------------------------------------

#' Date Columns
#'
#' Returns the names of Date columns.
#'
#' @param data A data frame.
#'
#' @return Character vector.
#'
#' @examples
#' birds <- data.frame(
#'   survey_date = as.Date(c("2024-01-01","2024-01-02"))
#' )
#'
#' bt_date_columns(birds)
#'
#' @export

bt_date_columns <- function(data){
  
  bt_check_dataframe(data)
  
  names(data)[
    vapply(
      data,
      function(x) inherits(x, "Date"),
      logical(1)
    )
  ]
  
}
#------------------------------------------------------------------------------
# Variable Types
#------------------------------------------------------------------------------

#' Variable Types
#'
#' Counts variables by data type.
#'
#' @param data A data frame.
#'
#' @return Data frame.
#'
#' @examples
#' birds <- data.frame(
#'   species = c("A","B"),
#'   mass = c(20,25),
#'   status = factor(c("LC","VU")),
#'   migratory = c(TRUE,FALSE),
#'   survey_date = as.Date(c("2024-01-01","2024-01-02")),
#'   stringsAsFactors = FALSE
#' )
#'
#' bt_variable_types(birds)
#'
#' @export

bt_variable_types <- function(data){
  
  bt_check_dataframe(data)
  
  classes <- vapply(
    data,
    function(x) class(x)[1],
    character(1)
  )
  
  data.frame(
    
    Type = c(
      "character",
      "numeric",
      "factor",
      "logical",
      "Date",
      "other"
    ),
    
    Count = c(
      
      sum(classes == "character"),
      
      sum(classes == "numeric"),
      
      sum(classes == "factor"),
      
      sum(classes == "logical"),
      
      sum(classes == "Date"),
      
      sum(!classes %in%
            c(
              "character",
              "numeric",
              "factor",
              "logical",
              "Date"
            ))
      
    ),
    
    stringsAsFactors = FALSE
    
  )
  
}
#------------------------------------------------------------------------------
# Data Dictionary
#------------------------------------------------------------------------------

#' Data Dictionary
#'
#' Creates a simple data dictionary.
#'
#' @param data A data frame.
#'
#' @return Data frame.
#'
#' @examples
#' birds <- data.frame(
#'   species = c("A","B"),
#'   mass = c(20,25),
#'   stringsAsFactors = FALSE
#' )
#'
#' bt_data_dictionary(birds)
#'
#' @export

bt_data_dictionary <- function(data){
  
  bt_check_dataframe(data)
  
  data.frame(
    
    Variable = names(data),
    
    Class = vapply(
      data,
      function(x) class(x)[1],
      character(1)
    ),
    
    Missing = vapply(
      data,
      function(x) sum(is.na(x)),
      integer(1)
    ),
    
    Unique = vapply(
      data,
      function(x) length(unique(x)),
      integer(1)
    ),
    
    Example = vapply(
      data,
      function(x){
        
        x <- x[!is.na(x)]
        
        if(length(x)==0)
          return(NA_character_)
        
        as.character(x[1])
        
      },
      
      character(1)
      
    ),
    
    stringsAsFactors = FALSE
    
  )
  
}
#------------------------------------------------------------------------------
# Glimpse
#------------------------------------------------------------------------------

#' Glimpse Dataset
#'
#' Displays a compact overview of a dataset.
#'
#' @param data A data frame.
#'
#' @return Invisibly returns the data.
#'
#' @examples
#' birds <- data.frame(
#'   species = c("A","B"),
#'   mass = c(20,25),
#'   stringsAsFactors = FALSE
#' )
#'
#' bt_glimpse(birds)
#'
#' @export

bt_glimpse <- function(data){
  
  bt_check_dataframe(data)
  
  cat("\n")
  
  cat("Rows    :", nrow(data), "\n")
  cat("Columns :", ncol(data), "\n\n")
  
  for(i in seq_along(data)){
    
    cat(
      
      names(data)[i],
      
      "<",
      
      class(data[[i]])[1],
      
      "> : "
      
    )
    
    values <- utils::head(
      unique(data[[i]]),
      5
    )
    
    cat(
      
      paste(values, collapse=", "),
      
      "\n"
      
    )
    
  }
  
  invisible(data)
  
}
#------------------------------------------------------------------------------
# Summary Report
#------------------------------------------------------------------------------

#' Summary Report
#'
#' Produces a complete summary report.
#'
#' @param data A data frame.
#'
#' @return Invisibly returns TRUE.
#'
#' @examples
#' birds <- data.frame(
#'   species=c("A","B"),
#'   family=c("F1","F1"),
#'   order=c("O1","O1"),
#'   stringsAsFactors=FALSE
#' )
#'
#' bt_summary_report(birds)
#'
#' @export

bt_summary_report <- function(data){
  
  bt_check_dataframe(data)
  
  print(
    bt_dataset_summary(data)
  )
  
  cat("\n")
  
  cat("---------------------------------------\n")
  cat("Variable Types\n")
  cat("---------------------------------------\n")
  
  print(
    bt_variable_types(data)
  )
  
  cat("\n")
  
  cat("---------------------------------------\n")
  cat("Data Dictionary\n")
  cat("---------------------------------------\n")
  
  print(
    bt_data_dictionary(data)
  )
  
  invisible(TRUE)
  
}