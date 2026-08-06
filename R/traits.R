#===============================================================================
# BirdTraitsR
# Trait Analysis Functions
#
# Functions for summarizing categorical bird traits.
#===============================================================================

#------------------------------------------------------------------------------
# Trait Summary
#------------------------------------------------------------------------------

#' Summarize a Bird Trait
#'
#' Produces counts and percentages for any categorical trait.
#'
#' @param data A data frame.
#' @param trait Column name containing the trait.
#'
#' @return A BirdTraitsTrait object.
#'
#' @examples
#' birds <- data.frame(
#'   species = c("A", "B", "C"),
#'   diet = c("Insectivore", "Omnivore", "Carnivore"),
#'   stringsAsFactors = FALSE
#' )
#'
#' bt_trait_summary(birds, "diet")
#'
#' @export

bt_trait_summary <- function(data, trait){
  
  bt_check_dataframe(data)
  
  if(!trait %in% names(data)){
    stop(
      paste0(
        "Column '",
        trait,
        "' not found."
      ),
      call. = FALSE
    )
  }
  
  values <- data[[trait]]
  
  counts <- table(values, useNA = "ifany")
  
  output <- data.frame(
    Trait = names(counts),
    Count = as.integer(counts),
    Percentage = round(
      as.integer(counts) /
        sum(counts) * 100,
      2
    ),
    stringsAsFactors = FALSE
  )
  
  output <- output[
    order(output$Count, decreasing = TRUE),
  ]
  
  result <- list(
    
    trait = trait,
    
    total_species = nrow(data),
    
    n_levels = nrow(output),
    
    summary = output
    
  )
  
  class(result) <- "BirdTraitsTrait"
  
  result
  
}

#------------------------------------------------------------------------------
# Print Method
#------------------------------------------------------------------------------

#' @export

print.BirdTraitsTrait <- function(x,...){
  
  cat("\n")
  
  cat("=========================================\n")
  cat(" Bird Trait Summary\n")
  cat("=========================================\n\n")
  
  cat("Trait           :",x$trait,"\n")
  cat("Species         :",x$total_species,"\n")
  cat("Trait Levels    :",x$n_levels,"\n\n")
  
  print(
    x$summary,
    row.names = FALSE
  )
  
  invisible(x)
  
}

#------------------------------------------------------------------------------
# Trait Frequency
#------------------------------------------------------------------------------

#' Trait Frequency
#'
#' Returns only frequency counts.
#'
#' @param data Data frame.
#' @param trait Trait column.
#'
#' @return Data frame.
#'
#' @export

bt_trait_frequency <- function(data,trait){
  
  bt_trait_summary(
    data,
    trait
  )$summary[,c("Trait","Count")]
  
}

#------------------------------------------------------------------------------
# Trait Percentage
#------------------------------------------------------------------------------

#' Trait Percentage
#'
#' Returns percentages only.
#'
#' @param data Data frame.
#' @param trait Trait column.
#'
#' @return Data frame.
#'
#' @export

bt_trait_percentage <- function(data,trait){
  
  bt_trait_summary(
    data,
    trait
  )$summary[,c("Trait","Percentage")]
  
}

#------------------------------------------------------------------------------
# Trait Levels
#------------------------------------------------------------------------------

#' Trait Levels
#'
#' Lists unique levels of a trait.
#'
#' @param data Data frame.
#' @param trait Trait column.
#'
#' @return Character vector.
#'
#' @export

bt_trait_levels <- function(data,trait){
  
  bt_check_dataframe(data)
  
  if(!trait %in% names(data)){
    stop(
      paste0(
        "Column '",
        trait,
        "' not found."
      ),
      call. = FALSE
    )
  }
  
  sort(
    unique(
      data[[trait]]
    )
  )
  
}

#------------------------------------------------------------------------------
# Missing Trait Values
#------------------------------------------------------------------------------

#' Missing Trait Values
#'
#' Counts missing values.
#'
#' @param data Data frame.
#' @param trait Trait column.
#'
#' @return Integer.
#'
#' @export

bt_trait_missing <- function(data,trait){
  
  bt_check_dataframe(data)
  
  if(!trait %in% names(data)){
    stop(
      paste0(
        "Column '",
        trait,
        "' not found."
      ),
      call. = FALSE
    )
  }
  
  sum(
    is.na(
      data[[trait]]
    )
  )
  
}
