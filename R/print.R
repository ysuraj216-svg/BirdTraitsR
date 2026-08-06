#===============================================================================
# BirdTraitsR
# Print Methods for Custom S3 Classes
#
# Pretty printing for BirdTraitsR output objects.
#
# NOTE: print.BirdTraitsSummary, print.BirdTraitsValidation, and
# print.BirdTraitsDiversity are intentionally NOT defined here. They already
# exist in summary.R, print_BirdTraitsValidation.R, and diversity.R
# respectively. Defining them again here would create duplicate S3 methods.
#===============================================================================

#------------------------------------------------------------------------------
# Print BirdTraitsReport
#------------------------------------------------------------------------------

#' Print Method for BirdTraitsReport
#'
#' Custom print method for BirdTraitsReport objects.
#'
#' @param x A BirdTraitsReport character vector.
#' @param ... Additional arguments (unused).
#'
#' @return Invisibly returns x.
#'
#' @export

print.BirdTraitsReport <- function(x, ...){
  
  if(!inherits(x, "BirdTraitsReport")){
    stop("Object must be of class BirdTraitsReport.", call. = FALSE)
  }
  
  # Simply print the character vector (already formatted)
  cat(paste(x, collapse = "\n"))
  cat("\n\n")
  
  invisible(x)
  
}

#------------------------------------------------------------------------------
# Print BirdTraitsConservation
#------------------------------------------------------------------------------

#' Print Method for BirdTraitsConservation
#'
#' Custom print method for BirdTraitsConservation objects.
#'
#' @param x A BirdTraitsConservation data frame.
#' @param ... Additional arguments (unused).
#'
#' @return Invisibly returns x.
#'
#' @export

print.BirdTraitsConservation <- function(x, ...){
  
  if(!inherits(x, "BirdTraitsConservation")){
    stop("Object must be of class BirdTraitsConservation.", call. = FALSE)
  }
  
  cat("\n")
  cat(strrep("=", 70), "\n")
  cat("Conservation Status Summary\n")
  cat(strrep("=", 70), "\n")
  cat("\n")
  
  # Print the data frame
  print(as.data.frame(x), row.names = FALSE)
  
  cat("\n")
  cat(strrep("=", 70), "\n")
  cat("\n")
  
  invisible(x)
  
}

#------------------------------------------------------------------------------
# Print Generic Summary Methods
#
# These forward to whichever print.* method already exists for the class
# (defined in other files). Safe to keep - no duplication, since S3 dispatch
# doesn't care which file a method lives in.
#------------------------------------------------------------------------------

#' Summary Method for BirdTraitsSummary Objects
#'
#' @param object A BirdTraitsSummary object.
#' @param ... Additional arguments (unused).
#'
#' @return Invisibly returns object.
#'
#' @export

summary.BirdTraitsSummary <- function(object, ...){
  print(object, ...)
}

#' Summary Method for BirdTraitsValidation Objects
#'
#' @param object A BirdTraitsValidation object.
#' @param ... Additional arguments (unused).
#'
#' @return Invisibly returns object.
#'
#' @export

summary.BirdTraitsValidation <- function(object, ...){
  print(object, ...)
}

#' Summary Method for BirdTraitsReport Objects
#'
#' @param object A BirdTraitsReport object.
#' @param ... Additional arguments (unused).
#'
#' @return Invisibly returns object.
#'
#' @export

summary.BirdTraitsReport <- function(object, ...){
  print(object, ...)
}

#' Summary Method for BirdTraitsConservation Objects
#'
#' @param object A BirdTraitsConservation object.
#' @param ... Additional arguments (unused).
#'
#' @return Invisibly returns object.
#'
#' @export

summary.BirdTraitsConservation <- function(object, ...){
  print(object, ...)
}

#' Summary Method for BirdTraitsDiversity Objects
#'
#' @param object A BirdTraitsDiversity object.
#' @param ... Additional arguments (unused).
#'
#' @return Invisibly returns object.
#'
#' @export

summary.BirdTraitsDiversity <- function(object, ...){
  print(object, ...)
}