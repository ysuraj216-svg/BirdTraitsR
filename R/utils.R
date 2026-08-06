#===============================================================================
# BirdTraitsR
# Utility Functions
#===============================================================================

#' Check Data Frame
#'
#' Internal helper that verifies the input is a data frame.
#'
#' @param data Object supplied by the user.
#'
#' @return Invisibly returns TRUE.
#'
#' @keywords internal

bt_check_dataframe <- function(data){
  
  if(!is.data.frame(data)){
    
    stop(
      
      "Input must be a data.frame.",
      
      call. = FALSE
      
    )
    
  }
  
  invisible(TRUE)
  
}
#-------------------------------------------------------------------------------
#' Check Community Matrix
#'
#' Internal helper that verifies the input is a community matrix.
#'
#' @param data Object supplied by the user.
#'
#' @return Invisibly returns TRUE.
#'
#' @keywords internal

bt_check_community <- function(data){
  
  if(!(is.matrix(data) || is.data.frame(data))){
    
    stop(
      
      "Input must be a matrix or data.frame.",
      
      call. = FALSE
      
    )
    
  }
  
  if(!all(
    vapply(
      as.data.frame(data),
      is.numeric,
      logical(1)
    )
  )){
    
    stop(
      
      "Community matrix must contain only numeric abundance values.",
      
      call. = FALSE
      
    )
    
  }
  
  invisible(TRUE)
  
}
#-------------------------------------------------------------------------------
#' Check Required Columns
#'
#' Internal helper that checks required columns exist.
#'
#' @param data Data frame.
#' @param required Character vector.
#'
#' @return Invisibly returns TRUE.
#'
#' @keywords internal

bt_check_columns <- function(data, required){
  
  missing <- setdiff(required, names(data))
  
  if(length(missing) > 0){
    
    stop(
      
      paste(
        
        "Missing required columns:",
        
        paste(missing, collapse = ", ")
        
      ),
      
      call. = FALSE
      
    )
    
  }
  
  invisible(TRUE)
  
}
#-------------------------------------------------------------------------------
#' Check Numeric Variable
#'
#' @keywords internal

bt_check_numeric <- function(x, name){
  
  if(!is.numeric(x)){
    
    stop(
      
      paste(name, "must be numeric."),
      
      call. = FALSE
      
    )
    
  }
  
  invisible(TRUE)
  
}
#-------------------------------------------------------------------------------
#' BirdTraitsR Message
#'
#' @keywords internal

bt_message <- function(...){
  
  message(
    
    "[BirdTraitsR] ",
    
    paste(...)
    
  )
  
}
#-------------------------------------------------------------------------------
#' BirdTraitsR Stop
#'
#' @keywords internal

bt_stop <- function(...){
  
  stop(
    
    "[BirdTraitsR] ",
    
    paste(...),
    
    call. = FALSE
    
  )
  
}
#-------------------------------------------------------------------------------
# Global Variables
#-------------------------------------------------------------------------------

#' @importFrom rlang .data
NULL