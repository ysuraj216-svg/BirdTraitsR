#' Print Validation Summary
#'
#' @param x Validation object
#' @param ... ignored
#'
#' @export

print.BirdTraitsValidation <- function(x, ...){
  
  cat("\n")
  cat("=================================\n")
  cat(" BirdTraitsR Dataset Validation\n")
  cat("=================================\n\n")
  
  cat("Rows              :", x$n_rows, "\n")
  cat("Columns           :", x$n_columns, "\n")
  cat("Duplicate rows    :", x$duplicate_rows, "\n")
  cat("Missing values    :", x$missing_values, "\n")
  cat("Empty strings     :", x$empty_strings, "\n")
  
  if(length(x$missing_columns) > 0){
    
    cat("\nMissing Columns:\n")
    
    print(x$missing_columns)
    
  } else{
    
    cat("\nRequired columns: OK\n")
    
  }
  
  invisible(x)
  
}