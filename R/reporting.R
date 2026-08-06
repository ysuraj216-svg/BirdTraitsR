#===============================================================================
# BirdTraitsR
# Reporting Functions
#
# Generate publication-ready reports from bird ecology datasets.
#===============================================================================

#------------------------------------------------------------------------------
# Create Basic Report
#------------------------------------------------------------------------------

#' Create Data Report
#'
#' Generates a formatted report summarizing the dataset including dimensions,
#' data types, missing values, and basic statistics.
#'
#' @param data Data frame.
#' @param title Title for the report.
#' @param show_summary Logical. Include summary statistics?
#' @param show_missing Logical. Include missing data analysis?
#'
#' @return Character vector containing formatted report text.
#'
#' @examples
#' birds <- data.frame(
#'   species = c("A", "B", "C"),
#'   mass = c(20, 25, 30),
#'   habitat = c("Forest", "Grassland", "Forest")
#' )
#'
#' report <- bt_create_report(birds, title = "Bird Survey Report")
#' cat(report, sep = "\n")
#'
#' @export

bt_create_report <- function(
    data,
    title = "Data Report",
    show_summary = TRUE,
    show_missing = TRUE
){
  
  bt_check_dataframe(data)
  
  report <- c()
  
  # Title
  report <- c(report, "")
  report <- c(report, strrep("=", 70))
  report <- c(report, title)
  report <- c(report, strrep("=", 70))
  report <- c(report, "")
  
  # Dataset Dimensions
  report <- c(report, "DATASET DIMENSIONS")
  report <- c(report, strrep("-", 70))
  report <- c(report, paste("Rows:           ", nrow(data)))
  report <- c(report, paste("Columns:        ", ncol(data)))
  report <- c(report, paste("Total Cells:    ", nrow(data) * ncol(data)))
  report <- c(report, "")
  
  # Column Information
  report <- c(report, "COLUMN INFORMATION")
  report <- c(report, strrep("-", 70))
  report <- c(report, "Column Name             | Type           | Missing | Unique")
  report <- c(report, strrep("-", 70))
  
  for(i in seq_along(data)){
    col_name <- names(data)[i]
    col_type <- class(data[[i]])[1]
    col_missing <- sum(is.na(data[[i]]))
    col_unique <- length(unique(data[[i]][!is.na(data[[i]])]))
    
    report <- c(report, sprintf(
      "%-23s | %-14s | %6d | %6d",
      substr(col_name, 1, 23),
      substr(col_type, 1, 14),
      col_missing,
      col_unique
    ))
  }
  report <- c(report, "")
  
  # Missing Data Summary
  if(show_missing){
    report <- c(report, "MISSING DATA ANALYSIS")
    report <- c(report, strrep("-", 70))
    total_missing <- sum(is.na(data))
    pct_missing <- round(total_missing / (nrow(data) * ncol(data)) * 100, 2)
    report <- c(report, paste("Total Missing Values:   ", total_missing))
    report <- c(report, paste("Percentage Missing:     ", pct_missing, "%"))
    report <- c(report, "")
  }
  
  # Summary Statistics
  if(show_summary){
    numeric_cols <- which(sapply(data, is.numeric))
    if(length(numeric_cols) > 0){
      report <- c(report, "NUMERIC COLUMNS SUMMARY")
      report <- c(report, strrep("-", 70))
      report <- c(report, "Column Name             | Mean      | Median    | Min       | Max")
      report <- c(report, strrep("-", 70))
      
      for(i in numeric_cols){
        col_name <- names(data)[i]
        col_mean <- mean(data[[i]], na.rm = TRUE)
        col_median <- stats::median(data[[i]], na.rm = TRUE)
        col_min <- min(data[[i]], na.rm = TRUE)
        col_max <- max(data[[i]], na.rm = TRUE)
        
        report <- c(report, sprintf(
          "%-23s | %9.2f | %9.2f | %9.2f | %9.2f",
          substr(col_name, 1, 23),
          col_mean,
          col_median,
          col_min,
          col_max
        ))
      }
      report <- c(report, "")
    }
  }
  
  # Footer
  report <- c(report, strrep("=", 70))
  report <- c(report, paste("Report generated:", format(Sys.time(), "%Y-%m-%d %H:%M:%S")))
  report <- c(report, "")
  
  class(report) <- c("BirdTraitsReport", "character")
  report
  
}

#------------------------------------------------------------------------------
# Export Report as Text
#------------------------------------------------------------------------------

#' Export Report as Text File
#'
#' Saves a report to a text file.
#'
#' @param report Report object (from bt_create_report).
#' @param filename File path to save report.
#'
#' @return Invisibly returns the filename.
#'
#' @examples
#' birds <- data.frame(
#'   species = c("A", "B", "C"),
#'   mass = c(20, 25, 30)
#' )
#'
#' report <- bt_create_report(birds)
#' # bt_export_report(report, "bird_report.txt")
#'
#' @export

bt_export_report <- function(
    report,
    filename
){
  
  if(!inherits(report, "character")){
    stop(
      "Report must be a character vector from bt_create_report().",
      call. = FALSE
    )
  }
  
  writeLines(report, con = filename)
  
  message(paste("Report saved to:", filename))
  invisible(filename)
  
}

#------------------------------------------------------------------------------
# Generate HTML Report
#------------------------------------------------------------------------------

#' Generate HTML Report
#'
#' Creates an interactive HTML report of the dataset.
#'
#' @param data Data frame.
#' @param title Report title.
#' @param filename Output HTML file name.
#'
#' @return Invisibly returns the filename.
#'
#' @examples
#' birds <- data.frame(
#'   species = c("A", "B", "C"),
#'   mass = c(20, 25, 30)
#' )
#'
#' # bt_report_html(birds, filename = "report.html")
#'
#' @export

bt_report_html <- function(
    data,
    title = "BirdTraitsR Data Report",
    filename = "report.html"
){
  
  bt_check_dataframe(data)
  
  # Create HTML content
  html <- c(
    "<!DOCTYPE html>",
    "<html>",
    "<head>",
    paste0("  <title>", title, "</title>"),
    "  <meta charset='UTF-8'>",
    "  <style>",
    "    body { font-family: Arial, sans-serif; margin: 20px; }",
    "    h1, h2 { color: #2C7FB8; }",
    "    table { border-collapse: collapse; margin: 20px 0; }",
    "    th, td { border: 1px solid #ddd; padding: 8px; text-align: left; }",
    "    th { background-color: #2C7FB8; color: white; }",
    "    tr:nth-child(even) { background-color: #f9f9f9; }",
    "  </style>",
    "</head>",
    "<body>"
  )
  
  # Title
  html <- c(html, paste0("  <h1>", title, "</h1>"))
  
  # Dataset info
  html <- c(
    html,
    "  <h2>Dataset Overview</h2>",
    "  <table>",
    "    <tr><th>Metric</th><th>Value</th></tr>",
    paste0("    <tr><td>Rows</td><td>", nrow(data), "</td></tr>"),
    paste0("    <tr><td>Columns</td><td>", ncol(data), "</td></tr>"),
    paste0("    <tr><td>Complete Cases</td><td>", nrow(stats::na.omit(data)), "</td></tr>"),
    "  </table>"
  )
  
  # Column info table
  html <- c(
    html,
    "  <h2>Column Information</h2>",
    "  <table>",
    "    <tr>",
    "      <th>Column Name</th>",
    "      <th>Data Type</th>",
    "      <th>Missing Values</th>",
    "      <th>Unique Values</th>",
    "    </tr>"
  )
  
  for(i in seq_along(data)){
    col_name <- names(data)[i]
    col_type <- class(data[[i]])[1]
    col_missing <- sum(is.na(data[[i]]))
    col_unique <- length(unique(data[[i]][!is.na(data[[i]])]))
    
    html <- c(
      html,
      paste0(
        "    <tr><td>", col_name, "</td><td>", col_type, "</td><td>",
        col_missing, "</td><td>", col_unique, "</td></tr>"
      )
    )
  }
  
  html <- c(
    html,
    "  </table>",
    "  <hr>",
    paste0("  <p><small>Report generated: ", format(Sys.time()), "</small></p>"),
    "</body>",
    "</html>"
  )
  
  writeLines(html, con = filename)
  message(paste("HTML report saved to:", filename))
  invisible(filename)
  
}

#------------------------------------------------------------------------------
# Generate PDF Report
#------------------------------------------------------------------------------

#' Generate PDF Report
#'
#' Creates a PDF report of the dataset. Requires grDevices and pdf support.
#'
#' @param data Data frame.
#' @param title Report title.
#' @param filename Output PDF file name.
#'
#' @return Invisibly returns the filename.
#'
#' @examples
#' birds <- data.frame(
#'   species = c("A", "B", "C"),
#'   mass = c(20, 25, 30)
#' )
#'
#' # bt_report_pdf(birds, filename = "report.pdf")
#'
#' @export

bt_report_pdf <- function(
    data,
    title = "BirdTraitsR Data Report",
    filename = "report.pdf"
){
  
  bt_check_dataframe(data)
  
  # Create PDF
  grDevices::pdf(filename, width = 8.5, height = 11)
  
  # Title page
  graphics::plot.new()
  graphics::text(0.5, 0.7, title, cex = 2, font = 2)
  graphics::text(0.5, 0.5, paste("Rows:", nrow(data)), cex = 1.2)
  graphics::text(0.5, 0.45, paste("Columns:", ncol(data)), cex = 1.2)
  graphics::text(0.5, 0.1, format(Sys.time()), cex = 1, col = "gray")
  
  # Summary statistics
  graphics::plot.new()
  graphics::text(0.5, 0.95, "Dataset Summary", cex = 1.5, font = 2)
  
  y_pos <- 0.85
  graphics::text(0.1, y_pos, paste("Total Rows:", nrow(data)), cex = 1, adj = 0)
  y_pos <- y_pos - 0.05
  graphics::text(0.1, y_pos, paste("Total Columns:", ncol(data)), cex = 1, adj = 0)
  y_pos <- y_pos - 0.05
  graphics::text(0.1, y_pos, paste("Complete Cases:", nrow(stats::na.omit(data))), cex = 1, adj = 0)
  y_pos <- y_pos - 0.05
  graphics::text(
    0.1, y_pos,
    paste("Missing Data:", round(sum(is.na(data)) / (nrow(data) * ncol(data)) * 100, 2), "%"),
    cex = 1, adj = 0
  )
  
  grDevices::dev.off()
  
  message(paste("PDF report saved to:", filename))
  invisible(filename)
  
}