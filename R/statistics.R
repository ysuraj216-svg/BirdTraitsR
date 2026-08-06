#===============================================================================
# BirdTraitsR
# Basic Statistical Functions
#
# Descriptive statistics for bird ecology datasets.
#===============================================================================

#------------------------------------------------------------------------------
# Species Count
#------------------------------------------------------------------------------

#' Count Species
#'
#' Counts unique species.
#'
#' @param data Data frame.
#' @param species_col Species column.
#'
#' @return Integer.
#'
#' @examples
#' birds <- data.frame(
#'   species = c("A","B","C","A")
#' )
#'
#' bt_count_species(birds)
#'
#' @export

bt_count_species <- function(
    data,
    species_col = "species"
){
  
  bt_check_dataframe(data)
  
  if(!species_col %in% names(data))
    stop("Species column not found.", call. = FALSE)
  
  length(unique(data[[species_col]]))
  
}


#------------------------------------------------------------------------------
# Family Count
#------------------------------------------------------------------------------

#' Count Families
#'
#' Counts unique families.
#'
#' @param data Data frame.
#' @param family_col Family column.
#'
#' @return Integer.
#'
#' @export

bt_count_families <- function(
    data,
    family_col = "family"
){
  
  bt_check_dataframe(data)
  
  if(!family_col %in% names(data))
    stop("Family column not found.", call. = FALSE)
  
  length(unique(data[[family_col]]))
  
}


#------------------------------------------------------------------------------
# Order Count
#------------------------------------------------------------------------------

#' Count Orders
#'
#' Counts unique orders.
#'
#' @param data Data frame.
#' @param order_col Order column.
#'
#' @return Integer.
#'
#' @export

bt_count_orders <- function(
    data,
    order_col="order"
){
  
  bt_check_dataframe(data)
  
  if(!order_col %in% names(data))
    stop("Order column not found.", call.=FALSE)
  
  length(unique(data[[order_col]]))
  
}


#------------------------------------------------------------------------------
# Number of Records
#------------------------------------------------------------------------------

#' Count Records
#'
#' Returns total observations.
#'
#' @param data Data frame.
#'
#' @return Integer.
#'
#' @export

bt_count_records <- function(data){
  
  bt_check_dataframe(data)
  
  nrow(data)
  
}


#------------------------------------------------------------------------------
# Number of Variables
#------------------------------------------------------------------------------

#' Count Variables
#'
#' Returns number of variables.
#'
#' @param data Data frame.
#'
#' @return Integer.
#'
#' @export

bt_count_variables <- function(data){
  
  bt_check_dataframe(data)
  
  ncol(data)
  
}


#------------------------------------------------------------------------------
# Missing Values
#------------------------------------------------------------------------------

#' Count Missing Values
#'
#' Counts NA values.
#'
#' @param data Data frame.
#'
#' @return Integer.
#'
#' @export

bt_missing_values <- function(data){
  
  bt_check_dataframe(data)
  
  sum(is.na(data))
  
}


#------------------------------------------------------------------------------
# Duplicate Rows
#------------------------------------------------------------------------------

#' Count Duplicate Rows
#'
#' Counts duplicated observations.
#'
#' @param data Data frame.
#'
#' @return Integer.
#'
#' @export

bt_duplicate_rows <- function(data){
  
  bt_check_dataframe(data)
  
  sum(duplicated(data))
  
}


#------------------------------------------------------------------------------
# Empty Strings
#------------------------------------------------------------------------------

#' Count Empty Strings
#'
#' Counts empty text values.
#'
#' @param data Data frame.
#'
#' @return Integer.
#'
#' @export

bt_empty_strings <- function(data){
  
  bt_check_dataframe(data)
  
  sum(data == "", na.rm = TRUE)
  
}


#------------------------------------------------------------------------------
# Dataset Dimensions
#------------------------------------------------------------------------------

#' Dataset Dimensions
#'
#' Returns rows and columns.
#'
#' @param data Data frame.
#'
#' @return Named vector.
#'
#' @export

bt_dimensions <- function(data){
  
  bt_check_dataframe(data)
  
  c(
    
    Rows = nrow(data),
    
    Columns = ncol(data)
    
  )
  
}


#------------------------------------------------------------------------------
# Complete Dataset Summary
#------------------------------------------------------------------------------

#' Dataset Statistics
#'
#' Returns complete descriptive statistics.
#'
#' @param data Data frame.
#'
#' @return BirdTraitsStatistics object.
#'
#' @examples
#' birds <- data.frame(
#'   species = c("A","B","C","A"),
#'   family = c("F1","F1","F2","F1"),
#'   order = c("O1","O1","O2","O1")
#' )
#'
#' bt_statistics(birds)
#'
#' @export

bt_statistics <- function(data){
  
  bt_check_dataframe(data)
  
  result <- list(
    
    records = bt_count_records(data),
    
    variables = bt_count_variables(data),
    
    species = if("species" %in% names(data))
      bt_count_species(data)
    else
      NA,
    
    families = if("family" %in% names(data))
      bt_count_families(data)
    else
      NA,
    
    orders = if("order" %in% names(data))
      bt_count_orders(data)
    else
      NA,
    
    missing = bt_missing_values(data),
    
    duplicates = bt_duplicate_rows(data),
    
    empty_strings = bt_empty_strings(data)
    
  )
  
  class(result) <- "BirdTraitsStatistics"
  
  result
  
}


#------------------------------------------------------------------------------
# Print Method
#------------------------------------------------------------------------------

#' @export

print.BirdTraitsStatistics <- function(x,...){
  
  cat("\n")
  
  cat("=====================================\n")
  cat(" BirdTraitsR Dataset Statistics\n")
  cat("=====================================\n\n")
  
  cat("Records          :",x$records,"\n")
  cat("Variables        :",x$variables,"\n")
  cat("Species          :",x$species,"\n")
  cat("Families         :",x$families,"\n")
  cat("Orders           :",x$orders,"\n")
  cat("Missing Values   :",x$missing,"\n")
  cat("Duplicate Rows   :",x$duplicates,"\n")
  cat("Empty Strings    :",x$empty_strings,"\n")
  
  invisible(x)
  
}
#===============================================================================
# BirdTraitsR
# Statistics Functions (Part 2A)
#
# Frequency and Percentage Tables
#===============================================================================

#------------------------------------------------------------------------------
# Frequency Table
#------------------------------------------------------------------------------

#' Frequency Table
#'
#' Returns a frequency table for any categorical variable.
#'
#' @param data A data frame.
#' @param column Column name.
#'
#' @return Data frame.
#'
#' @examples
#' birds <- data.frame(
#'   diet = c(
#'     "Insectivore",
#'     "Omnivore",
#'     "Insectivore",
#'     "Carnivore"
#'   ),
#'   stringsAsFactors = FALSE
#' )
#'
#' bt_frequency_table(birds, "diet")
#'
#' @export

bt_frequency_table <- function(data, column){
  
  bt_check_dataframe(data)
  
  if(!column %in% names(data)){
    stop(
      paste0(
        "Column '",
        column,
        "' not found."
      ),
      call. = FALSE
    )
  }
  
  counts <- table(
    data[[column]],
    useNA = "ifany"
  )
  
  output <- data.frame(
    
    Category = names(counts),
    
    Count = as.integer(counts),
    
    stringsAsFactors = FALSE
    
  )
  
  output[
    order(
      output$Count,
      decreasing = TRUE
    ),
  ]
  
}

#------------------------------------------------------------------------------
# Percentage Table
#------------------------------------------------------------------------------

#' Percentage Table
#'
#' Returns percentages for a categorical variable.
#'
#' @param data A data frame.
#' @param column Column name.
#'
#' @return Data frame.
#'
#' @examples
#' birds <- data.frame(
#'   diet = c(
#'     "Insectivore",
#'     "Omnivore",
#'     "Insectivore",
#'     "Carnivore"
#'   ),
#'   stringsAsFactors = FALSE
#' )
#'
#' bt_percentage_table(birds, "diet")
#'
#' @export

bt_percentage_table <- function(data, column){
  
  bt_check_dataframe(data)
  
  if(!column %in% names(data)){
    stop(
      paste0(
        "Column '",
        column,
        "' not found."
      ),
      call. = FALSE
    )
  }
  
  counts <- table(
    data[[column]],
    useNA = "ifany"
  )
  
  percentages <-
    round(
      100 * counts / sum(counts),
      2
    )
  
  output <- data.frame(
    
    Category = names(percentages),
    
    Percentage = as.numeric(percentages),
    
    stringsAsFactors = FALSE
    
  )
  
  output[
    order(
      output$Percentage,
      decreasing = TRUE
    ),
  ]
  
}
#===============================================================================
# BirdTraitsR
# Statistics Functions (Part 2B)
#
# Crosstabs and Contingency Tables
#===============================================================================

#------------------------------------------------------------------------------
# Crosstab
#------------------------------------------------------------------------------

#' Crosstab
#'
#' Creates a contingency table between two categorical variables.
#'
#' @param data A data frame.
#' @param row Variable for rows.
#' @param column Variable for columns.
#'
#' @return Matrix.
#'
#' @examples
#' birds <- data.frame(
#'   diet = c(
#'     "Insectivore",
#'     "Omnivore",
#'     "Carnivore",
#'     "Insectivore"
#'   ),
#'   status = c(
#'     "LC",
#'     "LC",
#'     "VU",
#'     "NT"
#'   ),
#'   stringsAsFactors = FALSE
#' )
#'
#' bt_crosstab(
#'   birds,
#'   "diet",
#'   "status"
#' )
#'
#' @export

bt_crosstab <- function(data, row, column){
  
  bt_check_dataframe(data)
  
  if(!row %in% names(data)){
    stop(
      paste0(
        "Column '",
        row,
        "' not found."
      ),
      call. = FALSE
    )
  }
  
  if(!column %in% names(data)){
    stop(
      paste0(
        "Column '",
        column,
        "' not found."
      ),
      call. = FALSE
    )
  }
  
  table(
    data[[row]],
    data[[column]],
    useNA = "ifany"
  )
  
}

#------------------------------------------------------------------------------
# Proportion Table
#------------------------------------------------------------------------------

#' Proportion Table
#'
#' Creates a proportional contingency table.
#'
#' @param data A data frame.
#' @param row Row variable.
#' @param column Column variable.
#' @param margin Margin for proportions.
#'
#' @return Matrix.
#'
#' @examples
#' birds <- data.frame(
#'   diet = c(
#'     "Insectivore",
#'     "Omnivore",
#'     "Carnivore",
#'     "Insectivore"
#'   ),
#'   status = c(
#'     "LC",
#'     "LC",
#'     "VU",
#'     "NT"
#'   ),
#'   stringsAsFactors = FALSE
#' )
#'
#' bt_prop_table(
#'   birds,
#'   "diet",
#'   "status"
#' )
#'
#' @export

bt_prop_table <- function(
    data,
    row,
    column,
    margin = 1
){
  
  prop.table(
    
    bt_crosstab(
      data,
      row,
      column
    ),
    
    margin = margin
    
  )
  
}

#------------------------------------------------------------------------------
# Contingency Table
#------------------------------------------------------------------------------

#' Contingency Table
#'
#' Returns counts and percentages together.
#'
#' @param data A data frame.
#' @param row Row variable.
#' @param column Column variable.
#'
#' @return BirdTraitsContingency object.
#'
#' @examples
#' birds <- data.frame(
#'   diet = c(
#'     "Insectivore",
#'     "Omnivore",
#'     "Carnivore",
#'     "Insectivore"
#'   ),
#'   status = c(
#'     "LC",
#'     "LC",
#'     "VU",
#'     "NT"
#'   )
#' )
#'
#' bt_contingency_table(
#'   birds,
#'   "diet",
#'   "status"
#' )
#'
#' @export

bt_contingency_table <- function(
    data,
    row,
    column
){
  
  bt_check_dataframe(data)
  
  result <- list(
    
    counts = bt_crosstab(
      data,
      row,
      column
    ),
    
    proportions = round(
      
      bt_prop_table(
        data,
        row,
        column
      ),
      
      3
      
    )
    
  )
  
  class(result) <- "BirdTraitsContingency"
  
  result
  
}

#------------------------------------------------------------------------------
# Print Method
#------------------------------------------------------------------------------

#' @export

print.BirdTraitsContingency <- function(x, ...){
  
  cat("\n")
  
  cat("=====================================\n")
  cat(" BirdTraitsR Contingency Table\n")
  cat("=====================================\n\n")
  
  cat("Counts\n\n")
  
  print(x$counts)
  
  cat("\n")
  
  cat("Row Proportions\n\n")
  
  print(x$proportions)
  
  invisible(x)
  
}
#------------------------------------------------------------------------------
# Mean
#------------------------------------------------------------------------------

#' Mean
#'
#' Calculates the mean of a numeric variable.
#'
#' @param data A data frame.
#' @param column Numeric column.
#'
#' @return Numeric.
#'
#' @examples
#' birds <- data.frame(
#'   mass = c(20,25,18,30)
#' )
#'
#' bt_mean(birds, "mass")
#'
#' @export

bt_mean <- function(data, column){
  
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
  
  mean(x, na.rm = TRUE)
  
}


#------------------------------------------------------------------------------
# Median
#------------------------------------------------------------------------------

#' Median
#'
#' Calculates the median of a numeric variable.
#'
#' @param data A data frame.
#' @param column Numeric column.
#'
#' @return Numeric.
#'
#' @examples
#' birds <- data.frame(
#'   mass = c(20,25,18,30)
#' )
#'
#' bt_median(birds, "mass")
#'
#' @export

bt_median <- function(data, column){
  
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
  
  stats::median(x, na.rm = TRUE)
  
}


#------------------------------------------------------------------------------
# Variance
#------------------------------------------------------------------------------

#' Variance
#'
#' Calculates variance.
#'
#' @param data A data frame.
#' @param column Numeric column.
#'
#' @return Numeric.
#'
#' @examples
#' birds <- data.frame(
#'   mass = c(20,25,18,30)
#' )
#'
#' bt_variance(birds, "mass")
#'
#' @export

bt_variance <- function(data, column){
  
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
  
  stats::var(x, na.rm = TRUE)
  
}


#------------------------------------------------------------------------------
# Standard Deviation
#------------------------------------------------------------------------------

#' Standard Deviation
#'
#' Calculates standard deviation.
#'
#' @param data A data frame.
#' @param column Numeric column.
#'
#' @return Numeric.
#'
#' @examples
#' birds <- data.frame(
#'   mass = c(20,25,18,30)
#' )
#'
#' bt_standard_deviation(birds, "mass")
#'
#' @export

bt_standard_deviation <- function(data, column){
  
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
  
  stats::sd(x, na.rm = TRUE)
  
}


#------------------------------------------------------------------------------
# Range
#------------------------------------------------------------------------------

#' Range
#'
#' Returns minimum and maximum values.
#'
#' @param data A data frame.
#' @param column Numeric column.
#'
#' @return Numeric vector.
#'
#' @examples
#' birds <- data.frame(
#'   mass = c(20,25,18,30)
#' )
#'
#' bt_range(birds, "mass")
#'
#' @export

bt_range <- function(data, column){
  
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
  
  range(x, na.rm = TRUE)
  
}


#------------------------------------------------------------------------------
# Quantiles
#------------------------------------------------------------------------------

#' Quantiles
#'
#' Returns quartiles for a numeric variable.
#'
#' @param data A data frame.
#' @param column Numeric column.
#'
#' @return Numeric vector.
#'
#' @examples
#' birds <- data.frame(
#'   mass = c(20,25,18,30,27)
#' )
#'
#' bt_quantiles(birds, "mass")
#'
#' @export

bt_quantiles <- function(data, column){
  
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
  
  stats::quantile(x, na.rm = TRUE)
  
}
#===============================================================================
# BirdTraitsR
# Statistics Functions (Part 4A)
#
# Correlation Analysis
#===============================================================================

#------------------------------------------------------------------------------
# Correlation
#------------------------------------------------------------------------------

#' Correlation
#'
#' Calculates the correlation between two numeric variables.
#'
#' @param data A data frame.
#' @param x First numeric column.
#' @param y Second numeric column.
#' @param method Correlation method. One of
#' "pearson", "spearman", or "kendall".
#'
#' @return Numeric.
#'
#' @examples
#' birds <- data.frame(
#'   mass = c(20,25,18,30,27),
#'   wing = c(12,14,11,16,15)
#' )
#'
#' bt_correlation(
#'   birds,
#'   "mass",
#'   "wing"
#' )
#'
#' @export

bt_correlation <- function(
    data,
    x,
    y,
    method = "pearson"
){
  
  bt_check_dataframe(data)
  
  if(!x %in% names(data)){
    stop(
      paste0("Column '", x, "' not found."),
      call. = FALSE
    )
  }
  
  if(!y %in% names(data)){
    stop(
      paste0("Column '", y, "' not found."),
      call. = FALSE
    )
  }
  
  if(!is.numeric(data[[x]])){
    stop(
      paste0("Column '", x, "' is not numeric."),
      call. = FALSE
    )
  }
  
  if(!is.numeric(data[[y]])){
    stop(
      paste0("Column '", y, "' is not numeric."),
      call. = FALSE
    )
  }
  
  stats::cor(
    data[[x]],
    data[[y]],
    method = method,
    use = "complete.obs"
  )
  
}


#------------------------------------------------------------------------------
# Correlation Matrix
#------------------------------------------------------------------------------

#' Correlation Matrix
#'
#' Calculates a correlation matrix for all numeric variables.
#'
#' @param data A data frame.
#' @param method Correlation method.
#'
#' @return Matrix.
#'
#' @examples
#' birds <- data.frame(
#'   mass = c(20,25,18,30),
#'   wing = c(11,13,10,15),
#'   tail = c(7,8,6,9)
#' )
#'
#' bt_correlation_matrix(birds)
#'
#' @export

bt_correlation_matrix <- function(
    data,
    method = "pearson"
){
  
  bt_check_dataframe(data)
  
  numeric_data <-
    data[
      ,
      vapply(
        data,
        is.numeric,
        logical(1)
      ),
      drop = FALSE
    ]
  
  if(ncol(numeric_data) < 2){
    stop(
      "At least two numeric columns are required.",
      call. = FALSE
    )
  }
  
  stats::cor(
    numeric_data,
    method = method,
    use = "complete.obs"
  )
  
}
#------------------------------------------------------------------------------
# Chi-square Test
#------------------------------------------------------------------------------

#' Chi-square Test
#'
#' Performs a Chi-square test between two categorical variables.
#'
#' @param data A data frame.
#' @param row Row variable.
#' @param column Column variable.
#'
#' @return htest object.
#'
#' @examples
#' birds <- data.frame(
#'   diet=c("A","A","B","B"),
#'   status=c("LC","VU","LC","LC")
#' )
#'
#' bt_chisq_test(
#'   birds,
#'   "diet",
#'   "status"
#' )
#'
#' @export

bt_chisq_test <- function(data,row,column){
  
  bt_check_dataframe(data)
  
  stats::chisq.test(
    bt_crosstab(data,row,column)
  )
  
}
#------------------------------------------------------------------------------
# Fisher Exact Test
#------------------------------------------------------------------------------

#' Fisher Exact Test
#'
#' Performs Fisher's Exact Test.
#'
#' @param data A data frame.
#' @param row Row variable.
#' @param column Column variable.
#'
#' @return htest object.
#'
#' @export

bt_fisher_test <- function(data,row,column){
  
  bt_check_dataframe(data)
  
  stats::fisher.test(
    bt_crosstab(data,row,column)
  )
  
}
#------------------------------------------------------------------------------
# t-test
#------------------------------------------------------------------------------

#' Two Sample t-test
#'
#' Performs an independent t-test.
#'
#' @param data A data frame.
#' @param response Numeric variable.
#' @param group Grouping variable.
#'
#' @return htest object.
#'
#' @examples
#' birds <- data.frame(
#'   mass=c(20,25,18,27),
#'   sex=c("M","M","F","F")
#' )
#'
#' bt_t_test(
#'   birds,
#'   "mass",
#'   "sex"
#' )
#'
#' @export

bt_t_test <- function(data,response,group){
  
  bt_check_dataframe(data)
  
  stats::t.test(
    
    data[[response]] ~
      
      data[[group]]
    
  )
  
}
#------------------------------------------------------------------------------
# Wilcoxon Test
#------------------------------------------------------------------------------

#' Wilcoxon Rank Sum Test
#'
#' Performs a Wilcoxon test.
#'
#' @param data A data frame.
#' @param response Numeric variable.
#' @param group Grouping variable.
#'
#' @return htest object.
#'
#' @export

bt_wilcox_test <- function(data,response,group){
  
  bt_check_dataframe(data)
  
  stats::wilcox.test(
    
    data[[response]] ~
      
      data[[group]]
    
  )
  
}
#------------------------------------------------------------------------------
# One-way ANOVA
#------------------------------------------------------------------------------

#' One-way ANOVA
#'
#' Performs one-way ANOVA.
#'
#' @param data A data frame.
#' @param response Numeric variable.
#' @param group Grouping variable.
#'
#' @return anova object.
#'
#' @examples
#' birds <- data.frame(
#'   mass=c(20,22,25,28,30,35),
#'   diet=c("A","A","B","B","C","C")
#' )
#'
#' bt_anova(
#'   birds,
#'   "mass",
#'   "diet"
#' )
#'
#' @export

bt_anova <- function(data,response,group){
  
  bt_check_dataframe(data)
  
  fit <- stats::aov(
    
    data[[response]] ~
      
      data[[group]]
    
  )
  
  summary(fit)
  
}
#------------------------------------------------------------------------------
# Kruskal-Wallis Test
#------------------------------------------------------------------------------

#' Kruskal-Wallis Test
#'
#' Performs a Kruskal-Wallis test.
#'
#' @param data A data frame.
#' @param response Numeric variable.
#' @param group Grouping variable.
#'
#' @return htest object.
#'
#' @export

bt_kruskal_test <- function(data,response,group){
  
  bt_check_dataframe(data)
  
  stats::kruskal.test(
    
    data[[response]] ~
      
      data[[group]]
    
  )
  
}
#------------------------------------------------------------------------------
# Linear Regression
#------------------------------------------------------------------------------

#' Linear Regression
#'
#' Fits a linear regression model.
#'
#' @param data A data frame.
#' @param response Response variable.
#' @param predictors Predictor variable(s).
#'
#' @return lm object.
#'
#' @examples
#' birds <- data.frame(
#'   mass = c(20,25,18,30,27),
#'   wing = c(12,14,11,16,15)
#' )
#'
#' bt_linear_regression(
#'   birds,
#'   "mass",
#'   "wing"
#' )
#'
#' @export

bt_linear_regression <- function(
    data,
    response,
    predictors
){
  
  bt_check_dataframe(data)
  
  vars <- c(response, predictors)
  
  missing <- vars[!vars %in% names(data)]
  
  if(length(missing) > 0){
    stop(
      paste0(
        "Column(s) not found: ",
        paste(missing, collapse=", ")
      ),
      call. = FALSE
    )
  }
  
  formula <- stats::as.formula(
    
    paste(
      
      response,
      
      "~",
      
      paste(predictors, collapse = " + ")
      
    )
    
  )
  
  stats::lm(
    formula,
    data = data
  )
  
}
#------------------------------------------------------------------------------
# Logistic Regression
#------------------------------------------------------------------------------

#' Logistic Regression
#'
#' Fits a logistic regression model.
#'
#' @param data A data frame.
#' @param response Binary response variable.
#' @param predictors Predictor variable(s).
#'
#' @return glm object.
#'
#' @export

bt_logistic_regression <- function(
    data,
    response,
    predictors
){
  
  bt_check_dataframe(data)
  
  formula <- stats::as.formula(
    
    paste(
      
      response,
      
      "~",
      
      paste(predictors, collapse=" + ")
      
    )
    
  )
  
  stats::glm(
    
    formula,
    
    data = data,
    
    family = stats::binomial()
    
  )
  
}
#------------------------------------------------------------------------------
# Poisson Regression
#------------------------------------------------------------------------------

#' Poisson Regression
#'
#' Fits a Poisson regression model.
#'
#' @param data A data frame.
#' @param response Count response.
#' @param predictors Predictor variable(s).
#'
#' @return glm object.
#'
#' @export

bt_poisson_regression <- function(
    data,
    response,
    predictors
){
  
  bt_check_dataframe(data)
  
  formula <- stats::as.formula(
    
    paste(
      
      response,
      
      "~",
      
      paste(predictors, collapse=" + ")
      
    )
    
  )
  
  stats::glm(
    
    formula,
    
    data = data,
    
    family = stats::poisson()
    
  )
  
}
#------------------------------------------------------------------------------

#------------------------------------------------------------------------------
# Akaike Information Criterion
#------------------------------------------------------------------------------

#' Model AIC
#'
#' Returns AIC value.
#'
#' @param model A fitted model.
#'
#' @return Numeric.
#'
#' @export

bt_aic <- function(model){
  
  stats::AIC(model)
  
}
#------------------------------------------------------------------------------
# Model Predictions
#------------------------------------------------------------------------------

#' Predict Values
#'
#' Generates predictions from a fitted model.
#'
#' @param model A fitted model.
#' @param newdata Optional new data.
#'
#' @return Numeric vector.
#'
#' @export

bt_predict <- function(
    model,
    newdata = NULL
){
  
  stats::predict(
    
    model,
    
    newdata = newdata,
    
    type = "response"
    
  )
  
}
#===============================================================================
# BirdTraitsR
# Statistics Functions (Part 5H)
#
# Model Diagnostics
#===============================================================================

#------------------------------------------------------------------------------
# Model Residuals
#------------------------------------------------------------------------------

#' Model Residuals
#'
#' Returns residuals from a fitted model.
#'
#' @param model A fitted model object.
#'
#' @return Numeric vector.
#'
#' @examples
#' birds <- data.frame(
#'   y = c(20,25,18,30,27),
#'   x = c(12,14,11,16,15)
#' )
#'
#' model <- lm(y ~ x, data = birds)
#'
#' bt_residuals(model)
#'
#' @export

bt_residuals <- function(model){
  
  stats::residuals(model)
  
}


#------------------------------------------------------------------------------
# Fitted Values
#------------------------------------------------------------------------------

#' Fitted Values
#'
#' Returns fitted values from a model.
#'
#' @param model A fitted model object.
#'
#' @return Numeric vector.
#'
#' @examples
#' birds <- data.frame(
#'   y = c(20,25,18,30,27),
#'   x = c(12,14,11,16,15)
#' )
#'
#' model <- lm(y ~ x, data = birds)
#'
#' bt_fitted(model)
#'
#' @export

bt_fitted <- function(model){
  
  stats::fitted(model)
  
}


#------------------------------------------------------------------------------
# Residual Standard Error
#------------------------------------------------------------------------------

#' Residual Standard Error
#'
#' Returns the residual standard error of a linear model.
#'
#' @param model A linear model.
#'
#' @return Numeric.
#'
#' @examples
#' birds <- data.frame(
#'   y = c(20,25,18,30,27),
#'   x = c(12,14,11,16,15)
#' )
#'
#' model <- lm(y ~ x, data = birds)
#'
#' bt_rse(model)
#'
#' @export

bt_rse <- function(model){
  
  if(!inherits(model, "lm")){
    stop(
      "Model must be a linear model.",
      call. = FALSE
    )
  }
  
  summary(model)$sigma
  
}


#------------------------------------------------------------------------------
# R-squared
#------------------------------------------------------------------------------

#' R-squared
#'
#' Returns the coefficient of determination.
#'
#' @param model A linear model.
#'
#' @return Numeric.
#'
#' @examples
#' birds <- data.frame(
#'   y = c(20,25,18,30,27),
#'   x = c(12,14,11,16,15)
#' )
#'
#' model <- lm(y ~ x, data = birds)
#'
#' bt_r_squared(model)
#'
#' @export

bt_r_squared <- function(model){
  
  if(!inherits(model, "lm")){
    stop(
      "Model must be a linear model.",
      call. = FALSE
    )
  }
  
  summary(model)$r.squared
  
}


#------------------------------------------------------------------------------
# Adjusted R-squared
#------------------------------------------------------------------------------

#' Adjusted R-squared
#'
#' Returns adjusted R-squared.
#'
#' @param model A linear model.
#'
#' @return Numeric.
#'
#' @examples
#' birds <- data.frame(
#'   y = c(20,25,18,30,27),
#'   x = c(12,14,11,16,15)
#' )
#'
#' model <- lm(y ~ x, data = birds)
#'
#' bt_adjusted_r_squared(model)
#'
#' @export

bt_adjusted_r_squared <- function(model){
  
  if(!inherits(model, "lm")){
    stop(
      "Model must be a linear model.",
      call. = FALSE
    )
  }
  
  summary(model)$adj.r.squared
  
}


#------------------------------------------------------------------------------
# Bayesian Information Criterion
#------------------------------------------------------------------------------

#' Bayesian Information Criterion
#'
#' Returns the Bayesian Information Criterion (BIC).
#'
#' @param model A fitted model.
#'
#' @return Numeric.
#'
#' @examples
#' birds <- data.frame(
#'   y = c(20,25,18,30,27),
#'   x = c(12,14,11,16,15)
#' )
#'
#' model <- lm(y ~ x, data = birds)
#'
#' bt_bic(model)
#'
#' @export

bt_bic <- function(model){
  
  stats::BIC(model)
  
}


#------------------------------------------------------------------------------
# Log-Likelihood
#------------------------------------------------------------------------------

#' Log-Likelihood
#'
#' Returns model log-likelihood.
#'
#' @param model A fitted model.
#'
#' @return Numeric.
#'
#' @examples
#' birds <- data.frame(
#'   y = c(20,25,18,30,27),
#'   x = c(12,14,11,16,15)
#' )
#'
#' model <- lm(y ~ x, data = birds)
#'
#' bt_logLik(model)
#'
#' @export

bt_logLik <- function(model){
  
  as.numeric(stats::logLik(model))
  
}


#------------------------------------------------------------------------------
# Deviance
#------------------------------------------------------------------------------

#' Model Deviance
#'
#' Returns model deviance.
#'
#' @param model A fitted model.
#'
#' @return Numeric.
#'
#' @examples
#' birds <- data.frame(
#'   y = c(20,25,18,30,27),
#'   x = c(12,14,11,16,15)
#' )
#'
#' model <- lm(y ~ x, data = birds)
#'
#' bt_deviance(model)
#'
#' @export

bt_deviance <- function(model){
  
  stats::deviance(model)
  
}


#------------------------------------------------------------------------------
# Degrees of Freedom
#------------------------------------------------------------------------------

#' Degrees of Freedom
#'
#' Returns model degrees of freedom.
#'
#' @param model A fitted model.
#'
#' @return Integer vector.
#'
#' @examples
#' birds <- data.frame(
#'   y = c(20,25,18,30,27),
#'   x = c(12,14,11,16,15)
#' )
#'
#' model <- lm(y ~ x, data = birds)
#'
#' bt_df(model)
#'
#' @export

bt_df <- function(model){
  
  stats::df.residual(model)
  
}
#===============================================================================
# BirdTraitsR
# Statistics Functions (Part 5I)
#
# Model Comparison
#===============================================================================

#------------------------------------------------------------------------------
# Best Model (Lowest AIC)
#------------------------------------------------------------------------------

#' Best Model
#'
#' Returns the model with the lowest AIC.
#'
#' @param ... Two or more fitted model objects.
#'
#' @return A fitted model object.
#'
#' @examples
#' birds <- data.frame(
#'   y = c(10,12,13,16,18),
#'   x1 = c(2,3,4,5,6),
#'   x2 = c(5,4,6,7,8)
#' )
#'
#' m1 <- lm(y ~ x1, data = birds)
#' m2 <- lm(y ~ x2, data = birds)
#'
#' best <- bt_best_model(m1, m2)
#'
#' @export

bt_best_model <- function(...) {
  
  models <- list(...)
  
  if(length(models) < 2){
    stop(
      "Provide at least two fitted models.",
      call. = FALSE
    )
  }
  
  aics <- sapply(models, stats::AIC)
  
  models[[which.min(aics)]]
  
}


#------------------------------------------------------------------------------
# Delta AIC
#------------------------------------------------------------------------------

#' Delta AIC
#'
#' Computes Delta AIC for multiple models.
#'
#' @param ... Two or more fitted model objects.
#'
#' @return A data frame.
#'
#' @examples
#' birds <- data.frame(
#'   y = c(10,12,13,16,18),
#'   x1 = c(2,3,4,5,6),
#'   x2 = c(5,4,6,7,8)
#' )
#'
#' m1 <- lm(y ~ x1, data = birds)
#' m2 <- lm(y ~ x2, data = birds)
#'
#' bt_delta_aic(m1,m2)
#'
#' @export

bt_delta_aic <- function(...) {
  
  models <- list(...)
  
  if(length(models) < 2){
    stop(
      "Provide at least two fitted models.",
      call. = FALSE
    )
  }
  
  aics <- sapply(models, stats::AIC)
  
  delta <- aics - min(aics)
  
  data.frame(
    
    Model = paste0("Model_", seq_along(models)),
    
    AIC = round(aics,3),
    
    Delta_AIC = round(delta,3),
    
    row.names = NULL
    
  )
  
}


#------------------------------------------------------------------------------
# Akaike Weights
#------------------------------------------------------------------------------

#' Akaike Weights
#'
#' Calculates Akaike weights from multiple fitted models.
#'
#' @param ... Two or more fitted model objects.
#'
#' @return A data frame.
#'
#' @examples
#' birds <- data.frame(
#'   y = c(10,12,13,16,18),
#'   x1 = c(2,3,4,5,6),
#'   x2 = c(5,4,6,7,8)
#' )
#'
#' m1 <- lm(y ~ x1, data = birds)
#' m2 <- lm(y ~ x2, data = birds)
#'
#' bt_aic_weights(m1,m2)
#'
#' @export

bt_aic_weights <- function(...) {
  
  models <- list(...)
  
  if(length(models) < 2){
    stop(
      "Provide at least two fitted models.",
      call. = FALSE
    )
  }
  
  aics <- sapply(models, stats::AIC)
  
  delta <- aics - min(aics)
  
  weights <- exp(-0.5 * delta)
  
  weights <- weights / sum(weights)
  
  data.frame(
    
    Model = paste0("Model_", seq_along(models)),
    
    AIC = round(aics,3),
    
    Delta_AIC = round(delta,3),
    
    Weight = round(weights,4),
    
    row.names = NULL
    
  )
  
}
#===============================================================================
# BirdTraitsR
# Statistics Functions (Part 5J)
#
# Model Comparison
#===============================================================================

#------------------------------------------------------------------------------
# Compare Models
#------------------------------------------------------------------------------

#' Compare Multiple Models
#'
#' Compares fitted models using common model selection statistics.
#'
#' Returns a summary table containing:
#'
#' * AIC
#' * BIC
#' * Log-Likelihood
#' * Deviance
#' * Residual Degrees of Freedom
#'
#' Models are automatically ranked by AIC.
#'
#' @param ... Two or more fitted model objects.
#'
#' @return A data frame.
#'
#' @examples
#' birds <- data.frame(
#'   y=c(10,12,13,16,18),
#'   x1=c(2,3,4,5,6),
#'   x2=c(5,4,6,7,8)
#' )
#'
#' m1 <- lm(y~x1,birds)
#' m2 <- lm(y~x2,birds)
#'
#' bt_compare_models(m1,m2)
#'
#' @export

bt_compare_models <- function(...){
  
  models <- list(...)
  
  if(length(models) < 2){
    
    stop(
      "Provide at least two fitted models.",
      call. = FALSE
    )
    
  }
  
  results <- data.frame(
    
    Model = paste0("Model_", seq_along(models)),
    
    AIC = sapply(models, stats::AIC),
    
    BIC = sapply(models, stats::BIC),
    
    LogLik = sapply(
      models,
      function(x) as.numeric(stats::logLik(x))
    ),
    
    Deviance = sapply(models, stats::deviance),
    
    DF = sapply(models, stats::df.residual)
    
  )
  
  results <- results[order(results$AIC), ]
  
  rownames(results) <- NULL
  
  results
  
}
#------------------------------------------------------------------------------
# Likelihood Ratio Test
#------------------------------------------------------------------------------

#' Likelihood Ratio Test
#'
#' Performs a likelihood ratio test for two or more nested models.
#'
#' This is a wrapper around
#' \code{stats::anova(..., test = "Chisq")}.
#'
#' Models should be nested for valid inference.
#'
#' @param ... Two or more fitted model objects.
#'
#' @return An ANOVA table.
#'
#' @examples
#' birds <- data.frame(
#'   y = c(10,12,13,16,18),
#'   x1 = c(2,3,4,5,6),
#'   x2 = c(5,4,6,7,8)
#' )
#'
#' m1 <- glm(
#'   y ~ x1,
#'   data = birds,
#'   family = poisson()
#' )
#'
#' m2 <- glm(
#'   y ~ x1 + x2,
#'   data = birds,
#'   family = poisson()
#' )
#'
#' bt_likelihood_ratio_test(m1, m2)
#'
#' @export

bt_likelihood_ratio_test <- function(...){
  
  models <- list(...)
  
  if(length(models) < 2){
    
    stop(
      "Provide at least two fitted models.",
      call. = FALSE
    )
    
  }
  
  stats::anova(
    
    ...,
    
    test = "Chisq"
    
  )
  
}
#------------------------------------------------------------------------------
# ANOVA Model Comparison
#------------------------------------------------------------------------------

#' Compare Models Using ANOVA
#'
#' Compares two or more fitted models using analysis of variance.
#'
#' This function is a wrapper around \code{stats::anova()} and is
#' primarily intended for comparing nested linear models.
#'
#' @param ... Two or more fitted model objects.
#'
#' @return An ANOVA table.
#'
#' @examples
#' birds <- data.frame(
#'   y = c(10,12,13,16,18),
#'   x1 = c(2,3,4,5,6),
#'   x2 = c(5,4,6,7,8)
#' )
#'
#' m1 <- lm(y ~ x1, data = birds)
#' m2 <- lm(y ~ x1 + x2, data = birds)
#'
#' bt_anova_models(m1, m2)
#'
#' @export

bt_anova_models <- function(...){
  
  models <- list(...)
  
  if(length(models) < 2){
    
    stop(
      "Provide at least two fitted models.",
      call. = FALSE
    )
    
  }
  
  stats::anova(...)
  
}
#------------------------------------------------------------------------------
# Model Summary Table
#------------------------------------------------------------------------------

#' Model Summary Table
#'
#' Returns a summary table for one or more fitted models.
#'
#' The summary includes:
#'
#' * AIC
#' * BIC
#' * Log-Likelihood
#' * Deviance
#' * Residual Degrees of Freedom
#' * Number of Parameters
#'
#' @param ... Two or more fitted model objects.
#'
#' @return A data frame.
#'
#' @examples
#' birds <- data.frame(
#'   y = c(10,12,13,16,18),
#'   x1 = c(2,3,4,5,6),
#'   x2 = c(5,4,6,7,8)
#' )
#'
#' m1 <- lm(y ~ x1, data = birds)
#' m2 <- lm(y ~ x1 + x2, data = birds)
#'
#' bt_model_summary(m1, m2)
#'
#' @export

bt_model_summary <- function(...){
  
  models <- list(...)
  
  if(length(models) < 1){
    
    stop(
      "Provide at least one fitted model.",
      call. = FALSE
    )
    
  }
  
  data.frame(
    
    Model = paste0("Model_", seq_along(models)),
    
    Parameters = sapply(
      models,
      function(x) length(stats::coef(x))
    ),
    
    AIC = sapply(models, stats::AIC),
    
    BIC = sapply(models, stats::BIC),
    
    LogLik = sapply(
      models,
      function(x) as.numeric(stats::logLik(x))
    ),
    
    Deviance = sapply(models, stats::deviance),
    
    Residual_DF = sapply(
      models,
      stats::df.residual
    ),
    
    row.names = NULL
    
  )
  
}
#------------------------------------------------------------------------------
# Rank Models
#------------------------------------------------------------------------------

#' Rank Models
#'
#' Ranks multiple fitted models using Akaike Information Criterion (AIC).
#'
#' The returned table contains:
#'
#' * AIC
#' * Delta AIC
#' * Akaike Weight
#' * Rank
#'
#' Models are ordered from best (lowest AIC) to worst.
#'
#' @param ... Two or more fitted model objects.
#'
#' @return A data frame.
#'
#' @examples
#' birds <- data.frame(
#'   y = c(10,12,13,16,18),
#'   x1 = c(2,3,4,5,6),
#'   x2 = c(5,4,6,7,8)
#' )
#'
#' m1 <- lm(y ~ x1, data = birds)
#' m2 <- lm(y ~ x1 + x2, data = birds)
#'
#' bt_rank_models(m1, m2)
#'
#' @export

bt_rank_models <- function(...){
  
  models <- list(...)
  
  if(length(models) < 2){
    
    stop(
      "Provide at least two fitted models.",
      call. = FALSE
    )
    
  }
  
  aics <- sapply(models, stats::AIC)
  
  delta <- aics - min(aics)
  
  weights <- exp(-0.5 * delta)
  
  weights <- weights / sum(weights)
  
  results <- data.frame(
    
    Model = paste0("Model_", seq_along(models)),
    
    AIC = round(aics, 3),
    
    Delta_AIC = round(delta, 3),
    
    Akaike_Weight = round(weights, 4),
    
    stringsAsFactors = FALSE
    
  )
  
  results <- results[order(results$AIC), ]
  
  results$Rank <- seq_len(nrow(results))
  
  rownames(results) <- NULL
  
  results
  
}
#===============================================================================
# BirdTraitsR
# Statistics Functions (Part 5L)
#
# Effect Size Utilities
#===============================================================================

#' Standardized Coefficients
#' @param model A fitted lm object.
#' @return Named numeric vector.
#' @export
bt_standardized_coefficients <- function(model){
  if(!inherits(model,"lm")) stop("Model must be a linear model.",call.=FALSE)
  mf<-stats::model.frame(model)
  dat<-mf
  dat[]<-lapply(dat,function(x) if(is.numeric(x)) as.numeric(scale(x)) else x)
  stats::coef(stats::lm(stats::formula(model),data=dat))
}

#' Odds Ratios
#' @param model Binomial glm.
#' @return Numeric vector.
#' @export
bt_odds_ratio <- function(model){
  if(!inherits(model,"glm")) stop("Model must be a glm.",call.=FALSE)
  if(model$family$family!="binomial") stop("Model must use binomial family.",call.=FALSE)
  exp(stats::coef(model))
}

#' Incidence Rate Ratios
#' @param model Poisson glm.
#' @return Numeric vector.
#' @export
bt_incidence_rate_ratio <- function(model){
  if(!inherits(model,"glm")) stop("Model must be a glm.",call.=FALSE)
  if(model$family$family!="poisson") stop("Model must use poisson family.",call.=FALSE)
  exp(stats::coef(model))
}

#' Exponentiated Coefficients
#' @param model A fitted model.
#' @return Numeric vector.
#' @export
bt_exponentiate_coefficients <- function(model){
  exp(stats::coef(model))
}
#===============================================================================
# BirdTraitsR
# Statistics Functions (Part 5M)
#
# Assumption Checking
#===============================================================================

#------------------------------------------------------------------------------
# Check Normality of Residuals
#------------------------------------------------------------------------------

#' Check Normality
#'
#' Performs the Shapiro-Wilk test on model residuals.
#'
#' @param model A fitted model object.
#'
#' @return An htest object.
#'
#' @examples
#' birds <- data.frame(
#'   y = c(20,25,18,30,27,24),
#'   x = c(12,14,11,16,15,13)
#' )
#'
#' model <- lm(y ~ x, data = birds)
#'
#' bt_check_normality(model)
#'
#' @export

bt_check_normality <- function(model){
  
  if(is.null(stats::residuals(model))){
    stop(
      "Model does not contain residuals.",
      call. = FALSE
    )
  }
  
  stats::shapiro.test(
    stats::residuals(model)
  )
  
}


#------------------------------------------------------------------------------
# Check Homoscedasticity
#------------------------------------------------------------------------------

#' Check Homoscedasticity
#'
#' Computes the correlation between fitted values and squared residuals.
#'
#' Small correlations indicate more homogeneous variance.
#'
#' @param model A fitted model object.
#'
#' @return A list.
#'
#' @examples
#' birds <- data.frame(
#'   y = c(20,25,18,30,27,24),
#'   x = c(12,14,11,16,15,13)
#' )
#'
#' model <- lm(y ~ x, data = birds)
#'
#' bt_check_homoscedasticity(model)
#'
#' @export

bt_check_homoscedasticity <- function(model){
  
  fitted_values <- stats::fitted(model)
  
  residuals <- stats::residuals(model)
  
  correlation <- stats::cor(
    
    fitted_values,
    
    residuals^2,
    
    use = "complete.obs"
    
  )
  
  result <- list(
    
    Correlation = correlation,
    
    Message = if(abs(correlation) < 0.30){
      
      "No strong evidence of heteroscedasticity."
      
    }else{
      
      "Possible heteroscedasticity detected."
      
    }
    
  )
  
  class(result) <- "BirdTraitsHomoscedasticity"
  
  result
  
}


#------------------------------------------------------------------------------
# Print Method
#------------------------------------------------------------------------------

#' @export

print.BirdTraitsHomoscedasticity <- function(x,...){
  
  cat("\n")
  
  cat("=====================================\n")
  cat(" Homoscedasticity Check\n")
  cat("=====================================\n\n")
  
  cat(
    "Correlation :",
    round(x$Correlation,4),
    "\n"
  )
  
  cat(
    x$Message,
    "\n"
  )
  
  invisible(x)
  
}
#------------------------------------------------------------------------------
# Check Multicollinearity (Variance Inflation Factor)
#------------------------------------------------------------------------------

#' Check Multicollinearity
#'
#' Computes Variance Inflation Factors (VIF) for predictors in
#' a linear model.
#'
#' Values greater than 5 may indicate problematic
#' multicollinearity.
#'
#' @param model A fitted linear model.
#'
#' @return A named numeric vector.
#'
#' @examples
#' birds <- data.frame(
#'   y  = c(20,25,18,30,27,24,29,31),
#'   x1 = c(12,14,11,16,15,13,17,18),
#'   x2 = c(8,9,7,10,9,8,11,12)
#' )
#'
#' model <- lm(y ~ x1 + x2, data = birds)
#'
#' bt_check_vif(model)
#'
#' @export

bt_check_vif <- function(model){
  
  if(!inherits(model, "lm")){
    stop(
      "Model must be a linear model.",
      call. = FALSE
    )
  }
  
  X <- stats::model.matrix(model)
  
  if("(Intercept)" %in% colnames(X)){
    X <- X[, colnames(X) != "(Intercept)", drop = FALSE]
  }
  
  if(ncol(X) < 2){
    stop(
      "VIF requires at least two predictor variables.",
      call. = FALSE
    )
  }
  
  vif <- numeric(ncol(X))
  
  names(vif) <- colnames(X)
  
  for(i in seq_len(ncol(X))){
    
    fit <- stats::lm(
      
      X[, i] ~
        
        X[, -i]
      
    )
    
    r2 <- summary(fit)$r.squared
    
    vif[i] <- 1 / (1 - r2)
    
  }
  
  vif
  
}


#------------------------------------------------------------------------------
# Check Autocorrelation
#------------------------------------------------------------------------------

#' Check Autocorrelation
#'
#' Computes the Durbin-Watson statistic.
#'
#' Values close to 2 indicate little evidence of
#' autocorrelation.
#'
#' @param model A fitted linear model.
#'
#' @return Numeric.
#'
#' @examples
#' birds <- data.frame(
#'   y = c(20,25,18,30,27,24),
#'   x = c(12,14,11,16,15,13)
#' )
#'
#' model <- lm(y ~ x, data = birds)
#'
#' bt_durbin_watson(model)
#'
#' @export

bt_durbin_watson <- function(model){
  
  if(!inherits(model, "lm")){
    stop(
      "Model must be a linear model.",
      call. = FALSE
    )
  }
  
  e <- stats::residuals(model)
  
  numerator <-
    
    sum(
      
      diff(e)^2
      
    )
  
  denominator <-
    
    sum(
      
      e^2
      
    )
  
  numerator / denominator
  
}
#===============================================================================
# BirdTraitsR
# Statistics Functions (Part 5M - Part 3)
#
# Influence Diagnostics
#===============================================================================

#------------------------------------------------------------------------------
# Cook's Distance
#------------------------------------------------------------------------------

#' Cook's Distance
#'
#' Calculates Cook's Distance for each observation.
#'
#' @param model A fitted regression model.
#'
#' @return Numeric vector.
#'
#' @examples
#' model <- lm(mpg ~ wt, data = mtcars)
#' bt_cooks_distance(model)
#'
#' @export

bt_cooks_distance <- function(model){
  
  if(!inherits(model, c("lm","glm"))){
    stop(
      "Model must be an lm or glm object.",
      call. = FALSE
    )
  }
  
  stats::cooks.distance(model)
  
}


#------------------------------------------------------------------------------
# Leverage Values
#------------------------------------------------------------------------------

#' Leverage Values
#'
#' Calculates leverage (hat values).
#'
#' @param model A fitted regression model.
#'
#' @return Numeric vector.
#'
#' @examples
#' model <- lm(mpg ~ wt, data = mtcars)
#' bt_leverage(model)
#'
#' @export

bt_leverage <- function(model){
  
  if(!inherits(model, c("lm","glm"))){
    stop(
      "Model must be an lm or glm object.",
      call. = FALSE
    )
  }
  
  stats::hatvalues(model)
  
}


#------------------------------------------------------------------------------
# DFBETAs
#------------------------------------------------------------------------------

#' DFBETAs
#'
#' Calculates DFBETAs for each coefficient.
#'
#' @param model A fitted regression model.
#'
#' @return Matrix.
#'
#' @examples
#' model <- lm(mpg ~ wt, data = mtcars)
#' bt_dfbetas(model)
#'
#' @export

bt_dfbetas <- function(model){
  
  if(!inherits(model, c("lm","glm"))){
    stop(
      "Model must be an lm or glm object.",
      call. = FALSE
    )
  }
  
  stats::dfbetas(model)
  
}


#------------------------------------------------------------------------------
# DFFITS
#------------------------------------------------------------------------------

#' DFFITS
#'
#' Calculates DFFITS statistics.
#'
#' @param model A fitted regression model.
#'
#' @return Numeric vector.
#'
#' @examples
#' model <- lm(mpg ~ wt, data = mtcars)
#' bt_dffits(model)
#'
#' @export

bt_dffits <- function(model){
  
  if(!inherits(model, c("lm","glm"))){
    stop(
      "Model must be an lm or glm object.",
      call. = FALSE
    )
  }
  
  stats::dffits(model)
  
}


#------------------------------------------------------------------------------
# Covariance Ratio (COVRATIO)
#------------------------------------------------------------------------------

#' Covariance Ratio
#'
#' Calculates Covariance Ratios (COVRATIO).
#'
#' @param model A fitted linear model.
#'
#' @return Numeric vector.
#'
#' @examples
#' model <- lm(mpg ~ wt, data = mtcars)
#' bt_covratio(model)
#'
#' @export

bt_covratio <- function(model){
  
  if(!inherits(model, "lm")){
    stop(
      "Model must be a linear model.",
      call. = FALSE
    )
  }
  
  stats::covratio(model)
  
}
#===============================================================================
# BirdTraitsR
# Statistics Functions (Part 5M)
#
# Model Assumption Tests (Part 4 of 4)
#===============================================================================

#------------------------------------------------------------------------------
# Complete Assumption Report
#------------------------------------------------------------------------------

#' Complete Model Assumption Report
#'
#' Generates a complete assumption report for a fitted model.
#'
#' Included diagnostics:
#' * Residual Normality
#' * Homoscedasticity
#' * Multicollinearity (if predictors supplied)
#' * Durbin-Watson Autocorrelation Test
#'
#' @param model A fitted model.
#' @param data Original data frame (required only for VIF).
#' @param predictors Optional predictor names for VIF.
#'
#' @return A list.
#'
#' @examples
#' model <- lm(
#'   Sepal.Length ~ Sepal.Width + Petal.Length,
#'   data = iris
#' )
#'
#' bt_assumption_report(
#'   model,
#'   iris,
#'   c("Sepal.Width","Petal.Length")
#' )
#'
#' @export

bt_assumption_report <- function(
    model,
    data = NULL,
    predictors = NULL
){
  
  report <- list()
  
  report$Normality <-
    bt_shapiro_residuals(model)
  
  report$Homoscedasticity <-
    bt_breusch_pagan(model)
  
  report$Durbin_Watson <-
    bt_durbin_watson(model)
  
  if(
    !is.null(data) &&
    !is.null(predictors)
  ){
    
    report$VIF <-
      bt_vif(
        data,
        predictors
      )
    
  }
  
  class(report) <- "BirdTraitsAssumption"
  
  report
  
}


#------------------------------------------------------------------------------
# Print Method
#------------------------------------------------------------------------------

#' @export

print.BirdTraitsAssumption <- function(x, ...){
  
  cat("\n")
  cat("=====================================\n")
  cat(" BirdTraitsR Assumption Report\n")
  cat("=====================================\n\n")
  
  cat("Residual Normality\n")
  print(x$Normality)
  
  cat("\n-------------------------------------\n\n")
  
  cat("Breusch-Pagan Test\n")
  print(x$Homoscedasticity)
  
  cat("\n-------------------------------------\n\n")
  
  cat("Durbin-Watson Test\n")
  print(x$Durbin_Watson)
  
  if(!is.null(x$VIF)){
    
    cat("\n-------------------------------------\n\n")
    
    cat("Variance Inflation Factors\n")
    print(x$VIF)
    
  }
  
  invisible(x)
  
}