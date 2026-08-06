#===============================================================================
# BirdTraitsR
# Model Assumption Checks
#
# Helper functions used by bt_assumption_report().
#===============================================================================

#------------------------------------------------------------------------------
# Shapiro-Wilk Test on Residuals
#------------------------------------------------------------------------------

#' Shapiro-Wilk Normality Test on Model Residuals
#'
#' Tests whether the residuals of a fitted model are normally distributed.
#'
#' @param model A fitted model object (e.g. from lm()).
#'
#' @return A list with the test statistic, p-value, and a logical
#'   indicating whether residuals appear normal (p > 0.05).
#'
#' @examples
#' model <- lm(mpg ~ wt, data = mtcars)
#' bt_shapiro_residuals(model)
#'
#' @export

bt_shapiro_residuals <- function(model){
  
  resid_vals <- stats::residuals(model)
  
  test_result <- stats::shapiro.test(resid_vals)
  
  list(
    statistic = as.numeric(test_result$statistic),
    p_value = test_result$p.value,
    normal = test_result$p.value > 0.05
  )
  
}

#------------------------------------------------------------------------------
# Breusch-Pagan Test
#------------------------------------------------------------------------------

#' Breusch-Pagan Test for Homoscedasticity
#'
#' Tests whether the residual variance of a fitted model is constant
#' (homoscedastic) across fitted values.
#'
#' @param model A fitted model object (e.g. from lm()).
#'
#' @return A list with the test statistic, p-value, and a logical
#'   indicating whether variance appears homogeneous (p > 0.05).
#'
#' @examples
#' model <- lm(mpg ~ wt, data = mtcars)
#' bt_breusch_pagan(model)
#'
#' @export

bt_breusch_pagan <- function(model){
  
  resid_vals <- stats::residuals(model)
  fitted_vals <- stats::fitted(model)
  
  resid_sq <- resid_vals^2
  
  aux_model <- stats::lm(resid_sq ~ fitted_vals)
  
  n <- length(resid_vals)
  r_squared <- summary(aux_model)$r.squared
  
  test_stat <- n * r_squared
  p_value <- 1 - stats::pchisq(test_stat, df = 1)
  
  list(
    statistic = test_stat,
    p_value = p_value,
    homoscedastic = p_value > 0.05
  )
  
}

#------------------------------------------------------------------------------
# Variance Inflation Factor
#------------------------------------------------------------------------------

#' Variance Inflation Factor (VIF)
#'
#' Calculates the variance inflation factor for a set of predictor columns,
#' used to detect multicollinearity. Unlike bt_check_vif(), which takes an
#' already-fitted model, this function computes VIF directly from a data
#' frame and a vector of predictor column names.
#'
#' @param data A data frame.
#' @param predictors Character vector of two or more numeric predictor
#'   column names.
#'
#' @return A named numeric vector of VIF values, one per predictor.
#'
#' @examples
#' bt_vif(iris, c("Sepal.Width", "Petal.Length", "Petal.Width"))
#'
#' @export

bt_vif <- function(data, predictors){
  
  if(length(predictors) < 2){
    stop(
      "VIF requires at least two predictor variables.",
      call. = FALSE
    )
  }
  
  missing_cols <- predictors[!predictors %in% names(data)]
  
  if(length(missing_cols) > 0){
    stop(
      paste0(
        "Column(s) not found: ",
        paste(missing_cols, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  
  pred_data <- data[, predictors, drop = FALSE]
  
  if(any(!vapply(pred_data, is.numeric, logical(1)))){
    stop(
      "All predictor columns must be numeric.",
      call. = FALSE
    )
  }
  
  vif_values <- vapply(
    seq_along(predictors),
    function(i){
      formula_i <- stats::as.formula(
        paste(predictors[i], "~", paste(predictors[-i], collapse = " + "))
      )
      fit_i <- stats::lm(formula_i, data = pred_data)
      1 / (1 - summary(fit_i)$r.squared)
    },
    numeric(1)
  )
  
  names(vif_values) <- predictors
  
  vif_values
  
}