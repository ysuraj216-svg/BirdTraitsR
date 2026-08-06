#===============================================================================
# BirdTraitsR
# Module 05 - Filtering Functions
#===============================================================================
#' Filter by Species
#' @param data Data frame.
#' @param species Character vector of species names.
#' @return Filtered data frame.
#' @export
bt_filter_species <- function(data, species){
  bt_check_dataframe(data)
  if(!"species" %in% names(data)) stop("'species' column not found.", call.=FALSE)
  data[data$species %in% species,,drop=FALSE]
}
#' Filter by Family
#' @param data Data frame.
#' @param family Character vector.
#' @return Filtered data frame.
#' @export
bt_filter_family <- function(data, family){
  bt_check_dataframe(data)
  if(!"family" %in% names(data)) stop("'family' column not found.", call.=FALSE)
  data[data$family %in% family,,drop=FALSE]
}
#' Filter by Order
#' @param data Data frame.
#' @param order Character vector.
#' @return Filtered data frame.
#' @export
bt_filter_order <- function(data, order){
  bt_check_dataframe(data)
  if(!"order" %in% names(data)) stop("'order' column not found.", call.=FALSE)
  data[data$order %in% order,,drop=FALSE]
}
#' Filter by Any Trait
#' @param data Data frame.
#' @param trait Column name.
#' @param value Value(s) to keep.
#' @return Filtered data frame.
#' @export
bt_filter_trait <- function(data, trait, value){
  bt_check_dataframe(data)
  if(!trait %in% names(data)) stop("Trait column not found.", call.=FALSE)
  data[data[[trait]] %in% value,,drop=FALSE]
}
#' Filter by IUCN Category
#' @param data Data frame.
#' @param iucn Character vector.
#' @return Filtered data frame.
#' @export
bt_filter_iucn <- function(data, iucn){
  bt_check_dataframe(data)
  if(!"iucn_status" %in% names(data)) stop("'iucn_status' column not found.", call.=FALSE)
  data[data$iucn_status %in% iucn,,drop=FALSE]
}
#' Filter Using Multiple Conditions
#' @param data Data frame.
#' @param ...
#' Named filters, e.g. family="Accipitridae".
#' @return Filtered data frame.
#' @export
bt_filter_multiple <- function(data, ...){
  bt_check_dataframe(data)
  dots <- list(...)
  out <- data
  for(nm in names(dots)){
    if(!nm %in% names(out)) stop(paste("Column", nm, "not found."), call.=FALSE)
    out <- out[out[[nm]] %in% dots[[nm]],,drop=FALSE]
  }
  out
}
#' Remove Duplicate Rows
#' @param data Data frame.
#' @return Data frame.
#' @export
bt_remove_duplicates <- function(data){
  bt_check_dataframe(data)
  unique(data)
}
#' Select Columns
#' @param data Data frame.
#' @param columns Character vector.
#' @return Data frame.
#' @export
bt_select_columns <- function(data, columns){
  bt_check_dataframe(data)
  missing <- setdiff(columns, names(data))
  if(length(missing)>0)
    stop(paste("Missing columns:", paste(missing, collapse=", ")), call.=FALSE)
  data[, columns, drop=FALSE]
}
