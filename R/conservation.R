#===============================================================================
# BirdTraitsR
# Conservation Functions
#
# Analyze conservation status and threatened species in bird datasets.
#===============================================================================

#------------------------------------------------------------------------------
# IUCN Status Summary
#------------------------------------------------------------------------------

#' IUCN Conservation Status Summary
#'
#' Summarizes the distribution of IUCN conservation status categories
#' across species in the dataset.
#'
#' @param data Data frame.
#' @param iucn Column name containing IUCN status codes.
#' @param species Optional column name for species names.
#'
#' @return Data frame with status categories and counts.
#'
#' @examples
#' birds <- data.frame(
#'   species = c("A", "B", "C", "D", "E"),
#'   iucn = c("LC", "VU", "EN", "LC", "CR")
#' )
#'
#' bt_iucn_summary(birds, "iucn", "species")
#'
#' @export

bt_iucn_summary <- function(
    data,
    iucn = "iucn_status",
    species = NULL
){
  
  bt_check_dataframe(data)
  
  if(!iucn %in% names(data)){
    stop(
      paste0("Column '", iucn, "' not found."),
      call. = FALSE
    )
  }
  
  # IUCN categories
  status_order <- c("EX", "EW", "CR", "EN", "VU", "NT", "LC", "DD", "NE")
  status_labels <- c(
    "EX" = "Extinct",
    "EW" = "Extinct in the Wild",
    "CR" = "Critically Endangered",
    "EN" = "Endangered",
    "VU" = "Vulnerable",
    "NT" = "Near Threatened",
    "LC" = "Least Concern",
    "DD" = "Data Deficient",
    "NE" = "Not Evaluated"
  )
  
  # Count by status
  summary_table <- data.frame(
    Status_Code = character(),
    Status_Name = character(),
    Count = integer(),
    Percentage = numeric(),
    stringsAsFactors = FALSE
  )
  
  for(st in status_order){
    count <- sum(data[[iucn]] == st, na.rm = TRUE)
    if(count > 0){
      pct <- round(count / sum(!is.na(data[[iucn]])) * 100, 1)
      summary_table <- rbind(
        summary_table,
        data.frame(
          Status_Code = st,
          Status_Name = status_labels[st],
          Count = count,
          Percentage = pct,
          stringsAsFactors = FALSE
        )
      )
    }
  }
  
  class(summary_table) <- c("BirdTraitsConservation", "data.frame")
  summary_table
  
}

#------------------------------------------------------------------------------
# Threatened Species
#------------------------------------------------------------------------------

#' Identify Threatened Species
#'
#' Returns list of species categorized as threatened (CR, EN, or VU).
#'
#' @param data Data frame.
#' @param species Column name containing species names.
#' @param iucn Column name containing IUCN status.
#'
#' @return Data frame with threatened species and their status.
#'
#' @examples
#' birds <- data.frame(
#'   species = c("A", "B", "C", "D", "E"),
#'   iucn = c("LC", "VU", "EN", "LC", "CR")
#' )
#'
#' bt_threatened_species(birds, "species", "iucn")
#'
#' @export

bt_threatened_species <- function(
    data,
    species = "species",
    iucn = "iucn_status"
){
  
  bt_check_dataframe(data)
  
  if(!species %in% names(data)){
    stop(
      paste0("Column '", species, "' not found."),
      call. = FALSE
    )
  }
  
  if(!iucn %in% names(data)){
    stop(
      paste0("Column '", iucn, "' not found."),
      call. = FALSE
    )
  }
  
  threatened_categories <- c("CR", "EN", "VU")
  
  status_labels <- c(
    "CR" = "Critically Endangered",
    "EN" = "Endangered",
    "VU" = "Vulnerable"
  )
  
  threatened_data <- data[data[[iucn]] %in% threatened_categories, ]
  
  if(nrow(threatened_data) == 0){
    message("No threatened species found in dataset.")
    return(
      data.frame(
        Species = character(),
        Status = character(),
        Status_Name = character(),
        stringsAsFactors = FALSE
      )
    )
  }
  
  result <- data.frame(
    Species = threatened_data[[species]],
    Status = threatened_data[[iucn]],
    Status_Name = status_labels[threatened_data[[iucn]]],
    stringsAsFactors = FALSE
  )
  
  result <- result[order(result$Status, decreasing = TRUE), ]
  rownames(result) <- NULL
  
  result
  
}

#------------------------------------------------------------------------------
# Endemic Species
#------------------------------------------------------------------------------

#' Identify Endemic Species
#'
#' Identifies species with restricted geographic range (endemics).
#' Requires latitude and longitude columns.
#'
#' @param data Data frame.
#' @param species Column name containing species names.
#' @param latitude Column name containing latitude.
#' @param longitude Column name containing longitude.
#' @param range_threshold Range size threshold (in degrees) for considering endemic.
#'
#' @return Data frame with endemic species and their range.
#'
#' @examples
#' birds <- data.frame(
#'   species = c("A", "B", "C"),
#'   latitude = c(51.5, 52.0, 51.5),
#'   longitude = c(-0.1, -0.5, -0.1)
#' )
#'
#' bt_endemics(birds, "species", "latitude", "longitude")
#'
#' @export

bt_endemics <- function(
    data,
    species = "species",
    latitude = "latitude",
    longitude = "longitude",
    range_threshold = 5
){
  
  bt_check_dataframe(data)
  
  if(!species %in% names(data)){
    stop(
      paste0("Column '", species, "' not found."),
      call. = FALSE
    )
  }
  
  if(!latitude %in% names(data)){
    stop(
      paste0("Column '", latitude, "' not found."),
      call. = FALSE
    )
  }
  
  if(!longitude %in% names(data)){
    stop(
      paste0("Column '", longitude, "' not found."),
      call. = FALSE
    )
  }
  
  # Calculate range for each species
  result <- data.frame(
    Species = character(),
    Latitude_Range = numeric(),
    Longitude_Range = numeric(),
    Endemic = logical(),
    stringsAsFactors = FALSE
  )
  
  species_list <- unique(data[[species]])
  
  for(sp in species_list){
    sp_data <- data[data[[species]] == sp, ]
    
    lat_range <- max(sp_data[[latitude]], na.rm = TRUE) - 
      min(sp_data[[latitude]], na.rm = TRUE)
    lon_range <- max(sp_data[[longitude]], na.rm = TRUE) - 
      min(sp_data[[longitude]], na.rm = TRUE)
    
    is_endemic <- lat_range < range_threshold & lon_range < range_threshold
    
    result <- rbind(
      result,
      data.frame(
        Species = sp,
        Latitude_Range = round(lat_range, 2),
        Longitude_Range = round(lon_range, 2),
        Endemic = is_endemic,
        stringsAsFactors = FALSE
      )
    )
  }
  
  result <- result[result$Endemic, ]
  result <- result[order(result$Latitude_Range + result$Longitude_Range), ]
  rownames(result) <- NULL
  
  result
  
}

#------------------------------------------------------------------------------
# Conservation Score
#------------------------------------------------------------------------------

#' Calculate Conservation Score
#'
#' Calculates a composite conservation score based on IUCN status,
#' threat exposure, and range size.
#'
#' @param data Data frame.
#' @param species Column name containing species names.
#' @param iucn Column name containing IUCN status.
#' @param latitude Optional latitude column for range calculation.
#' @param longitude Optional longitude column for range calculation.
#'
#' @return Data frame with species and conservation scores (0-100).
#'
#' @examples
#' birds <- data.frame(
#'   species = c("A", "B", "C"),
#'   iucn = c("LC", "VU", "EN")
#' )
#'
#' bt_conservation_score(birds, "species", "iucn")
#'
#' @export

bt_conservation_score <- function(
    data,
    species = "species",
    iucn = "iucn_status",
    latitude = NULL,
    longitude = NULL
){
  
  bt_check_dataframe(data)
  
  if(!species %in% names(data)){
    stop(
      paste0("Column '", species, "' not found."),
      call. = FALSE
    )
  }
  
  if(!iucn %in% names(data)){
    stop(
      paste0("Column '", iucn, "' not found."),
      call. = FALSE
    )
  }
  
  # IUCN score weights (higher = more threatened)
  iucn_scores <- c(
    "EX" = 100,  # Extinct
    "EW" = 90,   # Extinct in wild
    "CR" = 80,   # Critically Endangered
    "EN" = 60,   # Endangered
    "VU" = 40,   # Vulnerable
    "NT" = 20,   # Near Threatened
    "LC" = 10,   # Least Concern
    "DD" = 25,   # Data Deficient
    "NE" = 15    # Not Evaluated
  )
  
  result <- data.frame(
    Species = unique(data[[species]]),
    IUCN_Score = numeric(length(unique(data[[species]]))),
    Range_Score = numeric(length(unique(data[[species]]))),
    Conservation_Score = numeric(length(unique(data[[species]]))),
    stringsAsFactors = FALSE
  )
  
  for(i in seq_along(result$Species)){
    sp <- result$Species[i]
    sp_data <- data[data[[species]] == sp, ]
    
    # IUCN score
    iucn_status <- sp_data[[iucn]][1]
    iucn_score <- iucn_scores[iucn_status]
    if(is.na(iucn_score)) iucn_score <- 0
    result$IUCN_Score[i] <- iucn_score
    
    # Range score (if lat/lon provided)
    if(!is.null(latitude) && !is.null(longitude) &&
       latitude %in% names(data) && longitude %in% names(data)){
      
      lat_range <- max(sp_data[[latitude]], na.rm = TRUE) - 
        min(sp_data[[latitude]], na.rm = TRUE)
      lon_range <- max(sp_data[[longitude]], na.rm = TRUE) - 
        min(sp_data[[longitude]], na.rm = TRUE)
      range_size <- lat_range * lon_range
      
      # Smaller range = higher score (more threatened)
      if(range_size > 0){
        range_score <- 100 / (1 + range_size)
      } else {
        range_score <- 0
      }
      result$Range_Score[i] <- range_score
    }
    
    # Combined score (average with weights)
    result$Conservation_Score[i] <- round(
      mean(c(result$IUCN_Score[i], result$Range_Score[i]), na.rm = TRUE),
      1
    )
  }
  
  result <- result[order(result$Conservation_Score, decreasing = TRUE), ]
  rownames(result) <- NULL
  
  result
  
}