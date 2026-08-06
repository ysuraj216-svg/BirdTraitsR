#===============================================================================
# BirdTraitsR
# Functional Diversity Functions
#
# Calculate functional diversity indices based on trait data.
#===============================================================================

#------------------------------------------------------------------------------
# Functional Richness
#------------------------------------------------------------------------------

#' Functional Richness
#'
#' Calculates functional richness (FRic) as the volume of trait space occupied
#' by species in a community.
#'
#' @param data Data frame with species as rows and traits as columns.
#' @param species Optional column name for species names (removed before calculation).
#'
#' @return Numeric value representing functional richness.
#'
#' @details
#' Functional richness represents the extent of trait space occupied by the
#' community. Higher values indicate greater trait diversity. Traits are
#' standardized (z-score: mean 0, sd 1) before computing range, so that
#' traits measured on different scales (e.g. mass in grams vs wing length
#' in cm) can be compared on equal footing without one trait's range
#' dominating the result.
#'
#' @examples
#' birds <- data.frame(
#'   species = c("A", "B", "C", "D"),
#'   mass = c(20, 25, 30, 22),
#'   wing = c(12, 14, 16, 13),
#'   tail = c(7, 8, 9, 8)
#' )
#'
#' bt_functional_richness(birds, "species")
#'
#' @export

bt_functional_richness <- function(
    data,
    species = NULL
){
  
  bt_check_dataframe(data)
  
  # Remove non-numeric columns if species column specified
  if(!is.null(species)){
    if(!species %in% names(data)){
      stop(
        paste0("Column '", species, "' not found."),
        call. = FALSE
      )
    }
    trait_data <- data[, sapply(data, is.numeric), drop = FALSE]
  } else {
    trait_data <- data[, sapply(data, is.numeric), drop = FALSE]
  }
  
  if(ncol(trait_data) == 0){
    stop(
      "No numeric columns found in data.",
      call. = FALSE
    )
  }
  
  # Remove rows with missing values
  trait_data <- stats::na.omit(trait_data)
  
  # Standardize traits using z-scores (mean 0, sd 1). Min-max scaling to
  # 0-1 would force every column's range to exactly 1, making the product
  # meaningless - z-scoring keeps traits comparable while preserving
  # genuine differences in spread between traits.
  trait_scaled <- as.data.frame(
    sapply(trait_data, function(x) as.numeric(scale(x)))
  )
  
  # Calculate convex hull volume using number of dimensions
  # Simplified: FRic is approximated as the product of standardized
  # trait ranges (a proxy for trait-space hypervolume).
  fric <- prod(sapply(trait_scaled, function(x) max(x) - min(x)))
  
  as.numeric(fric)
  
}

#------------------------------------------------------------------------------
# Functional Divergence
#------------------------------------------------------------------------------

#' Functional Divergence
#'
#' Calculates functional divergence (FDiv) as the regularity of species
#' distribution within trait space.
#'
#' @param data Data frame with species as rows and traits as columns.
#' @param species Optional column name for species names.
#'
#' @return Numeric value between 0 and 1 representing functional divergence.
#'
#' @details
#' Functional divergence measures how regularly species are distributed in
#' trait space. Higher values indicate more even distribution.
#'
#' @examples
#' birds <- data.frame(
#'   species = c("A", "B", "C", "D"),
#'   mass = c(20, 25, 30, 22),
#'   wing = c(12, 14, 16, 13)
#' )
#'
#' bt_functional_divergence(birds, "species")
#'
#' @export

bt_functional_divergence <- function(
    data,
    species = NULL
){
  
  bt_check_dataframe(data)
  
  # Remove non-numeric columns
  if(!is.null(species)){
    if(!species %in% names(data)){
      stop(
        paste0("Column '", species, "' not found."),
        call. = FALSE
      )
    }
    trait_data <- data[, sapply(data, is.numeric), drop = FALSE]
  } else {
    trait_data <- data[, sapply(data, is.numeric), drop = FALSE]
  }
  
  if(ncol(trait_data) == 0){
    stop(
      "No numeric columns found in data.",
      call. = FALSE
    )
  }
  
  # Remove rows with missing values
  trait_data <- stats::na.omit(trait_data)
  
  # Standardize traits
  trait_scaled <- as.data.frame(
    sapply(trait_data, function(x) (x - min(x)) / (max(x) - min(x)))
  )
  
  # Calculate centroid
  centroid <- colMeans(trait_scaled)
  
  # Calculate distances from centroid
  distances <- apply(trait_scaled, 1, function(x) sqrt(sum((x - centroid)^2)))
  
  # FDiv is approximated as the coefficient of variation in distances
  fdiv <- stats::sd(distances) / mean(distances)
  
  # Normalize to 0-1
  fdiv <- fdiv / (1 + fdiv)
  
  as.numeric(fdiv)
  
}

#------------------------------------------------------------------------------
# Functional Dispersion
#------------------------------------------------------------------------------

#' Functional Dispersion
#'
#' Calculates functional dispersion (FDis) as the weighted mean distance of
#' species from the centroid of trait space.
#'
#' @param data Data frame with species as rows and traits as columns.
#' @param species Optional column name for species names.
#' @param abundance Optional column name for abundance weights.
#'
#' @return Numeric value representing functional dispersion.
#'
#' @details
#' Functional dispersion measures the average distance of species from the
#' center of trait space. Higher values indicate greater trait divergence.
#'
#' @examples
#' birds <- data.frame(
#'   species = c("A", "B", "C", "D"),
#'   mass = c(20, 25, 30, 22),
#'   wing = c(12, 14, 16, 13)
#' )
#'
#' bt_functional_dispersion(birds, "species")
#'
#' @export

bt_functional_dispersion <- function(
    data,
    species = NULL,
    abundance = NULL
){
  
  bt_check_dataframe(data)
  
  if(!is.null(species) && !species %in% names(data)){
    stop(
      paste0("Column '", species, "' not found."),
      call. = FALSE
    )
  }
  
  # Identify numeric columns, excluding the abundance column if one was
  # supplied. Comparing names(data) to abundance is only safe once we know
  # abundance is not NULL - comparing anything to NULL with != returns an
  # empty result and silently drops every column.
  numeric_cols <- sapply(data, is.numeric)
  
  if(!is.null(abundance)){
    numeric_cols <- numeric_cols & names(data) != abundance
  }
  
  trait_data <- data[, numeric_cols, drop = FALSE]
  
  if(ncol(trait_data) == 0){
    stop(
      "No numeric columns found in data.",
      call. = FALSE
    )
  }
  
  # Remove rows with missing values
  trait_data <- stats::na.omit(trait_data)
  
  # Standardize traits
  trait_scaled <- as.data.frame(
    sapply(trait_data, function(x) (x - min(x)) / (max(x) - min(x)))
  )
  
  # Get abundance weights if provided
  if(!is.null(abundance) && abundance %in% names(data)){
    weights <- data[[abundance]][!is.na(rowSums(trait_data))]
    weights <- weights / sum(weights)
  } else {
    weights <- rep(1 / nrow(trait_scaled), nrow(trait_scaled))
  }
  
  # Calculate weighted centroid
  centroid <- colSums(trait_scaled * weights)
  
  # Calculate weighted distances from centroid
  distances <- apply(trait_scaled, 1, function(x) sqrt(sum((x - centroid)^2)))
  
  # FDis = weighted mean distance
  fdis <- sum(distances * weights)
  
  as.numeric(fdis)
  
}

#------------------------------------------------------------------------------
# Functional Evenness
#------------------------------------------------------------------------------

#' Functional Evenness
#'
#' Calculates functional evenness (FEve) as the regularity of distances
#' between neighboring species in trait space.
#'
#' @param data Data frame with species as rows and traits as columns.
#' @param species Optional column name for species names.
#'
#' @return Numeric value between 0 and 1 representing functional evenness.
#'
#' @details
#' Functional evenness measures how evenly species are distributed across
#' trait space. Higher values indicate more even distribution.
#'
#' @examples
#' birds <- data.frame(
#'   species = c("A", "B", "C", "D", "E"),
#'   mass = c(20, 25, 30, 22, 28),
#'   wing = c(12, 14, 16, 13, 15)
#' )
#'
#' bt_functional_evenness(birds, "species")
#'
#' @export

bt_functional_evenness <- function(
    data,
    species = NULL
){
  
  bt_check_dataframe(data)
  
  # Remove non-numeric columns
  if(!is.null(species)){
    if(!species %in% names(data)){
      stop(
        paste0("Column '", species, "' not found."),
        call. = FALSE
      )
    }
    trait_data <- data[, sapply(data, is.numeric), drop = FALSE]
  } else {
    trait_data <- data[, sapply(data, is.numeric), drop = FALSE]
  }
  
  if(ncol(trait_data) == 0){
    stop(
      "No numeric columns found in data.",
      call. = FALSE
    )
  }
  
  if(nrow(trait_data) < 2){
    warning("At least 2 species required to calculate functional evenness.")
    return(NA)
  }
  
  # Remove rows with missing values
  trait_data <- stats::na.omit(trait_data)
  
  # Standardize traits
  trait_scaled <- as.data.frame(
    sapply(trait_data, function(x) (x - min(x)) / (max(x) - min(x)))
  )
  
  # Calculate pairwise distances
  dist_matrix <- stats::dist(trait_scaled)
  
  # Calculate nearest neighbor distances
  dist_df <- as.matrix(dist_matrix)
  diag(dist_df) <- Inf
  nn_distances <- apply(dist_df, 1, min)
  
  # FEve = coefficient of variation in nearest neighbor distances (0-1 scale)
  if(stats::sd(nn_distances) == 0){
    feve <- 0
  } else {
    feve <- 1 - (stats::sd(nn_distances) / mean(nn_distances))
    feve <- max(0, min(1, feve))
  }
  
  as.numeric(feve)
  
}

#------------------------------------------------------------------------------
# Trait Space Plot
#------------------------------------------------------------------------------

#' Plot Trait Space
#'
#' Creates a 2D visualization of species in trait space.
#'
#' @param data Data frame with species as rows and traits as columns.
#' @param x Character. Name of trait for x-axis.
#' @param y Character. Name of trait for y-axis.
#' @param species Optional column name for species labels.
#' @param color Optional column name for coloring points.
#'
#' @return A ggplot object.
#'
#' @examples
#' birds <- data.frame(
#'   species = c("A", "B", "C", "D"),
#'   mass = c(20, 25, 30, 22),
#'   wing = c(12, 14, 16, 13)
#' )
#'
#' bt_plot_trait_space(birds, "mass", "wing", "species")
#'
#' @export

bt_plot_trait_space <- function(
    data,
    x,
    y,
    species = NULL,
    color = NULL
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
    stop("X trait must be numeric.", call. = FALSE)
  }
  
  if(!is.numeric(data[[y]])){
    stop("Y trait must be numeric.", call. = FALSE)
  }
  
  # Create aesthetic mapping
  aes_list <- list(
    x = rlang::sym(x),
    y = rlang::sym(y)
  )
  
  if(!is.null(color) && color %in% names(data)){
    aes_list$color <- rlang::sym(color)
  }
  
  # Build plot
  p <- ggplot2::ggplot(
    data,
    ggplot2::aes(!!!aes_list)
  ) +
    ggplot2::geom_point(size = 3, alpha = 0.7) +
    ggplot2::labs(
      title = paste(y, "vs", x),
      x = x,
      y = y
    ) +
    ggplot2::theme_bw(base_size = 13) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(
        face = "bold",
        hjust = 0.5
      )
    )
  
  # Add species labels if provided
  if(!is.null(species) && species %in% names(data)){
    p <- p +
      ggplot2::geom_text(
        ggplot2::aes(label = .data[[species]]),
        vjust = -0.5,
        size = 3
      )
  }
  
  p
  
}
