#===============================================================================
# BirdTraitsR
# Diversity Analysis
#===============================================================================

#------------------------------------------------------------------------------
# Species Richness
#------------------------------------------------------------------------------

#' Species Richness
#'
#' Calculates the number of unique species.
#'
#' @param data Data frame.
#' @param species Column containing species names.
#'
#' @return Integer.
#'
#' @examples
#' birds <- data.frame(
#'   species = c("A","B","C","A")
#' )
#'
#' bt_species_richness(birds)
#'
#' @export

bt_species_richness <- function(
    data,
    species = "species"
){
  
  bt_check_dataframe(data)
  
  if(!species %in% names(data))
    stop("Species column not found.", call.=FALSE)
  
  length(
    unique(
      data[[species]]
    )
  )
  
}

#------------------------------------------------------------------------------
# Number of Families
#------------------------------------------------------------------------------

#' Family Richness
#'
#' Counts unique bird families.
#'
#' @param data Data frame.
#' @param family Family column.
#'
#' @return Integer.
#'
#' @export

bt_family_richness <- function(
    data,
    family = "family"
){
  
  bt_check_dataframe(data)
  
  if(!family %in% names(data))
    stop("Family column not found.", call.=FALSE)
  
  length(
    unique(
      data[[family]]
    )
  )
  
}

#------------------------------------------------------------------------------
# Number of Orders
#------------------------------------------------------------------------------

#' Order Richness
#'
#' Counts unique bird orders.
#'
#' @param data Data frame.
#' @param order Order column.
#'
#' @return Integer.
#'
#' @export

bt_order_richness <- function(
    data,
    order = "order"
){
  
  bt_check_dataframe(data)
  
  if(!order %in% names(data))
    stop("Order column not found.", call.=FALSE)
  
  length(
    unique(
      data[[order]]
    )
  )
  
}

#------------------------------------------------------------------------------
# Diversity Summary
#------------------------------------------------------------------------------

#' Diversity Summary
#'
#' Summarizes species, families and orders.
#'
#' @param data Data frame.
#' @param species Species column.
#' @param family Family column.
#' @param order Order column.
#'
#' @return BirdTraitsDiversity object.
#'
#' @export

bt_diversity_summary <- function(
    data,
    species="species",
    family="family",
    order="order"
){
  
  result <- list(
    
    Species =
      bt_species_richness(data,species),
    
    Families =
      bt_family_richness(data,family),
    
    Orders =
      bt_order_richness(data,order)
    
  )
  
  class(result) <- "BirdTraitsDiversity"
  
  result
  
}

#------------------------------------------------------------------------------
# Print Method
#------------------------------------------------------------------------------

#' @export

print.BirdTraitsDiversity <- function(x,...){
  
  cat("\n")
  
  cat("===================================\n")
  cat(" Bird Diversity Summary\n")
  cat("===================================\n\n")
  
  cat("Species :",x$Species,"\n")
  cat("Families:",x$Families,"\n")
  cat("Orders  :",x$Orders,"\n\n")
  
  invisible(x)
  
}
#===============================================================================
# BirdTraitsR
# Diversity Analysis (Part 1)
#
# Relative abundance and occurrence functions
#===============================================================================

#------------------------------------------------------------------------------
# Relative Abundance
#------------------------------------------------------------------------------

#' Relative Abundance
#'
#' Calculates abundance and relative abundance (%) of each species.
#'
#' @param data Data frame.
#' @param species Species column.
#'
#' @return Data frame.
#'
#' @examples
#' birds <- data.frame(
#'   species=c("A","A","B","C","C","C")
#' )
#'
#' bt_relative_abundance(birds)
#'
#' @export

bt_relative_abundance <- function(
    data,
    species = "species"
){
  
  bt_check_dataframe(data)
  
  if(!species %in% names(data)){
    stop(
      "Species column not found.",
      call. = FALSE
    )
  }
  
  counts <- table(data[[species]])
  
  output <- data.frame(
    
    Species = names(counts),
    
    Abundance = as.integer(counts),
    
    Relative_Abundance =
      round(
        as.numeric(100 * counts / sum(counts)),
        2
      ),
    
    stringsAsFactors = FALSE
    
  )
  
  output <-
    output[
      order(
        output$Abundance,
        decreasing = TRUE
      ),
    ]
  
  rownames(output) <- NULL
  
  output
  
}

#------------------------------------------------------------------------------
# Species Frequency
#------------------------------------------------------------------------------

#' Species Frequency
#'
#' Counts the number of records for each species.
#'
#' @param data Data frame.
#' @param species Species column.
#'
#' @return Data frame.
#'
#' @examples
#' birds <- data.frame(
#'   species=c("A","A","B","C")
#' )
#'
#' bt_species_frequency(birds)
#'
#' @export

bt_species_frequency <- function(
    data,
    species = "species"
){
  
  bt_check_dataframe(data)
  
  if(!species %in% names(data)){
    stop(
      "Species column not found.",
      call. = FALSE
    )
  }
  
  counts <- table(data[[species]])
  
  output <- data.frame(
    
    Species = names(counts),
    
    Frequency = as.integer(counts),
    
    stringsAsFactors = FALSE
    
  )
  
  output <-
    output[
      order(
        output$Frequency,
        decreasing = TRUE
      ),
    ]
  
  rownames(output) <- NULL
  
  output
  
}

#------------------------------------------------------------------------------
# Occurrence Frequency
#------------------------------------------------------------------------------

#' Occurrence Frequency
#'
#' Calculates the percentage of sampling units in which each species occurs.
#'
#' @param data Data frame.
#' @param species Species column.
#' @param site Sampling site column.
#'
#' @return Data frame.
#'
#' @examples
#' birds <- data.frame(
#'   site=c(1,1,2,2,3),
#'   species=c("A","B","A","C","A")
#' )
#'
#' bt_occurrence_frequency(
#'   birds,
#'   site="site"
#' )
#'
#' @export

bt_occurrence_frequency <- function(
    data,
    species = "species",
    site = "site"
){
  
  bt_check_dataframe(data)
  
  bt_check_columns(
    data,
    c(species, site)
  )
  
  total_sites <-
    length(
      unique(data[[site]])
    )
  
  occurrence <-
    
    stats::aggregate(
      
      data[[site]],
      
      by = list(
        Species = data[[species]]
      ),
      
      FUN = function(x){
        
        length(unique(x))
        
      }
      
    )
  
  names(occurrence)[2] <- "Sites"
  
  occurrence$Occurrence_Percent <-
    
    round(
      
      occurrence$Sites /
        total_sites *
        100,
      
      2
      
    )
  
  occurrence <-
    
    occurrence[
      order(
        occurrence$Occurrence_Percent,
        decreasing = TRUE
      ),
    ]
  
  rownames(occurrence) <- NULL
  
  occurrence
  
}

#------------------------------------------------------------------------------
# Presence Absence Matrix
#------------------------------------------------------------------------------

#' Presence Absence Matrix
#'
#' Creates a species-by-site presence absence matrix.
#'
#' @param data Data frame.
#' @param species Species column.
#' @param site Site column.
#'
#' @return Matrix.
#'
#' @examples
#' birds <- data.frame(
#'   site=c(1,1,2,2,3),
#'   species=c("A","B","A","C","A")
#' )
#'
#' bt_presence_absence_matrix(
#'   birds,
#'   site="site"
#' )
#'
#' @export

bt_presence_absence_matrix <- function(
    data,
    species = "species",
    site = "site"
){
  
  bt_check_dataframe(data)
  
  bt_check_columns(
    data,
    c(species, site)
  )
  
  tab <- table(
    
    data[[species]],
    
    data[[site]]
    
  )
  
  matrix <- ifelse(
    tab > 0,
    1,
    0
  )
  
  matrix
  
}
#===============================================================================
# BirdTraitsR
# Diversity Analysis (Part 2)
#
# Diversity Indices (Base R)
#===============================================================================

#------------------------------------------------------------------------------
# Shannon Diversity Index
#------------------------------------------------------------------------------

#' Shannon Diversity Index
#'
#' Calculates Shannon-Wiener diversity index.
#'
#' @param data Data frame.
#' @param species Species column.
#'
#' @return Numeric.
#'
#' @examples
#' birds <- data.frame(
#'   species=c(
#'     "A","A","A",
#'     "B","B",
#'     "C",
#'     "D"
#'   )
#' )
#'
#' bt_shannon_index(birds)
#'
#' @export

bt_shannon_index <- function(
    data,
    species = "species"
){
  
  bt_check_dataframe(data)
  
  if(!species %in% names(data)){
    stop(
      "Species column not found.",
      call. = FALSE
    )
  }
  
  counts <- table(data[[species]])
  
  p <- counts / sum(counts)
  
  -sum(
    p * log(p)
  )
  
}

#------------------------------------------------------------------------------
# Simpson Diversity Index
#------------------------------------------------------------------------------

#' Simpson Diversity Index
#'
#' Calculates Simpson diversity (1-D).
#'
#' @param data Data frame.
#' @param species Species column.
#'
#' @return Numeric.
#'
#' @examples
#' birds <- data.frame(
#'   species=c(
#'     "A","A","A",
#'     "B","B",
#'     "C",
#'     "D"
#'   )
#' )
#'
#' bt_simpson_index(birds)
#'
#' @export

bt_simpson_index <- function(
    data,
    species = "species"
){
  
  bt_check_dataframe(data)
  
  if(!species %in% names(data)){
    stop(
      "Species column not found.",
      call. = FALSE
    )
  }
  
  counts <- table(data[[species]])
  
  p <- counts / sum(counts)
  
  1 - sum(p^2)
  
}

#------------------------------------------------------------------------------
# Pielou Evenness
#------------------------------------------------------------------------------

#' Pielou Evenness Index
#'
#' Calculates community evenness.
#'
#' @param data Data frame.
#' @param species Species column.
#'
#' @return Numeric.
#'
#' @examples
#' birds <- data.frame(
#'   species=c(
#'     "A","A","A",
#'     "B","B",
#'     "C",
#'     "D"
#'   )
#' )
#'
#' bt_evenness(birds)
#'
#' @export

bt_evenness <- function(
    data,
    species = "species"
){
  
  bt_check_dataframe(data)
  
  S <- bt_species_richness(
    data,
    species
  )
  
  if(S <= 1){
    
    return(NA_real_)
    
  }
  
  H <- bt_shannon_index(
    data,
    species
  )
  
  H / log(S)
  
}

#------------------------------------------------------------------------------
# Margalef Richness Index
#------------------------------------------------------------------------------

#' Margalef Richness Index
#'
#' Calculates Margalef richness.
#'
#' @param data Data frame.
#' @param species Species column.
#'
#' @return Numeric.
#'
#' @examples
#' birds <- data.frame(
#'   species=c(
#'     "A","A","A",
#'     "B","B",
#'     "C",
#'     "D"
#'   )
#' )
#'
#' bt_margalef_index(birds)
#'
#' @export

bt_margalef_index <- function(
    data,
    species = "species"
){
  
  bt_check_dataframe(data)
  
  S <- bt_species_richness(
    data,
    species
  )
  
  N <- nrow(data)
  
  if(N <= 1){
    
    return(NA_real_)
    
  }
  
  (S - 1) / log(N)
  
}

#------------------------------------------------------------------------------
# Menhinick Richness Index
#------------------------------------------------------------------------------

#' Menhinick Richness Index
#'
#' Calculates Menhinick richness.
#'
#' @param data Data frame.
#' @param species Species column.
#'
#' @return Numeric.
#'
#' @examples
#' birds <- data.frame(
#'   species=c(
#'     "A","A","A",
#'     "B","B",
#'     "C",
#'     "D"
#'   )
#' )
#'
#' bt_menhinick_index(birds)
#'
#' @export

bt_menhinick_index <- function(
    data,
    species = "species"
){
  
  bt_check_dataframe(data)
  
  S <- bt_species_richness(
    data,
    species
  )
  
  N <- nrow(data)
  
  if(N == 0){
    
    return(NA_real_)
    
  }
  
  S / sqrt(N)
  
}
#===============================================================================
# BirdTraitsR
# Diversity Analysis (Part 3)
#
# Advanced Diversity Indices
# (Wrappers around vegan)
#===============================================================================

#------------------------------------------------------------------------------
# Fisher's Alpha
#------------------------------------------------------------------------------

#' Fisher's Alpha Diversity
#'
#' Calculates Fisher's Alpha diversity index.
#'
#' Wrapper around vegan::fisher.alpha().
#'
#' @param x Community matrix.
#'
#' @return Numeric.
#'
#' @examples
#' mat <- matrix(
#' c(
#' 5,2,0,
#' 4,1,3
#' ),
#' nrow=2,
#' byrow=TRUE
#' )
#'
#' bt_fisher_alpha(mat)
#'
#' @export

bt_fisher_alpha <- function(x){
  
  if(!requireNamespace("vegan", quietly = TRUE)){
    stop(
      "Package 'vegan' is required.",
      call. = FALSE
    )
  }
  
  vegan::fisher.alpha(x)
  
}

#------------------------------------------------------------------------------
# Chao1 Richness Estimator
#------------------------------------------------------------------------------

#' Chao1 Richness
#'
#' Estimates species richness using Chao1.
#'
#' Wrapper around vegan::estimateR().
#'
#' @param x Community matrix.
#'
#' @return Numeric vector.
#'
#' @examples
#' mat <- matrix(
#' c(
#' 5,2,0,
#' 4,1,3
#' ),
#' nrow=2,
#' byrow=TRUE
#' )
#'
#' bt_chao1(mat)
#'
#' @export

bt_chao1 <- function(x){
  
  if(!requireNamespace("vegan", quietly = TRUE)){
    stop(
      "Package 'vegan' is required.",
      call. = FALSE
    )
  }
  
  est <- vegan::estimateR(x)
  
  est["S.chao1", ]
  
}

#------------------------------------------------------------------------------
# ACE Richness Estimator
#------------------------------------------------------------------------------

#' ACE Richness
#'
#' Estimates species richness using ACE.
#'
#' Wrapper around vegan::estimateR().
#'
#' @param x Community matrix.
#'
#' @return Numeric vector.
#'
#' @examples
#' mat <- matrix(
#' c(
#' 5,2,0,
#' 4,1,3
#' ),
#' nrow=2,
#' byrow=TRUE
#' )
#'
#' bt_ace(mat)
#'
#' @export

bt_ace <- function(x){
  
  if(!requireNamespace("vegan", quietly = TRUE)){
    stop(
      "Package 'vegan' is required.",
      call. = FALSE
    )
  }
  
  est <- vegan::estimateR(x)
  
  est["S.ACE", ]
  
}

#------------------------------------------------------------------------------
# Rarefaction
#------------------------------------------------------------------------------

#' Rarefaction
#'
#' Calculates expected species richness for a standardized sample size.
#'
#' Wrapper around vegan::rarefy().
#'
#' @param x Community matrix.
#' @param sample Sample size.
#'
#' @return Numeric vector.
#'
#' @examples
#' mat <- matrix(
#' c(
#' 5,2,0,
#' 4,1,3
#' ),
#' nrow=2,
#' byrow=TRUE
#' )
#'
#' bt_rarefaction(
#' mat,
#' sample = 5
#' )
#'
#' @export

bt_rarefaction <- function(
    x,
    sample
){
  
  if(!requireNamespace("vegan", quietly = TRUE)){
    stop(
      "Package 'vegan' is required.",
      call. = FALSE
    )
  }
  
  vegan::rarefy(
    x,
    sample = sample
  )
  
}

#------------------------------------------------------------------------------
# Species Accumulation
#------------------------------------------------------------------------------

#' Species Accumulation
#'
#' Computes species accumulation curves.
#'
#' Wrapper around vegan::specaccum().
#'
#' @param x Community matrix.
#' @param method Accumulation method.
#'
#' @return specaccum object.
#'
#' @examples
#' mat <- matrix(
#' c(
#' 5,2,0,
#' 4,1,3,
#' 1,0,7,
#' 6,1,2
#' ),
#' nrow=4,
#' byrow=TRUE
#' )
#'
#' bt_species_accumulation(mat)
#'
#' @export

bt_species_accumulation <- function(
    x,
    method = "random"
){
  
  if(!requireNamespace("vegan", quietly = TRUE)){
    stop(
      "Package 'vegan' is required.",
      call. = FALSE
    )
  }
  
  vegan::specaccum(
    x,
    method = method
  )
  
}
#===============================================================================
# BirdTraitsR
# Diversity Analysis (Part 4)
#
# Beta Diversity
# (Wrappers around vegan)
#===============================================================================

#------------------------------------------------------------------------------
# Beta Diversity Distance Matrix
#------------------------------------------------------------------------------

#' Beta Diversity
#'
#' Calculates pairwise beta diversity distances between sampling units.
#'
#' Wrapper around vegan::vegdist().
#'
#' @param x Community matrix (sites x species).
#' @param method Distance method.
#'
#' @return A dist object.
#'
#' @examples
#' mat <- matrix(
#' c(
#' 5,2,0,
#' 4,1,3,
#' 0,5,2,
#' 2,1,6
#' ),
#' nrow = 4,
#' byrow = TRUE
#' )
#'
#' bt_beta_diversity(mat)
#'
#' @export

bt_beta_diversity <- function(
    x,
    method = "bray"
){
  
  if(!requireNamespace("vegan", quietly = TRUE)){
    stop(
      "Package 'vegan' is required.",
      call. = FALSE
    )
  }
  
  vegan::vegdist(
    x,
    method = method
  )
  
}


#------------------------------------------------------------------------------
# Jaccard Distance
#------------------------------------------------------------------------------

#' Jaccard Distance
#'
#' Calculates pairwise Jaccard dissimilarity.
#'
#' Wrapper around vegan::vegdist().
#'
#' @param x Community matrix.
#' @param binary Treat data as presence/absence?
#'
#' @return A dist object.
#'
#' @examples
#' mat <- matrix(
#' c(
#' 5,2,0,
#' 4,1,3,
#' 0,5,2,
#' 2,1,6
#' ),
#' nrow = 4,
#' byrow = TRUE
#' )
#'
#' bt_jaccard(mat)
#'
#' @export

bt_jaccard <- function(
    x,
    binary = TRUE
){
  
  if(!requireNamespace("vegan", quietly = TRUE)){
    stop(
      "Package 'vegan' is required.",
      call. = FALSE
    )
  }
  
  vegan::vegdist(
    x,
    method = "jaccard",
    binary = binary
  )
  
}


#------------------------------------------------------------------------------
# Sorensen Distance
#------------------------------------------------------------------------------

#' Sorensen Distance
#'
#' Calculates Sorensen (Dice) dissimilarity.
#'
#' In vegan this is obtained using
#' Jaccard distance with binary = TRUE.
#'
#' @param x Community matrix.
#'
#' @return A dist object.
#'
#' @examples
#' mat <- matrix(
#' c(
#' 5,2,0,
#' 4,1,3,
#' 0,5,2,
#' 2,1,6
#' ),
#' nrow = 4,
#' byrow = TRUE
#' )
#'
#' bt_sorensen(mat)
#'
#' @export

bt_sorensen <- function(x){
  
  if(!requireNamespace("vegan", quietly = TRUE)){
    stop(
      "Package 'vegan' is required.",
      call. = FALSE
    )
  }
  
  vegan::vegdist(
    x,
    method = "bray",
    binary = TRUE
  )
  
}


#------------------------------------------------------------------------------
# Bray-Curtis Distance
#------------------------------------------------------------------------------

#' Bray-Curtis Distance
#'
#' Calculates Bray-Curtis dissimilarity.
#'
#' Wrapper around vegan::vegdist().
#'
#' @param x Community matrix.
#'
#' @return A dist object.
#'
#' @examples
#' mat <- matrix(
#' c(
#' 5,2,0,
#' 4,1,3,
#' 0,5,2,
#' 2,1,6
#' ),
#' nrow = 4,
#' byrow = TRUE
#' )
#'
#' bt_bray_curtis(mat)
#'
#' @export

bt_bray_curtis <- function(x){
  
  if(!requireNamespace("vegan", quietly = TRUE)){
    stop(
      "Package 'vegan' is required.",
      call. = FALSE
    )
  }
  
  vegan::vegdist(
    x,
    method = "bray"
  )
  
}


#------------------------------------------------------------------------------
# Pairwise Dissimilarity Matrix
#------------------------------------------------------------------------------

#' Pairwise Dissimilarity Matrix
#'
#' Converts a beta-diversity distance object into a matrix.
#'
#' @param x Community matrix.
#' @param method Distance method.
#'
#' @return Matrix.
#'
#' @examples
#' mat <- matrix(
#' c(
#' 5,2,0,
#' 4,1,3,
#' 0,5,2,
#' 2,1,6
#' ),
#' nrow = 4,
#' byrow = TRUE
#' )
#'
#' bt_dissimilarity_matrix(mat)
#'
#' @export

bt_dissimilarity_matrix <- function(
    x,
    method = "bray"
){
  
  as.matrix(
    
    bt_beta_diversity(
      x,
      method = method
    )
    
  )
  
}
#===============================================================================
# BirdTraitsR
# Diversity Analysis (Part 5)
#
# Rank Abundance
#===============================================================================

#------------------------------------------------------------------------------
# Rank-Abundance Plot
#------------------------------------------------------------------------------

#' Rank Abundance Plot
#'
#' Creates a Whittaker Rank-Abundance plot showing species ranked from
#' most abundant to least abundant.
#'
#' @param data Data frame.
#' @param species Species column.
#' @param abundance Optional abundance column.
#' If NULL, abundance is calculated from species frequencies.
#' @param color Line and point colour.
#' @param point_size Size of points.
#' @param line_size Width of connecting line.
#' @param log_scale Logical. Plot abundance on log10 scale?
#'
#' @return A ggplot object.
#'
#' @examples
#' birds <- data.frame(
#'   species = c(
#'     "A","A","A",
#'     "B","B",
#'     "C",
#'     "D","D","D","D"
#'   )
#' )
#'
#' p <- bt_rank_abundance(birds)
#' print(p)
#'
#' @export

bt_rank_abundance <- function(
    data,
    species = "species",
    abundance = NULL,
    color = "#2C7FB8",
    point_size = 3,
    line_size = 0.8,
    log_scale = FALSE
){
  
  bt_check_dataframe(data)
  
  if(!species %in% names(data)){
    stop(
      "Species column not found.",
      call. = FALSE
    )
  }
  
  #----------------------------------------------------------
  # Build abundance table
  #----------------------------------------------------------
  
  if(is.null(abundance)){
    
    counts <- table(data[[species]])
    
    rank_data <- data.frame(
      
      Species = names(counts),
      
      Abundance = as.numeric(counts),
      
      stringsAsFactors = FALSE
      
    )
    
  }else{
    
    if(!abundance %in% names(data)){
      stop(
        "Abundance column not found.",
        call. = FALSE
      )
    }
    
    rank_data <-
      
      stats::aggregate(
        
        data[[abundance]],
        
        by = list(data[[species]]),
        
        FUN = sum,
        
        na.rm = TRUE
        
      )
    
    names(rank_data) <-
      
      c(
        "Species",
        "Abundance"
      )
    
  }
  
  #----------------------------------------------------------
  # Rank species
  #----------------------------------------------------------
  
  rank_data <-
    
    rank_data[
      order(
        rank_data$Abundance,
        decreasing = TRUE
      ),
    ]
  
  rownames(rank_data) <- NULL
  
  rank_data$Rank <-
    
    seq_len(
      nrow(rank_data)
    )
  
  #----------------------------------------------------------
  # Plot
  #----------------------------------------------------------
  
  p <-
    
    ggplot2::ggplot(
      
      rank_data,
      
      ggplot2::aes(
        
        x = .data$Rank,
        
        y = .data$Abundance
        
      )
      
    ) +
    
    ggplot2::geom_line(
      
      linewidth = line_size,
      
      colour = color
      
    ) +
    
    ggplot2::geom_point(
      
      colour = color,
      
      size = point_size
      
    ) +
    
    ggplot2::labs(
      
      title = "Rank-Abundance (Whittaker) Plot",
      
      x = "Species Rank",
      
      y = "Abundance"
      
    ) +
    
    ggplot2::theme_bw(base_size = 13) +
    
    ggplot2::theme(
      
      plot.title =
        
        ggplot2::element_text(
          
          hjust = 0.5,
          
          face = "bold"
          
        )
      
    )
  
  if(log_scale){
    
    p <-
      
      p +
      
      ggplot2::scale_y_log10()
    
  }
  
  p
  
}


#------------------------------------------------------------------------------
# Rank-Abundance Data
#------------------------------------------------------------------------------

#' Rank-Abundance Table
#'
#' Returns the ranked abundance table used to create the Whittaker plot.
#'
#' @param data Data frame.
#' @param species Species column.
#' @param abundance Optional abundance column.
#'
#' @return Data frame.
#'
#' @export

bt_rank_abundance_table <- function(
    data,
    species = "species",
    abundance = NULL
){
  
  bt_check_dataframe(data)
  
  if(!species %in% names(data)){
    stop(
      "Species column not found.",
      call. = FALSE
    )
  }
  
  if(is.null(abundance)){
    
    counts <- table(data[[species]])
    
    out <- data.frame(
      
      Species = names(counts),
      
      Abundance = as.numeric(counts),
      
      stringsAsFactors = FALSE
      
    )
    
  }else{
    
    if(!abundance %in% names(data)){
      stop(
        "Abundance column not found.",
        call. = FALSE
      )
    }
    
    out <-
      
      stats::aggregate(
        
        data[[abundance]],
        
        by = list(data[[species]]),
        
        FUN = sum,
        
        na.rm = TRUE
        
      )
    
    names(out) <-
      
      c(
        "Species",
        "Abundance"
      )
    
  }
  
  out <-
    
    out[
      order(
        out$Abundance,
        decreasing = TRUE
      ),
    ]
  
  rownames(out) <- NULL
  
  out$Rank <-
    
    seq_len(
      nrow(out)
    )
  
  out
  
}
