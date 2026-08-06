#===============================================================================
# BirdTraitsR
# Visualization Functions
#
# Publication-ready plots for bird ecology datasets.
#===============================================================================

#' @importFrom rlang .data
#' @importFrom utils head
NULL

#------------------------------------------------------------------------------
# Trait Bar Plot
#------------------------------------------------------------------------------

#' Plot Bird Trait Distribution
#'
#' Creates a publication-ready bar plot for any categorical trait.
#'
#' @param data A data frame.
#' @param trait Character string specifying the trait column.
#' @param fill Fill colour for bars.
#' @param show_percent Logical. Display percentages above bars?
#'
#' @return A ggplot object.
#'
#' @examples
#' birds <- data.frame(
#'   diet = c(
#'     "Insectivore",
#'     "Omnivore",
#'     "Insectivore",
#'     "Carnivore"
#'   )
#' )
#'
#' p <- bt_plot_trait(birds, "diet")
#' print(p)
#'
#' @export

bt_plot_trait <- function(
    data,
    trait,
    fill = "#2C7FB8",
    show_percent = TRUE
){
  
  bt_check_dataframe(data)
  
  if(!trait %in% names(data)){
    
    stop(
      paste0(
        "Column '",
        trait,
        "' not found."
      ),
      call. = FALSE
    )
    
  }
  
  summary <- bt_trait_summary(data, trait)$summary
  
  p <-
    
    ggplot2::ggplot(
      
      summary,
      
      ggplot2::aes(
        
        x = stats::reorder(
          .data$Trait,
          .data$Count
        ),
        
        y = .data$Count
        
      )
      
    ) +
    
    ggplot2::geom_col(
      
      fill = fill,
      
      width = 0.7
      
    ) +
    
    ggplot2::coord_flip() +
    
    ggplot2::labs(
      
      x = trait,
      
      y = "Number of Species",
      
      title = paste(
        
        "Distribution of",
        
        trait
        
      )
      
    ) +
    
    ggplot2::theme_bw(base_size = 13) +
    
    ggplot2::theme(
      
      plot.title =
        
        ggplot2::element_text(
          
          face = "bold",
          
          hjust = 0.5
          
        )
      
    )
  
  if(show_percent){
    
    p <-
      
      p +
      
      ggplot2::geom_text(
        
        ggplot2::aes(
          
          label = paste0(
            
            .data$Percentage,
            
            "%"
            
          )
          
        ),
        
        hjust = -0.2,
        
        size = 4
        
      )
    
  }
  
  p
  
}

#===============================================================================
# BirdTraitsR
# Trait Pie Chart
#===============================================================================

#------------------------------------------------------------------------------
# Trait Pie Chart
#------------------------------------------------------------------------------

#' Plot Trait Pie Chart
#'
#' Creates a publication-ready pie chart for a categorical trait.
#'
#' @param data A data frame.
#' @param trait Character column name.
#' @param show_percent Logical.
#'
#' @return A ggplot object.
#'
#' @examples
#' birds <- data.frame(
#'   diet = c(
#'     "Insectivore",
#'     "Omnivore",
#'     "Carnivore",
#'     "Insectivore"
#'   )
#' )
#'
#' p <- bt_plot_pie(birds,"diet")
#' print(p)
#'
#' @export

bt_plot_pie <- function(
    data,
    trait,
    show_percent = TRUE
){
  
  bt_check_dataframe(data)
  
  if(!trait %in% names(data)){
    
    stop(
      paste0(
        "Column '",
        trait,
        "' not found."
      ),
      call. = FALSE
    )
    
  }
  
  summary <- bt_trait_summary(data, trait)$summary
  
  summary$fraction <- summary$Count / sum(summary$Count)
  
  summary$ymax <- cumsum(summary$fraction)
  
  summary$ymin <- c(0, utils::head(summary$ymax, -1))
  
  p <-
    
    ggplot2::ggplot(
      
      summary,
      
      ggplot2::aes(
        
        ymax = .data$ymax,
        
        ymin = .data$ymin,
        
        xmax = 4,
        
        xmin = 3,
        
        fill = .data$Trait
        
      )
      
    ) +
    
    ggplot2::geom_rect(
      
      colour = "white"
      
    ) +
    
    ggplot2::coord_polar(theta = "y") +
    
    ggplot2::xlim(c(2,4)) +
    
    ggplot2::theme_void() +
    
    ggplot2::labs(
      
      title = paste(
        
        "Composition of",
        
        trait
        
      ),
      
      fill = trait
      
    ) +
    
    ggplot2::theme(
      
      plot.title =
        
        ggplot2::element_text(
          
          hjust = 0.5,
          
          face = "bold"
          
        )
      
    )
  
  if(show_percent){
    
    summary$label <-
      
      paste0(
        
        summary$Percentage,
        
        "%"
        
      )
    
    summary$position <-
      
      (summary$ymax + summary$ymin)/2
    
    p <-
      
      p +
      
      ggplot2::geom_text(
        
        data = summary,
        
        ggplot2::aes(
          
          x = 3.5,
          
          y = .data$position,
          
          label = .data$label
          
        ),
        
        inherit.aes = FALSE,
        
        size = 4
        
      )
    
  }
  
  p
  
}

#===============================================================================
# BirdTraitsR
# Histogram
#===============================================================================

#' Plot Histogram
#'
#' Creates a publication-ready histogram for a numeric variable.
#'
#' @param data A data frame.
#' @param variable Numeric column name.
#' @param bins Number of histogram bins.
#' @param fill Fill colour.
#' @param color Border colour.
#'
#' @return A ggplot object.
#'
#' @examples
#' birds <- data.frame(
#'   mass = c(
#'     20,22,18,25,30,
#'     27,24,23,21,26
#'   )
#' )
#'
#' p <- bt_plot_histogram(
#'   birds,
#'   "mass"
#' )
#'
#' print(p)
#'
#' @export

bt_plot_histogram <- function(
    data,
    variable,
    bins = 20,
    fill = "#2C7FB8",
    color = "black"
){
  
  bt_check_dataframe(data)
  
  if(!variable %in% names(data)){
    
    stop(
      paste0(
        "Column '",
        variable,
        "' not found."
      ),
      call. = FALSE
    )
    
  }
  
  if(!is.numeric(data[[variable]])){
    
    stop(
      "Selected column must be numeric.",
      call. = FALSE
    )
    
  }
  
  p <-
    
    ggplot2::ggplot(
      
      data,
      
      ggplot2::aes(
        x = .data[[variable]]
      )
      
    ) +
    
    ggplot2::geom_histogram(
      
      bins = bins,
      
      fill = fill,
      
      color = color,
      
      linewidth = 0.4
      
    ) +
    
    ggplot2::labs(
      
      title = paste(
        "Histogram of",
        variable
      ),
      
      x = variable,
      
      y = "Frequency"
      
    ) +
    
    ggplot2::theme_bw(base_size = 13) +
    
    ggplot2::theme(
      
      plot.title =
        
        ggplot2::element_text(
          
          face = "bold",
          
          hjust = 0.5
          
        )
      
    )
  
  p
  
}

#===============================================================================
# BirdTraitsR
# Boxplot
#===============================================================================

#' Plot Boxplot
#'
#' Creates a publication-ready boxplot for a numeric variable.
#'
#' If a grouping variable is supplied, separate boxplots are produced
#' for each group.
#'
#' @param data A data frame.
#' @param y Numeric column.
#' @param group Optional grouping variable.
#' @param fill Fill colour.
#' @param outlier_color Colour of outliers.
#'
#' @return A ggplot object.
#'
#' @examples
#' birds <- data.frame(
#'   mass = c(
#'     20,22,18,25,30,
#'     27,24,23,21,26
#'   ),
#'   diet = c(
#'     "A","A","A","B","B",
#'     "B","C","C","C","C"
#'   )
#' )
#'
#' p1 <- bt_plot_boxplot(
#'   birds,
#'   y = "mass"
#' )
#'
#' p2 <- bt_plot_boxplot(
#'   birds,
#'   y = "mass",
#'   group = "diet"
#' )
#'
#' print(p1)
#' print(p2)
#'
#' @export

bt_plot_boxplot <- function(
    data,
    y,
    group = NULL,
    fill = "#2C7FB8",
    outlier_color = "red"
){
  
  bt_check_dataframe(data)
  
  if(!y %in% names(data)){
    
    stop(
      paste0(
        "Column '",
        y,
        "' not found."
      ),
      call. = FALSE
    )
    
  }
  
  if(!is.numeric(data[[y]])){
    
    stop(
      "Selected response variable must be numeric.",
      call. = FALSE
    )
    
  }
  
  if(is.null(group)){
    
    p <-
      
      ggplot2::ggplot(
        
        data,
        
        ggplot2::aes(
          x = "",
          y = .data[[y]]
        )
        
      ) +
      
      ggplot2::geom_boxplot(
        
        fill = fill,
        
        outlier.colour = outlier_color,
        
        width = 0.4
        
      ) +
      
      ggplot2::labs(
        
        title = paste(
          "Boxplot of",
          y
        ),
        
        x = "",
        
        y = y
        
      )
    
  } else {
    
    if(!group %in% names(data)){
      
      stop(
        paste0(
          "Column '",
          group,
          "' not found."
        ),
        call. = FALSE
      )
      
    }
    
    p <-
      
      ggplot2::ggplot(
        
        data,
        
        ggplot2::aes(
          
          x = .data[[group]],
          
          y = .data[[y]]
          
        )
        
      ) +
      
      ggplot2::geom_boxplot(
        
        fill = fill,
        
        outlier.colour = outlier_color
        
      ) +
      
      ggplot2::labs(
        
        title = paste(
          "Boxplot of",
          y,
          "by",
          group
        ),
        
        x = group,
        
        y = y
        
      )
    
  }
  
  p +
    
    ggplot2::theme_bw(base_size = 13) +
    
    ggplot2::theme(
      
      plot.title =
        
        ggplot2::element_text(
          
          face = "bold",
          
          hjust = 0.5
          
        )
      
    )
  
}

#===============================================================================
# BirdTraitsR
# Scatter Plot
#===============================================================================

#' Plot Scatter Plot
#'
#' Creates a publication-ready scatter plot between two numeric variables.
#'
#' @param data A data frame.
#' @param x Numeric x-axis variable.
#' @param y Numeric y-axis variable.
#' @param color Point colour.
#' @param point_size Size of points.
#' @param alpha Point transparency.
#' @param regression Logical. Add regression line?
#'
#' @return A ggplot object.
#'
#' @examples
#' birds <- data.frame(
#'   wing = c(12,14,11,16,15,13),
#'   mass = c(20,25,18,30,27,22)
#' )
#'
#' p <- bt_plot_scatter(
#'   birds,
#'   x = "wing",
#'   y = "mass"
#' )
#'
#' print(p)
#'
#' @export

bt_plot_scatter <- function(
    data,
    x,
    y,
    color = "#2C7FB8",
    point_size = 3,
    alpha = 0.8,
    regression = TRUE
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
      "X variable must be numeric.",
      call. = FALSE
    )
  }
  
  if(!is.numeric(data[[y]])){
    stop(
      "Y variable must be numeric.",
      call. = FALSE
    )
  }
  
  p <-
    
    ggplot2::ggplot(
      
      data,
      
      ggplot2::aes(
        
        x = .data[[x]],
        
        y = .data[[y]]
        
      )
      
    ) +
    
    ggplot2::geom_point(
      
      colour = color,
      
      size = point_size,
      
      alpha = alpha
      
    ) +
    
    ggplot2::labs(
      
      title = paste(
        y,
        "vs",
        x
      ),
      
      x = x,
      
      y = y
      
    ) +
    
    ggplot2::theme_bw(base_size = 13) +
    
    ggplot2::theme(
      
      plot.title =
        
        ggplot2::element_text(
          
          face = "bold",
          
          hjust = 0.5
          
        )
      
    )
  
  if(regression){
    
    p <-
      
      p +
      
      ggplot2::geom_smooth(
        
        method = "lm",
        
        formula = y ~ x,
        
        se = TRUE,
        
        colour = "red",
        
        linewidth = 0.8
        
      )
    
  }
  
  p
  
}

#===============================================================================
# BirdTraitsR
# Correlation Heatmap
#===============================================================================

#' Correlation Heatmap
#'
#' Creates a publication-ready correlation heatmap for all numeric variables.
#'
#' @param data A data frame.
#' @param method Correlation method ("pearson", "spearman", "kendall").
#' @param low_colour Colour for negative correlations.
#' @param mid_colour Colour for zero correlation.
#' @param high_colour Colour for positive correlations.
#'
#' @return A ggplot object.
#'
#' @examples
#' birds <- data.frame(
#'   mass = c(20,25,18,30,27),
#'   wing = c(12,14,11,16,15),
#'   tail = c(7,8,6,9,8)
#' )
#'
#' p <- bt_plot_heatmap(birds)
#' print(p)
#'
#' @export

bt_plot_heatmap <- function(
    data,
    method = "pearson",
    low_colour = "#2166AC",
    mid_colour = "white",
    high_colour = "#B2182B"
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
      "At least two numeric variables are required.",
      call. = FALSE
    )
    
  }
  
  cor_matrix <-
    stats::cor(
      numeric_data,
      method = method,
      use = "complete.obs"
    )
  
  cor_df <-
    as.data.frame(
      as.table(cor_matrix)
    )
  
  names(cor_df) <-
    c(
      "Variable1",
      "Variable2",
      "Correlation"
    )
  
  ggplot2::ggplot(
    
    cor_df,
    
    ggplot2::aes(
      
      x = .data$Variable1,
      
      y = .data$Variable2,
      
      fill = .data$Correlation
      
    )
    
  ) +
    
    ggplot2::geom_tile(
      
      colour = "white"
      
    ) +
    
    ggplot2::geom_text(
      
      ggplot2::aes(
        
        label = round(
          .data$Correlation,
          2
        )
        
      ),
      
      size = 4
      
    ) +
    
    ggplot2::scale_fill_gradient2(
      
      low = low_colour,
      
      mid = mid_colour,
      
      high = high_colour,
      
      midpoint = 0,
      
      limits = c(-1,1),
      
      name = "Correlation"
      
    ) +
    
    ggplot2::labs(
      
      title = "Correlation Heatmap",
      
      x = NULL,
      
      y = NULL
      
    ) +
    
    ggplot2::theme_bw(base_size = 13) +
    
    ggplot2::theme(
      
      plot.title =
        
        ggplot2::element_text(
          
          face = "bold",
          
          hjust = 0.5
          
        ),
      
      axis.text.x =
        
        ggplot2::element_text(
          
          angle = 45,
          
          hjust = 1
          
        )
      
    )
  
}

#===============================================================================
# PCA Plot
#===============================================================================

#' PCA Plot
#'
#' Performs Principal Component Analysis (PCA) on numeric variables and
#' produces a publication-ready scatter plot of the first two principal
#' components.
#'
#' @param data A data frame.
#' @param columns Optional character vector of numeric columns.
#' If NULL, all numeric variables are used.
#' @param colour Colour of points.
#' @param point_size Point size.
#' @param labels Logical. Display row names as labels?
#'
#' @return A ggplot object.
#'
#' @examples
#' birds <- data.frame(
#'   mass = c(20,25,18,30,27),
#'   wing = c(12,14,11,16,15),
#'   tail = c(7,8,6,9,8)
#' )
#'
#' p <- bt_plot_pca(birds)
#' print(p)
#'
#' @export

bt_plot_pca <- function(
    data,
    columns = NULL,
    colour = "#2C7FB8",
    point_size = 3,
    labels = FALSE
){
  
  bt_check_dataframe(data)
  
  if(is.null(columns)){
    
    pca_data <-
      
      data[
        ,
        vapply(
          data,
          is.numeric,
          logical(1)
        ),
        drop = FALSE
      ]
    
  } else {
    
    missing <- columns[!columns %in% names(data)]
    
    if(length(missing) > 0){
      
      stop(
        paste0(
          "Column(s) not found: ",
          paste(missing, collapse = ", ")
        ),
        call. = FALSE
      )
      
    }
    
    pca_data <- data[, columns, drop = FALSE]
    
  }
  
  if(ncol(pca_data) < 2){
    
    stop(
      "At least two numeric variables are required.",
      call. = FALSE
    )
    
  }
  
  if(any(!vapply(pca_data, is.numeric, logical(1)))){
    
    stop(
      "All selected columns must be numeric.",
      call. = FALSE
    )
    
  }
  
  pca <-
    
    stats::prcomp(
      
      pca_data,
      
      center = TRUE,
      
      scale. = TRUE
      
    )
  
  scores <-
    
    as.data.frame(
      pca$x
    )
  
  variance <-
    
    summary(pca)$importance[2, ]
  
  p <-
    
    ggplot2::ggplot(
      
      scores,
      
      ggplot2::aes(
        
        x = .data$PC1,
        
        y = .data$PC2
        
      )
      
    ) +
    
    ggplot2::geom_point(
      
      colour = colour,
      
      size = point_size,
      
      alpha = 0.8
      
    ) +
    
    ggplot2::labs(
      
      title = "Principal Component Analysis",
      
      x = paste0(
        "PC1 (",
        round(variance[1] * 100,1),
        "%)"
      ),
      
      y = paste0(
        "PC2 (",
        round(variance[2] * 100,1),
        "%)"
      )
      
    ) +
    
    ggplot2::theme_bw(base_size = 13) +
    
    ggplot2::theme(
      
      plot.title =
        
        ggplot2::element_text(
          
          face = "bold",
          
          hjust = 0.5
          
        )
      
    )
  
  if(labels){
    
    if(is.null(rownames(scores))){
      
      scores$Label <- seq_len(nrow(scores))
      
    } else {
      
      scores$Label <- rownames(scores)
      
    }
    
    p <-
      
      p +
      
      ggplot2::geom_text(
        
        data = scores,
        
        ggplot2::aes(
          
          label = .data$Label
          
        ),
        
        vjust = -0.6,
        
        size = 3.5
        
      )
    
  }
  
  p
  
}

#===============================================================================
# BirdTraitsR
# Violin Plot
#===============================================================================

#' Violin Plot
#'
#' Creates a publication-ready violin plot for comparing a numeric
#' variable across categorical groups.
#'
#' @param data A data frame.
#' @param x Grouping (categorical) variable.
#' @param y Numeric variable.
#' @param fill Fill colour.
#' @param alpha Transparency.
#'
#' @return A ggplot object.
#'
#' @examples
#' birds <- data.frame(
#'   diet = c(
#'     "Carnivore",
#'     "Carnivore",
#'     "Omnivore",
#'     "Omnivore",
#'     "Insectivore",
#'     "Insectivore"
#'   ),
#'   mass = c(
#'     120,
#'     140,
#'     55,
#'     62,
#'     18,
#'     20
#'   )
#' )
#'
#' p <- bt_plot_violin(
#'   birds,
#'   x = "diet",
#'   y = "mass"
#' )
#'
#' print(p)
#'
#' @export

bt_plot_violin <- function(
    data,
    x,
    y,
    fill = "#4DAF4A",
    alpha = 0.8
){
  
  bt_check_dataframe(data)
  
  if(!x %in% names(data)){
    stop(
      paste0(
        "Column '",
        x,
        "' not found."
      ),
      call. = FALSE
    )
  }
  
  if(!y %in% names(data)){
    stop(
      paste0(
        "Column '",
        y,
        "' not found."
      ),
      call. = FALSE
    )
  }
  
  if(!is.numeric(data[[y]])){
    stop(
      "Y variable must be numeric.",
      call. = FALSE
    )
  }
  
  ggplot2::ggplot(
    data,
    ggplot2::aes(
      x = .data[[x]],
      y = .data[[y]]
    )
  ) +
    
    ggplot2::geom_violin(
      fill = fill,
      alpha = alpha,
      color = "black",
      trim = FALSE
    ) +
    
    ggplot2::geom_boxplot(
      width = 0.12,
      fill = "white",
      outlier.shape = NA
    ) +
    
    ggplot2::labs(
      x = x,
      y = y,
      title = paste(
        "Violin Plot of",
        y,
        "by",
        x
      )
    ) +
    
    ggplot2::theme_bw(base_size = 13) +
    
    ggplot2::theme(
      plot.title =
        ggplot2::element_text(
          hjust = 0.5,
          face = "bold"
        )
    )
}
