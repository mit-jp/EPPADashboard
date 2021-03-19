# Bar Chart Hover ---------------------------------------------------------

barChartHoverUI <- function(id) {
  ns <- NS(id)
  uiOutput(ns('hoverInfo'), style = "pointer-events: none")
}

barChartHover <- function(input, output, session, hover, data, plot_type, subcategory) {
  output$hoverInfo <- renderUI({
    hover <- hover()
    df <- data()
    plot_type <- plot_type()
    subcat <- subcategory()

    val <- calculateHoverValue(hover, df, plot_type, subcat)
    if(is.null(val)) return(NULL)

    style <- paste0("left:", hover$coords_css$x, "px; top:", hover$coords_css$y, "px;")

    wellPanel(
      style = style,
      class = 'hoverPanel',
      p(HTML(val)))
  })
}

calculateHoverValue <- function(hover, df, plot_type, subcat) {

  # Make sure we're working with valid values
  if(is.null(hover) || is.null(df)) return(NULL)

  # Detect the year of the bar that is being hovered over
  hoverYear <- df[which.min(abs(df$year - hover$x)), 'year'][[1]]
  df <- dplyr::filter(df, year == hoverYear)

  # If there are negative values (most likely from a diff plot), flip their
  # signs so we can use the same logic as we do for positive bars.
  y <- hover$y
  if(y < 0) {
    df <- df[which(df$value < 0), ]
    df$value <- abs(df$value)
    y <- abs(y)
  } else {
    df <- df[which(df$value > 0), ]
  }

  if (plot_type == "line") {
    return(calculateLineHoverValue(hover, df, subcat))
  }

  if(y > sum(df$value)) return(NULL) # Above the bar

  # If there's a subcategory, we also need to find which category is being
  # hovered over
  if(subcat == 'none') {
    val <- round(df$value, digits = 1) * sign(hover$y)
    as.character(val)
  }
  else {
    # Find which segment of the stacked bar the hover is closest to
    stackedSum <- sum(df$value) - cumsum(df$value)
    index <- which(stackedSum - y < 0)[1]

    # Get the region name and value for display
    regionName <- df[[subcat]][index]
    val <- round(df$value[index], digits = 1) * sign(hover$y)
    paste0(regionName, ': ', val)
  }
}

calculateLineHoverValue <- function(hover, df, subcat) {
  point <- nearPoints(df, hover, threshold = 10, maxpoints = 1, addDist = TRUE)
  if (nrow(point) == 0) return(NULL)

  if (subcat == "none") {
    subcat <- "region"
  }

  # Get the region name and value for display
  regionName <- point[[subcat]]
  val <- round(point$value, digits = 1) * sign(hover$y)
  return(paste0(regionName, ': ', val))
}
