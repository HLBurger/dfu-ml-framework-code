plots_available <- c("barplot", "boxplot", "densityplot")

setClass(
  "visualizer",
  slots = list(data = "data.frame",
               x = "character",
               y = "character",
               plot_type = "character")
)

#Rough formatting of visualizations
visual_format <- function() {
  
}

#function that runs when class is initiated
visualizer <- function(data, x, y = NULL, plot_type = "barplot") {
  if (!plot_type %in% plots_available) {
    stop(paste("Invalid plot type. Choose one of the following:",
               paste(plots_available, collapse = ", ")))
  }
  new("visualizer", data = data, x = x, y = y, plot_type = plot_type)
}

setGeneric("plotdata", function(object) standardGeneric("plotdata"))

setMethod("plotdata", "visualizer", function(object) {

}

)