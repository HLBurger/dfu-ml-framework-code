library(ggplot2)
plots_available <- c("barplot", "boxplot", "densityplot")

setClass(
  "visualizer",
  slots = list(
    data = "data.frame",
    x = "character",
    y = "character",
    plot_type = "character"
  )
)

binary_format <- function(x, y,
                          title = "Placeholder Title",
                          labels = c("Male", "Female")) {
  ggplot(df, aes(x = x, fill = y)) +
    geom_bar(position = "stack") +
    geom_text(
      stat = "count", aes(label = "..count.."),
      position = position_stack(vjust = 0.5), size = 5,
      family = "serif", color = "white"
    ) +
    scale_fill_manual(
      values = c("royalblue1", "royalblue4"),
      labels = c("Male", "Female")
    ) +
    theme_minimal() +
    labs(fill = "") +
    theme(
      plot.title = element_text(
        size = 20, hjust = 0.5,
        family = "serif", face = "bold"
      ),
      axis.text.x = element_text(size = 16, family = "serif"),
      axis.text.y = element_text(size = 14, family = "serif"),
      axis.title.y = element_text(size = 16, family = "serif"),
      legend.text = element_text(size = 16, family = "serif"),
      legend.position = "top"
    ) +
    scale_x_discrete(
      labels =
        c("No Amputation", "Amputation", "No wound healing")
    ) +
    ggtitle("Gender distribution across Amputation groups") +
    ylab("Total count of patients") +
    xlab("")
}

boxplot_format <- function(x, y, title = "Placeholder Title",
                           labels = c("With Amputation", "Total")) {
  ggplot(aes(x = x, y = y)) +
    geom_boxplot() +
    theme_minimal() +
    theme(
      legend.position = "none",
      plot.title = element_text(
        size = 22, hjust = 0.5,
        family = "serif", face = "bold"
      ),
      axis.text.x = element_text(size = 18, family = "serif"),
      axis.text.y = element_text(size = 16, family = "serif"),
      axis.title.y = element_text(size = 18, family = "serif")
    ) +
    ggtitle("Boxplot of age") +
    scale_x_discrete(
      labels =
        c("No amputation", "Amputation", "No wound healing")
    ) +
    xlab("") +
    ylab("Age in years")
}

barplot_format <- function(x, y, group) {
  ggplot() +
    geom_bar(aes(x = x, y = y, fill = group), stat = "identity") +
    geom_bar(aes(x = x, y = y, fill = group), stat = "identity", alpha = 1) +
    geom_text(aes(x = x, y = y, label = paste0(round(Percentage, 0), "%")),
      vjust = -0.5, face = "bold", size = 6,
      family = "serif", color = "black"
    ) +
    scale_fill_manual(
      values = c("royalblue4", "grey"),
      labels = c("With Amputation", "Total")
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(
        size = 22, hjust = 0.5,
        face = "bold", family = "serif"
      ),
      legend.text = element_text(size = 18, family = "serif"),
      legend.title = element_blank(),
      axis.text.x = element_text(size = 16, family = "serif"),
      axis.text.y = element_text(size = 16, family = "serif"),
      axis.title.y = element_text(size = 18, family = "serif"),
      axis.title.x = element_text(size = 18, family = "serif"),
      legend.position = "top"
    ) +
    xlab("Wound Severity") +
    ylab("Count of patients") +
    labs(fill = "") +
    ggtitle("Ratio of amputations per texas wound classification")
}

# function that runs when class is initiated
visualizer <- function(data, x, y = NULL, plot_type = "barplot") {
  if (!plot_type %in% plots_available) {
    stop(paste(
      "Invalid plot type. Choose one of the following:",
      paste(plots_available, collapse = ", ")
    ))
  }
  new("visualizer", data = data, x = x, y = y, plot_type = plot_type)
}

setGeneric("plotdata", function(object) standardGeneric("plotdata"))

setMethod("plotdata", "visualizer", function(object) {
  if (object$plot_type == "barplot") {
    barplot_format(object$x, object$y)
  }
})
