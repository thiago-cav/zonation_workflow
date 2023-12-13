#' compare_perf_curves function
#'
#' This function compare minimum and mean performance curves between different Zonation 
#' variants.
#'
#' @param variants Indices of setups to compare.
#' @param rank_col Name of the column containing priority rank values.
#' @param mean_col Name of the column containing mean performacen values.
#' @param min_col Name of the column containing minimum performacen values.
#'
#' @details This function assumes a specific folder structure with setup 
#' subdirectories prefixed with "setup_". It also relies on the `ggplot2` package
#'
#' @importFrom ggplot2
#'
#' @examples
#' # Compare performance curves for setups 1 to 6 with default column names
#' compare_perf_curves(variants = 1:6)
#'
#' @export
#'
compare_perf_curves <- function(variants, rank_col = "rank", mean_col = "mean", min_col = "min") {
  setup_dirs <- list.files(pattern = "^setup_.*", full.names = TRUE)
  selected_setups <- setup_dirs[variants]
  summary_data <- list()
  
  for (i in seq_along(selected_setups)) {
    setup_dir <- selected_setups[i]
    output_dir <- file.path(setup_dir, "output")
    summary_path <- file.path(output_dir, "summary_curves.csv")
    
    if (file.exists(summary_path)) {
      data <- read.csv(summary_path, sep = " ")
      data$setup_id <- i
      summary_data[[i]] <- data
    } else {
      message(paste("Warning: Missing summary_curves.csv file in Setup", i, "- Skipping this setup."))
    }
  }
  
  combined_data <- do.call(rbind, summary_data)
  
  ggplot(combined_data, aes_string(x = rank_col, group = "setup_id", color = "factor(setup_id)")) +
    geom_line(aes(y = get(mean_col)), linewidth = 1, linetype = "solid") +
    geom_line(aes(y = get(min_col)), linewidth = 1, linetype = "dashed") +
    labs(x = "proportion of landscape removed",
         y = expression(atop("average proportion", "of species distributions remaining")),
         color = "Setup ID") +
    scale_color_discrete(labels = variants) +
    scale_x_continuous(breaks = seq(0, 1, 0.2))+
    scale_y_continuous(breaks = seq(0, 1, 0.2))+
    geom_vline(xintercept = 0.9, color = "red", linetype = "dashed") +
    geom_vline(xintercept = 0.7, color = "black", linetype = "dashed") +
    theme_minimal() +
    theme(
      legend.position = "right",
      axis.title = element_text(size = 16),
      axis.text = element_text(size = 12),
      axis.line = element_line(),
      legend.title = element_text(size = 12),
      legend.text = element_text(size = 12),
      legend.box.background = element_rect(color="black", size=1)
    )
}
