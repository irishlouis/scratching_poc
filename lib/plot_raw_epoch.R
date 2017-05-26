#' Title
#'
#' @param dt 
#' @param epoch_id 
#'
#' @return
#' @export
#'
#' @examples
plot_raw_epoch <- function(dt, epoch){
  # plot raw data for single epoch
  p1 <- dt[epoch_id == epoch] %>%
    ggplot(aes(datetime, vec.mag, group = device_id)) + 
    geom_line() +
    facet_wrap(~device_id, ncol = 1) + 
    labs(title = paste("Raw data for a epoch", epoch),
         y = "Accelerometer Vector Magnitude",
         x = "Time (s)") + 
    theme_bw() +
    theme(legend.position = "bottom",
          legend.key = element_rect(colour = "white"),
          panel.grid.minor = element_blank())
  return(p1)
}