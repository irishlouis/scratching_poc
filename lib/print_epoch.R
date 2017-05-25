#' print.epoch
#'
#' @param dt - data.table of summary epoch data 
#'
#' @return p faceted plot of steps per epoch
#' @export
#'
#' @examples
print.epoch <- function(dt){
  p <- ggplot(dt[,title := paste(device_id, "-", subj_type ),], 
         aes(epoch_id, steps, group = title)) + 
    geom_line() +
    facet_wrap(~title, ncol = 1) + 
    labs(title = "Summary of step activity",
         subtitle = "Steps per 5 second epoch as returned by Actilife software",
         caption = "Data filtered to show walking period",
         y = "Steps / epoch",
         x = "Time (hh:mm)") + 
    theme_bw() +
    theme(legend.position = "none",
          panel.grid.minor = element_blank())
  return(p)
}


