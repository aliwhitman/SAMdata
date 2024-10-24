#' get historical commercial reconstruction data and summarize
#'
#' @param hist.comm
#' @param species
#'
#' @return
#' @export
#'
#' @examples
getcrecon<- function(hist.comm,species){
  #load historical reconstruction
  # need to set a directory to find the data that's easy to modify later but I want to work locally now
  setwd<-"C:/Users/daubleal/Desktop/Species Assessment Monitoring/Data"
  hist.comm<-readxl::read_excel("1889-1986 OR Commercial Landings_v1.0.xls", sheet = "final_odfw_landings")
  # ignoring a bunch of warnings for now, doesn't seem to be an issue
  # aggregate by year and gear type for selected species
  hist.comm_summary <- hist.comm %>%
    group_by(SPECIES_CODE,YEAR,GEAR_CODE) %>%
    filter(SPECIES_CODE %in% species) %>%
    summarise(TOTAL_MT = sum(ROUND_MTONS)) %>%
    as.data.frame() %>%
    return(hist.comm_summary)

}
