data_downloader <- function(){
  url <- "https://www.arcgis.com/sharing/rest/content/items/f10774f1c63e40168479a1feb6c7ca74/data"
  direction <- dirname(rstudioapi::getSourceEditorContext()$path)
  destfile <- paste(direction, "/data.csv", sep="")
  #destfile
  download.file(url, destfile)
}
data_downloader()
