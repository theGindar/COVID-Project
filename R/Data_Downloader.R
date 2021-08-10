#' Läd die csv Datei mit COVID-Daten herunter.
#'
#' @param path Pfad in dem die csv Datei gespeichtert werden soll.
#'
#' @return NULL
#' @export
data_downloader <- function(path){
  message("downloading COVID data...")
  url <- "https://www.arcgis.com/sharing/rest/content/items/f10774f1c63e40168479a1feb6c7ca74/data"
  destfile <- paste(path, "/data.csv", sep="")
  download.file(url, destfile)
}
