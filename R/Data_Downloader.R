url <- "https://www.arcgis.com/sharing/rest/content/items/f10774f1c63e40168479a1feb6c7ca74/data"
destfile <- "C:\\Users\\Admin\\Documents\\COVID-Project\\R\\download.csv"
download.file(url, destfile)
direction <- dirname(rstudioapi::getSourceEditorContext()$path)
