install.packages("rdwd")
install.packages('bit64')
library(rdwd)

weather_link <- selectDWD("Potsdam", var="air_temperature", per="recent", res="subdaily")
weather_file <- dataDWD(weather_link, read=FALSE, dir="R/weather_data", force=NA, overwrite=TRUE)
weather_data <- readDWD(weather_file, varnames=FALSE)
weather_data
