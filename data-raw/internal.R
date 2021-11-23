url <- "http://gaia.tru.ca/birdMOVES/rscripts/anquery.csv"
url_count <- "http://gaia.tru.ca/birdMOVES/rscripts/anInit.csv"
url_loggers <- "http://gaia.tru.ca/birdMOVES/rscripts/anloggers.csv"

usethis::use_data(url, url_count, url_loggers, internal = TRUE, overwrite = TRUE)
