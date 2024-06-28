SimData <- read.csv(file = "data-raw/ExampleDataNASH.csv")

usethis::use_data(SimData, overwrite = TRUE)
