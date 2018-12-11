x <- read.csv("data-raw/riverNile.csv")

testData1 <- x[ , c("STATE", "LOCALITY", "SPID", "HHID",
                    "CH_ID_1", "CH_ID_2", "CH_ID_3", "CH_ID_4", "CH_ID_5")]
names(testData1) <- c("state", "locality", "psu", "hhid", "child1", "child2", "child3", "child4")
usethis::use_data(testData1, overwrite = TRUE)


testData2 <- x[ , c("STATE", "LOCALITY", "SPID", "HHID", "CH_DD_1")]
names(testData2) <- c("state", "locality", "psu", "hhid", "index")
usethis::use_data(testData2, overwrite = TRUE)
