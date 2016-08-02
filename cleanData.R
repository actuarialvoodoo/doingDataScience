library(readxl)
library(stringr)
library(dplyr)
library(ggplot2)

dataDir <- "rollingsales/"
file <- "rollingsales_manhattan.xls"

data <- read_excel(paste0(dataDir, file), sheet = 1, skip = 4)

myColNames <- c("borough", "neighborhood", "bldgClassCat", "taxClass", "block",
                "lot", "easement", "bldgClass", "address", "aptNum", "zip", 
                "residentialUnits", "commericalUnits", "totalUnits", "landsqft",
                "grosssqft", "yearbuilt", "taxClassAtSale", "bldgClassAtSale", 
                "salePrice", "saleDate")
colnames(data) <- myColNames
colTypes <- sapply(data, typeof)
names(colTypes) <- NULL

trimCols <- function (x){
     if(typeof(x) == "character"){
          x <- str_trim(x)     
     }
     x
}

data <- lapply(data, trimCols) %>% data.frame(stringsAsFactors = FALSE) %>% tbl_df()

# there are 7593 units with salesPrice == 0, in addition there are many propeties with sale
#    prices that seem way too low for manhattan. It's not clear where to draw the line
data <- data %>% filter(salePrice > 0)

# Need to remove zip code == 0
data[data$zip == 0, "zip"] <- NA

# remove yearbuilt == 0
data[data$yearbuilt == 0, "yearbuilt"] <- NA

#how many buildings don't have total units?
#ggplot(data, aes(totalUnits)) + geom_histogram(binwidth = 1) + scale_x_continuous(limits = c(0:10))

#ggplot(data %>% filter(grosssqft > 0), aes(grosssqft)) + geom_histogram(binwidth = 1000) + 
#     scale_x_continuous(limits = c(100000, 200000))

# there are 8814 records with 0 residentital, 0 commerical, and 0 totalUnits

# clean up address names
test <- data$address %>% gsub(pattern = "\\s+", replacement = " ")

# I want to make " ST", " ST,", " ST.", " STREE", and " STRET" into " STREET" 
#370 + 137 + 42 + 1 + 3 = 553
test <- test %>% str_replace(pattern = " ST(.|REE|RET)?(?=(,|\\s|$))", " STREET")

# replace abbevations for EAST and WEST with full words
test <- test %>% str_replace("(?=\\s)W\\.?(?=\\s)", "WEST")
test <- test %>% str_replace("(?=\\s)E\\.?(?=\\s)", "EAST")

# replace AVE(.|,) with AVENUE
test <- test %>% str_replace(" AVE(\\.)?(?=(,|\\s))", " AVENUE")

# Make sure that numbered streets end with ordinal suffix (st|nd|rd|th)
test <-test %>% str_replace("([0-9]+)(?<=1)(?= STREET)", "\\1ST") 

# fields that might be useful based on completeness/cleanliness of data as well as the
# number of distinct values (too many and it's not going to be predictive)
# 1. neighborhood        Y
# 2. bldgClassCat        Y
# 3. taxClass            Y
# 4. block               N
# 5. lot                 N
# 6. easement            N
# 7. bldgClass           N
# 8. residentialUnits    N
# 9. commericalUnits     N
# 10. totalUNits         Y
# 11. landsqft           N
# 12. grosssqft          N
# 13. yearbuilt          Y    
# 14. taxClassAtSale     Y
# 15. bldgClassAtSale    N
# 16. salePrice          Y

# I just don't believe a sale price of less than 250k
data <- data %>% filter(salePrice > 250000)

ggplot(useData, aes(log(grosssqft), log(salePrice))) + geom_point() + geom_smooth(method = "lm") + facet_wrap(~neighborhood, ncol = 5)

