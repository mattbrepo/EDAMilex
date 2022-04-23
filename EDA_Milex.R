library("readxl")
library(matrixStats)

# list of countries of interest
countries <- c("Bulgaria", "Czechia", "Estonia", "Hungary", "Latvia", 
               "Lithuania", "Poland", "Romania", "Denmark", "France", 
               "Greece", "Italy", "Netherlands", "Norway", "Portugal", 
               "Spain", "UK")

# list of years of interest
colYears = paste(seq(2000, 2020))
colAll =  c("Country", colYears)

readData <- function(sheetName, skipNum, usePerc) {
  # read the sheet of the Milex file
  data <- read_excel("SIPRI-Milex-data-1949-2020_0.xlsx", 
                     sheet = sheetName,
                     skip = skipNum, col_names = TRUE)
  
  # get data only for the countries and years of interest
  dataCS <- data[data$Country %in% countries, (names(data) %in% colAll)]
  # convert numeric columns 
  dataCS[,  names(dataCS) != "Country"] <- sapply(dataCS[, names(dataCS) != "Country"], as.numeric)
  
  # add average columns
  dataCS['Avg2000-2015'] <- rowMeans(dataCS[, paste(seq(2000, 2015))])
  dataCS['Avg2000-2010'] <- rowMeans(dataCS[, paste(seq(2000, 2010))])
  dataCS['Avg2011-2015'] <- rowMeans(dataCS[, paste(seq(2011, 2015))])
  dataCS['Avg2016-2020'] <- rowMeans(dataCS[, paste(seq(2016, 2020))])
  dataCS['SD2000-2015'] = rowSds(as.matrix(dataCS[, names(dataCS) %in% paste(seq(2000, 2015))]))
  
  if (usePerc) {
    # it tracks a difference of at least 0.5%
    dataCS['GT2000-2015'] <- (dataCS['Avg2016-2020'] - dataCS['Avg2000-2015'] > 0.001)
    dataCS['GT2000-2010'] <- (dataCS['Avg2016-2020'] - dataCS['Avg2000-2010'] > 0.001)
    dataCS['GT2011-2015'] <- (dataCS['Avg2016-2020'] - dataCS['Avg2011-2015'] > 0.001)
  } else {
    # it tracks a difference of at least 500 M USD
    dataCS['GT2000-2015'] <- (dataCS['Avg2016-2020'] - dataCS['Avg2000-2015'] > 500)
    dataCS['GT2000-2010'] <- (dataCS['Avg2016-2020'] - dataCS['Avg2000-2010'] > 500)
    dataCS['GT2011-2015'] <- (dataCS['Avg2016-2020'] - dataCS['Avg2011-2015'] > 500)
  }
  
  # is 2016-2020 greater than one standard deviation?
  #dataCS['GT2000-2015'] <- (dataCS['Avg2016-2020'] - dataCS['Avg2000-2015'] > dataCS['SD2000-2015'])
  #dataCS['GT2000-2010'] <- (dataCS['Avg2016-2020'] - dataCS['Avg2000-2010'] > dataCS['SD2000-2015'])
  #dataCS['GT2011-2015'] <- (dataCS['Avg2016-2020'] - dataCS['Avg2011-2015'] > dataCS['SD2000-2015'])
  
  return (dataCS)
}

showNumCountries <- function(dataCS, sheetName) {
  # show number of countries where the 2016-2020 period was greater than other periods
  resCounts <- c(sum(dataCS$`GT2000-2015` == TRUE),
               sum(dataCS$`GT2000-2010` == TRUE),
               sum(dataCS$`GT2011-2015` == TRUE))
  print(resCounts)
  barplot(resCounts, main=paste(sheetName, ", # countries where 2016-2020 was greater than ..."),
        names.arg=c("2000-2015", "2000-2010", "2011-2015")) 
}

showAvg <- function(dataCS, sheetName) {
  # show average per year
  avgYs <- dataCS[, names(dataCS) %in% colYears]
  avgYs <- colMeans(avgYs)
  barplot(avgYs, main=paste(sheetName, ", average of the countries"),
        names.arg=colYears)
}

#
# Main
#

sheetName = "Share of Govt. spending"
ds <- readData(sheetName, 6, TRUE)
showNumCountries(ds, sheetName)
showAvg(ds, sheetName)

sheetName = "Share of GDP"
ds <- readData(sheetName, 5, TRUE)
showNumCountries(ds, sheetName)
showAvg(ds, sheetName)

sheetName = "Constant (2019) USD"
ds <- readData(sheetName, 5, FALSE)
showNumCountries(ds, sheetName)
showAvg(ds, sheetName)

