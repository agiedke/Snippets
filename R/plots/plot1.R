# remove all data in memory
rm(list=ls())
dev.off()

# libraries
library(dplyr)

# datapaths
main_path <- "C:/Users/arne/DS_Programming_Courses/Coursera/ExploratoryDataAnalysis/week4"
inpath <- paste(main_path, "exdata_data_NEI_data", sep ="/")

# reading the data
NEI <- readRDS(paste(inpath, "summarySCC_PM25.rds", sep="/"))
SCC <- readRDS(paste(inpath, "Source_Classification_Code.rds", sep="/"))

# quick exploration
# exploring NEI
dim(NEI)
head(NEI)
str(NEI)
# exploring SCC
dim(SCC)
names(SCC)

# plot1: have total emmission in US decreased 1999-2008?
# group emissions by year
df <- aggregate(list(Emissions=NEI$Emissions), by=list(year=NEI$year), FUN = sum)
# scatterplot
with(df, plot(year,Emissions))
# barplot
png(filename = paste(main_path, "/plot1.png", sep =""),
    width = 480, height = 480)
with(df, barplot(Emissions, names.arg=year, yaxt="n", main="Total PM2.5 Emissions in the US"))
ticks<-c(0,1000000,2000000,3000000,4000000,5000000,6000000,7000000)
axis(2,at=ticks,labels=ticks)
dev.off()


