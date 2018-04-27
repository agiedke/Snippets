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

# plot2: have total emmission increased in Baltimore City 1999-2008?
# group emissions by year
df <- NEI[NEI$fips=="24510",]
df <- aggregate(list(Emissions=df$Emissions), by=list(year=df$year), FUN = sum)


# barplot
png(filename = paste(main_path, "/plot2.png", sep =""),
    width = 480, height = 480)
par(mfrow=c(1,2))
par("oma"=c(0,0,2,0))
with(df[df$year %in% c(1999, 2008),], 
     barplot(Emissions, names.arg=year, 
             ylab="Emissions", ylim=c(0,range(df$Emissions)[2])))
with(df, 
     plot(year,Emissions,ylab="",ylim=c(0,range(df$Emissions)[2])))
mtext("Total PM2.5 Emissions in Baltimore", outer = TRUE)
dev.off()
