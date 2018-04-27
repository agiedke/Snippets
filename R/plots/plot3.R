# remove all data in memory
rm(list=ls())
dev.off()

# libraries
library(dplyr)
library(ggplot2)
library(scales)
library(grid)

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
str(SCC)

unique(NEI$Pollutant)

# plot3: PM2.5 trend by source
# group emissions by year and type (use mean this time to account for number of measurements)
df <- NEI[NEI$fips=="24510",]
df <- aggregate(list(Emissions=df$Emissions), by=list(year=df$year, type=df$type), FUN = "mean")
# all in one barplot
ggplot(data=df, aes(x=year, y=Emissions, fill=type)) +
  geom_bar(stat="identity", position=position_dodge()) +
  scale_y_continuous(name="Fluorescent intensity/arbitrary units", labels = scales::comma) +
  theme(axis.title.y=element_blank()) +
  ggtitle("PM2.5 Emissions Trend by Type in Baltimore")
# scatterplot with trendline by type
png(filename = paste(main_path, "/plot3.png", sep =""),
    width = 480, height = 480)
g <- ggplot(data=df, aes(x=year, y=Emissions))
g + geom_point() + facet_grid(. ~ type) + geom_smooth(method = "lm") + theme_bw() +
  scale_x_continuous(breaks = df$year) +
  theme(panel.spacing = unit(2, "lines"), plot.title = element_text(hjust = 0.5)) +
  ggtitle("PM2.5 Emissions Trend (Mean Per Measurement) by Type in Baltimore")
dev.off()
