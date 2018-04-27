# remove all data in memory
rm(list=ls())
dev.off()

# libraries
library(dplyr)
library(ggplot2)
library(scales)
library(grid)
library(stringr)

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
unique(NEI$Pollutant)
# exploring SCC
dim(SCC)
names(SCC)
str(SCC)
sort(unique(SCC$SCC.Level.Four))

# merging SCC and NEI
tmp <- dim(NEI)
NEI$SCC <- as.factor(NEI$SCC)
df <- merge(NEI, SCC, by.x = "SCC", by.y = "SCC", all.x = T, all.y = F)

# checking for rows containing the words "coal" and "combustion"
coal_string <- "coal"
combustion_string <- "combustion"
#
# checking by what to group to obtain only coal combustion related emissions
z <- select(SCC, -SCC, -Map.To, -Last.Inventory.Year, -Created_Date, -Revised_Date)
z <- z %>%
  mutate_all(as.character)
vec_coal_scc <- unlist((apply((z), 1, function(row) any(str_detect(tolower(row), coal_string)))==T))
vec_combustion_scc <- unlist((apply((z), 1, function(row) any(str_detect(tolower(row), combustion_string)))==T))
y <- SCC[vec_coal_scc&vec_combustion_scc,]
# => EI.Sector
#
# keeping rows containing the words "coal" and "combustion"
x <- select(df, -SCC, -fips, -Pollutant, -Emissions, -type, -year, -Map.To, -Last.Inventory.Year, -Created_Date, -Revised_Date)
x <- x %>%
  mutate_all(as.character)
vec_coal <- unlist((apply((x), 1, function(row) any(str_detect(tolower(row), coal_string)))==T))
vec_combustion <- unlist((apply((x), 1, function(row) any(str_detect(tolower(row), combustion_string)))==T))

# subsetting
df <- df[vec_coal&vec_combustion,]

# plotting

length(unique(df$EI.Sector))
length(unique(y$EI.Sector))

# plot4: PM2.5 trend from coal combustion-related sources
# mean group emissions by year (using mean to account for number of measurements)
df <- aggregate(list(Emissions=df$Emissions), by=list(year=df$year, EI.Sector=df$EI.Sector), FUN = "mean")
# create stacked barplot
png(filename = paste(main_path, "/plot4.png", sep =""),
    width = 480, height = 480)
g <- ggplot(data=df, aes(x=year, y=Emissions, fill=EI.Sector))
g + geom_bar(stat="identity") +
  theme(axis.title.y=element_blank()) +
  scale_x_continuous(breaks = df$year) +
  labs(title="Coal Combustion-Related PM2.5 Emissions",
       subtitle="US 1999-2008 - Grouped by EI.Sector - Avg per Measurement")
dev.off()

