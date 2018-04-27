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

# subsetting for Baltimore and merging SCC and NEI
df <- NEI[NEI$fips=="24510",]
NEI$SCC <- as.factor(NEI$SCC)
df <- merge(df, SCC, by.x = "SCC", by.y = "SCC", all.x = T, all.y = F)

# checking for rows containing the words "motor" and "vehicle"
motor_string <- "motor"
vehicle_string <- "vehicle"
#
# checking by what to group to obtain only coal combustion related emissions
z <- select(SCC, -SCC, -Map.To, -Last.Inventory.Year, -Created_Date, -Revised_Date, -Short.Name)
z <- z %>%
  mutate_all(as.character)
vec_motor_scc <- unlist((apply((z), 1, function(row) any(str_detect(tolower(row), motor_string)))==T))
vec_vehicle_string <- unlist((apply((z), 1, function(row) any(str_detect(tolower(row), vehicle_string)))==T))
y <- SCC[vec_vehicle_string,]
# => SCC.Level.Three
#
# keeping rows containing the words "motor" and "vehicle"
x <- select(df, -SCC, -fips, -Pollutant, -Emissions, -type, -year, -Map.To, -Last.Inventory.Year, -Created_Date, -Revised_Date, -Short.Name)
x <- x %>%
  mutate_all(as.character)
vec_motor <- unlist((apply((x), 1, function(row) any(str_detect(tolower(row), motor_string)))==T))
vec_vehicle <- unlist((apply((x), 1, function(row) any(str_detect(tolower(row), vehicle_string)))==T))

# subsetting
df <- df[vec_vehicle,]
exclude <- c("All Processes", "Border Crossings", "Coal Mining, Cleaning, and Material Handling (See 305310)",
             "Commercial Equipment", "Compression Ignition Equipment except Rail and Marine",
             "Construction and Mining Equipment", "Filling Vehicle Gas Tanks - Stage II",
             "Industrial Equipment", "Industrial/Commercial/Institutional",
             "Iron Production (See 3-03-015 for Integrated Iron & Steel MACT)",
             "Lawn and Garden Equipment","Lime Manufacture", "Logging Equipment", "Motor Vehicle Fires", 
             "NOT USED - Previously all LDGT (1&2) under M5", "Printing Ink Manufacture", "Residential",
             "Road Construction", "Underground Mining Equipment")
df <- df[! df$SCC.Level.Three %in% exclude,]
length(unique(df$SCC.Level.Three))
length(unique(y$SCC.Level.Three))

# plotting
#
# plot5: How have emissions from motor vehicle sources changed from 1999-2008 in Baltimore City?
# mean group emissions by year (using mean to account for number of measurements)
df <- aggregate(list(Emissions=df$Emissions), by=list(year=df$year, SCC.Level.Three=df$SCC.Level.Three), FUN = "mean")
# create stacked barplot
png(filename = paste(main_path, "/plot5.png", sep =""),
    width = 480, height = 480)
g <- ggplot(data=df, aes(x=year, y=Emissions, fill=SCC.Level.Three))
g + geom_bar(stat="identity") +
  theme(axis.title.y=element_blank()) +
  scale_x_continuous(breaks = df$year) +
  labs(title="Motor Vehicle PM2.5 Emissions",
       subtitle="Baltimore 1999-2008 - Grouped by SCC.Level.Three - Avg per Measurement")
dev.off()

