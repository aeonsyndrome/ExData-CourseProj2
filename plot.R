## PLOT1
## Question: Have total emissions from PM2.5 decreased in the United States from 1999 to 2008?
## Make a plot showing the total PM2.5 emission from all sources for each of the years 1999, 2002, 2005, and 2008.
## Plotting system: base

# Load data
if(!exists("NEI")) NEI <- readRDS("summarySCC_PM25.rds") # slowwww
if(!exists("SCC")) SCC <- readRDS("Source_Classification_Code.rds")

# ETL
aggregated <- aggregate(Emissions ~ year,data=NEI,FUN=sum)

# Print to screen
barplot(height=aggregated$Emissions/10^6,names.arg=aggregated$year,
        xlab="year",ylab="Total PM2.5 Emissions (M Tons)",main="PM2.5 Emissions (M Tons) for Total US")

# Print to png
png("plot1.png", height=480, width=480)
barplot(height=aggregated$Emissions/10^6,names.arg=aggregated$year,
        xlab="year",ylab="Total PM2.5 Emissions (M Tons)",main="PM2.5 Emissions (M Tons) for Total US")
dev.off()

## PLOT2
## Question:Have total emissions from PM2.5 decreased in the Baltimore City, Maryland (fips == "24510") from 1999 to 2008?
## Plotting system: base

# Load data
if(!exists("NEI")) NEI <- readRDS("summarySCC_PM25.rds") # slowwww
if(!exists("SCC")) SCC <- readRDS("Source_Classification_Code.rds")

# ETL (filter & aggregate)
data <- NEI[NEI$fips == "24510",]
aggregated <- aggregate(Emissions ~ year,data=data,FUN=sum)

# Print to screen
barplot(height=aggregated$Emissions/10^6,names.arg=aggregated$year,
        xlab="year",ylab="Total PM2.5 Emissions (M Tons)",main="PM2.5 Emissions (M Tons) for Baltimore City")

# Print to png
png("plot2.png", height=480, width=480)
barplot(height=aggregated$Emissions/10^6,names.arg=aggregated$year,
        xlab="year",ylab="Total PM2.5 Emissions (M Tons)",main="PM2.5 Emissions (M Tons) for Baltimore City")
dev.off()

## PLOT3
## Question: Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) variable, which of these four sources have seen decreases in emissions from 1999-2008 for Baltimore City? Which have seen increases in emissions from 1999-2008?
## Plotting system: ggplot2

# Libraries
library(ggplot2)

# Load data
if(!exists("NEI")) NEI <- readRDS("summarySCC_PM25.rds") # slowwww
if(!exists("SCC")) SCC <- readRDS("Source_Classification_Code.rds")

# ETL
data <- NEI[NEI$fips == "24510",]
aggregated <- aggregate(Emissions ~ year + type,data=data,FUN=sum)

# Print to screen
g <- ggplot(aggregated, aes(year, Emissions, color = type)) + geom_line() + geom_point()
g <- g + labs(x="year",y="Total PM2.5 Emissions (M Tons)",title="PM2.5 Emissions (M Tons) for Baltimore by type")
g <- g + theme_bw() 
print(g)

# Print to png
png("plot3.png", height=480, width=480)
g <- ggplot(aggregated, aes(year, Emissions, color = type)) + geom_line() + geom_point()
g <- g + labs(x="year",y="Total PM2.5 Emissions (M Tons)",title="PM2.5 Emissions (M Tons) for Baltimore by type")
g <- g + theme_bw() 
print(g)
dev.off()

## PLOT4
## Question: Across the United States, how have emissions from coal combustion-related sources changed from 1999-2008?
## Plotting system: any

## PLOT5
## Question: How have emissions from motor vehicle sources changed from 1999-2008 in Baltimore City?
## Plotting system: any

## PLOT6
## Question: Compare emissions from motor vehicle sources in Baltimore City with emissions from motor vehicle sources in Los Angeles County, California (fips == "06037"). Which city has seen greater changes over time in motor vehicle emissions?
## Plotting system: any