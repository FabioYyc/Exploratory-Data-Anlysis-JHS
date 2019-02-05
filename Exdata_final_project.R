
## This first line will likely take a few seconds. Be patient!
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")
head(SCC)
str(NEI)
unique(NEI$year)
###Using the base plotting system, 
#make a plot showing the total PM2.5 emission from all sources for 
#each of the years 1999, 2002, 2005, and 2008.

names(NEI)
EM<-NEI[,c(4,6)]
year_sum<-sapply(split(EM,EM[,2]), sum)
years=c(1999,2002,2005,2008)
emission=year_sum
plot(years, emission, pch=16, main="Total pm2.5 emission from 1999 to 2008")

##Have total emissions from PM2.5 decreased in the  Baltimore City, 
##Maryland ( \color{red}{\verb|fips == 24510|}fips==24510) from 1999 to 2008?
Baltimore<-NEI[NEI$fips==24510,]
Baltimore<-Baltimore[,c(4,6)]
names(Baltimore)
year_sum<-sapply(split(Baltimore, Baltimore$year),sum)
years=c(1999,2002,2005,2008)
emission=year_sum
plot(years, emission, pch=16, main="Baltimore pm2.5 emission from 1999 to 2008")


###Of the four types of sources indicated by the
#(point, nonpoint, onroad, nonroad) variable, which of these four sources have seen decreases in 
#emissions from 1999-2008 for Baltimore City? Which have seen 
#increases in emissions from 1999-2008? Use the ggplot2 plotting system to make a plot answer this question.
install.packages("ggplot2")
library(ggplot2)
Baltimore<-NEI[NEI$fips==24510,]
ggplot(Baltimore, aes(x=year, y=Emissions, color=type)) +stat_summary(fun.y="sum", geom="line", size=1)

##Across the United States, how have emissions from coal combustion-related sources changed from 1999-2008?
head(SCC)
names(SCC)
##filter the data of coal related sources
library(ggplot2)
Coal_emission<-SCC[grepl("Coal", SCC$EI.Sector),]
Coal_emission<-merge(NEI, Coal_emission, by="SCC")
Coal_emission<-Coal_emission[,c(4,6)]
ggplot(Coal_emission, aes(x=year, y=Emissions)) +stat_summary(fun.y="sum", geom="line", size=1, color="red")+ggtitle("Coal emissions from 1999-2008")



##How have emissions from motor vehicle sources changed from 1999-2008 in Baltimore City?
unique(SCC$SCC.Level.Two)
#Find the rows with matched SCC
library(ggplot2)
Baltimore<-NEI[NEI$fips==24510,]
merged<-merge(Baltimore,SCC, by="SCC")
vehicles<-merged[grepl("Vehicle", merged$SCC.Level.Two),]
vehicles<-vehicles[,c(4,6)]
year_sum<-sapply(split(vehicles,vehicles$year), sum)
years=c(1999,2002,2005,2008)
emission=year_sum
ggplot(Baltimore, aes(x=year, y=Emissions)) +stat_summary(fun.y="sum", geom="line", size=1, color="red")+ggtitle("Emissions from vehicle sources in Baltimore from 1999-2008")


##Compare emissions from motor vehicle sources in Baltimore City 
##with emissions from motor vehicle sources in Los Angeles County,
##Californiafips == "06037"
##Which city has seen greater changes over time in motor vehicle emissions?
library(ggplot2)
BAL_LA<-NEI[NEI$fips==24510|NEI$fips=="06037",]
merged<-merge(BAL_LA,SCC, by="SCC")
vehicles<-merged[grepl("Vehicle", merged$SCC.Level.Two),]
vehicles<-vehicles[,c(2,4,6)]

p<-ggplot(vehicles, aes(x=year, y=Emissions, color=fips))
p+stat_summary(fun.y="sum", geom="line", size=1)+facet_grid(fips~.,scales='free')+labs(color = "LA,Baltimore")+ggtitle("Emissions from vehicle sources in LA and Batimore")
                                                                                                                           ")
  