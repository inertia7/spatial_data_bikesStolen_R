# library(devtools)
# TO GET THE MOST RECENT UPDATES OF PACKAGES YOU SHOULD INSTALL 
# the 'devtools' PACKAGE AND INSTALL STRAIGHT FROM THE GITHUB REPOSITORY. 
# IF YOU WANT THE CRAN VERSION JUST USE 'install.package() WAY

# install.packages(devtools)
# devtools::install_github("dkahle/ggmap")
# devtools::install_github("hadley/ggplot2")
# install.packages(data.table)
# install.packages(mapdata)
# install.packages(plyr)
library(devtools)
library(ggplot2)
library(ggmap)
library(data.table)
library(mapdata)
library(plyr)

setwd("/home/myProjects/approp/working/dir/bikesStolen")
bikesStolen <- read.csv("rawFile.csv", stringsAsFactors = F)
# NOT INCLUDED IN THE PROJECT BUT USED TO HAVE AN 
# OVERVIEW OF THE DATA SET
View(bikesStolen)
bikesStolen <- data.table(bikesStolen)
head(bikesStolen)
str(bikesStolen)

# DATA WRANGLING

#### CONVERTING DATA TYPES
bikesStolen$fromDate <- as.Date(
  bikesStolen$fromDate,
  format = "%m/%d/%Y")

bikesStolen$toDate <- as.Date(
  bikesStolen$toDate, 
  format = "%m/%d/%Y")

bikesStolen$Brand <- as.factor(bikesStolen$Brand)
bikesStolen$Model <- as.factor(bikesStolen$Model)
bikesStolen$Speed <- as.factor(bikesStolen$Speed)

# BEFORE WE ASSIGN THE VARIABLES TO REDUCE WORK SPACE
colorLabels <- c("Black","Blue","White","Green","Red", "Grey",
  "Silver", "Yellow","Purple","Pink","Orange",
  "Brown","Light Green","Light Blue","Burgundy/Maroon","Turquoise",
  "Dark Green","Tan","Chrome","Gold","Diamond Blue",
  "Cream/Ivory","Beige", "Not Given")

bikesStolen$Color <- ordered(
  bikesStolen$Color,
  levels=c("BLK","BLU","WHI","GRN","RED","GRY",
           "SIL","YEL","PLE","PNK","ONG",
           "BRO","LGR","LBL","MAR","TRQ","DGR",
           "TAN","COM","GLD","DBL","CRM","BGE", ""),
  labels=colorLabels)

#### GEOCODING
coords <- geocode(bikesStolen$Location)
bikesStolen <- data.table(bikesStolen, coords)

### CHECK TO SEE IF THE FUNCTION WORKED PROPERLY
head(bikesStolen$Color)

### OUTPUTTING CSV FILE
write.csv(bikesStolen, file = "bikesStolen.csv")

# FINDING STATISTICAL SIGNIFICANCE OF MISSING DATA
bikesStolen$Color[bikesStolen$Color == "Not Given"] <- NA 
bikesStolen$Color

# HERE WE CALCULATE THE PERCENTAGE OF MISSING DATA FOR THE
# VARIABLE 'Color'
(sum(is.na(bikesStolen$Color))/length(bikesStolen$Color)) * 100

# Visual Representation of Missing Values for Color
colors <- data.table(table(bikesStolen$Color))
colors
colnames(colors) <- c("Color", "Counts")
# ADD THE NAMES BECAUSE THE DATA TABLE WILL 
# NAME THE COLUMNS V1 AND N 
# NEXT WE RUN THE 'ordered' FUNCTION AGAIN TO MAKE SURE 
# THEY ARE OUTPUTTED IN THE ORDER WE WOULD LIKE THEM TO BE!
colors

colors$Color <- ordered(
  colors$Color, 
  levels=colorLabels)

colors$Color

# CREATING THE BARPLOT FOR THE 'Color' VARIABLE
bC <- ggplot(colors, aes(Color, Counts, fill = Color)) + 
  geom_bar(stat='identity', colour = "turquoise2") + 
  labs(title = "Color of Bikes Stolen within Isla Vista (2011-2016)") + 
  theme(panel.background = element_rect(fill = "gray98"),
        axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "none") +
  scale_fill_manual(values = c("#000000", "#1f23af", "#ffffff", "#03c11d", 
                               "#dd1616", "#808080", "#C0C0C0", "#ffff00", 
                               "#800080","#FFC0CB", "#FF8C00", "#8B4513", 
                               "#76EE00", "#ADD8E6", "#800000", "turquoise4", 
                               "#006400", "#D2B48C","#a8a8a8", "#D4AF37", 
                               "#0EBFE9", "#FCFBE3", "#f5f5dc", "#2d423f")) 
bC

# EXPLORATORY ANALYSIS

## SPATIAL MAPS
# MELT DATA FRAME
density <- ddply(bikesStolen, .(Location), "nrow")
colnames(density) <- c("Location", "count")
DF <- merge(bikesStolen, density, by = "Location")
DF
duplicated(DF$Location)
DF[duplicated(DF$Location),]
DF
write.csv(DF, file = "testLongLat.csv")
# RENDERING THE MAP FOR ISLA VISTA
islaVistaMap <- qmap("Isla Vista", zoom = 15, color = "bw", legend = "topleft")
circle_scale_amt = .4

# BASIC SPATIAL MAP 
islaVistaMap + 
  geom_point(
    data=DF, mapping = aes(x = long, y = lat, colour = 'red'), 
    na.rm=TRUE
    ) +
  theme(legend.position="none") +
  scale_size_continuous(range=range(DF$count)) + 
  ggtitle("Frequency of Bikes Stolen in Isla Vista", subtitle = "Encompassing 2010-2016")

# BASIC MAP WITH COLOR CHANGED TO ONLY RED
  islaVistaMap + 
    geom_point(
      data=DF2, mapping = aes(x = long2, y = lat2, colour = 'red'), 
      na.rm=TRUE
    ) +
    theme(legend.position="none") + facet_wrap(~ fromDate)
  scale_size_continuous(range=range(DF$count)) + 
    ggtitle("Frequency of Bikes Stolen in Isla Vista", subtitle = "Encompassing 2010-2016")
  
  
# SPATIAL MAP USING THE SAME COLOR FOR geom_point
islaVistaMap + 
  geom_point(
    data=DF, mapping = aes(x = long, y = lat), 
    na.rm=TRUE, alpha = 1/40, colour = 'red',
    size = DF$count*circle_scale_amt) + 
  theme(legend.position="none") + 
  scale_size_continuous(range=range(DF$count)) + 
  labs(title = "Frequency of Bikes Stolen in Isla Vista", 
       subtitle = "Encompassing 2011-2016")

# CREATING DENSITY PLOTS
islaVistaMap +
  stat_density2d(
    aes(x = long, y = lat, fill = ..level..,  alpha = .15),
    size = 2, bins = 4, data = DF,
    geom = "polygon",
    na.rm = TRUE
  ) + 
  labs(title = "Frequency of Bikes Stolen in Isla Vista", 
       subtitle = "Encompassing 2010-2016") +
  theme(legend.position="none")

islaVistaMap +
  stat_density2d(
    aes(x = long, y = lat, fill = ..level..,  alpha = ..level..),
    size = 2, bins = 4, data = DF,
    geom = "polygon",
    na.rm = TRUE
  ) + 
  labs(title = "Frequency of Bikes Stolen in Isla Vista", x = 'Longitude', "Latitude") +
  theme(legend.position="none")



# HERE WE DIVIDE THE DATA FRAME BY YEARS AND DO HEAT MAPS ON THOSE

# 2011
DF2011 <- DF[fromDate <="2011-12-31" & fromDate >= "2010-07-10"]
islaVistaMap +
  stat_density2d(
    aes(x = long, y = lat, fill = ..level..,  alpha = ..level..),
    size = 2, bins = 4, data = DF2011,
    geom = "polygon",
    na.rm = TRUE
  ) + 
  labs(title = "Frequency of Bikes Stolen in Isla Vista", subtitle = "Encompassing the years 2010-2011") +
  theme(legend.position="none")

# 2012
DF2012 <- DF[fromDate <="2012-12-31" & fromDate >= "2012-01-01"]
View(DF2012)
islaVistaMap +
  stat_density2d(
    aes(x = long, y = lat, fill = ..level..,  alpha = ..level..),
    size = 2, bins = 4, data = DF2012,
    geom = "polygon",
    na.rm = TRUE
  )  + 
  labs(title = "Frequency of Bikes Stolen in Isla Vista", subtitle = "Encompassing the year 2012") +
  theme(legend.position="none")

# 2013
DF2013 <- DF[fromDate <="2013-12-31" & fromDate >= "2013-01-01"]
View(DF2013)
islaVistaMap +
  stat_density2d(
    aes(x = long, y = lat, fill = ..level..,  alpha = ..level..),
    size = 2, bins = 4, data = DF2013,
    geom = "polygon",
    na.rm = TRUE
  )  + 
  labs(title = "Frequency of Bikes Stolen in Isla Vista", subtitle = "Encompassing the year 2013") +
  theme(legend.position="none")

# 2014
DF2014 <- DF[fromDate <="2014-12-31" & fromDate >= "2014-01-01"]
View(DF2014)
islaVistaMap +
  stat_density2d(
    aes(x = long, y = lat, fill = ..level..,  alpha = ..level..),
    size = 2, bins = 4, data = DF2014,
    geom = "polygon",
    na.rm = TRUE
  )  + 
  labs(title = "Frequency of Bikes Stolen in Isla Vista", subtitle = "Encompassing the year 2014") +
  theme(legend.position="none")

# 2015
DF2015 <- DF[fromDate <="2015-12-31" & fromDate >= "2015-01-01"]
View(DF2015)
islaVistaMap +
  stat_density2d(
    aes(x = long, y = lat, fill = ..level..,  alpha = ..level..),
    size = 2, bins = 4, data = DF2015,
    geom = "polygon",
    na.rm = TRUE
  )  + 
  labs(title = "Frequency of Bikes Stolen in Isla Vista", subtitle = "Encompassing the year 2015") +
  theme(legend.position="none")

# 2016
DF2016 <- DF[fromDate <="2016-12-31" & fromDate >= "2016-01-01"]
View(DF2016)
islaVistaMap +
  stat_density2d(
    aes(x = long, y = lat, fill = ..level..,  alpha = ..level..),
    size = 2, bins = 4, data = DF2016,
    geom = "polygon",
    na.rm = TRUE
  )  + 
  labs(title = "Frequency of Bikes Stolen in Isla Vista", subtitle = "Encompassing the year 2016") +
  theme(legend.position="none")
