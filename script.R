# library(devtools)
# TO GET THE MOST RECENT UPDATES OF PACKAGES YOU SHOULD INSTALL 
# the 'devtools' PACKAGE AND INSTALL STRAIGHT FROM THE GITHUB REPOSITORY. 
# IF YOU WANT THE CRAN VERSION JUST USE 'install.package() WAY

# install.packages(devtools)
# devtools::install_github("dkahle/ggmap")
# devtools::install_github("hadley/ggplot2")
# install.packages(data.table)
# install.packages(plotly)
# install.packages(mapdata)
# install.packages(plyr)
# devtools::install_github("ropensci/plotly")
library(devtools)
library(ggplot2)
library(ggmap)
library(data.table)
library(plyr)
library(dplyr)

setwd("/myProjects/approp/working/dir/bikesStolen")
bikesStolen <- read.csv("bikesStolen.csv", stringsAsFactors = F)
# NOT INCLUDED IN THE PROJECT BUT USED TO HAVE AN 
# OVERVIEW OF THE DATA SET
# View(bikesStolen)
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
# RUN THIS SECTION IF YOU WISH TO GET THE LATITUDE AND LONGITUDE 
# OF THE LOCATIONS 
# FAIR WARNING WILL TAKE SOMETIME AND IF YOU RUN THIS TOO MANY TIMES
# IN ONE SESSION YOU WILL HAVE A WAITING PERIOD TO MAKE API CALLS 
# coords <- geocode(bikesStolen$Location)
# bikesStolen <- data.table(bikesStolen, coords)

### OUTPUTTING CSV FILE
# write.csv(bikesStolen, file = "bikesStolen.csv")

# EXPLORATORY ANALYSIS

# DATE RELATED STATISTICS
# FROM TIME STUFF
fromTime_zeros <- mapply(function(x, y) paste0(rep(x, y), collapse = ""), 0, 4 - nchar(DF$fromTime))

fromTime_temp <- paste0(fromTime_zeros, DF$fromTime)
fromTime_temp <- format(strptime(fromTime_temp, format="%H%M"), format = "%H:%M")
fromTime_temp[fromTime_temp == '00:01'] <- '01:00'
fromTime_temp <- data.table(fromTime_temp)
fromTime_date <- format(strptime(temp$temp, format = "%H:%M"), format = "%H:%M")

# TO TIME STUFF
toTime_zeros <- mapply(function(x, y) paste0(rep(x, y), collapse = ""), 0, 4 - nchar(DF$toTime))

toTime_temp <- paste0(toTime_zeros, DF$toTime)
toTime_temp <- format(strptime(toTime_temp, format="%H%M"), format = "%H:%M")
toTime_temp[toTime_temp == '00:01'] <- '01:00'
toTime_temp <- data.table(toTime_temp)

testTimeFrom <- strptime(temp$temp, format="%H:%M")
testTimeTo <- strptime(temp_to$temp_to, format="%H:%M")
# View(test_df)

# CONCATENATING TO DATE VALUES
fromStuff <- paste0(DF$fromDate, ' ', fromTime_date)
fromStuffNew <- strptime(fromStuff, format='%Y-%m-%d %H:%M')

# TO STUFF
toStuff <- paste0(DF$toDate, ' ', toTime_temp$toTime_temp)
toStuffNew <- strptime(toStuff, format='%Y-%m-%d %H:%M')

combine_df <- data.table(fromStuffNew, toStuffNew)
new_combine_df <- combine_df[complete.cases(combine_df$toStuffNew), ]
new_combine_df_hrs <- (new_combine_df$toStuffNew - new_combine_df$fromStuffNew)/(3600*24)
new_combine_df_hrs <- data.table(new_combine_df_hrs)
summary(new_combine_df_hrs$new_combine_df_hrs)
new_combine_df_hrs_vec <- as.matrix(new_combine_df_hrs)
# LET'S VIEW THE DISTRIBUTION OF THE DIFFERENCE IN HOURS 

a <- ggplot(new_combine_df_hrs, aes(new_combine_df_hrs)) + 
  geom_histogram(bins = '773', fill = 'white', colour = 'turquoise4') +
  labs(x = 'Hours', y = 'Count') + 
  ggtitle('Distribution of Time Difference',
          subtitle = 'Difference in "to" and "from" time') + 
  theme_minimal()

a
# STATS RELATED TO DATE TIMES
mean_time <- mean(new_combine_df_hrs$new_combine_df_hrs)
mean_time <- as.vector(mean_time)
sprintf('The average time interval for bicycle thefts is: %.4s days', (mean_time))

std_time <- sd(new_combine_df_hrs$new_combine_df_hrs)
std_time
sprintf('The standard deviation for time intervals of bicycle thefts is: %.5s', (std_time))

# FINDING STATISTICAL SIGNIFICANCE OF MISSING DATA
table(bikesStolen$Color)
# HERE WE CALCULATE THE PERCENTAGE OF MISSING DATA FOR THE
# VARIABLE 'Color'
(length(bikesStolen$Color[bikesStolen$Color == "Not Given"])/length(bikesStolen$Color)) * 100
# 13% 

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
  geom_bar(stat='identity', 
           colour = "turquoise2") + 
  labs(title = "Color of Bikes Stolen within Isla Vista (2011-2016)") + 
  theme(panel.background = element_rect(fill = "gray98"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none") +
  scale_fill_manual(values = c("#000000", "#1f23af", "#ffffff", "#03c11d", 
                               "#dd1616", "#808080", "#C0C0C0", "#ffff00", 
                               "#800080","#FFC0CB", "#FF8C00", "#8B4513", 
                               "#76EE00", "#ADD8E6", "#800000", "turquoise4", 
                               "#006400", "#D2B48C","#a8a8a8", "#D4AF37", 
                               "#0EBFE9", "#FCFBE3", "#f5f5dc", "#2d423f")) 
bC

# COLOR BY YEAR
byYearColor <- function(yr){
  temp <- bikesStolen[fromDate <= sprintf("%s-12-31", yr) & fromDate >= sprintf("%s-01-01", yr)]
  
  ggplot(temp, aes(Color, fill = Color)) + 
    geom_bar(stat='count', 
             colour = "turquoise2") + 
    labs(title = sprintf("Color of Bikes Stolen within Isla Vista (%s)", yr),
         x = 'Color',
         y = 'Count') + 
    theme(panel.background = element_rect(fill = "gray98"),
          axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "none") +
    scale_fill_manual(values = c("#000000", "#1f23af", "#ffffff", "#03c11d", 
                                 "#dd1616", "#808080", "#C0C0C0", "#ffff00", 
                                 "#800080","#FFC0CB", "#FF8C00", "#8B4513", 
                                 "#76EE00", "#ADD8E6", "#800000", "turquoise4", 
                                 "#006400", "#D2B48C","#a8a8a8", "#D4AF37", 
                                 "#0EBFE9", "#FCFBE3", "#f5f5dc", "#2d423f"),
                      limits = levels(temp$Color)) 
}

# TEST RUN THE FUNCTION TO SEE 2012
byYearColor(2012)

years <- c(2011, 2012, 2013, 2014, 2015, 2016)

# IF YOU WISH TO SAVE THE PLOTS RUN THIS FOR LOOP 
for (i in years){
  myPlots <- byYearColor(i)
  ggsave(myPlots, filename=paste("images/bikesStolenColorYear", i,".png",sep=""))
}

# TO DISPLAY ALL PLOTS WILL HAVE TO USE THE BACK BUTTON 
# ON RSTUDIO TO VIEW THEM ALL 
for (i in years){
  print(byYear(i))
}

## SPATIAL MAPS

# SIGNIFICANCE OF MISSING DATA WRT LOCATION
table(bikesStolen$Location)
bikesStolen$Location[bikesStolen$Location == "UNKNOWN LOCATION IN ISLA VISTA"]/length(bikesStolen$Location)
bikesStolen$Location
# HERE WE CALCULATE THE PERCENTAGE OF MISSING DATA FOR THE
# VARIABLE 'Location'
paste0((length(bikesStolen$Location[bikesStolen$Location == "UNKNOWN LOCATION IN ISLA VISTA"])/length(bikesStolen$Location)) * 100, '%')

# MELT DATA FRAME
density <- ddply(bikesStolen, .(Location), "nrow")
colnames(density) <- c("Location", "count")
DF <- merge(bikesStolen, density, by = "Location")
head(DF)

# RENDERING THE MAP FOR ISLA VISTA
islaVistaMap <- qmap("Isla Vista", zoom = 15, maptype = "hybrid", source = "google")
# FOR USE IN SPATIAL PLOTS 
circle_scale_amt = .35

# BASIC SPATIAL MAP 
islaVistaMap + 
  geom_point(
    data=DF, mapping = aes(x = long, 
                           y = lat),
    colour = 'red', 
    na.rm=TRUE
  ) +
  theme(legend.position="none") +
  scale_size_continuous(range=range(DF$count)) + 
  ggtitle("Frequency of Bikes Stolen in Isla Vista", 
          subtitle = "Encompassing 2010-2016")

# SPATIAL MAP CHANGING POINT ACCORDING TO FREQUENCY OF STOLEN BICYCLES
islaVistaMap + 
  geom_point(
    data=DF, mapping = aes(x = long, y = lat), 
    na.rm=TRUE, alpha = 1/40, colour = 'red',
    size = DF$count*circle_scale_amt
  ) + 
  theme(legend.position="none") + 
  scale_size_continuous(range=range(DF$count)) + 
  labs(title = "Frequency of Bikes Stolen in Isla Vista", 
       subtitle = "Encompassing 2011-2016")

# CREATING DENSITY PLOTS
islaVistaMap +
  stat_density2d(
    aes(x = long, y = lat, fill = ..level..,  alpha = .15),
    size = 1, bins = 4, data = DF,
    geom = "polygon",
    na.rm = TRUE
  ) + 
  labs(title = "Frequency of Bikes Stolen in Isla Vista", 
       subtitle = "Encompassing 2010-2016") +
  theme(legend.position="none")

# FUNCTION TO GET SPATIAL PLOTS PER YEAR
byYear <- function(yr){
  temp <- DF[fromDate <= sprintf("%s-12-31", yr) & fromDate >= sprintf("%s-01-01", yr)]
  islaVistaMap +
    stat_density2d(
      aes(x = long, y = lat, fill = ..level..,  alpha = ..level..),
      size = 2, bins = 4, data = temp,
      geom = "polygon",
      na.rm = TRUE
    )  + 
    labs(title = "Frequency of Bikes Stolen in Isla Vista", 
         subtitle = sprintf("Encompassing the year %s", yr)) +
    theme(legend.position="none")
}
# CHECK OUT 2013 TO SEE IF IT WORKED
byYear(2012)

# IF YOU WISH TO SAVE THE PLOTS RUN THIS FOR LOOP 
for (i in years){
  myPlots <- byYear(i)
  ggsave(myPlots, filename=paste("images/bikesStolenYear", i,".png",sep=""))
}

# TO DISPLAY ALL PLOTS WILL HAVE TO USE THE BACK BUTTON 
# ON RSTUDIO TO VIEW THEM ALL 
for (i in years){
  print(byYear(i))
}

# Fin
