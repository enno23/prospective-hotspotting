library(readr)
if(!require("dplyr")) install.packages("dplyr"); library("dplyr")
if(!require("ggplot2")) install.packages("ggplot2"); library("ggplot2")
if(!require("ggmap")) install.packages("ggmap"); library("ggmap")
if(!require("lubridate")) install.packages("lubridate"); library("lubridate")
#if(!require("ProjectTemplate")) install.packages("ProjectTemplate"); library("ProjectTemplate")
if(!require("raster")) install.packages("raster"); library("raster")
if(!require("data.table")) install.packages("data.table"); library("data.table")
#if(!require("MASS")) install.packages("MASS"); library("MASS")
if(!require("Hmisc")) install.packages("Hmisc"); library("Hmisc")
#if(!require("ks")) install.packages("ks"); library("ks")
if(!require("ggtern")) install.packages("ggtern"); library("ggtern")
library(viridis)  # better colors for everyone
## gis libraries
if(!require("maptools")) install.packages("maptools"); library("maptools")
if(!require("sp")) install.packages("sp"); library("sp")
if(!require("rgdal")) install.packages("rgdal"); library("rgdal")
if(!require("spatstat")) install.packages("spatstat"); library("spatstat")
if(!require("rgeos")) install.packages("rgeos"); library("rgeos")

#please download SanFrancisco Data from Kaggle https://www.kaggle.com/c/sf-crime
train <- read_csv("C:/Users/Tammena/Uni/Master Stat/multi verfahren/train.csv")

#train <- read_csv("your_datapath")
mini <- min(year(train$Dates))
maxi <- max(year(train$Dates))

#konvert date to weeks etc.
train <-
      train %>%
      mutate(Year  = factor(year(Dates), levels=mini:maxi),
             Month = factor(month(Dates), levels=1:12),
             Day   = day(Dates),
             Week = week(Dates), 
             Hour  = factor(hour(Dates), levels=0:23)
      )

train <-
      train %>%
      mutate(
            dayDate = as.POSIXct(round(Dates,"days")),
            DayOfWeek = factor(DayOfWeek, levels=c("Monday",
                                                   "Tuesday",
                                                   "Wednesday",
                                                   "Thursday",
                                                   "Friday",
                                                   "Saturday",
                                                   "Sunday"))
      )

train <-
      train %>%
      filter(Category %in% c("BURGLARY"))

offense <-subset(train, Year == 2015)
lims <- coord_map(xlim=c(-122.51, -122.35), ylim=c(37.70, 37.82))
bm <- ggmap(get_map(location = "SanFrancisco",  zoom = 12))

#1. teile 
#gewichtungsmatrix
gew <- read_delim("C:/Users/Tammena/Uni/Master Stat/multi verfahren/gew.csv", 
                  ";", escape_double = FALSE, col_names = FALSE, 
                  locale = locale(decimal_mark = ",", grouping_mark = ""), 
                  trim_ws = TRUE)
w <- as.matrix(gew)

#berücksichtige nur 8 wochen aus dem datensatz
crimedat <- as.data.table(subset(offense,  Week<=8))
crimedat$Z <- 1
#konstante limits und konstantes raster, damit wir die raster addieren können
#setze umfang und raster
r <- raster(xmn = -122.5149 ,
            xmx  = -122.357 ,
            ymn    = 37.70809 ,
            ymx     = 37.8324 ,  nrows=75, ncols=75)

for(i in 1:8){
      crimedat1 <-as.data.table(subset(crimedat,  Week==i ))
      cr <- crimedat1[, c("X", "Y")]
      cr$Z <- i/8
      coordinates(cr) <- ~X+Y
      x <- rasterize(cr, r, 'Z', fun=sum, background=0)
      f<- focal(x, w)
      assign(paste0("f", i), f)
}

gesamt <- f1+f2+f3+f4+f5+f6+f8

df <- as.data.frame(coordinates(gesamt))
df$z <- as.data.frame(gesamt)

df$z[df$z < 0.5] <- NA

#umwandlung in polygons für ggplot
rtp <- rasterToPolygons(gesamt)
rtp@data$id <- 1:nrow(rtp@data)   # add id column for join

rtpFort <- fortify(rtp, data = rtp@data)
rtpFortMer <- merge(rtpFort, rtp@data, by.x = 'id', by.y = 'id')  # join data
rtpFortMer$layer[rtpFortMer$layer < 0.5] <- NA
rtpFortMer$Summe_Gewichte <- rtpFortMer$layer

bm1<- bm + geom_polygon(data = rtpFortMer, 
                        aes(x = long, y = lat, group = group, fill = Summe_Gewichte), 
                        alpha = 0.5, 
                        size = 0) +  
      scale_fill_gradientn(colours = topo.colors(255))

#vergleiche mit auftreten in woche 9
crimepts <- as.data.table(subset(offense,  Week==9))
bm2 <- geom_point(data = crimepts, aes(x = X,y = Y))

bm1+bm2+lims
