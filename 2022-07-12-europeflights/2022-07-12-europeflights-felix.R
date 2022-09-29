library(data.table)
library(dplyr)
library(ggplot2)
library(ggmap)
library(gganimate)
# Get the Data

# Read in with tidytuesdayR package 
# Install from CRAN via: install.packages("tidytuesdayR")
# This loads the readme and all the datasets for the week of interest

# Either ISO-8601 date or year/week works!

tuesdata <- tidytuesdayR::tt_load('2022-07-12')

flights <- as.data.table(tuesdata$flights)

#test <- as.data.table(map_data("state", region = unique(flights[STATE_NAME])))

# get a map of the world (ggplot function)
world <- map_data("world")


# check existing countries - North Macedonia, Turkey, United Kingdom are assigned differently than in map_data
# (solution with %in%?)

setdiff(flights[,STATE_NAME],unique(world$region))


# recode North Macedonia so it fits the entry in "world"
flights[,STATE_NAME:=recode(STATE_NAME,'Republic of North Macedonia' = 'North Macedonia',"Türkiye" = "Turkey", "United Kingdom"= "UK")]


# we only want to plot regions (Europe) that appear in our flight data base
map_world <- as.data.table(map_data("world", region=unique(flights[,STATE_NAME])))


# map:

ggplot(map_world)+
  geom_polygon(aes(long, lat, group=group), fill="blue")

# use airportr to get ll of all airports according to their ICAO name

library(airportr)

# delete ICAOs that dont exist on aiportr database

flights <- flights[APT_ICAO!="LESJ"&APT_ICAO!="LERJ"&APT_ICAO!="LEIZ"&APT_ICAO!="LDZL"&APT_ICAO!="LERE"&APT_ICAO!="ENBL"&APT_ICAO!="EVJA",]

# now makinga  dataframe that contains eacht airport and the corresponding coordinates:
# use sapply to assign coordinates to each airport with te airport_location function from airportr,
# this returns a list with only long and lat values which I then transpose and convert to a data.table. 
# Then I rename the columns and add the airport
#airportlocations <- as.data.table(sapply(unique(flights[,APT_ICAO]),FUN=airport_location,input_type="ICAO"))%>%
#  t(.)%>%as.data.table(.)%>%setnames(.,c("V1","V2"),c("lat","long"))%>%.[,airport:=unique(flights[,APT_ICAO])]



#flights[,":="(aplatitude=airportlocations[airportlocations[1]],aplongitude=airportlocations[airportlocations[2]])]

# no need for an aoky function, with data.table you can just use the airport_loction function from the airportr package to add
# lat and long values to each airport
flights[,aplatitude:=airport_location(APT_ICAO,input_type="ICAO")[1],by=APT_ICAO]%>%
  .[,aplongitude:=airport_location(APT_ICAO,input_type="ICAO")[2],by=APT_ICAO]


# finally, we split the date given in FLT_DATE into days, months and years so we can subset the data yb month to show monthly data

flights[,yearlyflights:=sum(FLT_TOT_IFR_2,na.rm=T),by=.(YEAR,MONTH_NUM,APT_ICAO)]%>%
  .[,c("Y", "M", "D") := tstrsplit(FLT_DATE, "-")]
                                                               
ggplot(map_world)+
  geom_polygon(aes(long, lat, group=group), fill="white")+
  geom_point(data=flights[as.numeric(MONTH_NUM)==12&as.numeric(D)==31&YEAR==2016,],aes(aplongitude,aplatitude,size=yearlyflights),alpha=0.5,color="red")+
  geom_text(data=flights[as.numeric(MONTH_NUM)==12&as.numeric(D)==31&YEAR==2016,],aes(x=-27, y=80, label=YEAR))

p <- ggplot(map_world)+
  geom_polygon(aes(long, lat, group=group), fill="white")+
  geom_point(data=flights[as.numeric(MONTH_NUM)==12&as.numeric(D)==31,],aes(aplongitude,aplatitude,size=yearlyflights,group=APT_ICAO),alpha=0.5,color="red")+
  geom_text(data=flights[as.numeric(MONTH_NUM)==12&as.numeric(D)==31,],aes(x=-27, y=80, label=Y))+
  transition_time(YEAR)

#file_renderer(dir = "C:/Users/Felix/Documents/R/TidyTuesday", prefix = "gganim_plot", overwrite = FALSE)

anim_save("test.gif", p)

                                          