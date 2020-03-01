library("jsonlite")
library(tibble)

setwd("C:/Users/abderrahime/Documents/projet_web_scarping")

#retourner les données d'une ville a partir l'API
data_cities<-function(villes){
  url<-paste("http://api.openweathermap.org/data/2.5/group?id=",villes,"&units=metric&APPID=c800fa5b7ad6c01803c180e6b8102329",sep="")
  as_tibble(fromJSON(url))
}

cities_id <- read.csv("cites_id.txt")
l=unlist((cities_id[[1]][1:129])) 

#initialisation du tibble 
t <-tibble(lon=numeric(),
           lat=numeric(),
           country=character(),
           timezone=numeric(),
           sunrise=numeric(),
           sunset=numeric(),
           temp=numeric(),
           temp_ressenti=numeric(),
           temp_max=numeric(),
           pressure=numeric(),
           humidite=numeric(),
           wind_speed=numeric(),
           clouds=numeric(),
           name_city=character()    )

all_data <- function(l){
#parcourir toute la liste des 129 identifiants
for(i in l ){

url <-  paste("http://api.openweathermap.org/data/2.5/group?id=",i,"&units=metric&APPID=c800fa5b7ad6c01803c180e6b8102329",sep="")
data <- fromJSON(url)

get_lon<-data$list$coord$lon
get_lat<-data$list$coord$lat
get_country<-data$list$sys$country
get_timezone<-data$list$sys$timezone
get_sunrise<-data$list$sys$sunrise
get_sunset<-data$list$sys$sunset
get_temp<-data$list$main$temp
get_temp_feels_like<-data$list$main$feels_like
get_temp_max<-data$list$main$temp_max
get_pressure<-data$list$main$pressure
get_humidity<-data$list$main$humidity
get_wind_speed<-data$list$wind$speed
get_clouds<-data$list$clouds$all
get_name_city<-data$list$name


t <-add_row(t,lon=get_lon,
           lat=get_lat,
           country=get_country,
           timezone=get_timezone,
           sunrise=get_sunrise,
           sunset=get_sunset,
           temp=get_temp,
           temp_ressenti=get_temp_feels_like,
           temp_max=get_temp_max,
           pressure=get_pressure,
           humidite=get_humidity,
           wind_speed=get_wind_speed,
           clouds=get_clouds,
           name_city=get_name_city
           
)

}
return(t)
}
  
tib=all_data(l)  
view(tib)
