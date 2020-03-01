#Importation des librerie jsonlite et tibble
library(jsonlite)
library(tibble)

#*************************************
data_cities<-function(citie_id){
  url<-paste("http://api.openweathermap.org/data/2.5/group?id=",citie_id,"&units=metric&APPID=c800fa5b7ad6c01803c180e6b8102329",sep="")
  data1 <- fromJSON(url)
  data1 <- data1$list
  data1 <- as_tibble(data1)
  
  return(data1)
}

#***********************************
all_information <- function(data_citie){
  
  list_field_1 <- list('coord','sys','main','wind','clouds')
  
  new_data <- tibble()
  for (field in list_field_1){
    if (length(new_data) == 0){
      new_data <- data_citie[field][[1]]
    } 
    else{
      new_data <- cbind(new_data,data_citie[field][[1]])
    }
  }
  
  new_data <- cbind(new_data,data_citie['name'])
  new_data <- cbind(new_data,data_citie['id'])
  
  return(as_tibble(new_data))
}

#***********************************
data_we_need <- function(all_data_citie){
  
  list_field_2 <- list('country','id','name','lon','lat','temp','feels_like','temp_min','temp_max','pressure','humidity','speed','all')
  
  final_data <- tibble()
  for (field in list_field_2){
    if (length(final_data) == 0){
      final_data <- all_data_citie[field]
    } 
    else{
      final_data <- cbind(final_data,all_data_citie[field])
    }
  }
  return(final_data)
}



cities_id <- read.csv("cites_id.txt")
liste_cities <- unlist((cities_id[[1]][1:129]))

final_data_cities <- tibble()
for(city in liste_cities){
  data_citie <- data_cities(city)
  all_data_citie <- all_information(data_citie)
  all_data_we_need <- data_we_need(all_data_citie)
  
  if (length(final_data_cities)==0){
    final_data_cities <- all_data_we_need
  }
  else{
    final_data_cities <- rbind(final_data_cities,all_data_we_need)
  }
}
print(final_data_cities)
view(final_data_cities)

