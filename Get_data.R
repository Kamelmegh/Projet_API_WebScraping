# Installation des packages nécessaire s'il sont pas installer 

# jsonlite utilisées pour convertir entre des données JSON et des objets R.
#install.packages(jsonlite)

# tibble un bon moyen de créer des blocs de données. Il encapsule les meilleures pratiques 
#install.packages(tibble)

# listviewer pour des vues interactives des listes R en particulier les listes imbriquées, 
# peuvent être très difficiles à visualiser ou à représenter.
#install.packages(listviewer)

#Importation des librerie jsonlite, tibble et listviewer
library(jsonlite)
library(tibble)
library(listviewer)

# La Fonction setwd() nous permet de modifier la localisation de répertoire de notre projet
setwd("C:/Users/Kamel/Desktop/Etudes Reims/S2/Humanité numirique/Project_API_WebScraping")

#*************************************
# La Fonction fun_data_city() prend en entré liste des id des ville et renvoie une tibble
fun_data_city<-function(city_id){
  url<-paste("http://api.openweathermap.org/data/2.5/group?id=",city_id,"&units=metric&APPID=c800fa5b7ad6c01803c180e6b8102329",sep="")
  data_city <- fromJSON(url)
  data_city <- data_city$list
  data_city <- as_tibble(data_city)

  return(data_city)
}

#*************************************
# La Fonction fun_all_data_city() prend en paramètre le tibble retourné par la fonction  fun_data_city() 
# Renvoie un tibble qui contient les éléments de la liste  list_field_1
fun_all_data_city <- function(data_city){
  list_field_1 <- list('coord','sys','main','wind','clouds')
  initial_data <- tibble()
  for (field in list_field_1){
    if (length(initial_data) == 0){
      initial_data <- data_city[field][[1]]
    } 
    else{
      initial_data <- cbind(initial_data,data_city[field][[1]])
    }
  }
  
  initial_data <- cbind(initial_data,data_city['id'])
  initial_data <- cbind(initial_data,data_city['name'])
  initial_data <- cbind(initial_data,data_city['visibility'])
  
  return(initial_data)
}

#*************************************
# La fonction fun_data_we_need() prend en paramètre le tibble retourné par la fonction  fun_all_data_city() 
# Renvoie un tibble finale qui contient les éléments de la liste  list_field_2
# On peut modifier la liste list_field_2 pour récupérer d'autre informations
# Exemple on peut ajouter  a notre liste on va les les avoir dans notre Jeu de données  
fun_data_we_need <- function(all_data_city){
  
  list_field_2 <- list('country','name','id','lon','lat','temp','feels_like','temp_min','temp_max','pressure','humidity','speed','all','visibility')
  
  final_data <- tibble()
  for (field in list_field_2){
    if (length(final_data) == 0){
      final_data <- all_data_city[field]
    } 
    else{
      final_data <- cbind(final_data,all_data_city[field])
    }
  }
  return(final_data)
}

# La fonction fun_call_func() prend en paramére liste de 20 id des ville et fait appelle au autre fonction 
fun_call_func <- function(list_city_id){
  # Appelle au fonction
  data_city <- fun_data_city(list_city_id)
  all_data_city <- fun_all_data_city(data_city)
  all_data_we_need <- fun_data_we_need(all_data_city)
  
  return(all_data_we_need)
}

# On va charger le fichier qui contient les ID des Villes et le convertire a une liste 
city_id <-read.csv("cities_id.txt")
nbr_city_id <- length(city_id[[1]])
liste_city <- unlist((city_id[[1]][1:nbr_city_id]))

# On va parcourir notre liste liste_city, Pour chaque 20 id on fait appelle a nos fonction 
# Le resultat obtenus pour chaque 20 Ville on les ajoute a notre Tibble Final  (final_data_city)
x<-1
y<-20
final_data_city <- tibble()
while (y <= nbr_city_id) {
  list_city_id <- paste(liste_city[x:y],collapse = ',')
  
  all_data_city_id <- fun_call_func(list_city_id)
  
  # A chaque ittération on ajout 20 et on test si on dépasse pas la longeur de notre liste des id 
  # et Récupérer les dernier id même si inferieur a 20
  x <- x + 20
  if ((y +20) < nbr_city_id){
    y <- y+20
  }
  else{ 
    y <- y + (nbr_city_id %% 20)
  }
  
  #Récupérer le jeu de données Final en concaténant a chaque itération 
  if (length(final_data_city)==0){
    final_data_city <- all_data_city_id
  }
  else{
    final_data_city <- rbind(final_data_city,all_data_city_id)
  }
}


#On afftiche le résultat final
view(final_data_city)
jsonedit(final_data_city)

