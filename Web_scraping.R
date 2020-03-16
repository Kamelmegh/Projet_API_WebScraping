# Faciliter le téléchargement, puis la manipulation, HTML et XML.
library('rvest')
library(rgrs)

#Liste de des URL pour récupérer les données et les mettre a jour avec id des villes 
list_city_to_update <- list(
  c('https://www.accuweather.com/fr/za/mkuze/298949/current-weather/298949','975511'),
  c('https://www.accuweather.com/fr/za/rietfontein/298032/current-weather/298032','961935'),
  c('https://www.accuweather.com/fr/za/williston/299903/current-weather/299903','939702'),
  c('https://www.accuweather.com/fr/za/koffiefontein/297867/current-weather/297867','988429'),
  c('https://www.accuweather.com/fr/za/himeville/298992/current-weather/298992','996409'),
  c('https://www.accuweather.com/fr/za/germiston/305451/current-weather/305451','1002108'),
  c('https://www.accuweather.com/fr/za/ladysmith/298890/current-weather/298890','984998'),
  c('https://www.accuweather.com/fr/za/estcourt/298883/current-weather/298883','1004962'),
  c('https://www.accuweather.com/fr/za/hoedspruit/1147501/current-weather/1147501','996195'),
  #c('https://www.accuweather.com/fr/za/riversdale/301228/current-weather/301228','961152'),
  c('https://www.accuweather.com/fr/za/meyerton/298092/current-weather/298092','976764'),
  c('https://www.accuweather.com/fr/za/mtubatuba/298951/current-weather/298951','972803'),
  c('https://www.accuweather.com/fr/br/viana/38236/current-weather/38236','3385122'),
  c('https://www.accuweather.com/fr/eg/disuq/128705/current-weather/128705','358108'),
  c('https://www.accuweather.com/fr/eg/dishna/130205/current-weather/130205','358115'),
  c('https://www.accuweather.com/fr/eg/dikirnis/126818/current-weather/126818','358172'),
  c('https://www.accuweather.com/fr/eg/dayrut-ash-sharif/127396/current-weather/127396','358269'),
  c('https://www.accuweather.com/fr/eg/basyun/1497213/current-weather/1497213','358970'),
  c('https://www.accuweather.com/fr/eg/farshut/130206/current-weather/130206','356933')
  ) 

fun_update <- function(city_to_update){
  
  webpage <- read_html(url)
  visibilite_html <- html_nodes(webpage,'.current-conditions-card p:nth-child(8)')
  visibilite <- html_text(visibilite_html)
  visibilite <- as.numeric(substr(visibilite,17,18)) 
  
  # suppression des espaces  
  sub(pattern = "\\s",replacement = "",x = visibilite)
  
  #convertir en mètres par seconde
  visibilite <- visibilite * 1000
  return(visibilite)
  
}

data_final <- as_tibble(final_data_city)
DF_id_visibility <- data.frame()
DF_id_visibility_final <- data.frame()
#Parcours de notre liste a mettre a jour on appelle la fonction fun_update()
for (city_to_update in list_city_to_update) {
  
  url <- city_to_update[[1]][1]
  id <- city_to_update[[2]][1]
  
  visibilite <- fun_update(url)
  DF_id_visibility <- data.frame("id"=id,"visibility"=visibilite)
  
  if (length(DF_id_visibility_final)==0){
    DF_id_visibility_final <- DF_id_visibility
  }
  else{
    DF_id_visibility_final <- rbind(DF_id_visibility_final,DF_id_visibility)
  }
  
}

#Mise a jour des informations dans les jeu de données avec un merge 
data_final <- merge(data_final, DF_id_visibility_final, by = "id",all.x = TRUE)
data_final$visibility.x[is.na(data_final$visibility.x)] <- data_final$visibility.y[is.na(data_final$visibility.x)]
data_final$visibility.y <- NULL
data_final <- data_final[complete.cases(data_final),]
data_final <- renomme.variable(data_final, "visibility.x", "visibility")
view(data_final)
jsonedit(data_final)

write.csv(x = data_final, file = "monFichier.csv")