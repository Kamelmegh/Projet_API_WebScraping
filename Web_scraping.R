# Faciliter le téléchargement, puis la manipulation, HTML et XML.
library('rvest')

#Liste de des URL pour récupérer les données et les mettre a jour avec id des villes 
list_city_to_update <- list(c('https://www.accuweather.com/fr/ru/priiskovyy/1909774/current-weather/1909774','2051302'),
  c('https://www.accuweather.com/fr/ru/dzhaga/611093/current-weather/611093','563692'),
  c('https://accuweather.com/fr/dz/reggane/1942/current-weather/1942','2483761'),
  c('https://www.accuweather.com/fr/dz/el-abiodh-sidi-cheikh/3548/current-weather/3548','2498775'),
  c('https://www.accuweather.com/fr/dz/ksar-chellala/3488/current-weather/3488','2491335'),
  c('https://accuweather.com/fr/br/viana/38236/current-weather/38236','3385122'),
  c('https://www.accuweather.com/fr/br/sobradinho/2305182/current-weather/2305182','3447473'),
  c('https://www.accuweather.com/fr/br/rio-da-areia/2730066/current-weather/2730066','3451220'),
  #c('https://www.accuweather.com/fr/br/brasilia/43348/hourly-weather-forecast/43348','3469034'),
  c('https://www.accuweather.com/fr/eg/faqus/127341/current-weather/127341','356989'),
  c('https://www.accuweather.com/fr/td/oum-hadjer/52541/current-weather/52541','2426370'),
  c('https://www.accuweather.com/fr/td/moussoro/53564/current-weather/53564','2427336'),
  c('https://www.accuweather.com/fr/td/massakory/53496/current-weather/53496','2428228'),
  c('https://www.accuweather.com/fr/td/koumra/54164/current-weather/54164','2429605'),
  c('https://www.accuweather.com/fr/td/dourbali/53319/current-weather/53319','2433055'),
  c('https://www.accuweather.com/fr/td/zouar/52671/current-weather/52671','2424015'),
  c('https://www.accuweather.com/fr/td/kyabe/54165/current-weather/54165 ','2429344'),
  c('https://www.accuweather.com/fr/mv/hithadhoo/4580/current-weather/4580','1282256'),
  c('https://www.accuweather.com/fr/mv/eydhafushi/2148/current-weather/2148','1337606'),
  c('https://www.accuweather.com/fr/mv/kudahuvadhoo/232554/current-weather/232554','1337607'),
  c('https://www.accuweather.com/fr/mv/thinadhoo/232833/current-weather/232833','1337610'),
  c('https://www.accuweather.com/fr/mv/dhidhdhoo/3925/current-weather/3925','1337612'),
  c('https://www.accuweather.com/fr/nz/terrace-end/250925/current-weather/250925','2181258'),
  c('https://www.accuweather.com/fr/nz/cambridge/250642/current-weather/250642','6240770'),
  c('https://www.accuweather.com/fr/nz/ashburton/247031/current-weather/247031','6217081'),
  c('https://www.accuweather.com/fr/nz/tinwald/247195/current-weather/247195','2181108'))

data_final <- as_tibble(final_data_city)
fun_update <- function(city_to_update){
  
  url <- city_to_update[[1]][1]
  id <- city_to_update[[2]][1]
  
  webpage <- read_html(url)
  visibilite_html <- html_nodes(webpage,'.current-conditions-card p:nth-child(8)')
  visibilite <- html_text(visibilite_html)
  visibilite <- as.numeric(substr(visibilite,17,18)) 
  
  sub(pattern = "\\s",replacement = "",x = visibilite)
  print(visibilite)
  
  #Mise a jour des informations dans les jeu de données
  data_final$visibility[data_final$id==id] <- visibilite
  
}

#Parcours de notre liste a mettre a jour on appelle la fonction fun_update()
for (city_to_update in list_city_to_update) {
  
  fun_update(city_to_update)
  
}
view(data_final)

