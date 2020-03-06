library(ggplot2)
library(rgdal)
library(sp)
library(leaflet)

##paises ahora es mi DATA
paises <- read.csv(file = 'C:/Users/Gonza/Desktop/Noveno semestre/IA/datosFINAL.csv')
paises<-as.data.frame(paises)
paises$Country=sub("[)]","",paises$Country)
paises$Country=sub("[,]''[(]",", ",paises$Country)
paises$Country=sub("[[:space:]][(]",", ",paises$Country)


barrios_med=readOGR("C:/Users/Gonza/Desktop/Noveno semestre/IA/trabajo2clustering/shp_mapa_paises_mundo_2014/Mapa_paises_mundo.shp")
nombres_barrios=iconv(barrios_med@data$COUNTRY,"UTF-8","ISO_8859-1")

dataframeworld=as.data.frame(barrios_med)


##nombres_barrios
library(stringr)

barrios_med@data$NOMBRE  <- str_to_upper(nombres_barrios, locale = "es")
barrios_med@data$NOMBRE <- chartr('аимсз','AEIOU', barrios_med@data$NOMBRE)
library(dplyr)


DATOS <- arrange(paises, Country)
DATOS<-DATOS %>% 
  rename(
    COUNTRY = Country,
    
  )

#0-> Yellow , 1-> red, 2-> blue, 3-> black, 4->green


for(i in 1: nrow(DATOS)){
  if(DATOS$cluster[i]==0){DATOS$COLOR[i] <- "yellow"}
  if(DATOS$cluster[i]==1){DATOS$COLOR[i] <- "red"}
  if(DATOS$cluster[i]==2){DATOS$COLOR[i] <- "blue"}
  if(DATOS$cluster[i]==3){DATOS$COLOR[i] <- "black"}
  if(DATOS$cluster[i]==4){DATOS$COLOR[i] <- "green"}
  
}



#barrios_med@data$NOMBRE <- arrange(barrios_med@data, NOMBRE)


dim(merge(x = DATOS, y = barrios_med@data , by = "COUNTRY"))
barrios_med@data$COLOR<-rep("white",256)

for (i in 1:235) {
  for (j in 1:193) {
    if(is.na(barrios_med@data$COUNTRY[i])){
      print("hola")
    }
    else{
      
    if(barrios_med@data$COUNTRY[i]==DATOS$COUNTRY[j]){
      barrios_med@data$COLOR[i] <- DATOS$COLOR[j]
    }
    }
  }
  }
  




m=leaflet(barrios_med)
m=addTiles(m)
m

m=addPolygons(m,popup=nombres_barrios,color=barrios_med@data$COLOR)
m  




#colores=sample(x=c("orange","green","yellow"),size=length(nombres_barrios),replace=TRUE)

m=addPolygons(m,popup=nombres_barrios,color=barrios_med@data$COLOR)
m  


