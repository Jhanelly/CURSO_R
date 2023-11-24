#CURSO DE IMPORTACIÓN Y MANIPULACIÓN DE DATOS CON R----------
#Expositora: Econ. Jhanelly Chalá----------------------------

#Parte 1: Importación de datos
#Trabajar en un espacio de trabajo predefinido facilita la 
#importación de datos, dado que no es necesario especificar 
#la ruta completa del archivo en cada importación. 
#Por tal motivo, se recomienda almacenar toda la información en una misma ruta.

#Conoce tu ruta de trabajo
getwd()

#Cambia tu ruta de trabajo
setwd("C:/Users/USUARIO/Documents/Publicaciones/CURSO_R")

#Nota: Recuerda que tienes que usar \\ o /, además toda la ruta 
#debe estar dentro de las comillas.

#Otra forma de realizar es desde el atajo Files.

####Importando datos de extensión .csv ####
dir() #Permite obtener una lista de los archivos en tu directorio
#La función read.csv() viene por defecto en R,
#es decir no es necesario instalar ningún otro paquete
IDH<- read.csv("HDR21-22_Composite_indices_complete_time_series.csv")

#Otro paquete que puedes utilizar para importar datos .csv
#es data.table, mejora la eficiencia para cargar datos.
#install.packages("data.table")
library(data.table)
IDH<- fread("HDR21-22_Composite_indices_complete_time_series.csv")

#Comparando el tiempo de ejecución
system.time(read.csv("HDR21-22_Composite_indices_complete_time_series.csv"))
system.time(fread("HDR21-22_Composite_indices_complete_time_series.csv"))

#### Importar datos provenientes de excel ####
library(readxl)
interes <- read_excel("Tasa_Activa.xlsx")
#Selecciona las hojas
sectores<- read_excel("Tasa_Activa.xlsx",sheet = 2)

### Importar más de un archivo a la vez
# 1.- Guarda el nombre de tus archivos en un objeto
files<-list.files(path = "bases",full.names = TRUE)
print(files)
# 2.- Aplica la función read_xls
bases<-lapply(files,read_excel, sheet=1) 
# 3.- Extrae tus datos
electricidad <- bases[[1]]
pib <- bases[[2]] 

###Importar datos provenientes de internet####
url <- "https://raw.githubusercontent.com/zkamvar/r-novice-gapminder/main/episodes/data/gapminder_wide.csv"
#Cargar los datos
dato <- read.csv(url)

### Web Scraping ####
####Datos de extensión html ####
#install.packages("rvest")
library(rvest)
wiki <-read_html("https://es.wikipedia.org/wiki/Provincias_del_Ecuador")
tablas <- html_nodes(wiki, "table")
provincias <- html_table(tablas[[1]], fill = TRUE)

####API Banco Mundial####
#install.packages("wbstats")
library(wbstats)
#Países disponibles
wb_countries()
#Para obtener los indicadores en español:
wb_indicators(lang = "es") 
#Datos relacionados con turismo
wb_search(pattern = c("Unemployment"))
#Descarga los datos que necesites
paises_latinos <- c("ABW","ARG","ATG","BHS","BLZ","BOL", "BRA","BRB","CHL","COL",
                    "CRI","CUB","CUW","CYM","DMA","DOM","ECU","GRD","GTM","GUY","HND","HTI","JAM","KNA","LCA","MAF","MEX","NIC","PAN","PER","PRI","PRY","SLV","SUR", "SXM","TCA","TTO","URY","VCT","VEN","VGB","VIR")
desempleo <- wb_data(
  indicator = "SL.UEM.TOTL.ZS", 
  country = paises_latinos,
  start_date = 1960, end_date = 2020)

###Yahoo Finance API ####
# Asegúrate de que el paquete quantmod esté instalado
install.packages("quantmod")
library(quantmod)
# Obtener datos de Microsoft (MSFT)
getSymbols("MSFT", src = "yahoo")


#Parte 2: Manipulación de datos####
#Diferencia entre data.frame y un tibble
class(interes)
class(IDH)
interes
IDH

#Explicación breve de un data frame
años <- c(2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022)
pib_percap <- c(10000, 10500, 11000, 11500, 12000, 12500, 13000, 13500, 14000, 14500, 15000, 15500, 16000, 16500, 17000, 17500, 18000, 18500, 19000, 19500, 20000, 20500, 21000)
inflacion <- c(2.0, 2.1, 2.2, 2.3, 2.4, 2.5, 2.6, 2.7, 2.8, 2.9, 3.0, 3.1, 3.2, 3.3, 3.4, 3.5, 3.6, 3.7, 3.8, 3.9, 4.0, 4.1, 4.2)
mi_data <- data.frame(años, pib_percap, inflacion)

#De aquí en adelante trabajaremos con tibbles
#install.packages("tibble")
library(tibble)
datos <- as_tibble(dato) #convertir a un tibble
glimpse(datos)

#Dentro de R, dplyr es un paquete esencial para la manipulación de datos,
#este además de tibble forma parte del mundo tidyverse.
#install.packages("dplyr")
library(dplyr)

#filter()
#Filtra por el continente que sea Africa
datos %>% filter(continent=="Africa")
#Filtra las observaciones de gdpPercap_1952 que sean mayores o iguales a 5000
datos %>% filter(gdpPercap_1952>=5000)
#Filtra observaciones en las que el país NO sea Algeria.
datos %>% filter(!country=="Algeria")
#Filtrar por operadores lógicos
datos %>% filter(country=="Ecuador" | country=="Chile")
datos %>% filter(continent=="Europe", gdpPercap_1952<5000)

#select()
datos %>% select(country, gdpPercap_1952, lifeExp_1952, pop_1952)
#excluir variables
datos %>% select(-continent, -gdpPercap_1957)
#Columnas que inicien con "lifeExp"
datos %>% select(country,starts_with("lifeExp"))
#Seleccionar columnas que contengan "Gdp":
datos %>% select(country, contains("Gdp"))
#Seleccionar columnas que finalicen con "1997":
datos %>% select(country, ends_with("1997"))

#arrange()
#Orden ascendente
datos %>% arrange(country)
#Orden descendente
datos %>% arrange(desc(gdpPercap_1952))
#Ordenar por varias columnas
datos %>% arrange(continent,desc(country))

#Pausemos la manipulación
#¿Es una data rectangular?
#install.packages("tidyr")
library(tidyr)
#Pivotear
datos_pivot <- datos %>% 
  pivot_longer(cols = -c(continent, country),
               names_to="indicador",
               values_to = "Valor")  
#Separar columnas
datos_pivot <- datos_pivot %>% 
  separate(indicador,c("indicador","Año"),
           sep="_")

#Volvamos a pilotear
datos_pivot <-  datos_pivot %>% 
  pivot_wider(names_from=c(indicador),
              values_from = Valor)

#Continuemos con la manipulación
#Modificar variables sin crear unas nuevas
datos_pivot <- datos_pivot %>% 
  mutate(continent=as.factor(continent),
        country=as.factor(country),
        year=as.Date( paste(year,"01-01",sep="-")))

#Creando varibales a partir de las que ya existen
datos_pivot <- datos_pivot %>%
  mutate(log_gdpPercap=log(gdpPercap),
         log_lifeExp=log(lifeExp),
         log_pop=log(pop),
         var_gdpPercap=gdpPercap-lag(gdpPercap),
         varre_gdpPercap=gdpPercap/lag(gdpPercap)-1)

#Creando variables a partir de reglar establecidas
datos_pivot <- datos_pivot %>%
  mutate(
    clasificacion=
      case_when(
        gdpPercap <= 1136 ~ "Ingreso bajo",
        gdpPercap > 1136 & gdpPercap <= 4466 ~ "Ingreso medio bajo",
        gdpPercap > 4466 & gdpPercap <= 13865 ~ "Ingreso medio alto",
        gdpPercap > 13865 ~ "Ingreso alto"
      ))

#Con el uso del ifelse()
datos_pivot <- datos_pivot %>% 
  mutate(variacion=ifelse(var_gdpPercap>0,"positiva","negativa"))

#summarise()
datos_pivot %>% summarise(
  promedio_lifeExp=mean(lifeExp, na.rm=TRUE),      valor_max_gdp=max(gdpPercap),
  valor_min_gdp=min(gdpPercap),
  mediana=median(gdpPercap)
)

#Combinación del summarise() y group_by()
#Agrupa por continente
datos_pivot %>% 
  group_by(continent) %>% 
  summarise(promedio=mean(lifeExp))
#Agrupar por continente y año
datos_pivot %>% 
  group_by(continent,year) %>% 
  summarise(promedio=mean(lifeExp))

#Unión de datos
#Es importante que nuestra key coincida en el formato en
#ambas tablas
glimpse(desempleo)
desempleo <- desempleo %>%     
  select(country,date,SL.UEM.TOTL.ZS) %>%  
  rename(tasa_desem=SL.UEM.TOTL.ZS) %>% 
  mutate(date=as.Date(paste(date,"01-01",sep="-")))
attributes(desempleo$tasa_desem) <- NULL #quitar la etiqueta

#inner_join()
#Many to many?
datos_pivot %>% 
  inner_join(desempleo,
             by=c("country"))%>%
  relocate(tasa_desem, .after= year)
#Dos llaves
datos_pivot %>% 
  inner_join(desempleo,        
              by=c("country","year"="date"))%>%
  relocate(tasa_desem, .after= year)


#left_join()
datos_pivot %>% 
  left_join(desempleo,         
            by=c("country","year"="date"))%>%
  relocate(tasa_desem, .after= year)


#full_join()
datos_pivot %>% 
  full_join(desempleo,         
            by=c("country","year"="date"))%>%
  relocate(tasa_desem, .after= year) 

#Filtrado usando joins()
#semi_join()
datos_pivot %>% 
  semi_join(desempleo,        
            by=c("country","year"="date"))

#anti_join()
datos_pivot %>% 
  anti_join(desempleo,        
            by=c("country","year"="date"))