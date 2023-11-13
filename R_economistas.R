# Importar datos (Leer)------------------------------------------------------
#En R se pueden cargar datos de diferentes tipos de archivos como:
#.csv - datos separados por comas
#.dta- datos de Stata
#.sav - datos de SPSS
#.xlsx - datos excel

#Se pueden cargar datos en R de diversas maneras, ya sea usando
#los atajos o netamente con comandos.
#Es necesario tener en cuenta que la exportación de datos solo se puede
#ejecutar por medio de comandos:


#Antes de importar y posteriormente exportar datos es necesario tener 
#claro como enrutar un espacio de trabajo:

####Mi directorio de trabajo ####

#Para conocer el directorio de trabajo al cual esta enrutado 
#el proyecto de R, se debe ejecutar el comando getwd()

getwd()

#tambien se lo puede realizar desde files > More > Go to working directory


####Cambiando directorio de trabajo ####

#Si desea enrutar el proyecto a otro directorio en donde por 
#ejemplo se encuentre el archivo que desee importar:
# Se lo puede realizar desde:
#files>(busca la carpeta que desea)>more>set as working directorio

#Con comandos se debe ejecutar el comando: 
#setwd(especifiar la ruta de trabajo)
#recuerde que tiene que usar o bien \\ o / y debe ir todo en "


setwd("C:/Users/USUARIO/Documents/Publicaciones/CURSO_R")

#NOTA: Cuando cierre R el directorio nuevamente sera el anterior


#### Importando datos extension csv 

read.csv("Venta-publicidad.csv")
datos2 <- read.csv("Venta-publicidad.csv")
View(datos)


#### Importar datos .dta
install.packages("haven")
library(haven)

datos3 <- read_dta("Stata.dta")
datos3 <- read_dta("C:\\Users\\USUARIO\\Documents\\PROGRAMAS R Y PHYTON\\Stata.dta")

#### Importar datos .sav
datos4 <- read_spss("SPSS.sav") 


#### Importar datos de excel
install.packages("openxlsx")
install.packages("readxl")
library(openxlsx) 
library(readxl) 

####subir adicional: escoger hojas

data1 <- read_xlsx("datos.xlsx")
str(data1)

datos <- tibble(data1)
glimpse(datos)



library(dplyr)

#Verbos principales
#Filter()----------------------------------------
datos %>% filter(Provincia=="Azuay")
datos %>% filter(Provincia=="Pichincha" | Provincia=="Imbabura")

#Operador %in%
datos %>% filter(Provincia %in% c("Pichincha", "Imbabura", "Carchi"))

#Filtrar por excepción
datos %>% filter(!Provincia=="Azuay")
datos <- datos %>% filter(!`Sectores Economicos`=="Total")

#¿Es posible filtrar por más de una variable?
datos %>% filter(Provincia=="Sucumbíos"&`Sectores Economicos`=="Comercio") %>% View()

#¿Que provincias se encuentran en los datos?
datos %>% distinct(Provincia) %>% View()


#Trabajando con las cadenas
library(stringr)
datos %>% filter(str_detect(Provincia,"a"))
datos %>% filter(str_detect(Provincia,"^C"))
datos %>% filter(str_detect(Provincia,"a$"))


#select----------------------------------------
datos[,2]
datos %>% select(`Sectores Economicos`,Provincia,Total)
datos %>% select(-Total) %>% View()
datos %>% select(-Total, -Provincia) %>% View()                 
datos %>% select(-c(Total,Provincia)) %>% View()
datos %>% select(`Sectores Economicos`, Provincia, contains("Me")) %>% View()
datos %>% select(`Sectores Economicos`, Provincia, starts_with("M")) %>% View()
datos %>% select(Provincia,everything())
datos %>% select(Provincia:`Grande empresa`) %>% View()

#relocate----------------------------------------------
datos %>% relocate(Total)
datos %>% relocate(Total,.before=Provincia)

#arrange------------------------------------------------
datos %>% arrange(`Sectores Economicos`)

#mutate y transmute------------------------------------
glimpse(datos)
#Uso del mutate
datos %>% mutate(Sectores=as.factor(`Sectores Economicos`),
                 Provincia=as.factor(Provincia))
datos %>% transmute(Sectores=as.factor(`Sectores Economicos`),
                 Provincia=as.factor(Provincia))


#En R, y en la mayoría de los lenguajes de programación, los números no incluyen 
#comas como separadores de miles. Estas comas son interpretadas como caracteres,
#no como parte de la representación numérica.
as.numeric(datos$Microempresa)
#Cuando R intenta convertir una cadena que contiene caracteres no numéricos 
#(como letras o comas) a un tipo numérico, falla y normalmente devuelve NA 
#(que significa "Not Available" o "No Disponible")
library(readr)

#Forma larga
datos %>% mutate(Grande=str_replace(`Grande empresa`, pattern="-", replacement="0"),
                 Pequeña=str_replace(`Pequeña empresa`, pattern="-", replacement="0")) %>% View()
#Forma corta
dato2 <- datos %>% 
  mutate(across(c(Microempresa, `Mediana empresa "A"`, `Mediana empresa "B"`, 
                  `Pequeña empresa`, `Grande empresa`, Total),
                ~str_replace(., pattern="-", replacement="0"))) %>% 
  mutate(across(c(Microempresa, `Mediana empresa "A"`, `Mediana empresa "B"`, 
                  `Pequeña empresa`, `Grande empresa`, Total),
                ~ as.numeric(str_replace_all(., pattern=",", replacement="")) * 1000)) 


#Uso del transmute
datos %>% transmute(`Sectores Economicos`,
                    Provincia=toupper(Provincia),
                    Grande=str_replace(`Grande empresa`, pattern="-", replacement="0")) %>% View()

#summarise----------------------------------------------
# summarise()
dato2 %>% summarise(promedio_pequeñas=mean(`Pequeña empresa`),
                           desviacion=sd(`Pequeña empresa`)) %>% view() 

dato2 %>% filter(!`Sectores Economicos`=="Total") %>%
  summarise(promedio_pequeñas=mean(`Pequeña empresa`),
            desviacion=sd(`Pequeña empresa`)) %>% view("hu")

# funciones auxiliares
dato2 %>% summarise_at(vars(`Pequeña empresa`,`Grande empresa`),
                              list(PROMEDIO=~mean(.,na.rm = TRUE),
                                   DESVEST=~sd(.,na.rm=TRUE))) %>% view("summarise") #esta función auxiliar tiene la ventaja de permitir declarar en el argumento vars() 2 variables o más a ser resumidas
dato2 %>% summarise_if(is.numeric,
                              list(PROMEDIO=~mean(.,na.rm=TRUE),
                                   DESVEST=~sd(.,na.rm=TRUE))) %>% view("summarise_if") #esta función auxiliar tiene la ventaja de declarar el tipo de dato sobre el cual se aplicarán las medidas de resúmenes

dato2 %>% 
  reframe(Porcentaje = Microempresa / sum(Microempresa), .by=Provincia)

#agrupar: group_by()---------------------------------------------------
#Caso 1 sin agrupar: Obtener el porcentaje de cada sector económico por provincia
dato2 %>% 
  mutate(Porcentaje = Microempresa / sum(Microempresa) * 100)

#¿Se obtiene el porcentaje por provincia o por el total de la columna?
dato2 %>% 
  mutate(Porcentaje = Microempresa / sum(Microempresa) * 100) %>% 
  summarise(suma=sum(Porcentaje))

#Caso 2 agrupando por provincia
dato2 %>% 
  group_by(Provincia) %>% 
  mutate(Porcentaje = Microempresa / sum(Microempresa) * 100)

#¿Se obtiene el porcentaje por provincia o por el total de la columna?
dato2 %>% 
  group_by(Provincia) %>% 
  mutate(Porcentaje = Microempresa / sum(Microempresa) * 100) %>% 
  summarise(suma=sum(Porcentaje)) 

dato2 %>% 
  group_by(Provincia) %>% 
  mutate(Porcentaje = Microempresa / sum(Microempresa) * 100) %>% 
  summarise(mean=mean(Porcentaje)) %>% View()



#desagrupar: ungroup()
#Caso 1 sin ungroup
dato2 %>% 
  group_by(Provincia) %>% 
  mutate(Porcentaje = Microempresa / sum(Microempresa) * 100) %>% 
  count()
#Caso 2 con ungroup
dato2 %>% 
  group_by(Provincia) %>% 
  mutate(Porcentaje = Microempresa / sum(Microempresa) * 100) %>% 
  ungroup() %>% 
  count()

#reframe(): Una función que combina agrupar y resumir

dato2 %>% reframe(Porcentaje = Microempresa / sum(Microempresa)*100, .by = Provincia) 

#¿Se obtiene el 100% por provincia?
dato2 %>% reframe(Porcentaje = Microempresa / sum(Microempresa)*100, .by = Provincia) %>% 
  reframe(sum(Porcentaje),.by=Provincia)



library(tidyr)
datos3 <- dato2 %>%select(-Total) %>% 
  pivot_longer(
  cols= 3:7,
  names_to="Tipo de Empresa",
  values_to = "Ventas")%>% 
  relocate(Provincia) 

datos3 %>% 
  pivot_wider(
    names_from = `Sectores Economicos`, 
    values_from = Ventas,
    values_fill = list(Ventas = 0)  # Esto rellena con 0 donde no hay datos
  ) %>% View()





dir.create("bases")
files<-list.files(path = "bases",full.names = TRUE)

# importación masiva con lapply()
bases<-lapply(files,read_xls, sheet=1) 
electricidad <- bases[[1]]
pib <- bases[[2]] 

glimpse(pib)




paises_latinoamericanos <- c("Argentina", "Bolivia", "Brasil", "Chile", "Colombia", 
                             "Costa Rica", "Cuba", "Ecuador", "El Salvador", "Guatemala", 
                             "Honduras", "México", "Nicaragua", "Panamá", "Paraguay", 
                             "Perú", "Puerto Rico", "República Dominicana", "Uruguay", 
                             "Venezuela", "Haití")

names(pib) <- pib[3,]
pib <- pib %>% filter(`Country Name` %in% paises_latinoamericanos) %>% 
  select(-c(`Indicator Name`,`Indicator Code`))

pib %>% pivot_longer(cols = as.character(1960:2022),
                     names_to = "Año",
                     values_to = "pib_percap") %>% 
        mutate(pib_percap=as.numeric(pib_percap)) %>% View()
                    



# Definir la función
procesar_df <- function(df) {
  # Cambiar los nombres de las columnas
  names(df) <- df[3,]
  
  # Filtrar, seleccionar y pivotar
  df %>% 
    filter(`Country Name` %in% paises_latinoamericanos) %>%
    select(-c(`Indicator Name`, `Indicator Code`)) %>%
    pivot_longer(cols = as.character(1960:2022),
                 names_to = "Año",
                 values_to = "valor") %>%
    mutate(valor = as.numeric(valor)) 
}

bases2 <- lapply(bases, procesar_df)
electricidad <- bases2[[1]] 
pib <- bases2[[2]]


a <- pib %>% left_join(electricidad,by=c("Country Name",
                                    "Country Code",
                                    "Año")) %>% View()
  filter(complete.cases(.)) %>% 
  mutate(Año=as.numeric(Año))

a %>% group_by(`Country Name`) %>%
  summarize(minimo=min(Año),
            maximo=max(Año)) %>% View()

a <- pib %>% left_join(electricidad,by="Country Code") %>% View()

ecuador <- pib %>% filter(`Country Name`=="Ecuador",
                          !is.na(valor))
ecuador_elec <- electricidad %>% filter(`Country Name`=="Ecuador",
                                        !is.na(valor))

ecuador %>%
  semi_join(ecuador_elec,by=c("Country Code","Año")) %>% 
  View()

ecuador %>% mutate(lag=lag(valor),
                   acumulado=cumsum(valor),
                   row_number()) %>% View()

pib %>% group_by(`Country Name`) %>% 
  mutate(row_number()) %>% View()



# Cargar e instalar el paquete necesario

library(wbstats)

#Para conocer los diferentes indicadores
indicadores <- wbindicators() 

#Para ver la informacion en español
indicadores_es <- wbindicators(lang = "es") 
indicadores_es %>%
  filter(str_detect(indicator, "(?i)turismo")) %>% View()


#Para ver que paises contiene
paises <- wbcountries()

#Vector de indicadores 


turismo <- wb_data(indicator = "ST.INT.ARVL", country = "EC",
                   start_date = 1995, end_date = 2020)

ecuador %>% bind_rows(turismo) %>% View()


#fecha
ice <- read_excel("Índice de Confianza Empresarial (ICE).xlsx")

a <- ice %>% unite("fecha",AÑO,MES,sep="") %>% 
  mutate(fecha=ym(fecha)) %>% 
  arrange(fecha) %>% 
  mutate(
         GLOBAL=parse_number(GLOBAL,locale=locale(decimal_mark = ",")),
         variacion=GLOBAL -lag(GLOBAL))

a <- ice %>% unite("fecha",AÑO,MES,sep="-") %>% 
 
  arrange(fecha) %>% 
  mutate(
    GLOBAL=parse_number(GLOBAL,locale=locale(decimal_mark = ",")),
    variacion=GLOBAL -lag(GLOBAL))


library(lubridate)

# Fechas y horas en formato de cadena
fecha_hora1 <- "2017-06-14 15:00:00"
fecha_hora2 <- "2017-06-13 12:00:00"

# Convertir las cadenas a objetos de fecha y hora
fecha_hora1 <- ymd_hms(fecha_hora1)
fecha_hora2 <- ymd_hms(fecha_hora2)

# Calcular la diferencia en horas
diferencia_horas <- difftime(fecha_hora1, fecha_hora2, units = "mins")

# Mostrar la diferencia en horas
diferencia_horas
