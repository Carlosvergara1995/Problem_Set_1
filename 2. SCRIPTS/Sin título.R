#**********************  librerias****************************
library(pacman)
p_load("tidyverse","rvest","writexl","stargazer","ggplot2","reshape2", "dplyr","datasets", "skimr","gridExtra","data.table")

#********************** Scraping y base *********************
#Se realiza el scraping de los Chunks 1 a 10: 
url <- "https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_"
data <- data.frame()
for (i in 1:10) {
  url_i <- paste0(url, i, ".html")
  tablas <- url_i %>%
    read_html() %>%
    html_table() %>% .[[1]]
  data <- rbind.data.frame(data, tablas)
}

#Eliminamos la primera columna, la cual es de indices y no se requiere
data<-(data)[-1]

#Se transforma a tipo Tibble para un mejor análisis
data <- as_tibble(data)
view(data)
#*********************** exluimos los datos que nos interesan (mayores de 18 -  empleados)
##De este primer analisis se concluye que existe cero faltantes en edad, por lo cual el primero paso es seleccionar
## que la muestra sea mayor o igual a 18 años, para personas que se encuentran empleadas.
df<-(data %>%
       filter(age >= 18, dsi == 0))
#******************** Analisis de Datos **********************
sum(is.na(df))
# Nuestra base presenta 2084281 missing values
##Revisamos los datos faltantes para cada columna

max(colSums(is.na(df)))
colSums(is.na(df))

# analisamos las variables para el modelo_ salarios_individuales
#*para w
sum(is.na(df$y_salary_m_hu))
#presenta 12748 na

#para f(x)
sum(is.na(df$totalHoursWorked)) 
# presenta 6098 na
sum(is.na(df$p6210s1)) 
# no presenta na

#****************** Filtramos y renombramos las variables de nuestra data ***
# creamos nuestra base con las variables de interes 
base1 <- data_frame( df$age, df$totalHoursWorked,df$p6210s1)






