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

#Se transforma a tipo Tibble para un mejor análisisdsi
data <- as_tibble(data)
view(data)
#*********************** exluimos los datos que nos interesan (mayores de 18 -  empleados)
##De este primer analisis se concluye que existe cero faltantes en edad, por lo cual el primero paso es seleccionar
## que la muestra sea mayor o igual a 18 años, para personas que se encuentran empleadas.
data<-(data %>%
       filter(age >= 18, dsi == 0))

#****************** separate the target variable ***
#select variables used in the analysis
#f(x)
wage.features.of.interest = c(ApprovedSchoolGrade='p6210s1','age')
x= data %>% select (wage.features.of.interest)

#y
y <- data %>% 
  select(y_salary_m_hu)  %>%
  rename(HourlyWage= y_salary_m_hu)

dat = cbind(x, y)

#******************** Analisis na *********************


#Analisis de na por variable
colSums(is.na(dat))
porcentajeMiss <- function(dat) {sum(is.na(dat)) / length(dat)*100}
apply(dat, 2, porcentajeMiss)

# encontramos 12748 missing values en la variables dependiente hourlywage
# lo cual respresenta 56.30742 % de sus datos

#eliminamos todas las filas con unvalor faltante en esta columna 
dat<- dat[complete.cases(dat[,3]),]
colSums(is.na(dat))

#******************* estadisticos variables *************
#resumen de variables
skim(dat) 

#****************** Modelo Rgresion lineal
modelo1 <- lm(HourlyWage ~ ., data = dat, x = TRUE)

# calculo coeficientes
stargazer(modelo1, type="text")
summary(modelo1)

