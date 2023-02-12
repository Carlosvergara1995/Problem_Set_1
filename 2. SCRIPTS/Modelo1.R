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

#*********************** excluimos los datos que nos interesan (mayores de 18 -  empleados)
data<-(data %>%
       filter(age >= 18, dsi == 0))

#Analisamos la estructura de la base 
glimpse(data)

#******************** Analisis na *********************

#Analisis de na por variable
colSums(is.na(data))

#Calculamos el porcentaje de los datos diferentes de NA
sum(data$y_ingLab_m_ha > 0 & !is.na(data$y_ingLab_m_ha) )/length(data$y_ingLab_m_ha)
# se infiere que el 43% de la base presenta datos 

#eliminamos todas las filas con un valor faltante en la columna de nuestra valiable dependiente (y_ingLab_m_ha)
df <- data[!is.na(data$y_ingLab_m_ha), ] %>% as.data.frame()

#****************** separate the target variable ***
#select variables used in the analysis
#f(x)
df = data_frame( wage = df$y_ingLab_m_ha,ApprovedSchoolGrade= df$p6210s1, age =df$age, sizefirm=df$sizeFirm, oficio= df$oficio, totalHoursWorked=df$totalHoursWorked )

# Ahora vamos a revisar la distribución de nuestra variable a predecir
HistogramWage <- ggplot(df, aes(x = y_ingLab_m_ha)) +
  geom_histogram( fill = "#BFEFFF") +
  labs(x = "Salario por horas", y = "Pesos colombianos") +
  theme_bw()

##Realizamos un analisis exploratorio de valores atipicos para la varaible de interes que es y_ingLab_m_ha

BoxplotWage <- ggplot(df, aes(x = "Salarios", y = y_ingLab_m_ha)) +
  geom_boxplot(fill = "#CFDCEF", color = "#F5D8C0") +
  ggtitle("Diagrama de Cajas de salario por hora") +
  theme(plot.title = element_text(hjust = 0.5))

GrafDispercion <- plot(df$y_ingLab_m_ha,pch=19,col="#FFF6F5")

#normalisamos los datos aplicando log
lgwage <-log(df$y_salary_m_hu)
df<-cbind(df,lgwage)


BoxplotWage <- ggplot(df, aes(x = "Salarios", y = y_ingLab_m_ha)) +
  geom_boxplot(fill = "#CFDCEF", color = "#F5D8C0") +
  ggtitle("Diagrama de Cajas de salario por hora") +
  theme(plot.title = element_text(hjust = 0.5))

GrafDispercion <- plot(lgwage,pch=19,col="#FFF6F5")


# descriptive statistics


#divide the base into train and test
set.seed(123)

id_train <- sample(1:nrow(wage.features.of.interest), size = 0.7*nrow(df2), 
                   replace = FALSE)

train <- wage.features.of.interest[id_train, ]
test  <- wage.features.of.interest[-id_train, ]



x= data %>% select (wage.features.of.interest)

#y
y <- data %>% 
  select(y_ingLab_m_ha)  %>%
  rename(HourlyWage= y_ingLab_m_ha)

dat = cbind(x, y)



#******************* estadisticos variables *************
#resumen de variables
skim(dat) 

#****************** Modelo Rgresion lineal
modelo1 <- lm(HourlyWage ~ ., data = dat, x = TRUE)

# calculo coeficientes
stargazer(modelo1, type="text")
summary(modelo1)

