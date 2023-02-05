#Cargamos Nuestras libreria 
library(pacman)
p_load(rvest,tidyverse)
#Cargamos y creamos nuestra data
chunk_1 <- read_html('https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_1.html')
view(chunk_1)
chunk_2 <- read_html('https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_2.html')

chunk_3 <- read_html('https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_3.html')
chunk_4 <- read_html('https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_4.html')
chunk_5 <- read_html('https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_5.html')


