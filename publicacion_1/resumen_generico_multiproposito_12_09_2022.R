

# Código en Rstudio

#1.- Instalación de paquetes

#install.packages("tidyverse")
#install.packages("srvyr")

#2.- Declaración de ibrerias, que permiten:

library(haven)      # archivos de spss o stata
library(magrittr)   # código más legible
library(tidyverse)  # ciencia de datos
library(srvyr)      # muestras complejas
library(readxl)     # archivos de excel


#3.- Abrir la base de datos

dataset <- read_sav("nombre_base")     #archivos de spss
dataset <- read_stata("nombre_base")   #archivos de stata
dataset <- read_excel("nombre_base")   #archivos de excel
dataset <- read_csv ("nombre_base")    #archivos de texto
              
#4.- Diseño muestral

as_survey_design(
  .data,
  ids = NULL,
  probs = NULL,
  strata = NULL,
  variables = NULL,
  fpc = NULL,
  nest = FALSE,
  check_strata = !nest,
  weights = NULL,
  pps = FALSE,
  variance = c("HT", "YG"),
  ...
)


#5.- Ejemplo en Encuesta Multipropósito

#En este caso, se uso: 
  # data es la encuesta multiproposito, 
  #ids son los conglomerados, en este caso los upm, 
  #strata son los estratos de la base de datos
  #weights es el factor de expansión

#Enlace de interés:
#https://www.rdocumentation.org/packages/srvyr/versions/1.1.1/topics/as_survey_design
