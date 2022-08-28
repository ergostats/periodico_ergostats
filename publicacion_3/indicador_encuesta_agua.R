       

# ------------------------------------------------------------------------- #
#                ANÁLISIS DE INDICADORES DE CALIDAD DE AGUA                 #
# ------------------------------------------------------------------------- #

       
# Cargando las librerias ------------------------------------------------
       
library(haven)
library(tidyverse)
library(pins)

# Carpeta compartida RAR --------------------------------------------------

path <- "C:/Users/Alex/OneDrive/Documentos/RAR"

carpeta <- board_folder(path)

# Lectura de la base de datos ---------------------------------------------

tabla_agua <- read_dta("../nuevos_hospitales/calidad_agua/bdd_ash2019.dta")


# Primer vistazo a la estructura de los datos ------------------------------

glimpse(tabla_agua)


# Revisando que calse de objeto es nuestra tabla de datos ------------------

class(tabla_agua)


# Mostrar los atributos de las variables ----------------------------------

diccionario <- map(.x = tabla_agua,
                   .f = attributes)


# Estructura de la lista de atributos que componen nuestro diccionario ----

str(diccionario)

# Etiquetas de las variables ----------------------------------------------

diccionario_variables <- map(.x = diccionario,
                            .f = function(x){
                              x [["label"]]
                            }) %>% 
  unlist %>% 
  enframe




# Categorías de cada variable ---------------------------------------------

diccionario_categorias <- map(.x = diccionario,
                              .f = function(x){
                                x [["labels"]]
                              }) %>% 
  map(enframe) %>% 
  imap(.x = ., .f = function(tabla, nombre){
    tabla %>% 
      mutate(variable = nombre)
  }) 


# Escribir archivos rds con un comentario úti -----------------------------

write_rds(x = diccionario_variables, file = "diccionario_variables_agua.rds" )

write_rds(x = diccionario_categorias, file = "diccionario_categorias_agua.rds")

# Construcción de los indicadores -----------------------------------------


# Indicador 6.1 -----------------------------------------------------------
# Porcentaje de hogares que utiliza suministros seguros de agua para beber



indicador_6_1 <- 
  tabla_agua %>% 
  # select( vi16, vi17, vi26, 
  #         ra01e_f, ra03_f, ra04_f, ra022_f,
  #         ra023_f, vi17a, vi17b, vi17c) %>% 
  mutate(
    # Tipo de suministro
    
    tuberia = if_else(condition = vi16 <= 3,
                           true = 1, 
                           false = 2),
         agua_cp1 = case_when(vi17 %in% c(1:3,7,9)   ~ 1,
                              vi17 %in% c(4,8,10,12) ~ 2,
                              vi17 %in% c(11,13)     ~ 3,
                              vi17 %in% c(6,5) & tuberia==1 ~ 1,
                              vi17 %in% c(6,5) & tuberia==2 ~ 2
                              ),
         
         
         # Se identifican pruebas válidas fuente
         
         validtest_f = case_when(vi26 == 1 & !is.na(ra01e_f) ~ 0,
                                 between(ra01e_f,left = 24,right = 48) & ra03_f == 2 & ra04_f == 2 ~ 1,
                                 vi26 == 2 ~ 2),
    
    # Presencia de coliformes fuente
    
         coli_f = case_when(
           validtest_f == 1 & ra023_f == 1 ~ 1,
           validtest_f == 1 & ra023_f == 2 ~ 2,
           validtest_f == 1 ~ 0),
    
    # Ausencia de E. coli fuente
    
         agua_cp2_f = case_when(
           validtest_f == 1 & (ra023_f == 2 | ra022_f == 2) ~ 1,
           validtest_f == 1 & ra023_f == 1  ~ 2,
           validtest_f == 1 & ra023_f == 1  ~ 0),
    
    # Cercanía del suministro
    # Supuesto: agua embotellada cerca para el consumo
    # Se  encuentra fuera de la vivienda, edifico/lote y no sabe a cuanto se encuentra (en minutos) la fuente 
         agua_cp3 = case_when(
           vi17a %in% c(1, 2,5,6) ~ 1, 
           vi17a == 3 & vi17b <= 30 ~ 2,
           vi17a == 3 & vi17b > 30 ~ 3,
           vi17b == 999 & (vi17a != 1 | vi17a != 2) ~ NA_real_),
    
         agua_cp4 = case_when(vi17c== 1 ~ 1,
                              vi17c == 3 ~ NA_real_,
                              vi17c > 1 ~ 2)
    )


# Corrección indicador de agua: -------------------------------------------


indicador_6_1 <- indicador_6_1 %>% 
  mutate(
    
    i_agua = case_when(agua_cp1 == 1 & agua_cp2_f == 1 & agua_cp3 == 1 & agua_cp4 == 1 ~ 1,
                       agua_cp1 == 1 & agua_cp2_f == 1 & agua_cp3 == 1 & agua_cp4 == 2 ~ 2,
                       agua_cp1 == 1 & agua_cp2_f == 1 & agua_cp3 == 2 ~ 2,
                       agua_cp1 == 1 & agua_cp2_f == 2 & agua_cp3 == 1 ~ 3,
                       agua_cp1 == 1 & agua_cp2_f == 2 & agua_cp3 == 2 ~ 3,
                       agua_cp1 == 1 & agua_cp3 == 3 ~ 4,
                       agua_cp1 == 2 ~ 5,
                       agua_cp1 == 3 ~ 6))
    

indicador_6_1 %>% 
  count(i_agua)

# Indicador 6.2.1: 
# Porcentaje de hogares que usa servicios de saneamiento básico -----------

#Componentes del indicador: 
#1) tipo de sistema de saneamiento
#2) exclusividad del servicio,
#3) manejo de desechos 


# Excusado y alcantarillado: 

sistema_saneamiento <- select(tabla_agua, vi13, vi13a, vi13b, vi13c, vi13d, vi14, vi15)

sistema_saneamiento <- 
  sistema_saneamiento %>% 
  mutate(sanea_cp1 = if_else(condition = vi13 == 1, true = 1, false = if_else(condition = vi13 == 2, true = 2, false = if_else(condition = vi13 == 3, true = 3, false = if_else(condition = vi13 == 4 & vi13b == 1, true = 4, false = if_else(condition = vi13 == 4 & vi13b == 2, true = 5, false = if_else(condition = vi13 == 5 & vi13d == 2, true = 6, false = if_else(condition = vi13 == 5 & vi13d == 2 & vi14==4 , true = 7, false = if_else(condition = vi13 == 5 & vi13d == 1, true = 8, false = 0)))))))),
         sanea_cp2 = if_else(condition = vi15 == 2, true = 1, false = if_else(condition = vi15 == 1 & vi13 == 5, true = 2, false = 0)),
         sanea_cp3 = if_else(condition = vi13 >= 2 & vi13 <= 3 & vi13a >= 2 & vi13a <= 4 & vi13c == 2, true = 1, false = if_else(condition = vi13 == 4 & vi13c == 2, true = 1, false=0)),
         sanea_cp3 = if_else(condition = vi13 >= 2 & vi13 <= 3 & vi13a >= 2 & vi13a <= 4 & vi13c == 2, true = 1, false = if_else(condition = vi13 >= 2 & vi13 <= 3 & vi13a == 1, true = 2, false = if_else(condition = vi13 == 4 & vi13c >= 1 & vi13c <= 3, true = 2, false = if_else(condition = vi13 == 4 & vi13c == 1, true = 2, false=0)))))


i_sanea <- select(sistema_saneamiento, sanea_cp1, sanea_cp2, sanea_cp3) 

i_sanea <- 
  i_sanea %>% 
  mutate(básico = if_else(condition = sanea_cp1 >= 1 & sanea_cp1 <= 4 & sanea_cp2 == 1 & sanea_cp3 == 1, true = 1, false = if_else(condition = sanea_cp1 >= 1 & sanea_cp1 <= 4 & sanea_cp2 == 1 & sanea_cp3 == 2, true = 1, false = 0)),
         limitado = if_else(condition = sanea_cp1 >= 1 & sanea_cp1 <= 4 & sanea_cp2 == 2, true = 2, false = if_else(condition = sanea_cp1 >= 2 & sanea_cp1 <= 3 & sanea_cp2 == 2 & sanea_cp3 == 1, true = 2, false = if_else(sanea_cp1 == 6, true = 2, false = 0))),
         no_mejorado = if_else(condition = sanea_cp1 >= 2 & sanea_cp1 <= 4 & sanea_cp2 == 2 & sanea_cp3 == 2, true = 3, false = if_else(condition = sanea_cp1 >=5 & sanea_cp1 <= 7, true = 3, false = 0)),
         aire_libre = if_else (condition = sanea_cp1 == 8, true = 4, false = 0))


#Indicador 6.2.2: Porcentaje de hogares que dispone de una instalación para lavarse las manos con agua y jabón  

higiene <- select(tabla_agua, vi29, vi31a, vi31b, vi31e, vi32, vi33, vi30)

higiene <- 
  higiene %>% 
  mutate(higiene_cp1 = if_else(condition = vi29 ==1, true = 1, false = if_else (condition = vi29==2 & vi32 >= 1 & vi32 <=3, true = 1, false = 0)),
         higiene_cp2 = if_else(condition = vi29 ==1 & vi30 ==1, true = 1, false = if_else(condition = vi29 == 1 & vi30 ==2, true = 2, false =  if_else(condition = vi29==2 & vi32 >= 1 & vi32 <= 3, true = 2, false = 0))),
         higiene_cp3 = if_else(condition = vi29 ==1 & vi30 ==1, true = 1, false = if_else(condition = vi31a ==1 & vi31b ==1 & vi31e ==2, true = 1, false = if_else(condition = vi29 ==1 & vi31a ==1 & vi31b ==1, true = 2, false =  if_else(condition = vi29 ==2 & vi32 >= 1 & vi32 <= 3, true = 2, false =0)))))




i_higiene <-  select(higiene, higiene_cp1, higiene_cp2, higiene_cp3)

i_higiene <- 
  i_higiene %>% 
  mutate(básico = if_else(condition = higiene_cp1 ==1 & higiene_cp2 ==1 & higiene_cp3 ==1, true = 1, false = 0),
         limitado = if_else(condition = higiene_cp1 ==1 & higiene_cp2 ==1 & higiene_cp3 ==2, true = 2, false = if_else(condition = higiene_cp1 ==1 & higiene_cp2 ==2 & higiene_cp3 ==1, true = 2, false = if_else(condition = higiene_cp1 ==1 & higiene_cp2 ==2 & higiene_cp3 ==2, true = 2, false = 0))),                                                                                                                 
         sin_instalación = if_else(condition = higiene_cp1 ==2, true = 3, false = 0))

