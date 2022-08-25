       
       ##################################################
       #                                                #
       #   ANÁLISIS DE INDICADORES DE CALIDAD DE AGUA   #  
       #                                                #
       ##################################################

       
# Instalación de librerías ------------------------------------------------
       
library(haven)
library(tidyverse)



# Lectura de la base de datos ---------------------------------------------

tabla_agua <- read_dta("publicacion_1/agua_ash2019.dta")


# Visualización de los datos ----------------------------------------------

glimpse(tabla_agua)


# Verificar el tipo de datos ----------------------------------------------

class(tabla_agua)



# Mostrar los atributos de las variables ----------------------------------

diccionario <- map(.x = tabla_agua,
                   .f = attributes)


# Información sobre diccionario -------------------------------------------

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

indicador <- select(tabla_agua, vi16, vi17, vi26, ra01e_f, ra03_f, ra04_f, ra022_f, ra023_f, vi17a, vi17b, vi17c)

indicador <- 
  indicador %>% 
  mutate(tuberia = if_else(condition = vi16 <=3,true = 1, false = 0),
         agua_cp1 = if_else(condition = vi17 <=6 & vi17 >=5 & tuberia==1, true = 1, false = 0),
         validtest_f = if_else (condition = ra01e_f >=24 & ra01e_f <=48 & ra03_f==2 & ra04_f==2,true = 1, false = 0),
         coli_f = if_else(condition = validtest_f==1 & ra022_f==1, true = 1, false = 0),
         agua_cp2_f = if_else(condition = validtest_f==1 & (ra023_f==2 | ra022_f==2), true = 1, false =  0),
         agua_cp3 = if_else(condition = vi17a >=1 & vi17a <=2 & vi17 >=5 & vi17 <=6, true = 1, false = if_else(condition = vi17a==3 & vi17b <=30, true =  2, false = 3 )),
         agua_cp4 = if_else(condition = vi17c ==1, true = 1, false = if_else(condition = vi17c>=1 & vi17c <=2, true =  2, false = 3 )))


#Creación de un dataframe más conciso

i_agua <- select(indicador, agua_cp1, agua_cp2_f, agua_cp3, agua_cp4)

# manejo seguro
i_gua <- 
  i_agua %>% 
  mutate(manejo_seguro = if_else(condition = agua_cp1==1 & agua_cp2_f==1 & agua_cp3==1 & agua_cp4==1, true = 1, false = 0),
         básico_1 = if_else(condition = agua_cp1==1 & agua_cp2_f==1 & agua_cp3==1 & agua_cp4==2, true = 1, false = 0),
         basico_1 = if_else(condition = agua_cp1==1 & agua_cp2_f==1 & agua_cp3==2, true = 1, false = 0),
         basico_2 = if_else(condition = agua_cp1==1 & agua_cp2_f==2 & agua_cp3==1, true = 1, false = 0),
         basico_2 = if_else(condition = agua_cp1==1 & agua_cp2_f==2 & agua_cp3==2, true = 1, false = 0),
         limitado = if_else(condition = agua_cp1==1 & agua_cp3==3, true = 1, false = 0),
         no_mejorado = if_else(condition = agua_cp1 == 2, true = 1, false = 0),
         superficial = if_else(condition = agua_cp1==3, true = 1, false = 0))





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

