## ---- eval=FALSE, include=TRUE-----------------------------------------------
## "Protocolo:
## 
##  1. Daniel Felipe Villa Rengifo
## 
##  2. Lenguaje: R
## 
##  3. Tema: Manejo de la libreria tidyvverse
## 
##  4. Fuentes:
##     https://rpubs.com/Rene_Diaz/716415
##     https://www.policia.gov.co/grupo-informacion-criminalidad/estadistica-delictiva"


## ----------------------------------------------------------------------------
# Instalamos el paquete:
#install.packages("tidyverse", dependencies = T)
library(tidyverse)


## ----------------------------------------------------------------------------
# Importamos la base:
# Utilizamos lafuncion read_csv de readr en vez de read.csv del paquete base de R
assasins <- read_csv(file = "AsesinatosFBI.csv")

#Cada fila representa un estado con cada una de las cinco columnas proveyendo una variable diferente relacionada con estos estados: nombre, abreviatura, región, población y total de asesinatos.

"Es un ejermplo de un data frame tidy"


## ----------------------------------------------------------------------------
# Vamos añadir una nueva columna que tenga la tasa sobre 100.000 personas de asesionos por estado:

"estos datos estan aproximados a 2 cifras decimales"
assasins <- mutate(assasins, tasaX100000 = round(total/population * 100000, digits = 2))

#miremos nuestra nueva columna:
print(assasins)
"La nueva columna se lee como: por cada 100.000 personas hay ~X~ numero de asesinos"


# Podemos crear subconjuntos con la funcion filter:
"filter retorna un dataframe que cumpla una condicion especifica"

# Según el estado norteamericano se considera bajo indice de delicuencia sin superar 3 por 100.000

bajadelincuencia <- filter(assasins, tasaX100000 <= 3)
cat("Como podemos ver devuelve un dataframe:\n")
print(bajadelincuencia)


## ----------------------------------------------------------------------------
delincuencia_3 <- dplyr::select(bajadelincuencia, state, population,total)
write.table(delincuencia_3, file= "delincuencia_3.txt", row.names = F)


## ----------------------------------------------------------------------------
cat("16 %>% sqrt() # <- Raiz Cuadradrada de 16 (sqrt(16)=4)", "\n \nOUTPUT:\n")
print(16 %>% sqrt())



## ----------------------------------------------------------------------------
# Vamos a mostrar tres variables (estado, region, tasa) para los estados que tienen tasas de asesinatos por debajo de 1.5

bajadelincuencia <- assasins %>%
    dplyr::select(state, region, tasaX100000) %>%
    filter(tasaX100000 <= 1.5)

write.csv(bajadelincuencia, file = "bajadelincuencia.csv", row.names = F, sep = ",")
print(bajadelincuencia)


## ----------------------------------------------------------------------------
# Sacaremos los datos de la pagina de la policia sobre hurto a personas:
# PERÍODO COMPRENDIDO ENTRE EL 01 DE ENERO AL 30 DE ABRIL AÑO 2021

"NOTA: *Agrupación referente a la clasificación del código de infancia y adolescencia ley 1098 de del 8 de noviembre de 2006, en su artículo 3 donde se establecen los menores entre los 0 y los 12 años, adolescentes entre los 13 y los 17 años y adultos de 18 en adelante."

# imprtamos la base de datos de hurtos trimestral:
hurto <- read.csv(file = "hurto_personas_2021_1.csv", sep = ",", header = T)

# Quitemos los NA:
levels(hurto$ARMAS.MEDIOS)[1] <- "No Identificado"

levels(hurto$ARMAS.MEDIOS)[2] <- "ARMA BLANCA"

levels(hurto$GENERO)[1] <- "No Especificado"

levels(hurto$X.AGRUPA.EDAD.PERSONA)[1] <- "No Especificado"

# Convertimos varias columnas a factor y trabajar más facil:
hurto$DEPARTAMENTO <- as.factor(hurto$DEPARTAMENTO)

hurto$MUNICIPIO <- as.factor(hurto$MUNICIPIO)

hurto$ARMAS.MEDIOS <- as.factor(hurto$ARMAS.MEDIOS)

hurto$GENERO <- as.factor(hurto$GENERO)

hurto$X.AGRUPA.EDAD.PERSONA <- as.factor(hurto$X.AGRUPA.EDAD.PERSONA)

# Convetimos en fecha para un trabajo más sencillo:
library(lubridate)
hurto$FECHA.HECHO <- dmy(hurto$FECHA.HECHO)


## ----------------------------------------------------------------------------
# Cargamos la libreria de ser necesario
#library(ggplot2)

# Creamos la foto
png(filename = "ArmaMedio.png", height = 720, width = 1080)
# funcion para crear el plot
robo <- ggplot(hurto, aes(x= ARMAS.MEDIOS, fill = GENERO)) + geom_bar(position = "dodge")

# Dandole labels a los ejes y el titulo
robo + xlab("Tipo de Arma para el Robo") + ylab("Frecuencia") + ggtitle("Arma que Genera más Robo, distribuida por genero")

# Cerramos la foto para exportar
dev.off()

"Como podemos ver, la mayor arma es el engaño, muchas veces no llevan armas y solamente con la intimidación consiguen lo que quieren, es decir un hombre intimida (ya que como vemos los que más roban son hombres sin armas)"
