rm(list=ls())
setwd("~")

############################
            #
############################

require(foreign)
#require(plyr)
require(dplyr)
require(tidyverse)
require(readstata13)
require(reshape2)
require(readxl)

out <- "/Users/carloscabellogutierrez/Downloads"
grf <- "/Users/carloscabellogutierrez/Downloads"

data <- read_xlsx(paste(out, "miniencuestaRespuestas2.xlsx", sep="/"))

names(data) <- tolower(names(data))

names(data)[13] <- "partido"
names(data)[14] <- "simpatizas"

data <- data %>%
        select(n, rep, edad, carrera, sexo, partido, simpatizas, w1, eva, carr)
#detach("package:plyr")

View(data)
data$con <- 1
don <- group_by(data, n)
don <- summarise(don, fre=sum(con))
datos <- full_join(data, don, by="n")
View(datos)

# crear ponderador
datos$pon <- round((1/datos$fre),1)

datos$fre <- NULL

datos <- spread(datos, eva, value=pon)
datos[is.na(datos)] <- 0

#1. frecuencia de conotación
tempo <- datos  %>%  
         summarise(ne=sum(neg), po=sum(pos), nu=sum(neu)) %>% 
         mutate(tot=ne+po+nu) %>% 
         mutate(positivo=round(po/tot*100, 0),
                negativo=round(ne/tot*100, 0),
                neutral=round(nu/tot*100, 0)) %>% 
        select(positivo, negativo, neutral) %>% 
        gather(cat, value=frecuencia, positivo:neutral)

gr <- ggplot(tempo, aes(x=frecuencia, y=cat)) +
  geom_point() +
  labs(title="Palabras relacionadas con reelección", x="Porcentaje", y="Connotación")

#2. relacion de porcentaje de cada una por partido
tempo <- datos %>%
        mutate(partidos= ifelse(partido=="Ninguno", "Ninguno/Otro", 
                        ifelse(partido=="Otro", "Ninguno/Otro", partido))) %>%
         group_by(partidos) %>%
         summarise(ne=sum(neg), po=sum(pos), nu=sum(neu)) %>%
         mutate(tot=ne+po+nu)

tempo$pp <- round((tempo$po/tempo$tot)*100, 0)
tempo$pn <- round((tempo$ne/tempo$tot)*100, 0)
tempo$pu <- round((tempo$nu/tempo$tot)*100, 0)  
tempo$prueba <- tempo$pp + tempo$pn +tempo$pu

tempo <- select(tempo, partidos, pp, pn, pu)
tempo <- gather(tempo, cat, value=porcen, pp: pu)
tempo <- tempo %>%
  mutate(ca= ifelse(cat=="pn", "Negativa",
                    ifelse(cat=="pu", "Neutral", "Positiva")))

gr <- ggplot(tempo, aes(x = partidos, y=porcen, fill=cat)) +
  geom_bar(stat = "identity") +
  labs(title="Connotación de palabras relacionadas con reelección por partido", x="Partido", y="Porcentaje")
dev.off()

gr <- ggplot(tempo, aes(x = partidos, y=porcen, color=ca)) + 
  geom_bar(stat="identity") +
  labs(title="Connotación de palabras relacionadas con reelección por partido", x="Partido", y="Porcentaje")

#3. relacion por sexo
tempo <- group_by(datos, sexo)
tempo <- summarise(tempo, ne=sum(neg), po=sum(pos), nu=sum(neu))
tempo$tot <- tempo$ne + tempo$po +tempo$nu
tempo$pp <- round((tempo$po/tempo$tot)*100, 0)
tempo$pn <- round((tempo$ne/tempo$tot)*100, 0)
tempo$pu <- round((tempo$nu/tempo$tot)*100, 0)  
tempo$prueba <- tempo$pp + tempo$pn +tempo$pu

tempo <- select(tempo, sexo, pp, pn, pu)
tempo <- gather(tempo, cat, value=porcen, pp: pu)
tempo <- tempo %>%
  mutate(ca= ifelse(cat=="pn", "Negativa",
                    ifelse(cat=="pu", "Neutral", "Positiva")))

gr <- ggplot(tempo, aes(x = sexo, y=porcen, fill=cat)) +
  geom_bar(stat = "identity") +
  labs(title="Connotación de palabras relacionadas con reelección por sexo", x="Sexo", y="Porcentaje")

gr <- ggplot(tempo, aes(x = sexo, y=porcen, color=cat)) + 
  geom_bar(stat="identity") +
  labs(title="Connotación de palabras relacionadas con reelección por sexo", x="Sexo", y="Porcentaje")


#4. licenciatura por ciencia politica, RRII, Eco, otras, derecho
tempo <- group_by(datos, carr)
tempo <- summarise(tempo, ne=sum(neg), po=sum(pos), nu=sum(neu))
tempo$tot <- tempo$ne + tempo$po +tempo$nu
tempo$pp <- round((tempo$po/tempo$tot)*100, 0)
tempo$pn <- round((tempo$ne/tempo$tot)*100, 0)
tempo$pu <- round((tempo$nu/tempo$tot)*100, 0)  
tempo$prueba <- tempo$pp + tempo$pn +tempo$pu

tempo <- select(tempo, carr, pp, pn, pu)
tempo <- gather(tempo, cat, value=porcen, pp: pu)
tempo <- tempo %>%
  mutate(carrera= ifelse(carr==1, "Ciencia Politica", 
                         ifelse(carr==2, "RRII", 
                                ifelse(carr==3, "Economia",
                                       ifelse(carr==4, "Otra",
                                              ifelse(carr==5, "Derecho",NA))))))  %>%
  mutate(ca= ifelse(cat=="pn", "Negativa",
             ifelse(cat=="pu", "Neutral", "Positiva")))

gr <- ggplot(tempo, aes(x = carrera, y=porcen, fill=ca)) +
  geom_bar(stat = "identity") +
  labs(title="Connotación de palabras relacionadas con reelección por carrera", x="Carrera", y="Porcentaje del total")

gr <- ggplot(tempo, aes(x = carrera, y=porcen, color=ca)) +
  geom_bar(stat="identity", color=ca) +
  labs(title="Conotación de palabras relacionadas con reelección por carrera", x="Carrera", y="Porcentaje del total") 

  
gr <- ggplot(tempo, aes(x = carrera, y=porcen, color=ca)) + 
      geom_point(size=4.3) +
      labs(title="Conotación de palabras relacionadas con reelección por carrera", x="Carrera", y="Porcentaje del total") 



##scatter intesidad con connotación

tempo <- datos  %>%  
  summarise(ne=sum(neg), po=sum(pos), nu=sum(neu)) %>% 
  mutate(tot=ne+po+nu) %>% 
  mutate(positivo=round(po/tot*100, 0),
         negativo=round(ne/tot*100, 0),
         neutral=round(nu/tot*100, 0)) %>% 
  select(positivo, negativo, neutral) %>% 
  gather(cat, value=frecuencia, positivo:neutral)



##spineplot
#install.packages("ggmosaic")
require(ggmosaic)

tempo <- data %>%
  mutate(partidos= ifelse(partido=="Ninguno", "Ninguno/Otro", 
                          ifelse(partido=="Otro", "Ninguno/Otro", partido))) %>%
  group_by(sexo, partidos) %>%
  summarise(total = sum(con)) %>%
  ungroup()

gr <- ggplot(data = tempo) +
  geom_mosaic(aes(weight = total, x = product(partidos, sexo), fill=factor(partidos)), na.rm=TRUE) +
  labs(title="¿Quiénes contestaron la encuesta?", x="Sexo", y="Partidos", fill="")


### segnudo spineplot
tempo <- datos  %>%
  group_by(partido) %>% 
  summarise(ne=sum(neg), po=sum(pos), nu=sum(neu)) %>% 
  mutate(partidos= ifelse(partido=="Ninguno", "Ninguno/Otro", 
                          ifelse(partido=="Otro", "Ninguno/Otro", partido))) %>%
  mutate(tot=ne+po+nu) %>% 
  mutate(positivo=round(po/tot*100, 0),
         negativo=round(ne/tot*100, 0),
         neutral=round(nu/tot*100, 0))

tempo$prueba <- tempo$positivo + tempo$negativo +tempo$neutral

temp <- tempo %>% 
     select(partidos, positivo, negativo, neutral) %>%
     gather(connotacion, value=porcentaje, positivo:neutral)  

gr <- ggplot(data = temp) +
  geom_mosaic(aes(weight = porcentaje, x = product(connotacion, partidos), fill=factor(connotacion)), na.rm=TRUE) +
  labs(title="¿Cómo se siente cada partido?", x="Partido", y="Porcentaje", fill="")

##tercer spinepot
tempo <- datos  %>%
  group_by(sexo) %>% 
  summarise(ne=sum(neg), po=sum(pos), nu=sum(neu)) %>% 
  mutate(tot=ne+po+nu) %>% 
  mutate(positivo=round(po/tot*100, 0),
         negativo=round(ne/tot*100, 0),
         neutral=round(nu/tot*100, 0))

tempo$prueba <- tempo$positivo + tempo$negativo +tempo$neutral

temp <- tempo %>% 
  select(sexo, positivo, negativo, neutral) %>%
  gather(connotacion, value=porcentaje, positivo:neutral)  

gr <- ggplot(data = temp) +
  geom_mosaic(aes(weight = porcentaje, x = product(connotacion, sexo), fill=factor(connotacion)), na.rm=TRUE) +
  labs(title="¿Mujeres o hombres más de acuerdo?", x="Porcentaje por sexo", y="Porcentajepor connotacion ", fill="")