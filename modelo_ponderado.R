##### Modelo considerando las funciones de utilidad #####

library(QuantPsyc)
library(dplyr)
library(readxl)
library(rpart)
library(party)

list.files()

data <- read_excel('base_ponderada.xlsx', sheet=1, col_names = TRUE)
glimpse(data)

#### Graficas

plot(cbind(data$EvaluacionGlobalIES, sqrt(data$Consultasporusuario)))
plot(data[c("EvaluacionGlobalIES", "Consultasporusuario")])

#### Modelo

modelo <- lm(EvaluacionGlobalIES ~ Produccioncientifica + TitularidadTC + Informacionparalaevaluacion + Formacionposgrado + 
              Conectividad + Planificaciondelainvestigacion, data)
summary(modelo)

lm.beta(modelo)


Investigacionregional
Escalafon1 **
Eficienciaterminalposgrado
Admisionaestudiosdeposgrado
PorcentajeTC
HorasclaseMTTP
Planificaciondelainvestigacion **
Consultasporusuario
---------------------------------
RemuneracionMTTP
PosgradoenFormacion
Librosrevisadosporpares


data <- data %>% mutate(EST= 0.21452 + 0.97072*Produccioncientifica + 2.45186*TitularidadTC + 3.96950*Informacionparalaevaluacion + 
                  4.42327*Formacionposgrado + 3.81179*Conectividad + 2.02324*Planificaciondelainvestigacion, dif= EST-EvaluacionGlobalIES)

data <- data %>% mutate(NEW_CAL=ifelse(round(EST,2) >= 0.6, "A", ifelse(round(EST,2) >= 0.45, "B", ifelse(round(EST,2) >= 0.35, "C", "D"))))

table(data$categoria, data$NEW_CAL)

data %>% select(Código, categoria, NEW_CAL) %>% filter(categoria != NEW_CAL)

data2 <- tbl_df(cbind(index=seq(1,54), data))
dife1 <- data2 %>% select(index, dif) %>% filter(between(dif, -0.05, 0.05)) 
dife2 <- data2 %>% select(index, dif) %>% filter(dif < -0.05) 
dife3 <- data2 %>% select(index, dif) %>% filter(dif > 0.05)

plot(dife1, ylim=c(-0.1, 0.1), xlim=c(0,60), xlab="IES", ylab="Diferencia", col='green', pch=16, main="Residuos")
par(new=TRUE)
plot(dife2, ylim=c(-0.1, 0.1), xlim=c(0,60), xlab="IES", ylab="Diferencia", col='red', pch=16)
par(new=TRUE)
plot(dife3, ylim=c(-0.1, 0.1), xlim=c(0,60), xlab="IES", ylab="Diferencia", col='red', pch=16)

abline(a = -0.05, b = 0, col = "blue", lty = 5)
abline(a = 0.05, b = 0, col = "blue", lty = 5)

# IES con error fuera del 0.05
data %>% select(Código, dif) %>% filter(dif < -0.05)
data %>% select(Código, dif) %>% filter(dif > 0.05)


data %>% select(dif) %>% unlist() %>% shapiro.test()
data %>% select(dif) %>% unlist() %>% qqnorm()
data %>% select(dif) %>% unlist() %>% qqline()

data %>% select(Código, categoria, NEW_CAL) %>% filter(categoria != NEW_CAL)
