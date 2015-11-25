##### Modelo considerando las funciones de utilidad #####

library(QuantPsyc)
library(dplyr)
library(readxl)
library(rpart)
library(party)

list.files()

data <- read_excel('base_ponderada.xlsx', sheet=1, col_names = TRUE)
glimpse(data)

prepos <- c(1001, 1002, 1003, 1005, 1006, 1007, 1008, 1010, 1011, 1012, 1013, 1014, 1015, 1016, 1017, 1018, 1019, 1020, 1021,
              1024, 1025, 1027, 1028, 1030, 1031, 1032, 1033, 1034, 1036, 1037, 1038, 1040, 1041, 1042, 1044, 1045, 1049,
              1050, 1051, 1053, 1056)

data <- data %>% filter(Código %in% prepos)

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


data <- data %>% mutate(EST= 0.23416 + 1.20096*Produccioncientifica + 2.25966*TitularidadTC + 5.65325*Informacionparalaevaluacion + 
                  2.76458*Formacionposgrado + 4.02928*Conectividad + 2.06669*Planificaciondelainvestigacion, dif= EST-EvaluacionGlobalIES)

data <- data %>% mutate(NEW_CAL=ifelse(round(EST,2) >= 0.6, "A", ifelse(round(EST,2) >= 0.45, "B", ifelse(round(EST,2) >= 0.35, "C", "D"))))

table(data$categoria, data$NEW_CAL)

data %>% select(Código, categoria, NEW_CAL) %>% filter(categoria != NEW_CAL)

data2 <- tbl_df(cbind(index=seq(1,41), data))
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

# Pruebas de normalidad errores
data %>% select(dif) %>% unlist() %>% shapiro.test()
data %>% select(dif) %>% unlist() %>% qqnorm()
data %>% select(dif) %>% unlist() %>% qqline()

# Matriz cruzada
data %>% select(Código, categoria, NEW_CAL) %>% filter(categoria != NEW_CAL)
