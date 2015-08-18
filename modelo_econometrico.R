### Modelo Econometrico ###
library(dplyr)
library(readxl)
library(rpart)
library(party)

list.files()

data <- read_excel('base_modelo.xlsx', sheet=1, col_names = TRUE)
glimpse(data)

data <- data %>% mutate(DUMMY = ifelse(VALORACION>=0.4, 1, 0))

arbol <- rpart(DUMMY ~ OFERTA + MANTENIMIENTO, data)
plot(arbol, uniform=TRUE, main="Arbol de clasificación")
text(arbol, use.n=TRUE, all=TRUE, cex=.8)

plot(ctree(VALORACION ~ DIRECCIONMUJERES + CONCURSO, data))

data <- data %>% mutate(D_MANTENIMIENTO = ifelse(MANTENIMIENTO=="Publica", 0.4736842, ifelse(MANTENIMIENTO=="Privada", 0.3684211, 0.1578947)))

plot(cbind(data$VALORACION, sqrt(data$CONSULTASPORUSUARIO)))

plot(data[c("VALORACION", "CONSULTASPORUSUARIO")])

modelo <- lm(VALORACION ~ FORMACIONPOSGRADO + ESTUDIANTESPORDOCENTETC + HORASCLASESMTTP + CONCURSO, data)
summary(modelo)
modelo <- lm(VALORACION ~ log(REMUNERACIONTC) + DIRECCIONMUJERES + EFICIENCIATERMINALPOSGRADO + PRODUCCIONCIENTIFICA + LIBROSREVISADOSPORPARES, data)
summary(modelo)
modelo <- lm(VALORACION ~ CONSULTASPORUSUARIO  + CONECTIVIDAD  + COBERTURAAESTUDIANTES + OFICINASMTTP, data)
summary(modelo)

# Modelo final
# modelo <- lm(VALORACION ~ FORMACIONPOSGRADO + HORASCLASESMTTP + CONCURSO + log(REMUNERACIONTC) + log(LIBROSREVISADOSPORPARES+0.1) + 
#               log(CONECTIVIDAD+0.1)  + sqrt(COBERTURAAESTUDIANTES) + OFICINASMTTP, data)
# summary(modelo)

modelo <- lm(VALORACION ~ FORMACIONPOSGRADO + HORASCLASESMTTP + CONCURSO + log(REMUNERACIONTC) + sqrt(LIBROSREVISADOSPORPARES) + 
               log(CONECTIVIDAD+0.1)  + sqrt(COBERTURAAESTUDIANTES) + OFICINASMTTP + sqrt(PRODUCCIONCIENTIFICA), data)
summary(modelo)

data <- data %>% mutate(EST= -0.5961489 + 0.0024294*FORMACIONPOSGRADO -0.0048324*HORASCLASESMTTP + 0.0006680*CONCURSO +0.1101033*log(REMUNERACIONTC)+
                          0.1372203*sqrt(LIBROSREVISADOSPORPARES) + 0.0315445*log(CONECTIVIDAD + 0.1) + 0.0073168*sqrt(COBERTURAAESTUDIANTES) -0.0019802*OFICINASMTTP +
                          0.0458477*sqrt(PRODUCCIONCIENTIFICA), dif= abs(EST-VALORACION))

data <- data %>% mutate(NEW_CAL=ifelse(round(EST,2) >= 0.6, "A", ifelse(round(EST,2) >= 0.45, "B", ifelse(round(EST,2) >= 0.35, "C", "D"))))

table(data$CATEGORIA, data$NEW_CAL)

dife <- data %>% mutate(dife = EST-VALORACION) %>% select(dife)
plot(cbind(seq(1,54), dife), ylim=c(-0.1, 0.1), xlim=c(0,60), xlab="IES", ylab="Diferencia")

# Resumen diferencia
dife %>% summary()

# Estadisticos por Categoria
by(data$VALORACION, data$CATEGORIA, summary)

data %>% select(VALORACION, CATEGORIA) %>% filter(CATEGORIA=="B") %>% head(20)

data %>% select(VALORACION, EST, CATEGORIA, NEW_CAL) %>% tail(30)

## Error en la estimacion
1-(5+23+10)/54
