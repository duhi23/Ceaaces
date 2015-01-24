#### Gráficos Producto N2 ####
# Indicadores vs Región

library(ggplot2)

var <-c("Codigo", "Nombre", "Formacion posgrado", "Posgrado en formacion", "Doctores TC", 
"Estudiantes por docente TC", "Horas clase TC", "Porcentaje de profesores TC", 
"Horas clase MT-TP", "Titularidad", "Titularidad TC", "Concurso", "Escalafon", "Evaluacion",
"Remuneracion TC", "Remuneracion MT-TP", "Direccion mujeres", "Docencia mujeres",
"Eficiencia terminal posgrado", "Eficiencia terminal pregrado",
"Tasa de retencion inicial pregrado", "Admision a estudios de pregrado",    
"Admision a estudios de posgrado", "Planificacion de la investigacion",
"Investigacion regional", "Produccion cientifica",     
"Libros revisados por pares", "Uso del seguimiento a graduados",  
"Programas de vinculacion", "Presupuesto de programas de vinculacion",
"Rendicion anual de cuentas", "Transparencia financiera", "Etica y responsabilidad", 
"Calidad del gasto", "Informacion para la evaluacion", "Regimen academico",             
"Accion afirmativa", "Espacio para estudiantes", "Titulos de libros", "Gestion de biblioteca",               
"Consultas por usuario", "Conectividad", "Innovacion tecnologica", "Cobertura a estudiantes",      
"Calidad de aulas", "Oficinas TC", "Oficinas MT-TP", "Espacios de bienestar",                 
"oferta", "mantenimiento", "INSTITUCION", "CA08", "CA05", "CA03", "CA01", "CA02", "CA07", "CA09",                             
"CA10", "region", "Academia", "Organizacion","Investigacion", "Infraestructura",                      
"Eficiencia", "factor_ponderacion", "valoracion", "nueva_valoracion", "ctgr")

for(i in c(3:12,15:21,25:27,29:30,34,38:39,41:42,44:47)){
  mypath <- file.path("","Users","Diego","Dropbox","Proyectos","Ceaaces","Informes",
                      paste(var[i], ".png", sep = ""))
  png(file=mypath)
  grafico <- ggplot(data_new, aes(x=data_new[,60], y=data_new[,i], fill=as.factor(data_new[,60]))) + 
    geom_boxplot(outlier.colour="red",outlier.shape=16, outlier.size=4) + labs(x=" ") + labs(fill=" ") +
    labs(y="Indicador")+stat_boxplot(geom ='errorbar') + labs(title=var[i]) + scale_fill_manual(values=colores) +
    theme(legend.position="none") + theme(text = element_text(size=15)) +
    annotate("text",y=aggregate(data_new[,i] ~ data_new[,60], data_new, function(i) round(median(i),2))[,2],
             x=c(1,2,3), label=aggregate(data_new[,i] ~ data_new[,60], data_new, function(i) round(median(i),2))[,2], size=6, colour="snow") +
    annotate("segment",x=0.5, xend=3.5, y=median(data_new[,i]), yend=median(data_new[,i]), colour="red2", linetype="dotted", size=1.1)
  print(grafico)
  dev.off()
}

lapply(c(3:12,15:21,25:27,29:30,34,38:39,41:42,44:47), function(i){mypath <- file.path("","Users","Diego","Dropbox","Proyectos","Ceaaces","Informes",
                                                   paste(var[i], ".png", sep = ""))
                                png(file=mypath)
                               grafico <- ggplot(data_new, aes(x=data_new[,60], y=data_new[,i], fill=as.factor(data_new[,60]))) + 
                                 geom_boxplot(outlier.colour="red",outlier.shape=16, outlier.size=4) + labs(x=" ") + labs(fill=" ") +
                                 labs(y="Indicador")+stat_boxplot(geom ='errorbar') + labs(title=var[i]) + scale_fill_manual(values=colores) +
                                 theme(legend.position="none") + theme(text = element_text(size=15)) +
                                 annotate("text",y=aggregate(data_new[,i] ~ data_new[,60], data_new, function(i) round(median(i),2))[,2],
                                          x=c(1,2,3), label=aggregate(data_new[,i] ~ data_new[,60], data_new, function(i) round(median(i),2))[,2], size=6, colour="snow") +
                                 annotate("segment",x=0.5, xend=3.5, y=median(data_new[,i]), yend=median(data_new[,i]), colour="red2", linetype="dotted", size=1.1)
                               print(grafico)
                               dev.off()})



var[c(3:12,15:21,25:27,29:30,34,38:39,41:42,44:47)]

i<-47
boxplot.with.outlier.label(data_new[,i]~data_new[,60], data_new[,1], push_text_right = .6, range = .2,
                           segement_width_as_percent_of_label_dist = 0.35, data=data_new, spread_text = F)
var[i]


### Histogramas para variables categóricas ###

colore <- c("green3","gold","orangered")
# Escalafón
data_new$Escalaf.n <- factor(data_new$Escalaf.n, labels=c("ALTO","MEDIO","BAJO"))
ggplot(data_new, aes(x=Escalaf.n, fill=as.factor(Escalaf.n))) + geom_histogram(binwidth=4)+
  scale_fill_manual(values=colore) + labs(x=" ") + labs(title="Escalafon") + labs(fill="SEGMENTACION") +
  labs(y="CASOS") + theme(text=element_text(size=14), legend.position="none") + facet_grid(.~ region)

# Evaluacion
data_new$Evaluaci.n <- factor(data_new$Evaluaci.n, labels=c("ALTO","MEDIO","BAJO"))
ggplot(data_new, aes(x=Evaluaci.n, fill=as.factor(Evaluaci.n))) + geom_histogram(binwidth=4)+
  scale_fill_manual(values=colore) + labs(x=" ") + labs(title="Evaluacion") + 
  labs(y="CASOS") + theme(text=element_text(size=14), legend.position="none") + facet_grid(.~ region)

# Admisión a estudios de pregrado
var <- ifelse(data_new$Admisi.n.a.estudios.de.pregrado == "ALTO",1,
              ifelse(data_new$Admisi.n.a.estudios.de.pregrado == "MEDIO",2,3))  
data_new$Admisi.n.a.estudios.de.pregrado <- factor(var, levels=c(1,2,3), labels=c("ALTO","MEDIO","BAJO"))

ggplot(data_new, aes(x=Admisi.n.a.estudios.de.pregrado, fill=as.factor(Admisi.n.a.estudios.de.pregrado))) + geom_histogram(binwidth=4)+
  scale_fill_manual(values=colore) + labs(x=" ") + labs(title="Admision a estudios de pregrado") + 
  labs(y="CASOS") + theme(text=element_text(size=14), legend.position="none") + facet_grid(.~ region)

# Admisión a estudios de posgrado
var <- ifelse(data_new$Admisi.n.a.estudios.de.posgrado == "ALTO",1,
              ifelse(data_new$Admisi.n.a.estudios.de.posgrado == "MEDIO",2,3))  
data_new$Admisi.n.a.estudios.de.posgrado <- factor(var, levels=c(1,2,3), labels=c("ALTO","MEDIO","BAJO"))

ggplot(data_new, aes(x=Admisi.n.a.estudios.de.posgrado, fill=as.factor(Admisi.n.a.estudios.de.posgrado))) + geom_histogram(binwidth=4)+
  scale_fill_manual(values=colore) + labs(x=" ") + labs(title="Admision a estudios de posgrado") + 
  labs(y="CASOS") + theme(text=element_text(size=14), legend.position="none") + facet_grid(.~ region)

# Planificación de la investigación
var <- ifelse(data_new$Planificaci.n.de.la.investigaci.n == "ALTO",1,
              ifelse(data_new$Planificaci.n.de.la.investigaci.n == "MEDIO",2,3))  
data_new$Planificaci.n.de.la.investigaci.n <- factor(var, levels=c(1,2,3), labels=c("ALTO","MEDIO","BAJO"))

ggplot(data_new, aes(x=Planificaci.n.de.la.investigaci.n, fill=as.factor(Planificaci.n.de.la.investigaci.n))) + geom_histogram(binwidth=4)+
  scale_fill_manual(values=colore) + labs(x=" ") + labs(title="Planificacion de la investigacion") + 
  labs(y="CASOS") + theme(text=element_text(size=14), legend.position="none") + facet_grid(.~ region)

# Uso del seguimiento a graduados
var <- ifelse(data_new$Uso.del.seguimiento.a.graduados == "ALTO",1,
              ifelse(data_new$Uso.del.seguimiento.a.graduados == "MEDIO",2,3))  
data_new$Uso.del.seguimiento.a.graduados <- factor(var, levels=c(1,2,3), labels=c("ALTO","MEDIO","BAJO"))

ggplot(data_new, aes(x=Uso.del.seguimiento.a.graduados, fill=as.factor(Uso.del.seguimiento.a.graduados))) + geom_histogram(binwidth=4)+
  scale_fill_manual(values=colore) + labs(x=" ") + labs(title="Uso del seguimiento a graduados") + 
  labs(y="CASOS") + theme(text=element_text(size=14), legend.position="none") + facet_grid(.~ region)

# Rendicion anual de cuentas
var <- ifelse(data_new$Rendici.n.anual.de.cuentas == "ALTO",1,
              ifelse(data_new$Rendici.n.anual.de.cuentas == "MEDIO",2,3))  
data_new$Rendici.n.anual.de.cuentas <- factor(var, levels=c(1,2,3), labels=c("ALTO","MEDIO","BAJO"))

ggplot(data_new, aes(x=Rendici.n.anual.de.cuentas, fill=as.factor(Rendici.n.anual.de.cuentas))) + geom_histogram(binwidth=4)+
  scale_fill_manual(values=colore) + labs(x=" ") + labs(title="Rendicion anual de cuentas") + 
  labs(y="CASOS") + theme(text=element_text(size=14), legend.position="none") + facet_grid(.~ region)

# Transparencia financiera
var <- ifelse(data_new$Transparencia.financiera == "ALTO",1,
              ifelse(data_new$Transparencia.financiera == "MEDIO",2,3))  
data_new$Transparencia.financiera <- factor(var, levels=c(1,2,3), labels=c("ALTO","MEDIO","BAJO"))

ggplot(data_new, aes(x=Transparencia.financiera, fill=as.factor(Transparencia.financiera))) + geom_histogram(binwidth=4)+
  scale_fill_manual(values=colore) + labs(x=" ") + labs(title="Transparencia financiera") + 
  labs(y="CASOS") + theme(text=element_text(size=14), legend.position="none") + facet_grid(.~ region)

# Etica y responsabilidad
var <- ifelse(data_new$X.tica.y.responsabilidad == "ALTO",1,
              ifelse(data_new$X.tica.y.responsabilidad == "MEDIO",2,3))  
data_new$X.tica.y.responsabilidad <- factor(var, levels=c(1,2,3), labels=c("ALTO","MEDIO","BAJO"))

ggplot(data_new, aes(x=X.tica.y.responsabilidad, fill=as.factor(X.tica.y.responsabilidad))) + geom_histogram(binwidth=4)+
  scale_fill_manual(values=colore) + labs(x=" ") + labs(title="Etica y responsabilidad") + 
  labs(y="CASOS") + theme(text=element_text(size=14), legend.position="none") + facet_grid(.~ region)

# Informacion para la evaluacion
var <- ifelse(data_new$Informaci.n.para.la.evaluaci.n == "ALTO",1,
              ifelse(data_new$Informaci.n.para.la.evaluaci.n == "MEDIO",2,3))  
data_new$Informaci.n.para.la.evaluaci.n <- factor(var, levels=c(1,2,3), labels=c("ALTO","MEDIO","BAJO"))

ggplot(data_new, aes(x=Informaci.n.para.la.evaluaci.n, fill=as.factor(Informaci.n.para.la.evaluaci.n))) + geom_histogram(binwidth=4)+
  scale_fill_manual(values=colore) + labs(x=" ") + labs(title="Informacion para la evaluacion") + 
  labs(y="CASOS") + theme(text=element_text(size=14), legend.position="none") + facet_grid(.~ region)


# Regimen academico
var <- ifelse(data_new$R.gimen.acad.mico == "ALTO",1,
              ifelse(data_new$R.gimen.acad.mico == "MEDIO",2,3))  
data_new$R.gimen.acad.mico <- factor(var, levels=c(1,2,3), labels=c("ALTO","MEDIO","BAJO"))

ggplot(data_new, aes(x=R.gimen.acad.mico, fill=as.factor(R.gimen.acad.mico))) + geom_histogram(binwidth=4)+
  scale_fill_manual(values=colore) + labs(x=" ") + labs(title="Regimen academico") + 
  labs(y="CASOS") + theme(text=element_text(size=14), legend.position="none") + facet_grid(.~ region)

# Accion afirmativa
var <- ifelse(data_new$Acci.n.afirmativa == "ALTO",1,
              ifelse(data_new$Acci.n.afirmativa == "MEDIO",2,3))  
data_new$Acci.n.afirmativa <- factor(var, levels=c(1,2,3), labels=c("ALTO","MEDIO","BAJO"))

ggplot(data_new, aes(x=Acci.n.afirmativa, fill=as.factor(Acci.n.afirmativa))) + geom_histogram(binwidth=4)+
  scale_fill_manual(values=colore) + labs(x=" ") + labs(title="Accion afirmativa") + 
  labs(y="CASOS") + theme(text=element_text(size=14), legend.position="none") + facet_grid(.~ region)


# Gestion de biblioteca
var <- ifelse(data_new$Gesti.n.de.biblioteca == "ALTO",1,
              ifelse(data_new$Gesti.n.de.biblioteca == "MEDIO",2,3))  
data_new$Gesti.n.de.biblioteca <- factor(var, levels=c(1,2,3), labels=c("ALTO","MEDIO","BAJO"))

ggplot(data_new, aes(x=Gesti.n.de.biblioteca, fill=as.factor(Gesti.n.de.biblioteca))) + geom_histogram(binwidth=4)+
  scale_fill_manual(values=colore) + labs(x=" ") + labs(title="Gestion de biblioteca") + 
  labs(y="CASOS") + theme(text=element_text(size=14), legend.position="none") + facet_grid(.~ region)

# Innovacion tecnologica
var <- ifelse(data_new$Innovaci.n.tecnol.gica == "ALTO",1,
              ifelse(data_new$Innovaci.n.tecnol.gica == "MEDIO",2,3))  
data_new$Innovaci.n.tecnol.gica <- factor(var, levels=c(1,2,3), labels=c("ALTO","MEDIO","BAJO"))

ggplot(data_new, aes(x=Innovaci.n.tecnol.gica, fill=as.factor(Innovaci.n.tecnol.gica))) + geom_histogram(binwidth=4)+
  scale_fill_manual(values=colore) + labs(x=" ") + labs(title="Innovacion tecnologica") + 
  labs(y="CASOS") + theme(text=element_text(size=14), legend.position="none") + facet_grid(.~ region)

# Espacios de bienestar
var <- ifelse(data_new$Espacios.de.bienestar == "ALTO",1,
              ifelse(data_new$Espacios.de.bienestar == "MEDIO",2,3))  
data_new$Espacios.de.bienestar <- factor(var, levels=c(1,2,3), labels=c("ALTO","MEDIO","BAJO"))

ggplot(data_new, aes(x=Espacios.de.bienestar, fill=as.factor(Espacios.de.bienestar))) + geom_histogram(binwidth=4)+
  scale_fill_manual(values=colore) + labs(x=" ") + labs(title="Espacios de bienestar") + 
  labs(y="CASOS") + theme(text=element_text(size=14), legend.position="none") + facet_grid(.~ region)



