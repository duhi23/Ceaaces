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

i<-4
boxplot.with.outlier.label(data_new[,i]~data_new[,60], data_new[,1], push_text_right = .6, range = .2,
                           segement_width_as_percent_of_label_dist = 0.35, data=data_new, spread_text = F)
var[i]


### Histogramas para variables categóricas ###

colore <- c("green3","yellow","orange","red")
ggplot(data,aes(x=mantenimiento,fill=as.factor(mantenimiento)))+geom_histogram(binwidth=4)+scale_fill_manual(values=colore)+
  labs(x="MANTENIMIENTO")+labs(y="IES") +labs(fill="MANTENIMIENTO") + theme(text = element_text(size = 16))




