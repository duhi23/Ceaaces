### Script regionalización ###

# 1 - Costa
# 2 - Sierra
# 3 - Amazonía

region <- c(2,2,1,2,2,1,2,2,1,2,1,1,1,1,2,1,2,1,2,2,1,2,1,1,1,2,2,1,2,1,2,2,2,2,2,1,2,2,2,2,1,2,1,1,2,2,2,2,3,2,1,2,1,2,2,1)
region <- factor(region, levels=c(1,2,3), labels=c("Costa", "Sierra", "Amazonia"))
table(region)
data <- cbind(data,region)

# Categoria
categoria <- read.table(file="categoria.csv", header=TRUE, sep=";", dec=",", stringsAsFactors=FALSE)
valoraciones <- read.table(file="valoraciones.csv", header=TRUE, sep=";", dec=",", stringsAsFactors=FALSE)
var <- merge(valoraciones, categoria, by.x="Codigo", by.y="cod")
data_new <- merge(data, var, by.x="C.digo", by.y="Codigo")
data_new$ctgr <- factor(data_new$ctgr)

## Diagramas de cajas por region
boxplot(Academia~region, data=data_new)
boxplot(Eficiencia~region, data=data_new)
boxplot(Investigacion~region, data=data_new)
boxplot(Organizacion~region, data=data_new)
boxplot(Infraestructura~region, data=data_new)

ggplot(data_new, aes(x=region, y=Academia, fill=as.factor(region))) + 
  geom_boxplot(outlier.colour="red",outlier.shape=16, outlier.size=4) + labs(x=" ") + labs(y="Indicador")+
  stat_boxplot(geom ='errorbar') + labs(fill=" ") + labs(title="Academia")+
  scale_fill_manual(values=colores) + theme(legend.position="none") + theme(text = element_text(size=15))+
  geom_text(data = aggregate(Academia ~ region , data_new, function(i) round(median(i),2)), 
            aes(label = Academia,family=c("serif")), position = position_dodge(width=0.8), 
            colour="red", size=7)


ggplot(data_new, aes(x=region, y=Eficiencia, fill=as.factor(region))) + 
  geom_boxplot(outlier.colour="red",outlier.shape=16, outlier.size=4) + labs(x=" ") + labs(y="Indicador")+
  stat_boxplot(geom ='errorbar') + labs(fill=" ") + labs(title="Eficiencia")+
  scale_fill_manual(values=colores) + theme(legend.position="none") + theme(text = element_text(size=15))+
  geom_text(data = aggregate(Eficiencia ~ region , data_new, function(i) round(median(i),2)), 
            aes(label = Eficiencia ,family=c("serif")), position = position_dodge(width=0.8), 
            colour="red", size=7)

ggplot(data_new, aes(x=region, y=Investigacion, fill=as.factor(region))) + 
  geom_boxplot(outlier.colour="red",outlier.shape=16, outlier.size=4) + labs(x=" ") + labs(y="Indicador")+
  stat_boxplot(geom ='errorbar') + labs(fill=" ") + labs(title="Investigacion")+
  scale_fill_manual(values=colores) + theme(legend.position="none") + theme(text = element_text(size=15))+
  geom_text(data = aggregate(Investigacion ~ region , data_new, function(i) round(median(i),2)), 
            aes(label = Investigacion ,family=c("serif")), position = position_dodge(width=0.8), 
            colour="red", size=7)

ggplot(data_new, aes(x=region, y=Organizacion, fill=as.factor(region))) + 
  geom_boxplot(outlier.colour="red",outlier.shape=16, outlier.size=4) + labs(x=" ") + labs(y="Indicador")+
  stat_boxplot(geom ='errorbar') + labs(fill=" ") + labs(title="Organizacion")+
  scale_fill_manual(values=colores) + theme(legend.position="none") + theme(text = element_text(size=15))+
  geom_text(data = aggregate(Organizacion ~ region , data_new, function(i) round(median(i),2)), 
            aes(label = Organizacion ,family=c("serif")), position = position_dodge(width=0.8), 
            colour="red", size=7)

ggplot(data_new, aes(x=region, y=Infraestructura, fill=as.factor(region))) + 
  geom_boxplot(outlier.colour="red",outlier.shape=16, outlier.size=4) + labs(x=" ") + labs(y="Indicador")+
  stat_boxplot(geom ='errorbar') + labs(fill=" ") + labs(title="Infraestructura")+
  scale_fill_manual(values=colores) + theme(legend.position="none") + theme(text = element_text(size=15))+
  geom_text(data = aggregate(Infraestructura ~ region , data_new, function(i) round(median(i),2)), 
            aes(label = Infraestructura ,family=c("serif")), position = position_dodge(width=0.8), 
            colour="red", size=7)

## Diagramas de cajas por categoria

ggplot(data_new, aes(x=ctgr, y=Academia, fill=as.factor(ctgr))) + 
  geom_boxplot(outlier.colour="red",outlier.shape=16, outlier.size=4) + labs(x=" ") + labs(y="Indicador")+
  stat_boxplot(geom ='errorbar') + labs(fill=" ") + labs(title="Academia")+
  scale_fill_manual(values=colores) + theme(legend.position="none") + theme(text = element_text(size=15))+
  geom_text(data = aggregate(Academia ~ ctgr , data_new, function(i) round(median(i),2)), 
            aes(label = Academia,family=c("serif")), position = position_dodge(width=0.8), 
            colour="red", size=7)


ggplot(data_new, aes(x=ctgr, y=Eficiencia, fill=as.factor(ctgr))) + 
  geom_boxplot(outlier.colour="red",outlier.shape=16, outlier.size=4) + labs(x=" ") + labs(y="Indicador")+
  stat_boxplot(geom ='errorbar') + labs(fill=" ") + labs(title="Eficiencia")+
  scale_fill_manual(values=colores) + theme(legend.position="none") + theme(text = element_text(size=15))+
  geom_text(data = aggregate(Eficiencia ~ ctgr , data_new, function(i) round(median(i),2)), 
            aes(label = Eficiencia ,family=c("serif")), position = position_dodge(width=0.8), 
            colour="red", size=7)

ggplot(data_new, aes(x=ctgr, y=Investigacion, fill=as.factor(ctgr))) + 
  geom_boxplot(outlier.colour="red",outlier.shape=16, outlier.size=4) + labs(x=" ") + labs(y="Indicador")+
  stat_boxplot(geom ='errorbar') + labs(fill=" ") + labs(title="Investigacion")+
  scale_fill_manual(values=colores) + theme(legend.position="none") + theme(text = element_text(size=15))+
  geom_text(data = aggregate(Investigacion ~ ctgr , data_new, function(i) round(median(i),2)), 
            aes(label = Investigacion ,family=c("serif")), position = position_dodge(width=0.8), 
            colour="red", size=7)

ggplot(data_new, aes(x=ctgr, y=Organizacion, fill=as.factor(ctgr))) + 
  geom_boxplot(outlier.colour="red",outlier.shape=16, outlier.size=4) + labs(x=" ") + labs(y="Indicador")+
  stat_boxplot(geom ='errorbar') + labs(fill=" ") + labs(title="Organizacion")+
  scale_fill_manual(values=colores) + theme(legend.position="none") + theme(text = element_text(size=15))+
  geom_text(data = aggregate(Organizacion ~ ctgr , data_new, function(i) round(median(i),2)), 
            aes(label = Organizacion ,family=c("serif")), position = position_dodge(width=0.8), 
            colour="red", size=7)

ggplot(data_new, aes(x=ctgr, y=Infraestructura, fill=as.factor(ctgr))) + 
  geom_boxplot(outlier.colour="red",outlier.shape=16, outlier.size=4) + labs(x=" ") + labs(y="Indicador")+
  stat_boxplot(geom ='errorbar') + labs(fill=" ") + labs(title="Infraestructura")+
  scale_fill_manual(values=colores) + theme(legend.position="none") + theme(text = element_text(size=15))+
  geom_text(data = aggregate(Infraestructura ~ ctgr, data_new, function(i) round(median(i),2)), 
            aes(label = Infraestructura ,family=c("serif")), position = position_dodge(width=0.8), 
            colour="red", size=7)



library(gplots)
plotmeans(Academia~region, xlab="Plot Means with Error Bars", data=data_new)

## Anova

library('agricolae')
model <- aov(Academia~region, data=data_new)
out <- LSD.test(model, "region", p.adj="bonferroni")
bar.group(out$groups,ylim=c(0,1),density=4,border="blue")


df<-df.residual(model)
MSerror<-deviance(model)/df
out <- LSD.test(Academia,region,df,MSerror, p.adj="bonferroni", group=FALSE)
bar.err(out$means,variation="range",ylim=c(0,1),bar=FALSE,col=0)
LSD.test(model,"region",p.adj="bon",console=TRUE)

