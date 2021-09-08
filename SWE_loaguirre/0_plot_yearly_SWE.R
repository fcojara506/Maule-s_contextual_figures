library(reshape2)
library(ggplot2)
setwd("~/Desktop/Maule's_contextual_figures/SWE_loaguirre")

pp_image <- function(savefile_name,file_data,title,periodo) {
  
data<-read.csv(file =file_data, header = TRUE,check.names=FALSE)
data$promedio=rowMeans(data[,1:15])
data$mes=as.numeric(rownames(data))

labdate=c("Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dic","Ene","Feb","Mar")
periodo="2001-2020"

meltData=melt(data,id.vars = "mes")
meltData$cat=periodo
meltData$cat[meltData$variable=="2016"]="2016"
meltData$cat[meltData$variable=="2019"]="2019"
meltData$cat[meltData$variable=="2021"]="2021"
meltData$cat[meltData$variable=="promedio"]=paste0("Promedio ",periodo)

ggplot()+
  geom_line(data = meltData,aes(x=factor(mes),y=value,group=variable,col=cat),size=1.2)+
  scale_color_manual(name="Año Hidrológico",values = c("grey","springgreen3","orange", "red", "black"))+
  labs(title="Máximo mensual del Equivalente en agua de nieve",
       subtitle = "Estación Lo Aguirre (2001-2007/2012-2021)",
       x="",
       y="SWE (mm)")+
  scale_x_discrete(labels=labdate,expand = c(0,0.01))+
  scale_y_continuous(expand = c(0,0.01))+
  theme(legend.position="bottom")+
  guides(col=guide_legend(nrow=2,byrow=TRUE))

ggsave(savefile_name,width =5.5,height = 3.5,units = "in",dpi=500)
}

savefile_name<-"data_output/swe_LoAguirre.png"
file_data="data_input/swe_max_2001_2020_LoAguirre.csv"


pp_image(savefile_name,file_data)
