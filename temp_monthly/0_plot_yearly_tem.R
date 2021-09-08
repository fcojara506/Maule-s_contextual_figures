library(reshape2)
library(ggplot2)
setwd("~/Desktop/Maule's_contextual_figures/temp_monthly")
savefile_name<-"data_output/tmp_Armerillo.png"
file_data="data_input/tem_promedio_2012_2019_Armerillo.csv"


data<-read.csv(file =file_data, header = TRUE,check.names=FALSE)
data$promedio=rowMeans(data[,1:8])
data$mes=as.numeric(rownames(data))
labdate=c("Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dic","Ene","Feb","Mar")

meltData=melt(data,id.vars = "mes")
meltData$cat="2012-2018"
meltData$cat[meltData$variable=="2016"]="2016"

meltData$cat[meltData$variable=="2019"]="2019"
meltData$cat[meltData$variable=="2021"]="2021"

meltData$cat[meltData$variable=="promedio"]="Promedio 2012-2020"
#meltData$cat[driest_year]=paste0(gsub("-","/",data$WY[driest_year])," (Año más seco)")
#meltData$cat[is.na(data$cat)]=range_years


ggplot()+
  geom_line(data = meltData,aes(x=factor(mes),y=value,group=variable,col=cat),size=1.2)+
  scale_color_manual(name="Año Hidrológico",values = c("grey","springgreen3","orange", "red", "black"))+
  labs(title="Temperatura promedio mensual",
       subtitle = "Estación Armerillo (2012-2021)",
       x="",
       y="Temperatura del aire (°C)")+
  scale_x_discrete(labels=labdate,expand = c(0,0.01))+
  scale_y_continuous(expand = c(0,0.01))+
  theme(legend.position="bottom")+
  guides(col=guide_legend(nrow=2,byrow=TRUE))


ggsave(savefile_name,width = 5.5,height = 3.5,units = "in",dpi=500)
