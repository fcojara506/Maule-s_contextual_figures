library(dplyr)
library(readr)
library(reshape2)
library(ggplot2)
library(forcats)
library(data.table)

rm(list = ls())
setwd("~/Desktop/Maule's_contextual_figures/pp_acum_monthly")

plot_CumPrecipitation=function(pattern,title,savefile_name){


data          <- read.csv(file =  pattern) 

mean_pp       <- c(-9999,colMeans(as.matrix(data[1:nrow(data)-1,2:ncol(data)])))
data          <- rbind(data,mean_pp)


range_years=paste0(gsub("-","/",data$WY[[1]])," - 2019/20")
data$cat=NA
data$cat[data$WY=="2016-17"]="2016/17"
data$cat[data$WY=="2019-20"]="2019/20"
data$cat[data$WY=="2021-22"]="2021/22"
data$cat[data$WY==-9999]="Promedio 1948/49-2020/21"
data$cat[is.na(data$cat)]=range_years
df=reshape2::melt(data,id.vars=c("WY","cat"))

ggplot()+
  geom_line(data = df[df$cat==range_years,],aes(x=factor(variable),y=value,group=WY,color=cat),size=1)+ #todas las lineas
  geom_line(data = df[df$cat!=range_years,],aes(x=factor(variable),y=value,group=WY,color=cat),size=1)+ #lineas más importantes
  #geom_line(data = df[df$WY=="1998-99",],aes(x=factor(variable),y=value,group=WY,color=cat),size=1.2)+
  scale_color_manual(name="Año Hidrológico",values = c("grey","springgreen3", "orange", "red", "black"))+
  labs( title= "Precipitación acumulada mensual",
        subtitle=title,
       x="",
       y="Precipitación (mm)")+
  scale_x_discrete(expand = c(0,0.01))+
  scale_y_continuous(expand = c(0,0.01))+
  theme_bw()+
  theme(legend.position="bottom")+
  guides(col=guide_legend(nrow=2,byrow=TRUE))

#ggsave(savefile_name,width = 8,height = 3,units = "in",dpi=400)
ggsave(savefile_name,width = 6,height = 4,units = "in",dpi=400)
}
#__________________________MAIN__________________________________________


#pattern<- "pp_linares_1967_2020_analisis.csv"
#title  <-"Precipitación en estación Linares (1967-2019). Alt= 157 msnm"
#savefile_name<-"PP_linares.png"

# pattern<- "pp_longavi_2000_2020_analisis.csv"
# title  <-"Precipitación en estación Longavi en Quiriquina (2001-2019). 455 m"
# savefile_name<-"PP_longavi.png"
# plot_CumPrecipitation(pattern = pattern,title= title, savefile_name = savefile_name)
# 
# pattern<- "pp_ancoa_2000_2020_analisis.csv"
# title  <-"Precipitación en estación Ancoa en el Morro (2000-2019). 430 m"
# savefile_name<-"PP_ancoa.png"
# plot_CumPrecipitation(pattern = pattern,title= title, savefile_name = savefile_name)
# 
# pattern<- "pp_palos_colorado_1948_2020_analisis.csv"
# title<-"Río Palos en junta con Colorado (2000-2019). 600 m"
# savefile_name<-"PP_Lontue.png"
# plot_CumPrecipitation(pattern = pattern,title= title, savefile_name = savefile_name)

pattern<- "pp_armerillo_1948_presente_analisis.csv"
title<-"Estación Armerillo (1948-2015)/Maule en Armerillo (2016-2021)"
savefile_name<-"data_output/PP_Armerillo.png"
plot_CumPrecipitation(pattern = pattern,title= title, savefile_name = savefile_name)


                   