library(dplyr)
library(readr)
library(reshape2)
library(ggplot2)
#library(forcats)
library(data.table)

rm(list = ls())
setwd("~/Desktop/Maule's_contextual_figures/pp_acum_monthly")
filename="Ancoa_en_el_Morro.csv"
load_data <- function(filename) {
  print(filename)
  meses         <-  paste0("X",sprintf("%02d", c(4:12,1:3)))
  station_name  <- filename %>% sub(".csv","",.) %>% gsub("_"," ",.)
  data          <- read.csv(file =  paste0("data_input/",filename) )
  
  df            <- rbind(data,c(-9999, (data %>% select(meses) %>% head(-1) %>% apply(2,mean)))) %>%
    transform(WY = replace(WY, WY == -9999,"promedio")) %>%
    transform(cat = "Rango histórico") %>% #paste0(gsub("-","/",WY[[1]])," - 2020/21")) %>% 
    transform(cat = replace(cat, WY == "2016-17","2016/17")) %>%
    transform(cat = replace(cat, WY == "2019-20","2019/20")) %>% 
    transform(cat = replace(cat, WY == "2021-22","2021/22")) %>% 
    transform(cat = replace(cat, WY == "promedio", paste0("Promedio "))) %>% 
    reshape2::melt(id.vars=c("WY","cat")) %>%
    transform(name=station_name) %>% 
    transform(variable=sub("X","",variable) ) 
    
    
  
  return(df)
}

plot_CumPrecipitation=function(df){
  
  level_order <- factor(df$variable, level =sprintf("%02d", c(4:12,1:3)))
  level_cat  <- factor(df$cat)
  
  ggplot()+
    geom_line(data = df,aes(x=level_order,y=value,group=WY,color=level_cat),size=0.7)+ #todas las lineas
    #geom_line(data = df[df$WY=="1998-99",],aes(x=factor(variable),y=value,group=WY,color=cat),size=1.2)+
    scale_color_manual(name="Año Hidrológico",values = c("springgreen3", "orange", "red","black", "grey"))+
    geom_line(data = df[df$cat!="rango histórico",],aes(x=level_order,y=value,group=WY,color=level_cat),size=0.7)+ #lineas más importantes
    facet_wrap(~name,scales = "free_y")+
    labs( title= "Precipitación acumulada mensual en estaciones meteorológicas",
          x="",
          y="Precipitación (mm)")+
    scale_x_discrete(expand = c(0,0.01),breaks=sprintf("%02d", c(5,7,9,11,1,3)))+
    scale_y_continuous(expand = c(0,0.01))+
    theme(legend.position="bottom")+
    guides(col=guide_legend(nrow=2,byrow=TRUE))
  
  savefile_name="data_output/pp_acum_todas_estaciones.png"
  ggsave(savefile_name,width = 8,height = 5,units = "in",dpi=400)
}
#__________________________MAIN__________________________________________


filenames=list.files(path = "data_input")

df=lapply(filenames, load_data) %>% rbindlist()
plot_CumPrecipitation(df=df)

