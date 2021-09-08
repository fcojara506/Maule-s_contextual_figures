library(reshape2)
library(ggplot2)
library(dplyr)
library(data.table)

setwd("~/Desktop/Maule's_contextual_figures/fsca_daily")
basin="Achibueno"

water_year <- function(date){
  library(lubridate)
  agno <- lubridate::year(date)
  mes  <- lubridate::month(date)
  wy   <- ifelse(mes > 3, agno, as.character(as.numeric(agno) - 1))
  return(wy)
}

load_data <- function(basin) {
  filename           <- paste0("data_input/TimeSeries_fsca_",toupper(basin),".csv")
  data               <- read.csv(file =filename, header = TRUE,check.names=FALSE) %>%
    transform(date = as.Date(date,tryFormats=c("%d/%m/%Y"))) %>%
    merge(data.frame(date = seq(head(as.Date(.$date),1),
                                tail(as.Date(.$date),1), by="days")),by.x='date',by.y='date',all.x=T,all.y=T) %>% 
    transform(wy=water_year(date)) %>%
    transform(value=zoo::na.fill(value,c("extend"))) %>% 
    subset(wy>2000) %>% 
    data.table %>% 
    .[,dowy := seq(1,.N),by="wy"] %>% 
    acast(dowy~wy) %>% 
    transform(mes = data.frame(mes = seq.Date(as.Date("1999-04-01"),as.Date("2000-03-31"),by="days")))%>% 
    #format(format="%m-%d")))
    #transform(mediana = apply(select(.,as.character(paste0("X",seq(2001,2020)))) , 1, median, na.rm=T)  ) %>%
    transform(promedio = apply(select(.,as.character(paste0("X",seq(2001,2020)))) , 1, mean, na.rm=T)  ) %>%
    reshape2::melt(id.vars = "mes") %>%
    transform(mes=as.Date(mes,tryFormats=c("%m-%d"))) %>% 
    transform(cat="2001-2020") %>% 
    transform(cat= replace(cat, variable == "X2016","2016")) %>% 
    transform(cat= replace(cat, variable == "X2019","2019")) %>% 
    transform(cat= replace(cat, variable == "X2021","2021")) %>% 
    #transform(cat= replace(cat, variable == "mediana","Mediana 2001/02-2020/21")) %>% 
    transform(cat= replace(cat, variable == "promedio","Promedio 2001-2020")) %>% 
    transform(cuenca=basin)
  return(data)
}


print_fsca_plot <- function(data) {
  
  
  ggplot()+
    geom_line(data = data,
              aes(x = mes, y = value, group = variable, col = cat),
              size = 0.4)+
    facet_wrap(~cuenca)+
    scale_color_manual(name = "Año hidrológico",
                       values = c("grey","springgreen3","orange", "red", "black","blue"))+
    labs(title = "Porcentaje de cobertura nival diaria (2001-2021)",
         x = "",
         y = "fsca MODIS (%)")+
    scale_x_date(date_breaks = "2 month", date_labels =  "%m")+ 
    scale_y_continuous(expand = c(0,0.01))+
    #theme_bw()+
    theme(legend.position="bottom")+
    guides(col=guide_legend(nrow=2,byrow=TRUE))
  
  save_filename= paste0("data_output/fsca_daily_2000_2021.png")
  ggsave(save_filename,width = 8,height = 5,units = "in",dpi=400)
}

basins=c("Ancoa","Achibueno","Longavi","Melado","Maule","Lontue")
data=lapply(basins,load_data) %>% data.table::rbindlist()
print_fsca_plot(data=data)

