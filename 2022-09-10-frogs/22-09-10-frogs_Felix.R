library(ggplot2)
library(dplyr)
library(patchwork)
library(geomtextpath)
library(data.table)

tuesdata <- tidytuesdayR::tt_load('2022-08-02')

frogs <- as.data.table(tuesdata$frogs)

colnames(frogs)[colnames(frogs)=="Water Type"] <- "WaterType"

# Hole size
hsize <- 3

frogs <- frogs %>%
  mutate(x = hsize)%>%
  mutate(label_pos=cumsum(count(Structure)))


plot_str <- frogs[,N_str:=.N,by=Structure]%>%.[,offs_str:=ifelse(Structure=="Leaf litter",-1,-2)]%>%
  ggplot(data=.)+
  #geom_col(data=frogs,aes(y=1,x=hsize,fill=Water))+
  geom_col(aes(y=N_str,x=hsize,fill=Structure))+
  geom_textpath(aes(y=N_str,x=hsize,label=Structure,vjust=offs_str),text_only = TRUE,position="stack")+
  #geom_text_repel(aes(y=label_pos,x=hsize,label=Structure),position = "stack")+

  #geom_text(aes(y=1,x=hsize,label=Structure))+
  xlim(1,hsize+0.5)+
  coord_polar(theta="y")+
  scale_fill_brewer(palette = "GnBu")+
  theme_minimal()+
  theme(panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        legend.position = "none")


plot_wat <- frogs[,N:=.N,by=Water]%>%#.[,offs:=ifelse(Structure=="Leaf litter",-3,-5)]%>%
  ggplot(data=.)+
  #geom_col(data=frogs,aes(y=1,x=hsize,fill=Water))+
  geom_col(aes(y=N,x=hsize,fill=Water))+
  geom_textpath(aes(y=N,x=hsize,label=Water,vjust= -1),text_only = TRUE,position="stack")+
  #geom_text_repel(aes(y=label_pos,x=hsize,label=Structure),position = "stack")+

  #geom_text(aes(y=1,x=hsize,label=Structure))+
  xlim(1,hsize+0.5)+
  coord_polar(theta="y")+
  scale_fill_brewer(palette = "GnBu")+
  theme_minimal()+
  theme(panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        legend.position = "none")



plot_typ <- frogs[,N:=.N,by=Type]%>%#.[,offs:=ifelse(Structure=="Leaf litter",-3,-5)]%>%
  ggplot(data=.)+
  #geom_col(data=frogs,aes(y=1,x=hsize,fill=Water))+
  geom_col(aes(y=N,x=hsize,fill=Type))+
  geom_textpath(aes(y=N,x=hsize,label=Type,vjust = -1),text_only = TRUE,position="stack")+
  #geom_text_repel(aes(y=label_pos,x=hsize,label=Structure),position = "stack")+

  #geom_text(aes(y=1,x=hsize,label=Structure))+
  xlim(1,hsize+0.5)+
  coord_polar(theta="y")+
  scale_fill_brewer(palette = "GnBu")+
  theme_minimal()+
  theme(panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        legend.position = "none")



all <- plot_str / plot_wat / plot_typ

all
