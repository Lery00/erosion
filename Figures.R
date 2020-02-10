library(ggupset)
library(tidyverse)
library(readxl)
library(ggplot2)
library(tidyverse)
library(ggupset)
library(dplyr)     # for %>%, inner_join(), arrange(), group_by, summarize()
library(reshape2)
library(gghalves)
load("datasets_output/datasets_article.rda")


id.vars <- c("ids","Article","Publication Year")
#---------------------------------------------------------------
data <- melt(datasets[,c(1:3,5:15)],
             id.vars= id.vars,
             variable.name = "Erosion",
             value.name = "count" 
)

data <- subset(data,count=="1" )
data$Erosion <- as.character(data$Erosion)

fig1 <- data %>% 
  group_by(ids,Article,`Publication Year`) %>%
  summarize(Erosion = list(Erosion)) %>%
  ggplot(aes(x=Erosion,y=`Publication Year`)) +
  geom_half_boxplot(position = position_nudge(x=.6),width=.6,fill="gray")+
  #geom_violin(fill = "grey80", colour = "black",scale = "width") +
  geom_jitter(width = 0.1)+
  theme_classic()+
  scale_x_upset(order_by = "degree")+
  ylim(1997, 2020)+
  xlab("Erosion type")+
  ylab("Publication Year") + 
  theme(axis.text = element_text(colour = "black"))


library(ggVennDiagram)
library(ggplot2)
library(ggplotify)
library(venn)
load("~/Desktop/R_codes/datasets_output/datasets_article.rda")
x1 <- list(Global=which(datasets[,16]>0),
           Continental=which(datasets[,17]>0),
           Regional=which(datasets[,18]>0),
           Watershed=which(datasets[,19]>0),
           Hillslope=which(datasets[,20]>0))

#fig2 <- as.ggplot(~venn.diagram(x1,filename = "fig2.tiff",margin = 0.1
#                                fill = c("cornflowerblue", "green", "gray","yellow", "darkorchid1")))

fig2 <- as.ggplot(~venn(x1,zcolor = "style",box = FALSE,sncs = 0.8))

x2 <- list(Tropical=which(datasets[,21]>0),
           Subtropical=which(datasets[,22]>0),
           Warm=which(datasets[,23]>0),
           Semiarid=which(datasets[,24]>0),
           Arid=which(datasets[,25]>0),
           Semihumid=which(datasets[,26]>0),
           Humid=which(datasets[,27]>0))

fig3 <- as.ggplot(~venn(x2,zcolor = "style",box = FALSE,sncs = 0.8))
#-----------------------------------------------
x3 <- list(Modeling=which(datasets[,28]>0),
           "Controled experiment"=which(datasets[,29]>0),
           Isotope=which(datasets[,30]>0),
           "3S"=which(datasets[,31]>0),
           Geostatistical=which(datasets[,32]>0),
           Magnetic=which(datasets[,33]>0))



fig4 <- as.ggplot(~venn(x3,zcolor = "style",box = FALSE,sncs = 0.8))
  

library(patchwork)

figure <- (fig1/(fig2|fig4|fig3)) +
  plot_annotation(tag_levels = 'A')+
  plot_layout(heights = c(0.8, 1))
figure
ggsave("figure.pdf",figure,
       width = 21,
       height =21,
       units = "cm",
       dpi = 600)
#--------------------------------------------------------------