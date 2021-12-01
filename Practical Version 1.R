
#Load libraries
library(tidyverse)
library(ggplot2)
library(magrittr)
library(corrgram)
library(ggpubr)

#Data load and clean up 
pub_data <- read.csv("pub.csv")

pub_data[,2] <- as.factor(pub_data[,2])
pub_data[,3] <- as.factor(pub_data[,3])
colnames(pub_data) <- c("Articles","Female","Marital_Status","No_of_kids","Prestige_Score","Mentor_publications")
str(pub_data)
attach(pub_data)


#Numerical Summary
pub_data_summary <- pub_data 
pub_data_summary[,4] <- as.factor(pub_data_summary[,4])
summary(pub_data_summary)


#Overdispersion summary
Article_summary <- data.frame(Mean = mean(pub_data_summary[,1]) ,
                                    Variance = var(pub_data_summary[,1]),
                                    Ratio = Article_count_summary[,2]/Article_count_summary[,1] )

Article_summary

#Correlation Statistics
pub_data %>% 
  mutate_if(is.factor, as.numeric) %>% 
  cor %>%
  corrgram(order=NULL, 
           lower.panel=panel.shade, 
           upper.panel=NULL, 
           label.pos = c(0.5, 0.15),
           text.panel=panel.txt, 
           labels = c("Articles","Gender","Marital_Status","Number_of_kids","Prestige","Mentor_Publications"),
           col.regions = colorRampPalette(c("#FFFFFF","#eff3ff","#c6dbef","#3182bd","#08519c"))
  )

#Logic for removing marital status
pub_data %>%
  group_by (Marital_Status, No_of_kids) %>% 
  summarize(value = length(Marital_Status))%>%
  ggplot(aes(fill= Marital_Status, y= value, x= No_of_kids)) + 
  geom_bar(position = position_dodge2(preserve = "single",padding = 0) , 
           stat="identity",
           colour = "black", 
           width = 0.75)+ 
  scale_fill_manual(values=c("#eff3ff","#9ecae1"))+
  labs(title = "Marriage vs kids",  y = "Frequency" )+
  theme(plot.title = element_text(hjust = 0.5))

#Articles vs poisson distribution
x <- seq(min(Articles), max(Articles), by = 1)
barplot(table(Articles), beside = TRUE, ylim = c(0, 0.35*nrow(pub_data)), col = "#eff3ff" )
lines(x+0.5,dpois(x,lambda=mean(Articles))*nrow(pub_data), col="#08519c",lwd = 2)
title(xlab="Number of articles", ylab="Frequency")
#why is this not 'beside' ? how to do this in ggplot?

#Model1
pub.glm <- glm(articles ~ .,data = pub_data, family = poisson)
summary(pub.glm)
)

#Archive
  
  ggplot(pub_data,aes (x= Mentor_publications, y = Articles))+
    geom_point(position = position_jitter(width = 1))+
    theme(legend.position = "top",
          legend.direction = "horizontal",
          plot.title = element_text(hjust = 0.5))+
    labs(title = "Data distribution by category levels", x = "Mentor publications", y = "Articles published" )+
    facet_grid(~Female)
  
  
  boxplot_fn <- function(x_data, x, xlab) 
  {
    col <- c("#eff3ff","#c6dbef","#9ecae1","#3182bd","#08519c")
    ggplot(pub_data, aes(x = as.factor(pub_data[,x]), y = Articles)) +
      geom_boxplot(show.legend = FALSE, fill = col[x]) + 
      labs(y = "Articles published", x = xlab) +
      scale_x_discrete()+
      theme(
        axis.title.x = element_text(size = 7.5),
        axis.title.y = element_text(size = 7.5)
      )
  }
  
  ggarrange(boxplot_fn(pub_data,2,"female"),
            boxplot_fn(swim_data,3,"married") ,
            boxplot_fn(swim_data,4,"kids")
  )