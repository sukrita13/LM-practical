
#Load libraries
library(tidyverse)
library(ggplot2)
library(magrittr)
library(corrgram)

#Data load and summary 
pub_data <- as.data.frame(readr::read_csv("pub.csv",
                            skip =1,
                            col_names = c("Articles","Gender","Marital_Status",
                                          "No_of_kids","Prestige_Score","Mentor_publications"),
                            col_types = "iffidi"))
levels(pub_data$Gender) <- c("Male","Female")
levels(pub_data$Marital_Status) <- c("Married","Unmarried")
pub_data$Marital_Status <- fct_relevel(pub_data$Marital_Status, c("Unmarried","Married"))
str(pub_data)

pub_data %>% mutate_at("No_of_kids",as.factor)%>%summary

#Overdispersion summary
c(Mean = mean(pub_data[,1]) ,
          Variance = var(pub_data[,1]),
          Ratio = var(pub_data[,1])/mean(pub_data[,1]) )


#Correlation Statistic
pub_data %>% 
  mutate_if(is.factor, as.numeric) %>% 
  cor %>% 
  corrgram(order=NULL, 
           lower.panel=panel.shade, 
           upper.panel= panel.cor,
           cex.cor = 3,
           text.panel=panel.txt, 
           col.regions = colorRampPalette(c("#08519c","#3182bd","#eff3ff","#3182bd","#08519c")))

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
x <- seq(min(pub_data$Articles), max(pub_data$Articles), by = 1)
barplot(table(pub_data$Articles), beside = TRUE, ylim = c(0, 0.35*nrow(pub_data)), col = "#eff3ff" )
lines(x+0.5,dpois(x,lambda=mean(pub_data$Articles))*nrow(pub_data), col="#08519c",lwd = 2)
title(xlab="Number of articles", ylab="Frequency")
#why is this not 'beside' ? how to do this in ggplot?


#Model1
pub.glm <- glm(pub_data$Articles ~ .,data = pub_data, family = poisson)
summary(pub.glm)

#Archive
  
  ggplot(pub_data,aes (x= Prestige_Score, y = Articles, col = No_of_kids))+
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
  
detach(pub_data)