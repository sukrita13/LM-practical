#Load libraries
library(tidyverse)
library(ggplot2)
library(magrittr)
library(corrgram)
library(ggpubr)
library(gridExtra)
library(forcats)

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

pub_data_factored <- pub_data %>% 
  mutate_at("No_of_kids",as.factor)%>%
  mutate( Prestige_Score_Range= as.factor(paste(ceiling(Prestige_Score)-1,"-",ceiling(Prestige_Score)," ",sep = "")) )%>%
  select(-Prestige_Score)

summary(pub_data_factored)

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

#Visualization for categorical data
plot_by_category <- function(x_data,x, cat)  {
  x_data %>%
    ggplot( aes(x=x_data[,x], color=x_data[,cat],alpha=.25, fill=x_data[,cat])) +
    geom_histogram(aes(y=..density..), binwidth = 1, 
                   position="identity")+
    geom_density(alpha=.1) +
    theme(
      legend.position="none",
      panel.spacing = unit(0.1, "lines"),
      strip.text.x = element_text(size = 8)
    ) +
    #scale_color_brewer("Blues")+
    #scale_fill_brewer("Blues")+
    xlab(paste("By",colnames(x_data)[cat])) +
    ylab("")+
    facet_wrap(~x_data[,cat])
}

grid.arrange(
  plot_by_category(pub_data_factored,1,2), 
  plot_by_category(pub_data_factored,1,4),
  plot_by_category(pub_data_factored,1,3), 
              pub_data_factored%>%
                mutate(Prestige_Score = fct_collapse(Prestige_Score_Range,"0-2 " = c("0-1 ","1-2 ")))%>%
                select(-Prestige_Score_Range )%>%
                plot_by_category(1,6),
  ncol =2,
  top = "Distribution of number of articles published split by category levels")

#Visualization for numerical data



#Model1
pub.glm <- glm(pub_data$Articles ~ .,data = pub_data, family = poisson)
summary(pub.glm)

#Archive
pub_data %>% group_by(Articles, Gender, Mentor_publications, Marital_Status) %>% summarise(value = length(Articles)) %>% 
  ggplot(aes (x= Mentor_publications, y = Articles, size = value, col = Marital_Status))+
    geom_point(alpha = 0.5)+
    theme(legend.position = "top",
          legend.direction = "horizontal",
          plot.title = element_text(hjust = 0.5))+
    labs(title = "Data distribution by category levels", x = "Mentor publications", y = "Articles published" )+
    facet_grid(~Gender)
  
  
  boxplot_fn <- function(x_data, x, xlab) 
  {
    col <- c("#eff3ff","#c6dbef","#9ecae1","#3182bd","#08519c")
    ggplot(pub_data, aes(x = as.factor(pub_data[,x]), y = (Articles))) +
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

  


