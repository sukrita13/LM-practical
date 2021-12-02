#Load libraries
library(tidyverse)
library(ggplot2)
library(magrittr)
library(corrgram)
library(ggpubr)
library(gridExtra)
library(forcats)
library(rsq)

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

#EDA Plots

plots_for_EDA <- function() { 
plot_by_category <- function(x_data,x, cat)  {
  x_data %>%
    ggplot( aes(x=x_data[,x],alpha=.25, fill=x_data[,cat])) +
    geom_histogram(aes(y=..density..), binwidth = 1, 
                   position="identity")+
    geom_density(alpha=.1, color="#08519c") +
    theme_bw()+
    theme(legend.position="none",
          panel.spacing = unit(0.1, "lines"),
          strip.text.x = element_text(size = 8)
          )+
    scale_color_brewer("Blues")+
    scale_fill_brewer("Blues")+
    labs(title = paste("By",colnames(x_data)[cat]), 
         x = "Number of articles published", 
         y = "Frequency") +
    facet_wrap(~x_data[,cat])
}

g1 <- pub_data_factored %>% 
      group_by(Mentor_publications) %>% 
      summarise(Mean = mean(Articles), No_of_students  = length(Articles)) %>% 
      ggplot(aes (x= Mentor_publications, y = Mean, size = No_of_students))+
      geom_point(alpha = 0.5, col = "#08519c")+
      ylim(0,5)+
      theme_bw()+
      theme(legend.position = "none",
            panel.background = element_blank())+
      scale_size(range = c(0.1, 15))+
      labs(title = "By number of mentor publications", 
           x = "Number of publications by mentor", 
           y = "Mean number of publications by student" )

g2 <- plot_by_category(pub_data_factored,1,2)
g3 <- plot_by_category(pub_data_factored,1,3)
g4 <- plot_by_category(pub_data_factored,1,4)
g5 <- pub_data_factored%>%
      mutate(Prestige_Score = fct_collapse(Prestige_Score_Range,"0-2 " = c("0-1 ","1-2 ")))%>%
      select(-Prestige_Score_Range )%>%
      plot_by_category(1,6)
 
grid.arrange(
  arrangeGrob(g1,arrangeGrob(g2, g3, nrow=2),ncol=2),
  arrangeGrob(g4,g5,ncol =2),
  nrow = 2,
  top = "Distribution of number of articles published split by category levels")
}
  
plots_for_EDA()

#Model1 - Natural Poisson
pub.glm <- glm(Articles ~ .,data = pub_data, family = poisson)
summary(pub.glm)
par(mfrow = c(3,2))
plot(pub.glm, which = c(1:6))
rsq.kl(pub.glm)

#Null and Saturated likelihood
y <-pub_data$Articles[pub_data$Articles != 0]
lnull <- sum(pub_data$Articles * log(mean(pub_data$Articles)) - mean(pub_data$Articles) - log(factorial(pub_data$Articles)))
lf <- sum(y * log(y) - y - log(factorial(y)))
2*(lf - lnull) 

#Deviance check - don't use for poisson 
1 - pchisq(deviance(pub.glm), 915 - 6)

#Model 2 - Remove non-significant terms
pub.glm_reduced <- glm(Articles ~ Mentor_publications + Gender +  No_of_kids,
                      data = pub_data,
                      family = poisson)
summary(pub.glm_reduced)
rsq.kl(pub.glm_reduced)

#Deviance check - 2 vs 1
dev1 <- deviance(pub.glm)
dev2 <- deviance(pub.glm_reduced)
1 - pchisq(dev2 - dev1,2)
#small p value so rej Ho

#Model 3 - Check for interactions
pub.glm_interactions <- glm(Articles ~ Gender*Mentor_publications + Gender*No_of_kids,
                       data = pub_data,
                       family = poisson)
summary(pub.glm_interactions)


#Standardized deviance residuals vs fitted values 
summary(rstandard(pub.glm_reduced))
var(rstandard(pub.glm_reduced))
plot(predict(pub.glm_reduced,
             type="response"),
     rstandard(pub.glm_reduced),
     xlab=expression(hat(mu)), ylab="Deviance Residuals",
     pch=19, col = "#08519c")


#Dispersion correction
phi_estimate <- sum(residuals(pub.glm_reduced, type="pearson")^2)/(915-4)
pub.glm_dispersed <- glm(pub_data$Articles ~ Mentor_publications + Gender +  No_of_kids,data = pub_data, family = quasipoisson)
summary(pub.glm_dispersed)
names(pub.glm_dispersed)
rsq.kl(pub.glm_dispersed)
1-pub.glm_dispersed$deviance/pub.glm_dispersed$null.deviance


#Zero count correction - Hurdle mixture model
pub.glm_zeroinf <- hurdle(Articles ~ ., data = pub_data)
summary(pub.glm_zeroinf)
deviance(pub.glm_zeroinf)
1-pub.glm_zeroinf$deviance/pub.glm_zeroinf$null.deviance



  


