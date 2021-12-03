#Load libraries
library(tidyverse)
library(ggplot2)
library(magrittr)
library(corrgram)
library(ggpubr)
library(gridExtra)
library(forcats)
library(rsq)
library(pscl)

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


plots_for_EDA <- function(x_data) { 
  
plot_by_category <- function(x_data,x,cat)  {
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

g2 <- plot_by_category(x_data,1,2)
g3 <- plot_by_category(x_data,1,3)
g4 <- plot_by_category(x_data,1,4)
g5 <- x_data%>%
      mutate(Prestige_Score = fct_collapse(Prestige_Score_Range,"0-2 " = c("0-1 ","1-2 ")))%>%
      select(-Prestige_Score_Range )%>%
      plot_by_category(1,6)
 
grid.arrange(
  arrangeGrob(g1,arrangeGrob(g2, g3, nrow=2),ncol=2),
  arrangeGrob(g4,g5,ncol =2),
  nrow = 2,
  top = "Distribution of number of articles published split by category levels")
}
  
plots_for_EDA(pub_data_factored)

#Model1 - First Poisson
pub.glm <- glm(Articles ~ .,data = pub_data, family = poisson)
summary(pub.glm)
rsq.kl(pub.glm)

#Null and Saturated likelihood
y <-pub_data$Articles[pub_data$Articles != 0]
lnull <- sum(pub_data$Articles * log(mean(pub_data$Articles)) - mean(pub_data$Articles) - log(factorial(pub_data$Articles)))
lf <- sum(y * log(y) - y - log(factorial(y)))
2*(lf - lnull) 

#Model 2 - Check for interactions
pub.glm_interactions <- glm(Articles ~ Gender*Marital_Status + Gender*No_of_kids +
                                      Gender*Prestige_Score + Gender*Mentor_publications ,
                            data = pub_data,
                            family = poisson)
summary(pub.glm_interactions)
rsq.kl(pub.glm)
step(pub.glm_interactions)

#Model 3 - Remove non-significant terms
pub.glm_reduced <- glm(Articles ~ Gender +  No_of_kids + Mentor_publications + Gender*Prestige_Score,
                      data = pub_data,
                      family = poisson)
summary(pub.glm_reduced)
rsq.kl(pub.glm_reduced)

#Deviance check - 3 vs 2
dev2 <- deviance(pub.glm_interactions)
dev3 <- deviance(pub.glm_reduced)
1 - pchisq(dev3 - dev2,length(coefficients(pub.glm_interactions))-length(coefficients(pub.glm_reduced)))
#large p value so fail to rej Ho - accept reduced model

#Should not maybe - not sure ????
      #Model 4a - Add mentor^2 
      pub.glm_quadratic <- glm(Articles ~ Gender +  No_of_kids + Mentor_publications + I(Mentor_publications^2)  + Gender*Prestige_Score,
                             data = pub_data,
                             family = poisson)
      summary(pub.glm_quadratic)
      rsq.kl(pub.glm_quadratic)
      
      #Deviance check - 4 vs 3
      dev4 <- deviance(pub.glm_quadratic)
      1 - pchisq(dev3 - dev4,length(coefficients(pub.glm_quadratic))-length(coefficients(pub.glm_reduced)))
      #small p value so rej Ho - accept model 4

      #Model 4b - Overdispersion correction
      pub.glm_quadratic_dispersed <- glm(Articles ~ Gender +  No_of_kids + Mentor_publications + I(Mentor_publications^2)  + Gender*Prestige_Score,
                               data = pub_data,
                               family = quasipoisson)
      summary(pub.glm_dispersed)
      rsq.kl(pub.glm_dispersed)

#Final Model
pub.glm_final <- pub.glm_reduced
summary(pub.glm_final)
rsq.kl(pub.glm_final)


#phi estimate
sum(residuals(pub.glm_final, type="pearson")^2)/(nrow(pub_data)- length(coefficients(pub.glm_final)))
#Dispersion corrected model
pub.glm_final_dispersed <- glm(Articles ~ Gender + No_of_kids + Mentor_publications + Gender*Prestige_Score,
                               data = pub_data, 
                               family = quasipoisson)
summary(pub.glm_final_dispersed)
rsq.kl(pub.glm_final_dispersed)
1-pub.glm_final_dispersed$deviance/pub.glm_final_dispersed$null.deviance


#Zero count correction - Hurdle mixture model
pub.glm_zeroinf <- hurdle(Articles ~ Gender +  No_of_kids  + Gender*Prestige_Score | Mentor_publications, data = pub_data)
summary(pub.glm_zeroinf)


#Diagnostic plots
qqnorm(rstandard(pub.glm_final), pch=19, main="")
qqline(rstandard(pub.glm_final))

diagnostic_plots <- function(x_glm, x_data) {
p <- length(coefficients(x_glm))
n <- nrow(x_data)
h <- 8/(n - 2*p)

g1 <- ggplot(x_glm,aes(x = predict(x_glm,type="response"), y = rstandard(x_glm))) +
  geom_point(size = 0.75)+
  labs(title = "Residuals vs Fitted", y = "Standardized deviance residuals", x = "Fitted values")+
  geom_hline(yintercept=0,linetype="dashed")+
  theme(plot.title = element_text(hjust = 0.5,size=9.5),
        axis.title.x = element_text(size = 7.5),
        axis.title.y = element_text(size = 7.5))+
  geom_text(data = . %>% 
              mutate(label = ifelse(cooks.distance(x_glm) >5*h, 1:n, "")),
            aes(label = label), 
            hjust = 1.2,
            size = 2.5,
            show.legend = FALSE)

g2 <- ggplot(x_glm, aes(x = predict(x_glm,type="response"), y = influence(x_glm)$hat/(p/n))) +
  geom_point(size = 0.75)+
  geom_hline(yintercept=2,color="red",linetype="dashed")+
  labs(title = "Leverage vs Fitted values", y = "Leverage / (p/n)", x = "Fitted values")+
  theme(plot.title = element_text(hjust = 0.5,size=9.5),
        axis.title.x = element_text(size = 7.5),
        axis.title.y = element_text(size = 7.5))+
  geom_text(data = . %>% 
            mutate(label = ifelse(influence(x_glm)$hat/(p/n) > 10, 1:n, "")),
            aes(label = label), 
            hjust = 1.2,
            size = 2.5,
            show.legend = FALSE)

g3 <- ggplot(x_glm, aes(x = predict(x_glm,type="response"), y = cooks.distance(x_glm))) +
  geom_point(size = 0.75)+
  labs(title = "Cook's distance", y = "Cook's distance", x = "Fitted values")+
  geom_hline(yintercept=h,color="red",linetype="dashed")+
  geom_text( data = . %>% 
               mutate(label = ifelse(cooks.distance(x_glm) > 5*h,1:n, "")),
             aes(label = label), 
             hjust = 1.2,
             size = 2.5,
             show.legend = FALSE)+
  theme(plot.title = element_text(hjust = 0.5,size=9.5),
        axis.title.x = element_text(size = 7.5),
        axis.title.y = element_text(size = 7.5))


grid.arrange( g1,arrangeGrob(g2, g3, ncol=2),nrow=2,top = "Model diagnostic plots")

}

diagnostic_plots(pub.glm_final,pub_data)

#Outliers
pub_data[c(328,911,912,913,914,915),]

#Remove outliers
pub_data_outliers_remove_check <- pub_data[-c(328,911,912,913,914,915),]
pub.glm_outliers_remove_check <- glm(Articles ~ Gender + No_of_kids + Mentor_publications + Gender*Prestige_Score,
                     data = pub_data_outliers_remove_check,
                     family = poisson)
summary(pub.glm_outliers_remove_check)
diagnostic_plots(pub.glm_outliers_remove_check,pub_data_outliers_remove_check)
rsq.kl(pub.glm_outliers_remove_check)

#Interpretation tables



  


