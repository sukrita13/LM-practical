library(tidyverse)
library(ggplot2)
library(magrittr)
library(corrgram)
library(ggpubr)

pub_data <- read.csv("pub.csv")
attach(pub_data)
str(pub_data)
pub_data[,2] <- as.factor(pub_data[,2])
pub_data[,3] <- as.factor(pub_data[,3])
str(pub_data)
summary(pub_data)
tab1 <- table(articles, female)
barplot(tab1, beside = TRUE)
boxplot(mwt ~ low, xlab= "low bw", ylab="mother weight")
plot(pub_data, upper_panel = NULL)

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

ggplot(pub_data,aes (x=mentor, y = articles))+
  geom_point(position = position_jitter(width = 0.1))+
  theme(legend.position = "top",
        legend.direction = "horizontal",
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))+
  labs(title = "Data distribution by category levels", x = "Mentor publications", y = "Articles published" )+
  scale_x_discrete()+
  facet_grid(~female)


boxplot_fn <- function(x_data, x, xlab) 
{
  col <- c("#eff3ff","#c6dbef","#9ecae1","#3182bd","#08519c")
  ggplot(pub_data, aes(x = as.factor(pub_data[,x]), y = articles)) +
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


pub.glm <- glm(articles ~ .,data = pub_data, family = poisson)
summary(pub.glm)
hist(pub_data$articles)
x <- 0:20
curve(dpois(lambda = mean(articles)))
mean(articles)

pub_data[,1] %>% 
  as_tibble() %>% 
  ggplot(aes(value)) + 
  geom_histogram(stat = "density") +
  stat_function(fun = function(x) {dpois(x, mean(articles)), color = "red") 
par(mfrow = c(2,1))
barplot(dpois(0:20,mean(articles)),names = 0:20)
hist(pub_data$articles,  breaks = 20, freq=FALSE)
curve(dpois(x,lambda=mean(articles)), add=TRUE,col="red")