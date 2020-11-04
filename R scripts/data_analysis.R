data_gene = gene_data
pacman::p_load(ggplot2, ggthemes, tidyr, patchwork, ggExtra)
library(tidyverse)
library(tidyr)
library(dygraphs)
library(ggplot2)
library(GGally)
library(zoo)

head(data_gene, 10)

names(gene_data) <- c( "Time", "x1","x2","y","x4","x5")
head(gene_data)
summary(gene_data)




#Time Seies using Tidyr-----------------------------------------

data_tidy_time <- gather(gene_data, Cells, TimeExpression , -Time)
data_tidy_time

time_series_graph <-
  ggplot(data_tidy_time, aes(x=Time)) +
  geom_line(aes(x= Time, y= TimeExpression, color= Cells), size=0.2, alpha=1)+
  theme_light(base_size = 16)+
  labs(title = "Time Series Plot")

time_series_graph
ggplotly(time_series_graph)
plot.ts(data)


data

#Relationship between input and output------------------------------------

qplot(gene_data$x4,gene_data$y, xlab='Input Gene X4',
      ylab='Output',
      main='Relationship between X4 and output y')

qplot(gene_data$x5,gene_data$y,xlab='Input Gene X5',
      ylab='Output',
       main='Relationship between X5 and output y')

#-----------------Correlation---------------------------------#

ggcorr(gene_data, 
       label = TRUE, 
       label_alpha = TRUE)

 ggpairs(data_gene, 
        columns = c("x1", "x2", "y", "x4","x5"), 
        upper = list(continuous = wrap("cor", size = 5)),
        lower = list(continuous = "smooth") )
 
# Correlation between input x4 and x5 and output x3----------------------

cor.test(data$x4, data$y, alternative = "two.sided", method = "pearson")
cor.test(data$x5, data$y, alternative = "two.sided", method = "pearson")


#-------------------------------PCA------------------------------------------------------------------3
pcaCharts <- function(x) {
  x.var <- x$sdev ^ 2
  x.pvar <- x.var/sum(x.var)
  print("proportions of variance:")
  print(x.pvar)
  
  par(mfrow=c(2,2))
  plot(x.pvar,xlab="Principal component", ylab="Proportion of variance explained", ylim=c(0,1), type='b')
  plot(cumsum(x.pvar),xlab="Principal component", ylab="Cumulative Proportion of variance explained", ylim=c(0,1), type='b')
  screeplot(x)
  screeplot(x,type="p")
  par(mfrow=c(1,1))
}

genePCA <- prcomp(scale(gene_data[,-1]))
summary(genePCA)

pcaCharts(genePCA)
biplot(genePCA,scale=0, cex=.8)

#-------------------------------Histogram--------------------------------------------------------#
#Add time variable to preserve the time-series structure
gene_data$Time = 1:nrow(data)
data

data_long <- (gather(data, Cells, TimeExpression ,factor_key = T))
data_long

(hists = ggplot(data_long, aes(x = TimeExpression, fill= Cells))+
    geom_histogram(bins = 30)+
    facet_wrap(~Cells, scales = "free")+
    theme_few()+
    xlab("")+
    scale_fill_few()+
    guides(fill = F)+
    theme(strip.background = element_rect(fill="white")))

#Density Plots
library("ggpubr")
ggdensity(data$x4, 
          main = "Density plot of x4",
          xlab = "Time")

ggdensity(data$x5, 
          main = "Density plot of x5",
          xlab = "Time")
ggdensity(data$y, 
          main = "Density plot of y",
          xlab = "Time")

ggqqplot(data$x4, title = "Normality Distribution for x4")
ggqqplot(data$x5, title = "Normality Distribution for x5" )
ggqqplot(data$y, title = "Normality Distribution for output y")

#------------------------------Q-Q Plots-----------------------------------------------------------------#

(qqplots = ggplot(data_long, aes(sample = TimeExpression, color = Cells))+
    stat_qq()+
    facet_wrap(~Cells, scales = "free")+
    theme_few()+
    scale_color_few()+
    guides(color = F)+
    xlab("")+
    theme(strip.text = element_blank()))

(distribution_plot = hists / qqplots)


#--------------------------------Model fitting-------------------------------------------------------#
data
X_linear = cbind(X4, X5)
thetaHat_linear = solve(t(X_linear)%*% X_linear) %*% t(X_linear) %*% data$y
print(thetaHat_linear) 
y_hat_linear = X_linear %*% thetaHat_linear
Y_hat_linear = data.frame(y_hat_linear)
y_hat_linear = Y_hat_linear$y_hat_linear

histogram_pred <- ggplot(data = Y_hat_linear, aes(y_hat_linear)) +
geom_histogram(col = "orchid", fill = "pink") + 
labs(title = "Histogram of the predictions")

ggplotly(histogram_pred)
 
MSE_linear = mean((data$y - y_hat_linear)^2) 
print (MSE_linear)

error_linear = y - y_hat_linear
plot(error_linear , y)

residuals_sum_of_squares_linear = sum((error_linear)^2)
residuals_sum_of_squares_linear

histogram_error_linear <- ggplot(data = Error, aes(error)) + 
 geom_histogram(col = "blue", fill = "red") +
 labs(title = "Histogram of the Error") 
histogram_error_linear

plot(y_hat_linear, error_linear,col=c("blue"))

