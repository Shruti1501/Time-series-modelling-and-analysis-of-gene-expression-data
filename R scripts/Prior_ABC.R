library(tidyverse)
library(tidyr)
library(dygraphs)
library(ggplot2)
library(ggthemes)
library(plotly)
library(GGally)
library(zoo)
library(viridis)
sampling_n = 3e7 
prior_ones = runif(sampling_n, -2, 2)
prior_x4_4 = runif(sampling_n, -2, 2)
prior_x5_3 = runif(sampling_n, -2, 2)
#merge priors to one matrix so we can simply take rows as theta candidates
priors = cbind(prior_ones, prior_x4_4, prior_x5_3)
priors = as.data.frame(priors)
priors
colnames(priors) = c("ones", "x4_4", "x5_3")

#Plot priors

priors_long = gather(priors, prior, sample, ones:x5_3, factor_key = T)
(priors_plot = ggplot(priors_long, aes(x = sample, color = prior))+
        geom_density()+
        facet_wrap(~prior, scales = "free")+
        theme_few()+
        scale_color_few()+
        guides(color = F)+
        xlab(""))
priors_plot


#Create X matrix with predictors as columns
X_pred = cbind(ones, x$x4^4,x$x5^3)

posterior =data.frame(ones = numeric(),
                      x4_4 = numeric(),
                      x5_3 = numeric())

data
tolerance = 0.05
for (i in 1:nrow(priors)) {
    candidate = t(as.matrix(priors[i,]))
    simulated = X_pred %*% candidate
    MSE = mean((data$y - simulated)^2)
    if (MSE < tolerance) {
        append = t(candidate)
        colnames(append) = c("ones", "x4_4", "x5_3")
        posterior = rbind(posterior, append)
    }
}
cat(nrow(posterior), "accepted fot tolerance <= 0.05.")
write.csv(posterior, "data/posterior.csv")


#write.csv(posterior, "data/posterior.csv")
h1 = ggplot(posterior, aes(x = ones))+
    geom_density()+
    labs(x = "", y="")+
    scale_x_continuous(expand = c(0, 0),breaks = round(seq(min(posterior$ones), max(posterior$ones), by = 0.5),2))+
    theme_few()
h1
h2 = ggplot(posterior, aes(x = x4_4))+
    geom_density()+
    labs(x = "", y="")+
    scale_x_continuous(expand = c(0, 0),breaks = round(seq(min(posterior$x4_4), max(posterior$x4_4), by = 0.2),2))+
    theme_few()
h2
h3 = ggplot(posterior, aes(x = x5_3))+
    geom_density()+
    labs(x = "", y="")+
    scale_x_continuous(expand = c(0, 0), breaks = round(seq(min(posterior$x5_3), max(posterior$x5_3), by = 0.2),2))+
    theme_few()
h3

p1 = ggplot(posterior, aes(x=ones, y=x4_4) ) +
    stat_density_2d(aes(fill = ..density..), geom = "raster", contour = FALSE) +
    scale_fill_viridis() +
    scale_x_continuous(expand = c(0, 0),   breaks = round(seq(min(posterior$ones), max(posterior$ones), by = 0.2),1)) +
    scale_y_continuous(expand = c(0, 0),  breaks = round(seq(min(posterior$x4_4), max(posterior$x4_4), by = 0.2),1)) +
    theme(
        legend.position='none'
    )+
    guides(fill = F)+
    labs(x = "ones", y="x4_4")+
    theme_few()

p1

p2 = ggplot(posterior, aes(x=ones, y=x5_3) ) +
    stat_density_2d(aes(fill = ..density..), geom = "raster", contour = FALSE) +
    scale_fill_viridis() +
    scale_x_continuous(expand = c(0, 0), breaks = round(seq(min(posterior$ones), max(posterior$ones), by = 0.2),1)) +
    scale_y_continuous(expand = c(0, 0), breaks = round(seq(min(posterior$x5_3), max(posterior$x5_3), by = 0.5),2)) +
    theme(
        legend.position='none'
    )+
    guides(fill = F)+
    labs(x = "ones", y="x5_3")+
    theme_few()
p2

p3 = ggplot(posterior, aes(x=x4_4, y=x5_3) ) +
    stat_density_2d(aes(fill = ..density..), geom = "raster", contour = FALSE) +
    scale_fill_viridis() +
    scale_x_continuous(expand = c(0, 0),breaks = round(seq(min(posterior$x4_4), max(posterior$x4_4), by = 0.2),1)) +
    scale_y_continuous(expand = c(0, 0),breaks =  round(seq(min(posterior$x5_3), max(posterior$x5_3), by = 0.5),2)) +
    theme(
        legend.position='none'
    )+
    guides(fill = F)+
    labs(x = "x4_4", y="x5_3")+
    theme_few()
p3


final_plot =
    (h1+(p1+coord_flip())+(p2+coord_flip()))/
    (p1+h2+(p3+coord_flip()))/
    (p2+p3+h3)
final_plot


#Generate predictions

posterior = read.csv("data/posterior.csv", row.names = 1)
predictions = data.frame()

for (i in 1:nrow(posterior))
    {
candidate = t(as.matrix(posterior[i,]))
simulated = X_pred %*% candidate
append = data.frame(y = simulated, which = rep(i, length(simulated)))
colnames(append)[1] = "y"
predictions = rbind(predictions, append)
}


predictions$which = as.factor(predictions$which)
predictions$y = rep(data$x4, nrow(posterior))
predictions$y = rep(data$x5, nrow(posterior))
predictions$truth = rep(data$y, nrow(posterior))

predictions_plot = ggplot(predictions, aes(x = x4_4, y = y, color=which))+
geom_line(alpha = 0.2)+
geom_line(aes(y = truth), color="black")+
guides(color = F)+
theme_few()
predictions_plots
