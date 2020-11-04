data_gene = gene_data
head(data_gene, 10)
library(tidyverse)
library(tidyr)
library(dygraphs)
library(ggplot2)
library(GGally)
library(ggpubr)
library(zoo)


names(gene_data) <- c( "Time", "x1","x2","y","x4","x5")
head(gene_data)
summary(gene_data)
input_timeseries <- gene_data %>% select(x4,x5,y)
output <- cbind(gene_data$y)
output


x = cbind(gene_data %>% select(x4,x5))


y <- cbind(gene_data$y)

data = cbind(x,y)



#--------------------------------------Candidate Matrix X--------------------------------
X4 = data_gene$x4
X5 = data_gene$x5

ones = matrix(1 , length(X4),1)
X = cbind(ones,X4, X4^2, X4^3, X4^4, X5, X5^2, X5^3, X5^4)
X
colnames(X) = c("ones","X4", "X4^2", "X4^3", "X4^4", "X5", "X5^2", "X5^3", "X5^4")
y 

#----------------------------------Model Selection------------------------------------------------------------------#
set.seed(123)
n = dim(X)[1]

n_train = 0.8 * n
X_train = X[1:n_train, ]
X_train
y_train = as.matrix(y[1:n_train, ])
y_train
X_test = X[(n_train+1):n, ]
y_test = as.matrix(y[(n_train+1):n, ])



MSE_table = data.frame(terms = colnames(X), MSE = rep(0, length(9)))
MSE_table

#Create a for loop to find the minimum MSE Values
for (i in 1:9){
  thetaHat = solve(t(X_train[,i]) %*% X_train[,i]) %*% t(X_train[,i]) %*% y_train
  y_hat = X_test[,i] %*% thetaHat

  error = y_test - y_hat
  MSE = mean(error^2)
  MSE_table[i, 2] = MSE 
}
MSE_table

new_term1 = MSE_table$terms[which.min(MSE_table$MSE)]
new_term1

new_train1 = as.matrix(X_train[, which.min(MSE_table$MSE)])
new_train1
new_test1 = as.matrix(X_test[, which.min(MSE_table$MSE)])

X_train1 = X_train[, - which.min(MSE_table$MSE)]
X_test1 = X_test[, - which.min(MSE_table$MSE)]

MSE_table1 = data.frame(terms = colnames(X_train1), MSE = rep(0, length(8))) 
MSE_table1



for (i in 1:8){
  train_x1 = cbind(new_train1, X_train1[, i])
  test_x1 = cbind(new_test1, X_test1[, i])
  thetaHat = solve(t(train_x1) %*% train_x1) %*% t(train_x1) %*% y_train
  y_hat = test_x1 %*% thetaHat

  error = y_test - y_hat
  MSE1 = mean(error^2)
  MSE_table1[i, 2] = MSE1 
}
#print mse_table1
MSE_table1

new_term2 = MSE_table1$terms[which.min(MSE_table1$MSE)]
new_term2


new_train2 = as.matrix( X_train1[, which.min(MSE_table1$MSE)])
new_train2
new_test2 = as.matrix(X_test1[, which.min(MSE_table1$MSE)])
new_test2

X_train2 = X_train1[, - which.min(MSE_table1$MSE)]
X_test2 = X_test1[, - which.min(MSE_table1$MSE)]

MSE_table2 = data.frame(terms = colnames(X_train2), MSE = rep(0, length(7))) 
MSE_table2

for (i in 1:7){
  train_x2 = cbind(new_train1,new_train2,X_train2[, i])
  test_x2 = cbind(new_test1,new_test2, X_test2[, i])
  thetaHat = solve(t(train_x2) %*% train_x2) %*% t(train_x2) %*% y_train
  y_hat = test_x2 %*% thetaHat
  #Finding the errors
  error = y_test - y_hat
  MSE2 = mean(error^2)
  MSE_table2[i, 2] = MSE2 
}
MSE_table2

#New term 3
new_term3 = MSE_table2$terms[which.min(MSE_table2$MSE)]
new_term3


new_train3 = as.matrix(X_train2[, which.min(MSE_table2$MSE)])
new_test3 = as.matrix(X_test2[, which.min(MSE_table2$MSE)])


#Final model

xfinal_train = cbind(new_train3, new_train2, new_train1)
xfinal_test = cbind(new_test3, new_test2, new_test1)
colnames(xfinal_train) = c("ones", "X4_4", "X5_3")
colnames(xfinal_test) = c("ones", "X4_4", "X5_3")
final_thetaHat = solve(t(xfinal_train) %*% xfinal_train) %*% t(xfinal_train) %*% y_train
y_hat_final = xfinal_train %*% final_thetaHat


error = y_train - y_hat_final
residuals = y[1:240]- y_hat_final
residuals


error
MSE_final = mean(error^2)
MSE_final
Error = data.frame(error)
error = Error$error

Residuals = data.frame(residuals)
residuals = Residuals$residuals

sse = norm(error , type = "2")^2
print(sse)


residual_plot <- ggplot(data = Residuals, aes(residuals)) +
  geom_histogram(col = "orchid", fill = "light blue") +
  labs(title = "Residuals Histogram ")+
  geom_vline(data=Residuals, aes(xintercept = mean(residuals)), colour= ("black"),show.legend = TRUE,size=2) +
  geom_vline(data=Residuals, aes(xintercept = median(residuals)), colour=("red"),linetype="dashed",size=2)+
  ggdensity(Residuals$residuals, main = "Density plot of residuals", xlab = "Time") +
  ggqqplot(Residuals$residuals, title = "Normality Distribution")

residual_plot

final_thetaHat


#-----------------------------------Error/SSE/R-Squared-------------------------------------------#

residuals_sum_of_squares = sum((error)^2) 
sigma_2 = sse/( n - 1 )
residuals_sum_of_squares

#-----------------------------------covariance matrix----------------------------------------------#

cov_thetahat =  1 * (solve(t(xfinal_train) %*% xfinal_train)) 
colnames(cov_thetahat) = c("one_theta", "two_theta", "three_theta")
rownames(cov_thetahat) = c("one_theta", "two_theta", "three_theta")
cov_thetaHat_inv = (t(X) %*% X) * (1/sigma_2) 
det_cov_thetaHat = det(cov_thetahat)
cov_thetahat
det_cov_thetaHat
#------------------------------------------Contour--------------------------------------------#
final_thetaHat

num_param = 3
num_points = 20 # no point on the plot
one_theta = seq(0.053 , 0.063, length=num_points)
two_theta = seq(-0.070 , -0.060, length=num_points)
three_theta = seq(0.250, 0.350, length=num_points)
prob_den_func = matrix(0 , num_points , num_points)
cov_thetaHat_inv = (t(X) %*% X) * (1/sigma_2) # inverse of cov_thetaHat
cov_thetaHat_inv
det_cov_thetahat = det(cov_thetahat) # determinent of cov_thetaHat
#theta_1 and theta_2 combination
for(r in 1:20){
  for(c in 1:20){
    one_two_theta = matrix( c(one_theta[r] , two_theta[c], final_thetaHat[3,] ) , num_param , 1)
    thetahat_theta = one_two_theta - final_thetaHat 
    
    prob_den_func[r,c] = ( 1/sqrt( ( (2*pi)^num_param ) * det_cov_thetahat) ) *
      exp( -0.5 * t(-thetahat_theta) %*% solve(cov_thetahat) %*% -thetahat_theta)
  }
}

c1 = contour(one_theta, two_theta,prob_den_func)
p1 = persp(one_theta, two_theta, prob_den_func, theta = 50 , phi = 40)

library(plotly)

fig <- plot_ly(z = ~p1) %>% add_surface(
  contours = list(
    z = list(
      show=TRUE,
      usecolormap=TRUE,
      highlightcolor="#ff0000",
      project=list(z=TRUE)
    )
  )
)
fig <- fig %>% layout(
  scene = list(
    camera=list(
      eye = list(x=1.87, y=0.88, z=-0.64)
    )
  )
)
fig

#theta_2 and theta_3 combination
for(r in 1:20){
  for(c in 1:20){
    two_three_theta = matrix( c(two_theta[r] , three_theta[c], final_thetaHat[2,] ) , num_param , 1)
    thetahat_theta = two_three_theta - final_thetaHat
    
    prob_den_func[r,c] = ( 1/sqrt( ( (2*pi)^num_param ) * det_cov_thetahat) ) *
      exp( -0.5 * t(-thetahat_theta) %*% solve(cov_thetahat) %*% -thetahat_theta)
  }
}

contour(two_theta, three_theta,prob_den_func)
p2 = persp(two_theta, three_theta, prob_den_func, theta = 60 , phi = 30)


fig2 <- plot_ly(z = ~p2) %>% add_surface(
  contours = list(
    z = list(
      show=TRUE,
      usecolormap=TRUE,
      highlightcolor="#ff0000",
      project=list(z=TRUE)
    )
  )
)
fig2 <- fig2 %>% layout(
  scene = list(
    camera=list(
      eye = list(x=1.87, y=0.88, z=-0.64)
    )
  )
)
fig2



#theta_3 and theta_1 combination
for(r in 1:20){
  for(c in 1:20){
    one_three_theta = matrix( c(one_theta[r] , three_theta[c], final_thetaHat[1,] ) , num_param , 1)
    thetahat_theta = one_three_theta - final_thetaHat
    
    prob_den_func[r,c] = ( 1/sqrt( ( (2*pi)^num_param ) * det_cov_thetahat) ) *
      exp( -0.5 * t(-thetahat_theta) %*% solve(cov_thetahat) %*% -thetahat_theta)
  }
}

c = contour(one_theta, three_theta,prob_den_func)
p3 =  persp(one_theta, three_theta, prob_den_func, theta = 40 , phi = 10)

fig3 <- plot_ly(z = ~p3) %>% add_surface(
  contours = list(
    z = list(
      show=TRUE,
      usecolormap=TRUE,
      highlightcolor="#ff0000",
      project=list(z=TRUE)
    )
  )
)
fig3 <- fig3 %>% layout(
  scene = list(
    camera=list(
      eye = list(x=1.87, y=0.88, z=-0.64)
    )
  )
)
fig3


#-------------------------------- Confidence Interval---------------------------------------------------#

t=240
var_y_hat = matrix(0 , t, 1)
var_y_hat

for( i in 1:t){
  X_i = matrix( xfinal_train[i,] , 1 , 3) 
  var_y_hat[i,1] = X_i %*% cov_thetahat %*% t(X_i) 
}

CI = 2 * sqrt(var_y_hat) # Confidance interval
CI


plot(x$x4[1:240], y_hat_final , type = "p")
segments(x$x4[1:240], y_hat_final-CI,x$x4[1:240], y_hat_final+CI, col = "red")


plot(x$x5[1:240], y_hat_final , type = "p")
segments(x$x5[1:240], y_hat_final-CI,x$x5[1:240], y_hat_final+CI, col = "green")

