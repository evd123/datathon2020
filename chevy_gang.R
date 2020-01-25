library(nlme)
library(caret)
library(EnvStats)
library(pls)
library(glmnet)

chevron <- read.csv("C:/Users/5stev/Documents/chevronChallenge/filesForStartOfDatathon/training.csv")
#Making depth variable
#using caret to split data
data_partition <- createDataPartition(chevron$rate_of_penetration, list = FALSE)
chevron_training <- chevron[data_partition,]
chevron_testing <- chevron[-data_partition,]

#Getting a feel for what I'm working with  - prelim reg
first_regression <- lm(rate_of_penetration ~ drillbit_size + min_depth + max_depth + surface_weight_on_bit + surface_rpm, data = chevron_training)
first_prediction <- predict(first_regression, newdata = chevron_testing)

#Now with factors
all_regression <- lm(rate_of_penetration ~ . -segment_id, data = chevron_training)

#First look at MC
first_alias <- alias(all_regression)

#New regression with depth 
max_depth_regression <- lm(rate_of_penetration ~ . -segment_id -min_depth, data = chevron_training)

#Checking for non-linearity? 
first_predictions_max <- predict(max_depth_regression, newdata = chevron_training)
first_residuals_max <-chevron_training$rate_of_penetration - first_predictions_max

(ssr_max = sqrt(sum(first_residuals_max^2) /6154)) #RMSE 

#Residual Plots
plot((first_predictions_max), first_residuals_max); abline(h = 0, lwd = 2, col = "blue")

#qQ Plots
set.seed(334455) # set seed for pseudo-random number generator 

my_plot = function(sample, my_title){
  qqnorm(sample, main = my_title); qqline(sample, lwd = 2, col = "blue")
}

#Log regression transformation 
max_log_regression <- lm(log(rate_of_penetration) ~ . -segment_id -min_depth - area_id, data = chevron_training)
maxlog1p_predictions <- predict(max_log_regression, newdata = chev_training)
maxlog1p_residuals <- log(chevron_training$rate_of_penetration) - maxlog1p_predictions
(sqrt(sum(maxlog1p_residuals^2) / 3418))

#chev_testing <- chevron_testing[,-1]
#maxlogp_test <- predict(max_log_regression, newdata = chev_testing)
#mlogp_resid <- log(chev_testing$rate_of_penetration) - maxlog1p_test
#(sqrt(sum(maxlogp_resid^2) / 3420))

(my_plot(first_residuals_max, "first_plot"))
(my_plot(maxlog1p_residuals, "afudfhs"))

#No formation?
no_formation_regression <- lm(logp(rate_of_penetration) ~ . -segment_id -min_depth - formation_id - area_id, data = chevron_training)
nf_predictions <- predict(no_formation_regression, newdata = chevron_training)
nf_residuals <- log(chevron_training$rate_of_penetration) - nf_predictions

(sqrt(sum(nf_residuals^2) / 3418))
(my_plot(nf_residuals, "nf"))

nf_test_predictions <- predict(bss_nfr, newdata = chevron_testing)
nf_test_residuals <- log(chevron_training$rate_of_penetration) - nf_test_predictions

(sqrt(sum(nf_test_residuals^2) / 3420))


#Generalized Least Squares
gls_regression <- gls(log(rate_of_penetration) ~ . -segment_id -min_depth - formation_id - area_id, data = chevron_training, control = list(singular.ok = TRUE))
gls_wf_regression <-  gls(log(rate_of_penetration) ~ . -segment_id -min_depth  - area_id, data = chevron_training, control = list(singular.ok = TRUE))
gls_predictions <- predict(object = gls_wf_regression, newdata = chevron_training)
gls_residuals <- log(chevron_training$rate_of_penetration) - gls_predictions 
(my_plot(gls_residuals, "gls"))
