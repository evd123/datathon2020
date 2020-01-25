library(nlme)
library(caret)
library(EnvStats)

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
min_depth_regression <- lm(rate_of_penetration ~ . -segment_id -max_depth, data = chevron_training)

#Checking for non-linearity? 
first_predictions_max <- predict(max_depth_regression, newdata = chevron_training)
first_predictions_min <- predict(min_depth_regression, newdata = chevron_training)
first_residuals_max <-chevron_training$rate_of_penetration - first_predictions_max
first_residuals_min <-chevron_training$rate_of_penetration - first_predictions_min

(ssr_max = sqrt(sum(first_residuals_max^2) /6154))

#Residual Plots
plot((first_predictions_max), first_residuals_max); abline(h = 0, lwd = 2, col = "blue")


#qQ Plots
set.seed(334455) # set seed for pseudo-random number generator 

my_plot = function(sample, my_title){
  qqnorm(sample, main = my_title); qqline(sample, lwd = 2, col = "blue")
}



#Log1p regression
max_log_regression <- lm(log1p(rate_of_penetration) ~ . -segment_id -min_depth, data = chevron_training)
bss_maxlog1p <- step(max_log_regression, direction = "backward")

maxlog1p_predictions <- predict(bss_maxlog1p, newdata = chevron_training)
maxlog1p_residuals <- chevron_training$rate_of_penetration - maxlog1p_predictions

par(mfrow = c(1, 2))
(my_plot(first_residuals_max, "first_plot"))
(my_plot(maxlog1p_residuals, "afudfhs"))

#Outliers

summary(bss_maxlog1p)


