library(nlme)

chevron <- read.csv("C:/Users/5stev/Documents/chevronChallenge/filesForStartOfDatathon/training.csv")
#Making depth variable
chevron_training <- chevron[1:6154,]
chevron_testing <- chevron[6515:6838,]

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

#Residual Plots
plot(first_predictions_max, first_residuals_max); abline(h = 0, lwd = 2, col = "blue")


#qQ Plots
set.seed(334455) # set seed for pseudo-random number generator 

my_plot = function(sample, my_title){
  qqnorm(sample, main = my_title); qqline(sample, lwd = 2, col = "blue")
}

(my_plot(first_residuals_max, "first_plot"))


