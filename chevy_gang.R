chevron <- read.csv("C:/Users/5stev/Documents/chevronChallenge/filesForStartOfDatathon/training.csv")
chevron_training <- chevron[1:6154,]
chevron_testing <- chevron[6515:6838,]

#Getting a feel for what I'm working with  - prelim reg
first_regression <- lm(rate_of_penetration ~ drillbit_size + min_depth + max_depth + surface_weight_on_bit + surface_rpm, data = chevron_training)
first_prediction <- predict(first_regression, newdata = chevron_testing)

#Now with factors
all_regression <- lm(rate_of_penetration ~ . -segment_id, data = chevron_training)

#First look at MC
first_alias <- as.data.frame(alias(all_regression))
