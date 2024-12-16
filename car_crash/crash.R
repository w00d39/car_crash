library(readr)
library(tree)
library(ISLR2)
library(boot)
library(randomForest)
library(gbm)
library(ISLR)
library(MASS)
library(caret)

#tree plot 
crash <- read.csv("clean_file_final.csv")

# Split data
n <- nrow(crash)
car_split <- sample(1:n, n / 2)

trainset <- crash[car_split, ]
testset <- crash[-car_split, ]

trainset[] <- lapply(trainset, function(x) if(is.character(x)) factor(x) else x)
testset[] <- lapply(testset, function(x) if(is.character(x)) factor(x) else x)


crash_model <- tree(BODILY_INJURY ~ . - (UNIQUE_ID + COLLISION_ID + CRASH_DATE + CRASH_TIME + PERSON_ID + VEHICLE_ID), data = trainset)
summary(crash_model)
par(mar = c(5, 5, 5, 5))  # Adjust margins
plot(crash_model)
text(crash_model, cex = 0.7)  # Reduce text size


crash2_model <- tree(COMPLAINT  ~ (PERSON_SEX), data = trainset)
par(mar = c(5, 5, 5, 5))  # Adjust margins
plot(crash2_model)
text(crash2_model, cex = 0.7)  # Reduce text size

#scatterplot matrix

crash[] <- lapply(crash, function(x) if (is.character(x)) factor(x) else x)
crash[] <- lapply(crash, function(x) if (is.factor(x)) as.numeric(x) else x)


pairs(crash[, c("PERSON_TYPE", "PERSON_INJURY", "EJECTION", "EMOTIONAL_STATUS", 
                "BODILY_INJURY", "POSITION_IN_VEHICLE", "COMPLAINT", "PERSON_SEX")], 
      main = "Scatterplot Matrix", 
      col = as.numeric(crash$PERSON_SEX)) 

#correlation matrix
crash_numeric <- crash
crash_numeric[] <- lapply(crash_numeric, function(x) {
  if (is.factor(x) || is.character(x)) as.numeric(as.factor(x)) else x
})


crash_columns <- crash_numeric[, c("PERSON_TYPE", "PERSON_INJURY", "EJECTION", 
                                     "EMOTIONAL_STATUS", "BODILY_INJURY", 
                                     "POSITION_IN_VEHICLE", "COMPLAINT", "PERSON_SEX")]


crash_matrix <- cor(crash_columns, use = "complete.obs")
print(crash_matrix)

summary(crash_matrix)

#different scatterplot matrix with all variables non excluded.
pairs(crash)

#linear regression model
crash_lm <- lm(PERSON_SEX ~. - (UNIQUE_ID + PERSON_ID), crash)
summary(crash_lm)