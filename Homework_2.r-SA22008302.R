# Load packages
install.packages("ade4")
install.packages("caret")
install.packages("car")

# Load packages
library(ade4)
library(car)
library(caret)

#reading env and fish data
data(doubs)
env <- doubs$env
fish <- doubs$fish

#summarizing fish abundance data by sites
fish_sum <- apply(fish, 1, sum)
fish_df <- data.frame(Site = rownames(fish), Fish_Abundance = fish_sum)

#combining env and total fish to a new data frame named as "env_fish"
env_fish <- cbind(env, fish_df[match(rownames(env), fish_df$Site), 2])

#visualizing the features of the new env_fish set using scatterplot
names(env_fish)[12] <- "Fish_Abundance"

#Setting the x and y variables
x_vars <- c("dfs", "alt", "slo", "flo", "pH", "har", "pho", "nit", "amm", "oxy", "bdo")
y_var <- "Fish_Abundance"

#Extract the x and y variables from the data set
x_data <- env_fish[, x_vars]
y_data <- env_fish[[y_var]]

#The scatterplotMatrix() function is used to generate a scatterplot matrix
scatterplotMatrix(x_data, diagonal = list(method = "boxplot"), smooth = TRUE, 
                  regLine = TRUE, legend = TRUE,
                  var.labels = c("\n\n\nDFS", "\n\n\nALT", "\n\n\nSLO", "\n\n\nFLO", "\n\n\npH", 
                                 "\n\n\nHAR", "\n\n\nPHO", "\n\n\nNIT", "\n\n\nAMM", "\n\n\nOXY", "\n\n\nBDO"),
                  cex = 1, cex.labels = 1.5, cex.axis = 1.5,
                  pch = c(16, 16, 16), col = c("red", "green3", "blue"), row1attop = TRUE)

#Delete sites without fish
env_fish <- env_fish[rowSums(fish) > 0, ]

#Delete rows with null or outlier values
env_fish <- env_fish[complete.cases(env_fish), ]

nzv <- nearZeroVar(env_fish[, 1:11])
if (length(nzv) > 0) {
  env_fish <- env_fish[, -nzv]
}

#Detect collinearity between environment variables
cor.env <- cor(env_fish[, 1:11])
highlyCor <- findCorrelation(cor.env, cutoff = 0.75)
env_fish <- env_fish[, -highlyCor]

# Scale and center environment variables
env_fish[, 1:5] <- scale(env_fish[, 1:5], center = TRUE, scale = TRUE)

#Split the data into training sets and test sets
set.seed(123)
trainIndex <- sample(1:nrow(env_fish), 0.7*nrow(env_fish))
train <- env_fish[trainIndex, ]
test <- env_fish[-trainIndex, ]

#Keep the response variable as a continuous value
train$Fish_Abundance <- as.numeric(train$Fish_Abundance)
test$Fish_Abundance <- as.numeric(test$Fish_Abundance)

#Create a control parameter object
ctrl <- trainControl(method = "repeatedcv", 
                     number = 5, 
                     repeats = 3, 
                     summaryFunction = defaultSummary)

#Train the random forest regression model
rf_model <- train(Fish_Abundance ~ ., 
                  data = train, 
                  method = "rf",
                  metric = "RMSE", 
                  trControl = ctrl)

#Make predictions on test data
predictions <- predict(rf_model, newdata = test)

#Output prediction result
print(predictions)
print(rf_model)
