library(tidyverse)
library(caret)

# Package for easy timing in R
library(tictoc)



# Demo of timer function --------------------------------------------------
# Run the next 5 lines at once
tic()
Sys.sleep(3)
timer_info <- toc()
runtime <- timer_info$toc - timer_info$tic
runtime

# Get data ----------------------------------------------------------------
# Accelerometer Biometric Competition Kaggle competition data
# https://www.kaggle.com/c/accelerometer-biometric-competition/data
train <- read_csv("~/Downloads/train.csv")

# YOOGE!
dim(train)



# knn modeling ------------------------------------------------------------
model_formula <- as.formula(Device ~ X + Y + Z)

# Values to use:
n_values <- c(10,50,100,500,1000,5000,10000,50000,100000,500000) #add 100000,500000
k_values <- c(2, 3, 4, 10,15,20,50,75,100)

runtime_dataframe <- expand.grid(n_values, k_values) %>%
  as_tibble() %>%
  rename(n=Var1, k=Var2) %>%
  mutate(runtime = n*k)
runtime_dataframe

# Time knn here -----------------------------------------------------------

stopwatch <- function(k,n){
  data <- sample_n(train, n)
  tic()
  model_knn <- knn3(model_formula, data=data, k = k)
  timer_info <- toc()
  time <- timer_info$toc - timer_info$tic
  return(time)
}

#tic()
system.time(function_times <- mapply(stopwatch, k = runtime_dataframe$k, n = runtime_dataframe$n)) #15.44
function_times<-mapply(stopwatch, k = runtime_dataframe$k, n = runtime_dataframe$n)
#toc()
mean1<-mean(function_times) #0.668

loop_times <- c()
system.time(for(i in k_values){for(j in n_values){loop_times <- c(loop_times, stopwatch(i,j))}}) #15.09
mean2 <- mean(loop_times) #0.608

runtimes_dataframe <- cbind(runtime_dataframe, function_times)

# Plot your results ---------------------------------------------------------
# Think of creative ways to improve this barebones plot. Note: you don't have to
# necessarily use geom_point

runtime_plot <- ggplot(runtime_dataframe, aes(x=n, y=runtime, col=factor(k))) +
  geom_line() + labs(title = "KNN Runtime", y = "runtime")

runtime_plot
ggsave(filename="Jeff_Lancaster.png", width=16, height = 9)




# Runtime complexity ------------------------------------------------------
# Can you write out the rough Big-O runtime algorithmic complexity as a function
# of:
# -n: number of points in training set
# -k: number of neighbors to consider
# -d: number of predictors used? In this case d is fixed at 3

#Confused what you are getting at here? Graph appears to say that the more bigger our splits are the faster the knn runs, which makes sense.
