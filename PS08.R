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
train <- read_csv("/Users/JeffLancaster/Downloads/questions.csv")
# YOOGE!
dim(train)

# knn modeling ------------------------------------------------------------
model_formula <- as.formula(DeviceId ~ X + Y + Z)

# Values to use:
n_values <- c(10, 20, 30)
k_values <- c(2, 3, 4)

runtime_dataframe <- expand.grid(n_values, k_values) %>%
  as_tibble() %>%
  rename(n=Var1, k=Var2) %>%
  mutate(runtime = n*k)
runtime_dataframe

# Time knn here -----------------------------------------------------------

set.seed(100)
summary(train)



# Plot your results ---------------------------------------------------------
# Think of creative ways to improve this barebones plot. Note: you don't have to
# necessarily use geom_point

runtime_plot <- ggplot(runtime_dataframe, aes(x=n, y=k))+geom_raster(aes(fill=runtime),interpolate=T)
#interpolating to guess at the runtime for combinations of n and k that we didn't test for ^
runtime_plot

  
# Runtime complexity ------------------------------------------------------
# Can you write out the rough Big-O runtime algorithmic complexity as a function
# of:
# -n: number of points in training set
# -k: number of neighbors to consider
# -d: number of predictors used? In this case d is fixed at 3




n_values <- c(10000, 20000, 30000, 5000, 1000, 100000, 50000, 25000, 75000)
k_values <- c(1, 3, 5, 7, 9, 11, 13, 15)
runtime<-rep(0, length(n_values)*length(k_values))
k<-rep(0,length(n_values)*length(k_values)) #storage for k values
n<-rep(0,length(n_values)*length(k_values)) #storage for n values
count=1
for (j in 1:length(k_values)){ #iterate through vals of k
  for (i in 1:length(n_values)){ #iterate through vals of n
    tic()
    m1<-caret::knn3(model_formula, data=slice(train, 1:n_values[i]), k=k_values[j]) #create model
    timer_info=toc()
    runtime[count] <- timer_info$toc - timer_info$tic
    k[count]=k_values[j] #store k-val
    n[count]=n_values[i] #store n-val
    count=count+1
  }
}
times<-data.frame(n=n, k=k, runtime=runtime)

# Plot your results ---------------------------------------------------------
# Think of creative ways to improve this barebones plot. Note: you don't have to
# necessarily use geom_point
runtime_plot <- ggplot(times, aes(x=n*k, y=runtime)) +
  geom_point() + ggtitle("Runtime by N*K") +xlab("N*K") + ylab("Runtime")
#I did not really like the colored plot. I did not think it was very helpful, so I changed the plot.


