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
n_values <- c(500,1000,5000,10000,50000,100000,500000,1000000,5000000,10000000) #add 100000,500000
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
system.time(function_times <- mapply(stopwatch, k = runtime_dataframe$k, n = runtime_dataframe$n)) 
function_times<-mapply(stopwatch, k = runtime_dataframe$k, n = runtime_dataframe$n)#252.06
#toc()
mean1<-mean(function_times) #2.0785

loop_times <- c()
system.time(for(i in k_values){for(j in n_values){loop_times <- c(loop_times, stopwatch(i,j))}}) #246.364
mean2 <- mean(loop_times) #1.9927

runtime_dataframe <- cbind(runtime_dataframe, function_times)

# Plot your results ---------------------------------------------------------
# Think of creative ways to improve this barebones plot. Note: you don't have to
# necessarily use geom_point

runtime_plot <- ggplot(runtime_dataframe, aes(x=n, y=runtime, col=as.factor(k))) +
  geom_line() + labs(title = "KNN Runtime", y = "runtime")+theme_classic()

runtime_plot
ggsave(filename="Jeff_Lancaster.png", width=16, height = 9)




# Runtime complexity ------------------------------------------------------
# Can you write out the rough Big-O runtime algorithmic complexity as a function
# of:
# -n: number of points in training set
# -k: number of neighbors to consider
# -d: number of predictors used? In this case d is fixed at 3

#For n: As n increases, the Runtime of the function increases somewhere along the lines of O(nlog(n)) or something  positive like that.
a<-runtime_dataframe %>% 
  filter(k==100)
ggplot(a,aes(x=n,runtime))+geom_point()
#As we can see above, for k=100, the line has a positive slope (notice how the 1x10^7 point corresponds to 1x10^9 on the y-axis)

#For k: As k increases, runtime increases significantly. I'm guessing at least O(n^2), possibly even O(n^3).

#For d: D is fixed in our case, but if I had to guess, it would be O(n)


#For all 3 combined, we can see that the overall KNN for (n,k,d) 
#is something more than O(n) and something less than O(n^2), so I'm 
#going to guess that the overal runtime is something like 
#Runtime = k/n+d
                                                                    