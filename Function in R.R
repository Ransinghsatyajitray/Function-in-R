# Benefit of writing functions:

#Functions eliminate repetitions from your code which
#> can reduce your workload and
#> help avoid errors

#Functions also allows code reuse and sharing

#Converting scripts into functions

#my_fun <- function(arg1,arg2){  # This is called signature
#     body
#}   



#Below we have two similar codes one for geography and one for english

library(readr)
test_scores_geography <- read_csv("test_scores_geography.csv")
library(dplyr)
library(lubridate)
test_scores_geography_clean <- test_scores_geography %>% select(person_id,first_name,last_name,score) %>% 
  mutate(test_date=mdy(test_date)) %>% filter(!is.na(score))



library(readr)
test_scores_english <- read_csv("test_scores_geography.csv")
library(dplyr)
library(lubridate)
test_scores_english_clean <- test_scores_english %>% select(person_id,first_name,last_name,score) %>% 
  mutate(test_date=mdy(test_date)) %>% filter(!is.na(score))



#Step 1:
# Make a template:

import_test_scores <- function(){
  
  
}


# Step 2:
# Paste the script into the body
import_test_scores <- function(){
  test_scores_english <- read_csv("test_scores_geography.csv")
  test_scores_english_clean <- test_scores_english %>% select(person_id,first_name,last_name,score) %>% 
    mutate(test_date=mdy(test_date)) %>% filter(!is.na(score))
}


# Step 3:
# Choose the argument:
import_test_scores <- function(filename){
  test_scores_english <- read_csv("test_scores_geography.csv")
  test_scores_english_clean <- test_scores_english %>% select(person_id,first_name,last_name,score) %>% 
    mutate(test_date=mdy(test_date)) %>% filter(!is.na(score))
}



# Step 4:
# Replace the specific values with the arguments
import_test_scores <- function(filename){
  test_scores_english <- read_csv(filename) #<----
  test_scores_english_clean <- test_scores_english %>% select(person_id,first_name,last_name,score) %>% 
    mutate(test_date=mdy(test_date)) %>% filter(!is.na(score))
}



# Step 5:
# Generalize the variable names
import_test_scores <- function(filename){
  test_scores_raw <- read_csv(filename) #<---- variables name generalised
  test_scores_clean <- test_scores_raw %>%    #<---- variables name generalised
    select(person_id,first_name,last_name,score) %>% 
    mutate(test_date=mdy(test_date)) %>% filter(!is.na(score))
}


# Step 6:
# Remove the final assignment (Funtion in r return the last value that is calculated in the body
# so no need of assignment)

import_test_scores <- function(filename){
  test_scores_raw <- read_csv(filename) #<---- variables name generalised
  
  
  test_scores_raw %>%    #<---- variables name generalised
    select(person_id,first_name,last_name,score) %>% 
    mutate(test_date=mdy(test_date)) %>% filter(!is.na(score))
}




#Example 1:
# Coin toss example:
# Update the function to return n_flips coin tosses
toss_coin <- function(n_flips) {
  coin_sides <- c("head", "tail")
  sample(coin_sides, n_flips, replace = TRUE)  #Sample function choose from coin_sides
}

# Generate 10 coin tosses
toss_coin(10)



#Example 2:
# Update the function so heads have probability p_head
toss_coin <- function(n_flips, p_head) {
  coin_sides <- c("head", "tail")
  # Define a vector of weights
  weights <- c(p_head, 1 - p_head)
  # Modify the sampling to be weighted 
  sample(coin_sides, n_flips, replace = TRUE, prob = weights)
}

# Generate 10 coin tosses
toss_coin(10, p_head = 0.8)




# Function -> verb
# understanding code >> typing code

# Types of data arguments
# Data argument: what you compute on
# Detail argument: how you perform the computation





# R's generalized linear regression function, glm(), suffers the same usability problems as lm(): 
# its name is an acronym, and its formula and data arguments are in the wrong order.



# From previous step
run_poisson_regression <- function(data, formula) {
  glm(formula, data, family = poisson)
}

# Re-run the Poisson regression, using your function
model <- snake_river_visits %>%
  run_poisson_regression(n_visits ~ gender + income + travel)

# Run this to see the predictions
snake_river_explanatory %>%
  mutate(predicted_n_visits = predict(model, ., type = "response"))%>%
  arrange(desc(predicted_n_visits))


# Default arguments----
# they are written in function head in signature

toss_coin <- function(n_flips, p_head=0.5){
  coin_sides <- c("head","tail")
  weights <- c(p_head,1-p_head)
  sample(coin_sides,n_flips,replace=TRUE,prob=weights)
}


#Null default and Categorical default

args(prop.test)

#function (x, n, p = NULL, alternative = c("two.sided", "less", 
#"greater"), conf.level = 0.95, correct = TRUE) 

# Process of converting numerical variables to categorical variable is called cutting





# Example:
# A numeric vector of the number of visits to Snake River is provided as n_visits

# cut_by_quantile() converts a numeric vector into a categorical variable where quantiles 
# define the cut points. This is a useful function, but at the moment you have to specify 
# five arguments to make it work. This is too much thinking and typing.


# Set the default for n to 5
cut_by_quantile <- function(x, n = 5, na.rm, labels, interval_type) { #interval type is for boundary
  probs <- seq(0, 1, length.out = n + 1)   # Partitions the data from 0 to 1
  qtiles <- quantile(x, probs, na.rm = na.rm, names = FALSE) # x is a numeric vector, quantile finds the values
  right <- switch(interval_type, "(lo, hi]" = TRUE, "[lo, hi)" = FALSE) # TRUE means on FALSE means off, logical, indicating if the intervals should be closed on the right (and open on the left) or vice versa.
  cut(x, qtiles, labels = labels, right = right, include.lowest = TRUE) # this is what we are evaluating, it convert numeric to factor
}




# Remove the n argument from the call
cut_by_quantile(
  n_visits,
  na.rm = FALSE, 
  labels = c("very low", "low", "medium", "high", "very high"),
  interval_type = "(lo, hi]" # number to left of a break or right of break
)


# n_visit  (A numeric vector of the number of visits to Snake River )
# [1]   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
# [19]   0  12 100  35   1   6   2   1   1   1   1   1   1 100  80 104  55 350
# [37]  20  60 250 100  50  40   9 200 200 100   8   6   2  15  12  30 120  52
# [55]  35  30  75  10 250  15   4  25  50 114  50 100  15  30 120  30   0 160
# [73]  12  25   3  15  14  15   8 125  96 260  25  30  30   1  50   6  12  72
# [91]  20  25  50  30   1   1   5   1   3   6  50  10   9   4  12   2  15  50
# [109]   7 100  10  50   2  50   2 100  30   1   1   1   1   2   1   1   1   1
# [127]   1   1   4   1   1   2   2   1   1   2   1   1   1   2   2  10   3   3
# [145]   4   5   1   1   2   1   2   6   1   1   1   1 200  13 150  25  10  40
# [163]  10   1  30   6  35  24 100  17  20  40  52  15  60  30  20   6  70  35
# [181]  30  24 300 100   0  30  50  26  17   6 100  30  50  20 150  20  70  20
# [199] 100   1  20 100  50   0   1  10  60  10   2   7  24   6   0   4  90  20
# [217]  75  12  70  22   2  20   3  52   5  25  25  30   1  12  20  20  50  25
# [235]   3   3   1   1   1   1   7   2   1   0   1   3  15   1   1   3   1   1
# [253]   1   1   1   1   1   1   1   1 150   5   0 150   3  40   5   2  50   6
# [271]   6   6   2  25   6  25  50   3 150  60   3   4  50  14   4  60  30   3
# [289]   2   1  10   1   1   1   1   1   2   1   1   1   1   1   1   1   1   2
# [307]   1   1   1   1   2   1   1   2   1   1   1   1   2   1   1   1   1   1
# [325]   2   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1
# [343]   1   1   1   8   1   1   1   1   1   1   1   1   1   1   1   1   2   1
# [361]   1   1   1   1   1   1   1   1   1   1   2   1   0   1  50  30  40 208
# [379]  50  20 150  50  80  75   6  10   6  26  60  30  30  15  12  30  20  20
# [397] 120  15  75   4  35   2  30  76   2   1   3   1   3   2


# Logical defaults----
# cut_by_quantile() is now slightly easier to use, but you still always have to specify the na.rm argument. This removes
# missing values - it behaves the same as the na.rm argument to mean() or sd().

# Set the default for na.rm to FALSE
cut_by_quantile <- function(x, n = 5, na.rm = FALSE, labels, interval_type) { # na.rm = FALS
  probs <- seq(0, 1, length.out = n + 1)
  qtiles <- quantile(x, probs, na.rm = na.rm, names = FALSE)
  right <- switch(interval_type, "(lo, hi]" = TRUE, "[lo, hi)" = FALSE)
  cut(x, qtiles, labels = labels, right = right, include.lowest = TRUE)
}

# Remove the na.rm argument from the call
cut_by_quantile(
  n_visits,
  labels = c("very low", "low", "medium", "high", "very high"),
  interval_type = "(lo, hi]"
)


# Where functions have an argument for removing missing values, the best practice is to not remove them by default (in case you hadn't spotted 
# that you had missing values). That means that the default for na.rm should be FALSE.


# NULL defaults ----
# The cut() function used by cut_by_quantile() can automatically provide sensible labels for each category. The code to generate these labels is pretty complicated, so rather 
# than appearing in the function signature directly, its labels argument defaults to NULL




# Set the default for labels to NULL
cut_by_quantile <- function(x, n = 5, na.rm = FALSE, labels = NULL, interval_type) {
  probs <- seq(0, 1, length.out = n + 1)
  qtiles <- quantile(x, probs, na.rm = na.rm, names = FALSE)
  right <- switch(interval_type, "(lo, hi]" = TRUE, "[lo, hi)" = FALSE)
  cut(x, qtiles, labels = labels, right = right, include.lowest = TRUE)
}

# Remove the labels argument from the call
cut_by_quantile(
  n_visits,
  interval_type = "(lo, hi]"
)



# Categorical defaults
# When cutting up a numeric vector, you need to worry about what happens if a value
# lands exactly on a boundary. You can either put this value into a category of the 
# lower interval or the higher interval. That is, you can choose your intervals to 
# include values at the top boundary but not the bottom (in mathematical terminology,
# "open on the left, closed on the right", or (lo, hi]). Or you can choose the opposite 
# ("closed on the left, open on the right", or [lo, hi)). cut_by_quantile() should allow
# these two choices.

# The pattern for categorical defaults is:
#   
#   function(cat_arg = c("choice1", "choice2")) {
#     cat_arg <- match.arg(cat_arg)
#   }


# Set the categories for interval_type to "(lo, hi]" and "[lo, hi)"
cut_by_quantile <- function(x, n = 5, na.rm = FALSE, labels = NULL, 
                            interval_type = c("(lo, hi]", "[lo, hi)")) {  
  # Match the interval_type argument
  interval_type <- match.arg(interval_type)
  probs <- seq(0, 1, length.out = n + 1)
  qtiles <- quantile(x, probs, na.rm = na.rm, names = FALSE)
  right <- switch(interval_type, "(lo, hi]" = TRUE, "[lo, hi)" = FALSE)
  cut(x, qtiles, labels = labels, right = right, include.lowest = TRUE)
}

# Remove the interval_type argument from the call
cut_by_quantile(n_visits) # interval_type is taken as (lo,hi] as nothing is mentioned match.arg matches arg against a table of candidate values as specified by choices, where NULL means to take the first one.




#Passing arguments between functions----

#Handling missing value----  
calc_geometric_mean <- function(x,na.rm=FALSE){
  x %>% log() %>% mean(na.rm=na.rm) %>% exp()   #mean(na.rm=na.rm)
}


# Using eplises ... argument
cal_geometric_mean <- function(x, ...){   # accept any other argument in cal_geometric_mean and pass it to mean
  x %>% log() %>% mean(...) %>% exp()   #
}




#Example
# Look at the Standard and Poor 500 data
glimpse(std_and_poor500)

# Write a function to calculate the reciprocal
get_reciprocal <- function(x) {
  1 / x
}



# From previous step
get_reciprocal <- function(x) {
  1 / x
}

# Write a function to calculate the harmonic mean
calc_harmonic_mean <- function(x) {
  x %>%
    get_reciprocal() %>%
    mean() %>%
    get_reciprocal()
}


#harmonic_mean(x)=1/arithmetic_mean(1/x)


# From previous steps
get_reciprocal <- function(x) {
  1 / x
}
calc_harmonic_mean <- function(x) {
  x %>%
    get_reciprocal() %>%
    mean() %>%
    get_reciprocal()
}

std_and_poor500 %>% 
  # Group by sector
  group_by(sector) %>% 
  # Summarize, calculating harmonic mean of P/E ratio
  summarize(hmean_pe_ratio = calc_harmonic_mean(pe_ratio))


# Add an na.rm arg with a default, and pass it to mean()
calc_harmonic_mean <- function(x, na.rm = FALSE) {
  x %>%
    get_reciprocal() %>%
    mean(na.rm = na.rm) %>%
    get_reciprocal()
}



std_and_poor500 %>% 
  # Group by sector
  group_by(sector) %>% 
  # Summarize, calculating harmonic mean of P/E ratio
  summarize(hmean_pe_ratio = calc_harmonic_mean(pe_ratio, na.rm = TRUE))


# Passing arguments with ...
# Rather than explicitly giving calc_harmonic_mean() and na.rm argument, you can use ... to simply "pass other arguments" to mean().
# 
# The dplyr package is loaded.


calc_harmonic_mean <- function(x, ...) {
  x %>%
    get_reciprocal() %>%
    mean(...) %>%
    get_reciprocal()
}

std_and_poor500 %>% 
  # Group by sector
  group_by(sector) %>% 
  # Summarize, calculating harmonic mean of P/E ratio
  summarize(hmean_pe_ratio = calc_harmonic_mean(pe_ratio, na.rm = TRUE))


# Using ... doesn't change how people use your function; it just means the function is more flexible.
# Whether flexible means better (or not) is up to you to decide.



# Checking arguments
install.packages("assertive")




# Throwing errors with bad arguments
# If a user provides a bad input to a function, the best course of action is to throw an error letting them know. The two rules are
# 
# Throw the error message as soon as you realize there is a problem (typically at the start of the function).
# Make the error message easily understandable.
# You can use the assert_*() functions from assertive to check inputs and throw errors when they fail.


calc_harmonic_mean <- function(x, na.rm = FALSE) {
  # Assert that x is numeric
  assert_is_numeric(x)
  x %>%
    get_reciprocal() %>%
    mean(na.rm = na.rm) %>%
    get_reciprocal()
}

# See what happens when you pass it strings
calc_harmonic_mean(std_and_poor500$sector)
# Error: is_numeric : x is not of class 'numeric'; it has class 'character'.




# Custom error logic
# Sometimes the assert_*() functions in assertive don't give the most informative error 
# message. For example, the assertions that check if a number is in a numeric range will 
# tell the user that a value is out of range, but the won't say why that's a problem. In 
# that case, you can use the is_*() functions in conjunction with messages, warnings, or 
# errors to define custom feedback.



calc_harmonic_mean <- function(x, na.rm = FALSE) {
  assert_is_numeric(x)
  # Check if any values of x are non-positive
  if(any(is_non_positive(x), na.rm = TRUE)) {
    # Throw an error
    stop("x contains non-positive values, so the harmonic mean makes no sense.")
  }
  x %>%
    get_reciprocal() %>%
    mean(na.rm = na.rm) %>%
    get_reciprocal()
}

# See what happens when you pass it negative numbers
calc_harmonic_mean(std_and_poor500$pe_ratio - 20)
# Error: x contains non-positive values, so the harmonic mean makes no sense.

# The harmonic mean only makes sense when x has all positive values. (Try calculating the harmonic mean of one and minus one to see why.) 






# Fixing function arguments
# The harmonic mean function is almost complete. However, you still need to provide 
# some checks on the na.rm argument. This time, rather than throwing errors when the 
# input is in an incorrect form, you are going to try to fix it.
# 
# na.rm should be a logical vector with one element (that is, TRUE, or FALSE).
# 
# The assertive package is loaded for you.


# Update calc_harmonic_mean() to fix the na.rm argument. Use use_first() to select 
# the first element, and coerce_to() to change it to logical.

# Update the function definition to fix the na.rm argument
calc_harmonic_mean <- function(x, na.rm = FALSE) {
  assert_is_numeric(x)
  if(any(is_non_positive(x), na.rm = TRUE)) {
    stop("x contains non-positive values, so the harmonic mean makes no sense.")
  }
  # Use the first value of na.rm, and coerce to logical
  na.rm <- coerce_to(use_first(na.rm), target_class = "logical")
  x %>%
    get_reciprocal() %>%
    mean(na.rm = na.rm) %>%
    get_reciprocal()
}

# See what happens when you pass it malformed na.rm
calc_harmonic_mean(std_and_poor500$pe_ratio, na.rm = 1:5)

# Warning message: Only the first value of na.rm (= 1) will be used.
# Warning message: Coercing use_first(na.rm) to class 'logical'.





# Returning values from functions----
# some times iits required to get result early




# Returning early
# Sometimes, you don't need to run through the whole body of a function to get the answer. In that case you can return early from that function using return().
# 
# To check if x is divisible by n, you can use is_divisible_by(x, n) from assertive.
# 
# Alternatively, use the modulo operator, %%. x %% n gives the remainder when dividing x by n, so x %% n == 0 determines whether x is divisible by n. Try 1:10 %% 3 == 0 in the console.
# 
# To solve this exercise, you need to know that a leap year is every 400th year (like the year 2000) or every 4th year that isn't a century (like 1904 but not 1900 or 1905).
# 
# assertive is loaded.


is_leap_year <- function(year) {
  # If year is div. by 400 return TRUE
  if(is_divisible_by(year, 400)) {
    return(TRUE)
  }
  # If year is div. by 100 return FALSE
  if(is_divisible_by(year, 100)) {
    return(FALSE)
  }  
  # If year is div. by 4 return TRUE
  if(is_divisible_by(year, 4)) {
    return(TRUE)
  }
  # Otherwise return FALSE
  FALSE
}



# Returning invisibly
# When the main purpose of a function is to generate output, like drawing a plot or printing something in the console, you may not want a return value to be printed as well. In that case, the value should be invisibly returned.

# Define a pipeable plot fn with data and formula args
pipeable_plot <- function(data, formula) {
  # Call plot() with the formula interface
  plot(formula, data)
  # Invisibly return the input dataset
  invisible(data)
}

# Draw the scatter plot of dist vs. speed again
plt_dist_vs_speed <- cars %>% 
  pipeable_plot(dist ~ speed)

# Now the plot object has a value
plt_dist_vs_speed





# Returning multiple values from functions 
# Multi assignment

library(zeallot)
c(vrsn, os, pkgs) %<-% sessionInfo()


#broom pkg: tidying model objects to dataframe
# glance -> model 
# tidy -> coefficients (p value)
# augment -> observation (residuals)


# Look at the structure of model (it's a mess!)
str(model)

# Use broom tools to get a list of 3 data frames
list(
  # Get model-level values
  model = glance(model),
  # Get coefficient-level values
  coefficients = tidy(model),
  # Get observation-level values
  observations = augment(model)
)


# Wrap this code into a function, groom_model
groom_model <- function(model) {
  list(
    model = glance(model),
    coefficients = tidy(model),
    observations = augment(model)
  )
}


# From previous step
groom_model <- function(model) {
  list(
    model = glance(model),
    coefficients = tidy(model),
    observations = augment(model)
  )
}

# Call groom_model on model, assigning to 3 variables
c(mdl, cff, obs) %<-% groom_model(model)

# See these individual variables
mdl; cff; obs





# Returning metadata
# Sometimes you want the return multiple things from a function, but you want the result 
# to have a particular class (for example, a data frame or a numeric vector), so 
# returning a list isn't appropriate. This is common when you have a result plus
# metadata about the result. (Metadata is "data about the data". For example, it
# could be the file a dataset was loaded from, or the username of the person who
# created the variable, or the number of iterations for an algorithm to converge.)


# attr(object, "attribute_name") <- attribute_value

pipeable_plot <- function(data, formula) {
  plot(formula, data)
  # Add a "formula" attribute to data
  attr(data, "formula") <- formula
  invisible(data)
}

# From previous exercise
plt_dist_vs_speed <- cars %>% 
  pipeable_plot(dist ~ speed)

# Examine the structure of the result
str(plt_dist_vs_speed)


# 'data.frame':	50 obs. of  2 variables:
#   $ speed: num  4 4 7 7 8 9 10 10 10 11 ...
# $ dist : num  2 10 4 22 16 10 18 26 34 17 ...
# - attr(*, "formula")=Class 'formula'  language dist ~ speed
# .. ..- attr(*, ".Environment")=<environment: 0x564985cc6980>



# Environment: ----
# Environment are like lists:
# Add capitals, national_parks, & population to a named list
rsa_lst <- list(
  capitals = capitals,
  national_parks = national_parks,
  population = population
)

# List the structure of each element of rsa_lst
ls.str(rsa_lst)



# From previous step
rsa_lst <- list(
  capitals = capitals,
  national_parks = national_parks,
  population = population
)

# Convert the list to an environment
rsa_env <- list2env(rsa_lst)

# List the structure of each variable
ls.str(rsa_env)



# From previous steps
rsa_lst <- list(
  capitals = capitals,
  national_parks = national_parks,
  population = population
)
rsa_env <- list2env(rsa_lst)

# Find the parent environment of rsa_env
parent <- parent.env(rsa_env)

# Print its name
environmentName(parent)



# Environments have parent

# Getting the parent environment
# parent <- parent.env(datacamp.env)
# environmentName(parent)

# sequence of parent environment
search()
# base has the biggest environment



# checking whether a variable exist in the environment as other (If they are not same envirnorment then it searches the parent then grandparent and so on)

library(zeallot)

founding_year <- 2013
testing <- list(yesr=2019)

testing_env <- list2env(testing)
exists("founding_year",envir = testing_env)

exists("founding_year",envir = testing_env, inherits = FALSE)


# Scope and precedence


# Assessing variables outside functions
 x_times_y <- function(x){  # x will be added to the environment 
   x*y  #x*y will performed in that environment
 }
#This will give error
 
 
 # when we define a function r gives an environment to store its variables
 
 y <- 4
 x_times_y <- function(x){  # x will be added to the environment 
   x*y  #x*y will performed in that environment
 }
 #This will work as r finds y in the parent environment of x
 
 
 #~~~~~~~~~~~~~~~~~~~~~~~``
 y <- 4
 x_times_y <- function(x){  # x will be added to the environment 
   x*y  #x*y will performed in that environment
 }
 
 print(x)
 # This will give an error as x is being looked in the global environment but it isnt there
 # This because function environment isnt there in the parent environment its in child
 
 
 #VIMP function can look for outside its environment but the reverse is not true
 
 
 
 #Looping....
 
 #Conditionals and Control Flow
 #break statement in for loop
 
 for(city in cities){
   if(nchar(city)==6){
     break
   }
   print(city)
 }
 
 
 
 for(i in 1:length(cities)){
   
 }
 
 
 