# Code by Kaan Akkaş

install.packages("tidyverse")
install.packages("dplyr")
install.packages("ineq")
install.packages("boot")
install.packages("REAT")
install.packages("stats")

library(tidyverse)
library(dplyr)
library(ineq)
library(boot)
library(REAT)
library(stats)

bootstrapping_indices <- function(data, repl, epsilon) {
  
  #functions to calculate indices
  gini_calc <- function(data, indices) {
    sample <- data[indices]
    return(Gini(sample))}
  
  atkinson_calc <- function(data, indices, epsilon) {
    sample <- data[indices]
    #ensure no zero or negative incomes
    if (any(sample <= 0)) {
      return(NaN)}
    
    atkinson_index <- Atkinson(sample, epsilon)
    return(atkinson_index)}
  
  theil_calc <- function(data, indices) {
    sample <- data[indices]
    return(Theil(sample))}
  
  #function to calculate CIs
  calculate_ci <- function(data, calc_func, R = repl) {
    #bootstrap sampling
    bootstrap_result <- boot(data, calc_func, R = R)
    
    #bootstrap CI
    ci_result <- boot.ci(bootstrap_result, type = "perc")
    
    mean_value <- mean(bootstrap_result$t)
    ci_low <- ci_result$perc[4]
    ci_high <- ci_result$perc[5]
    
    return(list(mean = mean_value, ci = c(ci_low, ci_high)))}
  
  #submit indices and their CIs
  gini_result <- calculate_ci(data, gini_calc, R = repl)
  print(paste("Gini coefficient: Mean =", gini_result$mean, " 95% CI =", gini_result$ci[1], "-", gini_result$ci[2]))
  
  # Pass epsilon to the atkinson_calc function using a wrapper
  atkinson_calc_wrapper <- function(data, indices) atkinson_calc(data, indices, epsilon)
  atkinson_result <- calculate_ci(data, atkinson_calc_wrapper, R = repl)
  print(paste("Atkinson index: Mean =", atkinson_result$mean, " 95% CI =", atkinson_result$ci[1], "-", atkinson_result$ci[2]))
  
  theil_result <- calculate_ci(data, theil_calc, R = repl)
  print(paste("Theil index: Mean =", theil_result$mean, " 95% CI =", theil_result$ci[1], "-", theil_result$ci[2]))
  
  return(list(
    gini = gini_result,
    atkinson = atkinson_result,
    theil = theil_result))}

subsampling_indices <- function(data, epsilon) {
  results <- data.frame(size=numeric(), Gini=numeric(), Atkinson=numeric(), Theil=numeric())
  
  #the subset sizes, taking half and 3/4 of data length is totally made up, proportions can change
  subset_sizes <- c(0.5, 0.75) * length(data)
  
  #loop through each subset size
  for (size in subset_sizes) {
    #loop to create multiple subsets
    #we create 100 different samples for each size
    for (i in 1:100) {
      #create a random subset of the data
      subset <- sample(data, size, replace = FALSE)
      results <- rbind(results, data.frame(size = size, 
                                           Gini = Gini(subset), 
                                           Atkinson = Atkinson(subset, parameter = epsilon), 
                                           Theil = Theil(subset)))}}
  
  #summarize the results by subset size
  summary_results <- results %>%
    group_by(size) %>%
    summarise(across(c(Gini, Atkinson, Theil), 
                     list(mean = mean, sd = sd), 
                     .names = "{col}_{fn}"))
  
  #function to plot histograms
  plot_histogram <- function(data, index, size) {
    ggplot(data, aes_string(x = index)) +
      geom_histogram(binwidth = 0.01, fill = "blue", alpha = 0.7, boundary = 0) +
      labs(title = paste(index, "Histogram for Sample Size", size), x = index, y = "Frequency") +
      theme_minimal()}
  
  #plot histograms for each index and subset size
  for (size in subset_sizes) {
    subset_data <- filter(results, size == size)
    
    print(plot_histogram(subset_data, "Gini", size))
    print(plot_histogram(subset_data, "Atkinson", size))
    print(plot_histogram(subset_data, "Theil", size))}
  return(summary_results)}

sensitivity_analysis <- function(data) {
  data <- as.numeric(data)
  data <- data[!is.na(data)]
  
  #calculate the thresholds, and modify the data
  threshold.upper <- quantile(data, 0.99)
  modified.data.upper <- data[data < threshold.upper]
  threshold.lower <- quantile(data, 0.01)
  modified.data.lower <- data[data > threshold.lower]
  
  gini.original <- Gini(data)
  gini.modified.upper <- Gini(modified.data.upper)
  gini.modified.lower <- Gini(modified.data.lower)
  
  atkinson.original <- Atkinson(data, parameter = 0.5)
  atkinson.modified.upper <- Atkinson(modified.data.upper, parameter = 0.5)
  atkinson.modified.lower <- Atkinson(modified.data.lower, parameter = 0.5)
  
  theil.original <- Theil(data)
  theil.modified.upper <- Theil(modified.data.upper)
  theil.modified.lower <- Theil(modified.data.lower)
  
  print(paste("Gini original:", gini.original, 
              "Gini coefficient after removing top 1%:", gini.modified.upper, 
              "Gini coefficient after removing bottom 1%:", gini.modified.lower))
  print(paste("Atkinson original:", atkinson.original, 
              "Atkinson index after removing top 1%:", atkinson.modified.upper, 
              "Atkinson index after removing bottom 1%:", atkinson.modified.lower))
  print(paste("Theil original:", theil.original, 
              "Theil index after removing top 1%:", theil.modified.upper, 
              "Theil index after removing bottom 1%:", theil.modified.lower))}

analyze_influence_on_inequality <- function(data, epsilon = 0.5) {
  
  incomes <- data
  
  gini_value <- Gini(incomes)
  atkinson_value <- Atkinson(incomes, parameter = epsilon)
  theil_value <- Theil(incomes)
  
  print(paste("Gini coefficient:", gini_value))
  print(paste("Atkinson index (epsilon =", epsilon, "):", atkinson_value))
  print(paste("Theil index:", theil_value))
  
  # Influence function approximation for Gini coefficient
  influence_gini <- function(x, incomes) {
    n <- length(incomes)
    mean_income <- mean(incomes)
    infl <- 2 * (rank(x) - (n + 1) / 2) / n^2 / mean_income
    return(infl)}
  
  influence_atkinson <- function(x, incomes, epsilon = 0.5) {
    n <- length(incomes)
    mean_income <- mean(incomes)
    inequality <- Atkinson(incomes, parameter = epsilon)
    infl <- if (epsilon != 1) {
      ((x / mean_income)^(-epsilon) - 1) / (1 - inequality)
    } else {log(mean_income / x) / mean_income}
    return(infl)}
  
  # Influence function approximation for Theil index
  influence_theil <- function(x, incomes) {
    n <- length(incomes)
    mean_income <- mean(incomes)
    infl <- (x / mean_income) * log(x / mean_income) / mean_income
    return(infl)}
  
  influence_values_gini <- influence_gini(incomes, incomes)
  influence_values_atkinson <- influence_atkinson(incomes, incomes, epsilon)
  influence_values_theil <- influence_theil(incomes, incomes)
  
  influence_data <- data.frame(
    income = incomes,
    influence_gini = influence_values_gini,
    influence_atkinson = influence_values_atkinson,
    influence_theil = influence_values_theil)
  
  p1 <- ggplot(influence_data, aes(x = income, y = influence_gini)) +
    geom_point() +
    labs(title = "Influence Function for Gini Coefficient",
         x = "Income",
         y = "Influence on Gini Coefficient") +
    theme_minimal()
  
  p2 <- ggplot(influence_data, aes(x = income, y = influence_atkinson)) +
    geom_point() +
    labs(title = paste("Influence Function for Atkinson Index (epsilon =", epsilon, ")"),
         x = "Income",
         y = "Influence on Atkinson Index") +
    theme_minimal()
  
  p3 <- ggplot(influence_data, aes(x = income, y = influence_theil)) +
    geom_point() +
    labs(title = "Influence Function for Theil Index",
         x = "Income",
         y = "Influence on Theil Index") +
    theme_minimal()
  print(p1)
  print(p2)
  print(p3)
  summary_influences <- function(influence_values) {
    list(
      mean = mean(influence_values),
      median = median(influence_values),
      sd = sd(influence_values),
      max_value = max(influence_values),
      max_index = which.max(influence_values))}
  
  gini_summary <- summary_influences(influence_values_gini)
  atkinson_summary <- summary_influences(influence_values_atkinson)
  theil_summary <- summary_influences(influence_values_theil)
  
  print("Summary of Gini influences:")
  print(gini_summary)
  
  print("Summary of Atkinson influences:")
  print(atkinson_summary)
  
  print("Summary of Theil influences:")
  print(theil_summary)
  
  # Return influence data and summaries for further analysis if needed
  list(
    influence_data = influence_data,
    gini_summary = gini_summary,
    atkinson_summary = atkinson_summary,
    theil_summary = theil_summary)}
