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

calc_indices <- function(data, epsilon, indices) {
  
  #functions to calculate indices
  gini_calc <- function(data, indices) {
    sample <- data[indices]
    return(Gini(sample))}
  
  atkinson_calc <- function(data, indices, epsilon) {
    sample <- data[indices]
    # Ensure no zero or negative incomes
    if (any(sample <= 0)) {
      return(NaN)}
    
    atkinson_index <- Atkinson(sample, epsilon)
    return(atkinson_index)}
  
  theil_calc <- function(data, indices) {
    sample <- data[indices]
    return(Theil(sample))}
  
  print(paste("Gini coefficient =", gini_calc(data, indices),
              "Atkinson index =", atkinson_calc(data, indices, epsilon),
              "Theil index =", theil_calc(data, indices)))}

p_ratio <- function(data, income_col, pX, pY) {
  income <- data[[income_col]]
  income <- income[!is.na(income)]
  
  # Calculate the Xth and Yth percentiles
  p_upp <- quantile(income, pX)
  p_upp_sum <- sum(income[income > pX])
  p_low <- quantile(income, pY)
  p_low_sum <- sum(income[income < pY])
  
  # Calculate the pX/pY ratio
  calc_p_ratio <- p_upp_sum / p_low_sum
  
  return(p_ratio)}

calc_indices_for_regions <- function(data, region1_col, region2_col, earning_col, plot_dir = "plots") {
  data_frame <- data.frame(
    Region.1 = data[[region1_col]],
    Region.2 = data[[region2_col]],
    Earning.Household = as.numeric(as.character(data[[earning_col]])))
  
  #remove NA values from the Earning.Household column
  data_frame <- data_frame[!is.na(data_frame$Earning.Household), ]
  
  #sort data by Region.1 and Region.2 and split it for same regions
  data_frame_sorted1 <- data_frame %>% arrange(Region.1)
  data_frame_sorted2 <- data_frame %>% arrange(Region.2)
  regional_list1 <- split(data_frame_sorted1, data_frame_sorted1$Region.1)
  regional_list2 <- split(data_frame_sorted2, data_frame_sorted2$Region.2)
  
  #create directory for plots if it doesn't exist
  if (!dir.exists(plot_dir)) {
    dir.create(plot_dir)}
  
  #function to calculate inequality metrics for each subset and plot Lorenz curves
  calculate_metrics_and_plot_lorenz <- function(data_list, region_col_name, plot_dir) {
    results <- lapply(names(data_list), function(region) {
      sub_data <- data_list[[region]]
      earnings <- sub_data$Earning.Household
      
      #calculate inequality metrics
      gini <- Gini(earnings)
      atkinson <- Atkinson(earnings, parameter = 0.5)
      theil <- Theil(earnings)
      
      #compute and plot lorenz curves
      lorenz_curve <- Lc(earnings)
      plot_path <- file.path(plot_dir, paste0("Lorenz_Curve_", region_col_name, "_", region, ".png"))
      png(filename = plot_path)
      plot(lorenz_curve, main = paste("Lorenz Curve for", region_col_name, region),
           xlab = "% of objects", ylab = "% of regarded variable", col = "blue", lwd = 1.5, lty = "solid",
           panel.first = {
             rect(0, 0, 1, 1, col = "gray95", border = NA)
             grid(col = "white", lwd = 2, lty = "solid")
           })
      abline(a = 0, b = 1, col = "black", lwd = 1.5, lty = "solid")
      dev.off()
      
      return(data.frame(Gini = gini, Atkinson = atkinson, Theil = theil))})
    
    names(results) <- names(data_list)
    return(results)}
  
  inequality_metrics1 <- calculate_metrics_and_plot_lorenz(regional_list1, "Region1", plot_dir)
  inequality_metrics2 <- calculate_metrics_and_plot_lorenz(regional_list2, "Region2", plot_dir)
  
  return(list(inequality_metrics1 = inequality_metrics1, inequality_metrics2 = inequality_metrics2))}

lorenz_curve <- function(data) {
  plot(Lc(data), main = "Lorenz Curve",
       xlab = "% of population", ylab = "% of income",
       col = "blue", lwd = 1.5, lty = "solid", 
       panel.first = {
         rect(0, 0, 1, 1, col = "gray95", border = NA)
         grid(col = "white", lwd = 2, lty = "solid")})
  abline(a = 0, b = 1, col = "black", lwd = 1.5, lty = "solid")}
