install.packages("tidyverse")
install.packages("readr")
install.packages("dplyr")
install.packages("sf")
install.packages("remotes")
remotes::install_github("htastan/TRmaps")

library(ggplot2)
library(readr)
library(dplyr)
library(sf)
library(TRmaps)

data("tr_nuts2")
tr_nuts2 <- st_as_sf(tr_nuts2)

RegionGINI <- read_csv("RegionGINI.csv")
RegionGINI <- RegionGINI %>% mutate(across(where(is.numeric), ~ round(.x, 3)))

RIS22 <- read.csv("RegionIncomeStatistics22.csv")
RIS23<- read.csv("RegionIncomeStatistics23.csv")

RIS22[, 2:6] <- round(RIS22[, 2:6])
RIS22[, 7:8] <- round(RIS22[, 7:8], 2)
RIS23[, 2:6] <- round(RIS23[, 2:6])
RIS23[, 7:8] <- round(RIS23[, 7:8], 2)

JoinedDF <- left_join(tr_nuts2, RegionGINI, by = c("NUTS2_code" = "...1"))
JoinedDF22 <- left_join(tr_nuts2, RIS22, by = c("NUTS2_code" = "Region"))
JoinedDF23 <- left_join(tr_nuts2, RIS23, by = c("NUTS2_code" = "Region"))

CreatePlot <- function(data, fill_var, label_var, title, subtitle = NULL, colors = c("green", "yellow", "red"), include_percent_sign = FALSE) {
  fill_var_quoted <- paste0("`", fill_var, "`")
  label_var_quoted <- paste0("`", label_var, "`")
  if (include_percent_sign && is.numeric(data[[label_var]])) {
    label_text <- ifelse(is.na(data[[label_var]]), "", sprintf("%.2f%%", data[[label_var]]))}
  else {label_text <- as.character(data[[label_var]])}
  ggplot(data = data) +
    geom_sf(aes_string(fill = fill_var_quoted)) +
    geom_sf_text(aes_string(label = label_var_quoted), label = label_text, size = 4, color = "black", fontface = "bold", show.legend = FALSE) +
    scale_fill_gradientn(colours = colors, guide = "none") +
    theme_void() +
    ggtitle(label = title, subtitle = subtitle) +
    theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 15),
          plot.subtitle = element_text(hjust = 0.5, face = "bold", size = 12),
          plot.background = element_rect(fill = "transparent", color = NA),
          text = element_text(face = "bold"))}

GINI2022 <- CreatePlot(JoinedDF, "GINI Coefficient - 2022", "GINI Coefficient - 2022", "GINI Coefficients of NUTS2 Regions of Turkiye", "2022")
GINI2023 <- CreatePlot(JoinedDF, "GINI Coefficient - 2023", "GINI Coefficient - 2023", "GINI Coefficients of NUTS2 Regions of Turkiye", "2023")
GINI2022Women <- CreatePlot(JoinedDF, "GINI Coefficient for Women - 2022", "GINI Coefficient for Women - 2022", "GINI Coefficients among Women of NUTS2 Regions of Turkiye", "2022 - Earnings GINI")
GINI2023Women <- CreatePlot(JoinedDF, "GINI Coefficient for Women - 2023", "GINI Coefficient for Women - 2023", "GINI Coefficients among Women of NUTS2 Regions of Turkiye", "2023 - Earnings GINI")
GINI2022Men <- CreatePlot(JoinedDF, "GINI Coefficient for Men - 2022", "GINI Coefficient for Men - 2022", "GINI Coefficients among Men of NUTS2 Regions of Turkiye", "2022 - Earnings GINI")
GINI2023Men <- CreatePlot(JoinedDF, "GINI Coefficient for Men - 2023", "GINI Coefficient for Men - 2023", "GINI Coefficients among Men of NUTS2 Regions of Turkiye", "2023 - Earnings GINI")
ScaledHousehold2022 <- CreatePlot(JoinedDF22, "Scaled.Household.Income", "Scaled.Household.Income", "Average Income (Scaled by Adjusted Household Size) of NUTS2 Regions of Turkiye", "2022 - Turkish Average = 49791", colors = c("red", "yellow", "green"))
ScaledHousehold2023 <- CreatePlot(JoinedDF23, "Scaled.Household.Income", "Scaled.Household.Income", "Average Income (Scaled by Adjusted Household Size) of NUTS2 Regions of Turkiye", "2023 - Turkish Average = 82946", colors = c("red", "yellow", "green"))
TransferRatio2022 <- CreatePlot(JoinedDF22, "Transfers.as...of.Income", "Transfers.as...of.Income", "Transfer Income to Mean Income Ratio of Households by NUTS2 Regions of Turkiye", "2022 - Turkish Average = 3.57%", include_percent_sign = TRUE)
TransferRatio2023 <- CreatePlot(JoinedDF23, "Transfers.as...of.Income", "Transfers.as...of.Income", "Transfer Income to Mean Income Ratio of Households by NUTS2 Regions of Turkiye", "2023 - Turkish Average = 3.64%", include_percent_sign = TRUE)

ggsave('GINI Coefficient by Region - Graph - 2022.png', GINI2022, bg='transparent', units = "px", width = 3000, height = 2000)
ggsave('GINI Coefficient by Region - Graph - 2023.png', GINI2023, bg='transparent', units = "px", width = 3000, height = 2000)
ggsave('GINI Coefficient by Region for Women - Graph - 2022.png', GINI2022Women, bg='transparent', units = "px", width = 3000, height = 2000)
ggsave('GINI Coefficient by Region for Women - Graph - 2023.png', GINI2023Women, bg='transparent', units = "px", width = 3000, height = 2000)
ggsave('GINI Coefficient by Region for Men - Graph - 2022.png', GINI2022Men, bg='transparent', units = "px", width = 3000, height = 2000)
ggsave('GINI Coefficient by Region for Men - Graph - 2023.png', GINI2023Men, bg='transparent', units = "px", width = 3000, height = 2000)
ggsave('Average Scaled Income by Region - Graph - 2022.png', ScaledHousehold2022, bg='transparent', units = "px", width = 3000, height = 2000)
ggsave('Average Scaled Income by Region - Graph - 2023.png', ScaledHousehold2023, bg='transparent', units = "px", width = 3000, height = 2000)
ggsave('Transfer Income to Mean Income Ratio by Region - Graph - 2022.png', TransferRatio2022, bg='transparent', units = "px", width = 3000, height = 2000)
ggsave('Transfer Income to Mean Income Ratio by Region - Graph - 2023.png', TransferRatio2023, bg='transparent', units = "px", width = 3000, height = 2000)
