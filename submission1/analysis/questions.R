if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggplot2, dplyr, lubridate, stringr, readxl, data.table, gdata)


# Load required libraries
library(dplyr)
library(ggplot2)

#Question 1: Plot the share of the adult population with direct purchase health insurance over time.

# Read the data from the file
insurance_data <- read.delim("data/output/acs_insurance.txt")

# Check the structure of the data
str(insurance_data)

# View the first few rows of the data
head(insurance_data)


# Filter data for direct purchase insurance
direct_purchase <- insurance_data %>%
  filter(!is.na(ins_direct))

# Calculate share of adult population with direct purchase insurance
direct_purchase_share <- direct_purchase %>%
  mutate(share_direct = ins_direct / adult_pop)

# Creating the plot and storing it as an object called question1
question1 <- ggplot(direct_purchase_share, aes(x = year, y = share_direct)) +
  geom_line() +
  geom_point() +
  labs(x = "Year", y = "Share of Adult Population with Direct Purchase Insurance") +
  ggtitle("Share of Adult Population with Direct Purchase Insurance Over Time")

# Viewing the plot
question1