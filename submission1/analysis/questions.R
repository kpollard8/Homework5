if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggplot2, dplyr, lubridate, stringr, readxl, data.table, gdata)


# Load required libraries
library(dplyr)
library(ggplot2)

##Question 1: Plot the share of the adult population with direct purchase health insurance over time.

final.insurance <- read_tsv("data/output/acs_insurance.txt")

library(ggplot2)
library(dplyr)

# Calculate the share of adult population with direct purchase health insurance for each year
final.insurance <- final.insurance %>%
  group_by(year) %>%
  summarise(total_adult_pop = sum(adult_pop),
            total_direct_ins = sum(ins_direct)) %>%
  mutate(share_direct_ins = total_direct_ins / total_adult_pop)

# Plotting the trend over time
question1 <- ggplot(final.insurance, aes(x = year, y = share_direct_ins)) +
  geom_line() +
  labs(x = "Year", y = "Share of Adult Population with Direct Purchase Health Insurance", 
       title = "Trend of Direct Purchase Health Insurance Over Time")

question1

#Question 2

#Question 3 

# Calculate the share of the adult population with Medicaid for each year
final.insurance <- final.insurance %>%
  mutate(share_medicaid = ins_medicaid / adult_pop)

# Create the ggplot object and store it in the variable question3
question3 <- ggplot(final.insurance, aes(x = year, y = share_medicaid)) +
  geom_line() +
  geom_point() +
  labs(x = "Year", y = "Share of Adult Population with Medicaid",
       title = "Share of Adult Population with Medicaid Over Time",
       subtitle = "Question 3") +
  theme_minimal()

# Display the plot
question3


#question 4
library(tidyverse)  
mcaid.data <- read_tsv("data/output/acs_medicaid.txt")
ins.plot.dat <- mcaid.data %>% filter(expand_year==2014 | is.na(expand_year), !is.na(expand_ever)) %>%
  mutate(perc_unins=uninsured/adult_pop) %>%
  group_by(expand_ever, year) %>% summarize(mean=mean(perc_unins))

question4 <- ggplot(data=ins.plot.dat, aes(x=year,y=mean,group=expand_ever,linetype=expand_ever)) + 
  geom_line() + geom_point() + theme_bw() +
  geom_vline(xintercept=2013.5, color="red") +
  geom_text(data = ins.plot.dat %>% filter(year == 2016), 
            aes(label = c("Non-expansion","Expansion"),
                x = year + 1,
                y = mean)) +
  guides(linetype="none") +
  labs(
    x="Year",
    y="Fraction Uninsured",
    title="Share of Uninsured over Time"
  )
question4

#Question 5
mcaid.data <- read_tsv("data/output/acs_medicaid.txt")
reg.dat <- mcaid.data %>% filter(expand_year==2012, 2015 | is.na(expand_year), !is.na(expand_ever)) %>%
  mutate(perc_unins=uninsured/adult_pop,
         post = (year>=2012), 
         treat=post*expand_ever)

dd.ins.reg <- lm(perc_unins ~ post + expand_ever + post*expand_ever, data=reg.dat)