rm(list = ls())
####################################
# Load necessary libraries
####################################
library(readxl)
library(skimr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(psych)
library(broom)

# Set working directory
setwd("C:/Users/user/Desktop/BU_Lectures/Business_Intelligence/Assignments/Tim")

# Read and load the dataset
data_table <- read_excel("coursework1.xlsx")

# Skim data to investigate if there are any missing values and outliers
skim(data_table)

##########################################
# Data Cleaning and Transformation
##########################################
# Filter out blank spaces in bonus, convert to numeric, and categorize into sub_cats
data_table <- data_table %>%
  filter(bonus != "") %>%
  mutate(bonus = as.numeric(bonus),
         bonus_category = ifelse(bonus > 2341, "above_average_bonus", "below_average_bonus"),
         bonus_category = as.factor(bonus_category))

table(data_table$office)

# Filter out blank spaces in qualification and convert to factor
data_table <- data_table %>%
  filter(qualification != "") %>%
  mutate(qualification = as.factor(qualification))

# Filter out "." in position and convert to factor
data_table <- data_table %>%
  filter(position != ".") %>%
  mutate(position = as.factor(position))

# Convert Gender to factor and maintain lower case for values under gender
data_table <- data_table %>%
  mutate(gender = tolower(gender),  
         gender = as.factor(gender))
         
# Filter out blank spaces in tenure and convert to tenure_yrs
data_table <- data_table %>%
  filter(tenure != "") %>%
  mutate(tenure = as.numeric(tenure),
         tenure_yrs = tenure / 12)

# Categorize tenure_yrs into long_tenure and short_tenure, then convert to factor
data_table <- data_table %>%
  mutate(tenure_category = case_when(
    tenure_yrs > 5.0 ~ "long_tenure",
    tenure_yrs <= 5.0 ~ "short_tenure"
  )) %>%
  mutate(tenure_category = as.factor(tenure_category))

# Categorise FTE into full_time and part_time, then convert to factor
data_table <- data_table %>%
  mutate(contract_type = ifelse(FTE == 1.0, "full_time", "part_time"),
         contract_type = as.factor(contract_type))

# Convert salary to numeric and mutate to total salary and average salary
data_table <- data_table %>%
  mutate(salary = as.numeric(salary),
         total_salary = sum(salary, na.rm = TRUE),
         average_salary = mean(salary, na.rm = TRUE))

# ====================== Preparing data for Visualisation ======================

# Aggregation of Variables to be used for visualisation

# Total Salary
sum_salary <- data_table%>%
  summarise(sum_salary = sum(salary, na.rm = TRUE))

# Total salary by gender
salary_by_gender <- data_table %>%
  group_by(gender) %>%
  summarise(total_salary = sum(salary, na.rm = TRUE))

# Average salary by gender
average_salary_by_gender <- data_table %>%
  group_by(gender) %>%
  summarise(average_salary = mean(salary, na.rm = TRUE))
print(average_salary_by_gender)

# Average salary by gender and tenure category
average_salary_by_gender_tenure_cat <- data_table %>%
  group_by(gender, tenure_category) %>%
  summarise(average_salary = mean(salary, na.rm = TRUE)) %>%
  ungroup()

# Average salary by gender and contract
avg_salary_by_gender_contractype <- data_table %>%
  group_by(gender, contract_type) %>%
  summarise(average_salary = mean(salary, na.rm = TRUE)) %>%
  ungroup()

# Average salary by gender and qualification
avg_salary_by_gender_qualification <- data_table %>%
  group_by(gender, qualification) %>%
  summarise(average_salary = mean(salary, na.rm = TRUE)) %>%
  ungroup()

# Average salary by gender and position
avg_salary_by_gender_position <- data_table %>%
  group_by(gender, position) %>%
  summarise(average_salary = mean(salary, na.rm = TRUE)) %>%
  ungroup()

# Calculate the percentage for each gender
data_gender_count <- data_table %>%
  group_by(gender) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100)

# Calculate the total salary and percentage for each gender
salary_by_gender <- salary_by_gender %>%
  mutate(percentage = (total_salary / sum(total_salary)) * 100)

# Calculate the total average salary and percentage for each gender
average_salary_by_gender <- average_salary_by_gender %>%
  mutate(percentage = (average_salary / sum(average_salary)) * 100)

# Calculate the total average salary and percentage for each gender within each tenure category
average_salary_by_gender_tenure_cat <- average_salary_by_gender_tenure_cat %>%
  group_by(tenure_category) %>%
  mutate(percentage = (average_salary / sum(average_salary)) * 100)

# Calculate the total average salary and percentage for each gender within each contract type
avg_salary_by_gender_contractype <- avg_salary_by_gender_contractype %>%
  group_by(contract_type) %>%
  mutate(percentage = (average_salary / sum(average_salary)) * 100)

# Calculate the total average salary and percentage for each gender within each qualification
avg_salary_by_gender_qualification <- avg_salary_by_gender_qualification %>%
  group_by(qualification) %>%
  mutate(percentage = (average_salary / sum(average_salary)) * 100)

# Calculate the total average salary and percentage for each gender within each position
avg_salary_by_gender_position <- avg_salary_by_gender_position %>%
  group_by(position) %>%
  mutate(percentage = (average_salary / sum(average_salary)) * 100)

# Calculate mean, median salary and percentage for each gender
salary_stats <- data_table %>%
  group_by(gender) %>%
  summarise(
    mean_salary = mean(salary, na.rm = TRUE),
    median_salary = median(salary, na.rm = TRUE)
  ) %>%
  mutate(
    total_salary = mean_salary + median_salary,
    mean_percent = (mean_salary / total_salary) * 100,
    median_percent = (median_salary / total_salary) * 100
  )

# Convert to long format for easier plotting
salary_stats_long <- salary_stats %>%
  gather(key = "statistic", value = "salary", mean_salary, median_salary) %>%
  gather(key = "percentage_stat", value = "percentage", mean_percent, median_percent) %>%
  filter(statistic == sub("_percent", "_salary", percentage_stat))  # Matching percentages with corresponding statistics


# Skim data again to ensure there are no missing values as well as outliers
skim(data_table) # Original data observations is 777, reduced to 530 after cleaning
summary(data_table)
# Descriptive Statistics of selected variable
DS <- data_table %>%
  select("salary", "gender", "tenure_category", "contract_type", "qualification", "position", "tenure_category", "bonus_category")
describe(DS)

###################################################
# Visualisations of Insights Relating to Pay Gap
###################################################

# Plot 1. Salary distribution using histogram
ggplot(data_table, aes(x = salary)) +
  geom_histogram(binwidth = 200, fill = "skyblue", color = "black", alpha = 0.7) +
  labs(title = "Salary Distribution", x = "Monthly Salary", y = "Frequency") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),  # Center and bold the title
    axis.title = element_text(face = "bold"),               # Make axis titles bold
    panel.grid.major = element_blank(),                     # Remove major grid lines
    panel.grid.minor = element_blank()                      # Remove minor grid lines
  )
  
# Salary distribution for male & female gender separately, using facet
ggplot(data_table, aes(x = salary, fill = gender)) +
  geom_histogram(binwidth = 200, color = "black", alpha = 0.7) +
  labs(title = "Salary Frequency Distribution by Gender", x = "Salary", y = "Frequency") +
  theme_minimal() +
  facet_wrap(~ gender) +  # Create separate plots for each gender
  scale_fill_manual(values = c("male" = "skyblue", "female" = "pink")) +  # Assign colors for male and female
  theme(
    legend.position = "none",  # Remove the legend
    plot.title = element_text(hjust = 0.5, face = "bold"),  # Center and bold the title
    axis.title = element_text(face = "bold"),               # Bold axis titles
    panel.grid.major = element_blank(),                     # Remove major grid lines
    panel.grid.minor = element_blank()                      # Remove minor grid lines
  )


# Plot separate plots for mean and median salary by gender
ggplot(salary_stats_long, aes(x = gender, y = salary, fill = gender)) +
  geom_bar(stat = "identity", position = "dodge", color = "black", alpha = 0.7) +
  geom_text(aes(label = round(salary, 1)), vjust = -0.3, position = position_dodge(width = 0.6), size = 3.5) +  # Add salary labels
  labs(title = "Mean and Median Salary by Gender", x = "Gender", y = "Salary") +
  theme_minimal() +
  scale_fill_manual(values = c("male" = "skyblue", "female" = "pink")) +  # Different colors for gender
  theme(legend.position = "none",
    plot.title = element_text(hjust = 0.5, face = "bold"),  # Center and bold the title
    axis.title = element_text(face = "bold"),               # Bold axis titles
    panel.grid.major = element_blank(),                     # Remove major grid lines
    panel.grid.minor = element_blank()                      # Remove minor grid lines
  ) +
  facet_wrap(~ statistic, scales = "free_y")  # Separate plots for mean and median

# Plot 2: Employee count by gender with percentages
ggplot(data_gender_count, aes(x = gender, y = count, fill = gender)) +
  geom_bar(stat = "identity", color = "black", alpha = 0.7, width = 0.6) +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), vjust = -0.5) +
  labs(title = "Percentage of Employees by Gender", x = "Gender", y = "Count") +
  theme_minimal()+
  scale_fill_manual(values = c("male" = "skyblue", "female" = "pink")) +  # Assign colors
  theme(legend.position = "none",   # Remove the legend
        panel.grid.major = element_blank(),  # Remove major grid lines
        panel.grid.minor = element_blank(),  # Remove minor grid lines
        plot.title = element_text(hjust = 0.5, face = "bold"),  # Center and bold the title
        axis.text.y = element_blank(),  # Remove Y-axis tick values
        axis.title.y = element_blank(),  # Remove Y-axis title
        axis.text.x = element_text(face = "bold"))  # Keep X-axis values bold

# Plot 3: Total employee salary by gender with percentages
ggplot(salary_by_gender, aes(x = gender, y = total_salary, fill = gender)) +
  geom_bar(stat = "identity", color = "black", alpha = 0.7, width = 0.6) +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), vjust = -0.5) +
  labs(title = "Total Employee Salary by Gender", x = "Gender", y = "Total Salary") +
  scale_y_continuous(limits = c(0, 1600000), breaks = seq(0, 1600000, by = 200000)) +
  theme_minimal()+
  scale_fill_manual(values = c("male" = "skyblue", "female" = "pink")) +  # Assign colors
  theme(legend.position = "none")  # Remove the legend


# Plot 4: Average monthly salary by gender  with percentages
ggplot(average_salary_by_gender, aes(x = gender, y = average_salary, fill = gender)) +
  geom_bar(stat = "identity", color = "black", alpha = 0.7, width = 0.6) +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), vjust = -0.5) +
  labs(title = "Average Monthly Salary by Gender", x = "Gender", y = "Average Salary") +
  theme_minimal() +
  scale_fill_manual(values = c("male" = "skyblue", "female" = "pink")) +  # Assign colors
  theme(legend.position = "none",  # Remove the legend
        panel.grid.major = element_blank(),  # Remove major grid lines
        panel.grid.minor = element_blank(),  # Remove minor grid lines
        plot.title = element_text(hjust = 0.5, face = "bold"),  # Center and bold the title
        #axis.text.y = element_blank(),  # Remove Y-axis tick values
        axis.title.y = element_blank(),  # Remove Y-axis title
        axis.text.x = element_text(face = "bold"))  # Keep X-axis values bold


# Plot 5: Average monthly salary by gender and tenure category with percentages
ggplot(average_salary_by_gender_tenure_cat, aes(x = gender, y = average_salary, fill = gender)) +
  geom_bar(stat = "identity", color = "black", alpha = 0.7, position = position_dodge(width = 0.9)) +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), vjust = -0.3, position = position_dodge(width = 0.9), size = 3.5) +
  labs(title = "Avg. Monthly Salary by Gender and Employment Tenure", x = "Tenure", y = "Average Salary") +
  theme_minimal() +
  facet_wrap(~ tenure_category) +
  scale_fill_manual(values = c("male" = "skyblue", "female" = "pink")) +  # Assign colors
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 10, face = "bold"),
        panel.grid.major = element_blank(),  # Remove major grid lines
        panel.grid.minor = element_blank(),  # Remove minor grid lines
        panel.border = element_rect(color = "black", fill = NA, size = 1),
        axis.text.x = element_blank()  
)

# Plot 6: Average monthly salary by gender and contract type with percentages
ggplot(avg_salary_by_gender_contractype, aes(x = gender, y = average_salary, fill = gender)) +
  geom_bar(stat = "identity", color = "black", alpha = 0.7, position = position_dodge(width = 0.9)) +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), vjust = -0.3, position = position_dodge(width = 0.9), size = 3.5) +
  labs(title = "Avg. Monthly Salary by Gender and Contract Type", x = "Contract Type", y = "Average Salary") +
  theme_minimal() +
  facet_wrap(~ contract_type) +
  scale_fill_manual(values = c("male" = "skyblue", "female" = "pink")) +  # Assign colors
  theme(strip.background = element_blank(),
        panel.grid.major = element_blank(),  # Remove major grid lines
        panel.grid.minor = element_blank(),  # Remove minor grid lines
        strip.text = element_text(size = 10, face = "bold"),
        panel.border = element_rect(color = "black", fill = NA, size = 1),
        axis.text.x = element_blank()) 



# Plot 7: Average monthly salary by gender and qualification with percentages
ggplot(avg_salary_by_gender_qualification, aes(x = gender, y = average_salary, fill = gender)) +
  geom_bar(stat = "identity", color = "black", alpha = 0.7, position = position_dodge(width = 0.9)) +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), vjust = -0.3, position = position_dodge(width = 0.9), size = 3.5) +
  labs(title = "Avg. Monthly Salary by Gender and Qualification", x = "Qualification", y = "Average Salary") +
  theme_minimal() +
  facet_wrap(~ qualification) +
  scale_fill_manual(values = c("male" = "skyblue", "female" = "pink")) +  # Assign colors
  theme(strip.background = element_blank(),
        panel.grid.major = element_blank(),  # Remove major grid lines
        panel.grid.minor = element_blank(),  # Remove minor grid lines
        strip.text = element_text(size = 7.8, face = "bold"),
        panel.border = element_rect(color = "black", fill = NA, size = 1),
        axis.text.x = element_blank())
  


# Plot 8: Average monthly salary by gender and position with percentages
ggplot(avg_salary_by_gender_position, aes(x = gender, y = average_salary, fill = gender)) +
  geom_bar(stat = "identity", color = "black", alpha = 0.7) +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), vjust = -0.5, position = position_dodge(width = 0.9), size = 3.2) +
  labs(title = "Avg. Monthly Salary by Gender and Position", x = "Position", y = "Average Salary") +
  theme_minimal() +
  facet_wrap(~ position) +
  scale_fill_manual(values = c("male" = "skyblue", "female" = "pink")) +  # Assign colors
  theme(strip.background = element_blank(),
        panel.grid.major = element_blank(),  # Remove major grid lines
        panel.grid.minor = element_blank(),  # Remove minor grid lines
        strip.text = element_text(size = 10, face = "bold"),
        panel.border = element_rect(color = "black", fill = NA, size = 1),
        axis.text.x = element_blank())


# ============== Raw Pay Gap Analysis ===============================
# Calculate average salaries by gender
average_salary_men <- data_table %>% filter(gender == 'male') %>% summarize(mean(salary, na.rm = TRUE)) %>% pull()
average_salary_women <- data_table %>% filter(gender == 'female') %>% summarize(mean(salary, na.rm = TRUE)) %>% pull()

# This is where I calculate the Raw pay gap in the dataset without controlling for any factor.

raw_pay_gap <- ((average_salary_men - average_salary_women) / average_salary_men) * 100

print(paste("Raw Pay Gap:", round(raw_pay_gap, 2), "%"))

####################################################################################
# Regression model 
# to identify key variable that are statistically significant in influencing pay gap
#####################################################################################
reg_model <- lm(salary ~ gender + tenure_category + contract_type + bonus_category + qualification + position + office, data = data_table)
summary(reg_model)

tidy_model <- tidy(reg_model)

# Export to CSV
write.csv(tidy_model, "regression_results.csv")

# This is where I check for the factors in the regression output
print(summary(reg_model)$coefficients)

# This where I extract the coefficient for Gender and mean of the male salary from the dataset
gender_coef <- summary(reg_model)$coefficients['gendermale', 'Estimate']
average_salary_men <- mean(data_table$salary[data_table$gender == "male"], na.rm = TRUE)

# This is where I calculate the pay gap after controlling for factors such as position, qualification, gender, etc. This pay gap is called Controlled Pay Gap
controlled_pay_gap <- (gender_coef / average_salary_men) * 100
print(paste("Controlled Pay Gap:", round(controlled_pay_gap, 2), "%"))
