##########################################
# 1.0 Load relevant libraries
##########################################
library(readxl)
library(skimr)
library(ggplot2)
library(psych)
library(dplyr)
#library(urca)
library(tseries)
library(vars)

###########################################
# 2.0 Set working directory
###########################################

setwd("C:/Users/user/Desktop/BU_Lectures/Research Methods_PG Mgt & Marketing/Deissertation_BA/Datasets/R_Datasets/Datasets")

#############################################
# 3.0 Data Preparation and transformation
#############################################

# Read and load dataset excel files.
# Imported data files are in wide data format
fdi <- read_excel("API_BX.KLT.DINV.WD.GD.ZS_DS2_en_excel_v2_261554.xls")
cpi <- read_excel("API_FP.CPI.TOTL.ZG_DS2_en_excel_v2_228438.xls")
gdpr <- read_excel("API_NY.GDP.MKTP.KD.ZG_DS2_en_excel_v2_228741.xls")
popl <- read_excel("API_SP.POP.GROW_DS2_en_excel_v2_228771_pop_growthRate.xls")
elect_users <- read_excel("API_EG.ELC.ACCS.ZS_DS2_en_excel_v2_228803.xls")

# Wide data format to be converted to long data format
# Filter datasets to Nigeria
fdi <- fdi %>%
  filter(`Country Code`== "NGA")
cpi <- cpi %>%
  filter(`Country Code`== "NGA")
gdpr <- gdpr %>%
  filter(`Country Code`== "NGA")
popl <- popl %>%
  filter(`Country Code`== "NGA")
elect_users <- elect_users %>%
  filter(`Country Code`== "NGA")

# Converting datasets from wide data format to long data format
# Extract years from column names
year <- gsub("Nigeria\\.", "", names(fdi)[-(1:4)]) 
year <- gsub("Nigeria\\.", "", names(cpi)[-(1:4)])
year <- gsub("Nigeria\\.", "", names(gdpr)[-(1:4)])
year <- gsub("Nigeria\\.", "", names(popl)[-(1:4)])
year <- gsub("Nigeria\\.", "", names(elect_users)[-(1:4)])

# Extract values corresponding to each year for Nigeria
fdi_inflow <- unlist(fdi[1, -(1:4)], use.names = FALSE)
infl_rate <- unlist(cpi[1, -(1:4)], use.names = FALSE)
gdp_grate <- unlist(gdpr[1, -(1:4)], use.names = FALSE)
popl_grate <- unlist(popl[1, -(1:4)], use.names = FALSE)
elect_access <- unlist(elect_users[1, -(1:4)], use.names = FALSE)

# Create a data frame with 'year' 
fdi <- data.frame(year = as.numeric(year), Nigeria = fdi_inflow)
cpi <- data.frame(year = as.numeric(year), Nigeria = infl_rate)
gdpr <- data.frame(year = as.numeric(year), Nigeria = gdp_grate)
popl <- data.frame(year = as.numeric(year), Nigeria = popl_grate)
elect_users <- data.frame(year = as.numeric(year), Nigeria = elect_access)

# Rename column from "Nigeria" to "respective column names"
colnames(fdi)[colnames(fdi) == "Nigeria"] <- "fdi_inflow"
colnames(cpi)[colnames(cpi) == "Nigeria"] <- "infl_rate"
colnames(gdpr)[colnames(gdpr) == "Nigeria"] <- "gdp_grate"
colnames(popl)[colnames(popl) == "Nigeria"] <- "popl_grate"
colnames(elect_users)[colnames(elect_users) == "Nigeria"] <- "elect_access"

# Merging/joining all data tables together, to have just one single data table
data_table <- merge(gdpr, cpi, by = "year", all = TRUE)
data_table <- merge(data_table, fdi, by = "year", all = TRUE)
data_table <- merge(data_table, popl, by = "year", all = TRUE)
data_table <- merge(data_table, elect_users, by = "year", all = TRUE)

# Filter year in new data table to 1991 to 2021
data_table <- subset(data_table, year >= 1991 & year <= 2021)

# Skim data to check for missing values or any outliers
skim(data_table)
summary(data_table)

# Get a clean descriptive statistic table for dataframe
data_table <- data_table %>%
  dplyr::select(gdp_grate, fdi_inflow, popl_grate, elect_access, infl_rate)

# Convert data to time series
data_ts <- ts(data_table, start = 1991, frequency = 1)

######################################################################
# 4.0 Descriptive statistics
######################################################################
DS <- describe(data_ts)
print(DS)

######################################################################
# 5.0 Visualisations
######################################################################

#################### Time series at level data #######################
visualize_series <- function(ts_data, variable_name) {
  df <- data.frame(Year = time(ts_data), Value = ts_data)
  p <- ggplot(df, aes(x = Year, y = Value)) +
    geom_line(size = 0.7) + 
    #geom_smooth(method = "lm", col = "darkblue", se = FALSE) +
    ggtitle(paste(variable_name)) +
    xlab(NULL) +
    ylab(NULL) +
    scale_x_continuous(breaks = seq(floor(min(time(ts_data))), ceiling(max(time(ts_data))), by = 3)) +
    theme(
      plot.title = element_text(face = "bold",size = 13, hjust = 0.5),
      axis.title.x = element_text(face = "bold", size = 11),
      axis.title.y = element_text(face = "bold", size = 11),
      axis.text.x = element_text(face = "bold", size = 10),
      axis.text.y = element_text(face = "bold", size = 10),
      panel.border = element_rect(colour = "black", fill = NA, size = 1),
      panel.background = element_rect(fill = "white"),  
      plot.background = element_rect(fill = "white"),
      panel.grid.minor.y = element_line(color = "grey90"),
      panel.grid.major.y = element_line(color = "grey90")
    )
  print(p)
}

# Figure 4.1 Plots of time series at level data
Figure4.1 <- visualize_series(data_ts[, "gdp_grate"], "GDP Growth Rate")
Figure4.2 <- visualize_series(data_ts[, "fdi_inflow"], "FDI Net Inflow (% of GDP)")
Figure4.3 <- visualize_series(data_ts[, "popl_grate"], "Population Growth Rate")
Figure4.4 <- visualize_series(data_ts[, "elect_access"], "Access to Electricity (% of Population)")
Figure4.5 <- visualize_series(data_ts[, "infl_rate"], "Inflation Rate")

# Perform Stationarity Checks
stationarity_tests <- function(time_series, variable_name) {
  adf_test <- adf.test(time_series, alternative = "stationary")
  pp_test <- pp.test(time_series)
  kpss_test <- kpss.test(time_series, null = "Level")
  
  cat("Stationarity tests for", variable_name, "\n")
  cat("ADF test: Statistic =", adf_test$statistic, ", P-value =", adf_test$p.value, "\n")
  cat("PP test: Statistic =", pp_test$statistic, ", P-value =", pp_test$p.value, "\n")
  cat("KPSS test: Statistic =", kpss_test$statistic, ", P-value =", kpss_test$p.value, "\n\n")
}

#Stationarity tests for GDP Growth Rate
stationarity_tests(data_ts[, "gdp_grate"], "GDP Growth Rate")
stationarity_tests(data_ts[, "fdi_inflow"], "FDI Inflow as Percent of GDP")
stationarity_tests(data_ts[, "popl_grate"], "Population Growth Rate")
stationarity_tests(data_ts[, "elect_access"], "Electricity Access")
stationarity_tests(data_ts[, "infl_rate"], "Inflation Rate")

# First level differencing  to achieve stationarity
data_diff <- diff(data_ts)

# Perform stationarity tests on differenced data
stationarity_tests(diff(data_ts[, "gdp_grate"]), "Differenced GDP Growth Rate")
stationarity_tests(diff(data_ts[, "fdi_inflow"]), "Differenced FDI Inflow as Percent of GDP")
stationarity_tests(diff(data_ts[, "popl_grate"]), "Differenced Populatio Growth Rate")
stationarity_tests(diff(data_ts[, "elect_access"]), "Differenced Electricity Access")
stationarity_tests(diff(data_ts[, "infl_rate"]), "Differenced Inflation Rate")

# Second level differencing to achieve stationarity
data_diff2 <- diff(data_diff)

# Perform stationarity tests on second differenced data
stationarity_tests(data_diff2[, "gdp_grate"], "Second Differenced GDP Growth Rate")
stationarity_tests(data_diff2[, "fdi_inflow"], "Second Differenced FDI Inflow as Percent of GDP")
stationarity_tests(data_diff2[, "popl_grate"], "Second Differenced Population Growth Rate")
stationarity_tests(data_diff2[, "elect_access"], "Second Differenced Electricity Access")
stationarity_tests(data_diff2[, "infl_rate"], "Second Differenced Inflation Rate")

#################### Plots time series at second level differencing ###############################
visualize_series2 <- function(ts_data, variable_name) {
  df <- data.frame(Year = time(ts_data), Value = ts_data)
  p <- ggplot(df, aes(x = Year, y = Value)) +
    geom_line(size = 0.7, col = "darkblue") + 
    geom_smooth(method = "lm", col = "black", size = 0.7, se = FALSE) +
    ggtitle(paste(variable_name)) +
    xlab(NULL) +
    ylab(NULL) +
    scale_x_continuous(breaks = seq(floor(min(time(ts_data))), ceiling(max(time(ts_data))), by = 3)) +
    theme(
      plot.title = element_text(face = "bold",size = 13, hjust = 0.5),
      axis.title.x = element_text(face = "bold", size = 11),
      axis.title.y = element_text(face = "bold", size = 11),
      axis.text.x = element_text(face = "bold", size = 10),
      axis.text.y = element_text(face = "bold", size = 10),
      panel.border = element_rect(colour = "black", fill = NA, size = 1),
      panel.background = element_rect(fill = "white"),  
      plot.background = element_rect(fill = "white"),
      #panel.grid.minor.y = element_line(color = "grey90"),
      #panel.grid.major.y = element_line(color = "grey90")
    )
  print(p)
}

# Figure 4.2 Visualised stationary series at second level differencing
visualize_series2(data_diff2[, "gdp_grate"], "Second Difference GDP Growth Rate")
visualize_series2(data_diff2[, "fdi_inflow"], "Second Differenced FDI Net Inflow (% of GDP)")
visualize_series2(data_diff2[, "popl_grate"], "Second Differenced Population Growth Rate")
visualize_series2(data_diff2[, "elect_access"], "Second Differenced Access to Electricity (% of Population)")
visualize_series2(data_diff2[, "infl_rate"], "Second Differenced Inflation Rate")

# Determine optimal lag length
lag_selection <- VARselect(data_diff2, lag.max = 2, type = "const")

# Print the lag selection criteria
print(lag_selection)

# Use lags as indicated by AIC, HQ, and FPE
optimal_lag <- 2

# Fit VAR model
var_model <- VAR(data_diff2, p = optimal_lag, type = "const")
summary(var_model)

# Extract VAR Model results and save to text file
var_summary <- capture.output(summary(var_model))
writeLines(var_summary, "VAR_model_summary.txt")

########################################################################
# 6.0 Perform diagnostic checks 
########################################################################

########## Extract the roots of the characteristic polynomial #########
roots <- roots(var_model)

# Figure 4.3: The roots of the characteristics of polynomial
# Define plot limits
plot_limit <- 1.2

# Open a new plot window with square aspect ratio
par(pty = "s")

# Plot the roots on the complex plane
plot(Re(roots), Im(roots), type = "p", pch = 19, xlim = c(-plot_limit, plot_limit), ylim = c(-plot_limit, plot_limit),
     xlab = "Real Part", ylab = "Imaginary Part",
     main = "Roots of the Characteristic Polynomial")

# Add the unit circle for reference
symbols(0, 0, circles = 1, inches = FALSE, add = TRUE, fg = "darkblue")

# Add lines at 0 for better visualization
abline(h = 0, v = 0, col = "grey")

# Print the roots for confirmation
print(roots)

########################### Serial correlation test ###################
serial_test <- serial.test(var_model, lags.pt = 16, type = "PT.asymptotic")
print(serial_test)

##################### Normality Residual Test #########################
norm_test <- normality.test(var_model)
print(norm_test)

# Figure 4.4: Plots of normality of residuals test
residuals <- resid(var_model)
par(mfrow = c(2, 3))

for (i in 1:ncol(residuals)) {
  qqnorm(residuals[, i], main = colnames(residuals)[i])
  qqline(residuals[, i], col = "red")
}

########################### Heteroskedasticity Test ##############################

arch_test <- arch.test(var_model, lags.multi = 5, multivariate.only = TRUE)
print(arch_test)

####################### Perform Granger Causality tests ##########################
granger_gdp_grate <- causality(var_model, cause = "gdp_grate")
granger_fdi_inflow <- causality(var_model, cause = "fdi_inflow")

# Print Granger causality test results
print(granger_gdp_grate)
print(granger_fdi_inflow)

############################# Stability Analysis #############################
stability_result <- stability(var_model, type = "OLS-CUSUM")

# Set the plot parameters
par(cex.main = 1.0)  # Reduce the title font size

# Figure 4.5: Plots of OLS-CUSUM Test for Structural Breaks
cusum_plot <- plot(stability_result, plot.type = "single")

# Save the plot
ggsave("CUSUM_plot.png", cusum_plot, width = 8, height = 6)

#########################################################################
# 7.0 POST ESTIMATION ANALYSIS
#########################################################################

######################### Impulse Response Functions ####################
irf_result <- irf(var_model, n.ahead = 10, boot = TRUE)
print(irf_result)

# Custom plot function with manual axis intervals
plot_irf <- function(irf_result, impulse, response, xlim = NULL, ylim = NULL) {
  irf_data <- irf_result$irf[[impulse]][, response]
  lower <- irf_result$Lower[[impulse]][, response]
  upper <- irf_result$Upper[[impulse]][, response]
  time <- 1:length(irf_data)
  
# Create the plot without axes
  plot(time, irf_data, type = "l", xlim = xlim, ylim = ylim, main = paste("Impulse Response of", response, "to", impulse),
       xlab = "Time", ylab = "Response", col = "blue", lwd = 2, axes = FALSE)  # axes = FALSE prevents automatic axis drawing
  
# Manually add the axes with interval of 2
  axis(1, at = seq(xlim[1], xlim[2], by = 2))  # x-axis with intervals of 2
  axis(2, at = seq(ylim[1], ylim[2], by = 2))  # y-axis with intervals of 2
  
  # Add the confidence bands
  lines(time, lower, col = "red", lty = 2, lwd = 2)
  lines(time, upper, col = "red", lty = 2, lwd = 2)
  
  # Add the reference line
  abline(h = 0, col = "black", lty = 2)  # Add reference line at y = 0 
  
  # Add box around the plot
  box()
}

# Set function for custom axis intervals
par(mfrow = c(3, 2), cex.lab = 1.0, cex.main = 1.0)

xlim_custom <- c(1, 11)  # Custom x-axis range
ylim_custom <- c(-6, 6)  # Custom y-axis range

# Figure 4.6: Plots of Impulse Response Function
plot_irf(irf_result, impulse = "fdi_inflow", response = "fdi_inflow", xlim = xlim_custom, ylim = ylim_custom)
plot_irf(irf_result, impulse = "fdi_inflow", response = "gdp_grate", xlim = xlim_custom, ylim = ylim_custom)
plot_irf(irf_result, impulse = "gdp_grate", response = "gdp_grate", xlim = xlim_custom, ylim = ylim_custom)
plot_irf(irf_result, impulse = "gdp_grate", response = "fdi_inflow", xlim = xlim_custom, ylim = ylim_custom)
plot_irf(irf_result, impulse = "infl_rate", response = "gdp_grate", xlim = xlim_custom, ylim = ylim_custom)
plot_irf(irf_result, impulse = "infl_rate", response = "fdi_inflow", xlim = xlim_custom, ylim = ylim_custom)


##################### Perform Variance Decomposition #############################

#Figure 4.7: Plots of Forecast Error Variance Decomposition (FEVD)
fevd_results <- fevd(var_model, n.ahead = 10)
print(fevd_results)
png("fevd_plot.png", width = 800, height = 600)
plot(fevd_results)
dev.off()
