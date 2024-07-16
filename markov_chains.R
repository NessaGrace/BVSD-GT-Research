#' Description:
#'     Reads in initial state and transition probability data from a CSV file
#'     and uses it to construct a Markov chain and visualize the results.

#' Functions Used:
#'     * get_column() : if the values in one column are equal to a particular
#'                      value, return the associated values (values in the same
#'                      row) from the target column; used to read in the data
#'                      from the CSV file
#'                      
#'     * get_kb() : for a given input string in the Variable_Type column of the 
#'                  CSV file, return the values in the Variable_Value column
#'                  that are associated with the string; used to get the K_B
#'                  values to prime the vectors between years
#'                  
#'     * make_matrix() : takes in a data vector (resulting from the get_column()
#'                       function) and builds transition matrix

# Install packages and load libraries:
library("dplyr")
library("readxl")
library("ggplot2")
library("reshape2")

# Load the functions script for use in this script
source("functions_lib.R")

# Read in data from CSV file:
data <- read_xlsx("Initial_state_vector_&_transition_probability_parameters.xlsx", 
                  sheet=1, 
                  guess_max = 10000,
                  col_names=FALSE)

# Remove the first and second rows to have correct names of headers
data <- data[-c(1,2), ]

# Correct to avoid multiple columns with same name (not allowed in R) & remove
# footnote labels from headers and Race column
colnames(data) = c("Year_i-1_Tableau", "Year_i_Tableau", "Grade_i-1_Tableau",
                   "Grade_i_Tableau", "Race", "Percent_of_Total_i-1",
                   "Percent_of_Total_i", "Number_in_GT_i-1", "Number_in_GT_i",
                   "Date_of_Spreadsheet", "Year_i-1_Hist_Count",
                   "Year_i_Hist_Count", "Grade_i-1_Hist_Count",
                   "Grade_i_Hist_Count", "Count_i-1", "Count_i",
                   "Variable_Type", "Variable_Name", "Variable_Meaning",
                   "Variable_Value")
data[101, "Race"] = "Other (Oth)"

# Adjust to rounding down to nearest whole number for number of students only,
# not for transition probabilities
data <- data %>%
  mutate(Variable_Value = as.numeric(Variable_Value))

data <- data %>%
  mutate(Variable_Value = case_when(
    grepl("initial", Variable_Type) ~ round(Variable_Value, digits = 0),
    grepl("K_B", Variable_Type) ~ round(Variable_Value, digits = 0),
    TRUE ~ Variable_Value
  ))

data$Variable_Value <- format(data$Variable_Value, scientific = FALSE)

# Get data for initial state vectors into vector format:
W_i <- get_column(data, Variable_Type, "White initial", Variable_Value)
A_i <- get_column(data, Variable_Type, "Asian initial", Variable_Value)
L_i <- get_column(data, Variable_Type, "Latinx initial", Variable_Value)
TMR_i <- get_column(data, Variable_Type, "TMR initial", Variable_Value)
Oth_i <- get_column(data, Variable_Type, "Other initial", Variable_Value)

################################################################################
# Set up transition matrices Year 1
################################################################################

# Get data for yr 1 transition probabilities into vectors:
# White Year 1:
W_yr1 <- get_column(data, Variable_Type, "White yr1", Variable_Value)

# Asian Year 1:
A_yr1 <- get_column(data, Variable_Type, "Asian yr1", Variable_Value)

# Hispanic/Latinx Year 1:
L_yr1 <- get_column(data, Variable_Type, "Latinx yr1", Variable_Value)

# TMR Year 1:
TMR_yr1 <- get_column(data, Variable_Type, "TMR yr1", Variable_Value)

# Other Year 1:
Oth_yr1 <- get_column(data, Variable_Type, "Other yr1", Variable_Value)

# Create the transition matrices for year 1
# White Matrix:
mat_w_yr1 <- make_matrix(W_yr1)

# Asian Matrix Rows:
mat_a_yr1 <- make_matrix(A_yr1)

# Hispanic/Latinx Rows:
mat_L_yr1 <- make_matrix(L_yr1)

# TMR Rows:
mat_TMR_yr1 <- make_matrix(TMR_yr1)

# Other Rows:
mat_Oth_yr1 <- make_matrix(Oth_yr1)

################################################################################
# Set up transition matrices Year 2
################################################################################

# Get data for yr 2 transition probabilities into vectors:
# White Year 2:
W_yr2 <- get_column(data, Variable_Type, "White yr2", Variable_Value)

# Asian Year 2:
A_yr2 <- get_column(data, Variable_Type, "Asian yr2", Variable_Value)

# Hispanic/Latinx Year 2:
L_yr2 <- get_column(data, Variable_Type, "Latinx yr2", Variable_Value)

# TMR Year 2:
TMR_yr2 <- get_column(data, Variable_Type, "TMR yr2", Variable_Value)

# Other Year 2:
Oth_yr2 <- get_column(data, Variable_Type, "Other yr2", Variable_Value)

# Create transition matrices for year 2
# White Matrix:
mat_w_yr2 <- make_matrix(W_yr2)

# Asian Matrix Rows:
mat_a_yr2 <- make_matrix(A_yr2)

# Hispanic/Latinx Rows:
mat_L_yr2 <- make_matrix(L_yr2)

# TMR Rows:
mat_TMR_yr2 <- make_matrix(TMR_yr2)

# Other Rows:
mat_Oth_yr2 <- make_matrix(Oth_yr2)

################################################################################
# Set up transition matrices Year 3
################################################################################

# Get data for yr 3 transition probabilities into vectors:
# White Year 3:
W_yr3 <- get_column(data, Variable_Type, "White yr3", Variable_Value)

# Asian Year 3:
A_yr3 <- get_column(data, Variable_Type, "Asian yr3", Variable_Value)

# Hispanic/Latinx Year 3:
L_yr3 <- get_column(data, Variable_Type, "Latinx yr3", Variable_Value)

# TMR Year 3:
TMR_yr3 <- get_column(data, Variable_Type, "TMR yr3", Variable_Value)

# Other Year 3:
Oth_yr3 <- get_column(data, Variable_Type, "Other yr3", Variable_Value)

# Create transition matrices for year 3
# White Matrix:
mat_w_yr3 <- make_matrix(W_yr3)

# Asian Matrix Rows:
mat_a_yr3 <- make_matrix(A_yr3)

# Hispanic/Latinx Rows:
mat_L_yr3 <- make_matrix(L_yr3)

# TMR Rows:
mat_TMR_yr3 <- make_matrix(TMR_yr3)

# Other Rows:
mat_Oth_yr3 <- make_matrix(Oth_yr3)

################################################################################
# Set up transition matrices Year 4
################################################################################

# Get data for yr 4 transition probabilities into vectors:
# White Year 4:
W_yr4 <- get_column(data, Variable_Type, "White yr4", Variable_Value)

# Asian Year 4:
A_yr4 <- get_column(data, Variable_Type, "Asian yr4", Variable_Value)

# Hispanic/Latinx Year 4:
L_yr4 <- get_column(data, Variable_Type, "Latinx yr4", Variable_Value)

# TMR Year 4:
TMR_yr4 <- get_column(data, Variable_Type, "TMR yr4", Variable_Value)

# Other Year 4:
Oth_yr4 <- get_column(data, Variable_Type, "Other yr4", Variable_Value)

# Create transition matrices for year 4
# White Matrix:
mat_w_yr4 <- make_matrix(W_yr4)

# Asian Matrix Rows:
mat_a_yr4 <- make_matrix(A_yr4)

# Hispanic/Latinx Rows:
mat_L_yr4 <- make_matrix(L_yr4)

# TMR Rows:
mat_TMR_yr4 <- make_matrix(TMR_yr4)

# Other Rows:
mat_Oth_yr4 <- make_matrix(Oth_yr4)

################################################################################
# Set up transition matrices Year 5
################################################################################

# Get data for yr 5 transition probabilities into vectors:
# White Year 5:
W_yr5 <- get_column(data, Variable_Type, "White yr5", Variable_Value)

# Asian Year 5:
A_yr5 <- get_column(data, Variable_Type, "Asian yr5", Variable_Value)

# Hispanic/Latinx Year 5:
L_yr5 <- get_column(data, Variable_Type, "Latinx yr5", Variable_Value)

# TMR Year 5:
TMR_yr5 <- get_column(data, Variable_Type, "TMR yr5", Variable_Value)

# Other Year 5:
Oth_yr5 <- get_column(data, Variable_Type, "Other yr5", Variable_Value)

# Create transition matrices for year 5
# White Matrix:
mat_w_yr5 <- make_matrix(W_yr5)

# Asian Matrix Rows:
mat_a_yr5 <- make_matrix(A_yr5)

# Hispanic/Latinx Rows:
mat_L_yr5 <- make_matrix(L_yr5)

# TMR Rows:
mat_TMR_yr5 <- make_matrix(TMR_yr5)

# Other Rows:
mat_Oth_yr5 <- make_matrix(Oth_yr5)

################################################################################
# Set up transition matrices Year 6
################################################################################

# Get data for yr 6 transition probabilities into vectors:
# White Year 6:
W_yr6 <- get_column(data, Variable_Type, "White yr6", Variable_Value)

# Asian Year 6:
A_yr6 <- get_column(data, Variable_Type, "Asian yr6", Variable_Value)

# Hispanic/Latinx Year 6:
L_yr6 <- get_column(data, Variable_Type, "Latinx yr6", Variable_Value)

# TMR Year 6:
TMR_yr6 <- get_column(data, Variable_Type, "TMR yr6", Variable_Value)

# Other Year 6:
Oth_yr6 <- get_column(data, Variable_Type, "Other yr6", Variable_Value)

# Create transition matrices for year 6
# White Matrix:
mat_w_yr6 <- make_matrix(W_yr6)

# Asian Matrix Rows:
mat_a_yr6 <- make_matrix(A_yr6)

# Hispanic/Latinx Rows:
mat_L_yr6 <- make_matrix(L_yr6)

# TMR Rows:
mat_TMR_yr6 <- make_matrix(TMR_yr6)

# Other Rows:
mat_Oth_yr6 <- make_matrix(Oth_yr6)

################################################################################
#' Create Markov chain training set (this is the training set only since this is
#' created with existing data instead of making predictions. This can be used
#' to make predictions/create simulations into the future later on.)
################################################################################

# White training set across all 4 yrs:
# Year 1
ts_1_w <- W_i %*% mat_w_yr1  # output of this (ts_1_w) is student #'s @ end
                              # of 18-19 academic year

# Year 2
ts_2_w_kb <- get_kb("White yr2 K_B")
ts_2_w <- (ts_1_w + ts_2_w_kb) %*% mat_w_yr2

# Year 3
ts_3_w_kb <- get_kb("White yr3 K_B")
ts_3_w <- (ts_2_w + ts_3_w_kb) %*% mat_w_yr3

# Year 4
ts_4_w_kb <- get_kb("White yr4 K_B")
ts_4_w <- (ts_3_w + ts_4_w_kb) %*% mat_w_yr4

# Asian training set across all 4 yrs:
# Year 1
ts_1_a <- A_i %*% mat_a_yr1 # output of this (ts_1_a) is student #'s @ end
                             # of 18-19 academic year

# Year 2
ts_2_a_kb <- get_kb("Asian yr2 K_B")
ts_2_a <- (ts_1_a + ts_2_a_kb) %*% mat_a_yr2

# Year 3
ts_3_a_kb <- get_kb("Asian yr3 K_B")
ts_3_a <- (ts_2_a + ts_3_a_kb) %*% mat_a_yr3

# Year 4
ts_4_a_kb <- get_kb("Asian yr4 K_B")
ts_4_a <- (ts_3_a + ts_4_a_kb) %*% mat_a_yr4

# Latinx training set across all 4 yrs:
# Year 1
ts_1_L <- L_i %*% mat_L_yr1 # output of this (ts_1_L) is student #'s @ end
                             # of 18-19 academic year

# Year 2
ts_2_L_kb <- get_kb("Latinx yr2 K_B")
ts_2_L <- (ts_1_L + ts_2_L_kb) %*% mat_L_yr2

# Year 3
ts_3_L_kb <- get_kb("Latinx yr3 K_B")
ts_3_L <- (ts_2_L + ts_3_L_kb) %*% mat_L_yr3

# Year 4
ts_4_L_kb <- get_kb("Latinx yr4 K_B")
ts_4_L <- (ts_3_L + ts_4_L_kb) %*% mat_L_yr4

# TMR training set across all 4 yrs:
# Year 1
ts_1_TMR <- TMR_i %*% mat_TMR_yr1 # output of this (ts_1_TMR) is student #'s
                                   # @ end of 18-19 academic year

# Year 2
ts_2_TMR_kb <- get_kb("TMR yr2 K_B")
ts_2_TMR <- (ts_1_TMR + ts_2_TMR_kb) %*% mat_TMR_yr2

# Year 3
ts_3_TMR_kb <- get_kb("TMR yr3 K_B")
ts_3_TMR <- (ts_2_TMR + ts_3_TMR_kb) %*% mat_TMR_yr3

# Year 4
ts_4_TMR_kb <- get_kb("TMR yr4 K_B")
ts_4_TMR <- (ts_3_TMR + ts_4_TMR_kb) %*% mat_TMR_yr4

# Other training set across all 4 yrs:
# Year 1
ts_1_Oth <- Oth_i %*% mat_Oth_yr1 # output of this (ts_1_Oth) is student #'s @
                                   # end of 18-19

# Year 2
ts_2_Oth_kb <- get_kb("Other yr2 K_B")
ts_2_Oth <- (ts_1_Oth + ts_2_Oth_kb) %*% mat_Oth_yr2

# Year 3
ts_3_Oth_kb <- get_kb("Other yr3 K_B")
ts_3_Oth <- (ts_2_Oth + ts_3_Oth_kb) %*% mat_Oth_yr3

# Year 4
ts_4_Oth_kb <- get_kb("Other yr4 K_B")
ts_4_Oth <- (ts_3_Oth + ts_4_Oth_kb) %*% mat_Oth_yr4

################################################################################
# Data visualization
################################################################################

# Graphing the results for the kindergartners who enter in Year 1 through Year
# 4, when they are in 3rd grade. This is tracking the 18-19 entry class of GT 
# students over the four years for which we have data: 

# Find the total numbers of GT students in grades K-3 in years 1-4, 
# respectively:
year <- c(1, 2, 3, 4)
total_gt_pop_18_19_yr1 <- ts_1_w[2] + ts_1_a[2] + ts_1_L[2] + ts_1_TMR[2] + 
  ts_1_Oth[2]
                            
total_gt_pop_18_19_yr2 <- ts_2_w[4] + ts_2_a[4] + ts_2_L[4] + ts_2_TMR[4] + 
  ts_2_Oth[4]
                            
total_gt_pop_18_19_yr3 <- ts_3_w[6] + ts_3_a[6] + ts_3_L[6] + ts_3_TMR[6] + 
  ts_3_Oth[6]
                                                                                          
total_gt_pop_18_19_yr4 <- ts_4_w[8] + ts_4_a[8] + ts_4_L[8] + ts_4_TMR[8] + 
  ts_4_Oth[8] 

# WHITE:

# Find percent of White students in GT in the 18-19 class:
white_gt_pop_18_19 <- c(100*(ts_1_w[2]/total_gt_pop_18_19_yr1), 
                          100*(ts_2_w[4]/total_gt_pop_18_19_yr2),
                          100*(ts_3_w[6]/total_gt_pop_18_19_yr3),
                          100*(ts_4_w[8]/total_gt_pop_18_19_yr4)) 

# Put White 18-19 class percents in data frame for graphing
df_w_18_19 <- data.frame(year, white_gt_pop_18_19)

# Plot the results
ggplot(df_w_18_19, aes(year, white_gt_pop_18_19)) +
  geom_point(size=3) + 
  labs(title="2018-19 Class of White GT Students, Grades K-3",
       x="Year", y="% of GT Population") +
  theme(plot.title = element_text(size = 20)) +
  theme(axis.title = element_text(size = 15)) +
  theme(axis.text = element_text(size = 13))

# ASIAN:

# Find percent of Asian students in GT in the 18-19 class:
asian_gt_pop_18_19 <- c(100*(ts_1_a[2]/total_gt_pop_18_19_yr1), 
                          100*(ts_2_a[4]/total_gt_pop_18_19_yr2),
                          100*(ts_3_a[6]/total_gt_pop_18_19_yr3),
                          100*(ts_4_a[8]/total_gt_pop_18_19_yr4)) 

# Put Asian 18-19 class percents in data frame for graphing
df_a_18_19 <- data.frame(year, asian_gt_pop_18_19)

# Plot the results
ggplot(df_a_18_19, aes(year, asian_gt_pop_18_19)) +
  geom_point(size=3) + 
  labs(title="2018-19 Class of Asian GT Students, Grades K-3",
       x="Year", y="% of GT Population") +
  theme(plot.title = element_text(size = 20)) +
  theme(axis.title = element_text(size = 15)) +
  theme(axis.text = element_text(size = 13))

# HISPANIC/LATINX:

# Find percent of Latinx students in GT in the 18-19 class:
latinx_gt_pop_18_19 <- c(100*(ts_1_L[2]/total_gt_pop_18_19_yr1), 
                           100*(ts_2_L[4]/total_gt_pop_18_19_yr2),
                           100*(ts_3_L[6]/total_gt_pop_18_19_yr3),
                           100*(ts_4_L[8]/total_gt_pop_18_19_yr4))

# Put Latinx 18-19 class percents in data frame for graphing
df_L_18_19 <- data.frame(year, latinx_gt_pop_18_19)

# Plot the results
ggplot(df_L_18_19, aes(year, latinx_gt_pop_18_19)) +
  geom_point(size=3) +
  labs(title="2018-19 Class of Latinx GT Students, Grades K-3",
       x="Year", y="% of GT Population") +
  theme(plot.title = element_text(size = 20)) +
  theme(axis.title = element_text(size = 15)) +
  theme(axis.text = element_text(size = 13))

# TMR:

# Find percent of TMR students in GT in the 18-19 class:
TMR_gt_pop_18_19 <- c(100*(ts_1_TMR[2]/total_gt_pop_18_19_yr1), 
                        100*(ts_2_TMR[4]/total_gt_pop_18_19_yr2),
                        100*(ts_3_TMR[6]/total_gt_pop_18_19_yr3),
                        100*(ts_4_TMR[8]/total_gt_pop_18_19_yr4)) 

# Put TMR 18-19 class percents in data frame for graphing
df_TMR_18_19 <- data.frame(year, TMR_gt_pop_18_19)

# Plot the results
ggplot(df_TMR_18_19, aes(year, TMR_gt_pop_18_19)) +
  geom_point(size=3) + 
  labs(title="2018-19 Class of TMR GT Students, Grades K-3",
       x="Year", y="% of GT Population") +
  theme(plot.title = element_text(size = 20)) +
  theme(axis.title = element_text(size = 15)) +
  theme(axis.text = element_text(size = 13))

# Other:

# Find percent of Other students in GT in the 18-19 class:
other_gt_pop_18_19 <- c(ts_1_Oth[2]/total_gt_pop_18_19_yr1, 
                          ts_2_Oth[4]/total_gt_pop_18_19_yr2,
                          ts_3_Oth[6]/total_gt_pop_18_19_yr3,
                          ts_4_Oth[8]/total_gt_pop_18_19_yr4) 

# Put Other 18-19 class percents in data frame for graphing
df_other_18_19 <- data.frame(year, other_gt_pop_18_19)

# Plot the results
ggplot(df_other_18_19, aes(year, other_gt_pop_18_19)) +
  geom_point(size=3) + 
  labs(title="2018-19 Class of Other GT Students, Grades K-3",
       x="Year", y="% of GT Population") +
  theme(plot.title = element_text(size = 20)) +
  theme(axis.title = element_text(size = 15)) +
  theme(axis.text = element_text(size = 13))

# Plot the 18-19 class over the 4 yrs in one plot together:

# Create data frame for plotting
df_all_races_18_19 <- data.frame(year, 
                                     all_races_18_19=c(white_gt_pop_18_19, 
                                                           asian_gt_pop_18_19,
                                                           latinx_gt_pop_18_19,
                                                           TMR_gt_pop_18_19,
                                                           other_gt_pop_18_19),
                                     Race = c("White", "White", "White", "White", 
                                              "Asian", "Asian", "Asian", "Asian", 
                                              "Latinx", "Latinx", "Latinx", "Latinx",
                                              "Two or more", "Two or more", "Two or more", "Two or more",
                                              "Other", "Other", "Other", "Other"))

# Plot the results
ggplot(df_all_races_18_19, aes(x=year, y=all_races_18_19, color=Race)) +
  geom_point(size=3) +
  labs(title="2018-19 Class of GT Students, Grades K-3",
       x="Year", y="% of GT Population") +
  theme(plot.title = element_text(size = 20)) +
  theme(axis.title = element_text(size = 15)) +
  theme(axis.text = element_text(size = 13)) + 
  theme(legend.text = element_text(size = 13)) +
  theme(legend.title = element_text(size = 15)) +
  theme(legend.position ="bottom")

################################################################################

#' This is for a different type of visualization. Now we are focusing on the
#' entire GT population and its composition over the 4 yrs.

# Finds the total GT population for all races and grades for each year
overall_gt_population_yr1 <- ts_1_w[2] + ts_1_w[4] + ts_1_w[6] + ts_1_w[8] +
                             ts_1_w[10] + ts_1_w[12] + ts_1_w[14] + 
                             ts_1_a[2] + ts_1_a[4] + ts_1_a[6] + ts_1_a[8] +
                             ts_1_a[10] + ts_1_a[12] + ts_1_a[14] + 
                             ts_1_L[2] + ts_1_L[4] + ts_1_L[6] + ts_1_L[8] +
                             ts_1_L[10] + ts_1_L[12] + ts_1_L[14] + 
                             ts_1_TMR[2] + ts_1_TMR[4] + ts_1_TMR[6] + ts_1_TMR[8] +
                             ts_1_TMR[10] + ts_1_TMR[12] + ts_1_TMR[14] + 
                             ts_1_Oth[2] + ts_1_Oth[4] + ts_1_Oth[6] + ts_1_Oth[8] +
                             ts_1_Oth[10] + ts_1_Oth[12] + ts_1_Oth[14] 

overall_gt_population_yr2 <- ts_2_w[2] + ts_2_w[4] + ts_2_w[6] + ts_2_w[8] +
                             ts_2_w[10] + ts_2_w[12] + ts_2_w[14] + 
                             ts_2_a[2] + ts_2_a[4] + ts_2_a[6] + ts_2_a[8] +
                             ts_2_a[10] + ts_2_a[12] + ts_2_a[14] + 
                             ts_2_L[2] + ts_2_L[4] + ts_2_L[6] + ts_2_L[8] +
                             ts_2_L[10] + ts_2_L[12] + ts_2_L[14] + 
                             ts_2_TMR[2] + ts_2_TMR[4] + ts_2_TMR[6] + ts_2_TMR[8] +
                             ts_2_TMR[10] + ts_2_TMR[12] + ts_2_TMR[14] + 
                             ts_2_Oth[2] + ts_2_Oth[4] + ts_2_Oth[6] + ts_2_Oth[8] +
                             ts_2_Oth[10] + ts_2_Oth[12] + ts_2_Oth[14] 

overall_gt_population_yr3 <- ts_3_w[2] + ts_3_w[4] + ts_3_w[6] + ts_3_w[8] +
                             ts_3_w[10] + ts_3_w[12] + ts_3_w[14] + 
                             ts_3_a[2] + ts_3_a[4] + ts_3_a[6] + ts_3_a[8] +
                             ts_3_a[10] + ts_3_a[12] + ts_3_a[14] + 
                             ts_3_L[2] + ts_3_L[4] + ts_3_L[6] + ts_3_L[8] +
                             ts_3_L[10] + ts_3_L[12] + ts_3_L[14] + 
                             ts_3_TMR[2] + ts_3_TMR[4] + ts_3_TMR[6] + ts_3_TMR[8] +
                             ts_3_TMR[10] + ts_3_TMR[12] + ts_3_TMR[14] + 
                             ts_3_Oth[2] + ts_3_Oth[4] + ts_3_Oth[6] + ts_3_Oth[8] +
                             ts_3_Oth[10] + ts_3_Oth[12] + ts_3_Oth[14] 

overall_gt_population_yr4 <- ts_4_w[2] + ts_4_w[4] + ts_4_w[6] + ts_4_w[8] +
                             ts_4_w[10] + ts_4_w[12] + ts_4_w[14] + 
                             ts_4_a[2] + ts_4_a[4] + ts_4_a[6] + ts_4_a[8] +
                             ts_4_a[10] + ts_4_a[12] + ts_4_a[14] + 
                             ts_4_L[2] + ts_4_L[4] + ts_4_L[6] + ts_4_L[8] +
                             ts_4_L[10] + ts_4_L[12] + ts_4_L[14] + 
                             ts_4_TMR[2] + ts_4_TMR[4] + ts_4_TMR[6] + ts_4_TMR[8] +
                             ts_4_TMR[10] + ts_4_TMR[12] + ts_4_TMR[14] + 
                             ts_4_Oth[2] + ts_4_Oth[4] + ts_4_Oth[6] + ts_4_Oth[8] +
                             ts_4_Oth[10] + ts_4_Oth[12] + ts_4_Oth[14]

# Finds the fraction of students in each racial group in yr 1 out of the total
# number of students in GT in yr 1
overall_white_yr1 <- (ts_1_w[2] + ts_1_w[4] + ts_1_w[6] + ts_1_w[8] +
                     ts_1_w[10] + ts_1_w[12] + ts_1_w[14])/overall_gt_population_yr1

overall_asian_yr1 <- (ts_1_a[2] + ts_1_a[4] + ts_1_a[6] + ts_1_a[8] +
                     ts_1_a[10] + ts_1_a[12] + ts_1_a[14])/overall_gt_population_yr1

overall_latinx_yr1 <- (ts_1_L[2] + ts_1_L[4] + ts_1_L[6] + ts_1_L[8] +
                      ts_1_L[10] + ts_1_L[12] + ts_1_L[14])/overall_gt_population_yr1

overall_TMR_yr1 <- (ts_1_TMR[2] + ts_1_TMR[4] + ts_1_TMR[6] + ts_1_TMR[8] +
                     ts_1_TMR[10] + ts_1_TMR[12] + ts_1_TMR[14])/overall_gt_population_yr1

overall_other_yr1 <- (ts_1_Oth[2] + ts_1_Oth[4] + ts_1_Oth[6] + ts_1_Oth[8] +
                     ts_1_Oth[10] + ts_1_Oth[12] + ts_1_Oth[14])/overall_gt_population_yr1

# Finds the fraction of students in each racial group in yr 2 out of the total
# number of students in GT in yr 2
overall_white_yr2 <- (ts_2_w[2] + ts_2_w[4] + ts_2_w[6] + ts_2_w[8] +
  ts_2_w[10] + ts_2_w[12] + ts_2_w[14])/overall_gt_population_yr2

overall_asian_yr2 <- (ts_2_a[2] + ts_2_a[4] + ts_2_a[6] + ts_2_a[8] +
  ts_2_a[10] + ts_2_a[12] + ts_2_a[14])/overall_gt_population_yr2

overall_latinx_yr2 <- (ts_2_L[2] + ts_2_L[4] + ts_2_L[6] + ts_2_L[8] +
  ts_2_L[10] + ts_2_L[12] + ts_2_L[14])/overall_gt_population_yr2

overall_TMR_yr2 <- (ts_2_TMR[2] + ts_2_TMR[4] + ts_2_TMR[6] + ts_2_TMR[8] +
  ts_2_TMR[10] + ts_2_TMR[12] + ts_2_TMR[14])/overall_gt_population_yr2

overall_other_yr2 <- (ts_2_Oth[2] + ts_2_Oth[4] + ts_2_Oth[6] + ts_2_Oth[8] +
  ts_2_Oth[10] + ts_2_Oth[12] + ts_2_Oth[14])/overall_gt_population_yr2

# Finds the fraction of students in each racial group in yr 3 out of the total
# number of students in GT in yr 3
overall_white_yr3 <- (ts_3_w[2] + ts_3_w[4] + ts_3_w[6] + ts_3_w[8] +
  ts_3_w[10] + ts_3_w[12] + ts_3_w[14])/overall_gt_population_yr3

overall_asian_yr3 <- (ts_3_a[2] + ts_3_a[4] + ts_3_a[6] + ts_3_a[8] +
  ts_3_a[10] + ts_3_a[12] + ts_3_a[14])/overall_gt_population_yr3

overall_latinx_yr3 <- (ts_3_L[2] + ts_3_L[4] + ts_3_L[6] + ts_3_L[8] +
  ts_3_L[10] + ts_3_L[12] + ts_3_L[14])/overall_gt_population_yr3

overall_TMR_yr3 <- (ts_3_TMR[2] + ts_3_TMR[4] + ts_3_TMR[6] + ts_3_TMR[8] +
  ts_3_TMR[10] + ts_3_TMR[12] + ts_3_TMR[14])/overall_gt_population_yr3

overall_other_yr3 <- (ts_3_Oth[2] + ts_3_Oth[4] + ts_3_Oth[6] + ts_3_Oth[8] +
  ts_3_Oth[10] + ts_3_Oth[12] + ts_3_Oth[14])/overall_gt_population_yr3

# Finds the fraction of students in each racial group in yr 4 out of the total
# number of students in GT in yr 4
overall_white_yr4 <- (ts_4_w[2] + ts_4_w[4] + ts_4_w[6] + ts_4_w[8] +
  ts_4_w[10] + ts_4_w[12] + ts_4_w[14])/overall_gt_population_yr4

overall_asian_yr4 <- (ts_4_a[2] + ts_4_a[4] + ts_4_a[6] + ts_4_a[8] +
  ts_4_a[10] + ts_4_a[12] + ts_4_a[14])/overall_gt_population_yr4

overall_latinx_yr4 <- (ts_4_L[2] + ts_4_L[4] + ts_4_L[6] + ts_4_L[8] +
  ts_4_L[10] + ts_4_L[12] + ts_4_L[14])/overall_gt_population_yr4

overall_TMR_yr4 <- (ts_4_TMR[2] + ts_4_TMR[4] + ts_4_TMR[6] + ts_4_TMR[8] +
  ts_4_TMR[10] + ts_4_TMR[12] + ts_4_TMR[14])/overall_gt_population_yr4

overall_other_yr4 <- (ts_4_Oth[2] + ts_4_Oth[4] + ts_4_Oth[6] + ts_4_Oth[8] +
  ts_4_Oth[10] + ts_4_Oth[12] + ts_4_Oth[14])/overall_gt_population_yr4

# Finds the percent of each racial group in GT over all 4 yrs
overall_white_all_years <- c(overall_white_yr1*100, 
                             overall_white_yr2*100, 
                             overall_white_yr3*100, 
                             overall_white_yr4*100)

overall_asian_all_years <- c(overall_asian_yr1*100, 
                             overall_asian_yr2*100, 
                             overall_asian_yr3*100, 
                             overall_asian_yr4*100)

overall_latinx_all_years <- c(overall_latinx_yr1*100, 
                             overall_latinx_yr2*100, 
                             overall_latinx_yr3*100, 
                             overall_latinx_yr4*100)

overall_TMR_all_years <- c(overall_TMR_yr1*100, 
                             overall_TMR_yr2*100, 
                             overall_TMR_yr3*100, 
                             overall_TMR_yr4*100)

overall_other_all_years <- c(overall_other_yr1*100, 
                           overall_other_yr2*100, 
                           overall_other_yr3*100, 
                           overall_other_yr4*100)

# Makes data frame to graph GT racial/ethnic composition over the 4 yrs
df_overall <- data.frame(year=c('2018-19', '2019-20', '2020-21', '2021-22'), 
                         percent_gt=c(overall_white_all_years,
                                        overall_asian_all_years,
                                        overall_latinx_all_years,
                                        overall_TMR_all_years,
                                        overall_other_all_years),
                         Race=c("White", "White", "White", "White", 
                                "Asian", "Asian", "Asian", "Asian", 
                                "Latinx", "Latinx", "Latinx", "Latinx",
                                "Two or more", "Two or more", "Two or more", "Two or more",
                                "Other", "Other", "Other", "Other"))
# Plots the results
ggplot(df_overall, aes(x=year, y=percent_gt, color=Race)) +
  geom_point(size=5) +
  labs(title="District-Wide GT Racial Composition, 2018-19 to 2021-22",
       x="Year", y="% of GT Population") +
  theme(plot.title = element_text(size = 21)) +
  theme(axis.title = element_text(size = 17)) +
  theme(axis.text = element_text(size = 15)) + 
  theme(legend.text = element_text(size = 15)) +
  theme(legend.title = element_text(size = 17)) +
  theme(legend.position ="bottom")  

###############################################################################

# This is another type of visualization. Now we are focusing on the 
# representation index (RI) plots. Recall: RI = % gifted / % general

# Year 1 RI's:
RI_w_yr1 <- (overall_white_yr1*100) / 67.5 # denominators come from BVSD Metrics
                                           # data online for all grades in 
                                           # 2017-18
RI_a_yr1 <- (overall_asian_yr1*100) / 5.7

RI_L_yr1 <- (overall_latinx_yr1*100) / 19.6

RI_TMR_yr1 <- (overall_TMR_yr1*100) / 5.8

RI_Oth_yr1 <- (overall_other_yr1*100) / 1.4

# Create vector of all RI's for year 1
RI_yr1 <- c(RI_w_yr1,
            RI_a_yr1,
            RI_L_yr1,
            RI_TMR_yr1,
            RI_Oth_yr1)

# Create data frame for yr 1 RI's for plotting
df_RI_yr1 <- data.frame(Race=c("White", "Asian", "Latinx", "Two or more", "Other"), RI = RI_yr1)

# Plot results
ggplot(df_RI_yr1, aes(x=Race, y=RI)) +
  geom_col() +
  ggtitle("Representation Index (RI) in BVSD, 2018-19") +
  geom_text(aes(label=round(RI, digits=2), vjust=-0.5), size=4.5) +
  theme(plot.title = element_text(size = 19)) +
  theme(axis.title = element_text(size = 15)) +
  theme(axis.text = element_text(size = 13))

# Year 2 RI's:
RI_w_yr2 <- (overall_white_yr2*100) / 67.3

RI_a_yr2 <- (overall_asian_yr2*100) / 5.8

RI_L_yr2 <- (overall_latinx_yr2*100) / 19.5

RI_TMR_yr2 <- (overall_TMR_yr2*100) / 6.0

RI_Oth_yr2 <- (overall_other_yr2*100) / 1.3

# Create vector of all RI's for yr 2
RI_yr2 <- c(RI_w_yr2,
            RI_a_yr2,
            RI_L_yr2,
            RI_TMR_yr2,
            RI_Oth_yr2)

# Create data frame to plot yr2 RI's:
df_RI_yr2 <- data.frame(Race=c("White", "Asian", "Latinx", "Two or more", "Other"), RI = RI_yr2)

# Plot results
ggplot(df_RI_yr2, aes(x=Race, y=RI)) +
  geom_col() +
  ggtitle("Representation Index (RI) in BVSD, 2019-20") +
  geom_text(aes(label=round(RI, digits=2), vjust=-0.5), size=4.5) +
  theme(plot.title = element_text(size = 19)) +
  theme(axis.title = element_text(size = 15)) +
  theme(axis.text = element_text(size = 13))

# Year 3 RI's:
RI_w_yr3 <- (overall_white_yr3*100) / 67.0

RI_a_yr3 <- (overall_asian_yr3*100) / 5.7

RI_L_yr3 <- (overall_latinx_yr3*100) / 19.6

RI_TMR_yr3 <- (overall_TMR_yr3*100) / 6.3

RI_Oth_yr3 <- (overall_other_yr3*100) / 1.4

# Create vector of all yr 3 RI's
RI_yr3 <- c(RI_w_yr3,
            RI_a_yr3,
            RI_L_yr3,
            RI_TMR_yr3,
            RI_Oth_yr3)

# Create data frame to plot yr 3  RI's:
df_RI_yr3 <- data.frame(Race=c("White", "Asian", "Latinx", "Two or more", "Other"), RI = RI_yr3)

# plot results
ggplot(df_RI_yr3, aes(x=Race, y=RI)) +
  geom_col() +
  ggtitle("Representation Index (RI) in BVSD, 2020-21") +
  geom_text(aes(label=round(RI, digits=2), vjust=-0.5), size=4.5) +
  theme(plot.title = element_text(size = 19)) +
  theme(axis.title = element_text(size = 15)) +
  theme(axis.text = element_text(size = 13))

# Year 4 RI's:
RI_w_yr4 <- (overall_white_yr4*100) / 66.3

RI_a_yr4 <- (overall_asian_yr4*100) / 5.7

RI_L_yr4 <- (overall_latinx_yr4*100) / 20.1

RI_TMR_yr4 <- (overall_TMR_yr4*100) / 6.5

RI_Oth_yr4 <- (overall_other_yr4*100) / 1.4

# Create vector of all yr 3 RI's
RI_yr4 <- c(RI_w_yr4,
            RI_a_yr4,
            RI_L_yr4,
            RI_TMR_yr4,
            RI_Oth_yr4)

# Create data frame of all yr 3 RI's to plot:
df_RI_yr4 <- data.frame(Race=c("White", "Asian", "Latinx", "Two or more", "Other"), RI = RI_yr4)

# Plot results
ggplot(df_RI_yr4, aes(x=Race, y=RI)) +
  geom_col() +
  ggtitle("Representation Index (RI) in BVSD, 2021-22") +
  geom_text(aes(label=round(RI, digits=2), vjust=-0.5), size=4.5) +
  theme(plot.title = element_text(size = 19)) +
  theme(axis.title = element_text(size = 15)) +
  theme(axis.text = element_text(size = 13))

# make data frame of all RI values over the 4 yrs to prepare to plot:
df_RI <- data.frame(year=c('2018-19','2018-19','2018-19','2018-19','2018-19',
                           '2019-20','2019-20', '2019-20', '2019-20', '2019-20',
                           '2020-21', '2020-21', '2020-21', '2020-21', '2020-21',
                           '2021-22', '2021-22', '2021-22', '2021-22', '2021-22'),
                    RI=c(RI_yr1, RI_yr2, RI_yr3, RI_yr4),
                    Race=c("White", "Asian", "Latinx", "Two or more", "Other"))

# plot results
ggplot(df_RI, aes(x=year, y=RI, color=Race)) +
  geom_point(size=5) +
  labs(title="Representation Index (RI) in BVSD, 2018-19 to 2021-22", x="Year", y="RI") +
  theme(plot.title = element_text(size = 21)) +
  theme(axis.title = element_text(size = 17)) +
  theme(axis.text = element_text(size = 15)) +
  theme(legend.text = element_text(size = 15)) +
  theme(legend.title = element_text(size = 17)) +
  theme(legend.position ="bottom")