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
#'                       
#'     * run_sims() : takes in an initial state vector and list of transition
#'                    matrices to run simulations of the model
#'                    
#'     * trans_prob_extractor() : get transition probabilities for all years from 
#'                                a list of matrices for transitions K to 3
#'                                
#'     * average_trans_prob() : find average of list of transition probabilities                            

# Install packages and load libraries:
library("dplyr")
library("readxl")
library("ggplot2")
library("reshape2")
library("xtable")
library("knitr")

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

# Set names for rows and columns of transition matrices, to be used for all
# transition matrices:
row_col_names = c("K_B","K_GT","K_NGT","1_GT","1_NGT","2_GT", "2_NGT","3_GT",
                  "3_NGT","4_GT","4_NGT","5_GT","5_NGT","6_GT","6_NGT","7_GT",
                  "7_NGT","8_GT","8_NGT","9_GT","9_NGT","10_GT","10_NGT",
                  "11_GT","11_NGT","12_GT","12_NGT")

# Create the transition matrices for year 1
# White Matrix:
mat_w_yr1 <- make_matrix(W_yr1, row_col_names)

# Asian Matrix Rows:
mat_a_yr1 <- make_matrix(A_yr1, row_col_names)

# Hispanic/Latinx Rows:
mat_L_yr1 <- make_matrix(L_yr1, row_col_names)

# TMR Rows:
mat_TMR_yr1 <- make_matrix(TMR_yr1, row_col_names)

# Other Rows:
mat_Oth_yr1 <- make_matrix(Oth_yr1, row_col_names)

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
mat_w_yr2 <- make_matrix(W_yr2, row_col_names)

# Asian Matrix Rows:
mat_a_yr2 <- make_matrix(A_yr2, row_col_names)

# Hispanic/Latinx Rows:
mat_L_yr2 <- make_matrix(L_yr2, row_col_names)

# TMR Rows:
mat_TMR_yr2 <- make_matrix(TMR_yr2, row_col_names)

# Other Rows:
mat_Oth_yr2 <- make_matrix(Oth_yr2, row_col_names)

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
mat_w_yr3 <- make_matrix(W_yr3, row_col_names)

# Asian Matrix Rows:
mat_a_yr3 <- make_matrix(A_yr3, row_col_names)

# Hispanic/Latinx Rows:
mat_L_yr3 <- make_matrix(L_yr3, row_col_names)

# TMR Rows:
mat_TMR_yr3 <- make_matrix(TMR_yr3, row_col_names)

# Other Rows:
mat_Oth_yr3 <- make_matrix(Oth_yr3, row_col_names)

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
mat_w_yr4 <- make_matrix(W_yr4, row_col_names)

# Asian Matrix Rows:
mat_a_yr4 <- make_matrix(A_yr4, row_col_names)

# Hispanic/Latinx Rows:
mat_L_yr4 <- make_matrix(L_yr4, row_col_names)

# TMR Rows:
mat_TMR_yr4 <- make_matrix(TMR_yr4, row_col_names)

# Other Rows:
mat_Oth_yr4 <- make_matrix(Oth_yr4, row_col_names)

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
mat_w_yr5 <- make_matrix(W_yr5, row_col_names)

# Asian Matrix Rows:
mat_a_yr5 <- make_matrix(A_yr5, row_col_names)

# Hispanic/Latinx Rows:
mat_L_yr5 <- make_matrix(L_yr5, row_col_names)

# TMR Rows:
mat_TMR_yr5 <- make_matrix(TMR_yr5, row_col_names)

# Other Rows:
mat_Oth_yr5 <- make_matrix(Oth_yr5, row_col_names)

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
mat_w_yr6 <- make_matrix(W_yr6, row_col_names)

# Asian Matrix Rows:
mat_a_yr6 <- make_matrix(A_yr6, row_col_names)

# Hispanic/Latinx Rows:
mat_L_yr6 <- make_matrix(L_yr6, row_col_names)

# TMR Rows:
mat_TMR_yr6 <- make_matrix(TMR_yr6, row_col_names)

# Other Rows:
mat_Oth_yr6 <- make_matrix(Oth_yr6, row_col_names)

################################################################################
#' Create Markov chain training set (this is the training set only since this is
#' created with existing data instead of making predictions. This can be used 
#' to make predictions/create simulations into the future later on.) All numbers
#' rounded to nearest whole number.
################################################################################

# White training set across all 6 yrs:
# Year 1
ts_1_w <- round(W_i %*% mat_w_yr1)  # output of this (ts_1_w) is student #'s @ 
                                    # end of 18-19 academic year
# Year 2
ts_2_w_kb <- get_kb("White yr2 K_B")
ts_2_w <- round((ts_1_w + ts_2_w_kb) %*% mat_w_yr2)

# Year 3
ts_3_w_kb <- get_kb("White yr3 K_B")
ts_3_w <- round((ts_2_w + ts_3_w_kb) %*% mat_w_yr3)

# Year 4
ts_4_w_kb <- get_kb("White yr4 K_B")
ts_4_w <- round((ts_3_w + ts_4_w_kb) %*% mat_w_yr4)

# Year 5
ts_5_w_kb <- get_kb("White yr5 K_B")
ts_5_w <- round((ts_4_w + ts_5_w_kb) %*% mat_w_yr5)

# Year 6
ts_6_w_kb <- get_kb("White yr6 K_B")
ts_6_w <- round((ts_5_w + ts_6_w_kb) %*% mat_w_yr6)

# Asian training set across all 6 yrs:
# Year 1
ts_1_a <- round(A_i %*% mat_a_yr1) # output of this (ts_1_a) is student #'s @
                                   # end of 18-19 academic year

# Year 2
ts_2_a_kb <- get_kb("Asian yr2 K_B")
ts_2_a <- round((ts_1_a + ts_2_a_kb) %*% mat_a_yr2)

# Year 3
ts_3_a_kb <- get_kb("Asian yr3 K_B")
ts_3_a <- round((ts_2_a + ts_3_a_kb) %*% mat_a_yr3)

# Year 4
ts_4_a_kb <- get_kb("Asian yr4 K_B")
ts_4_a <- round((ts_3_a + ts_4_a_kb) %*% mat_a_yr4)

# Year 5
ts_5_a_kb <- get_kb("Asian yr5 K_B")
ts_5_a <- round((ts_4_a + ts_5_a_kb) %*% mat_a_yr5)

# Year 6
ts_6_a_kb <- get_kb("Asian yr6 K_B")
ts_6_a <- round((ts_5_a + ts_6_a_kb) %*% mat_a_yr6)

# Latinx training set across all 6 yrs:
# Year 1
ts_1_L <- round(L_i %*% mat_L_yr1) # output of this (ts_1_L) is student #'s @
                                   # end of 18-19 academic year

# Year 2
ts_2_L_kb <- get_kb("Latinx yr2 K_B")
ts_2_L <- round((ts_1_L + ts_2_L_kb) %*% mat_L_yr2)

# Year 3
ts_3_L_kb <- get_kb("Latinx yr3 K_B")
ts_3_L <- round((ts_2_L + ts_3_L_kb) %*% mat_L_yr3)

# Year 4
ts_4_L_kb <- get_kb("Latinx yr4 K_B")
ts_4_L <- round((ts_3_L + ts_4_L_kb) %*% mat_L_yr4)

# Year 5
ts_5_L_kb <- get_kb("Latinx yr5 K_B")
ts_5_L <- round((ts_4_L + ts_5_L_kb) %*% mat_L_yr5)

# Year 6
ts_6_L_kb <- get_kb("Latinx yr6 K_B")
ts_6_L <- round((ts_5_L + ts_6_L_kb) %*% mat_L_yr6)

# TMR training set across all 6 yrs:
# Year 1
ts_1_TMR <- round(TMR_i %*% mat_TMR_yr1) # output of this (ts_1_TMR) is student
                                         # #'s @ end of 18-19 academic year

# Year 2
ts_2_TMR_kb <- get_kb("TMR yr2 K_B")
ts_2_TMR <- round((ts_1_TMR + ts_2_TMR_kb) %*% mat_TMR_yr2)

# Year 3
ts_3_TMR_kb <- get_kb("TMR yr3 K_B")
ts_3_TMR <- round((ts_2_TMR + ts_3_TMR_kb) %*% mat_TMR_yr3)

# Year 4
ts_4_TMR_kb <- get_kb("TMR yr4 K_B")
ts_4_TMR <- round((ts_3_TMR + ts_4_TMR_kb) %*% mat_TMR_yr4)

# Year 5
ts_5_TMR_kb <- get_kb("TMR yr5 K_B")
ts_5_TMR <- round((ts_4_TMR + ts_5_TMR_kb) %*% mat_TMR_yr5)

# Year 6
ts_6_TMR_kb <- get_kb("TMR yr6 K_B")
ts_6_TMR <- round((ts_5_TMR + ts_6_TMR_kb) %*% mat_TMR_yr6)

# Other training set across all 6 yrs:
# Year 1
ts_1_Oth <- round(Oth_i %*% mat_Oth_yr1) # output of this (ts_1_Oth) is student
                                         # #'s @ end of 18-19

# Year 2
ts_2_Oth_kb <- get_kb("Other yr2 K_B")
ts_2_Oth <- round((ts_1_Oth + ts_2_Oth_kb) %*% mat_Oth_yr2)

# Year 3
ts_3_Oth_kb <- get_kb("Other yr3 K_B")
ts_3_Oth <- round((ts_2_Oth + ts_3_Oth_kb) %*% mat_Oth_yr3)

# Year 4
ts_4_Oth_kb <- get_kb("Other yr4 K_B")
ts_4_Oth <- round((ts_3_Oth + ts_4_Oth_kb) %*% mat_Oth_yr4)

# Year 5
ts_5_Oth_kb <- get_kb("Other yr5 K_B")
ts_5_Oth <- round((ts_4_Oth + ts_5_Oth_kb) %*% mat_Oth_yr5)

# Year 6
ts_6_Oth_kb <- get_kb("Other yr6 K_B")
ts_6_Oth <- round((ts_5_Oth + ts_6_Oth_kb) %*% mat_Oth_yr6)

# compute current RI:

# change year 6's training set variable names for RI computation since these
# variables are referenced again later in the code
ts_6_w_curr = ts_6_w
ts_6_a_curr = ts_6_a
ts_6_L_curr = ts_6_L
ts_6_TMR_curr = ts_6_TMR
ts_6_Oth_curr = ts_6_Oth

# Find number of students in GT and NGT year 12 (since these are absorbing
# states)
ts_6_w_curr[26] = ts_6_w[26]-ts_5_w[26]
ts_6_w_curr[27] = ts_6_w[27]-ts_5_w[27]

ts_6_a_curr[26] = ts_6_a[26]-ts_5_a[26]
ts_6_a_curr[27] = ts_6_a[27]-ts_5_a[27]

ts_6_L_curr[26] = ts_6_L[26]-ts_5_L[26]
ts_6_L_curr[27] = ts_6_L[27]-ts_5_L[27]

ts_6_TMR_curr[26] = ts_6_TMR[26]-ts_5_TMR[26]
ts_6_TMR_curr[27] = ts_6_TMR[27]-ts_5_TMR[27]

ts_6_Oth_curr[26] = ts_6_Oth[26]-ts_5_Oth[26]
ts_6_Oth_curr[27] = ts_6_Oth[27]-ts_5_Oth[27]

# Add up all students in year 6:
combined_yr6 = cbind(ts_6_w_curr, ts_6_a_curr, ts_6_L_curr, ts_6_TMR_curr,
                     ts_6_Oth_curr)
total_yr6 = sum(combined_yr6)

# Add up all GT students year 6:
gt_categories = c("K_GT","1_GT","2_GT","3_GT","4_GT","5_GT","6_GT","7_GT",
                  "8_GT","9_GT","10_GT","11_GT","12_GT")
gt_total_yr6 = combined_yr6[, grep(paste(gt_categories, collapse="|"),
                                   colnames(combined_yr6))]
gt_total_yr6_sum = sum(gt_total_yr6)

# White % in total pop:
w_total_pop_6 = sum(ts_6_w_curr)
w_percent_total_pop_6 = (w_total_pop_6/total_yr6)*100

# White % in GT:
w_gt_6 = ts_6_w_curr[,gt_categories]
w_gt_6_sum = sum(w_gt_6)
w_percent_gt_6 = (w_gt_6_sum/gt_total_yr6_sum)*100

# White RI:
w_RI = w_percent_gt_6/w_percent_total_pop_6

# Asian % in total pop:
a_total_pop_6 = sum(ts_6_a_curr)
a_percent_total_pop_6 = (a_total_pop_6/total_yr6)*100

# Asian % in GT:
a_gt_6 = ts_6_a_curr[,gt_categories]
a_gt_6_sum = sum(a_gt_6)
a_percent_gt_6 = (a_gt_6_sum/gt_total_yr6_sum)*100

# Asian RI:
a_RI = a_percent_gt_6/a_percent_total_pop_6

# Latinx % in total pop:
L_total_pop_6 = sum(ts_6_L_curr)
L_percent_total_pop_6 = (L_total_pop_6/total_yr6)*100

# Latinx % in GT:
L_gt_6 = ts_6_L_curr[,gt_categories]
L_gt_6_sum = sum(L_gt_6)
L_percent_gt_6 = (L_gt_6_sum/gt_total_yr6_sum)*100

# Latinx RI:
L_RI = L_percent_gt_6/L_percent_total_pop_6

# TMR % in total pop:
TMR_total_pop_6 = sum(ts_6_TMR_curr)
TMR_percent_total_pop_6 = (TMR_total_pop_6/total_yr6)*100

# TMR % in GT:
TMR_gt_6 = ts_6_TMR_curr[,gt_categories]
TMR_gt_6_sum = sum(TMR_gt_6)
TMR_percent_gt_6 = (TMR_gt_6_sum/gt_total_yr6_sum)*100

# TMR RI:
TMR_RI = TMR_percent_gt_6/TMR_percent_total_pop_6

# Other % in total pop:
Oth_total_pop_6 = sum(ts_6_Oth_curr)
Oth_percent_total_pop_6 = (Oth_total_pop_6/total_yr6)*100

# Other % in GT:
Oth_gt_6 = ts_6_Oth_curr[,gt_categories]
Oth_gt_6_sum = sum(Oth_gt_6)
Oth_percent_gt_6 = (Oth_gt_6_sum/gt_total_yr6_sum)*100

# Other RI:
Oth_RI = Oth_percent_gt_6/Oth_percent_total_pop_6

# put all RI output in list
RI = data.frame(RI=c(w_RI, a_RI, L_RI, TMR_RI, Oth_RI),
                Race=c("White", "Asian", "Latinx", "Two or more", "Other"))

################################################################################
# Compare max transition probabilities for staying in
# (beta) and joining (epsilon) GT across races; also comparing for leaving GT
# (gamma = 1-beta) and not being selected (omicron = 1-epsilon)
################################################################################

# define relevant transition states to pull from matrices for each computation
beta_rows_cols <- list(c("K_GT", "1_GT"),
                       c("1_GT", "2_GT"),
                       c("2_GT", "3_GT"),
                       c("3_GT", "4_GT"),
                       c("4_GT", "5_GT"),
                       c("5_GT", "6_GT"),
                       c("6_GT", "7_GT"),
                       c("7_GT", "8_GT"),
                       c("8_GT", "9_GT"),
                       c("9_GT", "10_GT"),
                       c("10_GT", "11_GT"),
                       c("11_GT", "12_GT"))

epsilon_rows_cols <- list(c("K_B", "K_GT"),
                          c("K_NGT", "1_GT"),
                          c("1_NGT", "2_GT"),
                          c("2_NGT", "3_GT"),
                          c("3_NGT", "4_GT"),
                          c("4_NGT", "5_GT"),
                          c("5_NGT", "6_GT"),
                          c("6_NGT", "7_GT"),
                          c("7_NGT", "8_GT"),
                          c("8_NGT", "9_GT"),
                          c("9_NGT", "10_GT"),
                          c("10_NGT", "11_GT"),
                          c("11_NGT", "12_GT"))

gamma_rows_cols <- list(c("K_GT", "1_NGT"),
                        c("1_GT", "2_NGT"),
                        c("2_GT", "3_NGT"),
                        c("3_GT", "4_NGT"),
                        c("4_GT", "5_NGT"),
                        c("5_GT", "6_NGT"),
                        c("6_GT", "7_NGT"),
                        c("7_GT", "8_NGT"),
                        c("8_GT", "9_NGT"),
                        c("9_GT", "10_NGT"),
                        c("10_GT", "11_NGT"),
                        c("11_GT", "12_NGT"))

omicron_rows_cols <- list(c("K_B", "K_NGT"),
                          c("K_NGT", "1_NGT"),
                          c("1_NGT", "2_NGT"),
                          c("2_NGT", "3_NGT"),
                          c("3_NGT", "4_NGT"),
                          c("4_NGT", "5_NGT"),
                          c("5_NGT", "6_NGT"),
                          c("6_NGT", "7_NGT"),
                          c("7_NGT", "8_NGT"),
                          c("8_NGT", "9_NGT"),
                          c("9_NGT", "10_NGT"),
                          c("10_NGT", "11_NGT"),
                          c("11_NGT", "12_NGT"))

# White students
w_transition_matrices <- list(mat_w_yr1, mat_w_yr2, mat_w_yr3, mat_w_yr4,
                             mat_w_yr5, mat_w_yr6)

# Initialize max_epsilon and max_gamma vector to find average across all years
max_w_epsilons = numeric(length(w_transition_matrices))
max_w_gammas = numeric(length(w_transition_matrices))

# Loop through each matrix and find the max beta/epsilon/gamma/omicron values
# for each
for (i in 1:length(w_transition_matrices)) {
  mat <- w_transition_matrices[[i]]
  
  # Initialize variables to store the max beta/epsilon values
  # for the current matrix
  max_beta <- -Inf
  max_epsilon <- -Inf
  max_gamma <- -Inf
  max_omicron <- -Inf
  max_b_row <- NULL
  max_b_col <- NULL
  max_e_row <- NULL
  max_e_col <- NULL
  max_g_row <- NULL
  max_g_col <- NULL
  max_o_row <- NULL
  max_o_col <- NULL
  
  # Loop through each beta row-column combination
  for (j in 1:length(beta_rows_cols)) {
    row <- beta_rows_cols[[j]][1]
    col <- beta_rows_cols[[j]][2]
    
    # Get the beta value at the specified row and column
    beta <- mat[row, col]
    
    # Check if this is the maximum beta value found so far for this matrix
    if (beta > max_beta) {
      max_beta <- beta
      max_b_row <- row
      max_b_col <- col
    }
  }
  
  # Print the maximum beta value and its location for the current matrix
  #cat("White: Matrix", i, "- Max Beta Value:", max_beta, "found at Row:",
   #   max_b_row, "and Column:", max_b_col, "\n")
  
  # Loop through each epsilon row-column combination
  for (j in 1:length(epsilon_rows_cols)) {
    row <- epsilon_rows_cols[[j]][1]
    col <- epsilon_rows_cols[[j]][2]
    
    # Get the epsilon value at the specified row and column
    epsilon <- mat[row, col]
    
    # Check if this is the maximum epsilon value found so far for this matrix
    if (epsilon > max_epsilon) {
      max_epsilon <- epsilon
      max_e_row <- row
      max_e_col <- col
    }
  }
  
  # Add max_epsilon to max_w_epsilons vector
  max_w_epsilons[i] = max_epsilon
  
  # Print the maximum epsilon value and its location for the current matrix
 # cat("White: Matrix", i, "- Max Epsilon Value:", max_epsilon, "found at Row:",
 #    max_e_row, "and Column:", max_e_col, "\n")
  
  # Loop through each gamma row-column combination
  for (j in 1:length(gamma_rows_cols)) {
    row <- gamma_rows_cols[[j]][1]
    col <- gamma_rows_cols[[j]][2]
    
    # Get the gamma value at the specified row and column
    gamma <- mat[row, col]
    
    # Check if this is the maximum gamma value found so far for this matrix
    if (gamma > max_gamma) {
      max_gamma <- gamma
      max_g_row <- row
      max_g_col <- col
    }
  }
  
  # Add max_gamma to max_w_gammas vector
  max_w_gammas[i] = max_gamma
  
  # Print the maximum gamma value and its location for the current matrix
 # cat("White: Matrix", i, "- Max Gamma Value:", max_gamma, "found at Row:",
 #    max_g_row, "and Column:", max_g_col, "\n")
  
  # Loop through each omicron row-column combination
  for (j in 1:length(omicron_rows_cols)) {
    row <- omicron_rows_cols[[j]][1]
    col <- omicron_rows_cols[[j]][2]
    
    # Get the omicron value at the specified row and column
    omicron <- mat[row, col]
    
    # Check if this is the maximum omicron value found so far for this matrix
    if (omicron > max_omicron) {
      max_omicron <- omicron
      max_o_row <- row
      max_o_col <- col
    }
  }
  
  # Print the maximum omicron value and its location for the current matrix
 # cat("White: Matrix", i, "- Max Omicron Value:", max_omicron, "found at Row:",
  #    max_o_row, "and Column:", max_o_col, "\n")
}

# Find average max epsilon and gamma
avg_w_max_epsilon = mean(max_w_epsilons)
avg_w_max_gamma = mean(max_w_gammas)

# Asian students
a_transition_matrices <- list(mat_a_yr1, mat_a_yr2, mat_a_yr3, mat_a_yr4,
                              mat_a_yr5, mat_a_yr6)

# Initialize max_epsilon and max_gamma vector to find average across all years
max_a_epsilons = numeric(length(a_transition_matrices))
max_a_gammas = numeric(length(a_transition_matrices))

# Loop through each matrix and find the max beta/epsilon values for each
for (i in 1:length(a_transition_matrices)) {
  mat <- a_transition_matrices[[i]]
  
  # Initialize variables to store the max beta/epsilon values
  # for the current matrix
  max_beta <- -Inf
  max_epsilon <- -Inf
  max_b_row <- NULL
  max_b_col <- NULL
  max_e_row <- NULL
  max_e_col <- NULL
  
  # Loop through each beta row-column combination
  for (j in 1:length(beta_rows_cols)) {
    row <- beta_rows_cols[[j]][1]
    col <- beta_rows_cols[[j]][2]
    
    # Get the beta value at the specified row and column
    beta <- mat[row, col]
    
    # Check if this is the maximum beta value found so far for this matrix
    if (beta > max_beta) {
      max_beta <- beta
      max_b_row <- row
      max_b_col <- col
    }
  }
  
  # Print the maximum beta value and its location for the current matrix
 # cat("Asian: Matrix", i, "- Max Beta Value:", max_beta, "found at Row:",
  #    max_b_row, "and Column:", max_b_col, "\n")
  
  # Loop through each epsilon row-column combination
  for (j in 1:length(epsilon_rows_cols)) {
    row <- epsilon_rows_cols[[j]][1]
    col <- epsilon_rows_cols[[j]][2]
    
    # Get the epsilon value at the specified row and column
    epsilon <- mat[row, col]
    
    # Check if this is the maximum epsilon value found so far for this matrix
    if (epsilon > max_epsilon) {
      max_epsilon <- epsilon
      max_e_row <- row
      max_e_col <- col
    }
  }
  
  # Add max_epsilon to max_a_epsilons vector
  max_a_epsilons[i] = max_epsilon
  
  # Print the maximum epsilon value and its location for the current matrix
  # cat("Asian: Matrix", i, "- Max Epsilon Value:", max_epsilon, "found at Row:",
  #   max_e_row, "and Column:", max_e_col, "\n")
  
  # Loop through each gamma row-column combination
  for (j in 1:length(gamma_rows_cols)) {
    row <- gamma_rows_cols[[j]][1]
    col <- gamma_rows_cols[[j]][2]
    
    # Get the gamma value at the specified row and column
    gamma <- mat[row, col]
    
    # Check if this is the maximum gamma value found so far for this matrix
    if (gamma > max_gamma) {
      max_gamma <- gamma
      max_g_row <- row
      max_g_col <- col
    }
  }
  
  # Add max_gamma to max_a_gammas vector
  max_a_gammas[i] = max_gamma
  
  # Print the maximum gamma value and its location for the current matrix
  # cat("Asian: Matrix", i, "- Max Gamma Value:", max_gamma, "found at Row:",
  #   max_g_row, "and Column:", max_g_col, "\n")
  
  # Loop through each omicron row-column combination
  for (j in 1:length(omicron_rows_cols)) {
    row <- omicron_rows_cols[[j]][1]
    col <- omicron_rows_cols[[j]][2]
    
    # Get the omicron value at the specified row and column
    omicron <- mat[row, col]
    
    # Check if this is the maximum omicron value found so far for this matrix
    if (omicron > max_omicron) {
      max_omicron <- omicron
      max_o_row <- row
      max_o_col <- col
    }
  }
  
  # Print the maximum omicron value and its location for the current matrix
  #cat("Asian: Matrix", i, "- Max Omicron Value:", max_omicron, "found at Row:",
   #   max_o_row, "and Column:", max_o_col, "\n")
}

# Find average max epsilon and gamma
avg_a_max_epsilon = mean(max_a_epsilons)
avg_a_max_gamma = mean(max_a_gammas)

# Latinx students
L_transition_matrices <- list(mat_L_yr1, mat_L_yr2, mat_L_yr3, mat_L_yr4,
                              mat_L_yr5, mat_L_yr6)

# Initialize max_epsilon and max_gamma vector to find average across all years
max_L_epsilons = numeric(length(L_transition_matrices))
max_L_gammas = numeric(length(L_transition_matrices))

# Loop through each matrix and find the max beta/epsilon values for each
for (i in 1:length(L_transition_matrices)) {
  mat <- L_transition_matrices[[i]]
  
  # Initialize variables to store the max beta/epsilon values
  # for the current matrix
  max_beta <- -Inf
  max_epsilon <- -Inf
  max_b_row <- NULL
  max_b_col <- NULL
  max_e_row <- NULL
  max_e_col <- NULL
  
  # Loop through each beta row-column combination
  for (j in 1:length(beta_rows_cols)) {
    row <- beta_rows_cols[[j]][1]
    col <- beta_rows_cols[[j]][2]
    
    # Get the beta value at the specified row and column
    beta <- mat[row, col]
    
    # Check if this is the maximum beta value found so far for this matrix
    if (beta > max_beta) {
      max_beta <- beta
      max_b_row <- row
      max_b_col <- col
    }
  }
  
  # Print the maximum beta value and its location for the current matrix
 # cat("Latinx: Matrix", i, "- Max Beta Value:", max_beta, "found at Row:",
  #    max_b_row, "and Column:", max_b_col, "\n")
  
  # Loop through each epsilon row-column combination
  for (j in 1:length(epsilon_rows_cols)) {
    row <- epsilon_rows_cols[[j]][1]
    col <- epsilon_rows_cols[[j]][2]
    
    # Get the epsilon value at the specified row and column
    epsilon <- mat[row, col]
    
    # Check if this is the maximum epsilon value found so far for this matrix
    if (epsilon > max_epsilon) {
      max_epsilon <- epsilon
      max_e_row <- row
      max_e_col <- col
    }
  }
  
  # Add max_epsilon to max_L_epsilons vector
  max_L_epsilons[i] = max_epsilon
  
  # Print the maximum epsilon value and its location for the current matrix
  # cat("Latinx: Matrix", i, "- Max Epsilon Value:", max_epsilon, "found at Row:",
  #   max_e_row, "and Column:", max_e_col, "\n")
  
  # Loop through each gamma row-column combination
  for (j in 1:length(gamma_rows_cols)) {
    row <- gamma_rows_cols[[j]][1]
    col <- gamma_rows_cols[[j]][2]
    
    # Get the gamma value at the specified row and column
    gamma <- mat[row, col]
    
    # Check if this is the maximum gamma value found so far for this matrix
    if (gamma > max_gamma) {
      max_gamma <- gamma
      max_g_row <- row
      max_g_col <- col
    }
  }
  
  # Add max_gamma to max_L_gammas vector
  max_L_gammas[i] = max_gamma
  
  # Print the maximum gamma value and its location for the current matrix
  # cat("Latinx: Matrix", i, "- Max Gamma Value:", max_gamma, "found at Row:",
  #   max_g_row, "and Column:", max_g_col, "\n")
  
  # Loop through each omicron row-column combination
  for (j in 1:length(omicron_rows_cols)) {
    row <- omicron_rows_cols[[j]][1]
    col <- omicron_rows_cols[[j]][2]
    
    # Get the omicron value at the specified row and column
    omicron <- mat[row, col]
    
    # Check if this is the maximum omicron value found so far for this matrix
    if (omicron > max_omicron) {
      max_omicron <- omicron
      max_o_row <- row
      max_o_col <- col
    }
  }
  
  # Print the maximum omicron value and its location for the current matrix
  #cat("Latinx: Matrix", i, "- Max Omicron Value:", max_omicron, "found at Row:",
   #   max_o_row, "and Column:", max_o_col, "\n")
}

# Find average max epsilon and gamma
avg_L_max_epsilon = mean(max_L_epsilons)
avg_L_max_gamma = mean(max_L_gammas)

# TMR students
TMR_transition_matrices <- list(mat_TMR_yr1, mat_TMR_yr2, mat_TMR_yr3,
                                mat_TMR_yr4, mat_TMR_yr5, mat_TMR_yr6)

# Initialize max_epsilon and max_gamma vector to find average across all years
max_TMR_epsilons = numeric(length(TMR_transition_matrices))
max_TMR_gammas = numeric(length(TMR_transition_matrices))

# Loop through each matrix and find the max beta/epsilon values for each
for (i in 1:length(TMR_transition_matrices)) {
  mat <- TMR_transition_matrices[[i]]
  
  # Initialize variables to store the max beta/epsilon values
  # for the current matrix
  max_beta <- -Inf
  max_epsilon <- -Inf
  max_b_row <- NULL
  max_b_col <- NULL
  max_e_row <- NULL
  max_e_col <- NULL
  
  # Loop through each beta row-column combination
  for (j in 1:length(beta_rows_cols)) {
    row <- beta_rows_cols[[j]][1]
    col <- beta_rows_cols[[j]][2]
    
    # Get the beta value at the specified row and column
    beta <- mat[row, col]
    
    # Check if this is the maximum beta value found so far for this matrix
    if (beta > max_beta) {
      max_beta <- beta
      max_b_row <- row
      max_b_col <- col
    }
  }
  
  # Print the maximum beta value and its location for the current matrix
  #cat("TMR: Matrix", i, "- Max Beta Value:", max_beta, "found at Row:",
   #   max_b_row, "and Column:", max_b_col, "\n")
  
  # Loop through each epsilon row-column combination
  for (j in 1:length(epsilon_rows_cols)) {
    row <- epsilon_rows_cols[[j]][1]
    col <- epsilon_rows_cols[[j]][2]
    
    # Get the epsilon value at the specified row and column
    epsilon <- mat[row, col]
    
    # Check if this is the maximum epsilon value found so far for this matrix
    if (epsilon > max_epsilon) {
      max_epsilon <- epsilon
      max_e_row <- row
      max_e_col <- col
    }
  }
  
  # Add max_epsilon to max_TMR_epsilons vector
  max_TMR_epsilons[i] = max_epsilon
  
  # Print the maximum epsilon value and its location for the current matrix
  # cat("TMR: Matrix", i, "- Max Epsilon Value:", max_epsilon, "found at Row:",
  #   max_e_row, "and Column:", max_e_col, "\n")
  
  # Loop through each gamma row-column combination
  for (j in 1:length(gamma_rows_cols)) {
    row <- gamma_rows_cols[[j]][1]
    col <- gamma_rows_cols[[j]][2]
    
    # Get the gamma value at the specified row and column
    gamma <- mat[row, col]
    
    # Check if this is the maximum gamma value found so far for this matrix
    if (gamma > max_gamma) {
      max_gamma <- gamma
      max_g_row <- row
      max_g_col <- col
    }
  }
  
  # Add max_gamma to max_TMR_gammas vector
  max_TMR_gammas[i] = max_gamma
  
  # Print the maximum gamma value and its location for the current matrix
  # cat("TMR: Matrix", i, "- Max Gamma Value:", max_gamma, "found at Row:",
  #   max_g_row, "and Column:", max_g_col, "\n")
  
  # Loop through each omicron row-column combination
  for (j in 1:length(omicron_rows_cols)) {
    row <- omicron_rows_cols[[j]][1]
    col <- omicron_rows_cols[[j]][2]
    
    # Get the omicron value at the specified row and column
    omicron <- mat[row, col]
    
    # Check if this is the maximum omicron value found so far for this matrix
    if (omicron > max_omicron) {
      max_omicron <- omicron
      max_o_row <- row
      max_o_col <- col
    }
  }
  
  # Print the maximum omicron value and its location for the current matrix
  #cat("TMR: Matrix", i, "- Max Omicron Value:", max_omicron, "found at Row:",
   #   max_o_row, "and Column:", max_o_col, "\n")
}

# Find average max epsilon and gamma
avg_TMR_max_epsilon = mean(max_TMR_epsilons)
avg_TMR_max_gamma = mean(max_TMR_gammas)

# Other students
Oth_transition_matrices <- list(mat_Oth_yr1, mat_Oth_yr2, mat_Oth_yr3,
                                mat_Oth_yr4, mat_Oth_yr5, mat_Oth_yr6)

# Initialize max_epsilon and max_gamma vector to find average across all years
max_Oth_epsilons = numeric(length(Oth_transition_matrices))
max_Oth_gammas = numeric(length(Oth_transition_matrices))

# Loop through each matrix and find the max beta/epsilon values for each
for (i in 1:length(Oth_transition_matrices)) {
  mat <- Oth_transition_matrices[[i]]
  
  # Initialize variables to store the max beta/epsilon values
  # for the current matrix
  max_beta <- -Inf
  max_epsilon <- -Inf
  max_b_row <- NULL
  max_b_col <- NULL
  max_e_row <- NULL
  max_e_col <- NULL
  
  # Loop through each beta row-column combination
  for (j in 1:length(beta_rows_cols)) {
    row <- beta_rows_cols[[j]][1]
    col <- beta_rows_cols[[j]][2]
    
    # Get the beta value at the specified row and column
    beta <- mat[row, col]
    
    # Check if this is the maximum beta value found so far for this matrix
    if (beta > max_beta) {
      max_beta <- beta
      max_b_row <- row
      max_b_col <- col
    }
  }
  
  # Print the maximum beta value and its location for the current matrix
 # cat("Other: Matrix", i, "- Max Beta Value:", max_beta, "found at Row:",
  #    max_b_row, "and Column:", max_b_col, "\n")
  
  # Loop through each epsilon row-column combination
  for (j in 1:length(epsilon_rows_cols)) {
    row <- epsilon_rows_cols[[j]][1]
    col <- epsilon_rows_cols[[j]][2]
    
    # Get the epsilon value at the specified row and column
    epsilon <- mat[row, col]
    
    # Check if this is the maximum epsilon value found so far for this matrix
    if (epsilon > max_epsilon) {
      max_epsilon <- epsilon
      max_e_row <- row
      max_e_col <- col
    }
  }
  
  # Add max_epsilons to max_Oth_epsilons vector
  max_Oth_epsilons[i] = max_epsilon
  
  # Print the maximum epsilon value and its location for the current matrix
  # cat("Other: Matrix", i, "- Max Epsilon Value:", max_epsilon, "found at Row:",
  #   max_e_row, "and Column:", max_e_col, "\n")
  
  # Loop through each gamma row-column combination
  for (j in 1:length(gamma_rows_cols)) {
    row <- gamma_rows_cols[[j]][1]
    col <- gamma_rows_cols[[j]][2]
    
    # Get the gamma value at the specified row and column
    gamma <- mat[row, col]
    
    # Check if this is the maximum gamma value found so far for this matrix
    if (gamma > max_gamma) {
      max_gamma <- gamma
      max_g_row <- row
      max_g_col <- col
    }
  }
  
  # Add max_gamma to max_Oth_gammas vector
  max_Oth_gammas[i] = max_gamma
  
  # Print the maximum gamma value and its location for the current matrix
  # cat("Other: Matrix", i, "- Max Gamma Value:", max_gamma, "found at Row:",
  #   max_g_row, "and Column:", max_g_col, "\n")
  
  # Loop through each omicron row-column combination
  for (j in 1:length(omicron_rows_cols)) {
    row <- omicron_rows_cols[[j]][1]
    col <- omicron_rows_cols[[j]][2]
    
    # Get the omicron value at the specified row and column
    omicron <- mat[row, col]
    
    # Check if this is the maximum omicron value found so far for this matrix
    if (omicron > max_omicron) {
      max_omicron <- omicron
      max_o_row <- row
      max_o_col <- col
    }
  }
  
  # Print the maximum omicron value and its location for the current matrix
  #cat("Other: Matrix", i, "- Max Omicron Value:", max_omicron, "found at Row:",
   #   max_o_row, "and Column:", max_o_col, "\n")
}

# Find average max epsilon and gamma
avg_Oth_max_epsilon = mean(max_Oth_epsilons)
avg_Oth_max_gamma = mean(max_Oth_gammas)

################################################################################
# Sensitivity analysis: Individually increase the epsilon for
# each grade change for each race by 10 percentage points and see where RI
# changes most - assume number entering school each year not affected by 0.10
# addition
################################################################################
# Create named matrix list
matrix_list <- list(
  mat_w_yr1 = mat_w_yr1, mat_w_yr2 = mat_w_yr2, mat_w_yr3 = mat_w_yr3,
  mat_w_yr4 = mat_w_yr4, mat_w_yr5 = mat_w_yr5, mat_w_yr6 = mat_w_yr6,
  mat_a_yr1 = mat_a_yr1, mat_a_yr2 = mat_a_yr2, mat_a_yr3 = mat_a_yr3,
  mat_a_yr4 = mat_a_yr4, mat_a_yr5 = mat_a_yr5, mat_a_yr6 = mat_a_yr6,
  mat_L_yr1 = mat_L_yr1, mat_L_yr2 = mat_L_yr2, mat_L_yr3 = mat_L_yr3,
  mat_L_yr4 = mat_L_yr4, mat_L_yr5 = mat_L_yr5, mat_L_yr6 = mat_L_yr6,
  mat_TMR_yr1 = mat_TMR_yr1, mat_TMR_yr2 = mat_TMR_yr2,
  mat_TMR_yr3 = mat_TMR_yr3,mat_TMR_yr4 = mat_TMR_yr4,
  mat_TMR_yr5 = mat_TMR_yr5, mat_TMR_yr6 = mat_TMR_yr6,
  mat_Oth_yr1 = mat_Oth_yr1, mat_Oth_yr2 = mat_Oth_yr2,
  mat_Oth_yr3 = mat_Oth_yr3, mat_Oth_yr4 = mat_Oth_yr4,
  mat_Oth_yr5 = mat_Oth_yr5, mat_Oth_yr6 = mat_Oth_yr6
)

# first do simulation for increasing transition probability by 10% in all grades

# Initialize an empty list to store modified matrices
modified_matrix_list <- list()

# Loop through each matrix in the list by index
for (i in seq_along(matrix_list)) {
  mat <- matrix_list[[i]]
  mat_name <- names(matrix_list)[i] 
  
  for (entry in epsilon_rows_cols) {
    row <- entry[1]
    col <- entry[2]
    
    # Create a copy of the original matrix for modification
    modified_mat <- mat
    
    # Add 10 percentage points
    modified_mat[row, col] <- modified_mat[row, col] + 0.1
    
    # Create a name for the modified matrix
    modified_name <- paste0(mat_name, "_modified_", row, "_", col)
    
    # Append the modified matrix to the list with a unique name
    modified_matrix_list <- append(modified_matrix_list,
                                   setNames(list(modified_mat), modified_name))
  }
}

# Assign each matrix in the list to its own variable explicitly
for (name in names(modified_matrix_list)) {
  assign(name, modified_matrix_list[[name]])
}

# Run loop to modify each group of matrices
# Extract matrix names
mod_matrix_names <- names(modified_matrix_list)

# Group matrices by suffix
mod_matrix_groups <- list(
  group_K_B_K_GT = modified_matrix_list[grep("_modified_K_B_K_GT$",
                                             mod_matrix_names)],
  group_K_NGT_1_GT = modified_matrix_list[grep("_modified_K_NGT_1_GT$",
                                               mod_matrix_names)],
  group_1_NGT_2_GT = modified_matrix_list[grep("_modified_1_NGT_2_GT$",
                                               mod_matrix_names)],
  group_2_NGT_3_GT = modified_matrix_list[grep("_modified_2_NGT_3_GT$",
                                               mod_matrix_names)],
  group_3_NGT_4_GT = modified_matrix_list[grep("_modified_3_NGT_4_GT$",
                                               mod_matrix_names)],
  group_4_NGT_5_GT = modified_matrix_list[grep("_modified_4_NGT_5_GT$",
                                               mod_matrix_names)],
  group_5_NGT_6_GT = modified_matrix_list[grep("_modified_5_NGT_6_GT$",
                                               mod_matrix_names)],
  group_6_NGT_7_GT = modified_matrix_list[grep("_modified_6_NGT_7_GT$",
                                               mod_matrix_names)],
  group_7_NGT_8_GT = modified_matrix_list[grep("_modified_7_NGT_8_GT$",
                                               mod_matrix_names)],
  group_8_NGT_9_GT = modified_matrix_list[grep("_modified_8_NGT_9_GT$",
                                               mod_matrix_names)],
  group_9_NGT_10_GT = modified_matrix_list[grep("_modified_9_NGT_10_GT$",
                                                mod_matrix_names)],
  group_10_NGT_11_GT = modified_matrix_list[grep("_modified_10_NGT_11_GT$",
                                        mod_matrix_names)],
  group_11_NGT_12_GT = modified_matrix_list[grep("_modified_11_NGT_12_GT$",
                                        mod_matrix_names)]
)

# Assign each matrix group in the list to its own variable explicitly
for (name in names(mod_matrix_groups)) {
  assign(name, mod_matrix_groups[[name]])
}

# Loop through each group and apply run_sims function
initials = list(W_i=W_i,A_i=A_i,L_i=L_i,TMR_i=TMR_i,Oth_i=Oth_i)
ri_sim_results = list()
for (i in seq_along(mod_matrix_groups)) {
  ri_sim_results[[i]] <- run_sims(initials,mod_matrix_groups[[i]])
}

names(ri_sim_results) = c("K_B/K_GT", "K_NGT/1_GT", "1_NGT/2_GT",
                          "2_NGT/3_GT", "3_NGT/4_GT", "4_NGT/5_GT",
                          "5_NGT/6_GT", "6_NGT/7_GT", "7_NGT/8_GT",
                          "8_NGT/9_GT", "9_NGT/10_GT", "10_NGT/11_GT",
                          "11_NGT/12_GT")

# convert to dataframe, round to 3 decimal places, export to LaTeX code
ri_sim_results <- do.call(rbind, lapply(ri_sim_results, as.data.frame))
ri_sim_results[ , sapply(ri_sim_results, is.numeric)] <- round(ri_sim_results[ , sapply(ri_sim_results, is.numeric)], 3)
ri_sim_table = kable(ri_sim_results, format = "latex", booktabs = TRUE, col.names = colnames(ri_sim_results))
#cat(ri_sim_table)
              
# now do increase of 10% for Latinx only - simulate specific resources like
# bilingual textbooks and teachers

# Create named matrix list for Latinx students only, will increase 10%
L_matrix_list <- list(
  mat_L_yr1 = mat_L_yr1, mat_L_yr2 = mat_L_yr2, mat_L_yr3 = mat_L_yr3,
  mat_L_yr4 = mat_L_yr4, mat_L_yr5 = mat_L_yr5, mat_L_yr6 = mat_L_yr6
)

# Create named matrix list for non-Latinx students, will run as normal
nonL_matrix_list <- list(
  mat_w_yr1 = mat_w_yr1, mat_w_yr2 = mat_w_yr2, mat_w_yr3 = mat_w_yr3,
  mat_w_yr4 = mat_w_yr4, mat_w_yr5 = mat_w_yr5, mat_w_yr6 = mat_w_yr6,
  mat_a_yr1 = mat_a_yr1, mat_a_yr2 = mat_a_yr2, mat_a_yr3 = mat_a_yr3,
  mat_a_yr4 = mat_a_yr4, mat_a_yr5 = mat_a_yr5, mat_a_yr6 = mat_a_yr6,
  mat_TMR_yr1 = mat_TMR_yr1, mat_TMR_yr2 = mat_TMR_yr2,
  mat_TMR_yr3 = mat_TMR_yr3,mat_TMR_yr4 = mat_TMR_yr4,
  mat_TMR_yr5 = mat_TMR_yr5, mat_TMR_yr6 = mat_TMR_yr6,
  mat_Oth_yr1 = mat_Oth_yr1, mat_Oth_yr2 = mat_Oth_yr2,
  mat_Oth_yr3 = mat_Oth_yr3, mat_Oth_yr4 = mat_Oth_yr4,
  mat_Oth_yr5 = mat_Oth_yr5, mat_Oth_yr6 = mat_Oth_yr6
)

# Initialize an empty list to store modified matrices
L_modified_matrix_list <- list()

# Loop through each matrix in the list by index
for (i in seq_along(L_matrix_list)) {
  mat <- L_matrix_list[[i]]
  mat_name <- names(L_matrix_list)[i] 
  
  for (entry in epsilon_rows_cols) {
    row <- entry[1]
    col <- entry[2]
    
    # Create a copy of the original matrix for modification
    modified_mat <- mat
    
    # Add 10 percentage points
    modified_mat[row, col] <- modified_mat[row, col] + 0.1
    
    # Create a name for the modified matrix
    modified_name <- paste0(mat_name, "_L_modified_", row, "_", col)
    
    # Append the modified matrix to the list with a unique name
    L_modified_matrix_list <- append(L_modified_matrix_list,
                                   setNames(list(modified_mat), modified_name))
  }
}

# Assign each matrix in the list to its own variable explicitly
for (name in names(L_modified_matrix_list)) {
  assign(name, L_modified_matrix_list[[name]])
}

# Extract matrix names
L_mod_matrix_names <- names(L_modified_matrix_list)

# Group matrices by suffix
L_mod_matrix_groups <- list(
  L_group_K_B_K_GT = L_modified_matrix_list[grep("_L_modified_K_B_K_GT$",
                                                 L_mod_matrix_names)],
  L_group_K_NGT_1_GT = L_modified_matrix_list[grep("_L_modified_K_NGT_1_GT$",
                                                   L_mod_matrix_names)],
  L_group_1_NGT_2_GT = L_modified_matrix_list[grep("_L_modified_1_NGT_2_GT$",
                                                   L_mod_matrix_names)],
  L_group_2_NGT_3_GT = L_modified_matrix_list[grep("_L_modified_2_NGT_3_GT$",
                                                   L_mod_matrix_names)],
  L_group_3_NGT_4_GT = L_modified_matrix_list[grep("_L_modified_3_NGT_4_GT$",
                                                   L_mod_matrix_names)],
  L_group_4_NGT_5_GT = L_modified_matrix_list[grep("_L_modified_4_NGT_5_GT$",
                                                   L_mod_matrix_names)],
  L_group_5_NGT_6_GT = L_modified_matrix_list[grep("_L_modified_5_NGT_6_GT$",
                                                   L_mod_matrix_names)],
  L_group_6_NGT_7_GT = L_modified_matrix_list[grep("_L_modified_6_NGT_7_GT$",
                                                   L_mod_matrix_names)],
  L_group_7_NGT_8_GT = L_modified_matrix_list[grep("_L_modified_7_NGT_8_GT$",
                                                   L_mod_matrix_names)],
  L_group_8_NGT_9_GT = L_modified_matrix_list[grep("_L_modified_8_NGT_9_GT$",
                                                   L_mod_matrix_names)],
  L_group_9_NGT_10_GT = L_modified_matrix_list[grep("_L_modified_9_NGT_10_GT$",
                                                    L_mod_matrix_names)],
  L_group_10_NGT_11_GT = L_modified_matrix_list[grep("_L_modified_10_NGT_11_GT$",
                                                     L_mod_matrix_names)],
  L_group_11_NGT_12_GT = L_modified_matrix_list[grep("_L_modified_11_NGT_12_GT$",
                                                     L_mod_matrix_names)]
)

# Assign each matrix group in the list to its own variable explicitly
for (name in names(L_mod_matrix_groups)) {
  assign(name, L_mod_matrix_groups[[name]])
}

# merge Latinx and non-Latinx students matrix lists
for (i in seq_along(L_mod_matrix_groups)) {
  L_mod_matrix_groups[[i]] <- append(L_mod_matrix_groups[[i]], nonL_matrix_list)
  L_mod_matrix_groups[[i]] <- L_mod_matrix_groups[[i]][c(7:18, 1:6, 19:30)]
}

# Loop through each group and apply run_sims function
initials = list(W_i=W_i,A_i=A_i,L_i=L_i,TMR_i=TMR_i,Oth_i=Oth_i)
L_ri_sim_results = list()
for (i in seq_along(L_mod_matrix_groups)) {
  L_ri_sim_results[[i]] <- run_sims(initials,L_mod_matrix_groups[[i]])
}

names(L_ri_sim_results) = c("K_B/K_GT", "K_NGT/1_GT", "1_NGT/2_GT",
                          "2_NGT/3_GT", "3_NGT/4_GT", "4_NGT/5_GT",
                          "5_NGT/6_GT", "6_NGT/7_GT", "7_NGT/8_GT",
                          "8_NGT/9_GT", "9_NGT/10_GT", "10_NGT/11_GT",
                          "11_NGT/12_GT")

# convert to dataframe, round to 3 decimal places, export to LaTeX code
L_ri_sim_results <- do.call(rbind, lapply(L_ri_sim_results, as.data.frame))
L_ri_sim_results[ , sapply(L_ri_sim_results, is.numeric)] <- round(L_ri_sim_results[ , sapply(L_ri_sim_results, is.numeric)], 3)
L_ri_sim_table = kable(L_ri_sim_results, format = "latex", booktabs = TRUE, col.names = colnames(L_ri_sim_results))
#cat(L_ri_sim_table)

################################################################################
# Simulate universal screening in fourth grade
################################################################################

# extract transition probabilities for K->1, 1->2, 2->3 for each race, all yrs
white_k_to_3 = trans_prob_extractor(w_transition_matrices)
asian_k_to_3 = trans_prob_extractor(a_transition_matrices)
latinx_k_to_3 = trans_prob_extractor(L_transition_matrices)
TMR_k_to_3 = trans_prob_extractor(TMR_transition_matrices)
Oth_k_to_3 = trans_prob_extractor(Oth_transition_matrices)

# find average transition probabilities across all years for each race (in
# vectors below, 1st entry is K-2 transitions, 2nd entry is 2-3 transition)
white_K_3_avgs = average_trans_prob(white_k_to_3)
asian_K_3_avgs = average_trans_prob(asian_k_to_3)
latinx_K_3_avgs = average_trans_prob(latinx_k_to_3)
TMR_K_3_avgs = average_trans_prob(TMR_k_to_3)
Oth_K_3_avgs = average_trans_prob(Oth_k_to_3) # may need to handle the 0's

# find difference between K-2 transitions and 2-3 transitions (this is the
# contribution of universal screening compared to the general process for 
# identification alone) and halve the probability for fourth grade
white_univ_scr = 0.5*(white_K_3_avgs[[2]] - white_K_3_avgs[[1]])
asian_univ_scr = 0.5*(asian_K_3_avgs[[2]] - asian_K_3_avgs[[1]])
latinx_univ_scr = 0.5*(latinx_K_3_avgs[[2]] - latinx_K_3_avgs[[1]])
TMR_univ_scr = 0.5*(TMR_K_3_avgs[[2]] - TMR_K_3_avgs[[1]])
Oth_univ_scr = 0.5*(Oth_K_3_avgs[[2]] - Oth_K_3_avgs[[1]])

# use matrix_list from above and do simulation for increasing transition
# probability in 4th grade by univ_scr factor found above

# Initialize an empty list to store modified matrices
modified_4_matrix_list <- list()

# Loop through each matrix in the list by index
for (i in seq_along(matrix_list)) {
  mat <- matrix_list[[i]]
  mat_name <- names(matrix_list)[i] 
  
# Create a copy of the original matrix for modification
    modified_mat <- mat
    
    # Add univ_scr factor for each race
    if (grepl("_w_", mat_name)==TRUE){
      modified_mat["4_NGT", "5_GT"] <- modified_mat["4_NGT", "5_GT"] + white_univ_scr
    }
    if (grepl("_a_", mat_name)==TRUE){
      modified_mat["4_NGT", "5_GT"] <- modified_mat["4_NGT", "5_GT"] + asian_univ_scr  
    }
    if (grepl("_L_", mat_name)==TRUE){
      modified_mat["4_NGT", "5_GT"] <- modified_mat["4_NGT", "5_GT"] + latinx_univ_scr  
    }
    if (grepl("_TMR_", mat_name)==TRUE){
      modified_mat["4_NGT", "5_GT"] <- modified_mat["4_NGT", "5_GT"] + TMR_univ_scr  
    }
    if (grepl("_Oth_", mat_name)==TRUE){
      modified_mat["4_NGT", "5_GT"] <- modified_mat["4_NGT", "5_GT"] + Oth_univ_scr  
    }
    
    # Create a name for the modified matrix
    modified_name <- paste0(mat_name, "_modified_4")
    
    # Append the modified matrix to the list with a unique name
    modified_4_matrix_list <- append(modified_4_matrix_list,
                                   setNames(list(modified_mat), modified_name))
}

# Assign each matrix in the list to its own variable explicitly
for (name in names(modified_4_matrix_list)) {
  assign(name, modified_4_matrix_list[[name]])
}

# Loop through each group and apply run_sims function
initials = list(W_i=W_i,A_i=A_i,L_i=L_i,TMR_i=TMR_i,Oth_i=Oth_i)
univ_scr_sim_results = run_sims(initials,modified_4_matrix_list)

# convert to dataframe, round to 3 decimal places, export to LaTeX code
univ_scr_sim_results <- do.call(rbind, lapply(univ_scr_sim_results, as.data.frame))
univ_scr_sim_results[ , sapply(univ_scr_sim_results, is.numeric)] <- round(univ_scr_sim_results[ , sapply(univ_scr_sim_results, is.numeric)], 3)
univ_scr_sim_table = kable(univ_scr_sim_results, format = "latex", booktabs = TRUE, col.names = colnames(univ_scr_sim_results))
cat(univ_scr_sim_table)

################################################################################
# Data visualization
################################################################################

# Plot of model output for GT composition over 6 years
# Finds the total GT population for all races and grades for each year
overall_gt_population_yr1 <- ts_1_w[2] + ts_1_w[4] + ts_1_w[6] + ts_1_w[8] +
  ts_1_w[10] + ts_1_w[12] + ts_1_w[14] + ts_1_w[16] + ts_1_w[18] + ts_1_w[20] +
  ts_1_w[22] + ts_1_w[24] + (ts_1_w[26]-W_i[26]) +
  ts_1_a[2] + ts_1_a[4] + ts_1_a[6] + ts_1_a[8] +
  ts_1_a[10] + ts_1_a[12] + ts_1_a[14] +  ts_1_a[16] + ts_1_a[18] + ts_1_a[20] +
  ts_1_a[22] + ts_1_a[24] + (ts_1_a[26]-A_i[26]) +
  ts_1_L[2] + ts_1_L[4] + ts_1_L[6] + ts_1_L[8] +
  ts_1_L[10] + ts_1_L[12] + ts_1_L[14] + ts_1_L[16] + ts_1_L[18] + ts_1_L[20] +
  ts_1_L[22] + ts_1_L[24] + (ts_1_L[26]-L_i[26]) +
  ts_1_TMR[2] + ts_1_TMR[4] + ts_1_TMR[6] + ts_1_TMR[8] +
  ts_1_TMR[10] + ts_1_TMR[12] + ts_1_TMR[14] + ts_1_TMR[16] + ts_1_TMR[18] + ts_1_TMR[20] +
  ts_1_TMR[22] + ts_1_TMR[24] + (ts_1_TMR[26]-TMR_i[26]) +
  ts_1_Oth[2] + ts_1_Oth[4] + ts_1_Oth[6] + ts_1_Oth[8] +
  ts_1_Oth[10] + ts_1_Oth[12] + ts_1_Oth[14] + ts_1_Oth[16] + ts_1_Oth[18] + ts_1_Oth[20] +
  ts_1_Oth[22] + ts_1_Oth[24] + (ts_1_Oth[26]-Oth_i[26])

overall_gt_population_yr2 <- ts_2_w[2] + ts_2_w[4] + ts_2_w[6] + ts_2_w[8] +
  ts_2_w[10] + ts_2_w[12] + ts_2_w[14] + ts_2_w[16] + ts_2_w[18] + ts_2_w[20] +
  ts_2_w[22] + ts_2_w[24] + (ts_2_w[26]-ts_1_w[26]) +
  ts_2_a[2] + ts_2_a[4] + ts_2_a[6] + ts_2_a[8] +
  ts_2_a[10] + ts_2_a[12] + ts_2_a[14] + ts_2_a[16] + ts_2_a[18] + ts_2_a[20] +
  ts_2_a[22] + ts_2_a[24] + (ts_2_a[26]-ts_1_a[26]) +
  ts_2_L[2] + ts_2_L[4] + ts_2_L[6] + ts_2_L[8] +
  ts_2_L[10] + ts_2_L[12] + ts_2_L[14] + ts_2_L[16] + ts_2_L[18] + ts_2_L[20] +
  ts_2_L[22] + ts_2_L[24] + (ts_2_L[26]-ts_1_L[26]) +
  ts_2_TMR[2] + ts_2_TMR[4] + ts_2_TMR[6] + ts_2_TMR[8] +
  ts_2_TMR[10] + ts_2_TMR[12] + ts_2_TMR[14] + ts_2_TMR[16] + ts_2_TMR[18] + ts_2_TMR[20] +
  ts_2_TMR[22] + ts_2_TMR[24] + (ts_2_TMR[26]-ts_1_TMR[26]) +
  ts_2_Oth[2] + ts_2_Oth[4] + ts_2_Oth[6] + ts_2_Oth[8] +
  ts_2_Oth[10] + ts_2_Oth[12] + ts_2_Oth[14] + ts_2_Oth[16] + ts_2_Oth[18] + ts_2_Oth[20] +
  ts_2_Oth[22] + ts_2_Oth[24] + (ts_2_Oth[26]-ts_1_Oth[26]) 

overall_gt_population_yr3 <- ts_3_w[2] + ts_3_w[4] + ts_3_w[6] + ts_3_w[8] +
  ts_3_w[10] + ts_3_w[12] + ts_3_w[14] + ts_3_w[16] + ts_3_w[18] + ts_3_w[20] +
  ts_3_w[22] + ts_3_w[24] + (ts_3_w[26]-ts_2_w[26]) +
  ts_3_a[2] + ts_3_a[4] + ts_3_a[6] + ts_3_a[8] +
  ts_3_a[10] + ts_3_a[12] + ts_3_a[14] + ts_3_a[16] + ts_3_a[18] + ts_3_a[20] +
  ts_3_a[22] + ts_3_a[24] + (ts_3_a[26]-ts_2_a[26]) +
  ts_3_L[2] + ts_3_L[4] + ts_3_L[6] + ts_3_L[8] +
  ts_3_L[10] + ts_3_L[12] + ts_3_L[14] + ts_3_L[16] + ts_3_L[18] + ts_3_L[20] +
  ts_3_L[22] + ts_3_L[24] + (ts_3_L[26]-ts_2_L[26]) +
  ts_3_TMR[2] + ts_3_TMR[4] + ts_3_TMR[6] + ts_3_TMR[8] +
  ts_3_TMR[10] + ts_3_TMR[12] + ts_3_TMR[14] + ts_3_TMR[16] + ts_3_TMR[18] + ts_3_TMR[20] +
  ts_3_TMR[22] + ts_3_TMR[24] + (ts_3_TMR[26]-ts_2_TMR[26]) +
  ts_3_Oth[2] + ts_3_Oth[4] + ts_3_Oth[6] + ts_3_Oth[8] +
  ts_3_Oth[10] + ts_3_Oth[12] + ts_3_Oth[14] + ts_3_Oth[16] + ts_3_Oth[18] + ts_3_Oth[20] +
  ts_3_Oth[22] + ts_3_Oth[24] + (ts_3_Oth[26]-ts_2_Oth[26])

overall_gt_population_yr4 <- ts_4_w[2] + ts_4_w[4] + ts_4_w[6] + ts_4_w[8] +
  ts_4_w[10] + ts_4_w[12] + ts_4_w[14] + ts_4_w[16] + ts_4_w[18] + ts_4_w[20] +
  ts_4_w[22] + ts_4_w[24] + (ts_4_w[26]-ts_3_w[26]) +
  ts_4_a[2] + ts_4_a[4] + ts_4_a[6] + ts_4_a[8] +
  ts_4_a[10] + ts_4_a[12] + ts_4_a[14] + ts_4_a[16] + ts_4_a[18] + ts_4_a[20] +
  ts_4_a[22] + ts_4_a[24] + (ts_4_a[26]-ts_3_a[26]) +
  ts_4_L[2] + ts_4_L[4] + ts_4_L[6] + ts_4_L[8] +
  ts_4_L[10] + ts_4_L[12] + ts_4_L[14] + ts_4_L[16] + ts_4_L[18] + ts_4_L[20] +
  ts_4_L[22] + ts_4_L[24] + (ts_4_L[26]-ts_3_L[26]) +
  ts_4_TMR[2] + ts_4_TMR[4] + ts_4_TMR[6] + ts_4_TMR[8] +
  ts_4_TMR[10] + ts_4_TMR[12] + ts_4_TMR[14] + ts_4_TMR[16] + ts_4_TMR[18] + ts_4_TMR[20] +
  ts_4_TMR[22] + ts_4_TMR[24] + (ts_4_TMR[26]-ts_3_TMR[26]) +
  ts_4_Oth[2] + ts_4_Oth[4] + ts_4_Oth[6] + ts_4_Oth[8] +
  ts_4_Oth[10] + ts_4_Oth[12] + ts_4_Oth[14] + ts_4_Oth[16] + ts_4_Oth[18] + ts_4_Oth[20] +
  ts_4_Oth[22] + ts_4_Oth[24] + (ts_4_Oth[26]-ts_3_Oth[26])

overall_gt_population_yr5 <- ts_5_w[2] + ts_5_w[4] + ts_5_w[6] + ts_5_w[8] +
  ts_5_w[10] + ts_5_w[12] + ts_5_w[14] + ts_5_w[16] + ts_5_w[18] + ts_5_w[20] +
  ts_5_w[22] + ts_5_w[24] + (ts_5_w[26]-ts_4_w[26]) +
  ts_5_a[2] + ts_5_a[4] + ts_5_a[6] + ts_5_a[8] +
  ts_5_a[10] + ts_5_a[12] + ts_5_a[14] + ts_5_a[16] + ts_5_a[18] + ts_5_a[20] +
  ts_5_a[22] + ts_5_a[24] + (ts_5_a[26]-ts_4_a[26]) +
  ts_5_L[2] + ts_5_L[4] + ts_5_L[6] + ts_5_L[8] +
  ts_5_L[10] + ts_5_L[12] + ts_5_L[14] + ts_5_L[16] + ts_5_L[18] + ts_5_L[20] +
  ts_5_L[22] + ts_5_L[24] + (ts_5_L[26]-ts_4_L[26]) +
  ts_5_TMR[2] + ts_5_TMR[4] + ts_5_TMR[6] + ts_5_TMR[8] +
  ts_5_TMR[10] + ts_5_TMR[12] + ts_5_TMR[14] + ts_5_TMR[16] + ts_5_TMR[18] + ts_5_TMR[20] +
  ts_5_TMR[22] + ts_5_TMR[24] + (ts_5_TMR[26]-ts_4_TMR[26]) +
  ts_5_Oth[2] + ts_5_Oth[4] + ts_5_Oth[6] + ts_5_Oth[8] +
  ts_5_Oth[10] + ts_5_Oth[12] + ts_5_Oth[14] + ts_5_Oth[16] + ts_5_Oth[18] + ts_5_Oth[20] +
  ts_5_Oth[22] + ts_5_Oth[24] + (ts_5_Oth[26]-ts_4_Oth[26])

overall_gt_population_yr6 <- ts_6_w[2] + ts_6_w[4] + ts_6_w[6] + ts_6_w[8] +
  ts_6_w[10] + ts_6_w[12] + ts_6_w[14] + ts_6_w[16] + ts_6_w[18] + ts_6_w[20] +
  ts_6_w[22] + ts_6_w[24] + (ts_6_w[26]-ts_5_w[26]) +
  ts_6_a[2] + ts_6_a[4] + ts_6_a[6] + ts_6_a[8] +
  ts_6_a[10] + ts_6_a[12] + ts_6_a[14] + ts_6_a[16] + ts_6_a[18] + ts_6_a[20] +
  ts_6_a[22] + ts_6_a[24] + (ts_6_a[26]-ts_5_a[26]) +
  ts_6_L[2] + ts_6_L[4] + ts_6_L[6] + ts_6_L[8] +
  ts_6_L[10] + ts_6_L[12] + ts_6_L[14] + ts_6_L[16] + ts_6_L[18] + ts_6_L[20] +
  ts_6_L[22] + ts_6_L[24] + (ts_6_L[26]-ts_5_L[26]) +
  ts_6_TMR[2] + ts_6_TMR[4] + ts_6_TMR[6] + ts_6_TMR[8] +
  ts_6_TMR[10] + ts_6_TMR[12] + ts_6_TMR[14] + ts_6_TMR[16] + ts_6_TMR[18] + ts_6_TMR[20] +
  ts_6_TMR[22] + ts_6_TMR[24] + (ts_6_TMR[26]-ts_5_TMR[26]) +
  ts_6_Oth[2] + ts_6_Oth[4] + ts_6_Oth[6] + ts_6_Oth[8] +
  ts_6_Oth[10] + ts_6_Oth[12] + ts_6_Oth[14] + ts_6_Oth[16] + ts_6_Oth[18] + ts_6_Oth[20] +
  ts_6_Oth[22] + ts_6_Oth[24] + (ts_6_Oth[26]-ts_5_Oth[26])

# Finds the fraction of students in each racial group in yr 1 out of the total
# number of students in GT in yr 1
overall_white_yr1 <- (ts_2_w[2] + ts_1_w[4] + ts_1_w[6] + ts_1_w[8] +
                        ts_1_w[10] + ts_1_w[12] + ts_1_w[14] + ts_1_w[16] + ts_1_w[18] + ts_1_w[20] +
                        ts_1_w[22] + ts_1_w[24] + (ts_1_w[26]-W_i[26]))/overall_gt_population_yr1

overall_asian_yr1 <- (ts_1_a[2] + ts_1_a[4] + ts_1_a[6] + ts_1_a[8] +
                        ts_1_a[10] + ts_1_a[12] + ts_1_a[14] + ts_1_a[16] + ts_1_a[18] + ts_1_a[20] +
                        ts_1_a[22] + ts_1_a[24] + (ts_1_a[26]-A_i[26]))/overall_gt_population_yr1

overall_latinx_yr1 <- (ts_1_L[2] + ts_1_L[4] + ts_1_L[6] + ts_1_L[8] +
                         ts_1_L[10] + ts_1_L[12] + ts_1_L[14] + ts_1_L[16] + ts_1_L[18] + ts_1_L[20] +
                         ts_1_L[22] + ts_1_L[24] + (ts_1_L[26]-L_i[26]))/overall_gt_population_yr1

overall_TMR_yr1 <- (ts_1_TMR[2] + ts_1_TMR[4] + ts_1_TMR[6] + ts_1_TMR[8] +
                      ts_1_TMR[10] + ts_1_TMR[12] + ts_1_TMR[14] + ts_1_TMR[16] + ts_1_TMR[18] + ts_1_TMR[20] +
                      ts_1_TMR[22] + ts_1_TMR[24] + (ts_1_TMR[26]-TMR_i[26]))/overall_gt_population_yr1

overall_other_yr1 <- (ts_1_Oth[2] + ts_1_Oth[4] + ts_1_Oth[6] + ts_1_Oth[8] +
                        ts_1_Oth[10] + ts_1_Oth[12] + ts_1_Oth[14] + ts_1_Oth[16] + ts_1_Oth[18] + ts_1_Oth[20] +
                        ts_1_Oth[22] + ts_1_Oth[24] + (ts_1_Oth[26]-Oth_i[26]))/overall_gt_population_yr1

# Finds the fraction of students in each racial group in yr 2 out of the total
# number of students in GT in yr 2
overall_white_yr2 <- (ts_2_w[2] + ts_2_w[4] + ts_2_w[6] + ts_2_w[8] +
                        ts_2_w[10] + ts_2_w[12] + ts_2_w[14] + ts_2_w[16] + ts_2_w[18] + ts_2_w[20] +
                        ts_2_w[22] + ts_2_w[24] + (ts_2_w[26]-ts_1_w[26]))/overall_gt_population_yr2

overall_asian_yr2 <- (ts_2_a[2] + ts_2_a[4] + ts_2_a[6] + ts_2_a[8] +
                        ts_2_a[10] + ts_2_a[12] + ts_2_a[14] + ts_2_a[16] + ts_2_a[18] + ts_2_a[20] +
                        ts_2_a[22] + ts_2_a[24] + (ts_2_a[26]-ts_1_a[26]))/overall_gt_population_yr2

overall_latinx_yr2 <- (ts_2_L[2] + ts_2_L[4] + ts_2_L[6] + ts_2_L[8] +
                         ts_2_L[10] + ts_2_L[12] + ts_2_L[14] + ts_2_L[16] + ts_2_L[18] + ts_2_L[20] +
                         ts_2_L[22] + ts_2_L[24] + (ts_2_L[26]-ts_1_L[26]))/overall_gt_population_yr2

overall_TMR_yr2 <- (ts_2_TMR[2] + ts_2_TMR[4] + ts_2_TMR[6] + ts_2_TMR[8] +
                      ts_2_TMR[10] + ts_2_TMR[12] + ts_2_TMR[14] + ts_2_TMR[16] + ts_2_TMR[18] + ts_2_TMR[20] +
                      ts_2_TMR[22] + ts_2_TMR[24] + (ts_2_TMR[26]-ts_1_TMR[26]))/overall_gt_population_yr2

overall_other_yr2 <- (ts_2_Oth[2] + ts_2_Oth[4] + ts_2_Oth[6] + ts_2_Oth[8] +
                        ts_2_Oth[10] + ts_2_Oth[12] + ts_2_Oth[14] + ts_2_Oth[16] + ts_2_Oth[18] + ts_2_Oth[20] +
                        ts_2_Oth[22] + ts_2_Oth[24] + (ts_2_Oth[26]-ts_1_Oth[26]))/overall_gt_population_yr2

# Finds the fraction of students in each racial group in yr 3 out of the total
# number of students in GT in yr 3
overall_white_yr3 <- (ts_3_w[2] + ts_3_w[4] + ts_3_w[6] + ts_3_w[8] +
                        ts_3_w[10] + ts_3_w[12] + ts_3_w[14] + ts_3_w[16] + ts_3_w[18] + ts_3_w[20] +
                        ts_3_w[22] + ts_3_w[24] + (ts_3_w[26]-ts_2_w[26]))/overall_gt_population_yr3

overall_asian_yr3 <- (ts_3_a[2] + ts_3_a[4] + ts_3_a[6] + ts_3_a[8] +
                        ts_3_a[10] + ts_3_a[12] + ts_3_a[14] + ts_3_a[16] + ts_3_a[18] + ts_3_a[20] +
                        ts_3_a[22] + ts_3_a[24] + (ts_3_a[26]-ts_2_a[26]))/overall_gt_population_yr3

overall_latinx_yr3 <- (ts_3_L[2] + ts_3_L[4] + ts_3_L[6] + ts_3_L[8] +
                         ts_3_L[10] + ts_3_L[12] + ts_3_L[14] + ts_3_L[16] + ts_3_L[18] + ts_3_L[20] +
                         ts_3_L[22] + ts_3_L[24] + (ts_3_L[26]-ts_2_L[26]))/overall_gt_population_yr3

overall_TMR_yr3 <- (ts_3_TMR[2] + ts_3_TMR[4] + ts_3_TMR[6] + ts_3_TMR[8] +
                      ts_3_TMR[10] + ts_3_TMR[12] + ts_3_TMR[14] + ts_3_TMR[16] + ts_3_TMR[18] + ts_3_TMR[20] +
                      ts_3_TMR[22] + ts_3_TMR[24] + (ts_3_TMR[26]-ts_2_TMR[26]))/overall_gt_population_yr3

overall_other_yr3 <- (ts_3_Oth[2] + ts_3_Oth[4] + ts_3_Oth[6] + ts_3_Oth[8] +
                        ts_3_Oth[10] + ts_3_Oth[12] + ts_3_Oth[14] + ts_3_Oth[16] + ts_3_Oth[18] + ts_3_Oth[20] +
                        ts_3_Oth[22] + ts_3_Oth[24] + (ts_3_Oth[26]-ts_2_Oth[26]))/overall_gt_population_yr3

# Finds the fraction of students in each racial group in yr 4 out of the total
# number of students in GT in yr 4
overall_white_yr4 <- (ts_4_w[2] + ts_4_w[4] + ts_4_w[6] + ts_4_w[8] +
                        ts_4_w[10] + ts_4_w[12] + ts_4_w[14] + ts_4_w[16] + ts_4_w[18] + ts_4_w[20] +
                        ts_4_w[22] + ts_4_w[24] + (ts_4_w[26]-ts_3_w[26]))/overall_gt_population_yr4

overall_asian_yr4 <- (ts_4_a[2] + ts_4_a[4] + ts_4_a[6] + ts_4_a[8] +
                        ts_4_a[10] + ts_4_a[12] + ts_4_a[14] + ts_4_a[16] + ts_4_a[18] + ts_4_a[20] +
                        ts_4_a[22] + ts_4_a[24] + (ts_4_a[26]-ts_3_a[26]))/overall_gt_population_yr4

overall_latinx_yr4 <- (ts_4_L[2] + ts_4_L[4] + ts_4_L[6] + ts_4_L[8] +
                         ts_4_L[10] + ts_4_L[12] + ts_4_L[14] + ts_4_L[16] + ts_4_L[18] + ts_4_L[20] +
                         ts_4_L[22] + ts_4_L[24] + (ts_4_L[26]-ts_3_L[26]))/overall_gt_population_yr4

overall_TMR_yr4 <- (ts_4_TMR[2] + ts_4_TMR[4] + ts_4_TMR[6] + ts_4_TMR[8] +
                      ts_4_TMR[10] + ts_4_TMR[12] + ts_4_TMR[14] + ts_4_TMR[16] + ts_4_TMR[18] + ts_4_TMR[20] +
                      ts_4_TMR[22] + ts_4_TMR[24] + (ts_4_TMR[26]-ts_3_TMR[26]))/overall_gt_population_yr4

overall_other_yr4 <- (ts_4_Oth[2] + ts_4_Oth[4] + ts_4_Oth[6] + ts_4_Oth[8] +
                        ts_4_Oth[10] + ts_4_Oth[12] + ts_4_Oth[14] + ts_4_Oth[16] + ts_4_Oth[18] + ts_4_Oth[20] +
                        ts_4_Oth[22] + ts_4_Oth[24] + (ts_4_Oth[26]-ts_3_Oth[26]))/overall_gt_population_yr4

# Finds the fraction of students in each racial group in yr 5 out of the total
# number of students in GT in yr 5
overall_white_yr5 <- (ts_5_w[2] + ts_5_w[4] + ts_5_w[6] + ts_5_w[8] +
                        ts_5_w[10] + ts_5_w[12] + ts_5_w[14] + ts_5_w[16] + ts_5_w[18] + ts_5_w[20] +
                        ts_5_w[22] + ts_5_w[24] + (ts_5_w[26]-ts_4_w[26]))/overall_gt_population_yr5

overall_asian_yr5 <- (ts_5_a[2] + ts_5_a[4] + ts_5_a[6] + ts_5_a[8] +
                        ts_5_a[10] + ts_5_a[12] + ts_5_a[14] + ts_5_a[16] + ts_5_a[18] + ts_5_a[20] +
                        ts_5_a[22] + ts_5_a[24] + (ts_5_a[26]-ts_4_a[26]))/overall_gt_population_yr5

overall_latinx_yr5 <- (ts_5_L[2] + ts_5_L[4] + ts_5_L[6] + ts_5_L[8] +
                         ts_5_L[10] + ts_5_L[12] + ts_5_L[14] + ts_5_L[16] + ts_5_L[18] + ts_5_L[20] +
                         ts_5_L[22] + ts_5_L[24] + (ts_5_L[26]-ts_4_L[26]))/overall_gt_population_yr5

overall_TMR_yr5 <- (ts_5_TMR[2] + ts_5_TMR[4] + ts_5_TMR[6] + ts_5_TMR[8] +
                      ts_5_TMR[10] + ts_5_TMR[12] + ts_5_TMR[14] + ts_5_TMR[16] + ts_5_TMR[18] + ts_5_TMR[20] +
                      ts_5_TMR[22] + ts_5_TMR[24] + (ts_5_TMR[26]-ts_4_TMR[26]))/overall_gt_population_yr5

overall_other_yr5 <- (ts_5_Oth[2] + ts_5_Oth[4] + ts_5_Oth[6] + ts_5_Oth[8] +
                        ts_5_Oth[10] + ts_5_Oth[12] + ts_5_Oth[14] + ts_5_Oth[16] + ts_5_Oth[18] + ts_5_Oth[20] +
                        ts_5_Oth[22] + ts_5_Oth[24] + (ts_5_Oth[26]-ts_4_Oth[26]))/overall_gt_population_yr5

# Finds the fraction of students in each racial group in yr 6 out of the total
# number of students in GT in yr 6
overall_white_yr6 <- (ts_6_w[2] + ts_6_w[4] + ts_6_w[6] + ts_6_w[8] +
                        ts_6_w[10] + ts_6_w[12] + ts_6_w[14] + ts_6_w[16] + ts_6_w[18] + ts_6_w[20] +
                        ts_6_w[22] + ts_6_w[24] + (ts_6_w[26]-ts_5_w[26]))/overall_gt_population_yr6

overall_asian_yr6 <- (ts_6_a[2] + ts_6_a[4] + ts_6_a[6] + ts_6_a[8] +
                        ts_6_a[10] + ts_6_a[12] + ts_6_a[14] + ts_6_a[16] + ts_6_a[18] + ts_6_a[20] +
                        ts_6_a[22] + ts_6_a[24] + (ts_6_a[26]-ts_5_a[26]))/overall_gt_population_yr6

overall_latinx_yr6 <- (ts_6_L[2] + ts_6_L[4] + ts_6_L[6] + ts_6_L[8] +
                         ts_6_L[10] + ts_6_L[12] + ts_6_L[14] + ts_6_L[16] + ts_6_L[18] + ts_6_L[20] +
                         ts_6_L[22] + ts_6_L[24] + (ts_6_L[26]-ts_5_L[26]))/overall_gt_population_yr6

overall_TMR_yr6 <- (ts_6_TMR[2] + ts_6_TMR[4] + ts_6_TMR[6] + ts_6_TMR[8] +
                      ts_6_TMR[10] + ts_6_TMR[12] + ts_6_TMR[14] + ts_6_TMR[16] + ts_6_TMR[18] + ts_6_TMR[20] +
                      ts_6_TMR[22] + ts_6_TMR[24] + (ts_6_TMR[26]-ts_5_TMR[26]))/overall_gt_population_yr6

overall_other_yr6 <- (ts_6_Oth[2] + ts_6_Oth[4] + ts_6_Oth[6] + ts_6_Oth[8] +
                        ts_6_Oth[10] + ts_6_Oth[12] + ts_6_Oth[14] + ts_6_Oth[16] + ts_6_Oth[18] + ts_6_Oth[20] +
                        ts_6_Oth[22] + ts_6_Oth[24] + (ts_6_Oth[26]-ts_5_Oth[26]))/overall_gt_population_yr6

# Finds the percent of each racial group in GT over all 6 yrs
overall_white_all_years <- c(overall_white_yr1*100, 
                             overall_white_yr2*100, 
                             overall_white_yr3*100, 
                             overall_white_yr4*100,
                             overall_white_yr5*100,
                             overall_white_yr6*100)

overall_asian_all_years <- c(overall_asian_yr1*100, 
                             overall_asian_yr2*100, 
                             overall_asian_yr3*100, 
                             overall_asian_yr4*100,
                             overall_asian_yr5*100,
                             overall_asian_yr6*100)

overall_latinx_all_years <- c(overall_latinx_yr1*100, 
                              overall_latinx_yr2*100, 
                              overall_latinx_yr3*100, 
                              overall_latinx_yr4*100,
                              overall_latinx_yr5*100,
                              overall_latinx_yr6*100)

overall_TMR_all_years <- c(overall_TMR_yr1*100, 
                           overall_TMR_yr2*100, 
                           overall_TMR_yr3*100, 
                           overall_TMR_yr4*100,
                           overall_TMR_yr5*100,
                           overall_TMR_yr6*100)

overall_other_all_years <- c(overall_other_yr1*100, 
                             overall_other_yr2*100, 
                             overall_other_yr3*100, 
                             overall_other_yr4*100,
                             overall_other_yr5*100,
                             overall_other_yr6*100)

# Raw BVSD data for comparison
tableau_gt_percent_white = c(75.3,75.3,74.8,74,73.6,73.1)
tableau_gt_percent_asian = c(7.3,7,7,7.3,7.1,7.1)
tableau_gt_percent_latinx = c(9.6,9.4,9.5,9.8,10.1,10.6)
tableau_gt_percent_TMR = c(7.3,7.7,8,8.3,8.6,8.7)
tableau_gt_percent_Oth = c(100-tableau_gt_percent_white[1]-tableau_gt_percent_asian[1]-
                             tableau_gt_percent_latinx[1]-tableau_gt_percent_TMR[1],
                           100-tableau_gt_percent_white[2]-tableau_gt_percent_asian[2]-
                             tableau_gt_percent_latinx[2]-tableau_gt_percent_TMR[2],
                           100-tableau_gt_percent_white[3]-tableau_gt_percent_asian[3]-
                             tableau_gt_percent_latinx[3]-tableau_gt_percent_TMR[3],
                           100-tableau_gt_percent_white[4]-tableau_gt_percent_asian[4]-
                             tableau_gt_percent_latinx[4]-tableau_gt_percent_TMR[4],
                           100-tableau_gt_percent_white[5]-tableau_gt_percent_asian[5]-
                             tableau_gt_percent_latinx[5]-tableau_gt_percent_TMR[5],
                           100-tableau_gt_percent_white[6]-tableau_gt_percent_asian[6]-
                             tableau_gt_percent_latinx[6]-tableau_gt_percent_TMR[6])

# Makes data frame to graph GT racial/ethnic composition over the 6 yrs from model
df_overall <- data.frame(year=c('2018-19', '2019-20', '2020-21', '2021-22',
                                '2022-23','2023-24'), 
                         percent_gt_model=c(overall_white_all_years,
                                            overall_asian_all_years,
                                            overall_latinx_all_years,
                                            overall_TMR_all_years,
                                            overall_other_all_years),
                         percent_gt_tableau = c(tableau_gt_percent_white,
                                                tableau_gt_percent_asian,
                                                tableau_gt_percent_latinx,
                                                tableau_gt_percent_TMR,
                                                tableau_gt_percent_Oth),
                         Race=c("White", "White", "White", "White","White","White",
                                "Asian", "Asian", "Asian", "Asian","Asian","Asian",
                                "Latinx", "Latinx", "Latinx", "Latinx","Latinx",
                                "Latinx",
                                "Two or more", "Two or more", "Two or more",
                                "Two or more", "Two or more", "Two or more",
                                "Other", "Other", "Other", "Other","Other","Other"))
# Plots the results
gt_percent_plot = ggplot(df_overall, aes(x = year)) +
  geom_point(aes(y = percent_gt_model, color = "Model", shape = Race), size = 3,
             position = position_dodge(width = 0.5)) +
  geom_point(aes(y = percent_gt_tableau, color = "BVSD Data", shape = Race), size = 3,
             position = position_dodge(width = 0.5)) +
  scale_color_manual(values = c("Model" = "#E69F00", "BVSD Data" = "#56B4E9")) + 
  scale_shape_manual(values = c("White" = 15, "Asian" = 17, "Latinx" = 18, 
                                "Two or more" = 8, "Other" = 19)) +
  labs(x = "Year", y = "Percent of GT Population", color = "Type", shape = "Race") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(filename = "gt_percent_plot.svg", plot = gt_percent_plot, 
       width = 8, height = 6, device = "svg")

################################################################################

# RI plots across the 6 years 

# adjust training sets to remove accumulation of values in absorbing states
# (need number in grade 12 that year for RI calculation)
ts_1_w[26] = ts_1_w[26] - W_i[26]
ts_1_w[27] = ts_1_w[27] - W_i[27]
ts_1_a[26] = ts_1_a[26] - A_i[26]
ts_1_a[27] = ts_1_a[27] - A_i[27]
ts_1_L[26] = ts_1_L[26] - L_i[26]
ts_1_L[27] = ts_1_L[27] - L_i[27]
ts_1_TMR[26] = ts_1_TMR[26] - TMR_i[26]
ts_1_TMR[27] = ts_1_TMR[27] - TMR_i[27]
ts_1_Oth[26] = ts_1_Oth[26] - Oth_i[26]
ts_1_Oth[27] = ts_1_Oth[27] - Oth_i[27]
ts_2_w[26] = ts_2_w[26] - ts_1_w[26]
ts_2_w[27] = ts_2_w[27] - ts_1_w[27]
ts_2_a[26] = ts_2_a[26] - ts_1_a[26]
ts_2_a[27] = ts_2_a[27] - ts_1_a[27]
ts_2_L[26] = ts_2_L[26] - ts_1_L[26]
ts_2_L[27] = ts_2_L[27] - ts_1_L[27]
ts_2_TMR[26] = ts_2_TMR[26] - ts_1_TMR[26]
ts_2_TMR[27] = ts_2_TMR[27] - ts_1_TMR[27]
ts_2_Oth[26] = ts_2_Oth[26] - ts_1_Oth[26]
ts_2_Oth[27] = ts_2_Oth[27] - ts_1_Oth[27]
ts_3_w[26] = ts_3_w[26] - ts_2_w[26]
ts_3_w[27] = ts_3_w[27] - ts_2_w[27]
ts_3_a[26] = ts_3_a[26] - ts_2_a[26]
ts_3_a[27] = ts_3_a[27] - ts_2_a[27]
ts_3_L[26] = ts_3_L[26] - ts_2_L[26]
ts_3_L[27] = ts_3_L[27] - ts_2_L[27]
ts_3_TMR[26] = ts_3_TMR[26] - ts_2_TMR[26]
ts_3_TMR[27] = ts_3_TMR[27] - ts_2_TMR[27]
ts_3_Oth[26] = ts_3_Oth[26] - ts_2_Oth[26]
ts_3_Oth[27] = ts_3_Oth[27] - ts_2_Oth[27]
ts_4_w[26] = ts_4_w[26] - ts_3_w[26]
ts_4_w[27] = ts_4_w[27] - ts_3_w[27]
ts_4_a[26] = ts_4_a[26] - ts_3_a[26]
ts_4_a[27] = ts_4_a[27] - ts_3_a[27]
ts_4_L[26] = ts_4_L[26] - ts_3_L[26]
ts_4_L[27] = ts_4_L[27] - ts_3_L[27]
ts_4_TMR[26] = ts_4_TMR[26] - ts_3_TMR[26]
ts_4_TMR[27] = ts_4_TMR[27] - ts_3_TMR[27]
ts_4_Oth[26] = ts_4_Oth[26] - ts_3_Oth[26]
ts_4_Oth[27] = ts_4_Oth[27] - ts_3_Oth[27]
ts_5_w[26] = ts_5_w[26] - ts_4_w[26]
ts_5_w[27] = ts_5_w[27] - ts_4_w[27]
ts_5_a[26] = ts_5_a[26] - ts_4_a[26]
ts_5_a[27] = ts_5_a[27] - ts_4_a[27]
ts_5_L[26] = ts_5_L[26] - ts_4_L[26]
ts_5_L[27] = ts_5_L[27] - ts_4_L[27]
ts_5_TMR[26] = ts_5_TMR[26] - ts_4_TMR[26]
ts_5_TMR[27] = ts_5_TMR[27] - ts_4_TMR[27]
ts_5_Oth[26] = ts_5_Oth[26] - ts_4_Oth[26]
ts_5_Oth[27] = ts_5_Oth[27] - ts_4_Oth[27]
ts_6_w[26] = ts_6_w[26] - ts_5_w[26]
ts_6_w[27] = ts_6_w[27] - ts_5_w[27]
ts_6_a[26] = ts_6_a[26] - ts_5_a[26]
ts_6_a[27] = ts_6_a[27] - ts_5_a[27]
ts_6_L[26] = ts_6_L[26] - ts_5_L[26]
ts_6_L[27] = ts_6_L[27] - ts_5_L[27]
ts_6_TMR[26] = ts_6_TMR[26] - ts_5_TMR[26]
ts_6_TMR[27] = ts_6_TMR[27] - ts_5_TMR[27]
ts_6_Oth[26] = ts_6_Oth[26] - ts_5_Oth[26]
ts_6_Oth[27] = ts_6_Oth[27] - ts_5_Oth[27]

# Add up all students in each year:
combined_yr1 = cbind(ts_1_w, ts_1_a, ts_1_L, ts_1_TMR, ts_1_Oth)
combined_yr2 = cbind(ts_2_w, ts_2_a, ts_2_L, ts_2_TMR, ts_2_Oth)
combined_yr3 = cbind(ts_3_w, ts_3_a, ts_3_L, ts_3_TMR, ts_3_Oth)
combined_yr4 = cbind(ts_4_w, ts_4_a, ts_4_L, ts_4_TMR, ts_4_Oth)
combined_yr5 = cbind(ts_5_w, ts_5_a, ts_5_L, ts_5_TMR, ts_5_Oth)
combined_yr6 = cbind(ts_6_w, ts_6_a, ts_6_L, ts_6_TMR, ts_6_Oth)
total_yr1 = sum(combined_yr1)
total_yr2 = sum(combined_yr2)
total_yr3 = sum(combined_yr3)
total_yr4 = sum(combined_yr4)
total_yr5 = sum(combined_yr5)
total_yr6 = sum(combined_yr6)

# White % in total pop:
w_total_pop_1 = sum(ts_1_w)
w_percent_total_pop_1 = (w_total_pop_1/total_yr1)*100
w_total_pop_2 = sum(ts_2_w)
w_percent_total_pop_2 = (w_total_pop_2/total_yr2)*100
w_total_pop_3 = sum(ts_3_w)
w_percent_total_pop_3 = (w_total_pop_3/total_yr3)*100
w_total_pop_4 = sum(ts_4_w)
w_percent_total_pop_4 = (w_total_pop_4/total_yr4)*100
w_total_pop_5 = sum(ts_5_w)
w_percent_total_pop_5 = (w_total_pop_5/total_yr5)*100
w_total_pop_6 = sum(ts_6_w)
w_percent_total_pop_6 = (w_total_pop_6/total_yr6)*100

# White RI:
w_RI_1 = overall_white_all_years[1]/w_percent_total_pop_1
w_RI_2 = overall_white_all_years[2]/w_percent_total_pop_2
w_RI_3 = overall_white_all_years[3]/w_percent_total_pop_3
w_RI_4 = overall_white_all_years[4]/w_percent_total_pop_4
w_RI_5 = overall_white_all_years[5]/w_percent_total_pop_5
w_RI_6 = overall_white_all_years[6]/w_percent_total_pop_6

# Asian % in total pop:
a_total_pop_1 = sum(ts_1_a)
a_percent_total_pop_1 = (a_total_pop_1/total_yr1)*100
a_total_pop_2 = sum(ts_2_a)
a_percent_total_pop_2 = (a_total_pop_2/total_yr2)*100
a_total_pop_3 = sum(ts_3_a)
a_percent_total_pop_3 = (a_total_pop_3/total_yr3)*100
a_total_pop_4 = sum(ts_4_a)
a_percent_total_pop_4 = (a_total_pop_4/total_yr4)*100
a_total_pop_5 = sum(ts_5_a)
a_percent_total_pop_5 = (a_total_pop_5/total_yr5)*100
a_total_pop_6 = sum(ts_6_a)
a_percent_total_pop_6 = (a_total_pop_6/total_yr6)*100

# Asian RI:
a_RI_1 = overall_asian_all_years[1]/a_percent_total_pop_1
a_RI_2 = overall_asian_all_years[2]/a_percent_total_pop_2
a_RI_3 = overall_asian_all_years[3]/a_percent_total_pop_3
a_RI_4 = overall_asian_all_years[4]/a_percent_total_pop_4
a_RI_5 = overall_asian_all_years[5]/a_percent_total_pop_5
a_RI_6 = overall_asian_all_years[6]/a_percent_total_pop_6

# Latinx % in total pop:
L_total_pop_1 = sum(ts_1_L)
L_percent_total_pop_1 = (L_total_pop_1/total_yr1)*100
L_total_pop_2 = sum(ts_2_L)
L_percent_total_pop_2 = (L_total_pop_2/total_yr2)*100
L_total_pop_3 = sum(ts_3_L)
L_percent_total_pop_3 = (L_total_pop_3/total_yr3)*100
L_total_pop_4 = sum(ts_4_L)
L_percent_total_pop_4 = (L_total_pop_4/total_yr4)*100
L_total_pop_5 = sum(ts_5_L)
L_percent_total_pop_5 = (L_total_pop_5/total_yr5)*100
L_total_pop_6 = sum(ts_6_L)
L_percent_total_pop_6 = (L_total_pop_6/total_yr6)*100

# Latinx RI:
L_RI_1 = overall_latinx_all_years[1]/L_percent_total_pop_1
L_RI_2 = overall_latinx_all_years[2]/L_percent_total_pop_2
L_RI_3 = overall_latinx_all_years[3]/L_percent_total_pop_3
L_RI_4 = overall_latinx_all_years[4]/L_percent_total_pop_4
L_RI_5 = overall_latinx_all_years[5]/L_percent_total_pop_5
L_RI_6 = overall_latinx_all_years[6]/L_percent_total_pop_6

# TMR % in total pop:
TMR_total_pop_1 = sum(ts_1_TMR)
TMR_percent_total_pop_1 = (TMR_total_pop_1/total_yr1)*100
TMR_total_pop_2 = sum(ts_2_TMR)
TMR_percent_total_pop_2 = (TMR_total_pop_2/total_yr2)*100
TMR_total_pop_3 = sum(ts_3_TMR)
TMR_percent_total_pop_3 = (TMR_total_pop_3/total_yr3)*100
TMR_total_pop_4 = sum(ts_4_TMR)
TMR_percent_total_pop_4 = (TMR_total_pop_4/total_yr4)*100
TMR_total_pop_5 = sum(ts_5_TMR)
TMR_percent_total_pop_5 = (TMR_total_pop_5/total_yr5)*100
TMR_total_pop_6 = sum(ts_6_TMR)
TMR_percent_total_pop_6 = (TMR_total_pop_6/total_yr6)*100

# TMR RI:
TMR_RI_1 = overall_TMR_all_years[1]/TMR_percent_total_pop_1
TMR_RI_2 = overall_TMR_all_years[2]/TMR_percent_total_pop_2
TMR_RI_3 = overall_TMR_all_years[3]/TMR_percent_total_pop_3
TMR_RI_4 = overall_TMR_all_years[4]/TMR_percent_total_pop_4
TMR_RI_5 = overall_TMR_all_years[5]/TMR_percent_total_pop_5
TMR_RI_6 = overall_TMR_all_years[6]/TMR_percent_total_pop_6

# Other % in total pop:
Oth_total_pop_1 = sum(ts_1_Oth)
Oth_percent_total_pop_1 = (Oth_total_pop_1/total_yr1)*100
Oth_total_pop_2 = sum(ts_2_Oth)
Oth_percent_total_pop_2 = (Oth_total_pop_2/total_yr2)*100
Oth_total_pop_3 = sum(ts_3_Oth)
Oth_percent_total_pop_3 = (Oth_total_pop_3/total_yr3)*100
Oth_total_pop_4 = sum(ts_4_Oth)
Oth_percent_total_pop_4 = (Oth_total_pop_4/total_yr4)*100
Oth_total_pop_5 = sum(ts_5_Oth)
Oth_percent_total_pop_5 = (Oth_total_pop_5/total_yr5)*100
Oth_total_pop_6 = sum(ts_6_Oth)
Oth_percent_total_pop_6 = (Oth_total_pop_6/total_yr6)*100

# Other RI:
Oth_RI_1 = overall_other_all_years[1]/Oth_percent_total_pop_1
Oth_RI_2 = overall_other_all_years[2]/Oth_percent_total_pop_2
Oth_RI_3 = overall_other_all_years[3]/Oth_percent_total_pop_3
Oth_RI_4 = overall_other_all_years[4]/Oth_percent_total_pop_4
Oth_RI_5 = overall_other_all_years[5]/Oth_percent_total_pop_5
Oth_RI_6 = overall_other_all_years[6]/Oth_percent_total_pop_6

# ri vectors
white_ri = c(w_RI_1,w_RI_2,w_RI_3,w_RI_4,w_RI_5,w_RI_6)
asian_ri = c(a_RI_1,a_RI_2,a_RI_3,a_RI_4,a_RI_5,a_RI_6)
latinx_ri = c(L_RI_1,L_RI_2,L_RI_3,L_RI_4,L_RI_5,L_RI_6)
TMR_ri = c(TMR_RI_1,TMR_RI_2,TMR_RI_3,TMR_RI_4,TMR_RI_5,TMR_RI_6)
Oth_ri = c(Oth_RI_1,Oth_RI_2,Oth_RI_3,Oth_RI_4,Oth_RI_5,Oth_RI_6)

# make df of RIs over 6 years
ri_df = data.frame(year=c('2018-19', '2019-20', '2020-21', '2021-22',
                          '2022-23','2023-24'), 
                   ris=c(white_ri,
                         asian_ri,
                         latinx_ri,
                         TMR_ri,
                         Oth_ri),
                   Race=c("White", "White", "White", "White","White","White",
                          "Asian", "Asian", "Asian", "Asian","Asian","Asian",
                          "Latinx", "Latinx", "Latinx", "Latinx","Latinx",
                          "Latinx",
                          "Two or more", "Two or more", "Two or more",
                          "Two or more", "Two or more", "Two or more",
                          "Other", "Other", "Other", "Other","Other","Other"))

# make plot
ri_plot=ggplot(ri_df, aes(x = year, y = ris, shape = Race)) +
  geom_point(size = 3, position = position_dodge(width = 0.5)) +
  scale_shape_manual(values = c("White" = 15, "Asian" = 17, "Latinx" = 18, 
                                "Two or more" = 8, "Other" = 19)) +
  labs(x = "Year", y = "RI", shape = "Race") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(filename = "ri_plot.svg", plot = ri_plot, 
       width = 8, height = 6, device = "svg")

################################################################################

# Plot RI at end of year 6 for current GT populations
ri = ggplot(RI, aes(x=Race, y=RI)) +
  geom_col(fill = "skyblue", color = "black", width = 0.8) +  
  theme_minimal() +
  geom_text(aes(label=round(RI, digits=2), vjust=-0.5), size=4.5) +
  labs(x="Race", y="RI") +
  theme(axis.title = element_text(size = 17)) +
  theme(axis.text = element_text(size = 15))
ggsave(filename = "ri_current.svg", plot = ri, width = 8, height = 6, device = "svg")

################################################################################

# Plot avg max epsilon across all races
max_epsilon_df = data.frame(Race = c("White", "Asian", "Latinx", "TMR",
                                     "Other"),
                            Max_Epsilon = c(avg_w_max_epsilon,avg_a_max_epsilon,
                            avg_L_max_epsilon, avg_TMR_max_epsilon,
                            avg_Oth_max_epsilon))
max_epsilon_plot = ggplot(max_epsilon_df, aes(x = Race, y = Max_Epsilon)) +
  geom_col(fill = "skyblue", color = "black", width = 0.8) +  
  theme_minimal() +
  geom_text(aes(label=round(Max_Epsilon, digits=2), vjust=-0.5), size=4.5) +
  labs(x = "Race",
       y = "Max Probability of GT Selection") +
  theme(axis.title.x = element_text(size = 14),  
        axis.title.y = element_text(size = 14),  
        axis.text.x = element_text(size = 12),   
        axis.text.y = element_text(size = 12))
ggsave(filename = "max_epsilon.svg", plot = max_epsilon_plot, width = 8, height = 6, device = "svg")

################################################################################

# Plot avg max gamma across all races
max_gamma_df = data.frame(Race = c("White", "Asian", "Latinx", "TMR",
                                     "Other"),
                            Max_gamma = c(avg_w_max_gamma,avg_a_max_gamma,
                                            avg_L_max_gamma, avg_TMR_max_gamma,
                                            avg_Oth_max_gamma))
max_gamma_plot = ggplot(max_gamma_df, aes(x = Race, y = Max_gamma)) +
  geom_col(fill = "skyblue", color = "black", width = 0.8) +  
  theme_minimal() +
  geom_text(aes(label=round(Max_gamma, digits=2), vjust=-0.5), size=4.5) +
  labs(x = "Race",
       y = "Max Probability of Opting Out of GT") +
  theme(axis.title.x = element_text(size = 14),  
        axis.title.y = element_text(size = 14),  
        axis.text.x = element_text(size = 12),   
        axis.text.y = element_text(size = 12))
ggsave(filename = "max_gamma.svg", plot = max_gamma_plot, width = 8, height = 6,
       device = "svg")

################################################################################

# plot max RI change with 10% increase at all grade levels all races
max_row_ri_sim = sapply(ri_sim_results,
                        function(col) if(is.numeric(col)) max(col) else NA)
ri_all_race_grades = data.frame(Race = c("White", "Asian", "Latinx", "TMR",
                                         "Other"),RI_current = RI,
                                RI_simulated = max_row_ri_sim)

# Reshape the data frame to long format for ggplot2
ri_all_race_grades_long <- melt(ri_all_race_grades, id.vars = "Race", 
                                variable.name = "RI_Type", value.name = "RI_Value")
ri_all_race_grades_long$RI_Value <- as.numeric(ri_all_race_grades_long$RI_Value)

# remove extra rows introduced
ri_all_race_grades_long = ri_all_race_grades_long %>%
  filter(RI_Type != "RI_current.Race") %>%
  mutate(RI_Type = case_when(RI_Type=="RI_current.RI" ~ "Current RI",
                             RI_Type=="RI_simulated" ~ "Simulated RI"))

# Plot with two bars per race (RI_current and RI_simulated)
ri_all_race_grades_plot <- ggplot(ri_all_race_grades_long, aes(x = Race, y = RI_Value, fill = RI_Type)) +
  geom_col(position = "dodge", color = "black", width = 0.8) +
  theme_minimal() +
  geom_text(aes(label = round(RI_Value, digits = 2)), 
            position = position_dodge(width = 0.8), vjust = -0.5, size = 3) +
  labs(x = "Race", y = "RI", fill = "RI Type") +
  theme(axis.title = element_text(size = 17), axis.text = element_text(size = 15)) +
  theme(legend.position = "bottom") +  # Move legend to the bottom
  scale_fill_manual(values = c("Current RI" = "#E69F00", "Simulated RI" = "#56B4E9"),
                    labels = c("Current RI", "Simulated RI"))
# Save the plot
ggsave(filename = "ri_all_race_grades.svg", plot = ri_all_race_grades_plot, 
       width = 8, height = 6, device = "svg")

################################################################################

# plot max RI change with 10% increase at all grade levels Latinx only
max_row_L_ri_sim = sapply(L_ri_sim_results, function(col) if(is.numeric(col)) max(col) else NA)
ri_L_only = data.frame(Race = c("White", "Asian", "Latinx", "TMR",
                                         "Other"),RI_current = RI,
                                RI_simulated = max_row_L_ri_sim)

# Reshape the data frame to long format for ggplot2
ri_L_only_long <- melt(ri_L_only, id.vars = "Race", 
                                variable.name = "RI_Type", value.name = "RI_Value")
ri_L_only_long$RI_Value <- as.numeric(ri_L_only_long$RI_Value)

# remove extra rows introduced
ri_L_only_long = ri_L_only_long %>%
  filter(RI_Type != "RI_current.Race") %>%
  mutate(RI_Type = case_when(RI_Type=="RI_current.RI" ~ "Current RI",
                             RI_Type=="RI_simulated" ~ "Simulated RI"))

# Plot with two bars per race (RI_current and RI_simulated)
ri_L_only_plot <- ggplot(ri_L_only_long, aes(x = Race, y = RI_Value, fill = RI_Type)) +
  geom_col(position = "dodge", color = "black", width = 0.8) +
  theme_minimal() +
  geom_text(aes(label = round(RI_Value, digits = 2)), 
            position = position_dodge(width = 0.8), vjust = -0.5, size = 3) +
  labs(x = "Race", y = "RI", fill = "RI Type") +
  theme(axis.title = element_text(size = 17), axis.text = element_text(size = 15)) +
  theme(legend.position = "bottom") +  # Move legend to the bottom
  #scale_fill_discrete(labels = c("Current RI", "Simulated RI")) 
  scale_fill_manual(values = c("Current RI" = "#E69F00", "Simulated RI" = "#56B4E9"),
                    labels = c("Current RI", "Simulated RI"))
# Save the plot
ggsave(filename = "ri_L_only.svg", plot = ri_L_only_plot, 
       width = 8, height = 6, device = "svg")

################################################################################

# plot RI change given 4th grade round of universal screening
ri_fourth = data.frame(Race = c("White", "Asian", "Latinx", "TMR",
                                "Other"),RI_current = RI,
                       RI_simulated = univ_scr_sim_results[1])
ri_fourth = ri_fourth %>%
  rename(RI_simulated=X..i..)
# Reshape the data frame to long format for ggplot2
ri_fourth_long <- melt(ri_fourth, id.vars = "Race", 
                       variable.name = "RI_Type", value.name = "RI_Value")
ri_fourth_long$RI_Value <- as.numeric(ri_fourth_long$RI_Value)

# remove extra rows introduced
ri_fourth_long = ri_fourth_long %>%
  filter(RI_Type != "RI_current.Race") %>%
  mutate(RI_Type = case_when(RI_Type=="RI_current.RI" ~ "Current RI",
                             RI_Type=="RI_simulated" ~ "Simulated RI"))

# Plot with two bars per race (RI_current and RI_simulated)
ri_fourth_plot <- ggplot(ri_fourth_long, aes(x = Race, y = RI_Value, fill = RI_Type)) +
  geom_col(position = "dodge", color = "black", width = 0.8) +
  theme_minimal() +
  geom_text(aes(label = round(RI_Value, digits = 2)), 
            position = position_dodge(width = 0.8), vjust = -0.5, size = 3) +
  labs(x = "Race", y = "RI", fill = "RI Type") +
  theme(axis.title = element_text(size = 17), axis.text = element_text(size = 15)) +
  theme(legend.position = "bottom") +  # Move legend to the bottom
  #scale_fill_discrete(labels = c("Current RI", "Simulated RI")) 
  scale_fill_manual(values = c("Current RI" = "#E69F00", "Simulated RI" = "#56B4E9"),
                    labels = c("Current RI", "Simulated RI"))

# Save the plot
ggsave(filename = "ri_fourth.svg", plot = ri_fourth_plot, 
       width = 8, height = 6, device = "svg")