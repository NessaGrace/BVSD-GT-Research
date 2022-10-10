#' This script stores transition probabilities and uses them to construct
#' transition matrices. We also feed in an initial state vector.

#TODO: still need to add header name vector?

# Keep in case needed for header vector
#W_i_1 <- c(K_B_W_i, K_GT_W_i, K_NGT_W_i, Gr_1_GT_W_i, Gr_1_NGT_W_i,
#         Gr_2_GT_W_i, Gr_2_NGT_W_i, Gr_3_GT_W_i, Gr_3_NGT_W_i, Gr_4_GT_W_i,
#         Gr_4_NGT_W_i, Gr_5_GT_W_i, Gr_5_NGT_W_i, MS_GT_W_i, MS_NGT_W_i)

#TODO: clean up /improve code including random comments, doc best practices

#TODO: config file

#TODO: improve variable names? esp TMR vs 2MR

#TODO: make sure output makes sense, double check data file, check matrices again for neg #'s, 
#0's, NA's, config, make visualizations and do trends 
#modeling next (see other lists)

# Note: The absorbing states continue to grow. For example, the GT MS population is 66
# for Asian students in year 1 because it adds the 38 students in this state at the end of
# 17-18 from the initial state vector with the 28 students who enter this state at the end of
# year 1.

# Note: Students pass through the chain to the absorbing states where they collect. New students
# are fed into the chain using the K_B vectors to represent the number of kindergartners
# entering the district every year. K_B is always 0 at the end of the year, as students
# have been sorted into GT or non-GT by the end of the year. K_B's for year 1 (18-19) are 
# included in the initial state vectors. These are the kindergartners entering the district
# in the 18-19 year and were in pre-K in the 17-18 year.

install.packages("tidyverse")
library("dplyr")
library(readxl)

source("functions_lib.R")

# Read in data from file:
data <- read_xlsx("Initial_state_vector_&_transition_probability_parameters.xlsx", 
                  sheet=1, 
                  guess_max = 10000)

# Initial State Vectors:

#' The initial state vector is the distribution of students at the
#' end of the 2017-2018 academic year. It is the number of students in each of
#' the 15 transition states. There are five initial state vectors, one per
#' racial/ethnic group.

W_i <- get_column(data, Variable_Type, "White initial", Variable_Value)
A_i <- get_column(data, Variable_Type, "Asian initial", Variable_Value)
L_i <- get_column(data, Variable_Type, "Latinx initial", Variable_Value)
TMR_i <- get_column(data, Variable_Type, "2MR initial", Variable_Value)
Oth_i <- get_column(data, Variable_Type, "Other initial", Variable_Value)

##############YEAR 1##############################

#### Vectors of the data by race for year 1: #####

# White Year 1:
W_yr1 <- get_column(data, Variable_Type, "White yr1", Variable_Value)

# Asian Year 1:
A_yr1 <- get_column(data, Variable_Type, "Asian yr1", Variable_Value)

# Hispanic/Latinx Year 1:
L_yr1 <- get_column(data, Variable_Type, "Latinx yr1", Variable_Value)

# 2MR Year 1:
TMR_yr1 <- get_column(data, Variable_Type, "2MR yr1", Variable_Value)

# Other Year 1:
Oth_yr1 <- get_column(data, Variable_Type, "Other yr1", Variable_Value)

#### Transition matrices for year 1: ####

# White Matrix:
mat_w_yr1 <- make_matrix(W_yr1)

# Asian Matrix Rows:
mat_a_yr1 <- make_matrix(A_yr1)

# Hispanic/Latinx Rows:
mat_L_yr1 <- make_matrix(L_yr1)

# 2MR Rows:
mat_TMR_yr1 <- make_matrix(TMR_yr1)

# Other Rows:
mat_Oth_yr1 <- make_matrix(Oth_yr1)

##############Year 2##############################

#### Vectors of the data by race for Year 2: ####

# White Year 2:
W_yr2 <- get_column(data, Variable_Type, "White yr2", Variable_Value)

# Asian Year 2:
A_yr2 <- get_column(data, Variable_Type, "Asian yr2", Variable_Value)

# Hispanic/Latinx Year 2:
L_yr2 <- get_column(data, Variable_Type, "Latinx yr2", Variable_Value)

# 2MR Year 2:
TMR_yr2 <- get_column(data, Variable_Type, "2MR yr2", Variable_Value)

# Other Year 2:
Oth_yr2 <- get_column(data, Variable_Type, "Other yr2", Variable_Value)

#### Transition matrices for year 2: ####

# White Matrix:
mat_w_yr2 <- make_matrix(W_yr2)

# Asian Matrix Rows:
mat_a_yr2 <- make_matrix(A_yr2)

# Hispanic/Latinx Rows:
mat_L_yr2 <- make_matrix(L_yr2)

# 2MR Rows:
mat_TMR_yr2 <- make_matrix(TMR_yr2)

# Other Rows:
mat_Oth_yr2 <- make_matrix(Oth_yr2)

##############Year 3##############################

#### Vectors of the data by race for Year 3: ####

# White Year 3:
W_yr3 <- get_column(data, Variable_Type, "White yr3", Variable_Value)

# Asian Year 3:
A_yr3 <- get_column(data, Variable_Type, "Asian yr3", Variable_Value)

# Hispanic/Latinx Year 3:
L_yr3 <- get_column(data, Variable_Type, "Latinx yr3", Variable_Value)

# 2MR Year 3:
TMR_yr3 <- get_column(data, Variable_Type, "2MR yr3", Variable_Value)

# Other Year 3:
Oth_yr3 <- get_column(data, Variable_Type, "Other yr3", Variable_Value)

#### Transition matrices for year 3: ####

# White Matrix:
mat_w_yr3 <- make_matrix(W_yr3)

# Asian Matrix Rows:
mat_a_yr3 <- make_matrix(A_yr3)

# Hispanic/Latinx Rows:
mat_L_yr3 <- make_matrix(L_yr3)

# 2MR Rows:
mat_TMR_yr3 <- make_matrix(TMR_yr3)

# Other Rows:
mat_Oth_yr3 <- make_matrix(Oth_yr3)

##############Year 4##############################

#### Vectors of the data by race for Year 4: ####

# White Year 4:
W_yr4 <- get_column(data, Variable_Type, "White yr4", Variable_Value)

# Asian Year 4:
A_yr4 <- get_column(data, Variable_Type, "Asian yr4", Variable_Value)

# Hispanic/Latinx Year 4:
L_yr4 <- get_column(data, Variable_Type, "Latinx yr4", Variable_Value)

# 2MR Year 4:
TMR_yr4 <- get_column(data, Variable_Type, "2MR yr4", Variable_Value)

# Other Year 4:
Oth_yr4 <- get_column(data, Variable_Type, "Other yr4", Variable_Value)

#### Transition matrices for year 4: ####

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

# Simulations:

# White:
sim_1_w <- W_i %*% mat_w_yr1 #output of this (sim_1_w) is student #'s @ end of 18-19
#sim_1_w

sim_2_w_kb <- get_kb("White yr2 K_B")
sim_2_w <- (sim_1_w + sim_2_w_kb) %*% mat_w_yr2
#sim_2_w

sim_3_w_kb <- get_kb("White yr3 K_B")
sim_3_w <- (sim_2_w + sim_3_w_kb) %*% mat_w_yr3
#sim_3_w

sim_4_w_kb <- get_kb("White yr4 K_B")
sim_4_w <- (sim_3_w + sim_4_w_kb) %*% mat_w_yr4
#sim_4_w

# Add predictions below (start with 21-22 to 22-23 transition, i.e. yr5)

# Asian:
sim_1_a <- A_i %*% mat_a_yr1 #output of this (sim_1_a) is student #'s @ end of 18-19
#sim_1_a

sim_2_a_kb <- get_kb("Asian yr2 K_B")
sim_2_a <- (sim_1_a + sim_2_a_kb) %*% mat_a_yr2
#sim_2_a

sim_3_a_kb <- get_kb("Asian yr3 K_B")
sim_3_a <- (sim_2_a + sim_3_a_kb) %*% mat_a_yr3
#sim_3_a

sim_4_a_kb <- get_kb("Asian yr4 K_B")
sim_4_a <- (sim_3_a + sim_4_a_kb) %*% mat_a_yr4
#sim_4_a

# Add predictions below (start with 21-22 to 22-23 transition, i.e. yr5)

# Latinx:
sim_1_L <- L_i %*% mat_L_yr1 #output of this (sim_1_L) is student #'s @ end of 18-19
#sim_1_L

sim_2_L_kb <- get_kb("Latinx yr2 K_B")
sim_2_L <- (sim_1_L + sim_2_L_kb) %*% mat_L_yr2
#sim_2_L

sim_3_L_kb <- get_kb("Latinx yr3 K_B")
sim_3_L <- (sim_2_L + sim_3_L_kb) %*% mat_L_yr3
#sim_3_L

sim_4_L_kb <- get_kb("Latinx yr4 K_B")
sim_4_L <- (sim_3_L + sim_4_L_kb) %*% mat_L_yr4
#sim_4_L

# Add predictions below (start with 21-22 to 22-23 transition, i.e. yr5)

# TMR:
sim_1_TMR <- TMR_i %*% mat_TMR_yr1 #output of this (sim_1_TMR) is student #'s @ end of 18-19
#sim_1_TMR

sim_2_TMR_kb <- get_kb("2MR yr2 K_B")
sim_2_TMR <- (sim_1_TMR + sim_2_TMR_kb) %*% mat_TMR_yr2
#sim_2_TMR

sim_3_TMR_kb <- get_kb("2MR yr3 K_B")
sim_3_TMR <- (sim_2_TMR + sim_3_TMR_kb) %*% mat_TMR_yr3
#sim_3_TMR

sim_4_TMR_kb <- get_kb("2MR yr4 K_B")
sim_4_TMR <- (sim_3_TMR + sim_4_TMR_kb) %*% mat_TMR_yr4
#sim_4_TMR

# Add predictions below (start with 21-22 to 22-23 transition, i.e. yr5)

# Other:
sim_1_Oth <- Oth_i %*% mat_Oth_yr1 #output of this (sim_1_Oth) is student #'s @ end of 18-19
#sim_1_Oth

sim_2_Oth_kb <- get_kb("Other yr2 K_B")
sim_2_Oth <- (sim_1_Oth + sim_2_Oth_kb) %*% mat_Oth_yr2
#sim_2_Oth

sim_3_Oth_kb <- get_kb("Other yr3 K_B")
sim_3_Oth <- (sim_2_Oth + sim_3_Oth_kb) %*% mat_Oth_yr3
#sim_3_Oth

sim_4_Oth_kb <- get_kb("Other yr4 K_B")
sim_4_Oth <- (sim_3_Oth + sim_4_Oth_kb) %*% mat_Oth_yr4
#sim_4_Oth

# Add predictions below (start with 21-22 to 22-23 transition, i.e. yr5)