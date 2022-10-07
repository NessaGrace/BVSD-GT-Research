#' This script stores transition probabilities and uses them to construct
#' transition matrices. We also feed in an initial state vector.

#TODO: still need to add header name vector?

# Keep in case needed for header vector
#W_i_1 <- c(K_B_W_i, K_GT_W_i, K_NGT_W_i, Gr_1_GT_W_i, Gr_1_NGT_W_i,
#         Gr_2_GT_W_i, Gr_2_NGT_W_i, Gr_3_GT_W_i, Gr_3_NGT_W_i, Gr_4_GT_W_i,
#         Gr_4_NGT_W_i, Gr_5_GT_W_i, Gr_5_NGT_W_i, MS_GT_W_i, MS_NGT_W_i)

#TODO: clean up code, add make_matrix_rows(), make_matrix(), get_k_b() functions

#TODO: config file

#TODO: improve variable names? esp TMR vs 2MR

#TODO: add K_B's to simulations, make sure they make sense with output, and finish 
#other sim's, do functions & config, make visualizations and do trends modeling next 
#(see other lists)

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
data <- read_xlsx("Initial_state_vector_&_transition_probability_parameters.xlsx", sheet=1, guess_max = 10000)

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

# Hard-coded matrices for transition probabilities:

##############YEAR 1##############################

# Vectors of the data by race for year 1:

# White Year 1:
#W_yr1 <- c(data$Variable_Value[76:88]) #keep for reference just in case
W_yr1 <- get_column(data, Variable_Type, "White yr1", Variable_Value)

# Asian Year 1:
A_yr1 <- get_column(data, Variable_Type, "Asian yr1", Variable_Value)

# Hispanic/Latinx Year 1:
L_yr1 <- get_column(data, Variable_Type, "Latinx yr1", Variable_Value)

# 2MR Year 1:
TMR_yr1 <- get_column(data, Variable_Type, "2MR yr1", Variable_Value)

# Other Year 1:
Oth_yr1 <- get_column(data, Variable_Type, "Other yr1", Variable_Value)

# Vectors to feed into transition matrices for year 1:

# White Matrix Rows: (r stands for row)
# first 13 rows from csv, last 2 rows from absorbing states
W_yr1_r1 <- c(0, W_yr1[1], 1-W_yr1[1], integer(12))
W_yr1_r2 <- c(integer(3), W_yr1[2], 1-W_yr1[2], integer(10))
W_yr1_r3 <- c(integer(3), W_yr1[3], 1-W_yr1[3], integer (10))
W_yr1_r4 <- c(integer(5), W_yr1[4], 1-W_yr1[4], integer(8))
W_yr1_r5 <- c(integer(5), W_yr1[5], 1-W_yr1[5], integer(8))
W_yr1_r6 <- c(integer(7), W_yr1[6], 1-W_yr1[6], integer(6))
W_yr1_r7 <- c(integer(7), W_yr1[7], 1-W_yr1[7], integer(6))
W_yr1_r8 <- c(integer(9), W_yr1[8], 1-W_yr1[8], integer(4))
W_yr1_r9 <- c(integer(9), W_yr1[9], 1-W_yr1[9], integer(4))
W_yr1_r10 <- c(integer(11), W_yr1[10], 1-W_yr1[10], integer(2))
W_yr1_r11 <- c(integer(11), W_yr1[11], 1-W_yr1[11], integer(2))
W_yr1_r12 <- c(integer(13), W_yr1[12], 1-W_yr1[12])
W_yr1_r13 <- c(integer(13), W_yr1[13], 1-W_yr1[13])
W_yr1_r14 <- c(integer(13), 1, 0)
W_yr1_r15 <- c(integer(14), 1)

# Asian Matrix Rows:
A_yr1_r1 <- c(0, A_yr1[1], 1-A_yr1[1], integer(12))
A_yr1_r2 <- c(integer(3), A_yr1[2], 1-A_yr1[2], integer(10))
A_yr1_r3 <- c(integer(3), A_yr1[3], 1-A_yr1[3], integer (10))
A_yr1_r4 <- c(integer(5), A_yr1[4], 1-A_yr1[4], integer(8))
A_yr1_r5 <- c(integer(5), A_yr1[5], 1-A_yr1[5], integer(8))
A_yr1_r6 <- c(integer(7), A_yr1[6], 1-A_yr1[6], integer(6))
A_yr1_r7 <- c(integer(7), A_yr1[7], 1-A_yr1[7], integer(6))
A_yr1_r8 <- c(integer(9), A_yr1[8], 1-A_yr1[8], integer(4))
A_yr1_r9 <- c(integer(9), A_yr1[9], 1-A_yr1[9], integer(4))
A_yr1_r10 <- c(integer(11), A_yr1[10], 1-A_yr1[10], integer(2))
A_yr1_r11 <- c(integer(11), A_yr1[11], 1-A_yr1[11], integer(2))
A_yr1_r12 <- c(integer(13), A_yr1[12], 1-A_yr1[12])
A_yr1_r13 <- c(integer(13), A_yr1[13], 1-A_yr1[13])
A_yr1_r14 <- c(integer(13), 1, 0)
A_yr1_r15 <- c(integer(14), 1)

# Hispanic/Latinx Rows:
L_yr1_r1 <- c(0, L_yr1[1], 1-L_yr1[1], integer(12))
L_yr1_r2 <- c(integer(3), L_yr1[2], 1-L_yr1[2], integer(10))
L_yr1_r3 <- c(integer(3), L_yr1[3], 1-L_yr1[3], integer (10))
L_yr1_r4 <- c(integer(5), L_yr1[4], 1-L_yr1[4], integer(8))
L_yr1_r5 <- c(integer(5), L_yr1[5], 1-L_yr1[5], integer(8))
L_yr1_r6 <- c(integer(7), L_yr1[6], 1-L_yr1[6], integer(6))
L_yr1_r7 <- c(integer(7), L_yr1[7], 1-L_yr1[7], integer(6))
L_yr1_r8 <- c(integer(9), L_yr1[8], 1-L_yr1[8], integer(4))
L_yr1_r9 <- c(integer(9), L_yr1[9], 1-L_yr1[9], integer(4))
L_yr1_r10 <- c(integer(11), L_yr1[10], 1-L_yr1[10], integer(2))
L_yr1_r11 <- c(integer(11), L_yr1[11], 1-L_yr1[11], integer(2))
L_yr1_r12 <- c(integer(13), L_yr1[12], 1-L_yr1[12])
L_yr1_r13 <- c(integer(13), L_yr1[13], 1-L_yr1[13])
L_yr1_r14 <- c(integer(13), 1, 0)
L_yr1_r15 <- c(integer(14), 1)

# 2MR Rows:
TMR_yr1_r1 <- c(0, TMR_yr1[1], 1-TMR_yr1[1], integer(12))
TMR_yr1_r2 <- c(integer(3), TMR_yr1[2], 1-TMR_yr1[2], integer(10))
TMR_yr1_r3 <- c(integer(3), TMR_yr1[3], 1-TMR_yr1[3], integer (10))
TMR_yr1_r4 <- c(integer(5), TMR_yr1[4], 1-TMR_yr1[4], integer(8))
TMR_yr1_r5 <- c(integer(5), TMR_yr1[5], 1-TMR_yr1[5], integer(8))
TMR_yr1_r6 <- c(integer(7), TMR_yr1[6], 1-TMR_yr1[6], integer(6))
TMR_yr1_r7 <- c(integer(7), TMR_yr1[7], 1-TMR_yr1[7], integer(6))
TMR_yr1_r8 <- c(integer(9), TMR_yr1[8], 1-TMR_yr1[8], integer(4))
TMR_yr1_r9 <- c(integer(9), TMR_yr1[9], 1-TMR_yr1[9], integer(4))
TMR_yr1_r10 <- c(integer(11), TMR_yr1[10], 1-TMR_yr1[10], integer(2))
TMR_yr1_r11 <- c(integer(11), TMR_yr1[11], 1-TMR_yr1[11], integer(2))
TMR_yr1_r12 <- c(integer(13), TMR_yr1[12], 1-TMR_yr1[12])
TMR_yr1_r13 <- c(integer(13), TMR_yr1[13], 1-TMR_yr1[13])
TMR_yr1_r14 <- c(integer(13), 1, 0)
TMR_yr1_r15 <- c(integer(14), 1)

# Other Rows:
Oth_yr1_r1 <- c(0, Oth_yr1[1], 1-Oth_yr1[1], integer(12))
Oth_yr1_r2 <- c(integer(3), Oth_yr1[2], 1-Oth_yr1[2], integer(10))
Oth_yr1_r3 <- c(integer(3), Oth_yr1[3], 1-Oth_yr1[3], integer (10))
Oth_yr1_r4 <- c(integer(5), Oth_yr1[4], 1-Oth_yr1[4], integer(8))
Oth_yr1_r5 <- c(integer(5), Oth_yr1[5], 1-Oth_yr1[5], integer(8))
Oth_yr1_r6 <- c(integer(7), Oth_yr1[6], 1-Oth_yr1[6], integer(6))
Oth_yr1_r7 <- c(integer(7), Oth_yr1[7], 1-Oth_yr1[7], integer(6))
Oth_yr1_r8 <- c(integer(9), Oth_yr1[8], 1-Oth_yr1[8], integer(4))
Oth_yr1_r9 <- c(integer(9), Oth_yr1[9], 1-Oth_yr1[9], integer(4))
Oth_yr1_r10 <- c(integer(11), Oth_yr1[10], 1-Oth_yr1[10], integer(2))
Oth_yr1_r11 <- c(integer(11), Oth_yr1[11], 1-Oth_yr1[11], integer(2))
Oth_yr1_r12 <- c(integer(13), Oth_yr1[12], 1-Oth_yr1[12])
Oth_yr1_r13 <- c(integer(13), Oth_yr1[13], 1-Oth_yr1[13])
Oth_yr1_r14 <- c(integer(13), 1, 0)
Oth_yr1_r15 <- c(integer(14), 1)

# Transition matrices for year 1:

# White, Year 1:
mat_w_yr1 <- t(matrix(c(W_yr1_r1, W_yr1_r2, W_yr1_r3, W_yr1_r4,
                        W_yr1_r5, W_yr1_r6, W_yr1_r7, W_yr1_r8,
                        W_yr1_r9, W_yr1_r10, W_yr1_r11, W_yr1_r12,
                        W_yr1_r13, W_yr1_r14, W_yr1_r15), nrow=15))

# Asian, Year 1:
mat_a_yr1 <- t(matrix(c(A_yr1_r1, A_yr1_r2, A_yr1_r3, A_yr1_r4,
                        A_yr1_r5, A_yr1_r6, A_yr1_r7, A_yr1_r8,
                        A_yr1_r9, A_yr1_r10, A_yr1_r11, A_yr1_r12,
                        A_yr1_r13, A_yr1_r14, A_yr1_r15), nrow=15))
#mat_a_yr1

# Hispanic/Latinx, Year 1:
mat_L_yr1 <- t(matrix(c(L_yr1_r1, L_yr1_r2, L_yr1_r3, L_yr1_r4,
                        L_yr1_r5, L_yr1_r6, L_yr1_r7, L_yr1_r8,
                        L_yr1_r9, L_yr1_r10, L_yr1_r11, L_yr1_r12,
                        L_yr1_r13, L_yr1_r14, L_yr1_r15), nrow=15))

# 2MR, Year 1:
mat_TMR_yr1 <- t(matrix(c(TMR_yr1_r1, TMR_yr1_r2, TMR_yr1_r3, TMR_yr1_r4,
                          TMR_yr1_r5, TMR_yr1_r6, TMR_yr1_r7, TMR_yr1_r8,
                          TMR_yr1_r9, TMR_yr1_r10, TMR_yr1_r11, TMR_yr1_r12,
                          TMR_yr1_r13, TMR_yr1_r14, TMR_yr1_r15), nrow=15))

# Other, Year 1:
mat_Oth_yr1 <- t(matrix(c(Oth_yr1_r1, Oth_yr1_r2, Oth_yr1_r3, Oth_yr1_r4,
                          Oth_yr1_r5, Oth_yr1_r6, Oth_yr1_r7, Oth_yr1_r8,
                          Oth_yr1_r9, Oth_yr1_r10, Oth_yr1_r11, Oth_yr1_r12,
                          Oth_yr1_r13, Oth_yr1_r14, Oth_yr1_r15), nrow=15))

##############Year 2##############################

# Vectors of the data by race for Year 2:

# White Year 2:
#W_yr2 <- c(data$Variable_Value[76:88]) #keep for reference just in case
W_yr2 <- get_column(data, Variable_Type, "White yr2", Variable_Value)

# Asian Year 2:
A_yr2 <- get_column(data, Variable_Type, "Asian yr2", Variable_Value)

# Hispanic/Latinx Year 2:
L_yr2 <- get_column(data, Variable_Type, "Latinx yr2", Variable_Value)

# 2MR Year 2:
TMR_yr2 <- get_column(data, Variable_Type, "2MR yr2", Variable_Value)

# Other Year 2:
Oth_yr2 <- get_column(data, Variable_Type, "Other yr2", Variable_Value)

# Vectors to feed into transition matrices for Year 2:

# White Matrix Rows: (r stands for row)
# first 13 rows from csv, last 2 rows from absorbing states
W_yr2_r1 <- c(0, W_yr2[1], 1-W_yr2[1], integer(12))
W_yr2_r2 <- c(integer(3), W_yr2[2], 1-W_yr2[2], integer(10))
W_yr2_r3 <- c(integer(3), W_yr2[3], 1-W_yr2[3], integer (10))
W_yr2_r4 <- c(integer(5), W_yr2[4], 1-W_yr2[4], integer(8))
W_yr2_r5 <- c(integer(5), W_yr2[5], 1-W_yr2[5], integer(8))
W_yr2_r6 <- c(integer(7), W_yr2[6], 1-W_yr2[6], integer(6))
W_yr2_r7 <- c(integer(7), W_yr2[7], 1-W_yr2[7], integer(6))
W_yr2_r8 <- c(integer(9), W_yr2[8], 1-W_yr2[8], integer(4))
W_yr2_r9 <- c(integer(9), W_yr2[9], 1-W_yr2[9], integer(4))
W_yr2_r10 <- c(integer(11), W_yr2[10], 1-W_yr2[10], integer(2))
W_yr2_r11 <- c(integer(11), W_yr2[11], 1-W_yr2[11], integer(2))
W_yr2_r12 <- c(integer(13), W_yr2[12], 1-W_yr2[12])
W_yr2_r13 <- c(integer(13), W_yr2[13], 1-W_yr2[13])
W_yr2_r14 <- c(integer(13), 1, 0)
W_yr2_r15 <- c(integer(14), 1)

# Asian Matrix Rows:
A_yr2_r1 <- c(0, A_yr2[1], 1-A_yr2[1], integer(12))
A_yr2_r2 <- c(integer(3), A_yr2[2], 1-A_yr2[2], integer(10))
A_yr2_r3 <- c(integer(3), A_yr2[3], 1-A_yr2[3], integer (10))
A_yr2_r4 <- c(integer(5), A_yr2[4], 1-A_yr2[4], integer(8))
A_yr2_r5 <- c(integer(5), A_yr2[5], 1-A_yr2[5], integer(8))
A_yr2_r6 <- c(integer(7), A_yr2[6], 1-A_yr2[6], integer(6))
A_yr2_r7 <- c(integer(7), A_yr2[7], 1-A_yr2[7], integer(6))
A_yr2_r8 <- c(integer(9), A_yr2[8], 1-A_yr2[8], integer(4))
A_yr2_r9 <- c(integer(9), A_yr2[9], 1-A_yr2[9], integer(4))
A_yr2_r10 <- c(integer(11), A_yr2[10], 1-A_yr2[10], integer(2))
A_yr2_r11 <- c(integer(11), A_yr2[11], 1-A_yr2[11], integer(2))
A_yr2_r12 <- c(integer(13), A_yr2[12], 1-A_yr2[12])
A_yr2_r13 <- c(integer(13), A_yr2[13], 1-A_yr2[13])
A_yr2_r14 <- c(integer(13), 1, 0)
A_yr2_r15 <- c(integer(14), 1)

# Hispanic/Latinx Rows:
L_yr2_r1 <- c(0, L_yr2[1], 1-L_yr2[1], integer(12))
L_yr2_r2 <- c(integer(3), L_yr2[2], 1-L_yr2[2], integer(10))
L_yr2_r3 <- c(integer(3), L_yr2[3], 1-L_yr2[3], integer (10))
L_yr2_r4 <- c(integer(5), L_yr2[4], 1-L_yr2[4], integer(8))
L_yr2_r5 <- c(integer(5), L_yr2[5], 1-L_yr2[5], integer(8))
L_yr2_r6 <- c(integer(7), L_yr2[6], 1-L_yr2[6], integer(6))
L_yr2_r7 <- c(integer(7), L_yr2[7], 1-L_yr2[7], integer(6))
L_yr2_r8 <- c(integer(9), L_yr2[8], 1-L_yr2[8], integer(4))
L_yr2_r9 <- c(integer(9), L_yr2[9], 1-L_yr2[9], integer(4))
L_yr2_r10 <- c(integer(11), L_yr2[10], 1-L_yr2[10], integer(2))
L_yr2_r11 <- c(integer(11), L_yr2[11], 1-L_yr2[11], integer(2))
L_yr2_r12 <- c(integer(13), L_yr2[12], 1-L_yr2[12])
L_yr2_r13 <- c(integer(13), L_yr2[13], 1-L_yr2[13])
L_yr2_r14 <- c(integer(13), 1, 0)
L_yr2_r15 <- c(integer(14), 1)

# 2MR Rows:
TMR_yr2_r1 <- c(0, TMR_yr2[1], 1-TMR_yr2[1], integer(12))
TMR_yr2_r2 <- c(integer(3), TMR_yr2[2], 1-TMR_yr2[2], integer(10))
TMR_yr2_r3 <- c(integer(3), TMR_yr2[3], 1-TMR_yr2[3], integer (10))
TMR_yr2_r4 <- c(integer(5), TMR_yr2[4], 1-TMR_yr2[4], integer(8))
TMR_yr2_r5 <- c(integer(5), TMR_yr2[5], 1-TMR_yr2[5], integer(8))
TMR_yr2_r6 <- c(integer(7), TMR_yr2[6], 1-TMR_yr2[6], integer(6))
TMR_yr2_r7 <- c(integer(7), TMR_yr2[7], 1-TMR_yr2[7], integer(6))
TMR_yr2_r8 <- c(integer(9), TMR_yr2[8], 1-TMR_yr2[8], integer(4))
TMR_yr2_r9 <- c(integer(9), TMR_yr2[9], 1-TMR_yr2[9], integer(4))
TMR_yr2_r10 <- c(integer(11), TMR_yr2[10], 1-TMR_yr2[10], integer(2))
TMR_yr2_r11 <- c(integer(11), TMR_yr2[11], 1-TMR_yr2[11], integer(2))
TMR_yr2_r12 <- c(integer(13), TMR_yr2[12], 1-TMR_yr2[12])
TMR_yr2_r13 <- c(integer(13), TMR_yr2[13], 1-TMR_yr2[13])
TMR_yr2_r14 <- c(integer(13), 1, 0)
TMR_yr2_r15 <- c(integer(14), 1)

# Other Rows:
Oth_yr2_r1 <- c(0, Oth_yr2[1], 1-Oth_yr2[1], integer(12))
Oth_yr2_r2 <- c(integer(3), Oth_yr2[2], 1-Oth_yr2[2], integer(10))
Oth_yr2_r3 <- c(integer(3), Oth_yr2[3], 1-Oth_yr2[3], integer (10))
Oth_yr2_r4 <- c(integer(5), Oth_yr2[4], 1-Oth_yr2[4], integer(8))
Oth_yr2_r5 <- c(integer(5), Oth_yr2[5], 1-Oth_yr2[5], integer(8))
Oth_yr2_r6 <- c(integer(7), Oth_yr2[6], 1-Oth_yr2[6], integer(6))
Oth_yr2_r7 <- c(integer(7), Oth_yr2[7], 1-Oth_yr2[7], integer(6))
Oth_yr2_r8 <- c(integer(9), Oth_yr2[8], 1-Oth_yr2[8], integer(4))
Oth_yr2_r9 <- c(integer(9), Oth_yr2[9], 1-Oth_yr2[9], integer(4))
Oth_yr2_r10 <- c(integer(11), Oth_yr2[10], 1-Oth_yr2[10], integer(2))
Oth_yr2_r11 <- c(integer(11), Oth_yr2[11], 1-Oth_yr2[11], integer(2))
Oth_yr2_r12 <- c(integer(13), Oth_yr2[12], 1-Oth_yr2[12])
Oth_yr2_r13 <- c(integer(13), Oth_yr2[13], 1-Oth_yr2[13])
Oth_yr2_r14 <- c(integer(13), 1, 0)
Oth_yr2_r15 <- c(integer(14), 1)

# Transition matrices for Year 2:

# White, Year 2:
mat_w_yr2 <- t(matrix(c(W_yr2_r1, W_yr2_r2, W_yr2_r3, W_yr2_r4,
                        W_yr2_r5, W_yr2_r6, W_yr2_r7, W_yr2_r8,
                        W_yr2_r9, W_yr2_r10, W_yr2_r11, W_yr2_r12,
                        W_yr2_r13, W_yr2_r14, W_yr2_r15), nrow=15))

# Asian, Year 2:
mat_a_yr2 <- t(matrix(c(A_yr2_r1, A_yr2_r2, A_yr2_r3, A_yr2_r4,
                        A_yr2_r5, A_yr2_r6, A_yr2_r7, A_yr2_r8,
                        A_yr2_r9, A_yr2_r10, A_yr2_r11, A_yr2_r12,
                        A_yr2_r13, A_yr2_r14, A_yr2_r15), nrow=15))
#mat_a_yr2

# Hispanic/Latinx, Year 2:
mat_L_yr2 <- t(matrix(c(L_yr2_r1, L_yr2_r2, L_yr2_r3, L_yr2_r4,
                        L_yr2_r5, L_yr2_r6, L_yr2_r7, L_yr2_r8,
                        L_yr2_r9, L_yr2_r10, L_yr2_r11, L_yr2_r12,
                        L_yr2_r13, L_yr2_r14, L_yr2_r15), nrow=15))

# 2MR, Year 2:
mat_TMR_yr2 <- t(matrix(c(TMR_yr2_r1, TMR_yr2_r2, TMR_yr2_r3, TMR_yr2_r4,
                          TMR_yr2_r5, TMR_yr2_r6, TMR_yr2_r7, TMR_yr2_r8,
                          TMR_yr2_r9, TMR_yr2_r10, TMR_yr2_r11, TMR_yr2_r12,
                          TMR_yr2_r13, TMR_yr2_r14, TMR_yr2_r15), nrow=15))

# Other, Year 2:
mat_Oth_yr2 <- t(matrix(c(Oth_yr2_r1, Oth_yr2_r2, Oth_yr2_r3, Oth_yr2_r4,
                          Oth_yr2_r5, Oth_yr2_r6, Oth_yr2_r7, Oth_yr2_r8,
                          Oth_yr2_r9, Oth_yr2_r10, Oth_yr2_r11, Oth_yr2_r12,
                          Oth_yr2_r13, Oth_yr2_r14, Oth_yr2_r15), nrow=15))

##############Year 3##############################

# Vectors of the data by race for Year 3:

# White Year 3:
#W_yr3 <- c(data$Variable_Value[76:88]) #keep for reference just in case
W_yr3 <- get_column(data, Variable_Type, "White yr3", Variable_Value)
#W_yr3

# Asian Year 3:
A_yr3 <- get_column(data, Variable_Type, "Asian yr3", Variable_Value)

# Hispanic/Latinx Year 3:
L_yr3 <- get_column(data, Variable_Type, "Latinx yr3", Variable_Value)

# 2MR Year 3:
TMR_yr3 <- get_column(data, Variable_Type, "2MR yr3", Variable_Value)

# Other Year 3:
Oth_yr3 <- get_column(data, Variable_Type, "Other yr3", Variable_Value)
#Oth_yr3

# Vectors to feed into transition matrices for Year 3:

# White Matrix Rows: (r stands for row)
# first 13 rows from csv, last 2 rows from absorbing states
W_yr3_r1 <- c(0, W_yr3[1], 1-W_yr3[1], integer(12))
W_yr3_r2 <- c(integer(3), W_yr3[2], 1-W_yr3[2], integer(10))
W_yr3_r3 <- c(integer(3), W_yr3[3], 1-W_yr3[3], integer (10))
W_yr3_r4 <- c(integer(5), W_yr3[4], 1-W_yr3[4], integer(8))
W_yr3_r5 <- c(integer(5), W_yr3[5], 1-W_yr3[5], integer(8))
W_yr3_r6 <- c(integer(7), W_yr3[6], 1-W_yr3[6], integer(6))
W_yr3_r7 <- c(integer(7), W_yr3[7], 1-W_yr3[7], integer(6))
W_yr3_r8 <- c(integer(9), W_yr3[8], 1-W_yr3[8], integer(4))
W_yr3_r9 <- c(integer(9), W_yr3[9], 1-W_yr3[9], integer(4))
W_yr3_r10 <- c(integer(11), W_yr3[10], 1-W_yr3[10], integer(2))
W_yr3_r11 <- c(integer(11), W_yr3[11], 1-W_yr3[11], integer(2))
W_yr3_r12 <- c(integer(13), W_yr3[12], 1-W_yr3[12])
W_yr3_r13 <- c(integer(13), W_yr3[13], 1-W_yr3[13])
W_yr3_r14 <- c(integer(13), 1, 0)
W_yr3_r15 <- c(integer(14), 1)

# Asian Matrix Rows:
A_yr3_r1 <- c(0, A_yr3[1], 1-A_yr3[1], integer(12))
A_yr3_r2 <- c(integer(3), A_yr3[2], 1-A_yr3[2], integer(10))
A_yr3_r3 <- c(integer(3), A_yr3[3], 1-A_yr3[3], integer (10))
A_yr3_r4 <- c(integer(5), A_yr3[4], 1-A_yr3[4], integer(8))
A_yr3_r5 <- c(integer(5), A_yr3[5], 1-A_yr3[5], integer(8))
A_yr3_r6 <- c(integer(7), A_yr3[6], 1-A_yr3[6], integer(6))
A_yr3_r7 <- c(integer(7), A_yr3[7], 1-A_yr3[7], integer(6))
A_yr3_r8 <- c(integer(9), A_yr3[8], 1-A_yr3[8], integer(4))
A_yr3_r9 <- c(integer(9), A_yr3[9], 1-A_yr3[9], integer(4))
A_yr3_r10 <- c(integer(11), A_yr3[10], 1-A_yr3[10], integer(2))
A_yr3_r11 <- c(integer(11), A_yr3[11], 1-A_yr3[11], integer(2))
A_yr3_r12 <- c(integer(13), A_yr3[12], 1-A_yr3[12])
A_yr3_r13 <- c(integer(13), A_yr3[13], 1-A_yr3[13])
A_yr3_r14 <- c(integer(13), 1, 0)
A_yr3_r15 <- c(integer(14), 1)

# Hispanic/Latinx Rows:
L_yr3_r1 <- c(0, L_yr3[1], 1-L_yr3[1], integer(12))
L_yr3_r2 <- c(integer(3), L_yr3[2], 1-L_yr3[2], integer(10))
L_yr3_r3 <- c(integer(3), L_yr3[3], 1-L_yr3[3], integer (10))
L_yr3_r4 <- c(integer(5), L_yr3[4], 1-L_yr3[4], integer(8))
L_yr3_r5 <- c(integer(5), L_yr3[5], 1-L_yr3[5], integer(8))
L_yr3_r6 <- c(integer(7), L_yr3[6], 1-L_yr3[6], integer(6))
L_yr3_r7 <- c(integer(7), L_yr3[7], 1-L_yr3[7], integer(6))
L_yr3_r8 <- c(integer(9), L_yr3[8], 1-L_yr3[8], integer(4))
L_yr3_r9 <- c(integer(9), L_yr3[9], 1-L_yr3[9], integer(4))
L_yr3_r10 <- c(integer(11), L_yr3[10], 1-L_yr3[10], integer(2))
L_yr3_r11 <- c(integer(11), L_yr3[11], 1-L_yr3[11], integer(2))
L_yr3_r12 <- c(integer(13), L_yr3[12], 1-L_yr3[12])
L_yr3_r13 <- c(integer(13), L_yr3[13], 1-L_yr3[13])
L_yr3_r14 <- c(integer(13), 1, 0)
L_yr3_r15 <- c(integer(14), 1)

# 2MR Rows:
TMR_yr3_r1 <- c(0, TMR_yr3[1], 1-TMR_yr3[1], integer(12))
TMR_yr3_r2 <- c(integer(3), TMR_yr3[2], 1-TMR_yr3[2], integer(10))
TMR_yr3_r3 <- c(integer(3), TMR_yr3[3], 1-TMR_yr3[3], integer (10))
TMR_yr3_r4 <- c(integer(5), TMR_yr3[4], 1-TMR_yr3[4], integer(8))
TMR_yr3_r5 <- c(integer(5), TMR_yr3[5], 1-TMR_yr3[5], integer(8))
TMR_yr3_r6 <- c(integer(7), TMR_yr3[6], 1-TMR_yr3[6], integer(6))
TMR_yr3_r7 <- c(integer(7), TMR_yr3[7], 1-TMR_yr3[7], integer(6))
TMR_yr3_r8 <- c(integer(9), TMR_yr3[8], 1-TMR_yr3[8], integer(4))
TMR_yr3_r9 <- c(integer(9), TMR_yr3[9], 1-TMR_yr3[9], integer(4))
TMR_yr3_r10 <- c(integer(11), TMR_yr3[10], 1-TMR_yr3[10], integer(2))
TMR_yr3_r11 <- c(integer(11), TMR_yr3[11], 1-TMR_yr3[11], integer(2))
TMR_yr3_r12 <- c(integer(13), TMR_yr3[12], 1-TMR_yr3[12])
TMR_yr3_r13 <- c(integer(13), TMR_yr3[13], 1-TMR_yr3[13])
TMR_yr3_r14 <- c(integer(13), 1, 0)
TMR_yr3_r15 <- c(integer(14), 1)

# Other Rows:
Oth_yr3_r1 <- c(0, Oth_yr3[1], 1-Oth_yr3[1], integer(12))
Oth_yr3_r2 <- c(integer(3), Oth_yr3[2], 1-Oth_yr3[2], integer(10))
Oth_yr3_r3 <- c(integer(3), Oth_yr3[3], 1-Oth_yr3[3], integer (10))
Oth_yr3_r4 <- c(integer(5), Oth_yr3[4], 1-Oth_yr3[4], integer(8))
Oth_yr3_r5 <- c(integer(5), Oth_yr3[5], 1-Oth_yr3[5], integer(8))
Oth_yr3_r6 <- c(integer(7), Oth_yr3[6], 1-Oth_yr3[6], integer(6))
Oth_yr3_r7 <- c(integer(7), Oth_yr3[7], 1-Oth_yr3[7], integer(6))
Oth_yr3_r8 <- c(integer(9), Oth_yr3[8], 1-Oth_yr3[8], integer(4))
Oth_yr3_r9 <- c(integer(9), Oth_yr3[9], 1-Oth_yr3[9], integer(4))
Oth_yr3_r10 <- c(integer(11), Oth_yr3[10], 1-Oth_yr3[10], integer(2))
Oth_yr3_r11 <- c(integer(11), Oth_yr3[11], 1-Oth_yr3[11], integer(2))
Oth_yr3_r12 <- c(integer(13), Oth_yr3[12], 1-Oth_yr3[12])
Oth_yr3_r13 <- c(integer(13), Oth_yr3[13], 1-Oth_yr3[13])
Oth_yr3_r14 <- c(integer(13), 1, 0)
Oth_yr3_r15 <- c(integer(14), 1)

# Transition matrices for Year 3:

# White, Year 3:
mat_w_yr3 <- t(matrix(c(W_yr3_r1, W_yr3_r2, W_yr3_r3, W_yr3_r4,
                        W_yr3_r5, W_yr3_r6, W_yr3_r7, W_yr3_r8,
                        W_yr3_r9, W_yr3_r10, W_yr3_r11, W_yr3_r12,
                        W_yr3_r13, W_yr3_r14, W_yr3_r15), nrow=15))

# Asian, Year 3:
mat_a_yr3 <- t(matrix(c(A_yr3_r1, A_yr3_r2, A_yr3_r3, A_yr3_r4,
                        A_yr3_r5, A_yr3_r6, A_yr3_r7, A_yr3_r8,
                        A_yr3_r9, A_yr3_r10, A_yr3_r11, A_yr3_r12,
                        A_yr3_r13, A_yr3_r14, A_yr3_r15), nrow=15))
#mat_a_yr3

# Hispanic/Latinx, Year 3:
mat_L_yr3 <- t(matrix(c(L_yr3_r1, L_yr3_r2, L_yr3_r3, L_yr3_r4,
                        L_yr3_r5, L_yr3_r6, L_yr3_r7, L_yr3_r8,
                        L_yr3_r9, L_yr3_r10, L_yr3_r11, L_yr3_r12,
                        L_yr3_r13, L_yr3_r14, L_yr3_r15), nrow=15))

# 2MR, Year 3:
mat_TMR_yr3 <- t(matrix(c(TMR_yr3_r1, TMR_yr3_r2, TMR_yr3_r3, TMR_yr3_r4,
                          TMR_yr3_r5, TMR_yr3_r6, TMR_yr3_r7, TMR_yr3_r8,
                          TMR_yr3_r9, TMR_yr3_r10, TMR_yr3_r11, TMR_yr3_r12,
                          TMR_yr3_r13, TMR_yr3_r14, TMR_yr3_r15), nrow=15))

# Other, Year 3:
mat_Oth_yr3 <- t(matrix(c(Oth_yr3_r1, Oth_yr3_r2, Oth_yr3_r3, Oth_yr3_r4,
                          Oth_yr3_r5, Oth_yr3_r6, Oth_yr3_r7, Oth_yr3_r8,
                          Oth_yr3_r9, Oth_yr3_r10, Oth_yr3_r11, Oth_yr3_r12,
                          Oth_yr3_r13, Oth_yr3_r14, Oth_yr3_r15), nrow=15))

##############Year 4##############################

# Vectors of the data by race for Year 4:

# White Year 4:
#W_yr4 <- c(data$Variable_Value[76:88]) #keep for reference just in case
W_yr4 <- get_column(data, Variable_Type, "White yr4", Variable_Value)
#W_yr4

# Asian Year 4:
A_yr4 <- get_column(data, Variable_Type, "Asian yr4", Variable_Value)

# Hispanic/Latinx Year 4:
L_yr4 <- get_column(data, Variable_Type, "Latinx yr4", Variable_Value)

# 2MR Year 4:
TMR_yr4 <- get_column(data, Variable_Type, "2MR yr4", Variable_Value)
#TMR_yr4

# Other Year 4:
Oth_yr4 <- get_column(data, Variable_Type, "Other yr4", Variable_Value)
#Oth_yr4

# Vectors to feed into transition matrices for Year 4:

# White Matrix Rows: (r stands for row)
# first 13 rows from csv, last 2 rows from absorbing states
W_yr4_r1 <- c(0, W_yr4[1], 1-W_yr4[1], integer(12))
W_yr4_r2 <- c(integer(3), W_yr4[2], 1-W_yr4[2], integer(10))
W_yr4_r3 <- c(integer(3), W_yr4[3], 1-W_yr4[3], integer (10))
W_yr4_r4 <- c(integer(5), W_yr4[4], 1-W_yr4[4], integer(8))
W_yr4_r5 <- c(integer(5), W_yr4[5], 1-W_yr4[5], integer(8))
W_yr4_r6 <- c(integer(7), W_yr4[6], 1-W_yr4[6], integer(6))
W_yr4_r7 <- c(integer(7), W_yr4[7], 1-W_yr4[7], integer(6))
W_yr4_r8 <- c(integer(9), W_yr4[8], 1-W_yr4[8], integer(4))
W_yr4_r9 <- c(integer(9), W_yr4[9], 1-W_yr4[9], integer(4))
W_yr4_r10 <- c(integer(11), W_yr4[10], 1-W_yr4[10], integer(2))
W_yr4_r11 <- c(integer(11), W_yr4[11], 1-W_yr4[11], integer(2))
W_yr4_r12 <- c(integer(13), W_yr4[12], 1-W_yr4[12])
W_yr4_r13 <- c(integer(13), W_yr4[13], 1-W_yr4[13])
W_yr4_r14 <- c(integer(13), 1, 0)
W_yr4_r15 <- c(integer(14), 1)

# Asian Matrix Rows:
A_yr4_r1 <- c(0, A_yr4[1], 1-A_yr4[1], integer(12))
A_yr4_r2 <- c(integer(3), A_yr4[2], 1-A_yr4[2], integer(10))
A_yr4_r3 <- c(integer(3), A_yr4[3], 1-A_yr4[3], integer (10))
A_yr4_r4 <- c(integer(5), A_yr4[4], 1-A_yr4[4], integer(8))
A_yr4_r5 <- c(integer(5), A_yr4[5], 1-A_yr4[5], integer(8))
A_yr4_r6 <- c(integer(7), A_yr4[6], 1-A_yr4[6], integer(6))
A_yr4_r7 <- c(integer(7), A_yr4[7], 1-A_yr4[7], integer(6))
A_yr4_r8 <- c(integer(9), A_yr4[8], 1-A_yr4[8], integer(4))
A_yr4_r9 <- c(integer(9), A_yr4[9], 1-A_yr4[9], integer(4))
A_yr4_r10 <- c(integer(11), A_yr4[10], 1-A_yr4[10], integer(2))
A_yr4_r11 <- c(integer(11), A_yr4[11], 1-A_yr4[11], integer(2))
A_yr4_r12 <- c(integer(13), A_yr4[12], 1-A_yr4[12])
A_yr4_r13 <- c(integer(13), A_yr4[13], 1-A_yr4[13])
A_yr4_r14 <- c(integer(13), 1, 0)
A_yr4_r15 <- c(integer(14), 1)

# Hispanic/Latinx Rows:
L_yr4_r1 <- c(0, L_yr4[1], 1-L_yr4[1], integer(12))
L_yr4_r2 <- c(integer(3), L_yr4[2], 1-L_yr4[2], integer(10))
L_yr4_r3 <- c(integer(3), L_yr4[3], 1-L_yr4[3], integer (10))
L_yr4_r4 <- c(integer(5), L_yr4[4], 1-L_yr4[4], integer(8))
L_yr4_r5 <- c(integer(5), L_yr4[5], 1-L_yr4[5], integer(8))
L_yr4_r6 <- c(integer(7), L_yr4[6], 1-L_yr4[6], integer(6))
L_yr4_r7 <- c(integer(7), L_yr4[7], 1-L_yr4[7], integer(6))
L_yr4_r8 <- c(integer(9), L_yr4[8], 1-L_yr4[8], integer(4))
L_yr4_r9 <- c(integer(9), L_yr4[9], 1-L_yr4[9], integer(4))
L_yr4_r10 <- c(integer(11), L_yr4[10], 1-L_yr4[10], integer(2))
L_yr4_r11 <- c(integer(11), L_yr4[11], 1-L_yr4[11], integer(2))
L_yr4_r12 <- c(integer(13), L_yr4[12], 1-L_yr4[12])
L_yr4_r13 <- c(integer(13), L_yr4[13], 1-L_yr4[13])
L_yr4_r14 <- c(integer(13), 1, 0)
L_yr4_r15 <- c(integer(14), 1)

# 2MR Rows:
TMR_yr4_r1 <- c(0, TMR_yr4[1], 1-TMR_yr4[1], integer(12))
TMR_yr4_r2 <- c(integer(3), TMR_yr4[2], 1-TMR_yr4[2], integer(10))
TMR_yr4_r3 <- c(integer(3), TMR_yr4[3], 1-TMR_yr4[3], integer (10))
TMR_yr4_r4 <- c(integer(5), TMR_yr4[4], 1-TMR_yr4[4], integer(8))
TMR_yr4_r5 <- c(integer(5), TMR_yr4[5], 1-TMR_yr4[5], integer(8))
TMR_yr4_r6 <- c(integer(7), TMR_yr4[6], 1-TMR_yr4[6], integer(6))
TMR_yr4_r7 <- c(integer(7), TMR_yr4[7], 1-TMR_yr4[7], integer(6))
TMR_yr4_r8 <- c(integer(9), TMR_yr4[8], 1-TMR_yr4[8], integer(4))
TMR_yr4_r9 <- c(integer(9), TMR_yr4[9], 1-TMR_yr4[9], integer(4))
TMR_yr4_r10 <- c(integer(11), TMR_yr4[10], 1-TMR_yr4[10], integer(2))
TMR_yr4_r11 <- c(integer(11), TMR_yr4[11], 1-TMR_yr4[11], integer(2))
TMR_yr4_r12 <- c(integer(13), TMR_yr4[12], 1-TMR_yr4[12])
TMR_yr4_r13 <- c(integer(13), TMR_yr4[13], 1-TMR_yr4[13])
TMR_yr4_r14 <- c(integer(13), 1, 0)
TMR_yr4_r15 <- c(integer(14), 1)

# Other Rows:
Oth_yr4_r1 <- c(0, Oth_yr4[1], 1-Oth_yr4[1], integer(12))
Oth_yr4_r2 <- c(integer(3), Oth_yr4[2], 1-Oth_yr4[2], integer(10))
Oth_yr4_r3 <- c(integer(3), Oth_yr4[3], 1-Oth_yr4[3], integer (10))
Oth_yr4_r4 <- c(integer(5), Oth_yr4[4], 1-Oth_yr4[4], integer(8))
Oth_yr4_r5 <- c(integer(5), Oth_yr4[5], 1-Oth_yr4[5], integer(8))
Oth_yr4_r6 <- c(integer(7), Oth_yr4[6], 1-Oth_yr4[6], integer(6))
Oth_yr4_r7 <- c(integer(7), Oth_yr4[7], 1-Oth_yr4[7], integer(6))
Oth_yr4_r8 <- c(integer(9), Oth_yr4[8], 1-Oth_yr4[8], integer(4))
Oth_yr4_r9 <- c(integer(9), Oth_yr4[9], 1-Oth_yr4[9], integer(4))
Oth_yr4_r10 <- c(integer(11), Oth_yr4[10], 1-Oth_yr4[10], integer(2))
Oth_yr4_r11 <- c(integer(11), Oth_yr4[11], 1-Oth_yr4[11], integer(2))
Oth_yr4_r12 <- c(integer(13), Oth_yr4[12], 1-Oth_yr4[12])
Oth_yr4_r13 <- c(integer(13), Oth_yr4[13], 1-Oth_yr4[13])
Oth_yr4_r14 <- c(integer(13), 1, 0)
Oth_yr4_r15 <- c(integer(14), 1)

# Transition matrices for Year 4:

# White, Year 4:
mat_w_yr4 <- t(matrix(c(W_yr4_r1, W_yr4_r2, W_yr4_r3, W_yr4_r4,
                        W_yr4_r5, W_yr4_r6, W_yr4_r7, W_yr4_r8,
                        W_yr4_r9, W_yr4_r10, W_yr4_r11, W_yr4_r12,
                        W_yr4_r13, W_yr4_r14, W_yr4_r15), nrow=15))

# Asian, Year 4:
mat_a_yr4 <- t(matrix(c(A_yr4_r1, A_yr4_r2, A_yr4_r3, A_yr4_r4,
                        A_yr4_r5, A_yr4_r6, A_yr4_r7, A_yr4_r8,
                        A_yr4_r9, A_yr4_r10, A_yr4_r11, A_yr4_r12,
                        A_yr4_r13, A_yr4_r14, A_yr4_r15), nrow=15))
#mat_a_yr4

# Hispanic/Latinx, Year 4:
mat_L_yr4 <- t(matrix(c(L_yr4_r1, L_yr4_r2, L_yr4_r3, L_yr4_r4,
                        L_yr4_r5, L_yr4_r6, L_yr4_r7, L_yr4_r8,
                        L_yr4_r9, L_yr4_r10, L_yr4_r11, L_yr4_r12,
                        L_yr4_r13, L_yr4_r14, L_yr4_r15), nrow=15))

# 2MR, Year 4:
mat_TMR_yr4 <- t(matrix(c(TMR_yr4_r1, TMR_yr4_r2, TMR_yr4_r3, TMR_yr4_r4,
                          TMR_yr4_r5, TMR_yr4_r6, TMR_yr4_r7, TMR_yr4_r8,
                          TMR_yr4_r9, TMR_yr4_r10, TMR_yr4_r11, TMR_yr4_r12,
                          TMR_yr4_r13, TMR_yr4_r14, TMR_yr4_r15), nrow=15))

# Other, Year 4:
mat_Oth_yr4 <- t(matrix(c(Oth_yr4_r1, Oth_yr4_r2, Oth_yr4_r3, Oth_yr4_r4,
                          Oth_yr4_r5, Oth_yr4_r6, Oth_yr4_r7, Oth_yr4_r8,
                          Oth_yr4_r9, Oth_yr4_r10, Oth_yr4_r11, Oth_yr4_r12,
                          Oth_yr4_r13, Oth_yr4_r14, Oth_yr4_r15), nrow=15))
#mat_Oth_yr4

# Simulations:

# White:
sim_1_w <- W_i %*% mat_w_yr1 #output of this (sim_1_w) is student #'s @ end of 18-19
sim_1_w

sim_2_w_kb <- get_kb("White yr2 K_B")
sim_2_w <- (sim_1_w + sim_2_w_kb) %*% mat_w_yr2
sim_2_w

sim_3_w_kb <- get_kb("White yr3 K_B")
sim_3_w <- (sim_2_w + sim_3_w_kb) %*% mat_w_yr3
sim_3_w

sim_4_w_kb <- get_kb("White yr4 K_B")
sim_4_w <- (sim_3_w + sim_4_w_kb) %*% mat_w_yr4
sim_4_w

# Add predictions below (start with 21-22 to 22-23 transition, i.e. yr5)

# Asian:
sim_1_a <- A_i %*% mat_a_yr1 #output of this (sim_1_a) is student #'s @ end of 18-19
sim_1_a

sim_2_a_kb <- get_kb("Asian yr2 K_B")
sim_2_a <- (sim_1_a + sim_2_a_kb) %*% mat_a_yr2
sim_2_a

sim_3_a_kb <- get_kb("Asian yr3 K_B")
sim_3_a <- (sim_2_a + sim_3_a_kb) %*% mat_a_yr3
sim_3_a

sim_4_a_kb <- get_kb("Asian yr4 K_B")
sim_4_a <- (sim_3_a + sim_4_a_kb) %*% mat_a_yr4
sim_4_a

# Add predictions below (start with 21-22 to 22-23 transition, i.e. yr5)

# Latinx:
sim_1_L <- L_i %*% mat_L_yr1 #output of this (sim_1_L) is student #'s @ end of 18-19
sim_1_L

sim_2_L_kb <- get_kb("Latinx yr2 K_B")
sim_2_L <- (sim_1_L + sim_2_L_kb) %*% mat_L_yr2
sim_2_L

sim_3_L_kb <- get_kb("Latinx yr3 K_B")
sim_3_L <- (sim_2_L + sim_3_L_kb) %*% mat_L_yr3
sim_3_L

sim_4_L_kb <- get_kb("Latinx yr4 K_B")
sim_4_L <- (sim_3_L + sim_4_L_kb) %*% mat_L_yr4
sim_4_L

# Add predictions below (start with 21-22 to 22-23 transition, i.e. yr5)

# TMR:
sim_TMR <- TMR_i %*% mat_TMR_yr1 #output of this (sim_1_TMR) is student #'s @ end of 18-19
sim_1_TMR

sim_2_TMR_kb <- get_kb("2MR yr2 K_B")
sim_2_TMR <- (sim_1_TMR + sim_2_TMR_kb) %*% mat_TMR_yr2
sim_2_TMR

sim_3_TMR_kb <- get_kb("2MR yr3 K_B")
sim_3_TMR <- (sim_2_TMR + sim_3_TMR_kb) %*% mat_TMR_yr3
sim_3_TMR

sim_4_TMR_kb <- get_kb("2MR yr4 K_B")
sim_4_TMR <- (sim_3_TMR + sim_4_TMR_kb) %*% mat_TMR_yr4
sim_4_TMR

# Add predictions below (start with 21-22 to 22-23 transition, i.e. yr5)

# Other:
sim_Oth <- Oth_i %*% mat_Oth_yr1 #output of this (sim_1_Oth) is student #'s @ end of 18-19
sim_1_Oth

sim_2_Oth_kb <- get_kb("Other yr2 K_B")
sim_2_Oth <- (sim_1_Oth + sim_2_Oth_kb) %*% mat_Oth_yr2
sim_2_Oth

sim_3_Oth_kb <- get_kb("Other yr3 K_B")
sim_3_Oth <- (sim_2_Oth + sim_3_Oth_kb) %*% mat_Oth_yr3
sim_3_Oth

sim_4_Oth_kb <- get_kb("Other yr4 K_B")
sim_4_Oth <- (sim_3_Oth + sim_4_Oth_kb) %*% mat_Oth_yr4
sim_4_Oth

# Add predictions below (start with 21-22 to 22-23 transition, i.e. yr5)