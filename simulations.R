#' This script stores transition probabilities and uses them to construct
#' transition matrices. We also feed in an initial state vector.

#TODO:

#MAIN PRIORITY:#
# -add visualizations - RI plots, transition rate plots, predictions, Covid effect if time

#OTHER PRIORITIES (!! means most important):
# -add header name vector
# -clean up /improve code including random comments, doc best practices
# -improve variable names? esp TMR vs 2MR
# -make sure output makes sense!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# -double check data file!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# -check matrices again for neg #'s, 0's, NA's!!!!!!!!!!!!!!!!!!!!
# -config 
# -see other lists!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# -check for errors due to rounding!!!!!!!!!!!!!!!!!!!!!!!
# -see why calculations for number of students between years do not match up!!!!!!!!!!!!!!!

# Keep in case needed for header vector
#W_i_1 <- c(K_B_W_i, K_GT_W_i, K_NGT_W_i, Gr_1_GT_W_i, Gr_1_NGT_W_i,
#         Gr_2_GT_W_i, Gr_2_NGT_W_i, Gr_3_GT_W_i, Gr_3_NGT_W_i, Gr_4_GT_W_i,
#         Gr_4_NGT_W_i, Gr_5_GT_W_i, Gr_5_NGT_W_i, MS_GT_W_i, MS_NGT_W_i)

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
library("readxl")
library("ggplot2")
library("reshape2")

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

#### Visualization: ####

# can track 1 class from K-3rd grade over 4 years, may be interesting to start from different points or see
# how long it takes for a group to get students in GT (later ID more likely
# for minority groups), NOTE: uses class entering in 2018-19, should put in 1 graph

year <- c(1, 2, 3, 4)
total_gt_pop_1_class_yr1 <- sim_1_w[2] + sim_1_a[2] + sim_1_L[2] + sim_1_TMR[2] + sim_1_Oth[2]
                            
total_gt_pop_1_class_yr2 <- sim_2_w[4] + sim_2_a[4] + sim_2_L[4] + sim_2_TMR[4] + sim_2_Oth[4]
                            
total_gt_pop_1_class_yr3 <- sim_3_w[6] + sim_3_a[6] + sim_3_L[6] + sim_3_TMR[6] + sim_3_Oth[6]
                                                                                          
total_gt_pop_1_class_yr4 <- sim_4_w[8] + sim_4_a[8] + sim_4_L[8] + sim_4_TMR[8] + sim_4_Oth[8] 
  
# all y-axes will be fractions!!

#WHITE#:

white_gt_pop_1_class <- c(sim_1_w[2]/total_gt_pop_1_class_yr1, 
                          sim_2_w[4]/total_gt_pop_1_class_yr2,
                          sim_3_w[6]/total_gt_pop_1_class_yr3,
                          sim_4_w[8]/total_gt_pop_1_class_yr4) 

df_w_one_class <- data.frame(year, white_gt_pop_1_class)

ggplot(df_w_one_class, aes(year, white_gt_pop_1_class)) +
  geom_point()

#ASIAN#:

asian_gt_pop_1_class <- c(sim_1_a[2]/total_gt_pop_1_class_yr1, 
                          sim_2_a[4]/total_gt_pop_1_class_yr2,
                          sim_3_a[6]/total_gt_pop_1_class_yr3,
                          sim_4_a[8]/total_gt_pop_1_class_yr4) 

df_a_one_class <- data.frame(year, asian_gt_pop_1_class)

ggplot(df_a_one_class, aes(year, asian_gt_pop_1_class)) +
  geom_point()

#HISPANIC/LATINX#:

latinx_gt_pop_1_class <- c(sim_1_L[2]/total_gt_pop_1_class_yr1, 
                           sim_2_L[4]/total_gt_pop_1_class_yr2,
                           sim_3_L[6]/total_gt_pop_1_class_yr3,
                           sim_4_L[8]/total_gt_pop_1_class_yr4) 

df_L_one_class <- data.frame(year, latinx_gt_pop_1_class)

ggplot(df_L_one_class, aes(year, latinx_gt_pop_1_class)) +
  geom_point()

#2MR#:

TMR_gt_pop_1_class <- c(sim_1_TMR[2]/total_gt_pop_1_class_yr1, 
                        sim_2_TMR[4]/total_gt_pop_1_class_yr2,
                        sim_3_TMR[6]/total_gt_pop_1_class_yr3,
                        sim_4_TMR[8]/total_gt_pop_1_class_yr4) 

df_TMR_one_class <- data.frame(year, TMR_gt_pop_1_class)

ggplot(df_TMR_one_class, aes(year, TMR_gt_pop_1_class)) +
  geom_point()

#Other#:

other_gt_pop_1_class <- c(sim_1_Oth[2]/total_gt_pop_1_class_yr1, 
                          sim_2_Oth[4]/total_gt_pop_1_class_yr2,
                          sim_3_Oth[6]/total_gt_pop_1_class_yr3,
                          sim_4_Oth[8]/total_gt_pop_1_class_yr4) 

df_other_one_class <- data.frame(year, other_gt_pop_1_class)

ggplot(df_other_one_class, aes(year, other_gt_pop_1_class)) +
  geom_point() +
  labs(title="2018-19 Class, Grades K-3",
       x="Year", y="% of students in GT")

# can track school district demographics per yr by adding up GT #'s for that yr for each
# grade (e.g. add all GT #'s for White yr 1, for Asian yr 1, etc)

#YEAR 1#:
overall_gt_population_yr1 <- sim_1_w[2] + sim_1_w[4] + sim_1_w[6] + sim_1_w[8] +
                             sim_1_w[10] + sim_1_w[12] + sim_1_w[14] + 
                             sim_1_a[2] + sim_1_a[4] + sim_1_a[6] + sim_1_a[8] +
                             sim_1_a[10] + sim_1_a[12] + sim_1_a[14] + 
                             sim_1_L[2] + sim_1_L[4] + sim_1_L[6] + sim_1_L[8] +
                             sim_1_L[10] + sim_1_L[12] + sim_1_L[14] + 
                             sim_1_TMR[2] + sim_1_TMR[4] + sim_1_TMR[6] + sim_1_TMR[8] +
                             sim_1_TMR[10] + sim_1_TMR[12] + sim_1_TMR[14] + 
                             sim_1_Oth[2] + sim_1_Oth[4] + sim_1_Oth[6] + sim_1_Oth[8] +
                             sim_1_Oth[10] + sim_1_Oth[12] + sim_1_Oth[14] 

overall_gt_population_yr2 <- sim_2_w[2] + sim_2_w[4] + sim_2_w[6] + sim_2_w[8] +
                             sim_2_w[10] + sim_2_w[12] + sim_2_w[14] + 
                             sim_2_a[2] + sim_2_a[4] + sim_2_a[6] + sim_2_a[8] +
                             sim_2_a[10] + sim_2_a[12] + sim_2_a[14] + 
                             sim_2_L[2] + sim_2_L[4] + sim_2_L[6] + sim_2_L[8] +
                             sim_2_L[10] + sim_2_L[12] + sim_2_L[14] + 
                             sim_2_TMR[2] + sim_2_TMR[4] + sim_2_TMR[6] + sim_2_TMR[8] +
                             sim_2_TMR[10] + sim_2_TMR[12] + sim_2_TMR[14] + 
                             sim_2_Oth[2] + sim_2_Oth[4] + sim_2_Oth[6] + sim_2_Oth[8] +
                             sim_2_Oth[10] + sim_2_Oth[12] + sim_2_Oth[14] 

overall_gt_population_yr3 <- sim_3_w[2] + sim_3_w[4] + sim_3_w[6] + sim_3_w[8] +
                             sim_3_w[10] + sim_3_w[12] + sim_3_w[14] + 
                             sim_3_a[2] + sim_3_a[4] + sim_3_a[6] + sim_3_a[8] +
                             sim_3_a[10] + sim_3_a[12] + sim_3_a[14] + 
                             sim_3_L[2] + sim_3_L[4] + sim_3_L[6] + sim_3_L[8] +
                             sim_3_L[10] + sim_3_L[12] + sim_3_L[14] + 
                             sim_3_TMR[2] + sim_3_TMR[4] + sim_3_TMR[6] + sim_3_TMR[8] +
                             sim_3_TMR[10] + sim_3_TMR[12] + sim_3_TMR[14] + 
                             sim_3_Oth[2] + sim_3_Oth[4] + sim_3_Oth[6] + sim_3_Oth[8] +
                             sim_3_Oth[10] + sim_3_Oth[12] + sim_3_Oth[14] 

overall_gt_population_yr4 <- sim_4_w[2] + sim_4_w[4] + sim_4_w[6] + sim_4_w[8] +
                             sim_4_w[10] + sim_4_w[12] + sim_4_w[14] + 
                             sim_4_a[2] + sim_4_a[4] + sim_4_a[6] + sim_4_a[8] +
                             sim_4_a[10] + sim_4_a[12] + sim_4_a[14] + 
                             sim_4_L[2] + sim_4_L[4] + sim_4_L[6] + sim_4_L[8] +
                             sim_4_L[10] + sim_4_L[12] + sim_4_L[14] + 
                             sim_4_TMR[2] + sim_4_TMR[4] + sim_4_TMR[6] + sim_4_TMR[8] +
                             sim_4_TMR[10] + sim_4_TMR[12] + sim_4_TMR[14] + 
                             sim_4_Oth[2] + sim_4_Oth[4] + sim_4_Oth[6] + sim_4_Oth[8] +
                             sim_4_Oth[10] + sim_4_Oth[12] + sim_4_Oth[14]

overall_white_yr1 <- (sim_1_w[2] + sim_1_w[4] + sim_1_w[6] + sim_1_w[8] +
                     sim_1_w[10] + sim_1_w[12] + sim_1_w[14])/overall_gt_population_yr1

overall_asian_yr1 <- (sim_1_a[2] + sim_1_a[4] + sim_1_a[6] + sim_1_a[8] +
                     sim_1_a[10] + sim_1_a[12] + sim_1_a[14])/overall_gt_population_yr1

overall_latinx_yr1 <- (sim_1_L[2] + sim_1_L[4] + sim_1_L[6] + sim_1_L[8] +
                      sim_1_L[10] + sim_1_L[12] + sim_1_L[14])/overall_gt_population_yr1

overall_TMR_yr1 <- (sim_1_TMR[2] + sim_1_TMR[4] + sim_1_TMR[6] + sim_1_TMR[8] +
                     sim_1_TMR[10] + sim_1_TMR[12] + sim_1_TMR[14])/overall_gt_population_yr1

overall_other_yr1 <- (sim_1_Oth[2] + sim_1_Oth[4] + sim_1_Oth[6] + sim_1_Oth[8] +
                     sim_1_Oth[10] + sim_1_Oth[12] + sim_1_Oth[14])/overall_gt_population_yr1

#######################################################

overall_white_yr2 <- (sim_2_w[2] + sim_2_w[4] + sim_2_w[6] + sim_2_w[8] +
  sim_2_w[10] + sim_2_w[12] + sim_2_w[14])/overall_gt_population_yr2

overall_asian_yr2 <- (sim_2_a[2] + sim_2_a[4] + sim_2_a[6] + sim_2_a[8] +
  sim_2_a[10] + sim_2_a[12] + sim_2_a[14])/overall_gt_population_yr2

overall_latinx_yr2 <- (sim_2_L[2] + sim_2_L[4] + sim_2_L[6] + sim_2_L[8] +
  sim_2_L[10] + sim_2_L[12] + sim_2_L[14])/overall_gt_population_yr2

overall_TMR_yr2 <- (sim_2_TMR[2] + sim_2_TMR[4] + sim_2_TMR[6] + sim_2_TMR[8] +
  sim_2_TMR[10] + sim_2_TMR[12] + sim_2_TMR[14])/overall_gt_population_yr2

overall_other_yr2 <- (sim_2_Oth[2] + sim_2_Oth[4] + sim_2_Oth[6] + sim_2_Oth[8] +
  sim_2_Oth[10] + sim_2_Oth[12] + sim_2_Oth[14])/overall_gt_population_yr2

############################################

overall_white_yr3 <- (sim_3_w[2] + sim_3_w[4] + sim_3_w[6] + sim_3_w[8] +
  sim_3_w[10] + sim_3_w[12] + sim_3_w[14])/overall_gt_population_yr3

overall_asian_yr3 <- (sim_3_a[2] + sim_3_a[4] + sim_3_a[6] + sim_3_a[8] +
  sim_3_a[10] + sim_3_a[12] + sim_3_a[14])/overall_gt_population_yr3

overall_latinx_yr3 <- (sim_3_L[2] + sim_3_L[4] + sim_3_L[6] + sim_3_L[8] +
  sim_3_L[10] + sim_3_L[12] + sim_3_L[14])/overall_gt_population_yr3

overall_TMR_yr3 <- (sim_3_TMR[2] + sim_3_TMR[4] + sim_3_TMR[6] + sim_3_TMR[8] +
  sim_3_TMR[10] + sim_3_TMR[12] + sim_3_TMR[14])/overall_gt_population_yr3

overall_other_yr3 <- (sim_3_Oth[2] + sim_3_Oth[4] + sim_3_Oth[6] + sim_3_Oth[8] +
  sim_3_Oth[10] + sim_3_Oth[12] + sim_3_Oth[14])/overall_gt_population_yr3

##############################################

overall_white_yr4 <- (sim_4_w[2] + sim_4_w[4] + sim_4_w[6] + sim_4_w[8] +
  sim_4_w[10] + sim_4_w[12] + sim_4_w[14])/overall_gt_population_yr4

overall_asian_yr4 <- (sim_4_a[2] + sim_4_a[4] + sim_4_a[6] + sim_4_a[8] +
  sim_4_a[10] + sim_4_a[12] + sim_4_a[14])/overall_gt_population_yr4

overall_latinx_yr4 <- (sim_4_L[2] + sim_4_L[4] + sim_4_L[6] + sim_4_L[8] +
  sim_4_L[10] + sim_4_L[12] + sim_4_L[14])/overall_gt_population_yr4

overall_TMR_yr4 <- (sim_4_TMR[2] + sim_4_TMR[4] + sim_4_TMR[6] + sim_4_TMR[8] +
  sim_4_TMR[10] + sim_4_TMR[12] + sim_4_TMR[14])/overall_gt_population_yr4

overall_other_yr4 <- (sim_4_Oth[2] + sim_4_Oth[4] + sim_4_Oth[6] + sim_4_Oth[8] +
  sim_4_Oth[10] + sim_4_Oth[12] + sim_4_Oth[14])/overall_gt_population_yr4

############################################

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

df_overall <- data.frame(year, 
                         overall_white_all_years,
                         overall_asian_all_years,
                         overall_latinx_all_years,
                         overall_TMR_all_years,
                         overall_other_all_years)

races_for_legend = c("White", "Asian", "Latinx", "Two or more", "Other")

df_tall <- melt(df_overall ,  id.vars = 'year', variable.name = 'Race')

ggplot(df_tall, aes(year, value)) +
  geom_point(aes(color = Race)) +
  labs(title="Percentage of GT Population by Race 2017-18 to 2021-22",
       x="Year", y="% of students in GT") +
  scale_fill_discrete(name = 'Race', labels=races_for_legend)

#################################################

# Representation index (RI) plots: (note: previous fractions only considered GT pop)
# RI = % gifted / % general

# Year 1 RI's:
RI_w_yr1 <- (overall_white_yr1*100) / 67.5 # denominators come from BVSD Metrics data online
                                           # for all grades in 2017-18
RI_a_yr1 <- (overall_asian_yr1*100) / 5.7

RI_L_yr1 <- (overall_latinx_yr1*100) / 19.6

RI_TMR_yr1 <- (overall_TMR_yr1*100) / 5.8

RI_Oth_yr1 <- (overall_other_yr1*100) / 1.4

RI_yr1 <- c(RI_w_yr1,
            RI_a_yr1,
            RI_L_yr1,
            RI_TMR_yr1,
            RI_Oth_yr1)

df_RI_yr1 <- data.frame(Race=c("White", "Asian", "Latinx", "Two or more", "Other"), RI = RI_yr1)

ggplot(df_RI_yr1, aes(x=Race, y=RI)) +
  geom_col() +
  ggtitle("Representation Index (RI) in BVSD, 2018-19")

# Year 2 RI's:
RI_w_yr2 <- (overall_white_yr2*100) / 67.3

RI_a_yr2 <- (overall_asian_yr2*100) / 5.8

RI_L_yr2 <- (overall_latinx_yr2*100) / 19.5

RI_TMR_yr2 <- (overall_TMR_yr2*100) / 6.0

RI_Oth_yr2 <- (overall_other_yr2*100) / 1.3

RI_yr2 <- c(RI_w_yr2,
            RI_a_yr2,
            RI_L_yr2,
            RI_TMR_yr2,
            RI_Oth_yr2)

df_RI_yr2 <- data.frame(Race=c("White", "Asian", "Latinx", "Two or more", "Other"), RI = RI_yr2)

ggplot(df_RI_yr2, aes(x=Race, y=RI)) +
  geom_col() +
  ggtitle("Representation Index (RI) in BVSD, 2019-20")

# Year 3 RI's:
RI_w_yr3 <- (overall_white_yr3*100) / 67.0

RI_a_yr3 <- (overall_asian_yr3*100) / 5.7

RI_L_yr3 <- (overall_latinx_yr3*100) / 19.6

RI_TMR_yr3 <- (overall_TMR_yr3*100) / 6.3

RI_Oth_yr3 <- (overall_other_yr3*100) / 1.4

RI_yr3 <- c(RI_w_yr3,
            RI_a_yr3,
            RI_L_yr3,
            RI_TMR_yr3,
            RI_Oth_yr3)

df_RI_yr3 <- data.frame(Race=c("White", "Asian", "Latinx", "Two or more", "Other"), RI = RI_yr3)

ggplot(df_RI_yr3, aes(x=Race, y=RI)) +
  geom_col() +
  ggtitle("Representation Index (RI) in BVSD, 2020-21")

# Year 4 RI's:
RI_w_yr4 <- (overall_white_yr4*100) / 66.3

RI_a_yr4 <- (overall_asian_yr4*100) / 5.7

RI_L_yr4 <- (overall_latinx_yr4*100) / 20.1

RI_TMR_yr4 <- (overall_TMR_yr4*100) / 6.5

RI_Oth_yr4 <- (overall_other_yr4*100) / 1.4

RI_yr4 <- c(RI_w_yr4,
            RI_a_yr4,
            RI_L_yr4,
            RI_TMR_yr4,
            RI_Oth_yr4)

df_RI_yr4 <- data.frame(Race=c("White", "Asian", "Latinx", "Two or more", "Other"), RI = RI_yr4)

ggplot(df_RI_yr4, aes(x=Race, y=RI)) +
  geom_col() +
  ggtitle("Representation Index (RI) in BVSD, 2021-22")

# add RI tracking over time??