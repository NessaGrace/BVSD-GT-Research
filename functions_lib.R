#' get_column() function:
#'     DESCRIPTION:
#'     if the values in one column are equal to a particular
#'     value, return the associated values (values in the same
#'     row) from the target column; used to read in the data
#'     from the CSV file
#'     
#'     PARAMETERS:
#'     * tibble_name:
#'         * data type: tibble data frame
#'         * Name of the tibble user is trying to get the data from
#'         
#'     * query_column_name:
#'         * data type: string
#'         * Name of the column where the values must meet a certain condition
#'    
#'     * query_column_value:
#'         * data type: string in this case but does not have to be
#'         * Value the user is looking for in column with query_column_name
#'         
#'     * target_column_name:
#'         * data type: string
#'         * Name of the column the user wants to return data from
#'      
#'     RETURNS:
#'     * t(output_vector):
#'         * data type: vector
#'         * Transposed vector containing the values from target_column_name

get_column <- function(tibble_name,
                       query_column_name,
                       query_column_value,
                       target_column_name){
  output_vector <- tibble_name %>%
                    filter({{query_column_name}} == query_column_value) %>%
                    select({{target_column_name}})
  return (as.numeric(t(output_vector))) 
}

#' get_kb() function:
#'     DESCRIPTION:
#'     for a given input string in the Variable_Type column of the 
#'     CSV file, return the values in the Variable_Value column
#'     that are associated with the string; used to get the K_B
#'     values to prime the vectors between years
#'     
#'     PARAMETERS:
#'     * var_type_string:
#'         * data type: string in this case but does not have to be
#'         * Type of variable from the spreadsheet from the Variable_Type column
#'      
#'     RETURNS:
#'     * kb:
#'         * data type: vector of integers
#'         * Number of students of a given race entering grade K in a given yr

get_kb <- function(var_type_string){
  kb <- get_column(data, Variable_Type, var_type_string, Variable_Value)
  kb <- c(kb, integer(26))
  return(kb)
}

#' make_matrix() function:
#'     DESCRIPTION:
#'     takes in a data vector (resulting from the get_column()
#'     function) and builds transition matrix
#'     
#'     PARAMETERS:
#'     * data_vector_name:
#'         * data type: vector
#'         * Vector of transition probabilities from CSV file for a given racial
#'           group and year
#'      
#'     RETURNS:
#'     * matrix:
#'         * data type: matrix
#'         * Transition matrix for a given racial/ethnic group and year

make_matrix <- function(data_vector_name, row_col_names){
  # make the rows of the matrix (first 25 rows from CSV, last 2 rows from 
  # absorbing states)
  row1 <- c(0, data_vector_name[1], 1-data_vector_name[1], integer(24))
  row2 <- c(integer(3), data_vector_name[2], 1-data_vector_name[2], integer(22))
  row3 <- c(integer(3), data_vector_name[3], 1-data_vector_name[3], integer (22))
  row4 <- c(integer(5), data_vector_name[4], 1-data_vector_name[4], integer(20))
  row5 <- c(integer(5), data_vector_name[5], 1-data_vector_name[5], integer(20))
  row6 <- c(integer(7), data_vector_name[6], 1-data_vector_name[6], integer(18))
  row7 <- c(integer(7), data_vector_name[7], 1-data_vector_name[7], integer(18))
  row8 <- c(integer(9), data_vector_name[8], 1-data_vector_name[8], integer(16))
  row9 <- c(integer(9), data_vector_name[9], 1-data_vector_name[9], integer(16))
  row10 <- c(integer(11), data_vector_name[10], 1-data_vector_name[10], integer(14))
  row11 <- c(integer(11), data_vector_name[11], 1-data_vector_name[11], integer(14))
  row12 <- c(integer(13), data_vector_name[12], 1-data_vector_name[12], integer(12))
  row13 <- c(integer(13), data_vector_name[13], 1-data_vector_name[13], integer(12))
  row14 <- c(integer(15), data_vector_name[14], 1-data_vector_name[14], integer(10))
  row15 <- c(integer(15), data_vector_name[15], 1-data_vector_name[15], integer(10))
  row16 <- c(integer(17), data_vector_name[16], 1-data_vector_name[16], integer(8))
  row17 <- c(integer(17), data_vector_name[17], 1-data_vector_name[17], integer(8))
  row18 <- c(integer(19), data_vector_name[18], 1-data_vector_name[18], integer(6))
  row19 <- c(integer(19), data_vector_name[19], 1-data_vector_name[19], integer(6))
  row20 <- c(integer(21), data_vector_name[20], 1-data_vector_name[20], integer(4))
  row21 <- c(integer(21), data_vector_name[21], 1-data_vector_name[21], integer(4))
  row22 <- c(integer(23), data_vector_name[22], 1-data_vector_name[22], integer(2))
  row23 <- c(integer(23), data_vector_name[23], 1-data_vector_name[23], integer(2))
  row24 <- c(integer(25), data_vector_name[24], 1-data_vector_name[24])
  row25 <- c(integer(25), data_vector_name[25], 1-data_vector_name[25])
  row26 <- c(integer(25), 1, 0)
  row27 <- c(integer(26), 1)
  
  # correct for cases where students neither stay nor leave GT, because there
  # are no students in GT the previous year (staying already is accounted for
  # in calculating beta values in accompanying spreadsheet) - note that all 
  # students leaving GT between years has not been observed and is not accounted
  # for here as a result; also, this issue does not arise for joining/not being
  # selected for GT
  if (data_vector_name[2] == 0) {
    row2[5] <- 0
  }
  if (data_vector_name[4] == 0) {
    row4[7] <- 0
  }
  if (data_vector_name[6] == 0) {
    row6[9] <- 0
  }
  if (data_vector_name[8] == 0) {
    row8[11] <- 0
  }
  if (data_vector_name[10] == 0) {
    row10[13] <- 0
  }
  if (data_vector_name[12] == 0) {
    row12[15] <- 0
  }
  if (data_vector_name[14] == 0) {
    row14[17] <- 0
  }
  if (data_vector_name[16] == 0) {
    row16[19] <- 0
  }
  if (data_vector_name[18] == 0) {
    row18[21] <- 0
  }
  if (data_vector_name[20] == 0) {
    row20[23] <- 0
  }
  if (data_vector_name[22] == 0) {
    row22[25] <- 0
  }
  if (data_vector_name[24] == 0) {
    row24[27] <- 0
  }
  
  # create the matrix from the rows
  matrix = t(matrix(c(row1,row2,row3,row4,row5,row6,row7,row8,row9,row10,row11,
                      row12,row13,row14,row15,row16,row17,row18,row19,row20,
                      row21,row22,row23,row24,row25,row26,row27), nrow=27))
  
  # assign row and column names
  rownames(matrix) <- row_col_names
  colnames(matrix) <- row_col_names
  
  return(matrix)
}

#' run_sims() function:
#'     DESCRIPTION: Runs simulations based on vector of initial state values and
#'     transition probabilities. Computes RI during year 6.
#'     
#'     
#'     PARAMETERS:
#'     * initial_values
#'         * data type: list
#'         * vector of initial number of students in each state
#'     * group_values
#'         * data type: list
#'         * vector of transition matrices for years 1-6 and all 5 racial groups
#'      
#'     RETURNS:
#'     * RI
#'         * data type: list
#'         * list of all RI values for each racial group at the end of year 6
run_sims = function(initial_values, group_values){

# rename modified matrix to generic names to run simulation
names_for_sim = c("mat_w_yr1", "mat_w_yr2", "mat_w_yr3", "mat_w_yr4",
                  "mat_w_yr5", "mat_w_yr6", "mat_a_yr1", "mat_a_yr2",
                  "mat_a_yr3", "mat_a_yr4","mat_a_yr5", "mat_a_yr6",
                  "mat_L_yr1", "mat_L_yr2", "mat_L_yr3","mat_L_yr4",
                  "mat_L_yr5", "mat_L_yr6", "mat_TMR_yr1","mat_TMR_yr2",
                  "mat_TMR_yr3","mat_TMR_yr4", "mat_TMR_yr5","mat_TMR_yr6",
                  "mat_Oth_yr1","mat_Oth_yr2","mat_Oth_yr3","mat_Oth_yr4",
                  "mat_Oth_yr5","mat_Oth_yr6")
names(group_values) = names_for_sim
  
# White 
# Year 1
ts_1_w <- round(W_i %*% group_values$mat_w_yr1)  

# Year 2
ts_2_w_kb <- get_kb("White yr2 K_B")
ts_2_w <- round((ts_1_w + ts_2_w_kb) %*% group_values$mat_w_yr2)

# Year 3
ts_3_w_kb <- get_kb("White yr3 K_B")
ts_3_w <- round((ts_2_w + ts_3_w_kb) %*% group_values$mat_w_yr3)

# Year 4
ts_4_w_kb <- get_kb("White yr4 K_B")
ts_4_w <- round((ts_3_w + ts_4_w_kb) %*% group_values$mat_w_yr4)

# Year 5
ts_5_w_kb <- get_kb("White yr5 K_B")
ts_5_w <- round((ts_4_w + ts_5_w_kb) %*% group_values$mat_w_yr5)

# Year 6
ts_6_w_kb <- get_kb("White yr6 K_B")
ts_6_w <- round((ts_5_w + ts_6_w_kb) %*% group_values$mat_w_yr6)

# Asian:
# Year 1
ts_1_a <- round(A_i %*% group_values$mat_a_yr1)

# Year 2
ts_2_a_kb <- get_kb("Asian yr2 K_B")
ts_2_a <- round((ts_1_a + ts_2_a_kb) %*% group_values$mat_a_yr2)

# Year 3
ts_3_a_kb <- get_kb("Asian yr3 K_B")
ts_3_a <- round((ts_2_a + ts_3_a_kb) %*% group_values$mat_a_yr3)

# Year 4
ts_4_a_kb <- get_kb("Asian yr4 K_B")
ts_4_a <- round((ts_3_a + ts_4_a_kb) %*% group_values$mat_a_yr4)

# Year 5
ts_5_a_kb <- get_kb("Asian yr5 K_B")
ts_5_a <- round((ts_4_a + ts_5_a_kb) %*% group_values$mat_a_yr5)

# Year 6
ts_6_a_kb <- get_kb("Asian yr6 K_B")
ts_6_a <- round((ts_5_a + ts_6_a_kb) %*% group_values$mat_a_yr6)

# Latinx:
# Year 1
ts_1_L <- round(L_i %*% group_values$mat_L_yr1)

# Year 2
ts_2_L_kb <- get_kb("Latinx yr2 K_B")
ts_2_L <- round((ts_1_L + ts_2_L_kb) %*% group_values$mat_L_yr2)

# Year 3
ts_3_L_kb <- get_kb("Latinx yr3 K_B")
ts_3_L <- round((ts_2_L + ts_3_L_kb) %*% group_values$mat_L_yr3)

# Year 4
ts_4_L_kb <- get_kb("Latinx yr4 K_B")
ts_4_L <- round((ts_3_L + ts_4_L_kb) %*% group_values$mat_L_yr4)

# Year 5
ts_5_L_kb <- get_kb("Latinx yr5 K_B")
ts_5_L <- round((ts_4_L + ts_5_L_kb) %*% group_values$mat_L_yr5)

# Year 6
ts_6_L_kb <- get_kb("Latinx yr6 K_B")
ts_6_L <- round((ts_5_L + ts_6_L_kb) %*% group_values$mat_L_yr6)

# TMR:
# Year 1
ts_1_TMR <- round(TMR_i %*% group_values$mat_TMR_yr1)

# Year 2
ts_2_TMR_kb <- get_kb("TMR yr2 K_B")
ts_2_TMR <- round((ts_1_TMR + ts_2_TMR_kb) %*% group_values$mat_TMR_yr2)

# Year 3
ts_3_TMR_kb <- get_kb("TMR yr3 K_B")
ts_3_TMR <- round((ts_2_TMR + ts_3_TMR_kb) %*% group_values$mat_TMR_yr3)

# Year 4
ts_4_TMR_kb <- get_kb("TMR yr4 K_B")
ts_4_TMR <- round((ts_3_TMR + ts_4_TMR_kb) %*% group_values$mat_TMR_yr4)

# Year 5
ts_5_TMR_kb <- get_kb("TMR yr5 K_B")
ts_5_TMR <- round((ts_4_TMR + ts_5_TMR_kb) %*% group_values$mat_TMR_yr5)

# Year 6
ts_6_TMR_kb <- get_kb("TMR yr6 K_B")
ts_6_TMR <- round((ts_5_TMR + ts_6_TMR_kb) %*% group_values$mat_TMR_yr6)

# Other:
# Year 1
ts_1_Oth <- round(Oth_i %*% group_values$mat_Oth_yr1)

# Year 2
ts_2_Oth_kb <- get_kb("Other yr2 K_B")
ts_2_Oth <- round((ts_1_Oth + ts_2_Oth_kb) %*% group_values$mat_Oth_yr2)

# Year 3
ts_3_Oth_kb <- get_kb("Other yr3 K_B")
ts_3_Oth <- round((ts_2_Oth + ts_3_Oth_kb) %*% group_values$mat_Oth_yr3)

# Year 4
ts_4_Oth_kb <- get_kb("Other yr4 K_B")
ts_4_Oth <- round((ts_3_Oth + ts_4_Oth_kb) %*% group_values$mat_Oth_yr4)

# Year 5
ts_5_Oth_kb <- get_kb("Other yr5 K_B")
ts_5_Oth <- round((ts_4_Oth + ts_5_Oth_kb) %*% group_values$mat_Oth_yr5)

# Year 6
ts_6_Oth_kb <- get_kb("Other yr6 K_B")
ts_6_Oth <- round((ts_5_Oth + ts_6_Oth_kb) %*% group_values$mat_Oth_yr6)

# Compute RIs for year 6:

# Find number of students in GT and NGT year 12 (since these are absorbing
# states)
ts_6_w[26] = ts_6_w[26]-ts_5_w[26]
ts_6_w[27] = ts_6_w[27]-ts_5_w[27]

ts_6_a[26] = ts_6_a[26]-ts_5_a[26]
ts_6_a[27] = ts_6_a[27]-ts_5_a[27]

ts_6_L[26] = ts_6_L[26]-ts_5_L[26]
ts_6_L[27] = ts_6_L[27]-ts_5_L[27]

ts_6_TMR[26] = ts_6_TMR[26]-ts_5_TMR[26]
ts_6_TMR[27] = ts_6_TMR[27]-ts_5_TMR[27]

ts_6_Oth[26] = ts_6_Oth[26]-ts_5_Oth[26]
ts_6_Oth[27] = ts_6_Oth[27]-ts_5_Oth[27]

# Add up all students in year 6:
combined_yr6 = cbind(ts_6_w, ts_6_a, ts_6_L, ts_6_TMR, ts_6_Oth)
total_yr6 = sum(combined_yr6)

# Add up all GT students year 6:
gt_categories = c("K_GT","1_GT","2_GT","3_GT","4_GT","5_GT","6_GT","7_GT",
                  "8_GT","9_GT","10_GT","11_GT","12_GT")
gt_total_yr6 = combined_yr6[, grep(paste(gt_categories, collapse="|"),
                                   colnames(combined_yr6))]
gt_total_yr6_sum = sum(gt_total_yr6)

# White % in total pop:
w_total_pop_6 = sum(ts_6_w)
w_percent_total_pop_6 = (w_total_pop_6/total_yr6)*100

# White % in GT:
w_gt_6 = ts_6_w[,gt_categories]
w_gt_6_sum = sum(w_gt_6)
w_percent_gt_6 = (w_gt_6_sum/gt_total_yr6_sum)*100

# White RI:
w_RI = w_percent_gt_6/w_percent_total_pop_6

# Asian % in total pop:
a_total_pop_6 = sum(ts_6_a)
a_percent_total_pop_6 = (a_total_pop_6/total_yr6)*100

# Asian % in GT:
a_gt_6 = ts_6_a[,gt_categories]
a_gt_6_sum = sum(a_gt_6)
a_percent_gt_6 = (a_gt_6_sum/gt_total_yr6_sum)*100

# Asian RI:
a_RI = a_percent_gt_6/a_percent_total_pop_6

# Latinx % in total pop:
L_total_pop_6 = sum(ts_6_L)
L_percent_total_pop_6 = (L_total_pop_6/total_yr6)*100

# Latinx % in GT:
L_gt_6 = ts_6_L[,gt_categories]
L_gt_6_sum = sum(L_gt_6)
L_percent_gt_6 = (L_gt_6_sum/gt_total_yr6_sum)*100

# Latinx RI:
L_RI = L_percent_gt_6/L_percent_total_pop_6

# TMR % in total pop:
TMR_total_pop_6 = sum(ts_6_TMR)
TMR_percent_total_pop_6 = (TMR_total_pop_6/total_yr6)*100

# TMR % in GT:
TMR_gt_6 = ts_6_TMR[,gt_categories]
TMR_gt_6_sum = sum(TMR_gt_6)
TMR_percent_gt_6 = (TMR_gt_6_sum/gt_total_yr6_sum)*100

# TMR RI:
TMR_RI = TMR_percent_gt_6/TMR_percent_total_pop_6

# Other % in total pop:
Oth_total_pop_6 = sum(ts_6_Oth)
Oth_percent_total_pop_6 = (Oth_total_pop_6/total_yr6)*100

# Other % in GT:
Oth_gt_6 = ts_6_Oth[,gt_categories]
Oth_gt_6_sum = sum(Oth_gt_6)
Oth_percent_gt_6 = (Oth_gt_6_sum/gt_total_yr6_sum)*100

# Other RI:
Oth_RI = Oth_percent_gt_6/Oth_percent_total_pop_6

# put all RI output in list
RI = list(WhiteRI=w_RI, AsianRI=a_RI, LatinxRI=L_RI, TMRRI=TMR_RI, OthRI=Oth_RI)
return(RI)
}

#' trans_prob_extractor() function:
#'     DESCRIPTION: get transition probabilities for grade K to grade 1 through
#'     grade 2 to grade 3 transitions.
#'     
#'     PARAMETERS:
#'     * matrix_group
#'         * data type: list 
#'         * list of matrices for all years for a given race
#'      
#'     RETURNS:
#'     * mat_lists
#'         * data type: list 
#'         * list of transition probabilities over all years

trans_prob_extractor = function(matrix_group){
  
  K_1_mat_list = numeric(length(matrix_group))
  one_two_mat_list = numeric(length(matrix_group))
  two_three_mat_list = numeric(length(matrix_group))
  
  for (i in 1:length(matrix_group)) {
    mat <- matrix_group[[i]]
    
    prob_K_1 = mat["K_NGT","1_GT"]
    prob_1_2 = mat["1_NGT","2_GT"]
    prob_2_3 = mat["2_NGT","3_GT"]
    
    K_1_mat_list[i] = prob_K_1
    one_two_mat_list[i] = prob_1_2
    two_three_mat_list[i] = prob_2_3
    
  }
  
  mat_lists = list(K_1_mat_list,
                   one_two_mat_list,
                   two_three_mat_list)
  
  return(mat_lists) 
}

#' average_trans_prob() function:
#'     DESCRIPTION: calculate average of transitions from K to 2 and of transition
#'                  from grades 2 to 3.
#'     
#'     
#'     PARAMETERS:
#'     * trans_prob_list
#'         * data type: list
#'         * list of transition probabilities over all years
#'      
#'     RETURNS:
#'     * averages
#'         * data type: list
#'         * list of average transition probabilities between grades K to 2 and
#'           grades 2 and 3
         
average_trans_prob = function(trans_prob_list){
  
  avg_K_2 = mean(trans_prob_list[[1:2]])
  avg_2_3 = mean(trans_prob_list[[3]])
  
  averages = list(avg_K_2,avg_2_3)
  
  return(averages)
}