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
  # this uses dyplyr verbs from the tidyverse package (i.e. filter, select)
  output_vector <- tibble_name %>%
                    filter({{query_column_name}} == query_column_value) %>%
                    select({{target_column_name}})
  # t() takes the transpose of a vector
  return (t(output_vector)) 
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
  # integer(14) just gives 14 zeros, since vector size needs to be of length 15
  # for the vector addition in the markov_chains.R script
  kb <- c(kb, integer(14))
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

make_matrix <- function(data_vector_name){
  # make the rows of the matrix (first 13 rows from CSV, last 2 rows from 
  # absorbing states)
  row1 <- c(0, data_vector_name[1], 1-data_vector_name[1], integer(12))
  row2 <- c(integer(3), data_vector_name[2], 1-data_vector_name[2], integer(10))
  row3 <- c(integer(3), data_vector_name[3], 1-data_vector_name[3], integer (10))
  row4 <- c(integer(5), data_vector_name[4], 1-data_vector_name[4], integer(8))
  row5 <- c(integer(5), data_vector_name[5], 1-data_vector_name[5], integer(8))
  row6 <- c(integer(7), data_vector_name[6], 1-data_vector_name[6], integer(6))
  row7 <- c(integer(7), data_vector_name[7], 1-data_vector_name[7], integer(6))
  row8 <- c(integer(9), data_vector_name[8], 1-data_vector_name[8], integer(4))
  row9 <- c(integer(9), data_vector_name[9], 1-data_vector_name[9], integer(4))
  row10 <- c(integer(11), data_vector_name[10], 1-data_vector_name[10], integer(2))
  row11 <- c(integer(11), data_vector_name[11], 1-data_vector_name[11], integer(2))
  row12 <- c(integer(13), data_vector_name[12], 1-data_vector_name[12])
  row13 <- c(integer(13), data_vector_name[13], 1-data_vector_name[13])
  row14 <- c(integer(13), 1, 0)
  row15 <- c(integer(14), 1)
  
  # create the matrix from the rows
  matrix = t(matrix(c(row1, row2, row3, row4,
             row5, row6, row7, row8,
             row9, row10, row11, row12,
             row13, row14, row15), nrow=15))
  
  return(matrix)
}