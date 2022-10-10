get_column <- function(tibble_name,
                       query_column_name,
                       query_column_value,
                       target_column_name){
  output_vector <- tibble_name %>%
                    filter({{query_column_name}} == query_column_value) %>%
                    select({{target_column_name}})
  return (t(output_vector))
}

get_kb <- function(var_type_string){
  kb <- get_column(data, Variable_Type, var_type_string, Variable_Value)
  kb <- c(kb, integer(14))
  return(kb)
}

# first 13 rows from csv, last 2 rows from absorbing states
make_matrix <- function(data_vector_name){
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
  
  matrix = t(matrix(c(row1, row2, row3, row4,
             row5, row6, row7, row8,
             row9, row10, row11, row12,
             row13, row14, row15), nrow=15))
  
  return(matrix)
}