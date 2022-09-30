get_column <- function(tibble_name,
                       query_column_name,
                       query_column_value,
                       target_column_name){
  output_vector <- tibble_name %>%
                    filter({{query_column_name}} == query_column_value) %>%
                    select({{target_column_name}})
  return (t(output_vector))
}


# make_matrix_rows <- function(){
#   
# }
# 
# W_yr1_r1 <- c(0, W_yr1[1], 1-W_yr1[1], integer(12))
# W_yr1_r2 <- c(integer(3), W_yr1[2], 1-W_yr1[2], integer(10))
# W_yr1_r3 <- c(integer(3), W_yr1[3], 1-W_yr1[3], integer (10))
# W_yr1_r4 <- c(integer(5), W_yr1[4], 1-W_yr1[4], integer(8))
# W_yr1_r5 <- c(integer(5), W_yr1[5], 1-W_yr1[5], integer(8))
# W_yr1_r6 <- c(integer(7), W_yr1[6], 1-W_yr1[6], integer(6))
# W_yr1_r7 <- c(integer(7), W_yr1[7], 1-W_yr1[7], integer(6))
# W_yr1_r8 <- c(integer(9), W_yr1[8], 1-W_yr1[8], integer(4))
# W_yr1_r9 <- c(integer(9), W_yr1[9], 1-W_yr1[9], integer(4))
# W_yr1_r10 <- c(integer(11), W_yr1[10], 1-W_yr1[10], integer(2))
# W_yr1_r11 <- c(integer(11), W_yr1[11], 1-W_yr1[11], integer(2))
# W_yr1_r12 <- c(integer(13), W_yr1[12], 1-W_yr1[12])
# W_yr1_r13 <- c(integer(13), W_yr1[13], 1-W_yr1[13])
# W_yr1_r14 <- c(integer(13), 1, 0)  
# W_yr1_r15 <- c(integer(14), 1)