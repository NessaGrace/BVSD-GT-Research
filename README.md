# BVSD-GT-Research

*Need to add best practices*
## Project Description
In this project, we use absorbing Markov chains to model racial and ethnic 
underrepresentation in the Boulder Valley School District gifted and talented
program. Included in the project so far are a data file called 'Initial state
vector and transition probability parameters.csv' and a program written in R
called 'transition_matrices_R.' The data file contains variable names for the
number of students in each of the 15 transition states at the end of the 2017-
2018 academic year, which comprise the initial state vector at the beginning 
of the observation period. It also contains the transition probabilities for
the academic years 2018-19 to 2021-22, which are the probabilities that a 
student either retains GT services in the current academic year given that 
they received GT services in the previous grade or begins to receive GT
services in the current academic year given that they did not receive GT
services in the previous year. The R program reads the data file and 
generates 5 initial state vectors for each racial/ethnic group and
4 transition matrices for each of the five racial/ethnic groups for
a total of 20 transition matrices. Note that transition matrices are
populated with transition probabilities. The R program generates
5 Markov chains for each race/ethnicity, updating the transition 
matrices each year (which is why there are four of them for each group
for the 4 years being studied). The initial state vector initializes
each Markov chain, and multiplying the vectors by the transition matrix
for the 2018-19 year on the right gives the number of students in each
state at the end of the 2018-19 year, which can then be multipled by 
the transition matrix for the 2019-20 year to continue the chain. Note
that each vector is of size 1 x 15, and each matrix is of size 15 x 15.
The Markov chain can be used from here to make predictions for future
years and identify key trends in GT student populations in BVSD. These
features will be added to the code in a future release.

## Data File Description
- The first five groups in the file are the initial state vector entries.
  The remaining groups in the file are transition probabilities.
- Yr_1 is 2018-19, Yr_2 is 2019-20, Yr_3 is 2020-21, Yr_4 is 2021-22.
- W is White, 
- A is Asian/Asian American
- L is Latinx/Hispanic
- 2MR is two or more races
- Oth is other (Black/African American, Native American/Native Alaskan,
  Native Hawaiian/Pacific Islander)
- Need to add more details like complete variable name descriptions later 


