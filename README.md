# An analysis and modeling of some Boulder Valley School District metrics for racial equity

## Project Description
In this project, we analyze data for four equity metrics in Boulder Valley School District using
data from their [public Tableau dashboard](https://public.tableau.com/app/profile/boulder.valley.school.district/viz/StrategicPlanMetrics/Home). These metrics include suspension rates, gifted and
talented program selection, advanced classes, and upper-level math. Data analysis is provided to
analyze trends in these metrics as well as analyze the impact of a report by the Latinx Parent
Advisory Committee in 2020, which provided recommendations for improving representation within
the district. We zoom in on the gifted and talented metric and develop  absorbing Markov chains
to model underrepresentation in the gifted and talented program. We create five Markov chains for 
White, Asian, Latinx, Two or More Races, and Other, which includes Black, American Indian / 
Alaskan Native, and Native Hawaiian / Pacific Islander students (whose numbers
were too small to be analyzed individually). The R program markov_chains.R reads in data from an 
XLSX file, generates transition matrices, iterates through the Markov chain model for 6 years
between 2018-19 and 2023-24, and simulates the effect on representation index (the fraction of 
students in gifted and talented of each race compared to the fraction of each race in the overall
student body) of different changes to the transition probability of GT selection between grades.

## Installation and setup instructions
Run the following installations in your console (as applicable) prior to running "markov_chains.R":
install.packages("tidyverse")
install.packages("readxl")
install.packages("reshape2")
install.packages("xtable")
install.packages("knitr")

Ensure that the data file "Initial_state_vector_&_transition_probability_parameters.xlsx" and
the script "functions_lib.R" are in your working directory prior to running "markov_chains.R".
