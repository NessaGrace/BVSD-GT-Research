# A Mathematical Model for Underrepresentation in Gifted and Talented Programs in Boulder Valley School District

## Project Description
In this project, we use absorbing Markov chains to model racial and ethnic 
underrepresentation in the Boulder Valley School District gifted and talented
program. We create five Markov chains for the 5 racial/ethnic groups under study: White,
Asian, Latinx, Two or More Races, and Other, which includes Black, American Indian / 
Alaskan Native, and Native Hawaiian / Pacific Islander students (whose numbers
were too small to be analyzed individually). The R program markov_chains.R reads in a CSV data file and 
generates 5 initial state vectors for each racial/ethnic group and
4 transition matrices for each of the five racial/ethnic groups for
a total of 20 transition matrices. Note that transition matrices are
populated with transition probabilities. The R program generates
5 Markov chains for each race/ethnicity, updating the transition 
matrices each year (which is why there are four of them for each group
for the 4 years being studied). The initial state vector initializes
each Markov chain, and multiplying the vectors by the transition matrix
for the 2018-19 year on the right gives the number of students in each
state at the end of the 2018-19 year, which can then be primed with new
kindergartners and multiplied by the transition matrix for the 2019-20 year
to continue the chain. Note that each vector is of size 1 x 15, and each matrix
is of size 15 x 15. The Markov chain can be used from here to make predictions for future
years and identify key trends in GT student populations in BVSD. These
features will be added to the code in a future release, and only the training set
based on existing data is included in the current release.

The current state of markov_chains.R also visualizes trends from the data training
set.

A detailed project description and writeup can be found in Rotation 1 writeup.pdf,
including a description of the model and all transition states and probabilities.

## Dependencies
To run the script simulations.R, you must install the Tidyverse package on
your machine. You will also need to call the libraries dyplyr, readxl, ggplot2,
and reshape2 in the script. For example, you will need to have a line that says
library("dyplyr"). These are currently present in the script.

## Important Considerations
- Years are defined as academic years. Year 1 is 2018-19, Year 2 is 2019-20,
  Year 3 is 2020-21, and Year 4 is 2021-22.
- The initial state vector is the distribution of students at the
  end of the 2017-2018 academic year. It is the number of students in each of
  the 15 transition states. There are five initial state vectors, one per
  racial/ethnic group.
- Students pass through the chain to the absorbing states where they collect. New students
  are fed into the chain using the K_B vectors to represent the number of kindergartners
  entering the district every year. K_B is always 0 at the end of the year, as students
  have been sorted into GT or non-GT by the end of the year. K_B's for year 1 (18-19) are
  included in the initial state vectors. These are the kindergartners entering the district
  in the 18-19 year and were in pre-K in the 17-18 year. Note that absorbing states will continue
  to grow as time goes on and more students enter middle school (the absorbing state).
- If there is something not addressed in this README, it is likely addressed in Rotation 1 writeup.pdf.

## Data File Description
- Data file name: Initial_state_vector_&_transition_probability_parameters.csv
- The XLSX file contains the same data but is easier to work with.
- The first five groups in the file are the initial state vector entries.
  The remaining groups in the file are transition probabilities until
  row 337, where K_B (number of incoming kindergartners for each race
  and year) begin. Year 1's K_B values are included in the initial state
  vectors; while the rest of these vectors are from 2017-18 data, the K_B
  values are from the kindergartners who entered in the 2018-19 year.
- Yr_1 is 2018-19, Yr_2 is 2019-20, Yr_3 is 2020-21, Yr_4 is 2021-22.
- W is White
- A is Asian/Asian American
- L is Latinx/Hispanic
- 2MR is two or more races (in markov_chains.R, this tends to appear as 
  TMR due to variable naming rules in R)
- Oth is other (Black/African American, Native American/Native Alaskan,
  Native Hawaiian/Pacific Islander)
- GT refers to students receiving GT services, NGT refers to students who are not.
- The transition probabilities are the probabilities that a student either retains GT
  services in the current academic year given that they received GT services in the previous
  grade (beta parameters) or begins to receive GT services in the current academic year given
  that they did not receive GT services in the previous year (epsilon parameters). Other 
  possibilities can be included by computing 1 - beta or 1 - epsilon (see Rotation 1 writeup.pdf).

## How to Run the Code
To run the program markov_chains.R in RStudio, click within the code and
select all (Ctrl-A). Then Ctrl-Enter.

## Output
When the code is run, the blue text in the Console is simply the contents
of the script. If the user adds print statements, they will appear under
the blue script line corresponding to the print command. Currently, there
are no print statements added and all output is graphical. With the current
script, 12 graphs should be outputted. Broadly, these graphs will show racial/ethnic
trends for the class starting in the 2018-19 academic year over the four years,
the demographic composition of the district GT population over the four years, and
the representation index for each racial/ethnic group over the four years.

## Some Future Directions
- Validate the output (include rounding error considerations)
- Add more modularity to the main script (break up the code into more functions)
- Make the code more user-friendly by using a configuration file and/or command
  line arguments.
- Add error handling to the code (also makes it more user friendly)
- Test edge cases and overall functionality of the code with unit and functional
  tests.
- Add continuous integration using GitHub Actions.
- Improve variable names, documentation, or anything that makes the code more
  readable.
- The above (apart from the first one perhaps) all pertain to improving the 
  code from a software engineering perspective. More future directions directly
  related to the research can be found in Rotation 1 writeup.pdf.
