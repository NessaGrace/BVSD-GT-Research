library(ggplot2)
gtDif <- c()

for(count in 1:5) {
  gtDif<- c(gtDif, percentUnderOver(repGtSPost[[count]], TRUE, 1.2) - percentUnderOver(repGtSPost[[count]], TRUE, 0.8) - (percentUnderOver(repGtSPre[[count]], TRUE, 1.2) - percentUnderOver(repGtSPre[[count]], TRUE, 0.8)))
}


susDif <- c()

for(count in 1:5) {
  susDif<- c(susDif, percentUnderOver(repSusSPost[[count]], TRUE, 1.2) - percentUnderOver(repSusSPost[[count]], TRUE, 0.8) - (percentUnderOver(repSusSPre[[count]], TRUE, 1.2) - percentUnderOver(repSusSPre[[count]], TRUE, 0.8)))
}


advDif <- c()

for(count in 1:5) {
  advDif<- c(advDif, percentUnderOver(repAdvSPost[[count]], TRUE, 1.2) - percentUnderOver(repAdvSPost[[count]], TRUE, 0.8) - (percentUnderOver(repAdvSPre[[count]], TRUE, 1.2) - percentUnderOver(repAdvSPre[[count]], TRUE, 0.8)))
}

gtDifDisp <- 100*gtDif
#to make it show up on graph
gtDifDisp[3] <- gtDifDisp[3]+0.1
gtChange <- data.frame(Races = c("Asian", "Black", "Latino", "Two or More", "White"), gtDif = gtDifDisp)
testRamp<-colorRampPalette(c("#003300", "#330000"))(2)
ggplot(gtChange, aes(Races, gtDif, fill = testRamp[0.5*gtDif/abs(gtDif)+1.5])) + geom_col(position = "identity") + 
  ylab(str_wrap("Change in Percent of Schools with Equitable Gifted and Talented Programs (%)", 40)) + 
  xlab(element_blank()) +
  theme_bw(base_size = 29) +
  theme(text = element_text(size = 25), legend.position = "none")

susChange <- data.frame(Races = c("Asian", "Black", "Latino", "Two or More", "White"), susDif = 100*susDif)

ggplot(susChange, aes(Races, susDif, fill = testRamp[0.5*susDif/abs(susDif)+1.5])) + geom_col(position = "identity") + 
  ylab(str_wrap("Change in Percent of Schools with Equitable Suspension Rates (%)", 40)) + 
  xlab(element_blank()) +
  theme_bw(base_size = 29) +
  theme(text = element_text(size = 25), legend.position = "none")

advDifDisp <- 100*advDif
advDifDisp[2] <- advDifDisp[2]+0.1
advChange <- data.frame(Races = c("Asian", "Black", "Latino", "Two or More", "White"), advDif = advDifDisp)

ggplot(advChange, aes(Races, advDif, fill = testRamp[0.5*advDif/abs(advDif)+1.5])) + geom_col(position = "identity") + 
  ylab(str_wrap("Change in Percent of Equitable Schools for Advanced Classes (%)", 40)) + 
  xlab(element_blank()) +
  theme_bw(base_size = 29) +
  theme(text = element_text(size = 25), legend.position = "none")

