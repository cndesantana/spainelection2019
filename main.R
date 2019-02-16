##############################################
# Script to work with political surveys
# Collaboration OneMind and DataSCOUT
# Antonio Canepa Oneto
# Charles Novaes de Santana
# --------------------------------------------
# 2019/02/15
##############################################

library(readxl)
library(dplyr)
library(lubridate)
library(ggplot2)
library(reshape)

survey <- read.csv("./surveys_spain_2019.csv")

#convert survey to LONG format, to work with tidyverse
survey_long <- melt(survey, 
                    id.vars = c("Media", "Source", "ReleaseDate","FieldDate","Size"),
                    measure.vars = c("PP", "PSOE", "UP", "Cs", "Vox", "Others.Blank"))
names(survey_long)[6] <- "Party"

#plot time series of surveys
p1 <- survey_long %>%
  ggplot(aes(x = dmy(ReleaseDate), y = as.numeric(value), col = Party), size=3) +
  geom_point(stat="identity") +
  scale_color_manual(values = c("PP" = "cyan", 
                                "PSOE" = "red", 
                                "UP" = "purple",
                                "Cs" = "orange",
                                "Vox" = "lightgreen",
                                "Others.Blank" = "gray"))+
  geom_smooth(se=FALSE)+
  xlab("Date") + ylab("Voters(%)") + ggtitle("Spain elections 2019") 

png("tiemseries1.png",width=3200,height=1800,res=300)
print(p1)
dev.off()

#plot time series of surveys along last 5 months
p2 <- survey_long %>%
  filter(dmy(ReleaseDate) > dmy("01-10-2018"),
  dmy(ReleaseDate) < dmy("15-02-2019")) %>%
  ggplot(aes(x = dmy(ReleaseDate), y = as.numeric(value), col = Party), size=3) +
  geom_point(stat="identity") +
  scale_color_manual(values = c("PP" = "cyan", 
                                "PSOE" = "red", 
                                "UP" = "purple",
                                "Cs" = "orange",
                                "Vox" = "lightgreen",
                                "Others.Blank" = "gray"))+
  geom_smooth(se=FALSE)+
  xlab("Date") + ylab("Voters(%)") + ggtitle("Spain elections 2019") 
png("tiemseries2.png",width=3200,height=1800,res=300)
print(p2)
dev.off()

######
###### To run the Bayes model
###### Inspired by http://danielmarcelino.github.io/2018/will-brazil-goes-to-a-instant-runoff-election.html
######
######

## the survey needs to present the N
survey_N <- survey %>% 
  filter(Size > 0)

options(digits=3)

### -------- From now on, the code needs to be adjusted to our problem
#####################Adjusting for the wasting votes
wtd.polls[9,] <- data.frame(wtd.polls[8,1:4] +
                              wtd.polls[8,1:4] / 
                              sum(wtd.polls[8,1:4]) * 
                              wtd.polls[8,6], 
                            Swing=0, 
                            Wasting=0, 
                            N=(wtd.polls[8,7] - (wtd.polls[8,6] * wtd.polls[8,7])))

print(wtd.polls)

wtd.polls$N[9] *
  c(wtd.polls$Bolsonaro[9], 
    (wtd.polls$Haddad[9] + wtd.polls$Ciro[9] + wtd.polls$Others[9]), 
    1 - wtd.polls$Bolsonaro[9] - (wtd.polls$Haddad[9] - wtd.polls$Ciro[9] - wtd.polls$Others[9]))+1


###########Draw 1 million samples
poll <- c(4910, 3011, 1756, 2718)

library(SciencesPo)
library(MCMCpack)

############################# draw samples from the posterior
set.seed(1234)
MC <- 1000000

### Using uninformative prior (1,1,1,1)
#samples <- getDirichletSamples(MC, alpha = poll + rep(1,4))  

row= 9
prob2win = function(row, export=1){
  p=rdirichlet(100000,
               wtd.polls$N[row] *
                 c(wtd.polls$Bolsonaro[row], wtd.polls$Haddad[row] + wtd.polls$Ciro[row] + wtd.polls$Others[row], 1 - wtd.polls$Bolsonaro[row] - wtd.polls$Haddad[row] - wtd.polls$Ciro[row] - wtd.polls$Others[row])+1)
  if(export==1){
    mean(p[,1]<p[,2]) ## No exceeds Yes?
  } else {
    return(p)
  }
}


#####################look at the margins of Bolsonaro over the combined opposition candidates.
samples = prob2win(row= 9, export=0)

combinedOpposition <- (samples[,2])
frontRunner <- (samples[,1])

margin <- (combinedOpposition - frontRunner)

quantile(margin, probs = c(0.025, 0.5, 0.975))

hist(margin, 
     col="gray",
     prob = FALSE, # posterior distribution
     breaks = "FD", xlab = expression(p[Bolsonaro] > p[Opposition]),
     main = expression(paste(bold("Posterior Distribution of Elections With "),  p[Bolsonaro] > p[Opposition])));
# Bayes estimate (middle 95%)
abline(v=mean(margin), col='red', lwd=3, lty=3);

