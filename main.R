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

#we want to have a time series of this bayesian prediction. 
#But for now, we are taking only the newest surveys
wtd.polls <- survey_N %>% 
  filter(dmy(ReleaseDate) > dmy("01-01-2019"))
wtd.polls <- data.frame(wtd.polls[,c(6:11)]/100, wtd.polls[,5])

wtd.polls[nrow(wtd.polls)+1,] <- c(colMeans(wtd.polls[,1:6], na.rm=TRUE), sum(wtd.polls[,7], na.rm=TRUE))
names(wtd.polls)[ncol(wtd.polls)] <- "N"
### -------- The following table is now adjusted to our problem
#               Bolsonaro Haddad  Ciro   Others  Swing   Wasting  N
#Datafolha      0.360     0.220   0.130  0.200   0.0400  0.0500   19552
#Ibope          0.360     0.220   0.110  0.180   0.0500  0.0800   3010
#MDA            0.367     0.240   0.099  0.155   0.0600  0.0780   2002
#Ipespe         0.360     0.220   0.110  0.290   0.0200  0.0000   2000
#VoxPopuli      0.340     0.270   0.110  0.130   0.0700  0.0800   2000
#ParanaPesq     0.349     0.218   0.094  0.171   0.0460  0.1200   1080
#7              0.359     0.225   0.122  0.195   0.0433  0.0561   29644
#8              0.376     0.235   0.128  0.205   0.0000  0.0561   29644
#9              0.398     0.249   0.135  0.217   0.0000  0.0000   27980


###########Draw 1 million samples

library(SciencesPo)
library(MCMCpack)

############################# draw samples from the posterior
set.seed(1234)
MC <- 10000

### Using uninformative prior (1,1,1,1)

prob2win = function(row, export=1){
  p=rdirichlet(10000,
               wtd.polls$N[row] *
                 c(wtd.polls$PP[row] + wtd.polls$Cs[row]+ wtd.polls$Vox[row] , 
                   wtd.polls$PSOE[row]  + wtd.polls$UP[row] + wtd.polls$Others.Blank[row], 
                   1 - wtd.polls$PP[row] - wtd.polls$Cs[row] - wtd.polls$Vox[row] - wtd.polls$PSOE[row] - wtd.polls$UP[row] - wtd.polls$Others.Blank[row])
               +1)
  if(export==1){
    mean(p[,1]<p[,2]) ## No exceeds Yes?
  } else {
    return(p)
  }
}

#####################look at the margins of Bolsonaro over the combined opposition candidates.
row= nrow(wtd.polls)
samples = prob2win(row= row, export=0)

combinedOpposition <- (samples[,2])
frontRunner <- (samples[,1])

margin <- (combinedOpposition - frontRunner)

quantile(margin, probs = c(0.025, 0.5, 0.975))

hist(margin, 
     col="gray",
     prob = FALSE, # posterior distribution
     breaks = "FD", xlab = expression(p[FarRight] > p[Opposition]),
     main = expression(paste(bold("Posterior Distribution of Elections With "),  p[FarRight] > p[Opposition])));
# Bayes estimate (middle 95%)
abline(v=mean(margin), col='red', lwd=3, lty=3)
