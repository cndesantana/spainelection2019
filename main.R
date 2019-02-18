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

setwd("/home/charles/collaborations/OneMind/spainelection2019")
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
plotBayesianDistribution <- function(mypolls, MC, sufix){
  mypolls[nrow(mypolls)+1,] <- c(colMeans(mypolls[,1:6], na.rm=TRUE), sum(mypolls[,7], na.rm=TRUE))
  names(mypolls)[ncol(mypolls)] <- "N"
  ############################# draw samples from the posterior
  set.seed(1234)
  row= nrow(mypolls)
  samples = prob2win(mypolls, row= row, export=0, MC)
  #####################look at the margins of Far-right wing over the combined opposition candidates.
  combinedOpposition <- (samples[,2])
  frontRunner <- (samples[,1])
  margin <- (combinedOpposition - frontRunner)
  quantile(margin, probs = c(0.025, 0.5, 0.975))
  png(paste0("odds_farrightwingmajority_",sufix,".png"),width=3200,height=1800,res=300)
  hist(margin, 
       col="gray",
       prob = FALSE, # posterior distribution
       breaks = "FD", xlab = expression(p[FarRight] > p[Opposition]),
       main = expression(paste(bold("Posterior Distribution of Elections With "),  p[FarRight] > p[Opposition])));
  # Bayes estimate (middle 95%)
  abline(v=mean(margin), col='red', lwd=3, lty=3)
  dev.off()
}

### Using uninformative prior (1,1,1,1)
prob2win = function(polls, row, export=1, MC){
  p=rdirichlet(MC,
               polls$N[row] *
                 c(polls$PP[row] + polls$Cs[row]+ polls$Vox[row] , 
                   polls$PSOE[row]  + polls$UP[row] + polls$Others.Blank[row])+1)
  if(export==1){
    mean(p[,1]<p[,2]) ## No exceeds Yes?
  } else {
    return(p)
  }
}

## the survey needs to present the N
survey_N <- survey %>% 
  filter(Size > 0)

options(digits=5)

#we want to have a time series of this bayesian prediction. 
#But for now, we are taking only the newest surveys with data for all political parties
wtd.polls <- survey_N %>% 
  filter(dmy(ReleaseDate) > dmy("12-12-2018")) %>%
  dplyr::select(Media, ReleaseDate, PP, PSOE, UP, Cs, Vox, Others.Blank, Size)

### -------- The following table is now adjusted to our problem
# PP    PSOE  UP    Cs    Vox    Others.Blank  N
# 0.231 0.237 0.192 0.158 0.089  0.093         1100
# 0.179 0.254 0.139 0.170 0.117  0.141         1017
# 0.240 0.242 0.166 0.187 0.094  0.071         1000
# 0.230 0.265 0.116 0.171 0.098  0.120         1800
# 0.182 0.251 0.140 0.179 0.115  0.133         1042
# 0.238 0.241 0.161 0.196 0.081  0.083         1100
# 0.183 0.224 0.171 0.185 0.125  0.112         1800
# 0.192 0.226 0.158 0.188 0.129  0.107         1000
# 0.209 0.242 0.155 0.179 0.106  0.107         9859

###########Draw 1 million samples

library(SciencesPo)
library(MCMCpack)

wtd.polls$Media <- toupper(as.character(wtd.polls$Media))
allmedia <- unique(wtd.polls$Media)

for(m in allmedia){
  mypolls <- wtd.polls[which(wtd.polls$Media == m),-c(1,2)]
  MC <- 100000
  plotBayesianDistribution(mypolls, MC, m)
}
mypolls <- wtd.polls[,-c(1,2)]
MC <- 100000
plotBayesianDistribution(mypolls, MC, "general")
