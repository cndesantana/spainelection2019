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

setwd("/home/charles/GitRepos/armagedon/spainelection2019")
survey <- read.csv("./surveys_spain_2019.csv", sep=";")

#convert survey to LONG format, to work with tidyverse
survey_long <- melt(survey, 
                    id.vars = c("Media", "Source", "ReleaseDate","FieldDate","Size"),
                    measure.vars = c("PP", "PSOE", "UP", "Cs", "Vox", "Others.Blank"))
names(survey_long)[6] <- "Party"

#plot time series of surveys
p1 <- survey_long %>%
  ggplot(aes(x = dmy(ReleaseDate), y = as.numeric(value), col = Party), size=3) +
  geom_point(stat="identity") +
  scale_color_manual(values = c("PSOE" = "red", 
                                "PP" = "deepskyblue", 
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
  scale_color_manual(values = c("PP" = "deepskyblue", 
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

################
facebookdir <- "/home/charles/GitRepos/armagedon/spainelection2019/FacebookScrap"
setwd(facebookdir)
xlsfiles <- system("ls posts_*.xlsx", intern=TRUE)
df <- data.frame()
for(i in 1:length(xlsfiles)){
  file <- read_xlsx(xlsfiles[i])
  party <- unlist(strsplit(unlist(strsplit(xlsfiles[1],"_"))[2],"[.]"))[1]
  file$Party <- rep(party, nrow(file))
  if(i == 1){
    df <- file
  }else{
    df <- rbind(df,file)
  }
}

df <- df %>%
  mutate(dia = ymd_hms(created_time) %>% format("%d"))%>%
  mutate(mes = ymd_hms(created_time) %>% format("%m"))%>%
  mutate(ano = ymd_hms(created_time) %>% format("%Y"))%>%
  mutate(dia_mes_ano = dmy(paste(dia,mes,ano,sep="-"))) %>%
  select(dia_mes_ano, likes_count, type, comments_count, shares_count, love_count, haha_count, wow_count, sad_count, angry_count, Party)
##### Filter by Party
filter_survey <- survey_long %>% 
  filter(Party != "Others.Blank") %>%
  filter(dmy(ReleaseDate) > dmy("01-09-2018")) %>%
  mutate(date = dmy(ReleaseDate)) %>%
  mutate(dia = dmy(ReleaseDate) %>% format("%d"))%>%
  mutate(mes = dmy(ReleaseDate) %>% format("%m"))%>%
  mutate(ano = dmy(ReleaseDate) %>% format("%Y"))%>%
  mutate(dia_mes_ano = dmy(paste(dia,mes,ano,sep="-")))%>%
  filter(!is.na(value)) %>%
  mutate(value = as.numeric(value)) %>%
  filter(!is.na(value))
 
filter_survey <- filter_survey %>%
  select(dia_mes_ano, Party, value, Size)

##### Join survey and facebook
join_survey_face <- left_join(df, 
                              filter_survey, 
                              by = c("dia_mes_ano"))
join_survey_face <- join_survey_face %>% 
  filter(!is.na(value))

#### linear regression model
mylm <- glm(value ~ 
              likes_count + 
              type + 
              comments_count + 
              shares_count + 
              love_count +
              haha_count + 
              wow_count + 
              sad_count +
              angry_count, 
           data = join_survey_face,
           family = Gamma)

##### We have to do the following:
###3 to test if any of the variables in join_survey_face can explain the variability of the variable "value"
### to use linear regression or other regression functions
