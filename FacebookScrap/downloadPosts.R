library(Rfacebook)
library(lubridate)
library(tidyverse)
library(xlsx)
library(readxl)
library(gdata)
library(dplyr)
library(stringr)
library(stringi)

#workdir <- "/home/cdesantana/DataSCOUT/Objectiva/NucleoPolitico/Modelo"
workdir <- "/home/charles/GitRepos/armagedon/spainelection2019/FacebookScrap"
#workdir <- "/home/charles/GitRepos/armagedon/NucleoPolitico/Modelo/Espanha"
setwd(workdir)
data_inicio <- "2018-09-01"
data_fim <- "2019-04-09"

getFBID <- function(fburl){
   return(unlist(strsplit(httr::POST(url='https://findmyfbid.com',body=list(url = fburl), encode="json")$headers$`amp-redirect-to`,'/'))[5])
}

fb_oauth <- "EAARVKUum8icBAK5iioLNIAtQIE1Wc8AfYtK01CGYujOsSM0RZCrZC7K29SZBpWj1ZCbSjizsi3Ye7NwNbFCYEe1puFLTZA0gZCy117KKT2bCeYdPWOB3wwe4WmZBKjPGj64R79K4m7DRArjguizvmV8uZCYgjdPjsrCVYLXX0xyZAcvPRlpglilsbYAZBgJfmZA7pVzCoiS4OaOPAZDZD"
#fb_oauth <- "EAARVKUum8icBAPg0uSCp4DRQHrH3dSQuBSQ7GlyEKaH9jQqmUBoj7f2T3Bt7y3v4bVPumjZB5pFadQZCTdoSs1l2iNqw9w6mM6eI60MnB2ZBSIHiHqlsLCU6jWCUe8ym2BU7BrqQRHYP1PR1raaT2McGKS6dZClHnGw9diHCbSbx1Hm2G7T3hbrzk7ZAWIY2spIHxbocyWgZDZD"
#fb_oauth <- "EAARVKUum8icBAGxtHsyDZCPdqgXYknhWIGZCrLs02qZCwzKZBfZAr6rKGpSk1H2arbZCmaz3kA6DZCQffjCz3ugZBjuJknkbiTWBXjGZCREPDGNUmVy6DwIoCyuxOLkmA2BPZAfVZAalYrieZCcQsfE3EKDfB8Xv7kFBzyIjDT8PbZB3RhRY18rysUg7vDXEdcw5O4yd38zbz9U1tWgZDZD"
inputfile <- read.csv("./Candidatos.csv",sep=";")
for(i in 1:length(inputfile$Nome)){
    nome <- as.character(inputfile$Nome[i])
    cat(paste("Candidato: ",nome,sep=""),sep="\n");
    posts_timeseries_filename <- paste("posts_",nome,".xlsx",sep="");
    url <- as.character(inputfile$Link[i])
    id <- getFBID(url)
    cat(paste("PAGE-ID  - ",id,sep=""),sep="\n")
    page <- getPage(id, token = fb_oauth, feed=TRUE, since=data_inicio, until=data_fim, n=15000, api = "v2.12",reactions=TRUE)
    write.xlsx(page, file=posts_timeseries_filename)
}

for(i in 1:length(inputfile$Nome)){
  nome <- as.character(inputfile$Nome[i])
  cat(nome,sep="\n")
  posts_timeseries_filename <- paste("posts_",nome,".xlsx",sep="");
  page <- read_xlsx(posts_timeseries_filename)
  p1 <- page %>% mutate(type=case_when(
    grepl("live",story) ~ "live",
    grepl("is in",story) ~ "visita",
    TRUE ~ type
  )) %>% 
    mutate(Data = ymd_hms(created_time)%>%format("%Y-%m-%d")) %>% 
    group_by(Data, type) %>% 
    summarise(total = n()) %>% 
    ggplot(aes(x=ymd(Data),y=total,fill=type)) + 
    geom_bar(stat="identity") + 
    xlab("Data") + ylab("Posts") + 
    ggtitle(paste0("Posts por dia - ",unique(page$from_name)))
  
  p2 <- page %>% mutate(type=case_when(
    grepl("live",story) ~ "live",
    grepl("is in",story) ~ "visita",
    TRUE ~ type
  )) %>% 
    mutate(Data = ymd_hms(created_time)%>%format("%Y-%m-%d")) %>% 
    group_by(Data, type) %>% 
    summarise(total = sum(comments_count))%>% 
    ggplot(aes(x=ymd(Data),y=total,fill=type)) + 
    geom_bar(stat="identity") + 
    xlab("Data") + ylab("Comentários") + 
    ggtitle(paste0("Comentários por dia - ",unique(page$from_name)))
  
  p3 <- page %>% mutate(type=case_when(
    grepl("live",story) ~ "live",
    grepl("is in",story) ~ "visita",
    TRUE ~ type
  )) %>% 
    mutate(Data = ymd_hms(created_time)%>%format("%Y-%m-%d")) %>% 
    group_by(Data, type) %>% 
    summarise(total = sum(shares_count))%>% 
    ggplot(aes(x=ymd(Data),y=total,fill=type)) + 
    geom_bar(stat="identity") + 
    xlab("Data") + ylab("Compartilhamentos") + 
    ggtitle(paste0("Compartilhamentos por dia - ",unique(page$from_name)))
  
  png(paste0("PostsPorDia",nome,".png"),width=3200,height=1800,res=300)
  print(p1)
  dev.off()
  
  png(paste0("ComentariosPorDia",nome,".png"),width=3200,height=1800,res=300)
  print(p2)
  dev.off()
  
  png(paste0("CompartilhamentosPorDia",nome,".png"),width=3200,height=1800,res=300)
  print(p3)
  dev.off()
}
