plotSerieTemporalReactions <- function(day1, day7, lastday, data_fanpage){

   dataframe_ratiolasc <- list()
   
   while(ymd(day7) < ymd(lastday)){
      
      dmy_data <- data_fanpage %>% 
         mutate(day = ymd_hms(created_time) %>%
                   as.Date() %>%
                   format("%d"),
                month = ymd_hms(created_time) %>%
                   as.Date() %>%
                   format("%m"),
                year = ymd_hms(created_time) %>%
                   as.Date() %>%
                   format("%Y"),
                hour = ymd_hms(created_time) %>%
                   format("%H"),
                min = ymd_hms(created_time) %>%
                   format("%M"), date = ymd_hms(data_fanpage$created_time)%>%as.Date() %>% format("%Y/%m/%d"))
      
      ratio_la <- dmy_data %>% 
         filter(date > day1 & date < day7) %>% 
         select(from_name, likes_count, love_count, angry_count, haha_count, sad_count, wow_count) %>% 
         group_by(from_name) %>% 
         summarise(sumlikes = sum(likes_count, na.rm=TRUE),
                   sumloves = sum(love_count, na.rm=TRUE), 
                   sumangries = sum(angry_count, na.rm=TRUE),
                   sumhahas = sum(haha_count, na.rm=TRUE),
                   sumsads = sum(sad_count, na.rm=TRUE),
                   sumwows = sum(wow_count, na.rm=TRUE)) %>% 
         mutate(ratio_l_a = ifelse((sumangries+sumloves)==0,0,(sumloves-sumangries)/(sumloves + sumangries)),
                ratio_l_all = ifelse((sumangries+sumloves+sumhahas+sumsads+sumwows)==0,0,(sumloves - (sumangries+sumloves+sumhahas+sumsads+sumwows))/(sumangries+sumloves+sumhahas+sumsads+sumwows))) %>% 
         select(from_name, ratio_l_a, ratio_l_all,sumloves,sumangries,sumlikes,sumhahas,sumsads,sumwows)
      
      ratio_sc <- dmy_data %>% 
         filter(date > day1 & date < day7) %>% 
         select(from_name, shares_count, comments_count) %>% 
         group_by(from_name) %>% 
         summarise(sumshares = sum(shares_count, na.rm=TRUE), 
                   sumcomments = sum(comments_count, na.rm=TRUE)) %>% 
         mutate(ratio_s_c = ifelse((sumshares-sumcomments)==0,0,(sumshares-sumcomments)/(sumshares + sumcomments))) %>% 
         select(ratio_s_c,sumshares,sumcomments)
      
      dataframe_ratiolasc <- rbind(dataframe_ratiolasc, cbind(date = ymd(day1), ratio_la, ratio_sc = ratio_sc))
      #dataframe_ratiolasc <- rbind(dataframe_ratiolasc, cbind(date = ymd(day1),ratio_la))
      day1 <- ymd(day1) + days(1)
      day7 <- ymd(day7) + days(1)
      
   }
   
   names(dataframe_ratiolasc) <- c("date","Fã-Page","ratio_l_a","ratio_l_all","sumloves","sumangries","sumlikes","sumhahas","sumsads","sumwows","ratio_s_c","sumshares","sumcomments")

   p <- ggplot(dataframe_ratiolasc, aes(x = date, y=sumshares, col=`Fã-Page`)) + 
      geom_line(stat="identity") +
      facet_wrap(~`Fã-Page`) +
      geom_smooth(method="lm",se=FALSE) +
      ylab("Número de Compartilhamentos") +
      xlab("Data") 
   
   png("compara_timeseries_shares_semanal.png",width=3200,height=1800,res=300)
   print(p)
   dev.off()
   
   p <- ggplot(dataframe_ratiolasc, aes(x = date, y=sumcomments, col=`Fã-Page`)) + 
      geom_line(stat="identity") +
      facet_wrap(~`Fã-Page`) +
      geom_smooth(method="lm",se=FALSE) +
      ylab("Número de Comentários") +
      xlab("Data") 
   
   png("compara_timeseries_comments_semanal.png",width=3200,height=1800,res=300)
   print(p)
   dev.off()
   
   p <- ggplot(dataframe_ratiolasc, aes(x = date, y=sumsads, col=`Fã-Page`)) + 
      geom_line(stat="identity") +
      facet_wrap(~`Fã-Page`) +
      geom_smooth(method="lm",se=FALSE) +
      ylab("Número de Sads") +
      xlab("Data") 
   
   png("compara_timeseries_sads_reactions_semanal.png",width=3200,height=1800,res=300)
   print(p)
   dev.off()
   
   p <- ggplot(dataframe_ratiolasc, aes(x = date, y=sumhahas, col=`Fã-Page`)) + 
      geom_line(stat="identity") +
      facet_wrap(~`Fã-Page`) +
      geom_smooth(method="lm",se=FALSE) +
      ylab("Número de Hahas") +
      xlab("Data") 
   
   png("compara_timeseries_hahas_reactions_semanal.png",width=3200,height=1800,res=300)
   print(p)
   dev.off()
   
   p <- ggplot(dataframe_ratiolasc, aes(x = date, y=sumlikes, col=`Fã-Page`)) + 
      geom_line(stat="identity") +
      facet_wrap(~`Fã-Page`) +
      geom_smooth(method="lm",se=FALSE) +
      ylab("Número de Likes") +
      xlab("Data") 
   
   png("compara_timeseries_likes_reactions_semanal.png",width=3200,height=1800,res=300)
   print(p)
   dev.off()
   
   p <- ggplot(dataframe_ratiolasc, aes(x = date, y=sumangries, col=`Fã-Page`)) + 
      geom_line(stat="identity") +
      facet_wrap(~`Fã-Page`) +
      geom_smooth(method="lm",se=FALSE) +
      ylab("Número de Angries") +
      xlab("Data") 
   
   png("compara_timeseries_angries_reactions_semanal.png",width=3200,height=1800,res=300)
   print(p)
   dev.off()
   
   p <- ggplot(dataframe_ratiolasc, aes(x = date, y=sumloves, col=`Fã-Page`)) + 
      geom_line(stat="identity") +
      facet_wrap(~`Fã-Page`) +
      geom_smooth(method="lm",se=FALSE) +
      ylab("Número de Loves") +
      xlab("Data") 
   
   png("compara_timeseries_loves_reactions_semanal.png",width=3200,height=1800,res=300)
   print(p)
   dev.off()
   
   p <- ggplot(dataframe_ratiolasc, aes(x = date, y=ratio_l_a, col=`Fã-Page`)) + 
      geom_line(stat="identity") +
      facet_wrap(~`Fã-Page`) +
      geom_smooth(method="lm",se=FALSE) +
      ylab("Loves-Angries Index") +
      ylim(-1,1) + 
      xlab("Data") 
   
   png("compara_timeseries_ratio_loves_angries_semanal.png",width=3200,height=1800,res=300)
   print(p)
   dev.off()
   
   p <- ggplot(dataframe_ratiolasc, aes(x = date, y=ratio_l_all, col=`Fã-Page`)) + 
      geom_line(stat="identity") +       
      facet_wrap(~`Fã-Page`) +
      geom_smooth(method="lm",se=FALSE) +
      ylab("Loves-Reactions Index") +
      ylim(-1,1) + 
      xlab("Data") 
   
   png("compara_timeseries_ratio_loves_reactions_semanal.png",width=3200,height=1800,res=300)
   print(p)
   dev.off()
   
   p <- ggplot(dataframe_ratiolasc, aes(x = date, y=ratio_s_c, col=`Fã-Page`)) + 
      geom_line(stat="identity") +
      facet_wrap(~`Fã-Page`) +
      geom_smooth(method="lm", se=FALSE) +
      xlab("Data") + 
      ylim(-1,1) + 
      ylab("Shares-Comments Index") 
   
   png("compara_timeseries_ratio_shares_comments_semanal.png",width=3200,height=1800,res=300)
   print(p)
   dev.off()
}

library(Rfacebook)
library(readxl)
library(tidyverse)
library(reshape2) 
library(lubridate)
library(ggplot2)

workdir <- "/home/charles/GitRepos/armagedon/NucleoPolitico/Modelo/Espanha"
setwd(workdir)
filenames <- system(paste("ls *.xlsx",sep=""),intern=TRUE);

data_fanpage <- list()
for(i in 1:length(filenames)){
   page <- read_xlsx(filenames[i]);
   data_fanpage <- rbind(data_fanpage,page)
   
   save(data_fanpage,file="./data_fanpage_2019.Rdat");
}

plotSerieTemporalReactions("2019-01-01", "2019-01-07", "2019-04-07",data_fanpage[which(!is.na(data_fanpage$from_name)),])

