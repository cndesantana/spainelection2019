library(readxl)
library(ggplot2)
library(dplyr)
library(lubridate)
library(tidyr)
dat <- read_xlsx("./posts_JhonatasMonteiroOficial.xlsx")
dat2 <- dat %>% mutate(created_time = ymd_hms(created_time)%>%format("%Y-%m-%d")) %>% group_by(created_time) %>% summarise(total_loves = sum(love_count), total_angry = sum(angry_count), total_like = sum(likes_count), total_shares = sum(shares_count), total_comments = sum(comments_count))
dat2 <- dat2 %>% gather(key, values, -created_time)
png("ReacoesPorDia.png",width=3200,height=1800,res=300)
dat2 %>% ggplot(aes(x=ymd(created_time), y = value, color=Reações)) + geom_line(stat="identity",size=1.5) + xlab("Data") + ylab("Reações") + ggtitle("Jhonatas Monteiro Oficial - 2018")
dev.off()


