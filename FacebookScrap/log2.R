source("./log.R")
names(dat)
dat %>% arrange(comments_count) %>% tail(10)
dat %>% arrange(comments_count) %>% tail(10) %>% select(id,type,comments_count)
dat %>% arrange(comments_count) %>% tail(20) %>% select(id,type,comments_count)
dat %>% arrange(comments_count) %>% tail(10) %>% select(id,type,comments_count)
dat %>% arrange(comments_count) %>% tail(30) %>% select(id,type,comments_count) %>%
dat %>% arrange(comments_count) %>% tail(30) %>% select(id,type,comments_count) %>% group_by(type) %>% summarise(total = sum(comments_count))
dat %>% arrange(comments_count) %>% tail(30) %>% select(id,type,comments_count) %>% group_by(type) %>% summarise(total = mean(comments_count))
dat %>% arrange(comments_count) %>% tail(30) %>% select(id,type,comments_count) %>% group_by(type) %>% summarise(total = mean(comments_count)) %>% ggplot(aes(x=type,y=total,fill=total)) + geom_bar(stat="identity") + xlab("Tipo de Publicação") + ylab("Número médio de Comentários")
dat %>% arrange(comments_count) %>% tail(30) %>% select(id,type,comments_count) %>% group_by(type) %>% summarise(total = mean(comments_count)) %>% ggplot(aes(x=type,y=total,fill=type)) + geom_bar(stat="identity") + xlab("Tipo de Publicação") + ylab("Número médio de Comentários")
dat %>% arrange(comments_count) %>% tail(30) %>% select(id,type,comments_count) %>% group_by(type) %>% summarise(total = mean(comments_count)) %>% ggplot(aes(x=type,y=total,fill=type)) + geom_bar(stat="identity") + xlab("Tipo de Publicação") + ylab("Número médio de Comentários") + geom_text(aes(x=type,y=total,hjust=0,label=total))
dat %>% arrange(comments_count) %>% tail(30) %>% select(id,type,comments_count) %>% group_by(type) %>% summarise(total = mean(comments_count)) %>% ggplot(aes(x=type,y=total,fill=type)) + geom_bar(stat="identity") + xlab("Tipo de Publicação") + ylab("Número médio de Comentários") + geom_text(aes(x=type,y=total,hjust=0,label=signif(total,2)))
dat %>% arrange(comments_count) %>% tail(30) %>% select(id,type,comments_count) %>% group_by(type) %>% summarise(total = mean(comments_count)) %>% ggplot(aes(x=type,y=total,fill=type)) + geom_bar(stat="identity") + xlab("Tipo de Publicação") + ylab("Número médio de Comentários") + geom_text(aes(x=type,y=total,hjust=1,label=signif(total,2)))
dat %>% arrange(comments_count) %>% tail(30) %>% select(id,type,comments_count) %>% group_by(type) %>% summarise(total = mean(comments_count)) %>% ggplot(aes(x=type,y=total,fill=type)) + geom_bar(stat="identity") + xlab("Tipo de Publicação") + ylab("Número médio de Comentários") + geom_text(aes(x=type,y=total,vjust=0,label=signif(total,2)))
png("ComentariosPorTipoDePost.png",width=3200,height=1800,res=300)
dat %>% arrange(comments_count) %>% tail(30) %>% select(id,type,comments_count) %>% group_by(type) %>% summarise(total = mean(comments_count)) %>% ggplot(aes(x=type,y=total,fill=type)) + geom_bar(stat="identity") + xlab("Tipo de Publicação") + ylab("Número médio de Comentários") + geom_text(aes(x=type,y=total,vjust=0,label=signif(total,2)))
dev.off()
dat %>% arrange(shares_count) %>% tail(30) %>% group_by(type) %>% summarise(total = mean(shares_count)) %>% ggplot(aes(x=type,y=total,fill=type)) + geom_bar(stat="identity") + xlab("Tipo de Publicação") + ylab("Número médio de Compartilhamentos") + geom_text(aes(x=type,y=total,vjust=0,label=signif(total,2)))
png("CompartilhamentosPorTipoDePost.png",width=3200,height=1800,res=300)
dat %>% arrange(shares_count) %>% tail(30) %>% group_by(type) %>% summarise(total = mean(shares_count)) %>% ggplot(aes(x=type,y=total,fill=type)) + geom_bar(stat="identity") + xlab("Tipo de Publicação") + ylab("Número médio de Compartilhamentos") + geom_text(aes(x=type,y=total,vjust=0,label=signif(total,2)))
dev.off()
dat %>% arrange(likes_count) %>% tail(30) %>% group_by(type) %>% summarise(total = mean(likes_count)) %>% ggplot(aes(x=type,y=total,fill=type)) + geom_bar(stat="identity") + xlab("Tipo de Publicação") + ylab("Número médio de Curtidas") + geom_text(aes(x=type,y=total,vjust=0,label=signif(total,2)))
mean(dat$likes_count)
table(dat$type)
dat %>% arrange(likes_count) %>% tail(30) %>% group_by(type) %>% summarise(total = mean(likes_count)) %>% ggplot(aes(x=type,y=total,fill=type)) + geom_bar(stat="identity") + xlab("Tipo de Publicação") + ylab("Número médio de Curtidas") + geom_text(aes(x=type,y=total,vjust=0,label=signif(total,2)))
dat %>% arrange(likes_count) %>% tail(30) %>% group_by(type) %>% summarise(total = mean(likes_count))
dat %>% arrange(likes_count) %>% tail(30) %>% group_by(type) %>% summarise(total = mean(likes_count)) %>% ggplot(aes(x=type,y=total,fill=type)) + geom_bar(stat="identity") + xlab("Tipo de Publicação") + ylab("Número médio de Curtidas") + geom_text(aes(x=type,y=total,vjust=0,label=signif(total,1)))
dat %>% arrange(likes_count) %>% tail(30) %>% group_by(type) %>% summarise(total = mean(likes_count)) %>% ggplot(aes(x=type,y=total,fill=type)) + geom_bar(stat="identity") + xlab("Tipo de Publicação") + ylab("Número médio de Curtidas") + geom_text(aes(x=type,y=total,vjust=0,label=signif(total,3)))
png("CurtidasPorTipoDePost.png",width=3200,height=1800,res=300)
dat %>% arrange(likes_count) %>% tail(30) %>% group_by(type) %>% summarise(total = mean(likes_count)) %>% ggplot(aes(x=type,y=total,fill=type)) + geom_bar(stat="identity") + xlab("Tipo de Publicação") + ylab("Número médio de Curtidas") + geom_text(aes(x=type,y=total,vjust=0,label=signif(total,3)))
dev.off()
dat %>% arrange(comments_count) %>% tail(30)
dat %>% arrange(comments_count) %>% tail(20)
dat %>% arrange(comments_count) %>% tail(10)
dat %>% arrange(comments_count) %>% tail(10) %>% select(id,comments_count)
dat %>% arrange(comments_count) %>% tail(20) %>% select(id,comments_count)
dat %>% arrange(comments_count) %>% tail(10) %>% select(id,comments_count)
dat %>% arrange(comments_count) %>% tail(10) %>% select(id)
ids <- dat %>% arrange(comments_count) %>% tail(10) %>% select(id)
fb_oauth <- "EAARVKUum8icBAInyMpfsotjUZBYFR1PEaIdQTu8RrrebjnN6ZAT4PKuzZAZBia9erOMfeuB6KMEfkimeX0Aedqvbx5voHcUA2Jn4Ax2QJHijj11vfSxxkhPdzy8VRCFxekUuopyURUUSaz2XumtMpOaCVwRAHNmmA49ynZCPZCfbbvElW3oBZAXcAvRbJUOiLs1Y8VxL4ar5QZDZD"
df_posts <- data.frame()
for(postid in ids){
post <- getPost(post=postid,
token = fb_oauth,
n = 1000000,
comments = TRUE,
likes = FALSE,
n.likes = 1,
n.comments = 1000000)
tablepost <- post$comments
df_posts <- rbind(df_posts,paste(as.character(tablepost),collapse=" "))
}
library(Rfacebook)
for(postid in ids){
post <- getPost(post=postid,token = fb_oauth, n = 1000000, comments = TRUE, likes = FALSE, n.likes = 1, n.comments = 1000000)
tablepost <- post$comments
df_posts <- rbind(df_posts,paste(as.character(tablepost),collapse=" "))
}
ids
postid <- ids[1]
post <- getPost(post=postid,token = fb_oauth, n = 1000000, comments = TRUE, likes = FALSE, n.likes = 1, n.comments = 1000000)
postid
postid <- ids[1]
postid
ids
ids[1]
unlist(ids)[1]
as.character(unlist(ids)[1])
as.character(unlist(ids))
ids <- as.character(unlist(ids))
postid <- ids[1]
postid
post <- getPost(post=postid,token = fb_oauth, n = 1000000, comments = TRUE, likes = FALSE, n.likes = 1, n.comments = 1000000)
for(postid in ids){
post <- getPost(post=postid,token = fb_oauth, n = 1000000, comments = TRUE, likes = FALSE, n.likes = 1, n.comments = 1000000)
tablepost <- post$comments
df_posts <- rbind(df_posts,paste(as.character(tablepost),collapse=" "))
}
post
df_posts <- data.frame()
for(postid in ids){
post <- getPost(post=postid,token = fb_oauth, n = 1000000, comments = TRUE, likes = FALSE, n.likes = 1, n.comments = 1000000)
tablepost <- post$comments$message
df_posts <- rbind(df_posts,paste(as.character(tablepost),collapse=" "))
}
df_posts
savehistory("./log2.R")
