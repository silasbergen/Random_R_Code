consumer_key <- "dinP5BfT22NQxCnJtZdvZUnJA"
consumer_secret <- "BDxuRwz1lPIi1r1yNEfJRGOuJqCO0MRvFh7zuzdEGiU2kqqYQX"
access_token <- "1059757435-dIjOtQz2jZdpHh6oMUOkOBHNNTmvnJ2ipDpxb2j"
access_secret <- "iwUUy2ng7cH7RBDubNzhaLNrKsezRYUmLpSUYgKDUkWuK"
setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

test <- searchTwitter("#chrisrock",n=5000)#,geocode='44.0500,-91.6333,2000mi')
test2 <- searchTwitter("#oscarssowhite",n=100)#,geocode='44.0500,-91.6333,2000mi')
test3 <- searchTwitter("wsubasketball",n=100)#,geocode='44.0500,-91.6333,2000mi')

junk <- searchTwitter("@WinonaStateMBB", n=1000)#, since='2016-01-25', until='2016-01-27')
#Using code from blog to convert to dataframe
df <- do.call("rbind", lapply(junk, as.data.frame))

#Using twListToDF function (which is part of twitteR library) to convert to a dataframe
df <- twListToDF(junk)

write.csv(df,file="C:/Teaching/DSCI210/Datasets/TwitterPulls/WinonaStateMBB.csv")


mylist <- c()
for(i in 1:length(test)) mylist[i] <- test2[[i]]$getText()
all <- unlist(strsplit(mylist,' '))
all2 <- all[-grep('http|@',all)]
unwords <- unique(all2)
count.unwords <- c()
for(i in 1:length(unwords)) count.unwords[i] <- sum(all2==unwords[i])

want <-unwords[order(count.unwords,decreasing=TRUE)][1:1000]
all3 <- all2[all2%in%want & nchar(all2) > 3]
all3 <- gsub(',|\\.+','',all3)
write.csv(all3,file='tweets3.csv',row.names=FALSE)

gsub(' ','',as.character(test[[1]]))
wordcloud(all3,random.order=FALSE)

