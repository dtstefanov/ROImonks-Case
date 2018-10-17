# load data ----
rm(list=ls())

z <- read.csv('C:/Users/O38648/Box Sync/R Scripts/Roi Monks/Data set task.csv', sep=",", stringsAsFactors=F, na.strings=c(" ","",NA))


# summary ----
smrz=data.frame(names(z),sapply(z,class),colSums(is.na(z)))
colnames(smrz)=c("Varname","Varclass","Sum.of.NAs")

# corr ----
install.packages("corrplot")
library(corrplot)
g <- cor(z[,unlist(lapply(z, is.numeric))])
corrplot(g, type="upper", order="hclust", sig.level = 0.01, insig = "blank")

# text cleaning ----

z$st=gsub("[[:punct:]]","",z$Search.Term) 
z$st=gsub("[[:digit:]]", "", z$st)
z$st=gsub("http\\w+", "", z$st) 
z$st=gsub("[ \t]{2,}", "", z$st) 
z$st=gsub("^\\s+|\\s+$", "", z$st) 
z$st=tolower(z$st)
z=z[,c(1,2,16,3:15)]

# wordstems ----
install.packages("SnowballC")
library(SnowballC)
l1=strsplit(z$st,split="[ ]")
l2=lapply(l1,wordStem)

for (i in 1:length(l2)){
  l2[[i]]=l2[[i]][order(l2[[i]])]
}
 
l3=lapply(l2,paste,collapse=" ")
z$st=l3
z$st=as.character(z$st)

# tokens ----
library(quanteda)
tkns=tokens(z$st, what="word")

# remove stopwords ----

stwds=stopwords(language="en")
tknsm=list()
for (i in 1:length(tkns)){
  tknsm[[i]]=tkns[[i]][!(tkns[[i]] %in% stwds)]
}
tknsm=as.tokens(tknsm)

# create bigrams & trigrams ----

tkns12=tokens_ngrams(tknsm,n=c(1,2,3), concatenator = " ")

bag.of.words=unlist(tkns12)
a=data.frame(table(bag.of.words))
a=a[which(a$Freq>10),]
a$bag.of.words=as.character(a$bag.of.words)
a=a[order(a$bag.of.words),]



# features ----

z[,17:(16+dim(a)[1])]=0
names(z)[17:dim(z)[2]]=a$bag.of.words
for (i in 1:dim(z)[1]){
  z[i,17:dim(z)[2]]=a$bag.of.words %in% tkns12[[i]]
}

# define number of clusters ----

wss <- 0

for (i in 1:10) {
  km.out <- kmeans(z[17:455], centers = i, nstart = 20)
  wss[i] <- km.out$tot.withinss
}

plot(1:10, wss, type = "b", 
     xlab = "number of clusters", 
     ylab = "tot.withinss")

k <- 4

# clustering ----
set.seed(1)

km.out <- kmeans(z[17:455], centers = k, nstart = 20)

km.out$size

k1 <- data.frame(z[km.out$cluster==1,])
k2 <- data.frame(z[km.out$cluster==2,])
k3 <- data.frame(z[km.out$cluster==3,])
k4 <- data.frame(z[km.out$cluster==4,])

k1_top <- data.frame(names(k1[17:455]),colSums(k1[17:455]))
k1_top <- k1_top[order(-k1_top[,2]),]
k2_top <- data.frame(names(k2[17:455]),colSums(k2[17:455]))
k2_top <- k2_top[order(-k2_top[,2]),]
k3_top <- data.frame(names(k3[17:455]),colSums(k3[17:455]))
k3_top <- k3_top[order(-k3_top[,2]),]
k4_top <- data.frame(names(k4[17:455]),colSums(k4[17:455]))
k4_top <- k4_top[order(-k4_top[,2]),]

# frequency ----

k1$cluster <- 1
k2$cluster <- 2
k3$cluster <- 3
k4$cluster <- 4

cc <- rbind(k1,k2,k3,k4)

freq <- data.frame(table(cc$st))
freq$st <- freq$Var1
zz <- merge(cc, freq, by="st")
zz$Var1 <- NULL

View(zz[,c(1,3,456:457)])

# labels ----

library(data.table)
t_k1_top <- transpose(k1_top[1:5,])
t_k1_top <- t_k1_top[-2,]
t_k2_top <- transpose(k2_top[1:5,])
t_k2_top <- t_k2_top[-2,]
t_k3_top <- transpose(k3_top[1:5,])
t_k3_top <- t_k3_top[-2,]
t_k4_top <- transpose(k4_top[1:5,])
t_k4_top <- t_k4_top[-2,]

t<-rbind(t_k1_top,t_k2_top,t_k3_top,t_k4_top)
t$cluster<-(1:4)

t$V1=gsub("[[:punct:]]"," ",t$V1)
t$V2=gsub("[[:punct:]]"," ",t$V2)
t$V3=gsub("[[:punct:]]"," ",t$V3)
t$V4=gsub("[[:punct:]]"," ",t$V4)
t$V5=gsub("[[:punct:]]"," ",t$V5)

colnames(t)=c('tag1','tag2','tag3','tag4','tag5','cluster')

zz1 <- merge(zz, t, by="cluster")

View(zz1[,c(1,2,4,457:462)])

for (i in 1:dim(zz1)[1]){
  sum = 0;
  for (j in 458:462){
    res = grepl(zz1[i,j], zz1[i,2])
    sum = sum + res
  }
  zz1[i,463] = sum;
}

View(zz1[,c(1,2,4,457:463)])
