rm(list=ls())
setwd("C:/Users/galujanm/Documents/ML performance metrics/flow")
#host, score, rank
library(ggplot2)
library(stringr)

df <- read.csv('flow.csv', stringsAsFactors = F)
df <- df[!(df$X.4 == "" | substr(df$X.4,1,4) == 'NULL' | df$X.4 == "Strike Tuples" ),]

parsed <- c()

for (i in 1:nrow(df)){
  num <- str_count(df$X.4[i], "->")
  if (num > 0){
    s <- df$X.4[i]
    indexes <- c(1)
    for (k in 1:(nchar(s)-1)){
      a <- substr(s,k,k)
      b <- substr(s,k+1,k+1)
      if (grepl('[0-9]', a) && grepl('[A-Za-z]', b) && (b != 'N')) {indexes <- c(indexes,k,k+1)}
    }
    indexes <- c(indexes, nchar(s))
    for ( j in 1:num){
      counter <- j*2-1
      parsed <- c(parsed,substr(s,indexes[counter],indexes[counter+1]))
    }
  }
}


masterIndex <- matrix(NA, nrow=length(parsed), ncol=10)
for (j in 1:length(parsed)){
#for (j in 1:3){
  pointer <-1
  index <- (1)
  while (TRUE) {
    if (grepl('[A-Za-z]', substr(parsed[j],pointer, pointer)) && 
        grepl(' ', substr(parsed[j], pointer+1, pointer+1) ) ) {
      index <- c(index,pointer,pointer+2)
      pointer <- pointer  + 1
      #print('Break 1')
      break}
    pointer <- pointer + 1
  }
  while (TRUE) {
    if (grepl('[0-9]', substr(parsed[j],pointer, pointer)) && 
        grepl(':', substr(parsed[j],pointer+1, pointer+1) ) ){
      index <- c(index,pointer,pointer+2)
      pointer <- pointer  + 1
      #print('Break 2')
      break}
    pointer <- pointer + 1
  }
  while (TRUE) {
    if (grepl('[0-9]', substr(parsed[j],pointer, pointer)) && 
        grepl('-', substr(parsed[j], pointer+1, pointer+1) ) ) {
      index <- c(index,pointer,pointer+3)
      pointer <- pointer  + 1
      #print('Break 3')
      break}
    pointer <- pointer + 1
  }
  while (TRUE) {
    if (grepl('[0-9]', substr(parsed[j],pointer, pointer)) && 
        grepl(':',  substr(parsed[j], pointer+1, pointer+1) ) ) {
      index <- c(index,pointer,pointer+2)
      #print('Break 4')
      break}
    pointer <- pointer + 1
  }
  index <- c(index,nchar(parsed[j]))
  masterIndex[j,] <- index
}

master <- data.frame()
for(i in 1:length(parsed)){
#for(i in 1:3){
  tempDF <- data.frame(original = parsed[i],
                       protocol = substr(parsed[i],masterIndex[i,1],masterIndex[i,2]),
                       sip =      substr(parsed[i],masterIndex[i,3],masterIndex[i,4]),
                       sport =    substr(parsed[i],masterIndex[i,5],masterIndex[i,6]),
                       dip =      substr(parsed[i],masterIndex[i,7],masterIndex[i,8]),
                       dport =    substr(parsed[i],masterIndex[i,9],masterIndex[i,10]))
  master <- rbind(master,tempDF)
}

write.csv(master, 'flowAttacks2016_11_11.csv', row.names = F)
