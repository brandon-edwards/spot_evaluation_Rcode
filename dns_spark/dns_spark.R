#rm(list=ls())
setwd("/Users/edwardsb/LocalRepos/gustavo_evaluation_Rcode/dns_spark")
#host, score, rank
library(pROC)
library(ggplot2)

insertRow <- function(existingDF, newrow, r) {
  existingDF[seq(r+1,nrow(existingDF)+1),] <- existingDF[seq(r,nrow(existingDF)),]
  existingDF[r,] <- newrow
  existingDF
}

f5   <- list.files(pattern="dns_spark_5_")
f10  <- list.files(pattern="dns_spark_10_")
f15  <- list.files(pattern="dns_spark_15_")
f20  <- list.files(pattern="dns_spark_20_")
f30  <- list.files(pattern="dns_spark_30_")
f50  <- list.files(pattern="dns_spark_50_")
f100 <- list.files(pattern="dns_spark_100_")

listFiles <-list(f5,f10,f15,f20,f30,f50,f100)

replicates <- length(f5)

#Total rows: normal + abnormal
totalRows <- 174361844
topics <- c(5,10,15,20,30,50,100)

aucDF_master <- data.frame()


for (k in 1:length(topics)){
  
  AUC_tracking <- c()
  files <- listFiles[[k]]
  
  for(j in 1:replicates){
    
    print(paste("# Topics:",topics[k],"replicate:",j))
    
    all_content = readLines(files[j])
    #print("MUST DECIDE WHAT LINES TO SKIP IN FILES")
    #skip = all_content[-c(1:4)]
    #df = read.csv(textConnection(skip), header = F, stringsAsFactors = FALSE)
    df = read.csv(textConnection(all_content), header = F, stringsAsFactors = FALSE)
    # colnames(df) <- c('host','score','rank')
    colnames(df) <- c('timeStamp', 
                      'unixTimeStamp',
                      'frameLength',
                      'clientIP', 
                      'queryName', 
                      'queryClass', 
                      'queryType', 
                      'queryResponseCode',
                      'dnsRecordID', 
                      'score', 
                      'rank')
    
    df <- df[with(df, order(rank)),]
    df2 <- matrix(0, nrow= totalRows, ncol = 3)
    df2 <- as.data.frame(df2)
    
    df$timeStamp <- NULL
    df$unixTimeStamp <- NULL
    df$frameLength <- NULL
    df$clientIP <- NULL
    df$queryName <- NULL
    df$queryClass <- NULL
    df$queryType <- NULL
    df$queryResponseCode <- NULL
    
    colnames(df2) <- colnames(df)
    df$label <- 'bad'
    df2$label <- 'normal'
    df$y = 1
    toplot <- df[,c('rank','y')]
    toplot$rank <- toplot$rank + as.integer((rnorm(nrow(toplot), sd = 10)))
    toplot$y <-  rnorm(nrow(toplot))*.05 
    df$TP <- -99
    df$FP <- -99
    df <- insertRow(df,c(1,1,0,'null',1,-99,-99),1)
    #df <- insertRow(df,c(1,1,0,'good',1,-99,-99),2)
    df$rank <- as.numeric(df$rank)
    df <- df[order(df$rank),]
    df <- rbind(df,c(1,1,0,'null',1,1,1))
    df$rank <- as.numeric(df$rank)
    df$TP <- as.numeric(df$TP)
    df$FP <- as.numeric(df$FP)
    df$TP[1] <- 0
    df$FP[1] <- 0
    
    for (i in 2:(nrow(df)-1)){
      df$TP[i] <- (i-1)/(nrow(df)-2)
      df$FP[i] <- (df$rank[i]+1-i)/ (nrow(df2)-nrow(df)+2) # WAS: df$FP[i] <- (df$rank[i]-1-i)/ nrow(df2)
    }
    AUC <- 0
    for (i in 1:(nrow(df)-1)){
      #print(i)
      #print((df$TP[i+1]) *(df$FP[i+1]-df$FP[i]))
      AUC <- AUC + (df$TP[i]) *(df$FP[i+1]-df$FP[i]) # WAS: AUC <- AUC + (df$TP[i+1]) *(df$FP[i+1]-df$FP[i])
    }
    
    AUC_tracking <- c(AUC_tracking,AUC)
    
    print("Made it to just before writing metrics csv")
    
    df$y <- c(0:(nrow(df)-1))
    write.csv(df,paste0('metrics_df_',topics[k],'_',j,'.csv'), row.names = F)
    
    print("about to bind the current rank_tracking")
    
    if(j == 1){
      rank_tracking <- df 
    } else {
      rank_tracking <- cbind(rank_tracking,df)
    }
  }
  
  print ("Made it to just before plotting toplot")
  
  ggplot(toplot,aes(x=rank,y=y)) + geom_point(pch = 21,position = position_jitter(width = 1)) +
    ylim(-0.5,.5) + xlim(-round(nrow(df2)*.1,0), nrow(df2)) +
    ggtitle(paste("Rank for anomalies with", topics[k], "topics",'numLogs', nrow(df)-2))
  ggsave(paste0('rank_',topics[k],'.png'),width = 15, height =10, units = 'cm')
  
  print("Made it to just before plotting ROC")
  ggplot(df, aes(FP,TP)) + geom_line() +
    xlab("FP (1-specificity)" ) + ylab("TP (sensititiy)") +
    ggtitle(paste('ROC-AUC DNS for',topics[k], 'topics')) +
    geom_abline(slope=1, intercept=0)
  ggsave(paste0('AUC_',topics[k],'.png'),width = 10, height =10, units = 'cm')
  
  print("Made it to just before creating aucDF")
  
  aucDF <- data.frame( auc = AUC_tracking)
  aucDF$numTopics <- topics[k]
  aucDF$replicates <- 1:10
  aucDF_master <- rbind(aucDF_master,aucDF)
}

aucDF_master$numTopics <- as.factor(aucDF_master$numTopics)

print("Made it to just before aucDF_master numTopics and auc plot")

ggplot(aucDF_master, aes(numTopics,auc)) + 
  geom_boxplot() + geom_jitter(width = 0.2, height=0) + 
  stat_summary(fun.y=mean, colour="red", geom="point", 
               shape=18, size=3,show.legend = FALSE)  +
  ggtitle("AUC boxplots for different num Topics and 10 replicates")
ggsave('Boxplot.png',width = 15, height =10, units = 'cm')

print("Made it to just before summary statistics calcuation and plot")

summary(fm1 <- aov(auc ~ numTopics, data = aucDF_master))
tk <- TukeyHSD(fm1, "numTopics")
plot(tk)

dfTK <- data.frame(tk$numTopics)
dfTK$pairs <- row.names(dfTK)

print("Made it to just before dfTK plot")

ggplot(dfTK, aes(pairs, y = diff, ymin = lwr, ymax = upr)) +
  geom_point(aes(y = diff)) +
  geom_errorbar() + ylab("Difference in Mean") +
  geom_hline(yintercept = 0, colour="red", linetype = "longdash") +
  coord_flip() +
  ggtitle("Confidence intervals for mean differences")
ggsave('CI.png',width = 12, height =10, units = 'cm')

aucDF_master_spark <- aucDF_master


# lowest possible rank=0,highest possible rank=174361844
# number of attack entries = 14475
# boxplot showing statistics of individual stages of attacks
ggplot(subset(df, dnsRecordID != 1), aes(x=dnsRecordID, y=rank)) + geom_boxplot()+
  ylab("Word Rankings") + xlab("dnsRecordID = Attack Stage") +
  ggtitle("Statistics of Rank values within individual stages of attacks")
ggsave('WordRankingStatsByAttackStage.png')

