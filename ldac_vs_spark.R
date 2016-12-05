aucDF_master_c$algorithm <- as.factor("LDA-c")
aucDF_master_spark$algorithm <-as.factor("Spark-LDA")

flowMaster <- rbind(aucDF_master_c,aucDF_master_spark)

p5 <- ggplot(flowMaster, aes(x=numTopics, y=auc)) +
  geom_boxplot(aes(fill=algorithm), position=position_dodge(.9)) +
  facet_grid(.~numTopics, scales = "free") +
  #geom_jitter(width = 0.1) + 
  stat_summary(fun.y=mean, geom="point", aes(group=algorithm), position=position_dodge(.9), 
               color="red", shape=18, size=3) +
  ggtitle("Performance comparison LDA-c vs Spark-LDA (10 replicates)")

print(p5)

unic <- unique(flowMaster$numTopics)

for (i in 1:length(unic)){
  ldac  <- flowMaster[flowMaster$numTopics == unic[i] & flowMaster$algorithm == "LDA-c" , ]
  spark <- flowMaster[flowMaster$numTopics == unic[i] & flowMaster$algorithm == "Spark-LDA", ]
  testVar <- var.test(ldac$auc,spark$auc)
  #print(testVar)
  if(testVar$p.value < 0.05){
    testDif <- t.test(ldac$auc,spark$auc, var.equal=F, paired=FALSE)
    #print(testDif)
  } else {
    testDif <- t.test(ldac$auc,spark$auc, var.equal=T, paired=FALSE)
    #print(testDif)
  }
  if (testDif$p.value < 0.05){
    print(paste("Significant differences in numTopic = ", unic[i]))
    print(testDif)
  }
}