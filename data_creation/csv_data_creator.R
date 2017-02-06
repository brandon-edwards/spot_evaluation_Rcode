# Creates data files for use in testing dns_spark.R
# There should be 11 columns with the rank in the last column. For testing purposed only the rank value matters.
# For purposes of understanding rank it is helpful to know that the total number of possible rank values is (0 to 174361843 )

setwd("/Users/edwardsb/LocalRepos/gustavo_evaluation_Rcode/data_creation")
library(pROC)
library(ggplot2)


total_columns <- 11
total_attack_rows <- 14475
replicates <- 10
num_topics_list <- list(5, 10, 15, 20, 30, 50, 100)
purturbed <- FALSE

ranked_attacks <- data.frame()

# Create buffer column
buffer_column <- c()
for (row in 1:total_attack_rows){
  buffer_column <- c(buffer_column, 'buffer')
}

# Create rank column for producing 0.5 AUC value
rank_column <- c()
for (row in 1:total_attack_rows){
  rank_column <- c(rank_column, as.integer(row*12045.7232))
}

# Create the complete data frame for ranked attacks
ranked_attacks_data_frame = data.frame(buffer_column,
                                       buffer_column,
                                       buffer_column,
                                       buffer_column,
                                       buffer_column,
                                       buffer_column,
                                       buffer_column,
                                       buffer_column,
                                       buffer_column,
                                       buffer_column,
                                       rank_column)

colnames(ranked_attacks_data_frame) <- c('timeStamp', 
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


# If purturbed == TRUE, add a normally distributed random value to the ranks to create variation, then output to file
for (num_topics in num_topics_list){
  for (replicate in 1:replicates){
    out_df <- ranked_attacks_data_frame
    if (purturbed == TRUE){
      out_df$rank <- out_df$rank + as.integer((rnorm(total_attack_rows, sd = 100)))
    }
    write.csv(out_df,paste0('synth_dns_spark_',num_topics,'_',replicate,'.csv'), row.names = F)
  } 
}