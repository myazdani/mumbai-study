df = read.csv("~/Documents/IEEE-escience/data/clusters-merged-with-indicus-2.csv", header = TRUE, stringsAsFactors = FALSE)

df[,c("TOTAL_HOU2_N_19_11", "TOTAL_HOU3_N_19_11", "TOTAL_HOU4_N_19_11", 
      "TOTAL_HOU5_N_19_11", "TOTAL_HOU6_N_19_11", "TOTAL_HOU7_N_13_11", "TOTAL_HOU8_N_13_11")] = df[,c("TOTAL_HOU2_N_19_11", "TOTAL_HOU3_N_19_11", "TOTAL_HOU4_N_19_11", "TOTAL_HOU5_N_19_11", "TOTAL_HOU6_N_19_11", "TOTAL_HOU7_N_13_11", "TOTAL_HOU8_N_13_11")]/df$TOTAL_HOUS_N_19_11

library(dplyr)

## columns of interest
grp_cols <- names(df)[c(c(22:38))]


## compute averages for cluster ID for cols of interst
df[,c("cluster", grp_cols)] %>%
  group_by(cluster) %>%
  summarise_each(funs(mean)) -> df.agg

## tidy up data and visualize
library(reshape)
df.agg.m = melt(as.data.frame(df.agg), id.vars = "cluster")
ggplot(df.agg.m, aes(x = cluster, y = value)) + facet_wrap(~variable, scales = "free") + 
  geom_smooth(method = "lm") -> p

## compute correaltions for columns of interst
correlations = sapply(df.agg[,grp_cols], 
                      FUN = function(x) cor(df.agg$cluster, x, method = "spearman"))

plot(abs(correlations))
# max correlation (bigger better)
print(max(abs(correlations)))

# p-val estimate

cor.test(unlist(df.agg[,"cluster"]),
         unlist(df.agg[,grp_cols[which.max(abs(correlations))]]), 
         method = "spearman")
