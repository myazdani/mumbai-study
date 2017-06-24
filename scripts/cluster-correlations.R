df = read.csv("~/Downloads/clusters-merged-with-indicus-2.csv", header = TRUE, stringsAsFactors = FALSE)

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

# max correlation (bigger better)
print(max(abs(correlations)))
