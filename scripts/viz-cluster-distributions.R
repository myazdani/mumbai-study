mumbai.train = read.csv("~/Documents/IEEE-escience/results/50_cluster_distribution_mumbai_train.csv", header = TRUE, stringsAsFactors = FALSE)
mumbai.test = read.csv("~/Documents/IEEE-escience/results/50_cluster_distribution_mumbai_test.csv", header = TRUE, stringsAsFactors = FALSE)
not.mumbai = read.csv("~/Documents/IEEE-escience/results/50_cluster_distribution_not_mumbai.csv", header = TRUE, stringsAsFactors = FALSE)

df.mumbai.train = as.data.frame(table(mumbai.train$cluster))
names(df.mumbai.train)[2] = "mumbai.train"
df.mumbai.test = as.data.frame(table(mumbai.test$cluster))
names(df.mumbai.test)[2] = "mumbai.test"
df.not.mumbai = as.data.frame(table(not.mumbai$cluster))
names(df.not.mumbai)[2] = "not.mumbai"


merged.df = Reduce(function(...) merge(..., all=T, by = "Var1"), list(df.mumbai.train, df.mumbai.test, df.not.mumbai))
merged.df$not.mumbai[is.na(merged.df$not.mumbai)] = 0
merged.df$mumbai.train = merged.df$mumbai.train/sum(merged.df$mumbai.train)
merged.df$mumbai.test = merged.df$mumbai.test/sum(merged.df$mumbai.test)
merged.df$not.mumbai = merged.df$not.mumbai/sum(merged.df$not.mumbai)


library(reshape)

df.m = melt(merged.df, id.vars = "Var1")
library(ggplot2)

pg <- PlantGrowth    # Copy data into new data frame
# Rename the column and the values in the factor
levels(pg$group)[levels(pg$group)=="ctrl"] <- "Control"
levels(pg$group)[levels(pg$group)=="trt1"] <- "Treatment 1"
levels(pg$group)[levels(pg$group)=="trt2"] <- "Treatment 2"
names(pg)[names(pg)=="group"]  <- "Experimental Condition"

levels(df.m$variable)[levels(df.m$variable)=="mumbai.train"] = "Mumbai Train Set"
levels(df.m$variable)[levels(df.m$variable)=="mumbai.test"] = "Mumbai Test Set"
levels(df.m$variable)[levels(df.m$variable)=="not.mumbai"] = "Images outside Mumbai"
ggplot(df.m, aes(x = as.factor(Var1), y = value, colour = variable)) + 
  geom_point(size = 2) + xlab("") + ylab("Percentage of images in clusters") + 
  theme(legend.title=element_blank(),
        legend.position = c(0.8, 0.8),
        legend.text=element_text(size=10),
        axis.text=element_text(size=8),
        axis.title=element_text(size=12))-> p

ggsave("~/Documents/IEEE-escience/results/cluster-distributions.png", plot = p, 
       width = 8, height = 4)

