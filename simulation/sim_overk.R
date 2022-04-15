setwd("~/Box Sync/Research - Hui/Clustering Simulations/R scripts")

library(MASS)
library(Matrix)
library(ggplot2)
library(mvtnorm)
library(ggpubr)
library(mixtools)
library(RColorBrewer)
library(fossil)
library(dplyr)

#source('multivarGaussSim.R')
source('ClusterSimFunctions.R')

num=10000
set.seed(num)

d = 2
n = 100
rand_k = NULL
i=1
num_sim = 50

# Let's do it for several k values - start going from 2 to 10
# We're running this with all the same percent outliers (10%) - change out_perc if want
# Also running with lambda = 10
for (i in 2:7) {
  # Let's do 50 different sets per k

  # Set the total number of clusters
  c = i
  print(i)

  for (j in 1:num_sim) {
    # for each k, we want the repeated samples to be the same
    seed_num = num + j
    print(seed_num)
    #seed_num = 12351
    set.seed(seed_num)

    ################ Create the simulated data #################
    # Create the simulation data
    samples = replicate(c, multivarGaussian(n=n, d=2, out_perc=0.1, out_mag=5)) # changed from 5 to 10

    # Combine the c samples so that all samples are in one matrix
    samples = replicate(c, multivarGaussian(n=n, d=d, out_perc=perc_out, out_mag=5)) # changed from 5 to 10

    # Combine the c samples so that all samples are in one matrix
    sampleMat = samples[,1]$gauss
    if (c >1) {
      for (k in 2:c) {sampleMat = rbind(sampleMat, as.matrix(samples[,k]$gauss))}
    }
    # Combine the c mu's into one matrix
    sampleMu = t(sapply(1:c, function(x) samples[,x]$mu))

    # Combine the c sigmas into one list
    sampleSigma = lapply(1:c, function(x) as.matrix(samples[2,][[x]]))

    # Label the sample's clusters
    sampleMat1 = as.data.frame(cbind(sampleMat, rep(1:c, each=n)))
    colnames(sampleMat1) = c("X", "Y", "Cluster")
    sampleMat1$Cluster = as.character(sampleMat1$Cluster)

    result_rem = robustEM(sampleMat, c, Robust = T)
    result_standard = robustEM(sampleMat,c, Robust = F)
    true = as.numeric(sampleMat1$Cluster) ## The true label

    acc_rem = rand.index(true, result_rem$label)
    acc_std = rand.index(true, result_standard$label)

    curr_stand = c(perc_out*100, seed_num, "standard", acc_std)
    curr_rob = c(perc_out*100, seed_num, "robust", acc_rem)

    rand_outliers = rbind(rand_outliers, curr_stand, curr_rob)
  }
}
# Make it a data frame
## Using dplyr to manipulate the data
rand = as.data.frame(rand_outliers)
colnames(rand) = c("Variance_diag", "Seed_number", "Type_EM", "Rand_index")
## Using dplyr to manipulate the data
rand = rand %>% mutate(Variance_diag = as.numeric(as.character(Variance_diag)),
                       Seed_number = as.numeric(as.character(Seed_number)),
                       Rand_index = as.numeric(as.character(Rand_index)))
# Creating data frame with the mean and standard deviation to plot
rand_mean = rand %>% group_by(Variance_diag, Type_EM) %>%
  summarise(mean=mean(Rand_index),
            lower = mean - 1.96*sd(Rand_index),
            upper = min(mean + 1.96*sd(Rand_index),1))

## Plot the results
ggplot(rand_mean, aes(x = Variance_diag, y = mean, colour = Type_EM)) +
  geom_errorbar(aes(ymin=lower, ymax=upper, colour = NULL, group = Type_EM),
                width=.03, position = position_dodge(0.03)) +
  geom_line(position = position_dodge(0.03), stat = "identity") +
  geom_point(position = position_dodge(0.03), size=2) +
  labs(y = "Rand Index", x = "Covariance diagonals", colour = "Type of EM\nalgorithm") +
  #xlim(c(0,as.character(unique(rand_mean$Variance_diag)))) +
  ggtitle("Clustering accuracy with increasing variance") +
  scale_color_brewer(palette="Dark2")+
  theme(plot.title = element_text(hjust = 0.5))

