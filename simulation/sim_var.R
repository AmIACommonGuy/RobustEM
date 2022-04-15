setwd("C:/Users/njzmh/OneDrive/Desktop/RobustEM/RobustEM-1/simulation")
source('C:/Users/njzmh/OneDrive/Desktop/RobustEM/RobustEM-1/simulation/simulation_helper.R')

library(MASS)
library(Matrix)
library(ggplot2)
library(mvtnorm)
library(ggpubr)
library(mixtools)
library(RColorBrewer)
library(fossil)
library(dplyr)

num = 10000
set.seed(num)

d = 2
n = 100
mu1 = c(0,0)
mu2 = c(5,0)
sigma = matrix(c(1,0,0,1), d)
sigma_mag1 = 0.1

rand = NULL
i=8
num_sim = 50
c = 2


for (i in 10:15) {
  # Let's do 50 different sets per k

  # Set the total number of clusters
  sigma_mag = i*sigma_mag1
  print(sigma_mag)

  for (j in 1:num_sim) {
    # for each k, we want the repeated samples to be the same
    seed_num = num + j
    print(seed_num)
    #seed_num = 12351
    set.seed(seed_num)

    ################ Create the simulated data #################
    # Create the simulation data
    sampleMat = rbind(mvrnorm(n=n, mu=mu1, Sigma=sigma_mag*sigma),
                      mvrnorm(n=n, mu=mu2, Sigma=sigma_mag*sigma))# changed from 5 to 10
    # plot(sampleMat[,1], sampleMat[,2])

    # Combine the c mu's into one matrix
    #sampleMu = t(rbind(mu1, mu2))

    # Combine the c sigmas into one list
    #sampleSigma = lapply(1:c, function(x) as.matrix(samples[2,][[x]]))

    # Label the sample's clusters
    sampleMat1 = as.data.frame(cbind(sampleMat, rep(1:c, each=100)))
    colnames(sampleMat1) = c("X", "Y", "Cluster")
    sampleMat1$Cluster = as.character(sampleMat1$Cluster)

    result_rem = robustEM(sampleMat, c, Robust = T)
    result_standard = robustEM(sampleMat,c, Robust = F)
    true = as.numeric(sampleMat1$Cluster) ## The true label

    acc_rem = rand.index(true, result_rem$label)
    acc_std = rand.index(true, result_standard$label)

    curr_stand = c(sigma_mag, seed_num, "standard", acc_std)
    curr_rob = c(sigma_mag, seed_num, "robust", acc_rem)

    rand = rbind(rand, curr_stand, curr_rob)
  }
}
# Make it a data frame
## Using dplyr to manipulate the data
rand = as.data.frame(rand)
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




