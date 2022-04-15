## for fit, it needs to go through the functions EM_alg_GMM_stand or EM_alg_GMM_robust
## or at least have the same output
## The true should JUST be a vector of the true class

library(fossil)

calc_rand_ind = function(true, fit) {
  n = length(true)
  # Get the soft assignment for each fit
  Soft_assign = t(fit$T_mat)
  #Soft_assign2 = t(fit2$T_mat)

  # Now we need to work towards getting their hard assignment
  ## (aka which class they were clustered into)
  colnames(Soft_assign) = c(1:c)
  # Assign the cluster number to whichever one has the highest probability
  hard_assign = as.numeric(matrix(paste(apply(Soft_assign, 1, which.max)), n, 1))

  rand_ind = rand.index(true, hard_assign)

  return(rand_ind)

}
