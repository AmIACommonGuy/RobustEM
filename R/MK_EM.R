MK_EM = function(sampleMat, cluster, lambda = 10, inits) {

  ## Observations
  x = sampleMat
  n = nrow(x) # number of observations
  d = ncol(x) # number of dimensions
  tau = matrix(inits[[3]], cluster, 1) # initial cluster proportion
  mu = inits[[1]] # initial mean
  sigma = inits[[2]] # initial covariance
  T_mat = matrix(0,cluster,n) # probability of each point in each cluster
  max_it = 200 # Max iteration

  for (l in unique(c(Inf, lambda^(5:1)))) { # l: threshold for outliers
    old_mu = mu # keep it for termination condition
    for (it in 1:max_it) {
      old_T_mat = T_mat # keep it for termination condition

      # E STEP: Construct the the cuurent T_mat for each observation
      for (j in 1:cluster) {
        if (det(sigma[[j]]) < 1e-7) {
          sigma[[j]] = diag(d) * 1e-1
        }
        L = chol(sigma[[j]], pivot = TRUE)
        y = solve(t(L), t(x - rep(mu[j,],each=n)))
        T_mat[j,] = log(tau[j]) - 0.5 * colSums(y^2) - log(sqrt(2*pi)*abs(prod(diag(L)))) # last term needs to be cleared up
      }

      for (i in 1:n) {
        scale = max(T_mat[,i])
        normalizer <- sum(exp(T_mat[,i] - scale))
        T_mat[,i] = exp(T_mat[,i] - scale) / normalizer
      }

      # M STEP: Update tau, mu and sigma

      # Update tau
      for (j in 1:cluster) {
        e = matrix(0, n, d)
        tau[j,] = (1/n)*sum(T_mat[j,])

        x1 = x - rep(mu[j, ], each = n)
        sigma_inv = solve(as.matrix(sigma[[j]]))

        x1_sigma_inv = x1 %*% sigma_inv
        A = matrix(rowSums(x1_sigma_inv * x1), n, 1)

        indices <- c()

        for (i in 1:n) {
          if (A[i,] < l) {
            e[i,] = 0
            indices <- c(indices, i)
          }
          else {
            e[i,] = x[i,] - mu[j,]
          }
        }

        # Only use those points with error 0 to calc to the covariance matrix
        if (length(indices) > 10){
          # Update mu
          mu[j,] = colSums(T_mat[j, indices]%*%(x[indices, ] - e[indices, ])) / sum(T_mat[j, indices])
          # Update sigma
          num = matrix(0,d,d)
          denom = sum(T_mat[j,indices])
          x_prime = x[indices,]-e[indices,]-matrix(mu[j,], ncol=d, nrow=length(indices), byrow=T)
          num = t(x_prime)%*%diag(T_mat[j,indices])%*%x_prime
          sigma[[j]] = matrix(num/denom, d, d)
        }

        if (sum(T_mat[j,]!=0) <= 3) {
          mu[j,] = matrix(0, 1, d)
          sigma[[j]] = diag(d) * 1e-2
        }
      }
      if (max(abs(old_T_mat - T_mat)) < 1e-10) break
    }
    if(max(abs(old_mu - mu)) < 1e-6) break
  }

  returnList = list(mu, sigma, T_mat, tau, cluster, d, n, x)
  names(returnList) = c("mu", "sigma", "T_mat", "tau", "cluster", "d", "n", "raw")
  return(returnList)
}
