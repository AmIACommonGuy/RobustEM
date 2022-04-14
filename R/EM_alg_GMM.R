
#' Robust EM Algorithm for Gaussian Mixture Models.
#'
#' Given the prespecified number of clusters,
#' the algorithm will calculate the mean and covariance of each cluster. For complete examples
#' check out the documentation for robustEM.
#'
#' @param sampleMat
#' All the points in a matrix
#'
#' @param cluster
#' Number of clusters
#'
#' @param lambda
#' Regularization Parameter
#'
#' @param inits
#' List of initial values, include:
#'
#' mean (a c*d matrix)
#'
#' Covariance (a list with c matrices of dimension d*d)
#'
#' proportion of clusters (a vector with c elements adding up to one)
#'
#' Robust: if set to false, standardard EM will be performed
#'
#'
#' @return
#' A list of results, include:
#'
#' Updated mean vectors for each cluster
#'
#' Updated covariances for each cluster
#'
#' Probability of each point in each cluster(a cluster*n matrix)
#'
#' Number of clusters (cluster)
#'
#' Dimension (d)
#'
#' Number of points (n)
#'
#'
#' @export
#'
#'
EM_alg_GMM = function(sampleMat, cluster, lambda = 10, inits, Robust = T) {
  ## Observations
  x = sampleMat
  n = nrow(x) # number of observations
  d = ncol(x) # number of dimensions

  tau = matrix(inits[[3]], cluster, 1) # initial cluster proportion
  mu = inits[[1]] # initial mean
  sigma = inits[[2]] # initial covariance

  T_mat = matrix(0, cluster, n) # probability of each point in each cluster
  max_it = 200

  for (l in unique(c(Inf, lambda ^ (5:1)))) {
    old_mu = mu

    for (it in 1:max_it) {
      old_T_mat = T_mat

      # E STEP: Construct the vector of the denom for T_mat for each observation i
      for (j in 1:cluster) {
        if (det(sigma[[j]]) < 1e-7) {
          sigma[[j]] = diag(d) * 1e-1
        }

        # Cholesky decomposition to simplify the calculation
        L = chol(sigma[[j]], pivot = TRUE)
        y = solve(t(L), t(x - rep(mu[j,], each = n)))
        temp = abs(prod(diag(L)))
        T_mat[j,] = log(tau[j]) - 0.5 * colSums(y ^ 2) - log(sqrt((2 * pi)^d) * temp)
      }
      # Normalize the probability. Set a arbitrary cluster as the reference

      dif = exp(T_mat-rep(1,cluster)%*%t(as.matrix(apply(T_mat,2,max))))
      T_mat = sweep(dif,2,colSums(dif),'/')

      # M STEP: Update tau, mu and sigma

      # Update tau
      for (j in 1:cluster) {
        e = matrix(0, n, d)
        tau[j,] = (1 / n) * sum(T_mat[j,])

        x1 = x - rep(mu[j, ], each = n)
        sigma_inv = chol2inv(as.matrix(sigma[[j]]))

        x1_sigma_inv = x1 %*% sigma_inv
        A = matrix(rowSums(x1_sigma_inv * x1),n,1)
        indices <- c()
        if (Robust) {
          for (i in 1:n) {
            if (A[i,] < l) {
              e[i,] = 0
              indices <- c(indices, i)
            }
            else {
              e[i,] = x[i,] - mu[j,]
            }
          }
        }
        # Only use those points with error 0 to calc to the covariance martix
        if (length(indices) > 10) {
          Tjind = T_mat[j, indices]
          sT = sum(Tjind)
          xind = x[indices, ]
          eind = e[indices, ]
          # Update mu
          mu[j,] = colSums(Tjind %*% (xind - eind)) / sT

          # Update sigma
          num = matrix(0, d, d)
          denom = sT
          x_prime = xind - eind - matrix(
            mu[j,],
            ncol = d,
            nrow = length(indices),
            byrow = T
          )
          num = t(x_prime) %*% diag(Tjind) %*% x_prime
          sigma[[j]] = matrix(num / denom, d, d)
        }

        if (sum(T_mat[j,] != 0) <= 3) {
          mu[j,] = matrix(0, 1, d)
          sigma[[j]] = diag(d) * 1e-2
        }
      }
      if (max(abs(old_T_mat - T_mat)) < 1e-10)
        break
    }
    if (max(abs(old_mu - mu)) < 1e-6)
      break
  }
  label = apply(T_mat,2,which.max)
  returnList = list(mu, sigma, T_mat, tau, cluster, d, n, x,label)
  names(returnList) = c("mu", "sigma", "T_mat", "tau", "cluster", "d", "n", "raw","label")
  return(returnList)
}
