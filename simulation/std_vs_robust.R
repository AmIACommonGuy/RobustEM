n = 120
d = 2
cluster = 3
set.seed(22)
sim_info <- simMultGauss(n = 120, d = 2, cluster = cluster, out_perc = 0.2, out_mag = 4)
result_R <- robustEM(sim_info[["simdata"]], cluster = cluster,Robust = T)
result_S <- robustEM(sim_info[["simdata"]], cluster = cluster,Robust = F)
accuracy <- function(result) {
  l = result$label
  err = sum(abs(table(l)-rep(n,cluster)))
  return ((n*d-err)/(n*d))
}
plot(result_R)
plot(result_S)


## accuracy in terms of out_perc
r = c()
s = c()
cc = 10
for (i in 1:cc){
  set.seed(22)
  sim_info <- simMultGauss(n = 120, d = 3, cluster = cluster, out_perc = 0.01*i, out_mag = 4)
  result_R <- robustEM(sim_info[["simdata"]], cluster = cluster,Robust = T)
  result_S <- robustEM(sim_info[["simdata"]], cluster = cluster,Robust = F)
  r <- c(r,accuracy(result_R))
  s <- c(s,accuracy(result_S))
}

plot(1:cc,r)
plot(1:cc,s)


## accuracy in terms of magnitude
r = c()
s = c()
cc = 20
for (i in 1:cc){
  set.seed(22)
  sim_info <- simMultGauss(n = n, d = d, cluster = cluster, out_perc = 0.05, out_mag = i)
  result_R <- robustEM(sim_info[["simdata"]], cluster = cluster,Robust = T)
  result_S <- robustEM(sim_info[["simdata"]], cluster = cluster,Robust = F)
  r <- c(r,accuracy(result_R))
  s <- c(s,accuracy(result_S))
}

plot(1:cc,r)
plot(1:cc,s)

r = c()
s = c()
cc = 20
for (i in 1:cc){
  set.seed(22)
  sim_info <- simMultGauss(n = n, d = d, cluster = cluster, out_perc = 0.05, out_mag = i)
  result_R <- robustEM(sim_info[["simdata"]], cluster = cluster,Robust = T)
  result_S <- robustEM(sim_info[["simdata"]], cluster = cluster,Robust = F)
  r <- c(r,accuracy(result_R))
  s <- c(s,accuracy(result_S))
}




