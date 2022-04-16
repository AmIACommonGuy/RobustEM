set.seed(234)
sim_info <- simMultGauss(n = 120, d = 2, cluster = 6, out_perc = 0.03, out_mag = 4)

result_robust <- robustEM(sim_info[["simdata"]], cluster = 6,Robust = T)
result_std <- robustEM(sim_info[["simdata"]], cluster = 6,Robust = F)





x <- result_robust



raw_data <- x$raw # raw data

Axis1 <- NULL
Axis2 <- NULL

meanvec <- as.data.frame(x$mu) # mean vector
colnames(meanvec) <- c("Axis1", "Axis2")

covars <- x$sigma # covariance matrices

cluster_result <- data.frame(raw_data)
colnames(cluster_result) <- c("Axis1", "Axis2")

plot.new()
ellipses <- lapply(1:nrow(meanvec), function(j) ellipse(mu=meanvec[j,],
                                                        sigma=covars[[j]], npoints = 300, newplot=F))

covar_plot_f_EM <- function(j){
  boundary <- data.frame(ellipses[[j]])
  colnames(boundary) <- c("Axis1", "Axis2")
  return(geom_path(data = boundary, aes(x=Axis1, y=Axis2), size=0.5, color = 'red'))
}

covar_EM_plots <- lapply(1:nrow(x$mu), covar_plot_f_EM)

final_plot <- ggplot() +
  geom_point(data = cluster_result, aes(x=Axis1, y=Axis2), size = 0.8, alpha = 0.5) +
  geom_point(data = meanvec, aes(x=Axis1, y=Axis2), shape=23, size = 3, stroke = 2, color = 'red')
+covar_EM_plots

return(final_plot)
