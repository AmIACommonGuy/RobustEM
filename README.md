
<!-- README.md is generated from README.Rmd. Please edit that file -->

# RobustEM

The goal of RobustEM is to carry out clustering on high dimensional
points using expectation-maximization algorithm which is robust against
outliers.

## Installation

To install the package, make sure you have `devtools` package loaded and
type `install_github("AmIACommonGuy/RobustEM-1", ref="main")`.

``` r
# install.packages("devtools")
devtools::install_github("AmIACommonGuy/RobustEM-1")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(RobustEM)
#> Loading required package: mclust
#> Warning: package 'mclust' was built under R version 4.1.2
#> Package 'mclust' version 5.4.9
#> Type 'citation("mclust")' for citing this R package in publications.
```

The example here simulate 720 points belonging to 6 clusters. Each
cluster has 120 points. All the points have two dimensions. All the
clusters have approximately 6% of points as outliers. The outliers are
generated here as having the same mean but a covariance matrix with much
larger elements. This customized function `simMultGauss` not only
generates the data points but also includes detailed cluster information
(mean and covariance of each cluster).

``` r
set.seed(22)
sim_info <- simMultGauss(n = 120, d = 2, cluster = 6, out_perc = 0.03, out_mag = 4)
```

We can use function `robustEM` to cluster the points.

``` r
result <- robustEM(sim_info[["simdata"]], cluster = 6,Robust = T)
```

I have written a customized summary function to summarize the cluster
mean and the number of points in each cluster.

``` r
summary(result)
#> $`Cluster Point Count`
#> point_cluster
#>   1   2   3   4   5   6 
#> 121 119 119 121 120 120 
#> 
#> $`Cluster Mean`
#>            V1       V2
#> [1,] 16.49681 24.26420
#> [2,] 18.45567 14.19157
#> [3,] 35.74641 12.52521
#> [4,] 30.25588 26.08767
#> [5,] 27.99846 41.28102
#> [6,] 48.01971 34.62843
```

``` r
plot(result)
```

<img src="man/figures/README-unnamed-chunk-5-1.png" width="100%" /><img src="man/figures/README-unnamed-chunk-5-2.png" width="100%" />

You’ll still need to render `README.Rmd` regularly, to keep `README.md`
up-to-date. `devtools::build_readme()` is handy for this. You could also
use GitHub Actions to re-render `README.Rmd` every time you push. An
example workflow can be found here:
<https://github.com/r-lib/actions/tree/v1/examples>.
