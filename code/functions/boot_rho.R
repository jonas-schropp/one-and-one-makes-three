#' Bootstrap and format correlations
#' 
#' @param data a data.frame
#' @param x a numeric variable
#' @param y a numeric variable
#' @param n number of bootstrap repetitions
#' @param method one of 'kendall', 'spearman' or 'pearson'
#' 
#' @returns 
#' A formatted string with rho, 95% CI and p value
#' 
boot_rho <- function(data, x, y, n = 1000, method = 'kendall') {
  
  require(boot)
  require(boot.pval)
  
  set.seed(101)
  
  data <- data[,c(x, y)]
  
  boot_fun <- function(data, indices, method) {
    cor(x = data[indices, ], method = method)[2]
  }
  
  boot_res <- boot(
    data = data, 
    statistic = boot_fun, 
    R = n,
    method = method
  )
  
  ci <- boot.ci(
    boot.out = boot_res, 
    conf = 0.95, 
    type = "bca"
  )
  
  p <- boot.pval(
    boot_res = boot_res
  ) |>
    round(3)
  
  p <- ifelse(p == 0, '<.001', substr(p, 2, 5))
  
  res <- paste0(
    'rho=', round(boot_res$t0, 3), '; ',
    '95% CI=[', round(ci$bca, 3)[4], '-', round(ci$bca, 3)[5], ']; ',
    'p=', p
  )
  
  return(res)
  
}