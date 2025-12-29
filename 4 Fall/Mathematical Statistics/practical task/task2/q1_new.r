## Q1: mixture 0.4 * N(1, 3^2) + 0.6 * Laplace(2, b = 1/3)

set.seed(42)
n  <- 10000   # 稍大一点方便看时间
p1 <- 0.4
p2 <- 0.6

## ---------- Laplace 相关 ----------
dlap <- function(x, beta = 2, b = 1/3) (1/(2*b)) * exp(-abs(x - beta) / b)
plap <- function(x, beta = 2, b = 1/3){
  y <- x - beta
  ifelse(y < 0, 0.5 * exp(y/b), 1 - 0.5 * exp(-y/b))
}
qlap <- function(u, beta = 2, b = 1/3){
  ifelse(u < 0.5, beta + b * log(2*u), beta - b * log(2*(1-u)))
}

## ---------- 理论矩 ----------
mu1 <- 1;    var1 <- 3^2
mu2 <- 2;    var2 <- 2*(1/3)^2
mu_th  <- p1*mu1 + p2*mu2
var_th <- p1*(var1 + mu1^2) + p2*(var2 + mu2^2) - mu_th^2
cat(sprintf("Q1 theoretical: mean = %.6f, var = %.6f\n\n", mu_th, var_th))

## ----------------------------------------------------------
## Method 1: 逐样本分类 + 每个样本再抽一次 U
## ----------------------------------------------------------
q1_method1_loop <- function(n, p1, p2){
  x  <- numeric(n)
  n1 <- n2 <- 0L

  t <- system.time({
    for (i in 1:n) {
      ## 选分量
      u_comp <- runif(1)
      ## 再抽一次 U 做子分布采样
      if (u_comp < p1) {
        n1 <- n1 + 1L
        u2 <- runif(1)
        x[i] <- qnorm(u2, mean = 1, sd = 3)
      } else {
        n2 <- n2 + 1L
        u2 <- runif(1)
        x[i] <- qlap(u2, beta = 2, b = 1/3)
      }
    }
  })

  list(
    x    = x,
    n1   = n1, n2 = n2,
    mean = mean(x),
    var  = var(x),
    time = t["elapsed"]
  )
}

## ----------------------------------------------------------
## Method 2: 每个样本一个 U，既判段又做条件逆变换
##          U < p1        -> 段1, 用 U/p1 进 F1^{-1}
##          U >= p1       -> 段2, 用 (U-p1)/p2 进 F2^{-1}
## ----------------------------------------------------------
q1_method2_loop <- function(n, p1, p2){
  x  <- numeric(n)
  n1 <- n2 <- 0L

  t <- system.time({
    for (i in 1:n) {
      u <- runif(1)   # 唯一一次 RNG

      if (u < p1) {
        n1 <- n1 + 1L
        u1 <- u / p1
        x[i] <- qnorm(u1, mean = 1, sd = 3)
      } else {
        n2 <- n2 + 1L
        u2 <- (u - p1) / p2
        x[i] <- qlap(u2, beta = 2, b = 1/3)
      }
    }
  })

  list(
    x    = x,
    n1   = n1, n2 = n2,
    mean = mean(x),
    var  = var(x),
    time = t["elapsed"]
  )
}

## ------------------ 跑一遍比较 ------------------
res1 <- q1_method1_loop(n, p1, p2)
res2 <- q1_method2_loop(n, p1, p2)

cat("=== Q1 Method 1: per-sample cat + inner U ===\n")
cat(sprintf("N1=%d, N2=%d\n", res1$n1, res1$n2))
cat(sprintf("mean=%.6f, var=%.6f, time=%.3fs\n\n", res1$mean, res1$var, res1$time))

cat("=== Q1 Method 2: one U for both component & inverse ===\n")
cat(sprintf("N1=%d, N2=%d\n", res2$n1, res2$n2))
cat(sprintf("mean=%.6f, var=%.6f, time=%.3fs\n\n", res2$mean, res2$var, res2$time))
