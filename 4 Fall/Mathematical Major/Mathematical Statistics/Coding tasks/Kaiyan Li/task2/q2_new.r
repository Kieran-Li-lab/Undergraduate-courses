## Q2: f(x) = a(1 - x^2) on [-1,1], 0.5*exp(-(x-2)) on [2,inf), 0 else

set.seed(42)
n  <- 20000     # Q2 有 uniroot，比 Q1 稍慢，n 别太夸张
a  <- 3/8
p1 <- 0.5       # mass on [-1,1]
p2 <- 0.5       # mass on [2, inf)

## ---------- 条件密度 / CDF on [-1,1] ----------
## sub-pdf1: f1_cond(x) = (3/4)(1 - x^2) on [-1,1]
F1_cond <- function(x) (3/4)*(x - x^3/3 + 2/3)

## 通过 uniroot 做逆 CDF: 给定 u ∈ (0,1)，解 F1_cond(x) = u
q1_cond <- function(u){
  sapply(u, function(uu)
    uniroot(function(x) F1_cond(x) - uu, c(-1, 1), tol = 1e-10)$root
  )
}

## ---------- sub-pdf2: exp(-(x-2)) on [2,inf) ----------
q2_cond <- function(u) 2 - log(1 - u)

## ---------- 理论矩 ----------
mu1 <- 0
Ex2_1 <- integrate(function(t) t^2*(3/4)*(1 - t^2), -1, 1)$value
var1 <- Ex2_1
mu2 <- 3
var2 <- 1
mu_th  <- p1*mu1 + p2*mu2
var_th <- p1*(var1 + mu1^2) + p2*(var2 + mu2^2) - mu_th^2
cat(sprintf("Q2 theoretical: mean = %.6f, var = %.6f\n\n", mu_th, var_th))

## ----------------------------------------------------------
## Method 1: 逐样本分类 + 每个样本再抽一次 U
## ----------------------------------------------------------
q2_method1_loop <- function(n, p1, p2){
  x  <- numeric(n)
  n1 <- n2 <- 0L

  t <- system.time({
    for (i in 1:n) {
      ## 选分量
      u_comp <- runif(1)

      if (u_comp < p1) {
        n1 <- n1 + 1L
        u2 <- runif(1)
        x[i] <- q1_cond(u2)
      } else {
        n2 <- n2 + 1L
        u2 <- runif(1)
        x[i] <- q2_cond(u2)
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
##          U < p1  -> 段1, 用 U/p1 进 F1^{-1}
##          U >= p1 -> 段2, 用 (U-p1)/p2 进 F2^{-1}
## ----------------------------------------------------------
q2_method2_loop <- function(n, p1, p2){
  x  <- numeric(n)
  n1 <- n2 <- 0L

  t <- system.time({
    for (i in 1:n) {
      u <- runif(1)   # 唯一一次 RNG

      if (u < p1) {
        n1 <- n1 + 1L
        u1 <- u / p1
        x[i] <- q1_cond(u1)
      } else {
        n2 <- n2 + 1L
        u2 <- (u - p1) / p2
        x[i] <- q2_cond(u2)
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
res1 <- q2_method1_loop(n, p1, p2)
res2 <- q2_method2_loop(n, p1, p2)

cat("=== Q2 Method 1: per-sample cat + inner U ===\n")
cat(sprintf("N1=%d, N2=%d\n", res1$n1, res1$n2))
cat(sprintf("mean=%.6f, var=%.6f, time=%.3fs\n\n", res1$mean, res1$var, res1$time))

cat("=== Q2 Method 2: one U for both component & inverse ===\n")
cat(sprintf("N1=%d, N2=%d\n", res2$n1, res2$n2))
cat(sprintf("mean=%.6f, var=%.6f, time=%.3fs\n\n", res2$mean, res2$var, res2$time))
