set.seed(42)
n  <- 10000   # 稍大一点方便看时间差
p1 <- 1/4
p2 <- 1/4
p3 <- 1/2
p12 <- p1 + p2

## --------- 理论量（均值、方差） ----------
mu1 <- 1/3; var1 <- 2/9
mu2 <- 1.5; var2 <- 1/12
mu3 <- 3;   var3 <- 1
mu_th  <- p1*mu1 + p2*mu2 + p3*mu3
var_th <- p1*(var1 + mu1^2) + p2*(var2 + mu2^2) + p3*(var3 + mu3^2) - mu_th^2

cat(sprintf("Q3 theoretical: mean = %.6f, var = %.6f\n\n", mu_th, var_th))

## ----------------------------------------------------------
## Method 1: 逐样本 Bernoulli / 多项式 + 子分布再抽一次 U
## ----------------------------------------------------------
q3_method1_loop <- function(n, p1, p2, p3){
  p12 <- p1 + p2
  x   <- numeric(n)
  n1 <- n2 <- n3 <- 0L

  t <- system.time({
    for (i in 1:n) {
      ## 第一步：用一个 U1 选分量（对应讲义里的 Bernoulli / Uniform 方法）
      u_comp <- runif(1)
      ## 第二步：在对应分量里再抽一个 U2 做逆变换
      if (u_comp < p1) {
        n1 <- n1 + 1L
        u2 <- runif(1)
        x[i] <- -1 + 2*sqrt(u2)              # segment 1: [-1,1)
      } else if (u_comp < p12) {
        n2 <- n2 + 1L
        u2 <- runif(1)
        x[i] <- 1 + u2                       # segment 2: [1,2)
      } else {
        n3 <- n3 + 1L
        u2 <- runif(1)
        x[i] <- 2 - log(1 - u2)              # segment 3: [2, inf)
      }
    }
  })

  list(
    x   = x,
    n1  = n1, n2 = n2, n3 = n3,
    mean = mean(x),
    var  = var(x),
    time = t["elapsed"]
  )
}

## ----------------------------------------------------------
## Method 2: 每个样本只生成一个 U，既判段又做条件逆变换
##          U < p1        -> 段1, 用 U/p1 进 F1^{-1}
##          p1<=U<p1+p2   -> 段2, 用 (U-p1)/p2 进 F2^{-1}
##          U>=p1+p2      -> 段3, 用 (U-p1-p2)/p3 进 F3^{-1}
## ----------------------------------------------------------
q3_method2_loop <- function(n, p1, p2, p3){
  p12 <- p1 + p2
  x   <- numeric(n)
  n1 <- n2 <- n3 <- 0L

  t <- system.time({
    for (i in 1:n) {
      u <- runif(1)          # 唯一一次 RNG

      if (u < p1) {
        n1 <- n1 + 1L
        u1 <- u / p1                # [0,p1) -> [0,1]
        x[i] <- -1 + 2*sqrt(u1)
      } else if (u < p12) {
        n2 <- n2 + 1L
        u2 <- (u - p1)/p2           # [p1,p1+p2) -> [0,1]
        x[i] <- 1 + u2
      } else {
        n3 <- n3 + 1L
        u3 <- (u - p12)/p3          # [p1+p2,1] -> [0,1]
        x[i] <- 2 - log(1 - u3)
      }
    }
  })

  list(
    x   = x,
    n1  = n1, n2 = n2, n3 = n3,
    mean = mean(x),
    var  = var(x),
    time = t["elapsed"]
  )
}

## ------------------ 跑一遍比较 ------------------
res1 <- q3_method1_loop(n, p1, p2, p3)
res2 <- q3_method2_loop(n, p1, p2, p3)

cat("=== Q3 Method 1: per-sample Bernoulli + inner U ===\n")
cat(sprintf("N1=%d, N2=%d, N3=%d\n", res1$n1, res1$n2, res1$n3))
cat(sprintf("mean=%.6f, var=%.6f, time=%.4fs\n\n", res1$mean, res1$var, res1$time))

cat("=== Q3 Method 2: one U for both component & inverse ===\n")
cat(sprintf("N1=%d, N2=%d, N3=%d\n", res2$n1, res2$n2, res2$n3))
cat(sprintf("mean=%.6f, var=%.6f, time=%.4fs\n\n", res2$mean, res2$var, res2$time))
