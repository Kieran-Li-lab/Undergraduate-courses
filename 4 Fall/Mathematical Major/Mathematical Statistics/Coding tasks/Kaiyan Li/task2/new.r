## q2_rejection_loop.R - 修正版

set.seed(42)
n  <- 10000
a  <- 3 / 8
p1 <- 0.5     # mass on [-1,1]
p2 <- 0.5     # mass on [2, inf)

## ---------- 条件密度 / 逆变换 on [-1,1] ----------
## sub-pdf1 (normalized): f1_cond(x) = (3/4)(1 - x^2), x in [-1,1]
F1_cond <- function(x) (3/4)*(x - x^3/3 + 2/3)

q1_cond <- function(u){
  sapply(u, function(uu)
    uniroot(function(x) F1_cond(x) - uu, c(-1, 1), tol = 1e-10)$root
  )
}

## sub-pdf2 (normalized) on [2,inf): exp(-(x-2))
q2_cond <- function(u) 2 - log(1 - u)

## ---------- 目标 pdf & CDF (未修改) ----------
pdf_q2 <- function(x){
  y <- numeric(length(x))
  y[x >= -1 & x <= 1] <- (3/8)*(1 - x[x >= -1 & x <= 1]^2)
  y[x >= 2]          <- 0.5 * exp(-(x[x >= 2] - 2))
  y
}

cdf_q2 <- function(x){
  y <- numeric(length(x))
  idx1 <- (x >= -1 & x < 1)
  y[x < -1] <- 0
  y[idx1]   <- (3/8)*(x[idx1] - x[idx1]^3/3 + 2/3)
  y[x >= 1 & x < 2] <- 0.5
  idx3 <- (x >= 2)
  y[idx3]  <- 1 - 0.5 * exp(-(x[idx3] - 2))
  y
}

## ---------- 理论矩 (未修改) ----------
mu1 <- 0
Ex2_1 <- integrate(function(t) t^2*(3/4)*(1 - t^2), -1, 1)$value
var1 <- Ex2_1
mu2 <- 3
var2 <- 1

mu_th  <- p1*mu1 + p2*mu2
var_th <- p1*(var1 + mu1^2) + p2*(var2 + mu2^2) - mu_th^2
cat(sprintf("Q2 理论: mean = %.6f, var = %.6f\n\n", mu_th, var_th))

## ----------------------------------------------------------
## Method 1 & 2 (未修改)
## ----------------------------------------------------------
q2_method1_loop <- function(n, p1, p2){
  x  <- numeric(n)
  n1 <- n2 <- 0L
  t <- system.time({
    for (i in 1:n) {
      u_comp <- runif(1)
      if (u_comp < p1) {
        n1 <- n1 + 1L; u2 <- runif(1); x[i] <- q1_cond(u2)
      } else {
        n2 <- n2 + 1L; u2 <- runif(1); x[i] <- q2_cond(u2)
      }
    }
  })
  list(x = x, n1 = n1, n2 = n2,
       mean = mean(x), var = var(x),
       time = t["elapsed"])
}

q2_method2_loop <- function(n, p1, p2){
  x  <- numeric(n)
  n1 <- n2 <- 0L
  t <- system.time({
    for (i in 1:n) {
      u <- runif(1)
      if (u < p1) {
        n1 <- n1 + 1L; u1 <- u / p1; x[i] <- q1_cond(u1)
      } else {
        n2 <- n2 + 1L; u2 <- (u - p1)/p2; x[i] <- q2_cond(u2)
      }
    }
  })
  list(x = x, n1 = n1, n2 = n2,
       mean = mean(x), var = var(x),
       time = t["elapsed"])
}


## ----------------------------------------------------------
## Method 3: Rejection Sampling (修正 M 和 r_g_q2)
## ----------------------------------------------------------

## ----------------------------------------------------------
## Method 3: Rejection Sampling (修正为 g(x) != f(x))
## ----- Q2: g(x) = 0.6*Unif[-1,1] + 0.4*(2+Exp(1)) -----
## ----------------------------------------------------------

M <- 1.25  # 修正 M: 严格计算所得的上界 M=1.25

# 建议密度函数 g(x) = 0.6 * Unif[-1,1] + 0.4 * (2+Exp(1))
g_pdf_q2 <- function(x){
  y <- numeric(length(x))
  
  # x in [-1, 1]: 仅包含 Uniform 分量 (0.6 * 1/2 = 0.3)
  idx1 <- (x >= -1) & (x <= 1)
  y[idx1] <- 0.3
  
  # x in [2, inf): 仅包含 Exp 分量 (0.4 * exp(-(x-2)))
  idx2 <- x >= 2
  y[idx2] <- 0.4 * exp(- (x[idx2] - 2))
  
  # 其他区域 y=0
  y
}

# 采样函数 r_g_q2 必须与 g_pdf_q2 匹配
r_g_q2 <- function(m){
  z <- numeric(m)
  for (i in 1:m) {
    u_comp <- runif(1)
    if (u_comp < 0.6) { # 权重 w1 = 0.6
      # 分量 1: Unif[-1,1]
      z[i] <- runif(1, -1, 1)
    } else { # 权重 w2 = 0.4
      # 分量 2: 2 + Exp(1)。复用已定义的逆变换 q2_cond
      z[i] <- q2_cond(runif(1))
    }
  }
  z
}


q2_rejection <- function(n, M = 1.25){
  x_acc <- numeric(n)
  k <- 0L             # 已接受的样本数
  trial <- 0L         # 总尝试次数

  t <- system.time({
    while (k < n) {
      trial <- trial + 1L

      X <- r_g_q2(1)
      U <- runif(1)

      fx <- pdf_q2(X)
      gx <- g_pdf_q2(X)

      if (gx > 0) {
        ratio <- fx / (M * gx)
        if (U <= ratio) {
          k <- k + 1L
          x_acc[k] <- X
        }
      }
    }
  })

  accept_rate <- n / trial

  list(
    x = x_acc,
    mean = mean(x_acc),
    var = var(x_acc),
    time = t["elapsed"],
    total_trial = trial,
    accept_rate = accept_rate
  )
}




## ------------------ 跑一遍比较 (修正 M) ------------------
res1 <- q2_method1_loop(n, p1, p2)
res2 <- q2_method2_loop(n, p1, p2)
res3 <- q2_rejection(n, M)


cat("=== Q2: Theoretical vs 3 methods (M) ===\n")
q2_summary <- data.frame(
  Method = c("Theoretical", "Method1 (per-sample cat)", 
             "Method2 (one U)", "Rejection"),
  Mean   = round(c(mu_th, res1$mean, res2$mean, res3$mean), 6),
  Var    = round(c(var_th, res1$var,  res2$var,  res3$var),  6),
  Time_s = c(NA,
             round(res1$time, 4),
             round(res2$time, 4),
             round(res3$time, 4))
)

print(q2_summary, row.names = FALSE)
print(res3[c("total_trial","accept_rate")])

cat("\nComponent counts (empirical probabilities):\n")
q2_counts <- data.frame(
  Method = c("Method1", "Method2"),
  N1     = c(res1$n1, res2$n1),
  N2     = c(res1$n2, res2$n2),
  p1_hat = round(c(res1$n1, res2$n1)/n, 4),
  p2_hat = round(c(res1$n2, res2$n2)/n, 4)
)
print(q2_counts, row.names = FALSE)

## ---------- 绘图（PDF & CDF） (未修改) ----------
xgrid <- seq(-2, 8, length.out = 1000)
theo_pdf <- pdf_q2(xgrid)
theo_cdf <- cdf_q2(xgrid)

png("Q2_pdf_compare.png", 1600, 800, res = 150)
par(mfrow = c(1,1), mar = c(4,4,3,1))
plot(xgrid, theo_pdf, type = "l", lwd = 2, col = "black",
     xlab = "x", ylab = "density", main = "Q2: PDF comparison")
lines(density(res1$x), col = "red")
lines(density(res2$x), col = "blue")
lines(density(res3$x), col = "darkgreen")
legend("topright",
       legend = c("Theoretical", "Method1", "Method2", "Rejection"),
       col = c("black","red","blue","darkgreen"), lwd = 2, bty = "n")
dev.off()

png("Q2_cdf_compare.png", 1600, 800, res = 150)
par(mfrow = c(1,1), mar = c(4,4,3,1))
plot(xgrid, theo_cdf, type = "l", lwd = 2, col = "black",
     xlab = "x", ylab = "CDF", main = "Q2: CDF comparison")
lines(ecdf(res1$x), col = "red")
lines(ecdf(res2$x), col = "blue")
lines(ecdf(res3$x), col = "darkgreen")
legend("bottomright",
       legend = c("Theoretical", "Method1", "Method2", "Rejection"),
       col = c("black","red","blue","darkgreen"), lwd = 2, bty = "n")
dev.off()