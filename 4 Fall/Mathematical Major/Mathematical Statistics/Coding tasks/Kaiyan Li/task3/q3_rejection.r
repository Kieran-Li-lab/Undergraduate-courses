## q3_rejection_loop.R

set.seed(42)
n  <- 100000
p1 <- 0.25
p2 <- 0.25
p3 <- 0.5

## ---------- 目标 pdf & CDF ----------
pdf_q3 <- function(x){
  y <- numeric(length(x))
  y[x >= -1 & x < 1] <- (x[x >= -1 & x < 1] + 1)/8
  y[x >=  1 & x < 2] <- 1/4
  y[x >=  2]        <- 0.5 * exp(-(x[x >= 2] - 2))
  y
}

cdf_q3 <- function(x){
  y <- numeric(length(x))
  y[x < -1] <- 0
  idx1 <- (x >= -1 & x < 1)
  y[idx1] <- (x[idx1] + 1)^2 / 16
  idx2 <- (x >= 1 & x < 2)
  y[idx2] <- x[idx2] / 4
  idx3 <- (x >= 2)
  y[idx3] <- 1 - 0.5 * exp(-(x[idx3] - 2))
  y
}

## ---------- 理论矩 ----------
mu1 <- 1/3; var1 <- 2/9
mu2 <- 1.5; var2 <- 1/12
mu3 <- 3;   var3 <- 1

mu_th  <- p1*mu1 + p2*mu2 + p3*mu3
var_th <- p1*(var1 + mu1^2) + p2*(var2 + mu2^2) + p3*(var3 + mu3^2) - mu_th^2
cat(sprintf("Q3 理论: mean = %.6f, var = %.6f\n\n", mu_th, var_th))

## ----------------------------------------------------------
## Method 1: 逐样本分类 + 每个样本再抽一次 U
## ----------------------------------------------------------
q3_method1_loop <- function(n, p1, p2, p3){
  p12 <- p1 + p2
  x   <- numeric(n)
  n1 <- n2 <- n3 <- 0L

  t <- system.time({
    for (i in 1:n) {
      u_comp <- runif(1)
      if (u_comp < p1) {
        n1 <- n1 + 1L
        u2 <- runif(1)
        x[i] <- -1 + 2*sqrt(u2)         # seg1
      } else if (u_comp < p12) {
        n2 <- n2 + 1L
        u2 <- runif(1)
        x[i] <- 1 + u2                  # seg2
      } else {
        n3 <- n3 + 1L
        u2 <- runif(1)
        x[i] <- 2 - log(1 - u2)         # seg3
      }
    }
  })

  list(x = x, n1 = n1, n2 = n2, n3 = n3,
       mean = mean(x), var = var(x),
       time = t["elapsed"])
}

## ----------------------------------------------------------
## Method 2: 每个样本一个 U (复用)
## ----------------------------------------------------------
q3_method2_loop <- function(n, p1, p2, p3){
  p12 <- p1 + p2
  x   <- numeric(n)
  n1 <- n2 <- n3 <- 0L

  t <- system.time({
    for (i in 1:n) {
      u <- runif(1)
      if (u < p1) {
        n1 <- n1 + 1L
        u1 <- u / p1
        x[i] <- -1 + 2*sqrt(u1)
      } else if (u < p12) {
        n2 <- n2 + 1L
        u2 <- (u - p1)/p2
        x[i] <- 1 + u2
      } else {
        n3 <- n3 + 1L
        u3 <- (u - p12)/p3
        x[i] <- 2 - log(1 - u3)
      }
    }
  })

  list(x = x, n1 = n1, n2 = n2, n3 = n3,
       mean = mean(x), var = var(x),
       time = t["elapsed"])
}

## ----------------------------------------------------------
## Method 3: Rejection Sampling
## ----- Q3: g(x) = 0.2*Unif[-1,2] + 0.8*(2+Exp(1)) -----

g_pdf_q3 <- function(x){

  y <- numeric(length(x))
  idx12 <- (x >= -1) & (x < 2)
  y[idx12] <- 1/4
  idx3 <- (x >= 2)
  y[idx3] <- 1/4 + 0.25 * exp(-(x[idx3] - 2))
  y

}

r_g_q3 <- function(m){

  z <- numeric(m)
  for (i in 1:m) {
    u_comp <- runif(1)
    if (u_comp < 0.75) { # 新权重 w1 = 0.75
      z[i] <- runif(1, -1, 2) # from Unif[-1,2]
    } else {
      z[i] <- 2 - log(1 - runif(1)) # from 2 + Exp(1)
    }
  }
  z
}

q3_rejection <- function(n, M = 2){
  x_acc <- numeric(n)
  k <- 0L            # 已接受的样本数
  trial <- 0L        # 新增：总尝试次数

  t <- system.time({
    while (k < n) {
      trial <- trial + 1L # 新增：每次尝试，计数器加 1

      X <- r_g_q3(1)
      U <- runif(1)
      fx <- pdf_q3(X)
      gx <- g_pdf_q3(X)
      
      # 严谨性修正：避免 gx = 0 导致 NaN/Inf 错误
      if (gx > 0) {
        if (U <= fx / (M * gx)) {
          k <- k + 1L
          x_acc[k] <- X
        }
      }
    }
  })

  accept_rate <- n / trial # 新增：计算接受率

  list(x = x_acc,
       mean = mean(x_acc), 
       var = var(x_acc),
       time = t["elapsed"],
       total_trial = trial,     # 新增：返回总尝试次数
       accept_rate = accept_rate # 新增：返回接受率
  )
}

## ------------------ 跑一遍比较 ------------------
## ------------------ 跑一遍比较 ------------------
res1 <- q3_method1_loop(n, p1, p2, p3)
res2 <- q3_method2_loop(n, p1, p2, p3)
res3 <- q3_rejection(n, M = 1.0)

cat("=== Q3: Theoretical vs 3 methods ===\n")
q3_summary <- data.frame(
  Method = c("Theoretical", "Method1 (per-sample cat)", 
             "Method2 (one U)", "Rejection"),
  Mean   = round(c(mu_th, res1$mean, res2$mean, res3$mean), 6),
  Var    = round(c(var_th, res1$var,  res2$var,  res3$var),  6),
  Time_s = c(NA,
             round(res1$time, 4),
             round(res2$time, 4),
             round(res3$time, 4))
)

print(q3_summary, row.names = FALSE)
print(res3[c("total_trial","accept_rate")])

cat("\nComponent counts (empirical probabilities):\n")
q3_counts <- data.frame(
  Method = c("Method1", "Method2"),
  N1     = c(res1$n1, res2$n1),
  N2     = c(res1$n2, res2$n2),
  N3     = c(res1$n3, res2$n3),
  p1_hat = round(c(res1$n1, res2$n1)/n, 4),
  p2_hat = round(c(res1$n2, res2$n2)/n, 4),
  p3_hat = round(c(res1$n3, res2$n3)/n, 4)
)
print(q3_counts, row.names = FALSE)


## ---------- 绘图 ----------
xgrid <- seq(-2, 10, length.out = 1000)
theo_pdf <- pdf_q3(xgrid)
theo_cdf <- cdf_q3(xgrid)

png("Q3_pdf_compare.png", 1600, 800, res = 150)
par(mfrow = c(1,1), mar = c(4,4,3,1))
plot(xgrid, theo_pdf, type = "l", lwd = 2, col = "black",
     xlab = "x", ylab = "density", main = "Q3: PDF comparison")
lines(density(res1$x), col = "red")
lines(density(res2$x), col = "blue")
lines(density(res3$x), col = "darkgreen")
legend("topright",
       legend = c("Theoretical", "Method1", "Method2", "Rejection"),
       col = c("black","red","blue","darkgreen"), lwd = 2, bty = "n")
dev.off()

png("Q3_cdf_compare.png", 1600, 800, res = 150)
par(mfrow = c(1,1), mar = c(4,4,3,1))
plot(xgrid, theo_cdf, type = "l", lwd = 2, col = "black",
     xlab = "x", ylab = "CDF", main = "Q3: CDF comparison")
lines(ecdf(res1$x), col = "red")
lines(ecdf(res2$x), col = "blue")
lines(ecdf(res3$x), col = "darkgreen")
legend("bottomright",
       legend = c("Theoretical", "Method1", "Method2", "Rejection"),
       col = c("black","red","blue","darkgreen"), lwd = 2, bty = "n")
dev.off()
