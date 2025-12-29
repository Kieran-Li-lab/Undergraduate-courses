## Q2: f(x) = a(1 - x^2) on [-1,1], 0.5*exp(-(x-2)) on [2,inf), 0 else
set.seed(42)
n  <- 10000
a  <- 3/8
p1 <- 0.5   # mass on [-1,1]
p2 <- 0.5   # mass on [2, inf)

## ---------- full pdf / cdf ----------
pdf_full <- function(x){
  y <- numeric(length(x))
  y[x >= -1 & x <= 1] <- a*(1 - x[x >= -1 & x <= 1]^2)
  y[x >= 2]          <- 0.5 * exp(-(x[x >= 2] - 2))
  y
}
cdf_full <- function(x){
  res <- numeric(length(x))
  F1  <- function(xx) (3/8)*(xx - xx^3/3 + 2/3)
  res[x < -1] <- 0
  res[x >= -1 & x <= 1] <- F1(x[x >= -1 & x <= 1])
  res[x > 1 & x < 2] <- 0.5
  res[x >= 2] <- 1 - 0.5*exp(-(x[x >= 2] - 2))
  res
}

## ---------- conditional CDF / inverse on [-1,1] ----------
## density: (3/4)(1 - x^2), CDF F1_cond(x) = (3/4)(x - x^3/3 + 2/3)
F1_cond <- function(x) (3/4)*(x - x^3/3 + 2/3)
q1_cond <- function(u){
  sapply(u, function(uu)
    uniroot(function(x) F1_cond(x) - uu, c(-1,1), tol = 1e-10)$root)
}
r_sub1 <- function(m) q1_cond(runif(m))

## ---------- conditional inverse on [2,inf): exp(-(x-2)) ----------
q2_cond <- function(u) 2 - log(1 - u)
r_sub2 <- function(m) q2_cond(runif(m))

## ---------- theoretical moments ----------
mu1 <- 0
Ex2_1 <- integrate(function(t) t^2*(3/4)*(1 - t^2), -1, 1)$value
var1 <- Ex2_1
mu2 <- 3
var2 <- 1
mu_th  <- p1*mu1 + p2*mu2
var_th <- p1*(var1 + mu1^2) + p2*(var2 + mu2^2) - mu_th^2

## =====================================================================
## Method 1: per-sample categorical classification (sample)
## =====================================================================
t1 <- system.time({
  ind1 <- sample(1:2, size = n, replace = TRUE, prob = c(p1, p2))
  n1   <- sum(ind1 == 1)
  n2   <- sum(ind1 == 2)

  x1 <- numeric(n)
  x1[ind1 == 1] <- r_sub1(n1)
  x1[ind1 == 2] <- r_sub2(n2)

  m1  <- mean(x1)
  v1e <- var(x1)
})
cat(sprintf("Q2-Method1(per-sample cat): N1=%d, N2=%d, time=%.3fs\n", n1, n2, t1["elapsed"]))

## =====================================================================
## Method 2: U ~ Unif[0,1], threshold + reuse U in conditional inverse
## =====================================================================
t2 <- system.time({
  U  <- runif(n)
  x2 <- numeric(n)

  idx1 <- (U < p1)
  idx2 <- !idx1

  if (any(idx1)) {
    u1 <- U[idx1] / p1                      # [0,p1) -> [0,1]
    x2[idx1] <- q1_cond(u1)
  }
  if (any(idx2)) {
    u2 <- (U[idx2] - p1) / p2               # [p1,1] -> [0,1]
    x2[idx2] <- q2_cond(u2)
  }

  n1b <- sum(idx1); n2b <- sum(idx2)
  m2  <- mean(x2)
  v2e <- var(x2)
})
cat(sprintf("Q2-Method2(U->Fi^-1): N1=%d, N2=%d, time=%.3fs\n\n", n1b, n2b, t2["elapsed"]))

## ---------- print comparison ----------
cat("=== Q2: Theoretical vs Empirical ===\n")
cat(sprintf("Theory : mean=%.6f  var=%.6f\n", mu_th, var_th))
cat(sprintf("Method1: mean=%.6f  var=%.6f\n", m1, v1e))
cat(sprintf("Method2: mean=%.6f  var=%.6f\n\n", m2, v2e))

## ---------- plots ----------
png("q2_density_compare.png", 1600, 800, res = 150)
par(mfrow = c(1,2), mar = c(4,4,3,1))
hist(x1, breaks = "FD", freq = FALSE, col = "grey85", border = "white",
     main = "Q2 — Method 1: Density", xlab = "x")
curve(pdf_full(x), add = TRUE, lwd = 2)

hist(x2, breaks = "FD", freq = FALSE, col = "lightblue", border = "white",
     main = "Q2 — Method 2: Density", xlab = "x")
curve(pdf_full(x), add = TRUE, lwd = 2)
dev.off()

png("q2_cdf_compare.png", 1600, 800, res = 150)
par(mfrow = c(1,2), mar = c(4,4,3,1))
plot(ecdf(x1), main = "Q2 — Method 1: ECDF vs CDF", xlab = "x", ylab = "F", col = "blue")
curve(cdf_full(x), add = TRUE, lwd = 2, lty = 2)

plot(ecdf(x2), main = "Q2 — Method 2: ECDF vs CDF", xlab = "x", ylab = "F", col = "blue")
curve(cdf_full(x), add = TRUE, lwd = 2, lty = 2)
dev.off()
