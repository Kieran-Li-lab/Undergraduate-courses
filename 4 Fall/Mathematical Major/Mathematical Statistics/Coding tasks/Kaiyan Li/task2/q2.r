## ---------- Q2: piecewise; a(1-x^2) on [-1,1], 0.5 e^{-(x-2)} on [2,inf) ----------
set.seed(42)
n <- 100
a  <- 3/8
p1 <- 0.5; p2 <- 0.5

## full pdf/cdf (for plotting)
pdf_full <- function(x){
  y <- numeric(length(x))
  y[x >= -1 & x <= 1] <- a*(1 - x[x >= -1 & x <= 1]^2)
  y[x >= 2]          <- 0.5*exp(-(x[x >= 2]-2))
  y
}
cdf_full <- function(x){
  res <- numeric(length(x))
  F1  <- function(xx) (3/8)*(xx - xx^3/3 + 2/3)
  res[x < -1] <- 0
  res[x >= -1 & x <= 1] <- F1(x[x >= -1 & x <= 1])
  res[x > 1 & x < 2] <- 0.5
  res[x >= 2] <- 1 - 0.5*exp(-(x[x >= 2]-2))
  res
}

## conditional CDF/inverse on [-1,1]: (3/4)(x - x^3/3 + 2/3)
F1_cond <- function(x) (3/4)*(x - x^3/3 + 2/3)
q1_cond <- function(u){
  sapply(u, function(uu)
    uniroot(function(x) F1_cond(x) - uu, c(-1,1), tol=1e-10)$root)
}

## conditional inverse on [2,inf): e^{-(x-2)}
q2_cond <- function(u) 2 - log(1 - u)

## theoretical moments via sub-pdfs
mu1 <- 0
Ex2_1 <- integrate(function(t) t^2*(3/4)*(1 - t^2), -1, 1)$value
var1 <- Ex2_1
mu2 <- 3; var2 <- 1
mu_th  <- p1*mu1 + p2*mu2
var_th <- p1*(var1 + mu1^2) + p2*(var2 + mu2^2) - mu_th^2

## ---------------- Method 1: Binomial counts -> sub-pdf采样 ----------------
t1 <- system.time({
  N1 <- rbinom(1, n, p1); N2 <- n - N1
  x1 <- c(q1_cond(runif(N1)), q2_cond(runif(N2)))
  m1 <- mean(x1); v1e <- var(x1)
})
cat(sprintf("Q2-Method1 counts: N1=%d, N2=%d\n", N1, N2))

## ---------------- Method 2: U ~ Unif[0,1] -> 直接分段反解 -----------------
t2 <- system.time({
  U  <- runif(n)
  x2 <- numeric(n)

  idx1 <- (U < p1)
  if (any(idx1)) {
    u1 <- U[idx1] / p1                 # [0,0.5)->[0,1]
    x2[idx1] <- q1_cond(u1)
  }
  idx2 <- !idx1
  if (any(idx2)) {
    u2 <- (U[idx2] - p1) / p2          # [0.5,1]->[0,1]
    x2[idx2] <- q2_cond(u2)
  }

  N1b <- sum(idx1); N2b <- sum(idx2)   # 仅用于报告
  m2 <- mean(x2); v2e <- var(x2)
})
cat(sprintf("Q2-Method2(U->Fi^-1) counts: N1=%d, N2=%d\n\n", N1b, N2b))

## ---------------- Print & plots --------------------------------------------
cat("=== Q2: Theoretical vs Empirical ===\n")
cat(sprintf("Theory : mean=%.6f  var=%.6f\n", mu_th, var_th))
cat(sprintf("Method1(counts->subpdf RNG): mean=%.6f  var=%.6f | time=%.3fs\n",
            m1, v1e, t1["elapsed"]))
cat(sprintf("Method2(U->Fi^-1)         : mean=%.6f  var=%.6f | time=%.3fs\n\n",
            m2, v2e, t2["elapsed"]))

png("q2_density_compare.png", 1600, 800, res=150)
par(mfrow=c(1,2), mar=c(4,4,3,1))
hist(x1, breaks="FD", freq=FALSE, col="grey85", border="white",
     main="Q2 — Method 1: Density", xlab="x")
curve(pdf_full(x), add=TRUE, lwd=2)
hist(x2, breaks="FD", freq=FALSE, col="lightblue", border="white",
     main="Q2 — Method 2: Density", xlab="x")
curve(pdf_full(x), add=TRUE, lwd=2)
dev.off()

png("q2_cdf_compare.png", 1600, 800, res=150)
par(mfrow=c(1,2), mar=c(4,4,3,1))
plot(ecdf(x1), main="Q2 — Method 1: ECDF vs CDF", xlab="x", ylab="F", col="blue")
curve(cdf_full(x), add=TRUE, lwd=2, lty=2)
plot(ecdf(x2), main="Q2 — Method 2: ECDF vs CDF", xlab="x", ylab="F", col="blue")
curve(cdf_full(x), add=TRUE, lwd=2, lty=2)
dev.off()
