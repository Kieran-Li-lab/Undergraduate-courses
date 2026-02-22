## ---------- Q2: piecewise; two methods with explicit counts ----------
set.seed(42)
n <- 10000

a  <- 3/8
w1 <- 0.5; w2 <- 0.5

F_q2 <- function(x){
  res <- numeric(length(x))
  F1  <- function(xx) (3/8)*(xx - xx^3/3 + 2/3)
  res[x < -1] <- 0
  res[x >= -1 & x <= 1] <- F1(x[x >= -1 & x <= 1])
  res[x > 1 & x < 2] <- 0.5
  res[x >= 2] <- 1 - 0.5*exp(-(x[x >= 2]-2))
  res
}
f_q2 <- function(x){
  y <- numeric(length(x))
  y[x >= -1 & x <= 1] <- a*(1 - x[x >= -1 & x <= 1]^2)
  y[x >= 2] <- 0.5*exp(-(x[x >= 2]-2))
  y
}

## Global inverse via piecewise root/closed form
Finv_q2 <- function(u){
  out <- numeric(length(u))
  m1  <- (u < 0.5)
  if (any(m1)) {
    uu <- u[m1]
    out[m1] <- sapply(uu, function(s) {
      uniroot(function(x) (3/8)*(x - x^3/3 + 2/3) - s, c(-1,1), tol=1e-10)$root
    })
  }
  if (any(!m1)) {
    uu <- u[!m1]
    out[!m1] <- 2 - log(2*(1-uu))
  }
  out
}

## Theoretical moments (numerical)
EX  <- integrate(function(t) t*f_q2(t),  -1, 1)$value +
       integrate(function(t) t*f_q2(t),   2, Inf)$value
EX2 <- integrate(function(t) t^2*f_q2(t), -1, 1)$value +
       integrate(function(t) t^2*f_q2(t),  2, Inf)$value
mu_th  <- EX
var_th <- EX2 - EX^2

## ---------------- Method 1: counts -> U in segment intervals -> Finv ----------------
t1 <- system.time({
  N1 <- rbinom(1, n, w1); N2 <- n - N1
  U  <- numeric(n)
  if (N1 > 0) U[1:N1]     <- runif(N1, 0, 0.5)  # global CDF interval for [-1,1]
  if (N2 > 0) U[(N1+1):n] <- runif(N2, 0.5, 1)  # global CDF interval for [2,∞)
  x1 <- Finv_q2(U)
  m1 <- mean(x1); v1e <- var(x1)
})
cat(sprintf("Q2-Method1 counts: N1=%d, N2=%d\n", N1, N2))

## ---------------- Method 2: global U -> explicit counts -> Finv ---------------------
t2 <- system.time({
  U2   <- runif(n)
  idx1 <- (U2 < 0.5); idx2 <- !idx1
  N1b  <- sum(idx1); N2b <- sum(idx2)
  x2   <- numeric(n)
  x2[idx1] <- Finv_q2(U2[idx1])
  x2[idx2] <- Finv_q2(U2[idx2])
  m2 <- mean(x2); v2e <- var(x2)
})
cat(sprintf("Q2-Method2 counts: N1=%d, N2=%d\n\n", N1b, N2b))

## ---------------- Print & plots ----------------
cat("=== Q2: Theoretical vs Empirical ===\n")
cat(sprintf("Theory : mean=%.6f  var=%.6f\n", mu_th, var_th))
cat(sprintf("Method1(counts->Useg->Finv): mean=%.6f  var=%.6f | time=%.3fs\n", m1, v1e, t1["elapsed"]))
cat(sprintf("Method2(U->counts->Finv)  : mean=%.6f  var=%.6f | time=%.3fs\n\n", m2, v2e, t2["elapsed"]))
cat(sprintf("RelErr-1: mean=%.3f%% var=%.3f%%\n", 100*abs(m1-mu_th)/mu_th, 100*abs(v1e-var_th)/var_th))
cat(sprintf("RelErr-2: mean=%.3f%% var=%.3f%%\n\n", 100*abs(m2-mu_th)/mu_th, 100*abs(v2e-var_th)/var_th))

png("q2_density_compare.png", 1600, 800, res=150)
par(mfrow=c(1,2), mar=c(4,4,3,1))
hist(x1, breaks="FD", freq=FALSE, col="lightgray", border="white",
     main="Q2 — Method 1: Density", xlab="x"); curve(f_q2(x), add=TRUE, lwd=2)
hist(x2, breaks="FD", freq=FALSE, col="lightblue", border="white",
     main="Q2 — Method 2: Density", xlab="x"); curve(f_q2(x), add=TRUE, lwd=2)
dev.off()

png("q2_cdf_compare.png", 1600, 800, res=150)
par(mfrow=c(1,2), mar=c(4,4,3,1))
plot(ecdf(x1), main="Q2 — Method 1: ECDF vs CDF", xlab="x", ylab="F", col="blue")
curve(F_q2(x), add=TRUE, lwd=2, lty=2)
plot(ecdf(x2), main="Q2 — Method 2: ECDF vs CDF", xlab="x", ylab="F", col="blue")
curve(F_q2(x), add=TRUE, lwd=2, lty=2)
dev.off()
