## ---------- Q3: three segments; counts-then-inverse vs global-U-then-inverse ----------
set.seed(42)
n <- 10000
w1 <- 1/4; w2 <- 1/4; w3 <- 1/2

F_q3 <- function(x){
  res <- numeric(length(x))
  res[x < -1] <- 0
  s1 <- x >= -1 & x < 1
  res[s1] <- ((x[s1] + 1)^2)/16
  s2 <- x >= 1 & x < 2
  res[s2] <- x[s2]/4
  s3 <- x >= 2
  res[s3] <- 1 - 0.5*exp(-(x[s3]-2))
  res
}
f_q3 <- function(x){
  y <- numeric(length(x))
  y[x >= -1 & x < 1] <- (x[x >= -1 & x < 1] + 1)/8
  y[x >= 1  & x < 2] <- 1/4
  y[x >= 2]          <- 0.5*exp(-(x[x >= 2]-2))
  y
}

Finv_q3 <- function(u){
  out <- numeric(length(u))
  m1 <- (u < 0.25); m2 <- (u >= 0.25 & u < 0.5); m3 <- (u >= 0.5)
  if (any(m1)) out[m1] <- -1 + 4*sqrt(u[m1])
  if (any(m2)) out[m2] <- 4*u[m2]
  if (any(m3)) out[m3] <- 2 - log(2*(1-u[m3]))
  out
}

## theoretical moments (numerical)
EX  <- integrate(function(t) t*f_q3(t),   -1, 1)$value +
       integrate(function(t) t*f_q3(t),    1, 2)$value +
       integrate(function(t) t*f_q3(t),    2, Inf)$value
EX2 <- integrate(function(t) t^2*f_q3(t), -1, 1)$value +
       integrate(function(t) t^2*f_q3(t),  1, 2)$value +
       integrate(function(t) t^2*f_q3(t),  2, Inf)$value
mu_th  <- EX
var_th <- EX2 - EX^2

## ---------------- Method 1: counts then inverse ----------------
t1 <- system.time({
  Ns <- as.vector(rmultinom(1, n, prob=c(w1,w2,w3)))
  U  <- numeric(n); p1 <- 0.25; p2 <- 0.5
  if (Ns[1] > 0) U[1:Ns[1]] <- runif(Ns[1], 0, p1)
  if (Ns[2] > 0) {
    i2s <- (Ns[1]+1):(Ns[1]+Ns[2])
    U[i2s] <- runif(Ns[2], p1, p2)
  }
  if (Ns[3] > 0) U[(n-Ns[3]+1):n] <- runif(Ns[3], p2, 1)
  x1 <- Finv_q3(U)
  m1 <- mean(x1); v1e <- var(x1)
})

## ---------------- Method 2: global U then inverse ---------------
t2 <- system.time({
  U2 <- runif(n)
  x2 <- Finv_q3(U2)
  m2 <- mean(x2); v2e <- var(x2)
})

## ---------------- Print & plots ----------------
cat("=== Q3: Theoretical vs Empirical ===\n")
cat(sprintf("Theory : mean=%.6f  var=%.6f\n", mu_th, var_th))
cat(sprintf("Method1(counts->U in seg->Finv): mean=%.6f  var=%.6f | time=%.3fs\n",
            m1, v1e, t1["elapsed"]))
cat(sprintf("Method2(U in [0,1]->seg->Finv): mean=%.6f  var=%.6f | time=%.3fs\n\n",
            m2, v2e, t2["elapsed"]))
cat(sprintf("RelErr-1: mean=%.3f%% var=%.3f%%\n", 100*abs(m1-mu_th)/mu_th, 100*abs(v1e-var_th)/var_th))
cat(sprintf("RelErr-2: mean=%.3f%% var=%.3f%%\n\n", 100*abs(m2-mu_th)/mu_th, 100*abs(v2e-var_th)/var_th))

png("q3_density_compare.png", 1600, 800, res=150)
par(mfrow=c(1,2), mar=c(4,4,3,1))
hist(x1, breaks="FD", freq=FALSE, col="gray85", border="white",
     main="Q3 — Method 1: Density", xlab="x"); curve(f_q3(x), add=TRUE, lwd=2)
hist(x2, breaks="FD", freq=FALSE, col="lightblue", border="white",
     main="Q3 — Method 2: Density", xlab="x"); curve(f_q3(x), add=TRUE, lwd=2)
dev.off()

png("q3_cdf_compare.png", 1600, 800, res=150)
par(mfrow=c(1,2), mar=c(4,4,3,1))
plot(ecdf(x1), main="Q3 — Method 1: ECDF vs CDF", xlab="x", ylab="F", col="blue")
curve(F_q3(x), add=TRUE, lwd=2, lty=2)
plot(ecdf(x2), main="Q3 — Method 2: ECDF vs CDF", xlab="x", ylab="F", col="blue")
curve(F_q3(x), add=TRUE, lwd=2, lty=2)
dev.off()
