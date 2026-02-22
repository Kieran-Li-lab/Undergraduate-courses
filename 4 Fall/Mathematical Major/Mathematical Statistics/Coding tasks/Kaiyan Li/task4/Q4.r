## ================================================================
##  Lévy(μ=0, c=1) sampling: 4 methods
##  1) Direct sampling via Normal transform      (baseline, exact)
##  2) Rejection with Pareto proposal           (given in sheet)
##  3) Rejection with shifted Exponential       (given in sheet)
##  4) Rejection with half-Cauchy proposal      (own choice)
##
##  For each method:
##    - generate N samples
##    - record CPU time
##    - compute empirical mean / variance
##    - (for rejection) compute acceptance rate
##
##  All comments are in English as requested.
## ================================================================

set.seed(123)
N <- 10000                 # sample size

## ------------------------------------------------
##  Target Lévy(0,1) density
##  f(x) = sqrt(1/(2π)) * exp(-1/(2x)) / x^{3/2}, x>0
## ------------------------------------------------
levy_pdf <- function(x) {
  y <- numeric(length(x))
  idx <- x > 0
  z   <- x[idx]
  y[idx] <- sqrt(1/(2*pi)) * exp(-1/(2*z)) / (z^(3/2))
  y
}

## ------------------------------------------------
##  Method 1: exact sampling via Normal
##
##  If Z ~ N(0,1), then X = μ + c / Z^2 ~ Lévy(μ,c).
##  Here μ = 0, c = 1 → X = 1/Z^2.
## ------------------------------------------------
r_levy_normal <- function(n) {
  z <- rnorm(n)
  1 / (z^2)
}

t1 <- system.time({
  x1 <- r_levy_normal(N)
})

res1 <- list(
  method = "Normal transform",
  mean   = mean(x1),
  var    = var(x1),
  time   = t1["elapsed"],
  acc    = 1.0            # acceptance rate = 1, no rejection
)

## ------------------------------------------------
##  Generic rejection sampler
##  rprop : function(n)  -> proposal samples
##  dprop : function(x)  -> proposal pdf g(x)
##  M     : constant with f(x) <= M g(x) on practical range
## ------------------------------------------------
rejection_sampler <- function(n, rprop, dprop, M, name) {
  x      <- numeric(n)
  k      <- 0L           # accepted counter
  trials <- 0L           # total proposal draws

  t <- system.time({
    while (k < n) {
      y <- rprop(1)      # propose Y ~ g
      if (y <= 0) next   # Lévy support is (0,∞)
      gy <- dprop(y)
      fy <- levy_pdf(y)

      if (gy <= 0 || fy <= 0) next
      u  <- runif(1)
      trials <- trials + 1L

      if (u <= fy / (M * gy)) {
        k <- k + 1L
        x[k] <- y
      }
    }
  })

  list(
    method = name,
    x      = x,
    mean   = mean(x),
    var    = var(x),
    time   = t["elapsed"],
    trials = trials,
    acc    = n / trials
  )
}

## ================================================================
##  Method 2: Rejection with Pareto proposal
##
##  Proposal: Pareto(x_m, alpha), x >= x_m
##      g(x) = alpha * x_m^alpha / x^{alpha+1}
##
##  We choose alpha = 1/2 so that the tail x^{-alpha-1} = x^{-3/2}
##  matches the power of the Lévy tail.  Take x_m = 0.1 so that
##  support is close to 0, but not too small (otherwise M explodes).
##
##  For c=1, μ=0, alpha=1/2, x_m arbitrary:
##      f(x) = sqrt(1/(2π)) exp(-1/(2x)) / x^{3/2}
##      g(x) = (1/2) x_m^{1/2} / x^{3/2}
##
##  Ratio r(x) = f(x) / g(x) =
##      [sqrt(1/(2π)) / ((1/2) sqrt(x_m))] * exp(-1/(2x))
##  which is increasing in x and tends to
##      M_Pareto = sqrt(1/(2π)) / ((1/2) sqrt(x_m))
##              = (2 / sqrt(2π)) * 1/sqrt(x_m)
##  as x -> ∞.
##
##  So a valid majorizing constant is M = M_Pareto.
## ================================================================

xm   <- 0.1
alpha <- 0.5
M_pareto <- (2 / sqrt(2*pi)) * 1 / sqrt(xm)  # analytic bound

dpareto <- function(x) {
  ifelse(x >= xm, alpha * xm^alpha / (x^(alpha+1)), 0)
}

rpareto <- function(n) {
  u <- runif(n)
  # inverse CDF of Pareto: X = x_m * (1-U)^(-1/alpha)
  xm * (1 - u)^(-1/alpha)
}

res2 <- rejection_sampler(
  n     = N,
  rprop = rpareto,
  dprop = dpareto,
  M     = M_pareto,
  name  = "Pareto proposal"
)

## ================================================================
##  Method 3: Rejection with shifted Exponential proposal
##
##  Proposal: g(x) = b * exp(-b (x-a)), x > a
##  We choose a = 0, b = 0.01 so that the tail decays very slowly
##  (otherwise Lévy tail x^{-3/2} would eventually dominate).
##
##  On the practical range [eps, Xmax] we compute numerically
##      M_exp >= sup_x f(x)/g(x)
##  by scanning a dense grid.  For x outside this range the Lévy
##  density is already extremely small, so this is acceptable for
##  simulation.
## ================================================================

a_exp <- 0
b_exp <- 0.01

dexp_shift <- function(x) {
  ifelse(x > a_exp, b_exp * exp(-b_exp * (x - a_exp)), 0)
}

rexp_shift <- function(n) {
  a_exp + rexp(n, rate = b_exp)
}

## numerical upper bound M_exp on ratio f/g over [eps, Xmax]:
eps  <- 1e-4
Xmax <- 100
grid <- seq(eps, Xmax, length.out = 20000)
ratio_exp <- levy_pdf(grid) / dexp_shift(grid)
M_exp <- max(ratio_exp[is.finite(ratio_exp)], na.rm = TRUE)
M_exp <- 1.1 * M_exp   # small safety factor

res3 <- rejection_sampler(
  n     = N,
  rprop = rexp_shift,
  dprop = dexp_shift,
  M     = M_exp,
  name  = "Shifted exponential proposal"
)

## ================================================================
##  Method 4: Rejection with half-Cauchy proposal (own choice)
##
##  We use a half-Cauchy on (0,∞):
##      Y = |C|,  C ~ Cauchy(0, s)
##  pdf:
##      g(x) =  2 / [π s (1 + (x/s)^2)], x > 0
##
##  Tail ~ 1/x^2, faster than Lévy ~ 1/x^{3/2}, so Lévy has heavier
##  tail and cannot be *globally* dominated by g on (0,∞).
##  For simulation we again restrict to [eps, Xmax_C] and compute
##  a numerical M_Cauchy = sup f/g on this range.
## ================================================================

s_cauchy <- 2

dcauchy_half <- function(x) {
  ifelse(x > 0, 2 / (pi * s_cauchy * (1 + (x/s_cauchy)^2)), 0)
}

rcauchy_half <- function(n) {
  abs(rcauchy(n, location = 0, scale = s_cauchy))
}

Xmax_C <- 100
gridC  <- seq(eps, Xmax_C, length.out = 20000)
ratio_c <- levy_pdf(gridC) / dcauchy_half(gridC)
M_cauchy <- max(ratio_c[is.finite(ratio_c)], na.rm = TRUE)
M_cauchy <- 1.1 * M_cauchy  # safety factor

res4 <- rejection_sampler(
  n     = N,
  rprop = rcauchy_half,
  dprop = dcauchy_half,
  M     = M_cauchy,
  name  = "Half-Cauchy proposal"
)

## ================================================================
##  Summary: theoretical vs empirical
##  (True mean/variance are infinite; we just report empirical ones.)
## ================================================================

cat("Theoretical Lévy(0,1): mean = +Inf, var = +Inf\n\n")

summary_tab <- data.frame(
  Method = c(res1$method, res2$method, res3$method, res4$method),
  Empirical_Mean = c(res1$mean, res2$mean, res3$mean, res4$mean),
  Empirical_Var  = c(res1$var,  res2$var,  res3$var,  res4$var),
  Time_sec       = c(res1$time, res2$time, res3$time, res4$time),
  Accept_Rate    = c(res1$acc,  res2$acc,  res3$acc,  res4$acc)
)

## round only numeric columns
summary_tab$Empirical_Mean <- round(summary_tab$Empirical_Mean, 4)
summary_tab$Empirical_Var  <- round(summary_tab$Empirical_Var, 4)
summary_tab$Time_sec       <- round(summary_tab$Time_sec, 4)
summary_tab$Accept_Rate    <- round(summary_tab$Accept_Rate, 4)

print(summary_tab, row.names = FALSE)


## ================================================================
##  Simple plots: compare histograms with true pdf
## ================================================================

png("levy_pdf_compare.png", width = 1600, height = 900, res = 150)
par(mfrow = c(2,2), mar = c(4,4,3,1))

plot_density <- function(x, main_title) {
  hist(x, breaks = 80, freq = FALSE, xlim = c(0,50),
       main = main_title, xlab = "x", ylab = "density",
       border = "gray80", col = "gray90")
  xx <- seq(1e-4, 50, length.out = 2000)
  lines(xx, levy_pdf(xx), col = "red", lwd = 2)
  legend("topright", legend = c("Histogram", "True Lévy pdf"),
         col = c("gray50","red"), lwd = c(1,2), bty = "n")
}

plot_density(x1,                     "Method 1: Normal transform")
plot_density(res2$x,                 "Method 2: Pareto proposal")
plot_density(res3$x,                 "Method 3: Shifted exp proposal")
plot_density(res4$x,                 "Method 4: Half-Cauchy proposal")

dev.off()

cat("\nFigure saved to 'levy_pdf_compare.png'.\n")
