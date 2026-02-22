############################################################
## Levy(mu, c) sampling
## Method 1: Normal transform X = mu + c / Z^2,  Z ~ N(0,1)
## Method 2: Rejection sampling with Pareto proposal
## Method 3: Rejection sampling with shifted exponential proposal
############################################################

## -------------------------- 1. Global settings --------------------------
mu <- 1          # location parameter of Levy distribution
c  <- 4          # scale parameter of Levy distribution
N  <- 10000      # target sample size
set.seed(123)    # random seed (for reproducibility)


## -------------------------- 2. Sampling via normal transform --------------------------
## Sampling function: X = mu + c / Z^2,  Z ~ N(0,1)
levy_normal_sampling <- function(mu, c, n) {
  Z <- rnorm(n, mean = 0, sd = 1)    # standard normal sample
  Z <- Z[Z != 0]                     # remove possible zeros (avoid division by 0)
  X <- mu + c / (Z ^ 2)
  return(X[1:n])                     # ensure length n
}

## Draw samples and compute empirical moments
X_normal <- levy_normal_sampling(mu, c, N)
emp_mean_normal <- mean(X_normal)
emp_var_normal  <- var(X_normal)

cat("=== Method 1: Normal transform ===\n")
cat("Theoretical mean : +Inf (does not exist for Levy)\n")
cat("Theoretical var  : +Inf (does not exist for Levy)\n")
cat("Empirical mean   :", round(emp_mean_normal, 4), "\n")
cat("Empirical var    :", round(emp_var_normal, 4), "\n\n")


## -------------------------- 3. Common: target pdf f(x) --------------------------
levy_pdf <- function(x, mu, c) {
  ifelse(
    x <= mu,
    0,
    sqrt(c / (2 * pi)) * exp(-c / (2 * (x - mu))) / ((x - mu) ^ (3/2))
  )
}


## -------------------------- 4. Rejection Method 1: Pareto proposal --------------------------
## 4.1 Pareto pdf with xm = 1
pareto_pdf <- function(x, a, xm = 1) {
  ifelse(
    x < xm,
    0,
    (a * (xm ^ a)) / (x ^ (a + 1))
  )
}

## 4.2 Choose Pareto shape parameter a by minimizing M = max_x f(x)/g(x)
find_pareto_a <- function(mu, c, xm = 1) {
  objective <- function(a) {
    x_range <- seq(mu + 0.01, mu + 100, length.out = 1000)
    f_vals  <- sapply(x_range, levy_pdf, mu = mu, c = c)
    g_vals  <- sapply(x_range, pareto_pdf, a = a, xm = xm)
    max(f_vals / g_vals)
  }
  opt_result <- optimize(objective, interval = c(0.1, 5.0), maximum = FALSE)
  opt_result$minimum
}

a_estimated <- find_pareto_a(mu, c)
cat("=== Method 2: Rejection with Pareto proposal ===\n")
cat("Estimated Pareto shape a:", round(a_estimated, 4), "\n")

## 4.3 Rejection sampler with Pareto proposal
rejection_pareto <- function(mu, c, a, xm = 1, n) {
  samples         <- numeric(0)
  total_generated <- 0L

  ## Compute M = sup_x f(x)/g(x) on a grid
  x_range <- seq(mu + 0.01, mu + 100, length.out = 1000)
  M <- max(sapply(
    x_range,
    function(x) levy_pdf(x, mu, c) / pareto_pdf(x, a, xm)
  ))

  while (length(samples) < n) {
    ## Sample from Pareto using inverse CDF: X = xm / U^(1/a)
    u <- runif(1)
    x <- xm / (u ^ (1 / a))
    total_generated <- total_generated + 1L

    ## Acceptance probability
    num <- levy_pdf(x, mu, c)
    den <- pareto_pdf(x, a, xm)
    if (den <= 0) next

    accept_prob <- num / (M * den)
    if (runif(1) <= accept_prob) {
      samples <- c(samples, x)
    }
  }

  rejection_rate <- 1 - length(samples) / total_generated
  emp_mean <- mean(samples)
  emp_var  <- var(samples)

  list(
    samples       = samples,
    rejection_rate = rejection_rate,
    emp_mean      = emp_mean,
    emp_var       = emp_var,
    M             = M,
    total_generated = total_generated
  )
}

pareto_result <- rejection_pareto(mu, c, a_estimated, n = N)
cat("Rejection rate :", round(pareto_result$rejection_rate, 4), "\n")
cat("Empirical mean :", round(pareto_result$emp_mean, 4), "\n")
cat("Empirical var  :", round(pareto_result$emp_var, 4), "\n")
cat("M (sup f/g)    :", round(pareto_result$M, 4), "\n\n")


## -------------------------- 5. Rejection Method 2: shifted exponential proposal --------------------------
## 5.1 Shifted exponential pdf: g(x) = b * exp(-b (x - a)), x >= a
shifted_expon_pdf <- function(x, b, a = 1) {
  ifelse(
    x < a,
    0,
    b * exp(-b * (x - a))
  )
}

## 5.2 Choose rate b by minimizing M = max_x f(x)/g(x)
find_expon_b <- function(mu, c, a = 1) {
  objective <- function(b) {
    x_range <- seq(mu + 0.01, mu + 100, length.out = 1000)
    f_vals  <- sapply(x_range, levy_pdf, mu = mu, c = c)
    g_vals  <- sapply(x_range, shifted_expon_pdf, b = b, a = a)
    max(f_vals / g_vals)
  }
  opt_result <- optimize(objective, interval = c(0.1, 5.0), maximum = FALSE)
  opt_result$minimum
}

b_estimated <- find_expon_b(mu, c)
cat("=== Method 3: Rejection with shifted exponential proposal ===\n")
cat("Estimated exponential rate b:", round(b_estimated, 4), "\n")

## 5.3 Rejection sampler with shifted exponential proposal
rejection_expon <- function(mu, c, b, a = 1, n) {
  samples         <- numeric(0)
  total_generated <- 0L

  ## Compute M = sup_x f(x)/g(x) on a grid
  x_range <- seq(mu + 0.01, mu + 100, length.out = 1000)
  M <- max(sapply(
    x_range,
    function(x) levy_pdf(x, mu, c) / shifted_expon_pdf(x, b, a)
  ))

  while (length(samples) < n) {
    ## Sample from shifted exponential: X = a - log(1 - U) / b
    u <- runif(1)
    x <- a - log(1 - u) / b
    total_generated <- total_generated + 1L

    ## Acceptance probability
    num <- levy_pdf(x, mu, c)
    den <- shifted_expon_pdf(x, b, a)
    if (den <= 0) next

    accept_prob <- num / (M * den)
    if (runif(1) <= accept_prob) {
      samples <- c(samples, x)
    }
  }

  rejection_rate <- 1 - length(samples) / total_generated
  emp_mean <- mean(samples)
  emp_var  <- var(samples)

  list(
    samples        = samples,
    rejection_rate = rejection_rate,
    emp_mean       = emp_mean,
    emp_var        = emp_var,
    M              = M,
    total_generated = total_generated
  )
}

expon_result <- rejection_expon(mu, c, b_estimated, n = N)
cat("Rejection rate :", round(expon_result$rejection_rate, 4), "\n")
cat("Empirical mean :", round(expon_result$emp_mean, 4), "\n")
cat("Empirical var  :", round(expon_result$emp_var, 4), "\n")
cat("M (sup f/g)    :", round(expon_result$M, 4), "\n\n")
