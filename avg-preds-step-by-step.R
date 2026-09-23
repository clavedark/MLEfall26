# average-effects computation, step by step.
#
# The file works up in scale: one leader-year, then three, then all 16,068, then
# all of them at each of 25 years of tenure, then all of that at three values of
# Polity. Sections 0-8 build it a piece at a time; section 9 then runs the whole
# delta-method calculation in one block, and section 11 runs the same quantities
# by simulation in one block. Sections 9 and 11 each re-declare everything they
# use, so either one can be walked through on its own.
#
# Each quantity is built twice: once as an explicit loop, once in the compact
# form, with a check that the two agree. The loop shows the process; the compact
# form is what you would write once the process is familiar.

rm(list = ls())


library(tidyverse)
library(haven)
source("_common.R")     # bu_green, bu_lightgreen, bu_black, bu_lty, theme_bu()

set.seed(20260916)


# 0. THE DATA AND THE MODEL ----------------------------------------------------
#    out: est (the estimation sample), m3 (the fitted logit)

# person-period data: one row per calendar year each leader spell touches
archigos <- read_dta("https://www.rochester.edu/college/faculty/hgoemans/Archigos_4.1_stata14.dta") %>%
  mutate(startyr  = as.numeric(format(as.Date(startdate), "%Y")),
         endyr    = as.numeric(format(as.Date(enddate),   "%Y")),
         spellyrs = (endyr - startyr) + 1)

leaderyears <- archigos %>%
  uncount(spellyrs) %>%                                   # one row per year of the spell
  group_by(obsid) %>%
  mutate(year = startyr - 1 + row_number(),
         t    = row_number(),                             # survival time
         t2   = t^2,
         t3   = t^3,
         leftoffice = as.integer(t == n() & exit != "Still in Office")) %>%
  ungroup() %>%
  arrange(obsid, year) %>%                                # spell by spell, years in order
  mutate(age    = ifelse(yrborn > 0, year - yrborn, NA_integer_),
         female = as.integer(gender == "F"),
         entry  = factor(na_if(entry, "Unknown"),
                         levels = c("Regular", "Irregular", "Foreign Imposition")),
         region = factor(case_when(ccode < 200 ~ "Americas",
                                   ccode < 400 ~ "Europe",
                                   ccode < 630 ~ "Africa",
                                   ccode < 700 ~ "Middle East",
                                   TRUE        ~ "Asia/Oceania"),
                         levels = c("Europe", "Americas", "Africa",
                                    "Middle East", "Asia/Oceania")))

polity <- read_csv(here::here("data", "p5v2018.csv"), show_col_types = FALSE) %>%
  select(ccode, year, polity2)

est <- leaderyears %>%
  left_join(polity, by = c("ccode", "year"), relationship = "many-to-one") %>%
  drop_na(polity2, age, female, entry, region)

m3 <- glm(leftoffice ~ t + t2 + t3 + polity2 + age + female + entry + region,
          data = est, family = binomial(link = "logit"))


# 1. THE THREE PIECES EVERY QUANTITY BELOW IS BUILT FROM -----------------------
#    out: b (k coefficients), V (k x k covariance), n and k

# A predicted probability needs the coefficients and a row of data. An interval
# around it needs the covariance matrix as well. Nothing else is used after this.
b <- coef(m3)                     # k coefficients, a named vector
V <- vcov(m3)                     # k x k covariance matrix
k <- length(b)
n <- nobs(m3)

c(cases = n, coefficients = k)


# 2. THE COUNTERFACTUAL DESIGN MATRIX FOR AVERAGE EFFECTS ----------------------
#    in:  est, b
#    out: X5 -- every case as it would look at Polity 0, year 5 of its spell

# "Average effects" means: change the variable of interest for EVERY case, leave
# each case's own age, gender, route to power and region alone, predict, and
# average. So the first step is a copy of the data with two things overwritten.
# est_cf is the estimation sample, counterfactual version: every row of est,
# with polity2 and the spell clock overwritten and every other column left
# exactly as that leader-year had it. Same rows, same n, two columns changed.
est_cf5 <- est %>% mutate(polity2 = 0, t = 5, t2 = 5^2, t3 = 5^3)

# model.matrix turns that data frame into the numeric matrix glm actually used:
# factors become dummy columns, and the formula's t2 and t3 become their own
# columns. formula(m3)[-2] drops the left-hand side, so no outcome is needed.
X5 <- model.matrix(formula(m3)[-2], data = est_cf5)

dim(X5)                                           # n rows, k columns


# 2a. How to read the square brackets ------------------------------------------
#
# X5 has two dimensions, so it takes two indices separated by a comma. Leaving a
# slot EMPTY means "all of them":
#
#   X5[j, 4]   row j, column 4          -- one number
#   X5[j, ]    row j, every column      -- a vector of length k  (one case)
#   X5[, 4]    every row, column 4      -- a vector of length n  (one variable)
#
# The comma is what tells R the object is two-dimensional. Without it, R treats
# the matrix as one long vector running DOWN the columns, which is legal and
# almost never what you meant:

length(X5[1, ])            # k -- one case, all its variables
length(X5[, 1])            # n -- one variable, all the cases
X5[2]                      # NO comma: element 2 going down column 1, not row 2
X5[2, 1]                   # the same element, with both indices written out

# X5[j, ] comes back as a plain vector, not a 1-row matrix. That is why
# sum(X5[j, ] * b) works below, and why t(g) %*% V %*% g works in section 5.
is.matrix(X5[1, ])


# 3. ONE CASE ------------------------------------------------------------------
#    in:  X5, b
#    out: h_i (that case's hazard), g_i (that case's gradient)

# Take a row we can name. Margaret Thatcher's fifth year in office is 1983; the
# UK's actual Polity score that year is 10, and est_cf5 has set it to 0.
i <- which(est$leader == "Thatcher" & est$t == 5)

est[i, c("leader", "idacr", "year", "t", "age", "female", "entry", "region", "polity2")]

# Her row of the design matrix, laid out term by term. Two things to see: female
# is 1, and all four region dummies are 0 because Europe is the reference
# category -- there is no regionEurope column for it to occupy.
tibble(term = colnames(X5), x = X5[i, ], b = b, contribution = X5[i, ] * b)

# The linear predictor is that last column added up, and the hazard is the
# logistic function of it.
eta_i <- sum(X5[i, ] * b)                    # x_i'b -- one number
h_i   <- 1 / (1 + exp(-eta_i))               # same as plogis(eta_i)

c(eta = eta_i, h = h_i)

# The gradient is what the delta interval will need later: how h_i moves when each
# coefficient moves. For the logit, dh/deta = h(1-h), so by the chain rule the
# derivative with respect to coefficient m is h(1-h) times that case's x_m.
w_i <- h_i * (1 - h_i)                       # one number
g_i <- w_i * X5[i, ]                         # k numbers, one per coefficient

round(g_i, 5)


# 4. THREE CASES -- what "average" is an average OF ----------------------------
#    in:  X5, b
#    out: nothing kept; this is the estimand at n = 3

# Three leader-years, all set to Polity 0 and year 5, differing only in the
# covariates they brought with them.
three <- c(which(est$leader == "Thatcher" & est$t == 5),
           which(est$leader == "Mandela"  & est$t == 5),
           which(est$leader == "Nyerere"  & est$t == 5))

h_three <- numeric(3)
for (r in 1:3) {
  h_three[r] <- 1 / (1 + exp(-sum(X5[three[r], ] * b)))
}

tibble(leader = est$leader[three], age = est$age[three],
       region = est$region[three], h = round(h_three, 4))

mean(h_three)              # the average hazard over these three cases

# Section 5 does exactly this over all 16,068 rows instead of three. The spread
# above is why it has to be all of them: Mandela is 80 in 1998 and Nyerere is 43
# in 1965, and age is in the model.


# 5. ALL CASES -- the average hazard and its gradient --------------------------
#    in:  X5, b
#    out: hbar_loop (1 number), g_loop (k-vector)

# One pass over the rows, accumulating two running totals: the hazards, and the
# gradients. Dividing each by n at the end gives the average and its gradient.
h_sum <- 0
g_sum <- rep(0, k)

for (j in 1:n) {
  eta_j <- sum(X5[j, ] * b)
  h_j   <- 1 / (1 + exp(-eta_j))
  w_j   <- h_j * (1 - h_j)

  h_sum <- h_sum + h_j
  g_sum <- g_sum + w_j * X5[j, ]              # add this case's k-vector to the total
}

hbar_loop <- h_sum / n
g_loop    <- g_sum / n

# The same two quantities without the loop. X5 %*% b does all n linear
# predictors at once; colMeans averages DOWN the columns, i.e. over cases.
pr       <- plogis(as.vector(X5 %*% b))       # n hazards
hbar_vec <- mean(pr)
g_vec    <- colMeans((pr * (1 - pr)) * X5)    # each row scaled by its own w, then averaged

c(loop = hbar_loop, vectorized = hbar_vec)
all.equal(g_loop, g_vec)

# Compare that to mean(h_three) in section 4: three cases average to 0.144, all
# 16,068 average to 0.183. The estimand is the second one.

# The averaging runs over CASES, at fixed coefficients. Averaging the k columns
# instead would collapse the gradient to one number; averaging over coefficient
# draws is a third thing, and that one is section 11.


# 6. THE STANDARD ERROR OF THAT AVERAGE ----------------------------------------
#    in:  g_loop, V
#    out: se -- one number

# The delta method: se = sqrt(g'Vg). Written out, that is a sum over every PAIR
# of coefficients -- each pair's covariance, weighted by both their gradients.
# With k = 13 there are 169 pairs.
var_loop <- 0
for (r in 1:k) {
  for (cc in 1:k) {
    var_loop <- var_loop + g_loop[r] * V[r, cc] * g_loop[cc]
  }
}

var_matrix <- as.numeric(t(g_vec) %*% V %*% g_vec)     # the same 169 products

c(loop = sqrt(as.numeric(var_loop)), matrix = sqrt(var_matrix))

# the interval, on the probability scale
hbar_vec + c(-1.96, 0, 1.96) * sqrt(var_matrix)


# 7. EVERY YEAR OF TENURE ------------------------------------------------------
#    in:  est, b, V
#    out: hbar (25 hazards), se_h (25 ses), G (25 x k gradients)

tmax <- 25

# Sections 2-6 did one year. Now do 25 of them, so the results need somewhere to
# live: two vectors of length 25, and a MATRIX with one row per year and one
# column per coefficient -- because each year has a whole gradient, not a number.
hbar <- numeric(tmax)
se_h <- numeric(tmax)
G    <- matrix(NA, tmax, k)
colnames(G) <- names(b)

for (i in 1:tmax) {
  est_cf <- est %>% mutate(polity2 = 0, t = i, t2 = i^2, t3 = i^3)
  Xi <- model.matrix(formula(m3)[-2], data = est_cf)

  pr      <- plogis(as.vector(Xi %*% b))
  hbar[i] <- mean(pr)
  G[i, ]  <- colMeans((pr * (1 - pr)) * Xi)   # write a k-vector into row i
  se_h[i] <- sqrt(as.numeric(t(G[i, ]) %*% V %*% G[i, ]))   # read row i back out
}

dim(G)                     # 25 x k
length(G[3, ])             # k -- year 3's gradient, all coefficients
length(G[, 3])             # 25 -- coefficient 3's gradient, all years

head(tibble(t = 1:tmax, h = hbar, se = se_h,
            lo = hbar - 1.96 * se_h, hi = hbar + 1.96 * se_h))


# 8. THE SURVIVOR FUNCTION -- a running product and a running sum --------------
#    in:  hbar, G, V
#    out: S (25 survivor values), se_S (25 ses)

# S(t) is the chance of getting through year 1 AND year 2 AND ... AND year t,
# so it multiplies the complements of the hazards. Its gradient ADDS one term
# per year, because every hazard in the product is a function of the same b:
#
#   dS/db = -S(t) * sum_{j<=t} g(j) / (1 - hbar(j))
#
# The two accumulators below run side by side -- one multiplying, one adding.
S    <- numeric(tmax)
se_S <- numeric(tmax)

running_S <- 1                       # the product so far
running_q <- rep(0, k)               # the summed gradient so far, a k-vector

for (i in 1:tmax) {
  running_S <- running_S * (1 - hbar[i])
  running_q <- running_q + G[i, ] / (1 - hbar[i])

  S[i]    <- running_S
  se_S[i] <- running_S * sqrt(as.numeric(t(running_q) %*% V %*% running_q))
}

# cumprod and cumsum are those two accumulators, written compactly
S_vec    <- cumprod(1 - hbar)
Q        <- apply(G / (1 - hbar), 2, cumsum)      # 25 x k: row i is running_q at year i
se_S_vec <- S_vec * sqrt(rowSums((Q %*% V) * Q))  # sqrt(q'Vq) for all 25 rows at once

c(all.equal(S, S_vec), all.equal(se_S, se_S_vec))

# What the running sum is for: the same calculation with the 25 years treated as
# independent of each other, which is what dropping it amounts to. At t = 1 the
# two agree, because there is only one term to accumulate.
se_S_wrong <- numeric(tmax)
for (i in 1:tmax) {
  se_S_wrong[i] <- sqrt(sum((S[i] / (1 - hbar[1:i]))^2 * se_h[1:i]^2))
}

tibble(t = 1:tmax, S = round(S, 3),
       se_correct = round(se_S, 4), se_independent = round(se_S_wrong, 4)) %>%
  filter(t %in% c(1, 5, 10, 25))


# 9. THE FULL GRID -- the delta-method calculation, end to end -----------------
#    in:  est and m3, from section 0 -- everything else is declared below
#    out: qi (75 rows: 3 Polity values x 25 years, both quantities)

# This section stands on its own. Every setting it uses is re-declared here, and
# sections 2-8 are its body: build the counterfactual design matrix, average the
# predictions over cases, average the gradients, accumulate the product for S(t).
# Nothing new happens below -- it is the same work inside one more loop.

b     <- coef(m3)                  # k coefficients
V     <- vcov(m3)                  # k x k covariance matrix
k     <- length(b)
tmax  <- 25                        # years of tenure to plot
pvals <- c(-10, 0, 10)             # the three counterfactual Polity values

qi <- tibble()                     # results pile up here, one block per Polity value

for (p in pvals) {

  # the hazard at each year: sections 2, 5, 6 and 7, inside one loop
  hbar <- numeric(tmax)            # average hazard, one per year
  se_h <- numeric(tmax)            # its standard error
  G    <- matrix(NA, tmax, k)      # its gradient: one ROW per year, one column per coefficient

  for (i in 1:tmax) {
    est_cf <- est %>% mutate(polity2 = p, t = i, t2 = i^2, t3 = i^3)   # the counterfactual
    Xi <- model.matrix(formula(m3)[-2], data = est_cf)                 # n x k

    pr      <- plogis(as.vector(Xi %*% b))        # n predicted hazards
    hbar[i] <- mean(pr)                           # averaged over CASES
    G[i, ]  <- colMeans((pr * (1 - pr)) * Xi)     # each row scaled by its own w, then averaged
    se_h[i] <- sqrt(as.numeric(t(G[i, ]) %*% V %*% G[i, ]))        # sqrt(g'Vg)
  }

  # the survivor function: section 8
  S    <- numeric(tmax)
  se_S <- numeric(tmax)

  running_S <- 1                   # the product so far
  running_q <- rep(0, k)           # the summed gradient so far

  for (i in 1:tmax) {
    running_S <- running_S * (1 - hbar[i])
    running_q <- running_q + G[i, ] / (1 - hbar[i])

    S[i]    <- running_S
    se_S[i] <- running_S * sqrt(as.numeric(t(running_q) %*% V %*% running_q))
  }

  qi <- bind_rows(qi, tibble(polity = p, t = 1:tmax,
                             h = hbar, h_se = se_h, s = S, s_se = se_S))
}

qi <- qi %>%
  mutate(h_lo = h - 1.96 * h_se, h_hi = h + 1.96 * h_se,
         s_lo = s - 1.96 * s_se, s_hi = s + 1.96 * s_se,
         polity = factor(polity, levels = pvals,
                         labels = c("Polity -10", "Polity 0", "Polity +10")))

qi


# 10. THE TWO FIGURES ----------------------------------------------------------

ggplot(qi, aes(t, h, color = polity, fill = polity, linetype = polity)) +
  geom_ribbon(aes(ymin = h_lo, ymax = h_hi), alpha = 0.45, color = NA) +
  geom_line(linewidth = 0.9) +
  scale_color_manual(values = c(bu_black, bu_lightgreen, bu_green)) +
  scale_fill_manual(values = c(bu_black, bu_lightgreen, bu_green)) +
  scale_linetype_manual(values = bu_lty[1:3]) +
  labs(title = "Annual hazard of leaving office",
       subtitle = "every leader-year set to each Polity value; 95% delta-method intervals",
       x = "Years in office", y = "h(t)",
       color = NULL, fill = NULL, linetype = NULL) +   # linetype = NULL or ggplot draws two legends
  theme_bu() + theme(legend.position = "bottom")

ggplot(qi, aes(t, s, color = polity, fill = polity, linetype = polity)) +
  geom_ribbon(aes(ymin = s_lo, ymax = s_hi), alpha = 0.45, color = NA) +
  geom_line(linewidth = 0.9) +
  geom_hline(yintercept = 0.5, color = bu_grey, linetype = "dashed", linewidth = 0.4) +
  scale_color_manual(values = c(bu_black, bu_lightgreen, bu_green)) +
  scale_fill_manual(values = c(bu_black, bu_lightgreen, bu_green)) +
  scale_linetype_manual(values = bu_lty[1:3]) +
  labs(title = "Share of leaders still in office",
       subtitle = "same model and same counterfactuals as the hazard figure",
       x = "Years in office", y = "S(t)",
       color = NULL, fill = NULL, linetype = NULL) +
  theme_bu() + theme(legend.position = "bottom")



# 11. THE SAME QUANTITIES BY SIMULATION ----------------------------------------
#     in:  est and m3, from section 0 -- everything else is declared below
#     out: sim_qi (75 rows: 3 Polity values x 25 years, both quantities)

# This section stands on its own. Every setting it uses is re-declared here, so
# it can be read, or run, without sections 2-9 in front of you.

b      <- coef(m3)                 # k coefficients
V      <- vcov(m3)                 # k x k covariance matrix
k      <- length(b)
tmax   <- 25                       # years of tenure to plot
pvals  <- c(-10, 0, 10)            # the three counterfactual Polity values
ndraws <- 1000                     # coefficient vectors to draw

# The counterfactual design matrix, at Polity 0 and year 5, rebuilt here: every
# case as it would look under that counterfactual, its own covariates untouched.
est_cf5_sim <- est %>% mutate(polity2 = 0, t = 5, t2 = 5^2, t3 = 5^3)
X5_sim  <- model.matrix(formula(m3)[-2], data = est_cf5_sim)

dim(X5_sim)                        # n cases x k coefficients


# 11a. What a draw is ----------------------------------------------------------

# Sections 2-9 held the coefficients fixed at b and used calculus for the
# uncertainty. Simulation holds nothing fixed: it draws ndraws plausible
# coefficient vectors from the model's own sampling distribution and recomputes
# the whole quantity once per draw.
set.seed(20260916)   # fix the seed right before the draws, so anything run or printed
                     # earlier cannot shift them
sims <- MASS::mvrnorm(ndraws, b, V)

dim(sims)                          # ndraws rows, one DRAW each; k columns

# One draw is one ROW of sims -- a complete alternative set of coefficients,
# near b and correlated the way V says they are.
b_draw1 <- sims[1, ]
round(cbind(estimate = b, draw1 = b_draw1), 4)

# That draw's average hazard is the section-5 calculation with b_draw1 in place
# of b: n predictions, averaged over cases.
mean(plogis(as.vector(X5_sim %*% b_draw1)))


# 11b. All the draws at one year -----------------------------------------------

# Each pass of this loop produces ONE number: an average over the n cases, at
# that draw's coefficients.
h_draws_loop <- numeric(ndraws)
for (d in 1:ndraws) {
  h_draws_loop[d] <- mean(plogis(as.vector(X5_sim %*% sims[d, ])))
}

# The compact form of that loop. X5_sim %*% t(sims) is n x ndraws -- one row per
# case, one column per draw -- and colMeans averages DOWN each column, over cases.
# X5_sim %*% t(sims) produces a matrix nx1000 - each column is xb for one sim over
# 1000 cases. colMeans takes the average of that (or of the logit of that column)
h_draws_vec <- colMeans(plogis(X5_sim %*% t(sims)))

all.equal(h_draws_loop, h_draws_vec)

# rowMeans on that same matrix averages the other way, over DRAWS within a case:
# n numbers describing different leaders, not ndraws describing estimation
# uncertainty. Both calls run; only one answers the question. The matrix plogis(X5_sim 
# %*%t(sims))) is n rows x 1000 columns - each column is the predicted probabilities from 
# that particular simulation. The colMeans of that column is the average prediction from
# that simulation

length(colMeans(plogis(X5_sim %*% t(sims))))     # ndraws -- the sampling distribution
length(rowMeans(plogis(X5_sim %*% t(sims))))     # n      -- one number per leader-year

# The interval is the 2.5th and 97.5th percentiles of those ndraws values.
quantile(h_draws_vec, c(0.025, 0.5, 0.975))


# 11c. The full grid -----------------------------------------------------------

sim_qi <- tibble()

for (p in pvals) {

  hmat <- matrix(NA, tmax, ndraws)         # years down the rows, draws across

  for (i in 1:tmax) {
    est_cf <- est %>% mutate(polity2 = p, t = i, t2 = i^2, t3 = i^3)
    Xi <- model.matrix(formula(m3)[-2], data = est_cf)
    hmat[i, ] <- colMeans(plogis(Xi %*% t(sims)))   # row i: this year, every draw
  }

  # S(t) is built WITHIN a draw -- cumprod down one column at a time -- so each
  # draw's survivor curve uses that draw's own tmax hazards.
  smat <- matrix(NA, tmax, ndraws)
  for (d in 1:ndraws) {
    smat[, d] <- cumprod(1 - hmat[, d])
  }

  # The interval comes from the ndraws values in each ROW: apply(., 1, ...) runs
  # across the draws at a fixed year. apply(X, MARGIN, FUN, ...) runs FUN 
  #over one dimension of a matrix (MARGIN). X is the matrix you want to perform
  # some operation on, MARGIN is the dimension of that matrix you want to alter, row=1
  # or column=2; FUN is the function you want to apply to that dimension, eg median.
  # colMeans(M) is identical() to apply(M, 2, mean).
  
  sim_qi <- bind_rows(sim_qi,
    tibble(polity = p, t = 1:tmax,
           sim_h    = apply(hmat, 1, median),
           sim_h_lo = apply(hmat, 1, quantile, 0.025),
           sim_h_hi = apply(hmat, 1, quantile, 0.975),
           sim_s    = apply(smat, 1, median),
           sim_s_lo = apply(smat, 1, quantile, 0.025),
           sim_s_hi = apply(smat, 1, quantile, 0.975)))
}


# 11d. The two simulated figures -----------------------------------------------

# Same two panels as section 10, from the same model and the same
# counterfactuals. The only difference is where the ribbon came from: percentiles
# of 1,000 draws here, sqrt(g'Vg) there.
sim_plot <- sim_qi %>%
  mutate(polity = factor(polity, levels = pvals,
                         labels = c("Polity -10", "Polity 0", "Polity +10")))

ggplot(sim_plot, aes(t, sim_h, color = polity, fill = polity, linetype = polity)) +
  geom_ribbon(aes(ymin = sim_h_lo, ymax = sim_h_hi), alpha = 0.45, color = NA) +
  geom_line(linewidth = 0.9) +
  scale_color_manual(values = c(bu_black, bu_lightgreen, bu_green)) +
  scale_fill_manual(values = c(bu_black, bu_lightgreen, bu_green)) +
  scale_linetype_manual(values = bu_lty[1:3]) +
  labs(title = "Annual hazard of leaving office",
       subtitle = "95% simulated intervals: 2.5th and 97.5th percentiles of 1,000 draws",
       x = "Years in office", y = "h(t)",
       color = NULL, fill = NULL, linetype = NULL) +
  theme_bu() + theme(legend.position = "bottom")

ggplot(sim_plot, aes(t, sim_s, color = polity, fill = polity, linetype = polity)) +
  geom_ribbon(aes(ymin = sim_s_lo, ymax = sim_s_hi), alpha = 0.45, color = NA) +
  geom_line(linewidth = 0.9) +
  geom_hline(yintercept = 0.5, color = bu_grey, linetype = "dashed", linewidth = 0.4) +
  scale_color_manual(values = c(bu_black, bu_lightgreen, bu_green)) +
  scale_fill_manual(values = c(bu_black, bu_lightgreen, bu_green)) +
  scale_linetype_manual(values = bu_lty[1:3]) +
  labs(title = "Share of leaders still in office",
       subtitle = "95% simulated intervals; each draw's own 25 hazards chained into S(t)",
       x = "Years in office", y = "S(t)",
       color = NULL, fill = NULL, linetype = NULL) +
  theme_bu() + theme(legend.position = "bottom")





M <- matrix(1:6, nrow = 2)     # 2 x 3

apply(M, 1, sum)     # 9 12      -- 2 values, one per ROW    (rows kept)
apply(M, 2, sum)     # 3 7 11    -- 3 values, one per COLUMN (columns kept)