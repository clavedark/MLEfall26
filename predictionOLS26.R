# ============================================================================
# predictionOLS26.R
# Prediction methods in the linear model (OLS) -- live-demo script
# Companion to predictionOLS26.qmd. A preview, for the MLE class, of the
# prediction toolkit built in prediction26.qmd, worked in the familiar OLS
# setting where the "link" is the identity.
#
# Each concept appears TWICE:
#   [explicit]   -- the transparent code. The loops and hand-built data frames
#                   SHOW the process; that is why we learn them this way, and
#                   why they're here.
#   [efficient]  -- the streamlined version (model.matrix, map_dfr,
#                   marginaleffects, MASS::mvrnorm). Same result, less to get
#                   wrong, and it generalizes to the nonlinear models.
# Both run. Step through them side by side.
# ============================================================================

## ---- setup -----------------------------------------------------------------
library(tidyverse)
library(patchwork)
library(mvtnorm)       # rmvnorm, the manual parameter simulator
library(stargazer)     # the transparent-style table
library(modelsummary)  # the MLE-class table
library(MASS)          # mvrnorm, for the vectorized simulation (load AFTER dplyr;
                       # MASS::select masks dplyr::select -- use dplyr::select if needed)
library(marginaleffects)

# Binghamton palette + theme helpers (bu_green, bu_lightgreen, bu_black, ...).
# Run from inside MLEfall26/ so here::here() finds the project root.
source(here::here("_common.R"))

set.seed(20260811)


# ============================================================================
# 1. The running model
# ============================================================================

## ---- [explicit] read.csv from absolute path, %>% pipeline, stargazer ------
itt <- read.csv("/Users/dave/Documents/teaching/501/2023/exercises/ex4/ITT/data/ITT.csv")

itt$p1 <- itt$polity2 + 11        # shift Polity to 0-20
itt$p2 <- itt$p1^2                # quadratic Polity term (stays in the model; it's flat)
itt$loggdp <- log(itt$wdi_gdpc)   # GDP per capita is skewed, so log it

itt <- itt %>%
  group_by(ccode) %>%
  mutate(lagprotest = lag(protest), lagRA = lag(RstrctAccess), n = 1)

m1 <- lm(scarring ~ lagRA + civilwar + lagprotest + p1 + p2 + loggdp, data = itt)

stargazer(m1, type = "text", digits = 3, omit.stat = c("LL", "ser"))

## ---- [efficient] here::here + read_csv, modelsummary ---------------
# WHY: here::here + read_csv is machine-independent (the absolute path only
# works on my laptop); modelsummary is actively maintained and tables marginal
# effects, which stargazer cannot.
itt2 <- readr::read_csv(here::here("data", "ITT.csv"), show_col_types = FALSE) %>%
  group_by(ccode) %>%
  mutate(lagprotest = lag(protest), lagRA = lag(RstrctAccess)) %>%
  ungroup() %>%
  mutate(p1 = polity2 + 11, p2 = p1^2, loggdp = log(wdi_gdpc))

m1b <- lm(scarring ~ lagRA + civilwar + lagprotest + p1 + p2 + loggdp, data = itt2)

modelsummary(m1b,
  coef_rename = c("lagRA" = "Restricted access, t-1", "civilwar" = "Civil war",
                  "lagprotest" = "Protests, t-1", "p1" = "Polity",
                  "p2" = "Polity squared", "loggdp" = "GDP per capita (log)"),
  statistic = "std.error", gof_omit = "AIC|BIC|RMSE|Log")


# ============================================================================
# 2. Estimation sample + central tendencies (reused throughout)
# ============================================================================
itt$used <- TRUE
itt$used[na.action(m1)] <- FALSE
estdata <- itt %>% filter(used == "TRUE")

p1med  <- median(estdata$p1)          # Polity median (16 => polity2 = 5)
p2med  <- p1med^2
gdpmed <- median(estdata$loggdp)
promed <- median(estdata$lagprotest)

cat("N used:", nrow(estdata), "of", nrow(itt), "\n")


# ============================================================================
# 3. Building a scenario's linear prediction -- three ways
#    (In OLS, x'b IS y-hat: the identity link.)
# ============================================================================
b <- coef(m1)

## ---- [explicit] by hand, pulling coefficients by name ---------------------
xb_hand <- b["(Intercept)"] + b["lagRA"]*0 + b["civilwar"]*0 +
           b["lagprotest"]*promed + b["p1"]*p1med + b["p2"]*p2med + b["loggdp"]*gdpmed
as.numeric(xb_hand)

## ---- [efficient] model.matrix builds the design row from the formula ----------
# WHY: with a quadratic (or an interaction/factor) the by-hand version makes you
# track every product term yourself; model.matrix reads the formula and can't
# drop a term.
scenario <- data.frame(lagRA = 0, civilwar = 0, lagprotest = promed,
                       p1 = p1med, p2 = p2med, loggdp = gdpmed)
# model.matrix() builds the "design matrix" from a formula + data. The design
# matrix is just X in y = Xb -- the matrix of right-hand-side variables the model
# multiplies by the coefficients: one row per observation, one column per term,
# with a leading column of 1s for the intercept. (In our field we usually just
# call it the X matrix, or the matrix of covariates.) model.matrix() builds it
# from the formula -- intercept, dummies for factors, interaction/polynomial
# columns. Here it returns the one design row (one row of X) for our scenario
# (1 x 7), in the same column order as coef(m1), so X_row %*% coef(m1) is x'b.
X_row <- model.matrix(~ lagRA + civilwar + lagprotest + p1 + p2 + loggdp, data = scenario)
as.numeric(X_row %*% coef(m1))

predict(m1, newdata = scenario)   # fully automated -- and in OLS this equals x'b


# ============================================================================
# 4. At-means predictions over PROTEST (direct computation + EPT)
# ============================================================================

## ---- [explicit] build oosdata, predict(se.fit), EPT by hand ---------------
oosdata <- data.frame(lagRA = 0, civilwar = 0, p1 = p1med, p2 = p2med,
                      loggdp = gdpmed, lagprotest = c(seq(0, 37, 1)))

itt.predict <- data.frame(oosdata,
  predict(m1, interval = "confidence", se.fit = TRUE, level = .05, newdata = oosdata))

itt.predict <- itt.predict %>% mutate(ub = fit.fit + 1.96*se.fit) %>%
                               mutate(lb = fit.fit - 1.96*se.fit)

atmean <- ggplot(itt.predict, aes(x = lagprotest, y = fit.fit)) +
  geom_line(color = bu_green, linewidth = 1) +
  geom_ribbon(aes(ymin = lb, ymax = ub), fill = bu_green, alpha = .2) +
  xlab("Protests (t-1)") + ylab("Expected scarring torture") +
  ggtitle("At-mean effects") + theme_minimal()
print(atmean)

## ---- [efficient] marginaleffects::predictions() does grid + SE + interval ------
# WHY: no hand-built grid, no by-hand +/- 1.96*se, and it generalizes unchanged
# to logit/probit/count models -- swap the model and it handles link + delta SE.
am <- predictions(m1, newdata = datagrid(lagprotest = 0:37, lagRA = 0,
                  civilwar = 0, p1 = p1med, p2 = p2med, loggdp = gdpmed))
am_plot <- ggplot(am, aes(lagprotest, estimate)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), fill = bu_green, alpha = .2) +
  geom_line(color = bu_green, linewidth = 1) +
  xlab("Protests (t-1)") + ylab("Expected scarring torture") +
  ggtitle("At-mean effects (marginaleffects)") + theme_minimal()
print(am_plot)


# ============================================================================
# 5. Average effects over PROTEST
# ============================================================================

## ---- [explicit] explicit loop over the estimation sample ------------------
estdata <- estdata %>% mutate(original_lagprotest = lagprotest)

xb <- 0; se <- 0; pr <- 0
for (i in seq(0, 37, 1)) {
  estdata$lagprotest <- i
  ap <- data.frame(predict(m1, interval = "confidence", se.fit = TRUE,
                           level = .05, newdata = estdata))
  xb[i + 1] <- median(ap$fit.fit)
  se[i + 1] <- median(ap$se.fit)
  pr[i + 1] <- i
}
avg.pred <- data.frame(xb, se, pr) %>%
  mutate(ub = xb + 1.96*se, lb = xb - 1.96*se)
estdata <- estdata %>% mutate(lagprotest = original_lagprotest)

average <- ggplot(avg.pred, aes(x = pr, y = xb)) +
  geom_line(color = bu_green, linewidth = 1) +
  geom_ribbon(aes(ymin = lb, ymax = ub), fill = bu_green, alpha = .2) +
  xlab("Protests (t-1)") + ylab("Expected scarring torture") +
  ggtitle("Average effects") + theme_minimal()
print(average)

## ---- [efficient] map_dfr, or marginaleffects::avg_predictions -----------------
# WHY: (a) map_dfr removes the "i + 1" indexing and the save/restore dance;
#      (b) avg_predictions IS the average effect, with a proper delta-method SE
#          (the loop's median-of-SEs is only a rough band). Note: the average
#          effect is properly the MEAN of the predictions, not the median.
# avg_curve() takes ONE protest value; it copies the estimation data (estdata,
# from section 2), sets protest to that value for every row, and returns a one-row
# tibble of the value and the mean prediction. `value` is its only argument;
# estdata and m1 come from the environment.
avg_curve <- function(value) {
  cf <- estdata; cf$lagprotest <- value
  tibble(pr = value, xb = mean(predict(m1, newdata = cf)))
}
# map_dfr() is the loop's replacement: it calls avg_curve() once for each element
# of 0:37 (so `value` is 0, then 1, ... 37) and row-binds the 38 one-row tibbles
# into a single data frame -- no counter, no i+1 indexing.
avg_map <- map_dfr(0:37, avg_curve)

avg_me <- avg_predictions(m1, variables = list(lagprotest = 0:37))
head(avg_me)


# ============================================================================
# 6. Analytic standard errors
# ============================================================================

## ---- ML SE of the linear prediction: sqrt(diag(X V X')) --------------------
Xg <- model.matrix(~ lagRA + civilwar + lagprotest + p1 + p2 + loggdp, data = oosdata)
V  <- vcov(m1)
xb_grid <- as.vector(Xg %*% coef(m1))
se_lin  <- sqrt(diag(Xg %*% V %*% t(Xg)))

# check: predict()'s SE IS this matrix computation
data.frame(lagprotest = oosdata$lagprotest, se_byhand = se_lin,
           se_predict = predict(m1, newdata = oosdata, se.fit = TRUE)$se.fit)[1:5, ]

## ---- Delta method for a linear combination (SE of a marginal effect) -------
# The quadratic marginal effect of Polity = b_p1 + 2*b_p2*polity. Its SE must
# carry Cov(b_p1, b_p2). (This is the reason the flat quadratic stays in the
# model -- it's the vehicle for teaching the linear-combination SE. Predictions
# themselves are generated over protest, not Polity.)
b <- coef(m1); V <- vcov(m1)
# Two small functions, each taking a Polity value p (or a vector of them).
# me_polity() returns the marginal effect b_p1 + 2*b_p2*p; se_polity() returns its
# delta-method SE. Both read b and V from the line just above (not passed as
# arguments). Called below on the vector `poly`, they return one value per Polity.
me_polity <- function(p) b["p1"] + 2*b["p2"]*p
se_polity <- function(p) sqrt(V["p1","p1"] + 4*p^2*V["p2","p2"] + 4*p*V["p1","p2"])

poly <- seq(0, 20, 2)
data.frame(polity = poly,
           marg_effect = as.numeric(me_polity(poly)),
           se          = as.numeric(se_polity(poly))) %>%
  mutate(lb = marg_effect - 1.96*se, ub = marg_effect + 1.96*se)

## ---- [efficient] marginaleffects::slopes() -- no hand-written derivative -------
# WHY: you never transcribe "b1 + 2*b2*x" or the variance formula;
# marginaleffects differentiates numerically and pulls covariances from V.
slopes(m1, variables = "p1", newdata = datagrid(p1 = seq(0, 20, 2)))


# ============================================================================
# 7. Simulation, reading 1: PARAMETERS -> expected value -> CONFIDENCE interval
# ============================================================================

## ---- [explicit] rmvnorm, loop over protest, quantiles of x'b* -------------
B <- data.frame(rmvnorm(n = 10000, mean = coef(m1), sigma = vcov(m1)))
colnames(B) <- c("b0", "b_ra", "b_cw", "b_pro", "b_p1", "b_p2", "b_gdp")

sim.exp <- data.frame(lb = numeric(0), med = numeric(0), ub = numeric(0), pr = numeric(0))
for (i in seq(0, 37, 1)) {
  xbR <- quantile(B$b0 + B$b_ra*0 + B$b_cw*0 + B$b_pro*i +
                  B$b_p1*p1med + B$b_p2*p2med + B$b_gdp*gdpmed,
                  probs = c(.025, .5, .975))
  sim.exp[i + 1, 1:4] <- data.frame(t(xbR), i)
}
head(sim.exp)

## ---- [efficient] MASS::mvrnorm + one matrix multiply, no loop -----------------
# WHY: the matrix multiply replaces the loop and the fragile indexing, and
# model.matrix means you never re-type the linear predictor by hand.
sims <- MASS::mvrnorm(10000, coef(m1), vcov(m1))
XB   <- Xg %*% t(sims)                                   # 38 x 10000
sim.exp2 <- data.frame(t(apply(XB, 1, quantile, c(.025, .5, .975))),
                       pr = oosdata$lagprotest)
head(sim.exp2)


# ============================================================================
# 8. Simulation, reading 2: QI (add sigma) -> predicted value -> PREDICTION int.
# ============================================================================
sigma_hat <- sigma(m1)   # residual SD: the scatter of a single case around the line

sim.pred <- data.frame(lb = numeric(0), med = numeric(0), ub = numeric(0), pr = numeric(0))
for (i in seq(0, 37, 1)) {
  xstar <- B$b0 + B$b_pro*i + B$b_p1*p1med + B$b_p2*p2med + B$b_gdp*gdpmed  # binaries = 0
  ystar <- xstar + rnorm(length(xstar), mean = 0, sd = sigma_hat)          # fundamental noise
  q <- quantile(ystar, probs = c(.025, .5, .975))
  sim.pred[i + 1, 1:4] <- data.frame(t(q), i)
}
head(sim.pred)

## ---- check the two simulations against predict()'s CI and PI ---------------
check <- data.frame(lagprotest = oosdata$lagprotest,
  predict(m1, newdata = oosdata, interval = "confidence"),
  predict(m1, newdata = oosdata, interval = "prediction")) %>%
  rename(ci_lb = lwr, ci_ub = upr, pi_lb = lwr.1, pi_ub = upr.1)

two_sims <- ggplot() +
  geom_ribbon(data = sim.pred, aes(x = pr, ymin = lb, ymax = ub),
              fill = bu_lightgreen, alpha = .25) +
  geom_ribbon(data = sim.exp,  aes(x = pr, ymin = lb, ymax = ub),
              fill = bu_green, alpha = .35) +
  geom_line(data = sim.exp, aes(x = pr, y = med), color = bu_green, linewidth = 1) +
  geom_line(data = check, aes(lagprotest, ci_lb), color = bu_black, linetype = "dashed") +
  geom_line(data = check, aes(lagprotest, ci_ub), color = bu_black, linetype = "dashed") +
  geom_line(data = check, aes(lagprotest, pi_lb), color = bu_black, linetype = "dotted") +
  geom_line(data = check, aes(lagprotest, pi_ub), color = bu_black, linetype = "dotted") +
  geom_hline(yintercept = 0, color = bu_black, linewidth = .3) +
  annotate("text", x = 2, y = 11, label = "confidence interval\n(expected value)",
           hjust = 0, size = 3, color = bu_green) +
  annotate("text", x = 20, y = 26, label = "prediction interval\n(predicted value)",
           hjust = 0, size = 3, color = bu_black) +
  labs(title = "Two simulations: expected value vs. predicted value",
       subtitle = "solid/dark band = confidence interval; pale band = prediction interval; dashed/dotted = predict()",
       x = "Protests (t-1)", y = "Scarring torture") + theme_minimal()
print(two_sims)
# Note the prediction interval dips BELOW zero -- OLS ignores that scarring is a
# bounded count. That's the advertisement for the count models in a few weeks.


# ============================================================================
# 9. Simulating combinations of binary variables (restricted access x civil war)
# ============================================================================
# xb_scen() builds the simulated linear prediction for one access/war scenario.
# Arguments: ra (restricted access, 0/1) and cw (civil war, 0/1). The rest come
# from the environment -- the simulated coefficient columns B$... (section 7) and
# the held medians promed/p1med/p2med/gdpmed (section 2). Returns a full column of
# 10,000 simulated predictions. Called four times, once per ra/cw combination.
xb_scen <- function(ra, cw) {
  B$b0 + B$b_ra*ra + B$b_cw*cw + B$b_pro*promed +
  B$b_p1*p1med + B$b_p2*p2med + B$b_gdp*gdpmed
}
combos <- data.frame(ra0cw0 = xb_scen(0, 0), ra1cw0 = xb_scen(1, 0),
                     ra0cw1 = xb_scen(0, 1), ra1cw1 = xb_scen(1, 1))

combo_plot <- ggplot(combos) +
  geom_density(aes(ra0cw0), fill = bu_green,      alpha = .3) +
  geom_density(aes(ra1cw0), fill = bu_lightgreen, alpha = .3) +
  geom_density(aes(ra0cw1), fill = bu_palegreen,  alpha = .3) +
  geom_density(aes(ra1cw1), fill = bu_grey,       alpha = .4) +
  annotate("text", x = 5.7,  y = .55, label = "open access,\nno civil war",   color = bu_black, size = 3) +
  annotate("text", x = 13.6, y = .45, label = "open access,\ncivil war",      color = bu_black, size = 3) +
  annotate("text", x = 16.8, y = .35, label = "restricted access,\nno civil war", color = bu_green, size = 3) +
  annotate("text", x = 24.7, y = .30, label = "restricted access,\ncivil war",    color = bu_black, size = 3) +
  labs(title = "Simulated expected scarring for four scenarios",
       x = "Expected scarring torture", y = "density") + theme_minimal()
print(combo_plot)


# ============================================================================
# 10. Average effects, two ways: delta vs simulation (over protest)
# ============================================================================
X <- model.matrix(m1)     # design matrix R built when it fit m1 (n x k)
b <- coef(m1); V <- vcov(m1)

## ---- delta: average prediction is linear in b, gradient = colMeans(X) ------
# map_dfr (section 5) calls the inline anonymous function once per protest value v
# in 0:37 and row-binds the results. The function takes v and reads X, b, V from
# the environment; for each v it copies X, sets everyone's protest to v, and
# returns a one-row data frame with the average prediction and its delta interval.
ae_delta <- map_dfr(seq(0, 37, 1), function(v) {
  Xi <- X; Xi[, "lagprotest"] <- v
  ybar <- mean(as.vector(Xi %*% b))
  g    <- colMeans(Xi)
  se   <- sqrt(as.numeric(t(g) %*% V %*% g))
  data.frame(pr = v, avg = ybar, lb = ybar - 1.96*se, ub = ybar + 1.96*se)
})

## ---- simulation: average the predictions within each draw ------------------
# Same map_dfr + inline-function pattern as the delta version; the difference is
# inside -- average the 10,000 simulated predictions and take percentiles.
ae_sim <- map_dfr(seq(0, 37, 1), function(v) {
  Xi <- X; Xi[, "lagprotest"] <- v
  draws <- colMeans(Xi %*% t(sims))
  q <- quantile(draws, c(.025, .5, .975))
  data.frame(pr = v, avg = q[2], lb = q[1], ub = q[3])
})

ae_plot <- bind_rows(mutate(ae_delta, method = "delta"),
                     mutate(ae_sim,   method = "simulation")) %>%
  ggplot(aes(pr, avg)) +
  geom_ribbon(aes(ymin = lb, ymax = ub, fill = method), alpha = .3) +
  geom_line(aes(color = method), linewidth = 1) +
  scale_fill_manual(values = bu_two) + scale_color_manual(values = bu_two) +
  labs(title = "Average effects, two uncertainty techniques",
       subtitle = "delta-method and simulated intervals on the same average predictions",
       x = "Protests (t-1)", y = "average scarring torture") + theme_minimal()
print(ae_plot)
# In OLS these coincide EXACTLY: the average prediction is linear in b, so the
# delta method's first-order approximation has nothing to drop. Under a
# nonlinear link (in MLE) they separate near 0/1.

# ============================================================================
# end
# ============================================================================
