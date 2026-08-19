# ==============================================================================
# binarymodels26.R
# Code from binarymodels26.qmd -- "Binary Response Models" (606J-MLE, Fall 2026).
# Auto-extracted with knitr::purl(); each block below is marked
#   ## ----<chunk label>----   matching the chunk in the .qmd.
#
# HOW TO RUN: open from the MLEfall26/ project folder (or setwd() there) so that
# here::here(), source("_common.R"), and the data/ files all resolve. Then run
# top to bottom, or step through block by block for a live demo.
# Packages used (loaded in the setup block): tidyverse, haven, patchwork,
# modelsummary, broom, marginaleffects.
# ==============================================================================

## ----setup, include=FALSE, echo=FALSE, warning=FALSE--------------------------
options(htmltools.dir.version = FALSE)
knitr::opts_chunk$set(fig.retina = 2, fig.align = "center",
                      warning = FALSE, error = FALSE, message = FALSE)

library(here)
library(tidyverse)
library(haven)
library(patchwork)
library(modelsummary)
library(broom)
library(marginaleffects)

# Binghamton palette + plotting helpers (bucolors, bu_green, scale_color_bu, ...)
source(here::here("_common.R"))

set.seed(20260520)


## ----load-data----------------------------------------------------------------
#| echo: false
#| code-fold: false

dp <- read_dta(here::here("data", "dp.dta"))
dp$lncaprat <- log(dp$caprat)   # a capability ratio is multiplicative, so model it on the log scale


## ----lpm-vs-logit, message=FALSE, warning=FALSE-------------------------------
#| echo: true
#| code-fold: true
#| code-summary: "code"
#| label: fig-lpm-bounds

# I fit the two binary models and OLS on the same specification, to compare
# what each one predicts.
m_logit <- glm(dispute ~ border + deml + lncaprat + ally,
               family = binomial(link = "logit"), data = dp)
m_probit <- glm(dispute ~ border + deml + lncaprat + ally,
                family = binomial(link = "probit"), data = dp)
m_ols   <- lm(dispute ~ border + deml + lncaprat + ally, data = dp)

preds <- tibble(
  logit   = predict(m_logit, type = "response"),
  ols     = predict(m_ols),
  dispute = factor(dp$dispute)
)

n_oob <- sum(preds$ols < 0)   # OLS predictions below the probability floor

ggplot(preds, aes(x = logit, y = ols, color = dispute)) +
  geom_point(alpha = 0.5) +
  geom_hline(yintercept = 0) +
  annotate("text", x = 0.11, y = -0.11,
           label = paste(format(n_oob, big.mark = ","), "OLS predictions below 0"),
           color = "red", size = 3.5) +
  scale_color_manual(values = bu_two) +
  labs(title = "Predictions from logit and OLS",
       x = "logit prediction (probability)",
       y = "OLS prediction") +
  theme_minimal()


## ----lpm-residuals------------------------------------------------------------
#| echo: true
#| code-fold: true
#| code-summary: "code"

tibble(resid = residuals(m_ols), dispute = factor(dp$dispute)) %>%
  ggplot(aes(x = resid, color = dispute)) +
  geom_density() +
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_color_manual(values = bu_two) +
  labs(title = "Density of OLS residuals", x = "residual", y = "density") +
  theme_minimal()


## ----latent-threshold---------------------------------------------------------
#| echo: true
#| code-fold: true
#| code-summary: "code"
#| label: fig-latent-threshold
#| fig-width: 9
#| fig-height: 6.8

# TOP: the latent bell y* = xb + e ~ N(xb, 1); the shaded area past the fixed
# threshold at 0 is Pr(y=1). That area = 1 - Phi(-xb) = Phi(xb) (standardize --
# the threshold sits -xb SDs from the mean). BOTTOM: plot each shaded area
# against xb and it traces the CDF Phi, so the shaded area up top IS the CDF
# height below. Drawn for a normal error (probit); logit tells the same story.
xb_vals <- c(-1.5, 0, 1.5)
strip   <- sprintf("xb = %g   (shaded area = %.2f)", xb_vals, pnorm(xb_vals))

dens_df <- tibble(xb    = rep(xb_vals, each = 400),
                  ystar = rep(seq(-5, 5, length.out = 400), times = 3)) %>%
  mutate(dens  = dnorm(ystar, mean = xb, sd = 1),
         facet = factor(sprintf("xb = %g   (shaded area = %.2f)", xb, pnorm(xb)),
                        levels = strip))
shaded <- filter(dens_df, ystar > 0)

# Top row: the three bells, shaded area past the threshold = Pr(y=1) = Phi(xb).
p_bells <- ggplot(dens_df, aes(ystar, dens)) +
  geom_area(data = shaded, fill = bu_lightgreen, alpha = 0.7) +
  geom_line(color = bu_green, linewidth = 0.7) +
  geom_vline(xintercept = 0, linetype = "dashed") +           # the fixed threshold
  facet_wrap(~ facet, nrow = 1) +
  labs(title = "Top: the shaded area past the threshold is Pr(y=1) = Phi(xb)",
       x = expression(y^"*"~"(latent variable)"), y = NULL) +
  theme_minimal()

# Bottom row: those areas plotted against xb -- this traces the CDF, the link.
cdf_df <- tibble(xb = seq(-5, 5, length.out = 400), p = pnorm(xb))
marks  <- tibble(xb = xb_vals, p = pnorm(xb_vals))

p_cdf <- ggplot(cdf_df, aes(xb, p)) +
  geom_line(color = bu_green, linewidth = 1) +
  geom_segment(data = marks, aes(x = xb, xend = xb, y = 0, yend = p),
               color = bu_grey, linetype = "dotted") +        # read up from xb
  geom_segment(data = marks, aes(x = -5, xend = xb, y = p, yend = p),
               color = bu_grey, linetype = "dotted") +        # ... across to the height
  geom_point(data = marks, aes(xb, p), color = bu_black, size = 2) +
  geom_text(data = marks, aes(xb, p, label = sprintf("(%g, %.2f)", xb, p)),
            vjust = -0.9, hjust = -0.05, size = 3.1, color = bu_black) +
  labs(title = "Bottom: plot each shaded area against xb and you trace the CDF -- the link F",
       x = expression(x*beta), y = expression(Phi(x*beta))) +
  theme_minimal()

p_bells / p_cdf


## ----mapping-viz--------------------------------------------------------------
#| echo: true
#| code-fold: true
#| code-summary: "code"
#| label: fig-mapping
#| fig-width: 8
#| fig-height: 5

# The two CDFs as translators from the linear predictor to a probability,
# with a few (xb, pi) pairs marked on the logit curve.
xb <- seq(-5, 5, length.out = 400)
curve_df <- tibble(xb = xb, logit = plogis(xb), probit = pnorm(xb))

marks <- tibble(xb = c(-2, 0, 2)) %>% mutate(pi = plogis(xb))

ggplot(curve_df, aes(xb, logit)) +
  geom_line(color = bu_green, linewidth = 1) +
  geom_line(aes(y = probit), color = bu_lightgreen, linewidth = 1, linetype = "dashed") +
  geom_segment(data = marks,
               aes(x = xb, xend = xb, y = 0, yend = pi),
               color = bu_grey, linetype = "dotted") +
  geom_segment(data = marks,
               aes(x = -5, xend = xb, y = pi, yend = pi),
               color = bu_grey, linetype = "dotted") +
  geom_point(data = marks, aes(xb, pi), color = bu_black, size = 2) +
  geom_text(data = marks,
            aes(xb, pi, label = sprintf("(%g, %.2f)", xb, pi)),
            vjust = -0.8, hjust = -0.05, size = 3.4) +
  annotate("text", x = 3.5, y = 0.6, label = "logit~(Lambda)", parse = TRUE, color = bu_green) +
  annotate("text", x = 1.5, y = 0.95, label = "probit~(Phi)", parse = TRUE, color = bu_black) +
  scale_y_continuous(limits = c(0, 1.05)) +
  labs(title = "Link function as translator: from linear predictor to probability",
       x = expression(x*beta~"(linear prediction)"),
       y = expression(pi == F(x*beta))) +
  theme_minimal()


## ----estimate-----------------------------------------------------------------
#| echo: true
#| code-fold: true
#| code-summary: "code"

modelsummary(
  list("Logit" = m_logit, "Probit" = m_probit),
  coef_rename = c("border" = "Shared border",
                  "deml"   = "Min. democracy",
                  "lncaprat" = "Capabilities ratio (log)",
                  "ally"   = "Allied"),
  statistic = "std.error",
  gof_omit  = "AIC|BIC|RMSE|F|Log",
  notes     = "Standard errors in parentheses."
)


## ----predicted-probs----------------------------------------------------------
#| echo: true
#| code-fold: true
#| code-summary: "code"

# At-means (adjusted) predictions: democracy varies across its range; the other
# x-variables are held at representative values -- the mode for the binary
# covariates (border, ally), the median for skewed capability ratio.
# (datagrid's default would hold numerics at their mean; I set them
# to the mode/median to address skewness)
modal <- function(x) as.numeric(names(which.max(table(x))))

preds_dem <- predictions(
  m_logit,
  newdata = datagrid(deml   = seq(-10, 10, length.out = 21),
                     border = modal(dp$border),
                     ally   = modal(dp$ally),
                     lncaprat = median(dp$lncaprat, na.rm = TRUE))
)

ggplot(preds_dem, aes(deml, estimate)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
              fill = bu_green, alpha = 0.2) +
  geom_line(color = bu_green, linewidth = 1) +
  labs(title = "Predicted Pr(dispute = 1) across democracy",
       x = "minimum dyad democracy (Polity)",
       y = "Pr(y = 1)") +
  theme_minimal()


## ----linear-me----------------------------------------------------------------
#| echo: true
#| code-fold: true
#| code-summary: "code"

me_line <- tibble(x = seq(0, 10, 0.1), y = 2 * x)

ggplot(me_line, aes(x, y)) +
  geom_line(color = bu_green, linewidth = 1) +
  annotate("segment", x = 5,  xend = 5,  y = 0, yend = 10, color = "red") +
  annotate("segment", x = 10, xend = 10, y = 0, yend = 20, color = "red") +
  annotate("text", x = 3.5, y = 16, label = "y = 2x") +
  labs(title = "A linear marginal effect is constant", x = "x", y = "y") +
  theme_minimal()


## ----slide-me-----------------------------------------------------------------
#| echo: true
#| code-fold: true
#| code-summary: "code"
#| label: fig-slide-me
#| fig-width: 8
#| fig-height: 5

# The marginal effect of a covariate is the SLOPE of the link at the point where
# a case sits. For the logit that slope is the density lambda(xb), which works out
# to Lambda(xb)*(1-Lambda(xb)) = Pr(1)*Pr(0) (derived in "Logit marginal effects"
# below):
# tallest in the middle (0.25 at xb = 0, where Pr = .5), flat in the tails. A
# case's other covariates set WHERE on the curve it sits -- raise them and it
# slides toward the steep middle, lower them and it slides into the flat tail.
# Same coefficient, different effect.
sig <- tibble(xb = seq(-6, 6, length.out = 400), p = plogis(xb))

pts <- tibble(xb = c(-3, 0, 3)) %>%          # three positions on the curve
  mutate(p = plogis(xb), slope = p * (1 - p))

d <- 1.1                                      # half-length of each tangent, in xb units
tangents <- pts %>%
  mutate(x0 = xb - d, x1 = xb + d,
         y0 = p - slope * d, y1 = p + slope * d)

ggplot(sig, aes(xb, p)) +
  geom_line(color = bu_green, linewidth = 1) +
  geom_segment(data = tangents, aes(x = x0, xend = x1, y = y0, yend = y1),
               color = bu_black, linewidth = 0.8) +
  geom_point(data = pts, aes(xb, p), color = bu_black, size = 2) +
  geom_text(data = pts, aes(xb, p, label = sprintf("slope = %.2f", slope)),
            vjust = -1.3, size = 3.3) +
  annotate("segment", x = -3.3, xend = 3.3, y = -0.10, yend = -0.10,
           arrow = arrow(length = unit(0.15, "cm"), ends = "both"), color = bu_black) +
  annotate("text", x = 0, y = -0.16, size = 3, color = bu_black,
           label = "a case's other covariates slide it along the curve") +
  scale_y_continuous(limits = c(-0.18, 1.08)) +
  labs(title = "The marginal effect is the slope of the link -- steepest in the middle",
       x = expression(x*beta), y = expression(pi == Lambda(x*beta))) +
  theme_minimal()


## ----max-me-------------------------------------------------------------------
#| echo: true
#| code-fold: true
#| code-summary: "code"
#| label: fig-max-me
#| fig-width: 8
#| fig-height: 5

# Each link's PDF is the slope of its CDF, and the PDF's peak (at xb = 0) is the
# largest marginal-effect weight: 0.25 for the logit, 0.399 for the probit.
# Colors match fig-mapping: logit = green (solid), probit = light green (dashed).
z    <- seq(-5, 5, length.out = 400)
dens <- tibble(z = z,
               logit_cdf  = plogis(z), logit_pdf  = dlogis(z),
               probit_cdf = pnorm(z),  probit_pdf = dnorm(z))

ggplot(dens, aes(z)) +
  geom_line(aes(y = logit_cdf),  color = bu_green,      linewidth = 1) +
  geom_line(aes(y = probit_cdf), color = bu_lightgreen, linewidth = 1, linetype = "dashed") +
  geom_line(aes(y = logit_pdf),  color = bu_green,      linewidth = 0.8) +
  geom_line(aes(y = probit_pdf), color = bu_lightgreen, linewidth = 0.8, linetype = "dashed") +
  geom_hline(yintercept = 0.25,   color = bu_green,      linetype = "dotted") +
  geom_hline(yintercept = 0.3989, color = bu_lightgreen, linetype = "dotted") +
  annotate("text", x = -4.8, y = 0.28, hjust = 0, size = 3, color = bu_green,
           label = "logit PDF peak = 0.25") +
  annotate("text", x = -4.8, y = 0.42, hjust = 0, size = 3, color = bu_black,
           label = "probit PDF peak = 0.399") +
  annotate("text", x = 3.4,  y = 0.96, size = 3, color = bu_black, label = "CDF = the link") +
  annotate("text", x = 1.15, y = 0.33, size = 3, color = bu_black, label = "PDF = its slope") +
  labs(title = "The link's slope is its PDF; the PDF's peak is the largest marginal effect",
       x = expression(x*beta), y = "link (CDF) and its slope (PDF)") +
  theme_minimal()


## ----marginal-effects---------------------------------------------------------
#| echo: true
#| code-fold: true
#| code-summary: "code"

ames <- avg_slopes(m_logit)

ames %>%
  as_tibble() %>%
  dplyr::select(term, estimate, std.error, conf.low, conf.high) %>%
  knitr::kable(digits = 3, caption = "Average marginal effects, logit")

