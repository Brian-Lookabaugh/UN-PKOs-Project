############################################################################
###############-----------------PKO Results------------------###############
############################################################################

pacman::p_load(
  "broom", # For augment_columns
  "sensemakr", # Sensitivity Analysis
  install = FALSE
)

# Run Panel Data IPW Models
pko.dep.1 <- PanelEstimate(
  sets = ipw.dep.1,
  data = merged,
  se.method = "conditional",
  number.iterations = 500,
  confidence.level = .90
)

alt.pko.dep.1 <- PanelEstimate(
  sets = alt.dep.ipw.1,
  data = merged,
  se.method = "conditional",
  number.iterations = 500,
  confidence.level = .90
)

pko.dep.2 <- PanelEstimate(
  sets = ipw.dep.2,
  data = merged,
  se.method = "conditional",
  number.iterations = 500,
  confidence.level = .90
)

alt.pko.dep.2 <- PanelEstimate(
  sets = alt.dep.ipw.2,
  data = merged,
  se.method = "conditional",
  number.iterations = 500,
  confidence.level = .90
)

pko.dep.3 <- PanelEstimate(
  sets = ipw.dep.3,
  data = merged,
  se.method = "conditional",
  number.iterations = 500,
  confidence.level = .90
)

alt.pko.dep.3 <- PanelEstimate(
  sets = alt.dep.ipw.3,
  data = merged,
  se.method = "conditional",
  number.iterations = 500,
  confidence.level = .90
)

pko.wth.1 <- PanelEstimate(
  sets = ipw.wth.1,
  data = merged,
  se.method = "conditional",
  number.iterations = 500,
  confidence.level = .90
)

alt.pko.wth.1 <- PanelEstimate(
  sets = alt.wth.ipw.1,
  data = merged,
  se.method = "conditional",
  number.iterations = 500,
  confidence.level = .90
)

pko.wth.2 <- PanelEstimate(
  sets = ipw.wth.2,
  data = merged,
  se.method = "conditional",
  number.iterations = 500,
  confidence.level = .90
)

alt.pko.wth.2 <- PanelEstimate(
  sets = alt.wth.ipw.2,
  data = merged,
  se.method = "conditional",
  number.iterations = 500,
  confidence.level = .90
)

pko.wth.3 <- PanelEstimate(
  sets = ipw.wth.3,
  data = merged,
  se.method = "conditional",
  number.iterations = 500,
  confidence.level = .90
)

alt.pko.wth.3 <- PanelEstimate(
  sets = alt.wth.ipw.3,
  data = merged,
  se.method = "conditional",
  number.iterations = 500,
  confidence.level = .90
)

# Visualize Panel Data IPW Models
## PKO Deployments
png("Graphics/pko_dep_res.png", width = 2700, height = 1600, res = 300)

plot.new()
par(oma = c(5, 10, 1.5, 0),
    mar = c(0.8, .9, 1.5, 0.45),
    mfrow = c(2,3),
    pty = "s")

plot(pko.dep.1,
     main = "",
     xlab = "",
     ylab = "")
abline(v = 1, lty = "dotted")

plot(pko.dep.2,
     main = "",
     xlab = "",
     ylab = "")
abline(v = 1, lty = "dotted")

plot(pko.dep.3,
     main = "",
     xlab = "",
     ylab = "")
abline(v = 1, lty = "dotted")

plot(alt.pko.dep.1,
     main = "",
     xlab = "",
     ylab = "")
abline(v = 1, lty = "dotted")

plot(alt.pko.dep.2,
     main = "",
     xlab = "",
     ylab = "")
abline(v = 1, lty = "dotted")

plot(alt.pko.dep.3,
     main = "",
     xlab = "",
     ylab = "")
abline(v = 1, lty = "dotted")

mtext(1,text = "Years After Treatment",
      line = 3.5,
      at = 0.5, outer = TRUE, cex = 1)
mtext(2, text = "Effect of PKO \n Deployment (Restricted)",
      line = 2.5, at = .725, outer = TRUE,
      cex = .8)
mtext(2, text = "Effect of PKO \n Deployment (Unrestricted)",
      line = 2.5, at = .225, outer = TRUE,
      cex = .8)
mtext("1 Year Lag Criterion",
      line = -1, at = 0.17, outer = TRUE, cex = .8)
mtext("2 Year Lag Criterion",
      line = -1, at = 0.5, outer = TRUE, cex = .8)
mtext("3 Year Lag Criterion",
      line = -1, at = 0.83, outer = TRUE, cex = .8)

dev.off()

## PKO Withdrawals
png("Graphics/pko_wth_res.png", width = 2700, height = 1600, res = 300)

plot.new()
par(oma = c(5, 10, 1.5, 0),
    mar = c(0.8, .9, 1.5, 0.45),
    mfrow = c(2,3),
    pty = "s")

plot(pko.wth.1,
     main = "",
     xlab = "",
     ylab = "")
abline(v = 1, lty = "dotted")

plot(pko.wth.2,
     main = "",
     xlab = "",
     ylab = "")
abline(v = 1, lty = "dotted")

plot(pko.wth.3,
     main = "",
     xlab = "",
     ylab = "")
abline(v = 1, lty = "dotted")

plot(alt.pko.wth.1,
     main = "",
     xlab = "",
     ylab = "")
abline(v = 1, lty = "dotted")

plot(alt.pko.wth.2,
     main = "",
     xlab = "",
     ylab = "")
abline(v = 1, lty = "dotted")

plot(alt.pko.wth.3,
     main = "",
     xlab = "",
     ylab = "")
abline(v = 1, lty = "dotted")

mtext(1,text = "Years After Treatment",
      line = 3.5,
      at = 0.5, outer = TRUE, cex = 1)
mtext(2, text = "Effect of PKO \n Withdrawal (Restricted)",
      line = 2.5, at = .725, outer = TRUE,
      cex = .8)
mtext(2, text = "Effect of PKO \n Withdrawal (Unrestricted)",
      line = 2.5, at = .225, outer = TRUE,
      cex = .8)
mtext("1 Year Lag Criterion",
      line = -1, at = 0.17, outer = TRUE, cex = .8)
mtext("2 Year Lag Criterion",
      line = -1, at = 0.5, outer = TRUE, cex = .8)
mtext("3 Year Lag Criterion",
      line = -1, at = 0.83, outer = TRUE, cex = .8)

dev.off()

# Run Non-Panel Data IPW for Sensitivity Analysis
## Run the Logit Model to Generate Propensity Scores
prop_model <- glm(pko ~ lmilper + ldeaths + eth_con + democracy,
                  family = binomial(link = "logit"),
                  data = merged)

## Generate IPWs
merged_ipw <- augment_columns(prop_model, merged,
                              type.predict = "response") %>%
  rename(prop = .fitted) %>%
  mutate(ipw = 
           ((prop * pko) / prop) + 
           ((prop * (1 - pko)) / (1 - prop))
  )

## Label the Gov. Military Capacity Variable for Plotting
var_label(merged_ipw$lmilper) <- "Gov. Military Capacity"

## Run the IPW Model With Covariates
ipw_model_cov <- lm(lgdppc ~ pko + lmilper + ldeaths + eth_con + democracy,
                    data = merged_ipw, weights = ipw)

## Execute the Sensitivity Analysis
sens_results <- sensemakr(ipw_model_cov, treatment = "pko", 
                          benchmark_covariates = "lmilper",
                          kd = c(1, 2, 3))

## Renaming Labels for Better Punctuation
x_lab <- expression(Partial ~ R^2 ~ of ~ Confounder(s) ~ With ~ the ~ Outcome)
y_lab <- expression(Partial ~ R^2 ~ of ~ Confounder(s) ~ With ~ the ~ Treatment)

png("Graphics/sens.png", width = 1800, height = 1600, res = 300)

plot(sens_results, 
     ylab = x_lab, 
     xlab = y_lab)

dev.off()

