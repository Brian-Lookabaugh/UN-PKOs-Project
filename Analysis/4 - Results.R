############################################################################
###############-----------------PKO Results------------------###############
############################################################################

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
mtext("1 Lag \n Criteria",
      line = -2, at = 0.17, outer = TRUE, cex = .8)
mtext("2 Lags \n Criteria",
      line = -2, at = 0.5, outer = TRUE, cex = .8)
mtext("3 Lags \n Criteria",
      line = -2, at = 0.83, outer = TRUE, cex = .8)

dev.off()

## PKO Withdrawals
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
mtext("1 Lag \n Criteria",
      line = -2, at = 0.17, outer = TRUE, cex = .8)
mtext("2 Lags \n Criteria",
      line = -2, at = 0.5, outer = TRUE, cex = .8)
mtext("3 Lags \n Criteria",
      line = -2, at = 0.83, outer = TRUE, cex = .8)

dev.off()

# Run Non-Panel Data IPW
## Run the Logit Model to Generate Propensity Scores
prop_model <- glm(pko ~ lmilper + ldeaths + eth_con + democracy,
                  family = binomial(link = "logit"),
                  data = merged)

## Generate IPWs
merged_ipw <- augment_columns(prop_model, merged,
                              type.predict = "response") %>%
  rename(propensity = .fitted) %>%
  mutate(ipw = (propensity * pko) / propensity) + 
  ((propensity * (1 - pko)) / (1 - propensity))

## Run the IPW Model
ipw_model <- lm(lgdppc ~ pko, data = merged_ipw, weights = ipw)

