############################################################################
###############-----------PKO IPW/Matching Analysis----------###############
############################################################################

pacman::p_load(
  "tidyverse", # Data Manipulation and Visualization
  "PanelMatch", # Model Estimates From Matching/Weighting With Panel Data
  install = FALSE
)

############################################################################
################################ Run Models ################################
############################################################################
# NN Up to 5 Matches: Onset
nn_5_onset_res <- PanelEstimate(
  sets = nn_match_5_onset,
  data = merged,
  se.method = "conditional",
  number.iterations = 500,
  confidence.level = .95
)

# NN Up to 10 Matches: Onset
nn_10_onset_res <- PanelEstimate(
  sets = nn_match_10_onset,
  data = merged,
  se.method = "conditional",
  number.iterations = 500,
  confidence.level = .95
)

# IPW: Onset
ipw_onset_res <- PanelEstimate(
  sets = ipw_onset,
  data = merged,
  se.method = "conditional",
  number.iterations = 500,
  confidence.level = .95
)

# NN Up to 5 Matches: Withdrawal
nn_5_term_res <- PanelEstimate(
  sets = nn_match_5_term,
  data = merged,
  se.method = "conditional",
  number.iterations = 500,
  confidence.level = .95
)

# NN Up to 10 Matches: Withdrawal
nn_10_term_res <- PanelEstimate(
  sets = nn_match_10_term,
  data = merged,
  se.method = "conditional",
  number.iterations = 500,
  confidence.level = .95
)

# IPW: Withdrawal
ipw_term_res <- PanelEstimate(
  sets = ipw_term,
  data = merged,
  se.method = "conditional",
  number.iterations = 500,
  confidence.level = .95
)

############################################################################
############################# Visualize Models #############################
############################################################################
plot.new()
par(oma = c(5, 10, 1.5, 0),
    mar = c(0.8, .9, 1.5, 0.45),
    mfrow = c(2,3),
    pty = "s")

plot(nn_5_onset_res,
     main = "",
     xlab = "",
     ylab = "")
abline(v = 1, lty = "dotted")

plot(nn_10_onset_res,
     main = "",
     xlab = "",
     ylab = "")
abline(v = 1, lty = "dotted")

plot(ipw_onset_res,
     main = "",
     xlab = "",
     ylab = "")
abline(v = 1, lty = "dotted")

plot(nn_5_term_res,
     main = "",
     xlab = "",
     ylab = "")
abline(v = 1, lty = "dotted")

plot(nn_10_term_res,
     main = "",
     xlab = "",
     ylab = "")
abline(v = 1, lty = "dotted")

plot(ipw_term_res,
     main = "",
     xlab = "",
     ylab = "")
abline(v = 1, lty = "dotted")

mtext(1,text = "Years After Treatment",
      line = 3.5,
      at = 0.5, outer = TRUE, cex = 1)
mtext(2, text = "Estimated Effect of \n PKO Deployment",
      line = 2.5, at = .725, outer = TRUE,
      cex = .8)
mtext(2, text = "Estimated Effect of \n PKO Withdrawal",
      line = 2.5, at = .225, outer = TRUE,
      cex = .8)
mtext("NN Matching - Up to 5 \n",
      line = -2, at = 0.17, outer = TRUE, cex = .8)
mtext("NN Matching - Up to 10 \n",
      line = -2, at = 0.5, outer = TRUE, cex = .8)
mtext("IPW \n ",
      line = -2, at = 0.83, outer = TRUE, cex = .8)

dev.off()

m1 <- lm(lgdppc ~ lmilper, data = merged)
m2 <- lm(lgdppc ~ lmilper + lpop, data = merged)
m3 <- lm(lgdppc ~ lmilper + lpop + ldeaths, data = merged)
m4 <- lm(lgdppc ~ lmilper + lpop + ldeaths + wardur, data = merged)
m5 <- lm(lgdppc ~ lmilper + lpop + ldeaths + wardur + pko, data = merged)

models <- tribble(
  ~model_name, ~model_results,
  "M1", m1,
  "M2", m2,
  "M3", m3,
  "M4", m4,
  "M5", m5
) %>%
  mutate(tidied = map(model_results, ~tidy(., conf.int = TRUE)),
         effect = map(tidied, ~filter(., term == "lmilper"))) %>%
  unnest(effect) %>%
  select(-model_results, -tidied) %>%
  mutate(method = fct_inorder(model_name))

ggplot(models, aes(x = estimate, y = fct_rev(model_name), color = model_name)) +
  geom_vline(xintercept = 0.4, size = 1, linetype = "dashed", color = "black") +
  geom_pointrange(aes(xmin = conf.low, xmax = conf.high), size = 1) +
  scale_color_viridis_d(option = "viridis", end = 0.9, guide = "none") +
  labs(x = "", y = "") +
  theme_light()

