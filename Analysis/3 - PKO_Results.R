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
     ylab = "",
     yaxt = "n")
abline(v = 1, lty = "dotted")

plot(ipw_onset_res,
     main = "",
     xlab = "",
     ylab = "",
     yaxt = "n")
abline(v = 1, lty = "dotted")

plot(nn_5_term_res,
     main = "",
     xlab = "",
     ylab = "")
abline(v = 1, lty = "dotted")

plot(nn_10_term_res,
     main = "",
     xlab = "",
     ylab = "",
     yaxt = "n")
abline(v = 1, lty = "dotted")

plot(ipw_term_res,
     main = "",
     xlab = "",
     ylab = "",
     yaxt = "n")
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
