library(cjpowR)
library(dplyr)

# --------------------------------------------------
# Design
# --------------------------------------------------
n_resp <- 433
n_profiles <- 2
n_tasks <- 6
n_eff <- n_resp * n_profiles * n_tasks   # 5196

# --------------------------------------------------
# 1) ALMINDELIG AMCE UDEN INTERAKTION
# Generel/conservativ hovedpower -> levels = 4
# --------------------------------------------------
power_main_amce <- cjpowr_amce(
  amce = 0.05,
  n = n_eff,
  levels = 4,
  alpha = 0.05
)

main_amce_table <- data.frame(
  analyse = "AMCE uden interaktion (generel, konservativ)",
  respondents = n_resp,
  effective_n = n_eff,
  effect = 0.05,
  levels = 4,
  power = power_main_amce$power
)

# --------------------------------------------------
# 2) INTERAKTION: ejer vs ikke-ejer x afstand
# afstand = 3 niveauer
# ejerstatus = 2 niveauer
# => 3 x 2
# --------------------------------------------------
power_owner_distance <- cjpowr_amcie(
  delta0 = 0.5,
  delta3 = 0.05,
  levels = 3,
  levels2 = 2,
  n = n_eff,
  alpha = 0.05
)

owner_distance_table <- data.frame(
  analyse = "Interaktion: ejer vs ikke-ejer x afstand",
  respondents = n_resp,
  effective_n = n_eff,
  delta3 = 0.05,
  levels_attr = 3,
  levels_mod = 2,
  power = power_owner_distance$power
)

# --------------------------------------------------
# 3) INTERAKTION: én af andels-interaktionerne
# Her beholdt som 3 x 4, fordi det er den opsætning,
# du tidligere har kørt og sammenlignet
# --------------------------------------------------
power_share_interaction <- cjpowr_amcie(
  delta0 = 0.5,
  delta3 = 0.05,
  levels = 3,
  levels2 = 4,
  n = n_eff,
  alpha = 0.05
)

share_interaction_table <- data.frame(
  analyse = "Interaktion: andel x omfordeling/indvandring",
  respondents = n_resp,
  effective_n = n_eff,
  delta3 = 0.05,
  levels_attr = 3,
  levels_mod = 4,
  power = power_share_interaction$power
)

# --------------------------------------------------
# 4) REQUIRED N FOR 80% POWER: almindelig AMCE uden interaktion
# --------------------------------------------------
required_main_amce <- cjpowr_amce(
  amce = 0.05,
  power = 0.80,
  levels = 4,
  alpha = 0.05
)

required_main_amce_table <- data.frame(
  analyse = "Required N for AMCE uden interaktion",
  target_power = 0.80,
  effect = 0.05,
  levels = 4,
  required_effective_n = required_main_amce$n,
  required_respondents = ceiling(required_main_amce$n / (n_profiles * n_tasks))
)

# --------------------------------------------------
# 5) Samlet oversigt
# --------------------------------------------------
summary_table <- bind_rows(
  main_amce_table,
  owner_distance_table,
  share_interaction_table
)

summary_table
required_main_amce_table

# Vælg navn til den sidste interaktion
moderator_label <- "Omfordeling"   # eller "Indvandring"

power_table <- data.frame(
  Analyse = c(
    "AMCE",
    "Ejer/ikke-ejer x afstand",
    paste0(moderator_label, " x almennyttige boliger")
  ),
  Effekt = c(
    0.05,
    0.05,
    0.05
  ),
  Attribut_niveauer = c(
    4,
    3,
    3   # skift til 4 hvis du faktisk har kørt almene boliger som 4 niveauer
  ),
  Moderator_niveauer = c(
    NA,
    2,
    4
  ),
  Respondenter = c(
    433,
    433,
    433
  ),
  Effective_N = c(
    5196,
    5196,
    5196
  ),
  Power = c(
    round(power_main_amce$power, 3),
    round(power_owner_distance$power, 3),
    round(power_share_interaction$power, 3)
  )
)

stargazer(
  power_table,
  summary = FALSE,
  rownames = FALSE,
  type = "text",
  out = "Poweranalyse.html"
)