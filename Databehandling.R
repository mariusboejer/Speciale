#### Del 1 - indlæsning og opsætning ####

# Opsætter working directory
setwd("/Users/mariusboejer/Library/CloudStorage/OneDrive-UniversityofCopenhagen/Statskundskab/Speciale/R")

# Indlæser pakker
library(readxl)
library(dplyr)
library(tidyr)
library(janitor)
library(psych)
library(flextable)
library(officer)
library(ggplot2)
library(stargazer)

# Loader data med korrekte headers
respondents <- read_excel("Data clean.xlsx", sheet = "Respondents", skip = 2) %>%
  clean_names()

raw_responses <- read_excel("Data clean.xlsx", sheet = "Raw responses", skip = 2) %>%
  clean_names()

design_matrix <- read_excel("Data clean.xlsx", sheet = "Design matrix", skip = 2) %>%
  clean_names()

# Tjekker antal id per fane
n_distinct(respondents$participant_id)
n_distinct(raw_responses$participant_id)
n_distinct(design_matrix$participant_id)

# Tjekker forskelle mellem faner
setdiff(raw_responses$participant_id, respondents$participant_id)
setdiff(design_matrix$participant_id, respondents$participant_id)
setdiff(respondents$participant_id, raw_responses$participant_id)

# Tilpasser design_matrix og raw_responses efter respondents
raw_clean <- raw_responses %>%
  filter(participant_id %in% respondents$participant_id)

design_clean <- design_matrix %>%
  filter(participant_id %in% respondents$participant_id)

# Tjekker at filtrerede faner matcher respondents
setdiff(respondents$participant_id, design_clean$participant_id)

# Raw responses vendes fra bredt til langt format
raw_long <- raw_clean %>%
  pivot_longer(
    cols = starts_with("q"),
    names_to = "qes",
    names_prefix = "q",
    values_to = "chosen_alt"
  ) %>%
  mutate(
    qes = as.integer(qes),
    chosen_alt = as.integer(chosen_alt)
  )

# Merger data og tilføjer binær valgvariabel
# 1 = valgt, 0 = ikke valgt
conjoint_data <- design_clean %>%
  left_join(raw_long, by = c("participant_id", "qes")) %>%
  mutate(
    chosen = if_else(alt == chosen_alt, 1, 0)
  )

# Tjekker at hver task har to profiler
conjoint_data %>%
  count(participant_id, qes) %>%
  count(n)

# Tjekker at hver task har præcis én valgt profil
conjoint_data %>%
  group_by(participant_id, qes) %>%
  summarise(sum_chosen = sum(chosen), .groups = "drop") %>%
  count(sum_chosen)

# Beholder kun relevante conjoint-variabler
conjoint_data <- conjoint_data %>%
  dplyr::select(
    participant_id, qes, alt, chosen,
    afstand, hojde, andel_almene_boliger, udseende
  )

# Sikrer rigtige antal rækker og deltagere
nrow(conjoint_data)
n_distinct(conjoint_data$participant_id)

# Omdanner conjoint-variabler til faktorer
conjoint_data <- conjoint_data %>%
  mutate(
    chosen = as.numeric(chosen),
    afstand = factor(afstand, levels = c("300 m", "1 kilometer", "2 kilometer")),
    hojde = factor(hojde, levels = c("3 etager", "5 etager", "8 etager")),
    andel_almene_boliger = factor(
      andel_almene_boliger,
      levels = c("0 %", "25 %", "50 %", "100 %")
    ),
    udseende = factor(
      udseende,
      levels = c("Passer godt", "Passer nogenlunde", "Passer dårligt")
    )
  )

# Tjekker at levels er korrekte
levels(conjoint_data$afstand)
levels(conjoint_data$hojde)
levels(conjoint_data$andel_almene_boliger)
levels(conjoint_data$udseende)


#### Del 2 - Rensning og omkodning af respondents/baggrundsvariable ####

df_clean <- respondents %>%
  dplyr::select(participant_id, everything())

# V: Alder
df_clean <- df_clean %>%
  rename(alder = q2_gender_option_1_hvor_gammel_er_du) %>%
  mutate(alder = as.numeric(alder))

table(df_clean$alder)
summary(df_clean$alder)
str(df_clean$alder)

# V: Køn
df_clean <- df_clean %>%
  mutate(
    køn = case_when(
      q2_gender_option_2_mand == "M" ~ "Mand",
      q2_gender_option_3_kvinde == "F" ~ "Kvinde",
      q2_gender_option_4_andet == "X" ~ "Andet",
      TRUE ~ NA_character_
    ),
    køn = factor(køn, levels = c("Kvinde", "Mand", "Andet")),
    køn_binær = case_when(
      køn == "Mand" ~ 1,
      køn == "Kvinde" ~ 0,
      TRUE ~ NA_real_
    )
  )

table(df_clean$køn, useNA = "ifany")
table(df_clean$køn_binær, useNA = "ifany")

# V: Uddannelse
df_clean <- df_clean %>%
  mutate(
    uddannelse = case_when(
      q3_education_level == "Anden uddannelse" ~ NA_character_,
      TRUE ~ q3_education_level
    ),
    uddannelse = factor(
      uddannelse,
      levels = c(
        "Ingen uddannelse",
        "Grundskole (f.eks. folkeskole, privatskole, efterskole)",
        "Gymnasiel uddannelse (f.eks. STX, HF, HHX, HTX)",
        "Erhvervsuddannelse (f.eks. elektriker, tømrer, frisør, sosu-assistent)",
        "Kort videregående uddannelse (under 3 år, f.eks. politibetjent, laborant, installatør, finansøkonom)",
        "Mellemlang videregående uddannelse (professionsbachelor, 3-4 år, f.eks. folkelærer, sygeplejske, pædagog)",
        "Universitetsbachelor (f.eks. første del af en lang videregående uddannelse)",
        "Lang videregående uddannelse (f.eks. kandidatgrad, akademiker, jurist, læge, gymnasielærer)",
        "Forskeruddannelse (Ph.d)"
      ),
      labels = c(
        "Ingen uddannelse",
        "Grundskole",
        "Gymnasial uddannelse",
        "Erhvervsuddannelse",
        "Kort videregående uddannelse",
        "Mellemlang videregående uddannelse",
        "Universitetsbachelor",
        "Lang videregående uddannelse",
        "Forskeruddannelse"
      ),
      ordered = TRUE
    )
  )

table(df_clean$uddannelse, useNA = "ifany")

# V: Indkomst
df_clean <- df_clean %>%
  mutate(
    indkomst = case_when(
      q4_annual_income %in% c("Ved ikke", "Ønsker ikke at besvare") ~ NA_character_,
      TRUE ~ q4_annual_income
    ),
    indkomst = factor(
      indkomst,
      levels = c(
        "Under 100.000 kr",
        "100.000 - 199.999 kr.",
        "200.000 - 299.999 kr.",
        "300.000 - 399.999 kr.",
        "400.000 - 499.999 kr.",
        "500.000 - 599.999 kr.",
        "600.000 - 699.999 kr.",
        "700.000 - 799.999 kr.",
        "800.000 - 899.000 kr.",
        "900.000 - 999.999 kr.",
        "1.000.000 kr eller derover"
      ),
      ordered = TRUE
    )
  )

table(df_clean$indkomst, useNA = "ifany")

# V: Bydel
df_clean <- df_clean %>%
  rename(bydel = q5_district) %>%
  mutate(
    bydel = recode(
      bydel,
      "Kongens Enghave/Sydhavn" = "Kgs. Enghave/Sydhavn"
    ),
    bydel = factor(bydel)
  )

# Vesterbro og Kgs Enghave/Sydhavn lægges sammen jf. København Kommunes bydele

df_clean <- df_clean %>%
  mutate(
    bydel = case_when(
      bydel %in% c("Vesterbro", "Kgs. Enghave/Sydhavn") ~ "Vesterbro/Kgs. Enghave",
      TRUE ~ as.character(bydel)
    ),
    bydel = factor(bydel)
  )

table(df_clean$bydel, useNA = "ifany")

# V: Husstand
df_clean <- df_clean %>%
  rename(husstand = q6_living_situation) %>%
  mutate(husstand = factor(husstand))

table(df_clean$husstand, useNA = "ifany")

# V: Hjemmeboende børn
df_clean <- df_clean %>%
  rename(hjemmeboende_børn = q7_number_of_children_at_home) %>%
  mutate(
    hjemmeboende_børn = as.numeric(hjemmeboende_børn)
  )

summary(df_clean$hjemmeboende_børn)
table(df_clean$hjemmeboende_børn, useNA = "ifany")

# V: Boform
df_clean <- df_clean %>%
  mutate(
    boform = case_when(
      q8_housing_type == "Ejerbolig" ~ "Ejerbolig",
      q8_housing_type == "Andelsbolig" ~ "Andelsbolig",
      q8_housing_type == "Privat lejebolig (leje gennem privatperson eller privatselskab, f.eks. Kererby, Balder)" ~ "Privat lejebolig",
      q8_housing_type == "Almen lejebolig (leje gennem boligorganisation, f.eks. KAB, FSB, AAB)" ~ "Almen lejebolig",
      TRUE ~ NA_character_
    ),
    boform = factor(
      boform,
      levels = c("Ejerbolig", "Andelsbolig", "Privat lejebolig", "Almen lejebolig")
    ),
    ejer_ikke_ejer = case_when(
      boform == "Ejerbolig" ~ 1,
      boform %in% c("Andelsbolig", "Privat lejebolig", "Almen lejebolig") ~ 0,
      TRUE ~ NA_real_
    ),
    ejer_eller_andel = case_when(
      boform %in% c("Ejerbolig", "Andelsbolig") ~ 1,
      boform %in% c("Privat lejebolig", "Almen lejebolig") ~ 0,
      TRUE ~ NA_real_
    )
  )

table(df_clean$boform, useNA = "ifany")
table(df_clean$ejer_ikke_ejer, useNA = "ifany")
table(df_clean$ejer_eller_andel, useNA = "ifany")

# V: Boligtype
df_clean <- df_clean %>%
  rename(boligtype = q9_housing_type) %>%
  mutate(boligtype = factor(boligtype))

table(df_clean$boligtype, useNA = "ifany")

# V: År i lokalområdet
df_clean <- df_clean %>%
  rename(år_i_lokalområdet = q10_years_in_current_area) %>%
  mutate(
    år_i_lokalområdet = factor(
      år_i_lokalområdet,
      levels = c(
        "0-2 år",
        "3-5 år",
        "6-11 år",
        "11-20 år",
        "Mere end 20 år"
      ),
      ordered = TRUE
    )
  )

table(df_clean$år_i_lokalområdet, useNA = "ifany")
str(df_clean$år_i_lokalområdet)

# V: Partivalg

# Omdøber navnene og koder stemmer udenfor partierne til NA

df_clean <- df_clean %>%
  mutate(
    partivalg = case_when(
      q12_parliamentary_election_vote %in% c(
        "Stemte blankt",
        "Stemte ikke",
        "Ved ikke",
        "En kandidat uden for de opstillede partier"
      ) ~ NA_character_,
      
      q12_parliamentary_election_vote == "A: Socialdemokratiet" ~ "Socialdemokratiet",
      q12_parliamentary_election_vote == "B: Radikale Venstre" ~ "Radikale Venstre",
      q12_parliamentary_election_vote == "C: Det Konservative Folkeparti" ~ "Det Konservative Folkeparti",
      q12_parliamentary_election_vote == "F: SF - Socialistisk Folkeparti" ~ "SF - Socialistisk Folkeparti",
      q12_parliamentary_election_vote == "H: Borgernes Parti - Lars Boje Mathiesen" ~ "Borgernes Parti - Lars Boje Mathiesen",
      q12_parliamentary_election_vote == "I: Liberal Alliance" ~ "Liberal Alliance",
      q12_parliamentary_election_vote == "M: Moderaterne" ~ "Moderaterne",
      q12_parliamentary_election_vote == "O: Dansk Folkeparti" ~ "Dansk Folkeparti",
      q12_parliamentary_election_vote == "V: Venstre, Danmarks Liberale Parti" ~ "Venstre, Danmarks Liberale Parti",
      q12_parliamentary_election_vote == "Ø: Enhedslisten - De Rød-Grønne" ~ "Enhedslisten - De Rød-Grønne",
      q12_parliamentary_election_vote == "Å: Alternativet" ~ "Alternativet",
      
      TRUE ~ NA_character_
    ),
    partivalg = factor(partivalg)
  )

table(df_clean$partivalg, useNA = "ifany")

table(df_clean$partivalg, useNA = "ifany")

# V: Venstre-højre
df_clean <- df_clean %>%
  rename(
    venstre_højre = q13_political_left_right_scale_option_1_0_mest_venstreorienteret_10_mest_hojreorienteret
  ) %>%
  mutate(
    venstre_højre = as.numeric(venstre_højre)
  )

summary(df_clean$venstre_højre)
table(df_clean$venstre_højre, useNA = "ifany")
str(df_clean$venstre_højre)


#### Del 3 - Indeks ####

### Stedtilknytning

# Tilfredshed med lokalområdet
df_clean <- df_clean %>%
  mutate(
    tilfreds_med_lokalområdet = case_when(
      q11_agreement_with_statement_row_1_jeg_er_tilfreds_med_hvordan_mit_lokalomrade_ser_ud_column_1_meget_uenig != 0 ~ 1,
      q11_agreement_with_statement_row_1_jeg_er_tilfreds_med_hvordan_mit_lokalomrade_ser_ud_column_2_uenig != 0 ~ 2,
      q11_agreement_with_statement_row_1_jeg_er_tilfreds_med_hvordan_mit_lokalomrade_ser_ud_column_3_hverken_enig_eller_uenig != 0 ~ 3,
      q11_agreement_with_statement_row_1_jeg_er_tilfreds_med_hvordan_mit_lokalomrade_ser_ud_column_4_enig != 0 ~ 4,
      q11_agreement_with_statement_row_1_jeg_er_tilfreds_med_hvordan_mit_lokalomrade_ser_ud_column_5_meget_enig != 0 ~ 5,
      q11_agreement_with_statement_row_1_jeg_er_tilfreds_med_hvordan_mit_lokalomrade_ser_ud_column_6_ved_ikke != 0 ~ NA_real_,
      TRUE ~ NA_real_
    )
  )

table(df_clean$tilfreds_med_lokalområdet, useNA = "ifany")

# Lokalområdet er unikt
df_clean <- df_clean %>%
  mutate(
    lokalområdet_er_unikt = case_when(
      q11_agreement_with_statement_row_2_mit_lokalomrade_er_unikt_column_1_meget_uenig != 0 ~ 1,
      q11_agreement_with_statement_row_2_mit_lokalomrade_er_unikt_column_2_uenig != 0 ~ 2,
      q11_agreement_with_statement_row_2_mit_lokalomrade_er_unikt_column_3_hverken_enig_eller_uenig != 0 ~ 3,
      q11_agreement_with_statement_row_2_mit_lokalomrade_er_unikt_column_4_enig != 0 ~ 4,
      q11_agreement_with_statement_row_2_mit_lokalomrade_er_unikt_column_5_meget_enig != 0 ~ 5,
      q11_agreement_with_statement_row_2_mit_lokalomrade_er_unikt_column_6_ved_ikke != 0 ~ NA_real_,
      TRUE ~ NA_real_
    )
  )

table(df_clean$lokalområdet_er_unikt, useNA = "ifany")

# Bevare lokalområdets karakter
df_clean <- df_clean %>%
  mutate(
    bevare_lokalområdets_karakter = case_when(
      q11_agreement_with_statement_row_3_jeg_vil_gerne_bevare_mit_lokalomrades_karakter_column_1_meget_uenig != 0 ~ 1,
      q11_agreement_with_statement_row_3_jeg_vil_gerne_bevare_mit_lokalomrades_karakter_column_2_uenig != 0 ~ 2,
      q11_agreement_with_statement_row_3_jeg_vil_gerne_bevare_mit_lokalomrades_karakter_column_3_hverken_enig_eller_uenig != 0 ~ 3,
      q11_agreement_with_statement_row_3_jeg_vil_gerne_bevare_mit_lokalomrades_karakter_column_4_enig != 0 ~ 4,
      q11_agreement_with_statement_row_3_jeg_vil_gerne_bevare_mit_lokalomrades_karakter_column_5_meget_enig != 0 ~ 5,
      q11_agreement_with_statement_row_3_jeg_vil_gerne_bevare_mit_lokalomrades_karakter_column_6_ved_ikke != 0 ~ NA_real_,
      TRUE ~ NA_real_
    )
  )

table(df_clean$bevare_lokalområdets_karakter, useNA = "ifany")

# Additivt indeks
df_clean <- df_clean %>%
  mutate(
    stedtilknytning = rowMeans(
      cbind(
        tilfreds_med_lokalområdet,
        lokalområdet_er_unikt,
        bevare_lokalområdets_karakter
      ),
      na.rm = FALSE
    )
  )

summary(df_clean$stedtilknytning)

psych::alpha(df_clean[, c(
  "tilfreds_med_lokalområdet",
  "lokalområdet_er_unikt",
  "bevare_lokalområdets_karakter"
)])

# Cronbach’s alpha = 0.74

# Laver tabel

stedtilknytning_tab <- df_clean %>%
  summarise(
    N = sum(!is.na(stedtilknytning)),
    Gennemsnit = round(mean(stedtilknytning, na.rm = TRUE), 2),
    Median = round(median(stedtilknytning, na.rm = TRUE), 2),
    Standardafvigelse = round(sd(stedtilknytning, na.rm = TRUE), 2),
    Standardfejl = round(
      sd(stedtilknytning, na.rm = TRUE) / sqrt(sum(!is.na(stedtilknytning))),
      2
    ),
    Skævhed = round(
      mean((stedtilknytning - mean(stedtilknytning, na.rm = TRUE))^3, na.rm = TRUE) /
        sd(stedtilknytning, na.rm = TRUE)^3,
      2
    )
  )

stedtilknytning_tab

stargazer(
  stedtilknytning_tab,
  type = "html",
  summary = FALSE,
  rownames = FALSE,
  title = "Stedtilknytning indeks",
  out = "Indeks_stedtilknytning.html"
)

### Omfordeling

# Ulighed gavner samfundet
df_clean <- df_clean %>%
  mutate(
    ulighed_gavner_samfundet = case_when(
      q14_agreement_with_statement_row_1_okonomisk_ulighed_gavner_samfundet_column_1_meget_uenig != 0 ~ 1,
      q14_agreement_with_statement_row_1_okonomisk_ulighed_gavner_samfundet_column_2_uenig != 0 ~ 2,
      q14_agreement_with_statement_row_1_okonomisk_ulighed_gavner_samfundet_column_3_hverken_enig_eller_uenig != 0 ~ 3,
      q14_agreement_with_statement_row_1_okonomisk_ulighed_gavner_samfundet_column_4_enig != 0 ~ 4,
      q14_agreement_with_statement_row_1_okonomisk_ulighed_gavner_samfundet_column_5_helt_enig != 0 ~ 5,
      q14_agreement_with_statement_row_1_okonomisk_ulighed_gavner_samfundet_column_6_ved_ikke != 0 ~ NA_real_,
      TRUE ~ NA_real_
    )
  )

table(df_clean$ulighed_gavner_samfundet, useNA = "ifany")

# Acceptere ulighed
df_clean <- df_clean %>%
  mutate(
    acceptere_ulighed = case_when(
      q14_agreement_with_statement_row_2_for_at_skabe_forandring_og_fremgang_i_samfundet_ma_man_acceptere_en_vis_grad_af_ulighed_column_1_meget_uenig != 0 ~ 1,
      q14_agreement_with_statement_row_2_for_at_skabe_forandring_og_fremgang_i_samfundet_ma_man_acceptere_en_vis_grad_af_ulighed_column_2_uenig != 0 ~ 2,
      q14_agreement_with_statement_row_2_for_at_skabe_forandring_og_fremgang_i_samfundet_ma_man_acceptere_en_vis_grad_af_ulighed_column_3_hverken_enig_eller_uenig != 0 ~ 3,
      q14_agreement_with_statement_row_2_for_at_skabe_forandring_og_fremgang_i_samfundet_ma_man_acceptere_en_vis_grad_af_ulighed_column_4_enig != 0 ~ 4,
      q14_agreement_with_statement_row_2_for_at_skabe_forandring_og_fremgang_i_samfundet_ma_man_acceptere_en_vis_grad_af_ulighed_column_5_helt_enig != 0 ~ 5,
      q14_agreement_with_statement_row_2_for_at_skabe_forandring_og_fremgang_i_samfundet_ma_man_acceptere_en_vis_grad_af_ulighed_column_6_ved_ikke != 0 ~ NA_real_,
      TRUE ~ NA_real_
    )
  )

table(df_clean$acceptere_ulighed, useNA = "ifany")

# Reverse-koder begge variabler
df_clean <- df_clean %>%
  mutate(
    ulighed_gavner_samfundet = 6 - ulighed_gavner_samfundet,
    acceptere_ulighed = 6 - acceptere_ulighed
  )

# Additivt indeks
df_clean <- df_clean %>%
  mutate(
    omfordeling = rowMeans(
      cbind(ulighed_gavner_samfundet, acceptere_ulighed),
      na.rm = FALSE
    )
  )

summary(df_clean$omfordeling)

df_clean %>%
  dplyr::select(
    ulighed_gavner_samfundet,
    acceptere_ulighed,
    omfordeling
  ) %>%
  head(10)

psych::alpha(df_clean[, c(
  "acceptere_ulighed",
  "ulighed_gavner_samfundet"
)])

# Cronbach’s alpha = 0.74

# Samler i tabel

omfordeling_tab <- df_clean %>%
  summarise(
    N = sum(!is.na(omfordeling)),
    Gennemsnit = round(mean(omfordeling, na.rm = TRUE), 2),
    Median = round(median(omfordeling, na.rm = TRUE), 2),
    Standardafvigelse = round(sd(omfordeling, na.rm = TRUE), 2),
    Standardfejl = round(
      sd(omfordeling, na.rm = TRUE) / sqrt(sum(!is.na(omfordeling))),
      2
    ),
    Skævhed = round(
      mean((omfordeling - mean(omfordeling, na.rm = TRUE))^3, na.rm = TRUE) /
        sd(omfordeling, na.rm = TRUE)^3,
      2
    )
  )

omfordeling_tab

stargazer(
  omfordeling_tab,
  type = "html",
  summary = FALSE,
  rownames = FALSE,
  title = "Omfordeling indeks",
  out = "Indeks_omfordeling.html"
)

### Indvandring

# Færre flygtninge og indvandrere
df_clean <- df_clean %>%
  mutate(
    færre_flygtninge_og_indvandrere = case_when(
      q14_agreement_with_statement_row_3_danmark_bor_tage_imod_faerre_flygtninge_og_indvandrere_column_1_meget_uenig != 0 ~ 1,
      q14_agreement_with_statement_row_3_danmark_bor_tage_imod_faerre_flygtninge_og_indvandrere_column_2_uenig != 0 ~ 2,
      q14_agreement_with_statement_row_3_danmark_bor_tage_imod_faerre_flygtninge_og_indvandrere_column_3_hverken_enig_eller_uenig != 0 ~ 3,
      q14_agreement_with_statement_row_3_danmark_bor_tage_imod_faerre_flygtninge_og_indvandrere_column_4_enig != 0 ~ 4,
      q14_agreement_with_statement_row_3_danmark_bor_tage_imod_faerre_flygtninge_og_indvandrere_column_5_helt_enig != 0 ~ 5,
      q14_agreement_with_statement_row_3_danmark_bor_tage_imod_faerre_flygtninge_og_indvandrere_column_6_ved_ikke != 0 ~ NA_real_,
      TRUE ~ NA_real_
    )
  )

table(df_clean$færre_flygtninge_og_indvandrere, useNA = "ifany")

# Indvandring som kulturel trussel
df_clean <- df_clean %>%
  mutate(
    indvandring_trussel_mod_dansk_kultur = case_when(
      q14_agreement_with_statement_row_4_indvandring_udgor_en_alvorlig_trussel_mod_den_danske_kultur_column_1_meget_uenig != 0 ~ 1,
      q14_agreement_with_statement_row_4_indvandring_udgor_en_alvorlig_trussel_mod_den_danske_kultur_column_2_uenig != 0 ~ 2,
      q14_agreement_with_statement_row_4_indvandring_udgor_en_alvorlig_trussel_mod_den_danske_kultur_column_3_hverken_enig_eller_uenig != 0 ~ 3,
      q14_agreement_with_statement_row_4_indvandring_udgor_en_alvorlig_trussel_mod_den_danske_kultur_column_4_enig != 0 ~ 4,
      q14_agreement_with_statement_row_4_indvandring_udgor_en_alvorlig_trussel_mod_den_danske_kultur_column_5_helt_enig != 0 ~ 5,
      q14_agreement_with_statement_row_4_indvandring_udgor_en_alvorlig_trussel_mod_den_danske_kultur_column_6_ved_ikke != 0 ~ NA_real_,
      TRUE ~ NA_real_
    )
  )

table(df_clean$indvandring_trussel_mod_dansk_kultur, useNA = "ifany")

# Additivt indeks
df_clean <- df_clean %>%
  mutate(
    indvandringsskepsis = rowMeans(
      cbind(
        færre_flygtninge_og_indvandrere,
        indvandring_trussel_mod_dansk_kultur
      ),
      na.rm = FALSE
    )
  )

summary(df_clean$indvandringsskepsis)


psych::alpha(df_clean[, c(
  "færre_flygtninge_og_indvandrere",
  "indvandring_trussel_mod_dansk_kultur"
)])

# Cronbach’s alpha = 0.87

# Samler i tabel

indvandringsskepsis_tab <- df_clean %>%
  summarise(
    N = sum(!is.na(indvandringsskepsis)),
    Gennemsnit = round(mean(indvandringsskepsis, na.rm = TRUE), 2),
    Median = round(median(indvandringsskepsis, na.rm = TRUE), 2),
    Standardafvigelse = round(sd(indvandringsskepsis, na.rm = TRUE), 2),
    Standardfejl = round(
      sd(indvandringsskepsis, na.rm = TRUE) / sqrt(sum(!is.na(indvandringsskepsis))),
      2
    ),
    Skævhed = round(
      mean((indvandringsskepsis - mean(indvandringsskepsis, na.rm = TRUE))^3, na.rm = TRUE) /
        sd(indvandringsskepsis, na.rm = TRUE)^3,
      2
    )
  )

indvandringsskepsis_tab


stargazer(
  indvandringsskepsis_tab,
  type = "html",
  summary = FALSE,
  rownames = FALSE,
  title = "Indvandringsskepsis indeks",
  out = "Indeks_indvandringsskepsis.html"
)


#### Del 4 - Oprette nyt datasæt og merge med conjoint-svar ####

df_final <- df_clean %>%
  dplyr::select(
    participant_id,
    alder,
    køn,
    køn_binær,
    uddannelse,
    indkomst,
    bydel,
    husstand,
    hjemmeboende_børn,
    boform,
    ejer_ikke_ejer,
    ejer_eller_andel,
    boligtype,
    år_i_lokalområdet,
    partivalg,
    venstre_højre,
    tilfreds_med_lokalområdet,
    lokalområdet_er_unikt,
    bevare_lokalområdets_karakter,
    stedtilknytning,
    ulighed_gavner_samfundet,
    acceptere_ulighed,
    omfordeling,
    færre_flygtninge_og_indvandrere,
    indvandring_trussel_mod_dansk_kultur,
    indvandringsskepsis
  )

# Merger respondentdata på conjoint_data
conjoint_full <- conjoint_data %>%
  left_join(df_final, by = "participant_id")

# Tjek
nrow(conjoint_full) # 5196 observationer
n_distinct(conjoint_full$participant_id) # 433 respondenter
View(conjoint_full)

# Retter 1 kilometer og 2 kilometer til 1 km og 2 km

conjoint_full <- conjoint_full %>%
  mutate(
    afstand = factor(
      afstand,
      levels = c("300 m", "1 kilometer", "2 kilometer"),
      labels = c("300 m", "1 km", "2 km")
    )
  )

#### Del 5 - repræsentivitet ####

# Repræsentivitetstjek på alder, køn, uddannelse, bydel og boform

# Uploader benchmarkdata

benchmark_alder <- read_excel("Repræsentativitet.xlsx", sheet = "ALDER")
benchmark_køn <- read_excel("Repræsentativitet.xlsx", sheet = "KØN")
benchmark_uddannelse <- read_excel("Repræsentativitet.xlsx", sheet = "UDD")
benchmark_bydel <- read_excel("Repræsentativitet.xlsx", sheet = "BYDEL")
benchmark_boform <- read_excel("Repræsentativitet.xlsx", sheet = "BOFORM")
benchmark_partivalg <- read_excel("Repræsentativitet.xlsx", sheet = "PARTIER")

## Køn

# Populationens fordeling
pop_køn <- tibble::tribble(
  ~køn,       ~andel_befolkning,
  "Kvinde",   0.515,
  "Mand",     0.485
)

# Beregner stikprøvens fordeling
sample_køn <- df_final %>%
  filter(!is.na(køn), køn != "Andet") %>%
  count(køn) %>%
  mutate(andel_sample = n / sum(n))

N_sample <- df_final %>%
  filter(!is.na(køn), køn != "Andet") %>%
  nrow()

# Sammenligning
sammenlign_køn <- pop_køn %>%
  left_join(sample_køn, by = "køn") %>%
  mutate(
    forskel_pctpoint = (andel_sample - andel_befolkning) * 100,
    p_pop  = andel_befolkning,
    p_samp = andel_sample,
    se_diff = sqrt(
      (p_pop * (1 - p_pop)) / 644818 +
        (p_samp * (1 - p_samp)) / N_sample
    ),
    z = (p_samp - p_pop) / se_diff,
    p = 2 * (1 - pnorm(abs(z))),
    sig = case_when(
      p < 0.001 ~ "***",
      p < 0.01  ~ "**",
      p < 0.05  ~ "*",
      TRUE ~ ""
    )
  )

sammenlign_køn # Ikke signifikant afvigende

## Alder

# Opdeler alder i kategorier: 18-29, 30-39 osv

df_final <- df_final %>%
  mutate(
    alder_kat = case_when(
      alder >= 18 & alder <= 29 ~ "18-29",
      alder >= 30 & alder <= 39 ~ "30-39",
      alder >= 40 & alder <= 49 ~ "40-49",
      alder >= 50 & alder <= 59 ~ "50-59",
      alder >= 60 & alder <= 69 ~ "60-69",
      alder >= 70 ~ "70+",
      TRUE ~ NA_character_
    )
  )

table(df_final$alder_kat, useNA = "ifany") # Ser fint ud

# Angiver procentuel fordeling fra benchmark

pop_alder <- tibble::tribble(
  ~alder_kat, ~andel_befolkning,
  "18-29",    0.297,
  "30-39",    0.232,
  "40-49",    0.144,
  "50-59",    0.133,
  "60-69",    0.094,
  "70+",      0.10
)


# Beregner stikprøve fordeling
sample_alder <- df_final %>%
  filter(!is.na(alder_kat)) %>%
  count(alder_kat) %>%
  mutate(andel_sample = n / sum(n))

N_sample <- df_final %>%
  filter(!is.na(alder_kat)) %>%
  nrow()

# Sammenligning
sammenlign_alder <- pop_alder %>%
  left_join(sample_alder, by = "alder_kat") %>%
  mutate(
    forskel_pctpoint = (andel_sample - andel_befolkning) * 100,
    p_pop  = andel_befolkning,
    p_samp = andel_sample,
    se_diff = sqrt(
      (p_pop * (1 - p_pop)) / 644818 +
        (p_samp * (1 - p_samp)) / N_sample
    ),
    z = (p_samp - p_pop) / se_diff,
    p = 2 * (1 - pnorm(abs(z))),
    sig = case_when(
      p < 0.001 ~ "***",
      p < 0.01  ~ "**",
      p < 0.05  ~ "*",
      TRUE ~ ""
    )
  )

# Tjekker resultat
sammenlign_alder


## Uddannelse

# Ingen uddannelse og anden uddannelse fremgår ikke i benchmarkdata, så de tages ud af repræsentivitetstest

df_clean <- df_clean %>%
  mutate(
    uddannelse_repr = case_when(
      q3_education_level == "Ingen uddannelse" ~ NA_character_,
      q3_education_level == "Anden uddannelse" ~ NA_character_,
      q3_education_level == "Grundskole (f.eks. folkeskole, privatskole, efterskole)" ~ "Grundskole",
      q3_education_level == "Gymnasiel uddannelse (f.eks. STX, HF, HHX, HTX)" ~ "Gymnasial uddannelse",
      q3_education_level == "Erhvervsuddannelse (f.eks. elektriker, tømrer, frisør, sosu-assistent)" ~ "Erhvervsfaglig uddannelse",
      q3_education_level == "Kort videregående uddannelse (under 3 år, f.eks. politibetjent, laborant, installatør, finansøkonom)" ~ "Kort videregående uddannelse",
      q3_education_level == "Mellemlang videregående uddannelse (professionsbachelor, 3-4 år, f.eks. folkelærer, sygeplejske, pædagog)" ~ "Mellemlang videregående uddannelse",
      q3_education_level == "Universitetsbachelor (f.eks. første del af en lang videregående uddannelse)" ~ "Bacheloruddannelse",
      q3_education_level == "Lang videregående uddannelse (f.eks. kandidatgrad, akademiker, jurist, læge, gymnasielærer)" ~ "Lang videregående uddannelse",
      q3_education_level == "Forskeruddannelse (Ph.d)" ~ "Ph.d.- eller forskeruddannelse",
      TRUE ~ NA_character_
    )
  )

# Angiver procentuel fordeling fra benchmark
pop_uddannelse <- tibble::tribble(
  ~uddannelse_repr, ~andel_befolkning,
  "Grundskole", 0.140,
  "Gymnasial uddannelse", 0.158,
  "Erhvervsfaglig uddannelse", 0.119,
  "Kort videregående uddannelse", 0.046,
  "Mellemlang videregående uddannelse", 0.148,
  "Bacheloruddannelse", 0.089,
  "Lang videregående uddannelse", 0.261,
  "Ph.d.- eller forskeruddannelse", 0.025
)

# Stikprøve
sample_uddannelse <- df_clean %>%
  filter(!is.na(uddannelse_repr)) %>%
  count(uddannelse_repr) %>%
  mutate(andel_sample = n / sum(n))

# Antal observationer i stikprøven for uddannelse
N_sample <- df_clean %>%
  filter(!is.na(uddannelse_repr)) %>%
  nrow()

# Sammenligning
sammenlign_uddannelse <- pop_uddannelse %>%
  left_join(sample_uddannelse, by = "uddannelse_repr") %>%
  mutate(
    forskel_pctpoint = (andel_sample - andel_befolkning) * 100,
    p_pop  = andel_befolkning,
    p_samp = andel_sample,
    se_diff = sqrt(
      (p_pop * (1 - p_pop)) / 605808 +
        (p_samp * (1 - p_samp)) / N_sample
    ),
    z = (p_samp - p_pop) / se_diff,
    p = 2 * (1 - pnorm(abs(z))),
    sig = case_when(
      p < 0.001 ~ "***",
      p < 0.01  ~ "**",
      p < 0.05  ~ "*",
      TRUE ~ ""
    )
  )

# Tjekker resultat
sammenlign_uddannelse

## Bydel

# Benchmark fordeling  

pop_bydel <- tibble::tribble(
  ~bydel, ~andel_befolkning,
  "Indre by", 0.077,
  "Østerbro", 0.107,
  "Nørrebro", 0.105,
  "Vesterbro/Kgs. Enghave", 0.11,
  "Valby", 0.084,
  "Vanløse", 0.052,
  "Brønshøj-Husum", 0.054,
  "Bispebjerg/Nordvest", 0.073,
  "Amager Øst", 0.083,
  "Amager Vest", 0.116,
  "Frederiksberg", 0.137
)

# Stikprøve fordeling

sample_bydel <- df_clean %>%
  filter(!is.na(bydel)) %>%
  count(bydel) %>%
  mutate(andel_sample = n / sum(n))

N_sample <- df_clean %>%
  filter(!is.na(bydel)) %>%
  nrow()

# Sammenligning
sammenlign_bydel <- pop_bydel %>%
  left_join(sample_bydel, by = "bydel") %>%
  mutate(
    forskel_pctpoint = (andel_sample - andel_befolkning) * 100,
    p_pop  = andel_befolkning,
    p_samp = andel_sample,
    se_diff = sqrt(
      (p_pop * (1 - p_pop)) / 646106 +
        (p_samp * (1 - p_samp)) / N_sample
    ),
    z = (p_samp - p_pop) / se_diff,
    p = 2 * (1 - pnorm(abs(z))),
    sig = case_when(
      p < 0.001 ~ "***",
      p < 0.01  ~ "**",
      p < 0.05  ~ "*",
      TRUE ~ ""
    )
  )

# Tjekker resultat

sammenlign_bydel

# Partier

# Benchmark fordeling

pop_partivalg <- tibble::tribble(
  ~partivalg, ~andel_befolkning,
  "Socialdemokratiet", 0.145,
  "Radikale Venstre", 0.126,
  "Det Konservative Folkeparti", 0.089,
  "SF - Socialistisk Folkeparti", 0.136,
  "Borgernes Parti - Lars Boje Mathiesen", 0.007,
  "Liberal Alliance", 0.081,
  "Moderaterne", 0.081,
  "Dansk Folkeparti", 0.048,
  "Venstre, Danmarks Liberale Parti", 0.036,
  "Danmarksdemokraterne - Inger Støjberg", 0.006,
  "Enhedslisten - De Rød-Grønne", 0.175,
  "Alternativet", 0.069
)

# Stikprøve

sample_partivalg <- df_clean %>%
  filter(!is.na(partivalg)) %>%
  count(partivalg) %>%
  mutate(andel_sample = n / sum(n))

N_sample <- df_clean %>%
  filter(!is.na(partivalg)) %>%
  nrow()

# Sammenligning

sammenlign_partivalg <- pop_partivalg %>%
  left_join(sample_partivalg, by = "partivalg") %>%
  mutate(
    n = dplyr::coalesce(n, 0L),
    andel_sample = dplyr::coalesce(andel_sample, 0), # Sørger for Danmarksdemokraterne (0) fremgår, selvom de ikke er med i stikprøven
    forskel_pctpoint = (andel_sample - andel_befolkning) * 100,
    p_pop  = andel_befolkning,
    p_samp = andel_sample,
    se_diff = sqrt(
      (p_pop * (1 - p_pop)) / 429544 +
        (p_samp * (1 - p_samp)) / N_sample
    ),
    z = (p_samp - p_pop) / se_diff,
    p = 2 * (1 - pnorm(abs(z))),
    sig = case_when(
      p < 0.001 ~ "***",
      p < 0.01  ~ "**",
      p < 0.05  ~ "*",
      TRUE ~ ""
    )
  )

# Tjekker resultat

sammenlign_partivalg

table(df_final$partivalg)

# Blokfordeling til repræsentativitetstabel
# Moderaterne holdes for sig selv

N_sample <- nrow(df_clean)

# Bruger samme populations-N som allerede anvendes til partivalg
N_pop_partivalg <- 429544

blokfordeling <- sammenlign_partivalg %>%
  mutate(
    blok = case_when(
      partivalg %in% c(
        "Socialdemokratiet",
        "Radikale Venstre",
        "SF - Socialistisk Folkeparti",
        "Enhedslisten - De Rød-Grønne",
        "Alternativet"
      ) ~ "Rød blok",
      
      partivalg %in% c(
        "Det Konservative Folkeparti",
        "Borgernes Parti - Lars Boje Mathiesen",
        "Liberal Alliance",
        "Dansk Folkeparti",
        "Venstre, Danmarks Liberale Parti",
        "Danmarksdemokraterne - Inger Støjberg"
      ) ~ "Blå blok",
      
      partivalg == "Moderaterne" ~ "Moderaterne",
      
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(blok)) %>%
  group_by(blok) %>%
  summarise(
    p_pop = sum(andel_befolkning, na.rm = TRUE),
    p_samp = sum(andel_sample, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    Forskel = (p_samp - p_pop) * 100,
    se_diff = sqrt(
      (p_pop * (1 - p_pop)) / N_pop_partivalg +
        (p_samp * (1 - p_samp)) / N_sample
    ),
    z = (p_samp - p_pop) / se_diff,
    p = 2 * (1 - pnorm(abs(z))),
    Signifikans = case_when(
      p < 0.001 ~ "***",
      p < 0.01  ~ "**",
      p < 0.05  ~ "*",
      TRUE ~ ""
    ),
    Kategori = blok,
    Population = p_pop * 100,
    Stikprøve = p_samp * 100,
    Gruppe = "Blokfordeling"
  ) %>%
  select(
    Gruppe,
    Kategori,
    Population,
    Stikprøve,
    Forskel,
    Signifikans
  )


repr %>% filter(Gruppe == "Blokfordeling")

blokfordeling

## Boform

# Har samlet tal fra FRB og KK i Excelfilen

# Benchmark: samlet København + Frederiksberg
pop_boform <- tibble::tribble(
  ~boform,            ~n_befolkning,
  "Ejerbolig",        88958,
  "Andelsbolig",      111128,
  "Privat lejebolig", 125396,
  "Almen lejebolig",  73708
) %>%
  mutate(
    andel_befolkning = n_befolkning / sum(n_befolkning)
  )

# Stikprøve
sample_boform <- df_clean %>%
  filter(!is.na(boform)) %>%
  count(boform) %>%
  mutate(
    andel_sample = n / sum(n)
  )

N_sample <- df_clean %>%
  filter(!is.na(boform)) %>%
  nrow()

N_pop <- sum(pop_boform$n_befolkning)

# Sammenligning
sammenlign_boform <- pop_boform %>%
  left_join(sample_boform, by = "boform") %>%
  mutate(
    n = dplyr::coalesce(n, 0L),
    andel_sample = dplyr::coalesce(andel_sample, 0),
    forskel_pctpoint = (andel_sample - andel_befolkning) * 100,
    p_pop  = andel_befolkning,
    p_samp = andel_sample,
    se_diff = sqrt(
      (p_pop * (1 - p_pop)) / N_pop +
        (p_samp * (1 - p_samp)) / N_sample
    ),
    z = (p_samp - p_pop) / se_diff,
    p = 2 * (1 - pnorm(abs(z))),
    sig = case_when(
      p < 0.001 ~ "***",
      p < 0.01  ~ "**",
      p < 0.05  ~ "*",
      TRUE ~ ""
    )
  ) %>%
  mutate(
    andel_befolkning = round(andel_befolkning * 100, 1),
    andel_sample = round(andel_sample * 100, 1),
    forskel_pctpoint = round(forskel_pctpoint, 1),
    p = round(p, 3)
  )

# Tjekker resultat

sammenlign_alder
sammenlign_køn
sammenlign_uddannelse
sammenlign_partivalg
sammenlign_bydel
sammenlign_boform


# Alle repræsentivitetstabeller samles nu til en

# Samler først vigtigtste kolonner i hver tabel

køn <- sammenlign_køn %>%
  transmute(
    Kategori = køn,
    Population = andel_befolkning * 100,
    Stikprøve = andel_sample * 100,
    Forskel = (andel_sample - andel_befolkning) * 100,
    Signifikans = sig,
    Gruppe = "Køn"
  )

alder <- sammenlign_alder %>%
  transmute(
    Kategori = alder_kat,
    Population = andel_befolkning * 100,
    Stikprøve = andel_sample * 100,
    Forskel = (andel_sample - andel_befolkning) * 100,
    Signifikans = sig,
    Gruppe = "Alder"
  )

udd <- sammenlign_uddannelse %>%
  transmute(
    Kategori = uddannelse_repr,
    Population = andel_befolkning * 100,
    Stikprøve = andel_sample * 100,
    Forskel = (andel_sample - andel_befolkning) * 100,
    Signifikans = sig,
    Gruppe = "Uddannelse"
  )

bydel <- sammenlign_bydel %>%
  transmute(
    Kategori = bydel,
    Population = andel_befolkning * 100,
    Stikprøve = andel_sample * 100,
    Forskel = (andel_sample - andel_befolkning) * 100,
    Signifikans = sig,
    Gruppe = "Bydel"
  )

boform <- sammenlign_boform %>%
  transmute(
    Kategori = boform,
    Population = andel_befolkning,
    Stikprøve = andel_sample,
    Forskel = (andel_sample - andel_befolkning),
    Signifikans = sig,
    Gruppe = "Boform"
  )

parti <- sammenlign_partivalg %>%
  transmute(
    Kategori = partivalg,
    Population = andel_befolkning * 100,
    Stikprøve = andel_sample * 100,
    Forskel = (andel_sample - andel_befolkning) * 100,
    Signifikans = sig,
    Gruppe = "Partivalg"
  )


# Samler i en tabel

repr <- bind_rows(køn, alder, udd, bydel, boform, parti, blokfordeling)

View(repr)

# Runder tal ned og flytter signifikansniveau ind til forskelle

repr <- repr %>%
  mutate(
    Population = format(round(Population, 1), decimal.mark = ",", nsmall = 1),
    Stikprøve = format(round(Stikprøve, 1), decimal.mark = ",", nsmall = 1),
    Forskel = paste0(
      format(round(Forskel, 1), decimal.mark = ",", nsmall = 1),
      Signifikans
    )
  )

# Fjerner gruppenavne som kolonne og indsætter som egne rækker

repr_final <- bind_rows(
  tibble(Kategori = "Køn", Population = "", Stikprøve = "", Forskel = ""),
  repr %>% filter(Gruppe == "Køn") %>% dplyr::select(Kategori, Population, Stikprøve, Forskel),
  
  tibble(Kategori = "Alder", Population = "", Stikprøve = "", Forskel = ""),
  repr %>% filter(Gruppe == "Alder") %>% dplyr::select(Kategori, Population, Stikprøve, Forskel),
  
  tibble(Kategori = "Uddannelse", Population = "", Stikprøve = "", Forskel = ""),
  repr %>% filter(Gruppe == "Uddannelse") %>% dplyr::select(Kategori, Population, Stikprøve, Forskel),
  
  tibble(Kategori = "Bydel", Population = "", Stikprøve = "", Forskel = ""),
  repr %>% filter(Gruppe == "Bydel") %>% dplyr::select(Kategori, Population, Stikprøve, Forskel),
  
  tibble(Kategori = "Boform", Population = "", Stikprøve = "", Forskel = ""),
  repr %>% filter(Gruppe == "Boform") %>% dplyr::select(Kategori, Population, Stikprøve, Forskel),
  
  tibble(Kategori = "Partivalg", Population = "", Stikprøve = "", Forskel = ""),
  repr %>% filter(Gruppe == "Partivalg") %>% dplyr::select(Kategori, Population, Stikprøve, Forskel),
  
  tibble(Kategori = "Blokfordeling", Population = "", Stikprøve = "", Forskel = ""),
  repr %>% filter(Gruppe == "Blokfordeling") %>% dplyr::select(Kategori, Population, Stikprøve, Forskel)
)

tail(repr_final, 20)

View(repr_final)



# Gør klar til at udskrive til word

ft <- flextable(repr_final)
ft <- autofit(ft)

# Gør grupperækker fede
ft <- bold(ft, i = which(repr_final$Population == ""), bold = TRUE)

doc <- read_docx()
doc <- body_add_par(doc, "Tabel X. Population sammenlignet med stikprøve", style = "Normal")
doc <- body_add_flextable(doc, ft)
doc <- body_add_par(
  doc,
  "Note: Forskelle er angivet i procentpoint. Stjerner angiver statistisk signifikans (* p<0,05, ** p<0,01, *** p<0,001).",
  style = "Normal"
)

print(doc, target = "repræsentivitet_tabel.docx") # Udskriver som wordfil


nrow(blokfordeling)
blokfordeling

repr %>% filter(Gruppe == "Blokfordeling")

tail(repr_final, 10)

# Opretter en graf, der viser fordeling af selvrapporteret politisk placering (0 = venstre, 10 = højre)

plot_ideologi <- df_final %>%
  filter(!is.na(venstre_højre)) %>%
  count(venstre_højre) %>%
  mutate(procent = n / sum(n) * 100) %>%
  ggplot(aes(x = venstre_højre, y = procent)) +
  geom_col(width = 0.9, fill = "grey30") +
  geom_smooth(method = "loess", se = FALSE, colour = "red", linewidth = 1) +
  scale_x_continuous(breaks = 0:10) +
  labs(
    x = "Selvrapporteret politisk placering (0 = venstre, 10 = højre)",
    y = "Andel af respondenter (pct.)",
    title = "Selvrapporteret politisk placering"
  ) +
  theme_minimal()

ggsave("ideologi.png", plot = plot_ideologi, width = 8, height = 6)

# Opretter tabel med data på selvrapporteret politisk placering



ideologi_tab <- df_clean %>%
  summarise(
    N = sum(!is.na(venstre_højre)),
    Gennemsnit = round(mean(venstre_højre, na.rm = TRUE), 2),
    Median = round(median(venstre_højre, na.rm = TRUE), 2),
    Standardafvigelse = round(sd(venstre_højre, na.rm = TRUE), 2),
    Standardfejl = round(
      sd(venstre_højre, na.rm = TRUE) / sqrt(sum(!is.na(venstre_højre))),
      2
    ),
    Skævhed = round(
      mean((venstre_højre - mean(venstre_højre, na.rm = TRUE))^3, na.rm = TRUE) /
        sd(venstre_højre, na.rm = TRUE)^3,
      2
    )
  )




# Gemmer tabel

stargazer(
  ideologi_tab,
  type = "html",
  summary = FALSE,
  rownames = FALSE,
  title = "Ideologisk fordeling",
  out = "ideologi_tab.html"
)

summary(df_clean$venstre_højre)

##### Del 6 - datatjek og delvist robusthed #####


# Tester datastruktur
n_distinct(conjoint_data$participant_id)
nrow(conjoint_data)

conjoint_data %>%
  count(participant_id, qes) %>%
  count(n)

conjoint_data %>%
  group_by(participant_id, qes) %>%
  summarise(sum_chosen = sum(chosen), .groups = "drop") %>%
  count(sum_chosen)

# Ser fint ud

# Tjekker at alle respondenter har svaret på lige mange tasks

conjoint_data %>%
  distinct(participant_id, qes) %>%
  count(participant_id) %>%
  count(n)

# Ser rigtigt ud

# Balancetest - tester at alle attributer er nogenlunde lige balanceret - at de fremgår lige mange gange
# Skal laves bedre senerehen

conjoint_data %>% count(afstand) %>% mutate(pct = n / sum(n) * 100)
conjoint_data %>% count(hojde) %>% mutate(pct = n / sum(n) * 100)
conjoint_data %>% count(andel_almene_boliger) %>% mutate(pct = n / sum(n) * 100)
conjoint_data %>% count(udseende) %>% mutate(pct = n / sum(n) * 100)


# Ser fint ud

# Test af attribut uafhængighed

table(conjoint_data$afstand, conjoint_data$hojde)
table(conjoint_data$afstand, conjoint_data$andel_almene_boliger)
table(conjoint_data$afstand, conjoint_data$udseende)

table(conjoint_data$hojde, conjoint_data$andel_almene_boliger)
table(conjoint_data$hojde, conjoint_data$udseende)

table(conjoint_data$andel_almene_boliger, conjoint_data$udseende)

# Chi i anden test af uafhængighed
chisq.test(table(conjoint_data$afstand, conjoint_data$hojde))
chisq.test(table(conjoint_data$afstand, conjoint_data$andel_almene_boliger))
chisq.test(table(conjoint_data$afstand, conjoint_data$udseende))

chisq.test(table(conjoint_data$hojde, conjoint_data$andel_almene_boliger))
chisq.test(table(conjoint_data$hojde, conjoint_data$udseende))

chisq.test(table(conjoint_data$andel_almene_boliger, conjoint_data$udseende))

# Venstre højre test - bias mod side, rækkefølgeeffekter
# Laves bedre senere!!!

conjoint_data %>%
  filter(alt == 1) %>%
  summarise(andelen_valgt_alt1 = mean(chosen))

conjoint_data %>%
  filter(alt == 2) %>%
  summarise(andelen_valgt_alt2 = mean(chosen))

conjoint_data %>%
  filter(alt == 1) %>%
  group_by(qes) %>%
  summarise(andelen_valgt_alt1 = mean(chosen), .groups = "drop")


conjoint_data %>%
  filter(alt == 1) %>%
  group_by(qes) %>%
  summarise(andelen_valgt_alt1 = mean(chosen), .groups = "drop") %>%
  ggplot(aes(x = qes, y = andelen_valgt_alt1)) +
  geom_point() +
  geom_line() +
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    x = "Tasknummer",
    y = "Andel hvor profil 1 foretrækkes",
    title = "Valg af profil 1 på tværs af tasks"
  ) +
  theme_minimal()

# Indikerer ikke venstre højre bias

conjoint_data %>%
  filter(alt == 1) %>%
  group_by(participant_id) %>%
  summarise(
    andel_alt1_valgt = mean(chosen),
    .groups = "drop"
  ) %>%
  count(andel_alt1_valgt)

# Missingness

df_clean %>%
  summarise(
    missing_køn = sum(is.na(køn)),
    missing_uddannelse = sum(is.na(uddannelse)),
    missing_indkomst = sum(is.na(indkomst)),
    missing_boform = sum(is.na(boform)),
    missing_venstre_højre = sum(is.na(venstre_højre)),
    missing_stedtilknytning = sum(is.na(stedtilknytning)),
    missing_omfordeling = sum(is.na(omfordeling)),
    missing_indvandringsskepsis = sum(is.na(indvandringsskepsis))
  )


# I procent

df_clean %>%
  summarise(
    across(
      c(køn, uddannelse, indkomst, boform, venstre_højre,
        stedtilknytning, omfordeling, indvandringsskepsis),
      ~ mean(is.na(.)) * 100
    )
  )

# Ser fint ud

# Tjekker at referencekategorier/baseline er sat

conjoint_data <- conjoint_data %>%
  mutate(
    afstand = relevel(afstand, ref = "300 m"),
    hojde = relevel(hojde, ref = "3 etager"),
    andel_almene_boliger = relevel(andel_almene_boliger, ref = "0 %"),
    udseende = relevel(udseende, ref = "Passer godt")
  )

# Ser rigtigt ud

# Tjekker at conjoint full er rigtig merged

nrow(conjoint_full)
n_distinct(conjoint_full$participant_id)

# Ser fint ud

# Missingness efter merge

conjoint_full %>%
  summarise(
    missing_ejer_ikke_ejer = sum(is.na(ejer_ikke_ejer)),
    missing_stedtilknytning = sum(is.na(stedtilknytning)),
    missing_omfordeling = sum(is.na(omfordeling)),
    missing_indvandringsskepsis = sum(is.na(indvandringsskepsis))
  )



## LOADER cregg pakken, som er udgået fra cran - derfor kompleks at loade

install.packages("RcppArmadillo", type = "binary")
install.packages("minqa", type = "binary")
install.packages("survey", type = "binary")
install.packages("remotes")
remotes::install_github("leeper/cregg", dependencies = FALSE, upgrade = "never")
library(survey)
library(cregg)

# Tester AMCE

amce_test <- cj(
  data = conjoint_full,
  formula = chosen ~ afstand + hojde + andel_almene_boliger + udseende,
  id = ~participant_id
)

amce_test

plot(amce_test)

# Ser fint ud - pakken virker, selvom den er taget ned fra CRAN!

# # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # #
##### Del 7 - selve analysen ####
# # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # #

# Laver marginal means på hele undersøgelsen for at finde ud af, hvad hvert enkelt udfald gør ved sandsynligheden for et projekt foretrækkes

mm_generel <- cj(
  data = conjoint_full,
  formula = chosen ~ afstand + hojde + andel_almene_boliger + udseende,
  id = ~participant_id,
  estimate = "mm",
  h0 = 0.5
)

mm_generel # hurtig tabel, ser fin ud

plot(mm_generel) # hurtigt plot, ser fint ud

# Laver pæn tabel med html output

tab_mm <- mm_generel %>%
  transmute(
    Kategori = recode(
      feature,
      "afstand" = "Afstand",
      "hojde" = "Højde",
      "andel_almene_boliger" = "Andel almennyttige boliger",
      "udseende" = "Udseende"
    ),
    Niveau = level,
    estimate = round(estimate, 3),
    `95% KI` = sprintf("[%.3f, %.3f]", lower, upper),
    `p-værdi` = ifelse(
      is.na(p), "",
      ifelse(p < 0.001, "<0.001", sprintf("%.3f", p))
    )
  )

tab_mm

stargazer(
  tab_mm,
  type = "html",
  summary = FALSE,
  rownames = FALSE,
  title = "Conjointanalyse - Marginal Means",
  out = "mm_generel.html"
)



# plotter pænt med ggplot2

plot_mm_generel <- mm_generel %>%
  mutate(
    feature = recode( # Sikrer at navnene er korrekte
      feature,
      "afstand" = "Afstand",
      "hojde" = "Højde",
      "andel_almene_boliger" = "Andel almennyttig boliger",
      "udseende" = "Udseende"
    ),
    level,
  ) %>%
  ggplot(aes(x = estimate, y = reorder(level, desc(level)))) +
  geom_vline(xintercept = 0.5, linetype = "longdash") +
  geom_errorbarh(aes(xmin = lower, xmax = upper), height = 0.5) +
  geom_point() +
  xlab("Sandsynlighed for at boligprojekt foretrækkes (marginal means)") +
  ylab("") +
  facet_grid(feature ~ ., scales = "free_y", space = "free_y") +
  theme(
    text = element_text(family = "Times New Roman"),
    axis.text = element_text(size = 11),
    axis.title = element_text(size = 12),
    strip.text = element_text(size = 12)
  )

plot_mm_generel


# Gemmer som billede
ggsave("mm_general.png", plot = plot_mm_generel, width = 8, height = 9)


### Hypotese 1 ###

# Starter med MM

mm_h1 <- mm_generel %>%
  filter(feature == "afstand")

# Sætter i tabel og udskriver
tab_h1_mm <- mm_h1 %>%
  transmute(
    Niveau = level,
    estimate = round(estimate, 3),
    `95% KI` = sprintf("[%.3f, %.3f]", lower, upper),
    `p-værdi` = ifelse(
      is.na(p), "",
      ifelse(p < 0.001, "<0.001", sprintf("%.3f", p))
    )
  )

tab_h1_mm

stargazer(
  tab_h1_mm,
  type = "html",
  summary = FALSE,
  rownames = FALSE,
  title = "Afstand - Marginal Means",
  out = "h1_mm.html"
)

plot_h1_mm <- mm_h1 %>%
  mutate(
    level = factor(level, levels = c("2 km", "1 km", "300 m"))
  ) %>%
  ggplot(aes(x = estimate, y = level)) +
  geom_vline(xintercept = 0.5, linetype = "longdash", linewidth = 0.6) +
  geom_errorbarh(aes(xmin = lower, xmax = upper), height = 0.18, linewidth = 0.7) +
  geom_point(size = 2.5) +
  scale_x_continuous(
    limits = c(0.40, 0.60),
    breaks = seq(0.40, 0.60, by = 0.05),
    
  ) +
  labs(
    x = "Sandsynlighed for at boligprojekt foretrækkes (marginal means)",
    y = NULL,
    title = "Afstand til respondentens bolig"
  ) +
  theme(
    text = element_text(family = "Times New Roman"),
    axis.text = element_text(size = 11),
    axis.title = element_text(size = 12),
    plot.title = element_text(size = 13, face = "bold"),
    panel.background = element_rect(fill = "grey92", colour = NA),
    plot.background = element_rect(fill = "white", colour = NA),
    panel.grid.major = element_line(colour = "white", linewidth = 0.4),
    panel.grid.minor = element_blank()
  )

plot_h1_mm

# Gemmer
ggsave("plot_h1_mm.png", plot = plot_h1_mm, width = 8, height = 4)


# AMCE - H1

amce_h1 <- cj(
  data = conjoint_full,
  formula = chosen ~ afstand + hojde + andel_almene_boliger + udseende,
  id = ~participant_id,
  estimate = "amce"
) %>%
  dplyr::filter(feature == "afstand")

# Tabel

tab_h1_amce <- amce_h1 %>%
  transmute(
    Niveau = level,
    AMCE = round(estimate, 3),
    `95% KI` = sprintf("[%.3f, %.3f]", lower, upper),
    `p-værdi` = ifelse(is.na(p), "", ifelse(p < 0.001, "<0.001", sprintf("%.3f", p)))
  )

tab_h1_amce

# Udskriver tabel

stargazer(
  tab_h1_amce,
  type = "html",
  summary = FALSE,
  rownames = FALSE,
  title = "Afstand - AMCE",
  out = "h1_amce.html"
)

plot_h1_amce <- amce_h1 %>%
  mutate(
    level,
    # Reverseres fordi ggplot læser faktor-niveauer nedefra og op
    level = factor(level, levels = c("2 km", "1 km", "300 m"))
  ) %>%
  ggplot(aes(x = estimate, y = level)) +
  geom_vline(xintercept = 0, linetype = "longdash", linewidth = 0.8) +
  geom_errorbarh(
    data = \(x) dplyr::filter(x, !is.na(lower) & !is.na(upper)),
    aes(xmin = lower, xmax = upper),
    height = 0.15,
    linewidth = 0.9
  ) +
  geom_point(size = 3, shape = 16) +
  labs(
    title = "Afstand til respondentens bolig",
    x = "Ændring i sandsynlighed for at boligprojektet foretrækkes (AMCE)",
    y = NULL
  ) +
  coord_cartesian(xlim = c(-0.04, 0.08)) +
  theme_grey() +
  theme(
    text = element_text(family = "Times New Roman"),
    plot.title = element_text(size = 16, face = "bold", hjust = 0),
    axis.text = element_text(size = 12, colour = "black"),
    axis.title = element_text(size = 13),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(linewidth = 0.3),
    panel.grid.major.x = element_line(linewidth = 0.3)
  )

plot_h1_amce

ggsave("plot_h1_amce.png", plot = plot_h1_amce, width = 8, height = 4)

# H1 må afkræftes

### Hypotese 2: 

## Marginal means

# Ejer vs ikke ejer

conjoint_full <- conjoint_full %>%
  mutate(
    ejergruppe_stram = factor(
      ejer_ikke_ejer,
      levels = c(0, 1),
      labels = c("Ikke ejer", "Ejer")
    )
  )

mm_h2_ikke_ejer <- cj(
  data = conjoint_full %>% filter(ejer_ikke_ejer == 0),
  formula = chosen ~ afstand + hojde + andel_almene_boliger + udseende,
  id = ~participant_id,
  estimate = "mm",
  h0 = 0.5
) %>%
  dplyr::filter(feature == "afstand") %>%
  mutate(gruppe = "Ikke ejer")

mm_h2_ejer <- cj(
  data = conjoint_full %>% filter(ejer_ikke_ejer == 1),
  formula = chosen ~ afstand + hojde + andel_almene_boliger + udseende,
  id = ~participant_id,
  estimate = "mm",
  h0 = 0.5
) %>%
  dplyr::filter(feature == "afstand") %>%
  mutate(gruppe = "Ejer")

mm_h2_stram_tab <- bind_rows(mm_h2_ikke_ejer, mm_h2_ejer)

mm_h2_stram_tab

stargazer(
  mm_h2_stram_tab,
  type = "html",
  summary = FALSE,
  rownames = FALSE,
  title = "Afstand - Ejer vs ikke ejer - MM",
  out = "h2_mm_stram.html"
)

# Visualisere

plot_h2_mm_stram <- mm_h2_stram_tab %>%
  mutate(
    level = factor(level, levels = c("2 km", "1 km", "300 m")),
    gruppe = factor(gruppe, levels = c("Ikke ejer", "Ejer"))
  ) %>%
  ggplot(aes(x = estimate, y = level, color = gruppe, shape = gruppe)) +
  geom_vline(xintercept = 0.5, linetype = "longdash", linewidth = 0.7, color = "black") +
  geom_errorbarh(
    aes(xmin = lower, xmax = upper),
    height = 0.15,
    linewidth = 0.8,
    position = position_dodge(width = 0.5)
  ) +
  geom_point(
    size = 2.8,
    position = position_dodge(width = 0.5)
  ) +
  scale_color_manual(values = c("Ikke ejer" = "black", "Ejer" = "grey40")) +
  scale_shape_manual(values = c("Ikke ejer" = 16, "Ejer" = 17)) +
  labs(
    title = "Afstand til respondentens bolig efter ejerstatus",
    x = "Sandsynlighed for at boligprojektet foretrækkes (marginal means)",
    y = NULL,
    color = NULL,
    shape = NULL
  ) +
  theme(
    text = element_text(family = "Times New Roman"),
    axis.text = element_text(size = 11),
    axis.title = element_text(size = 12),
    plot.title = element_text(size = 16, face = "bold"),
    legend.text = element_text(size = 11),
    legend.position = "bottom",
    panel.background = element_rect(fill = "grey92", colour = NA),
    plot.background = element_rect(fill = "white", colour = NA),
    panel.grid.major = element_line(colour = "white", linewidth = 0.4),
    panel.grid.minor = element_blank()
  )

plot_h2_mm_stram

# Gemmer
ggsave("plot_h2_mm_stram.png", plot = plot_h2_mm_stram, width = 8, height = 4)

# Ingen signifikante forskelle


# Bred version: Ejer/andelshaver vs lejer
conjoint_full <- conjoint_full %>%
  mutate(
    ejergruppe = factor(
      ejer_eller_andel,
      levels = c(0, 1),
      labels = c("Lejer", "Ejer/andel")
    )
  )


mm_h2_lejer <- cj(
  data = conjoint_full %>% filter(ejer_eller_andel == 0),
  formula = chosen ~ afstand + hojde + andel_almene_boliger + udseende,
  id = ~participant_id,
  estimate = "mm",
  h0 = 0.5
) %>%
  dplyr::filter(feature == "afstand") %>%
  mutate(gruppe = "Lejer")

mm_h2_ejer_andel <- cj(
  data = conjoint_full %>% filter(ejer_eller_andel == 1),
  formula = chosen ~ afstand + hojde + andel_almene_boliger + udseende,
  id = ~participant_id,
  estimate = "mm",
  h0 = 0.5
) %>%
  dplyr::filter(feature == "afstand") %>%
  mutate(gruppe = "Ejer/andel")

mm_h2_bred_tab <- bind_rows(mm_h2_lejer, mm_h2_ejer_andel)

mm_h2_bred_tab

# Udskriver

stargazer(
  mm_h2_bred_tab,
  type = "html",
  summary = FALSE,
  rownames = FALSE,
  title = "Afstand - Ejer/andel vs lejer - MM",
  out = "h2_mm_bred.html"
)

# Visualisering

plot_h2_mm_bred <- mm_h2_bred_tab %>%
  mutate(
    level = recode(
      level,
      "1 kilometer" = "1 km",
      "2 kilometer" = "2 km"
    ),
    level = factor(level, levels = c("2 km", "1 km", "300 m")),
    gruppe = factor(gruppe, levels = c("Lejer", "Ejer/andel"))
  ) %>%
  ggplot(aes(x = estimate, y = level, color = gruppe, shape = gruppe)) +
  geom_vline(
    xintercept = 0.5,
    linetype = "longdash",
    linewidth = 0.7,
    color = "black"
  ) +
  geom_errorbarh(
    aes(xmin = lower, xmax = upper),
    height = 0.15,
    linewidth = 0.8,
    position = position_dodge(width = 0.5)
  ) +
  geom_point(
    size = 2.8,
    position = position_dodge(width = 0.5)
  ) +
  scale_color_manual(
    values = c("Lejer" = "black", "Ejer/andel" = "grey40")
  ) +
  scale_shape_manual(
    values = c("Lejer" = 16, "Ejer/andel" = 17)
  ) +
  scale_x_continuous(
    breaks = c(0.45, 0.50, 0.55)
  ) +
  labs(
    title = "Afstand til respondentens bolig efter ejerstatus",
    x = "Sandsynlighed for at boligprojektet foretrækkes (marginal means)",
    y = NULL,
    color = NULL,
    shape = NULL
  ) +
  theme(
    text = element_text(family = "Times New Roman"),
    axis.text = element_text(size = 11),
    axis.title = element_text(size = 12),
    plot.title = element_text(size = 16, face = "bold"),
    legend.text = element_text(size = 11),
    legend.position = "bottom",
    panel.background = element_rect(fill = "grey92", colour = NA),
    plot.background = element_rect(fill = "white", colour = NA),
    panel.grid.major = element_line(colour = "white", linewidth = 0.4),
    panel.grid.minor = element_blank()
  )

plot_h2_mm_bred

ggsave("plot_h2_mm_bred.png", plot = plot_h2_mm_bred, width = 8, height = 4)


# Ingen signifikant forskel.

# MM med alle fire grupper


# Ejerbolig
mm_h2_ejerbolig <- cj(
  data = conjoint_full %>% filter(boform == "Ejerbolig"),
  formula = chosen ~ afstand + hojde + andel_almene_boliger + udseende,
  id = ~participant_id,
  estimate = "mm",
  h0 = 0.5
) %>%
  dplyr::filter(feature == "afstand") %>%
  mutate(gruppe = "Ejerbolig")

# Andelsbolig
mm_h2_andelsbolig <- cj(
  data = conjoint_full %>% filter(boform == "Andelsbolig"),
  formula = chosen ~ afstand + hojde + andel_almene_boliger + udseende,
  id = ~participant_id,
  estimate = "mm",
  h0 = 0.5
) %>%
  dplyr::filter(feature == "afstand") %>%
  mutate(gruppe = "Andelsbolig")

# Privat lejebolig
mm_h2_privatleje <- cj(
  data = conjoint_full %>% filter(boform == "Privat lejebolig"),
  formula = chosen ~ afstand + hojde + andel_almene_boliger + udseende,
  id = ~participant_id,
  estimate = "mm",
  h0 = 0.5
) %>%
  dplyr::filter(feature == "afstand") %>%
  mutate(gruppe = "Privat lejebolig")

# Almen lejebolig
mm_h2_almenleje <- cj(
  data = conjoint_full %>% filter(boform == "Almen lejebolig"),
  formula = chosen ~ afstand + hojde + andel_almene_boliger + udseende,
  id = ~participant_id,
  estimate = "mm",
  h0 = 0.5
) %>%
  dplyr::filter(feature == "afstand") %>%
  mutate(gruppe = "Almen lejebolig")

# Samler grupper i tabel
mm_h2_alle <- bind_rows(
  mm_h2_ejerbolig,
  mm_h2_andelsbolig,
  mm_h2_privatleje,
  mm_h2_almenleje
)

mm_h2_alle


# Udskriver tabel

stargazer(
  mm_h2_alle,
  type = "html",
  summary = FALSE,
  rownames = FALSE,
  title = "Afstand - boform - MM",
  out = "h2_mm_alle.html"
)

# Visualisering

plot_h2_mm_alle <- mm_h2_alle %>%
  mutate(
    level = recode(
      level,
      "1 kilometer" = "1 km",
      "2 kilometer" = "2 km"
    ),
    level = factor(level, levels = c("300 m", "1 km", "2 km")),
    gruppe = factor(
      gruppe,
      levels = c(
        "Ejerbolig",
        "Andelsbolig",
        "Privat lejebolig",
        "Almen lejebolig"
      )
    )
  ) %>%
  ggplot(aes(x = estimate, y = level, color = gruppe, shape = gruppe)) +
  geom_vline(
    xintercept = 0.5,
    linetype = "longdash",
    linewidth = 0.7,
    color = "black"
  ) +
  geom_errorbarh(
    aes(xmin = lower, xmax = upper),
    height = 0.15,
    linewidth = 0.8,
    position = position_dodge(width = 0.5)
  ) +
  geom_point(
    size = 2.8,
    position = position_dodge(width = 0.5)
  ) +
  scale_shape_manual(
    values = c(
      "Ejerbolig" = 16,
      "Andelsbolig" = 17,
      "Privat lejebolig" = 15,
      "Almen lejebolig" = 18
    )
  ) +
  scale_color_manual(
    values = c(
      "Ejerbolig" = "black",
      "Andelsbolig" = "grey30",
      "Privat lejebolig" = "grey55",
      "Almen lejebolig" = "grey75"
    )
  ) +
  labs(
    title = "Afstand til respondentens bolig efter boform",
    x = "Sandsynlighed for at boligprojektet foretrækkes (marginal means)",
    y = NULL,
    color = NULL,
    shape = NULL
  ) +
  theme(
    text = element_text(family = "Times New Roman"),
    axis.text = element_text(size = 11),
    axis.title = element_text(size = 12),
    plot.title = element_text(size = 16, face = "bold"),
    legend.text = element_text(size = 11),
    legend.position = "bottom",
    panel.background = element_rect(fill = "grey92", colour = NA),
    plot.background = element_rect(fill = "white", colour = NA),
    panel.grid.major = element_line(colour = "white", linewidth = 0.4),
    panel.grid.minor = element_blank()
  )

plot_h2_mm_alle

ggsave("plot_h2_mm_alle.png", plot = plot_h2_mm_alle, width = 8, height = 8)

## AMCE

# Ejer vs ikke ejer

amce_h2_ikke_ejer <- cj(
  data = conjoint_full %>% filter(ejer_ikke_ejer == 0),
  formula = chosen ~ afstand + hojde + andel_almene_boliger + udseende,
  id = ~participant_id,
  estimate = "amce",
) %>%
  dplyr::filter(feature == "afstand") %>%
  mutate(gruppe = "Ikke ejer")

amce_h2_ejer <- cj(
  data = conjoint_full %>% filter(ejer_ikke_ejer == 1),
  formula = chosen ~ afstand + hojde + andel_almene_boliger + udseende,
  id = ~participant_id,
  estimate = "amce",
) %>%
  dplyr::filter(feature == "afstand") %>%
  mutate(gruppe = "Ejer")

amce_h2_stram_tab <- bind_rows(amce_h2_ikke_ejer, amce_h2_ejer)

# Tjekker tabel

amce_h2_stram_tab

# Udskriver tabel

stargazer(
  amce_h2_stram_tab,
  type = "html",
  summary = FALSE,
  rownames = FALSE,
  title = "Afstand - Ejer vs ikke ejer - AMCE",
  out = "h2_amce_stram.html"
)

# Visualisering
plot_h2_amce_stram <- amce_h2_stram_tab %>%
  filter(!is.na(estimate)) %>%
  mutate(
    level = factor(level, levels = c("2 km", "1 km", "300 m")),
    gruppe = factor(gruppe, levels = c("Ikke ejer", "Ejer"))
  ) %>%
  ggplot(aes(x = estimate, y = level, color = gruppe, shape = gruppe)) +
  geom_vline(xintercept = 0, linetype = "longdash", linewidth = 0.7, color = "black") +
  geom_errorbarh(
    aes(xmin = lower, xmax = upper),
    height = 0.15,
    linewidth = 0.8,
    position = position_dodge(width = 0.5)
  ) +
  geom_point(
    size = 2.8,
    position = position_dodge(width = 0.5)
  ) +
  scale_color_manual(values = c("Ikke ejer" = "black", "Ejer" = "grey40")) +
  scale_shape_manual(values = c("Ikke ejer" = 16, "Ejer" = 17)) +
  labs(
    title = "Afstand til respondentens bolig efter ejerstatus",
    x = "Ændring i sandsynligheden for at boligprojektet foretrækkes (AMCE)",
    y = NULL,
    color = NULL,
    shape = NULL
  ) +
  theme(
    text = element_text(family = "Times New Roman"),
    axis.text = element_text(size = 11),
    axis.title = element_text(size = 12),
    plot.title = element_text(size = 16, face = "bold"),
    legend.text = element_text(size = 11),
    legend.position = "bottom",
    panel.background = element_rect(fill = "grey92", colour = NA),
    plot.background = element_rect(fill =  "white", colour = NA),
    panel.grid.major = element_line(colour = "white", linewidth = 0.4),
    panel.grid.minor = element_blank()
  )

plot_h2_amce_stram

ggsave("plot_h2_amce_stram.png", plot = plot_h2_amce_stram, width = 8, height = 4)

# Bred version - ejer/andelshaver vs lejer

amce_h2_lejer <- cj(
  data = conjoint_full %>% filter(ejer_eller_andel == 0),
  formula = chosen ~ afstand + hojde + andel_almene_boliger + udseende,
  id = ~participant_id,
  estimate = "amce"
) %>%
  dplyr::filter(feature == "afstand") %>%
  mutate(gruppe = "Lejer")

amce_h2_ejer_andel <- cj(
  data = conjoint_full %>% filter(ejer_eller_andel == 1),
  formula = chosen ~ afstand + hojde + andel_almene_boliger + udseende,
  id = ~participant_id,
  estimate = "amce"
) %>%
  dplyr::filter(feature == "afstand") %>%
  mutate(gruppe = "Ejer/andel")

amce_h2_bred_tab <- bind_rows(amce_h2_lejer, amce_h2_ejer_andel)

# Tjekker tabel

amce_h2_bred_tab

# Udskriver tabel

stargazer(
  amce_h2_bred_tab,
  type = "html",
  summary = FALSE,
  rownames = FALSE,
  title = "Afstand - Ejer/andelshaver vs lejer - AMCE",
  out = "h2_amce_bred.html"
)

# Visualisering
plot_h2_amce_bred <- amce_h2_bred_tab %>%
  filter(!is.na(estimate)) %>%
  mutate(
    level = recode(
      level,
      "1 kilometer" = "1 km",
      "2 kilometer" = "2 km"
    ),
    level = factor(level, levels = c("2 km", "1 km", "300 m")),
    gruppe = factor(gruppe, levels = c("Ejer/andel", "Lejer"))
  ) %>%
  ggplot(aes(x = estimate, y = level, color = gruppe, shape = gruppe)) +
  geom_vline(xintercept = 0, linetype = "longdash", linewidth = 0.7, color = "black") +
  geom_errorbarh(
    aes(xmin = lower, xmax = upper),
    height = 0.15,
    linewidth = 0.8,
    position = position_dodge(width = 0.5)
  ) +
  geom_point(
    size = 2.8,
    position = position_dodge(width = 0.5)
  ) +
  scale_color_manual(values = c("Lejer" = "black", "Ejer/andel" = "grey40")) +
  scale_shape_manual(values = c("Lejer" = 16, "Ejer/andel" = 17)) +
  labs(
    title = "Afstand til respondentens bolig efter ejerstatus",
    x = "Ændring i sandsynligheden for at boligprojektet foretrækkes (AMCE)",
    y = NULL,
    color = NULL,
    shape = NULL
  ) +
  theme(
    text = element_text(family = "Times New Roman"),
    axis.text = element_text(size = 11),
    axis.title = element_text(size = 12),
    plot.title = element_text(size = 16, face = "bold"),
    legend.text = element_text(size = 11),
    legend.position = "bottom",
    panel.background = element_rect(fill = "grey92", colour = NA),
    plot.background = element_rect(fill = "white", colour = NA),
    panel.grid.major = element_line(colour = "white", linewidth = 0.4),
    panel.grid.minor = element_blank()
  )

plot_h2_amce_bred

# Gemmer
ggsave("plot_h2_amce_bred.png", plot = plot_h2_amce_bred, width = 8, height = 4)


# Alle fire grupper



amce_h2_ejerbolig <- cj(
  data = conjoint_full %>% filter(boform == "Ejerbolig"),
  formula = chosen ~ afstand + hojde + andel_almene_boliger + udseende,
  id = ~participant_id,
  estimate = "amce",
) %>%
  dplyr::filter(feature == "afstand") %>%
  mutate(gruppe = "Ejerbolig")

amce_h2_andelsbolig <- cj(
  data = conjoint_full %>% filter(boform == "Andelsbolig"),
  formula = chosen ~ afstand + hojde + andel_almene_boliger + udseende,
  id = ~participant_id,
  estimate = "amce",
) %>%
  dplyr::filter(feature == "afstand") %>%
  mutate(gruppe = "Andelsbolig")

amce_h2_privatleje <- cj(
  data = conjoint_full %>% filter(boform == "Privat lejebolig"),
  formula = chosen ~ afstand + hojde + andel_almene_boliger + udseende,
  id = ~participant_id,
  estimate = "amce",
) %>%
  dplyr::filter(feature == "afstand") %>%
  mutate(gruppe = "Privat lejebolig")

amce_h2_almenleje <- cj(
  data = conjoint_full %>% filter(boform == "Almen lejebolig"),
  formula = chosen ~ afstand + hojde + andel_almene_boliger + udseende,
  id = ~participant_id,
  estimate = "amce",
) %>%
  dplyr::filter(feature == "afstand") %>%
  mutate(gruppe = "Almen lejebolig")

# Samler i en tabel

amce_h2_alle <- bind_rows(
  amce_h2_ejerbolig,
  amce_h2_andelsbolig,
  amce_h2_privatleje,
  amce_h2_almenleje
)

amce_h2_alle

# Udskriver tabel

stargazer(
  amce_h2_alle,
  type = "html",
  summary = FALSE,
  rownames = FALSE,
  title = "Afstand - boform - AMCE",
  out = "h2_amce_alle.html"
)

# Visualisering

plot_h2_amce_alle <- amce_h2_alle %>%
  filter(!is.na(estimate)) %>%
  mutate(
    level = recode(
      level,
      "1 kilometer" = "1 km",
      "2 kilometer" = "2 km"
    ),
    level = factor(level, levels = c("2 km", "1 km", "300 m")),
    gruppe = factor(
      gruppe,
      levels = c(
        "Ejerbolig",
        "Andelsbolig",
        "Privat lejebolig",
        "Almen lejebolig"
      )
    )
  ) %>%
  ggplot(aes(x = estimate, y = level, color = gruppe, shape = gruppe)) +
  geom_vline(
    xintercept = 0,
    linetype = "longdash",
    linewidth = 0.7,
    color = "black"
  ) +
  geom_errorbarh(
    aes(xmin = lower, xmax = upper),
    height = 0.15,
    linewidth = 0.8,
    position = position_dodge(width = 0.5)
  ) +
  geom_point(
    size = 2.8,
    position = position_dodge(width = 0.5)
  ) +
  scale_shape_manual(
    values = c(
      "Ejerbolig" = 16,
      "Andelsbolig" = 17,
      "Privat lejebolig" = 15,
      "Almen lejebolig" = 18
    )
  ) +
  scale_color_manual(
    values = c(
      "Ejerbolig" = "black",
      "Andelsbolig" = "grey30",
      "Privat lejebolig" = "grey55",
      "Almen lejebolig" = "grey75"
    )
  ) +
  labs(
    title = "Afstand til respondentens bolig efter boform",
    x = "Ændring i sandsynligheden for at boligprojektet foretrækkes (AMCE)",
    y = NULL,
    color = NULL,
    shape = NULL
  ) +
  theme(
    text = element_text(family = "Times New Roman"),
    axis.text = element_text(size = 11),
    axis.title = element_text(size = 12),
    plot.title = element_text(size = 16, face = "bold"),
    legend.text = element_text(size = 11),
    legend.position = "bottom",
    panel.background = element_rect(fill = "grey92", colour = NA),
    plot.background = element_rect(fill = "white", colour = NA),
    panel.grid.major = element_line(colour = "white", linewidth = 0.4),
    panel.grid.minor = element_blank()
  )

plot_h2_amce_alle

ggsave("plot_h2_amce_alle.png", plot = plot_h2_amce_alle, width = 8, height = 6)

# H2 - interaktion mellem ejer/ikke ejer og afstand - formel test

mm_h2_int_stram <- cj(
  data = conjoint_full,
  formula = chosen ~ afstand,
  id = ~ participant_id,
  estimate = "mm",
  h0 = 0.5,
  by = ~ ejergruppe_stram
)

mm_h2_int_stram
plot(mm_h2_int_stram, group = "ejergruppe_stram", vline = 0.5)

# Omnibus test

H2_omnibus <- cj_anova(
  data = conjoint_full,
  formula = chosen ~ afstand,
  id = ~ participant_id,
  by = ~ ejergruppe_stram
)

# Tjekker 
H2_omnibus

# Udskriver

stargazer(
  H2_omnibus,
  type = "html",
  summary = FALSE,
  rownames = FALSE,
  title = "H2 omnibus-test, ejere x afstand",
  out = "h2_omnibus.html"
)

# Omnibus test bred

H2_omnibus_bred <- cj_anova(
  data = conjoint_full,
  formula = chosen ~ afstand,
  id = ~ participant_id,
  by = ~ ejergruppe
)

# Tjekker 
H2_omnibus_bred

# Udskriver

stargazer(
  H2_omnibus_bred,
  type = "html",
  summary = FALSE,
  rownames = FALSE,
  title = "H2 omnibus-test, ejere x afstand (ejer/andel)",
  out = "h2_omnibus_bred.html"
)

# Insignifikant sammenhæng

## Hypotese 3: Andel almene boliger

# Starter med marginal means

mm_h3 <- cj(
  data = conjoint_full,
  formula = chosen ~ afstand + hojde + andel_almene_boliger + udseende,
  id = ~ participant_id,
  estimate = "mm",
  h0 = 0.5
) %>%
  dplyr::filter(feature == "andel_almene_boliger")

mm_h3

# Laver tabel

tab_h3_mm <- mm_h3 %>%
  transmute(
    Niveau = level,
    estimate = round(estimate, 3),
    `95% KI` = sprintf("[%.3f, %.3f]", lower, upper),
    `p-værdi` = ifelse(
      is.na(p), "",
      ifelse(p < 0.001, "<0.001", sprintf("%.3f", p))
    )
  )

tab_h3_mm

# Udskriver tabel

stargazer(
  tab_h3_mm,
  type = "html",
  summary = FALSE,
  rownames = FALSE,
  title = "Andel almene boliger - MM",
  out = "h3_mm.html"
)

# Visualisering

plot_h3_mm <- mm_h3 %>%
  mutate(
    level = factor(level, levels = c("100 %", "50 %", "25 %", "0 %"))
  ) %>%
  ggplot(aes(x = estimate, y = level)) +
  geom_vline(xintercept = 0.5, linetype = "longdash", linewidth = 0.7, color = "black") +
  geom_errorbarh(aes(xmin = lower, xmax = upper), height = 0.15, linewidth = 0.8) +
  geom_point(size = 2.8) +
  labs(
    title = "Andel almennyttig boliger",
    x = "Sandsynlighed for at boligprojektet foretrækkes (marginal means)",
    y = NULL
  ) +
  theme(
    text = element_text(family = "Times New Roman"),
    axis.text = element_text(size = 11),
    axis.title = element_text(size = 12),
    plot.title = element_text(size = 16, face = "bold"),
    panel.background = element_rect(fill = "grey92", colour = NA),
    plot.background = element_rect(fill = "white", colour = NA),
    panel.grid.major = element_line(colour = "white", linewidth = 0.4),
    panel.grid.minor = element_blank()
  )

plot_h3_mm

ggsave("plot_h3_mm.png", plot = plot_h3_mm, width = 8, height = 4)

# H3 AMCE

amce_h3 <- cj(
  data = conjoint_full,
  formula = chosen ~ afstand + hojde + andel_almene_boliger + udseende,
  id = ~ participant_id,
  estimate = "amce"
) %>%
  dplyr::filter(feature == "andel_almene_boliger")

amce_h3

# sætter op i tabel
tab_h3_amce <- amce_h3 %>%
  mutate(
    level,
  ) %>%
  transmute(
    Niveau = level,
    AMCE = round(estimate, 3),
    `95% KI` = sprintf("[%.3f, %.3f]", lower, upper),
    `p-værdi` = ifelse(
      is.na(p), "",
      ifelse(p < 0.001, "<0.001", sprintf("%.3f", p))
    )
  )

tab_h3_amce

# Udskriver tabel 
stargazer(
  tab_h3_amce,
  type = "html",
  summary = FALSE,
  rownames = FALSE,
  title = "Andel almennyttige boliger - AMCE",
  out = "h3_amce.html"
)

# Visualisering

plot_h3_amce <- amce_h3 %>%
  filter(!is.na(estimate)) %>%
  mutate(
    ,
    level = factor(level, levels = c("100 %", "50 %", "25 %", "0 %"))
  ) %>%
  ggplot(aes(x = estimate, y = level)) +
  geom_vline(xintercept = 0, linetype = "longdash", linewidth = 0.7, color = "black") +
  geom_errorbarh(aes(xmin = lower, xmax = upper), height = 0.15, linewidth = 0.8) +
  geom_point(size = 2.8) +
  labs(
    title = "Andel almennyttige boliger",
    x = "Ændring i sandsynligheden for at boligprojektet foretrækkes (AMCE)",
    y = NULL
  ) +
  theme(
    text = element_text(family = "Times New Roman"),
    axis.text = element_text(size = 11),
    axis.title = element_text(size = 12),
    plot.title = element_text(size = 16, face = "bold"),
    panel.background = element_rect(fill = "grey92", colour = NA),
    plot.background = element_rect(fill = "white", colour = NA),
    panel.grid.major = element_line(colour = "white", linewidth = 0.4),
    panel.grid.minor = element_blank()
  )

plot_h3_amce
ggsave("plot_h3_amce.png", plot = plot_h3_amce, width = 8, height = 4)

### Test af holdninger til omfordeling og indvandrings indvirkning

# Først opdeles begge indeks i lav, mellem og høj
# Opdeling: 1 - 2.33 --> lav, 2.34 - 3.66 --> mellem, 3.67 - 5 --> høj

# Laver grupper på respondentniveau
moderator_grupper <- conjoint_full %>%
  distinct(participant_id, indvandringsskepsis, omfordeling) %>%
  mutate(
    indvandringsskepsis_gruppe = cut(
      indvandringsskepsis,
      breaks = c(1, 7/3, 11/3, 5),
      include.lowest = TRUE,
      labels = c("Lav", "Mellem", "Høj")
    ),
    omfordeling_gruppe = cut(
      omfordeling,
      breaks = c(1, 7/3, 11/3, 5),
      include.lowest = TRUE,
      labels = c("Lav", "Mellem", "Høj")
    )
  )

# Merger grupperne tilbage på conjoint-datasættet
conjoint_full <- conjoint_full %>%
  select(-any_of(c("indvandringsskepsis_gruppe", "omfordeling_gruppe"))) %>%
  left_join(
    moderator_grupper %>%
      select(participant_id, indvandringsskepsis_gruppe, omfordeling_gruppe),
    by = "participant_id"
  )

# Tjekker fordeling

moderator_grupper %>%
  count(indvandringsskepsis_gruppe)

moderator_grupper %>%
  count(omfordeling_gruppe)

moderator_grupper %>%
  group_by(omfordeling_gruppe) %>%
  summarise(gennemsnit = mean(omfordeling, na.rm = TRUE))

# Note: de vender hver sin vej - høj indvandringsskepsis = negativ overfor indvandrer, høj omfordeling = positiv overfor omfordeling

## Indvandringsskepsis

mm_h3_indvandring <- cj(
  data = conjoint_full %>% filter(!is.na(indvandringsskepsis_gruppe)),
  formula = chosen ~ andel_almene_boliger,
  id = ~ participant_id,
  estimate = "mm",
  h0 = 0.5,
  by = ~ indvandringsskepsis_gruppe
)

mm_h3_indvandring

# Udskriver tabel
stargazer(
  mm_h3_indvandring,
  type = "html",
  summary = FALSE,
  rownames = FALSE,
  title = "Indvandringsskepsis x Andel almennyttige boliger - MM",
  out = "h3_indvandring_mm.html"
)

# Visualisering

plot_h3_indvandring_mm <- mm_h3_indvandring %>%
  mutate(
    indvandringsskepsis_gruppe = factor(
      indvandringsskepsis_gruppe,
      levels = c("Lav", "Mellem", "Høj"),
      labels = c( # Tilføjer indvandringsskepsis 
        "Lav indvandringsskepsis", 
        "Mellem indvandringsskepsis",
        "Høj indvandringsskepsis"
      )
    ),
    level = factor(level, levels = c("100 %", "50 %", "25 %", "0 %"))
  ) %>%
  ggplot(aes(x = estimate, y = level, color = indvandringsskepsis_gruppe, shape = indvandringsskepsis_gruppe)) +
  geom_vline(xintercept = 0.5, linetype = "longdash", linewidth = 0.7, color = "black") +
  geom_errorbarh(
    aes(xmin = lower, xmax = upper),
    height = 0.15,
    linewidth = 0.8,
    position = position_dodge(width = 0.5)
  ) +
  geom_point(
    size = 2.8,
    position = position_dodge(width = 0.5)
  ) +
  scale_color_manual(values = c(
    "Lav indvandringsskepsis" = "black",
    "Mellem indvandringsskepsis" = "grey40",
    "Høj indvandringsskepsis" = "grey70"
  )) +
  scale_shape_manual(values = c(
    "Lav indvandringsskepsis" = 16,
    "Mellem indvandringsskepsis" = 17,
    "Høj indvandringsskepsis" = 15
  )) +
  labs(
    title = "Andel almennyttige boliger efter indvandringsskepsis",
    x = "Sandsynlighed for at boligprojektet foretrækkes (marginal means)",
    y = NULL,
    color = NULL,
    shape = NULL
  ) +
  theme(
    text = element_text(family = "Times New Roman"),
    axis.text = element_text(size = 11),
    axis.title = element_text(size = 12),
    plot.title = element_text(size = 16, face = "bold"),
    legend.text = element_text(size = 11),
    legend.position = "bottom",
    panel.background = element_rect(fill = "grey92", colour = NA),
    plot.background = element_rect(fill = "white", colour = NA),
    panel.grid.major = element_line(colour = "white", linewidth = 0.4),
    panel.grid.minor = element_blank()
  )

plot_h3_indvandring_mm
plot_h3_indvandring_mm

ggsave("plot_h3_indvandring_mm.png", plot = plot_h3_indvandring_mm, width = 8, height = 5)

# Omnibus test

omnibus_h3_indvandring <- cj_anova(
  data = conjoint_full %>% filter(!is.na(indvandringsskepsis_gruppe)),
  formula = chosen ~ andel_almene_boliger,
  id = ~ participant_id,
  by = ~ indvandringsskepsis_gruppe
)

# Udskriver tabel
stargazer(
  omnibus_h3_indvandring,
  type = "html",
  summary = FALSE,
  rownames = FALSE,
  title = "H3 omnibus-test, indvandringsskepsis x almennyttige boliger",
  out = "h3_omnibus_indvandring_mm.html"
)


## Omfordeling

mm_h3_omfordeling <- cj(
  data = conjoint_full %>% filter(!is.na(omfordeling_gruppe)),
  formula = chosen ~ andel_almene_boliger,
  id = ~ participant_id,
  estimate = "mm",
  h0 = 0.5,
  by = ~ omfordeling_gruppe
)

mm_h3_omfordeling

# Udskriver tabel

stargazer(
  mm_h3_omfordeling,
  type = "html",
  summary = FALSE,
  rownames = FALSE,
  title = "Omfordeling x Andel almennyttige boliger - MM",
  out = "h3_omfordeling_mm.html"
)


# Visualisering

plot_h3_omfordeling_mm <- mm_h3_omfordeling %>%
  mutate(
    omfordeling_gruppe = factor(
      omfordeling_gruppe,
      levels = c("Lav", "Mellem", "Høj"),
      labels = c(
        "Lav støtte til omfordeling",
        "Mellem støtte til omfordeling",
        "Høj støtte til omfordeling"
      )
    ),
    level = factor(level, levels = c("100 %", "50 %", "25 %", "0 %"))
  ) %>%
  ggplot(aes(x = estimate, y = level, color = omfordeling_gruppe, shape = omfordeling_gruppe)) +
  geom_vline(xintercept = 0.5, linetype = "longdash", linewidth = 0.7, color = "black") +
  geom_errorbarh(
    aes(xmin = lower, xmax = upper),
    height = 0.15,
    linewidth = 0.8,
    position = position_dodge(width = 0.5)
  ) +
  geom_point(
    size = 2.8,
    position = position_dodge(width = 0.5)
  ) +
  scale_color_manual(values = c(
    "Lav støtte til omfordeling" = "black",
    "Mellem støtte til omfordeling" = "grey40",
    "Høj støtte til omfordeling" = "grey70"
  )) +
  scale_shape_manual(values = c(
    "Lav støtte til omfordeling" = 16,
    "Mellem støtte til omfordeling" = 17,
    "Høj støtte til omfordeling" = 15
  )) +
  labs(
    title = "Andel almennyttige boliger efter støtte til omfordeling",
    x = "Sandsynlighed for at boligprojektet foretrækkes (marginal means)",
    y = NULL,
    color = NULL,
    shape = NULL
  ) +
  theme(
    text = element_text(family = "Times New Roman"),
    axis.text = element_text(size = 11),
    axis.title = element_text(size = 12),
    plot.title = element_text(size = 16, face = "bold"),
    legend.text = element_text(size = 11),
    legend.position = "bottom",
    panel.background = element_rect(fill = "grey92", colour = NA),
    plot.background = element_rect(fill = "white", colour = NA),
    panel.grid.major = element_line(colour = "white", linewidth = 0.4),
    panel.grid.minor = element_blank()
  )

plot_h3_omfordeling_mm

ggsave("plot_h3_omfordeling_mm.png", plot = plot_h3_omfordeling_mm, width = 8, height = 5)

# Omnibustest

omnibus_h3_omfordeling <- cj_anova(
  data = conjoint_full %>% filter(!is.na(omfordeling_gruppe)),
  formula = chosen ~ andel_almene_boliger,
  id = ~ participant_id,
  by = ~ omfordeling_gruppe
)

omnibus_h3_omfordeling

# Udskriver tabel
stargazer(
  omnibus_h3_omfordeling,
  type = "html",
  summary = FALSE,
  rownames = FALSE,
  title = "H3 omnibus-test, omfordeling x almennyttige boliger",
  out = "h3_omnibus_omfordeling_mm.html"
)

# H3 - test om almennyttige boliger er mere populære i områder med mange almennyttige boliger

# Bydelens andel almennyttige boliger. Data findes i vedlagt excelark. 

# Laver datasæt - andel almennyttige boliger angivet i procent.

bydel_almene <- tibble::tribble(
  ~bydel, ~andel_almen_pct,
  "Indre by", 5,
  "Østerbro", 12,
  "Frederiksberg", 12,
  "Vanløse", 14,
  "Amager Øst", 16,
  "Vesterbro/Kgs. Enghave", 16,
  "Valby", 20,
  "Amager Vest", 20,
  "Nørrebro", 22,
  "Bispebjerg/Nordvest", 31,
  "Brønshøj-Husum", 41
) %>%
  mutate(
    almen_bydel_gruppe = if_else(
      andel_almen_pct >= 20,
      "20 % eller mere",
      "Under 20 %"
    ),
    almen_bydel_gruppe = factor(
      almen_bydel_gruppe,
      levels = c("Under 20 %", "20 % eller mere")
    )
  )

bydel_almene

# Merger med conjoint

conjoint_full <- conjoint_full %>%
  select(-any_of(c("andel_almen_pct", "almen_bydel_gruppe"))) %>%
  left_join(
    bydel_almene,
    by = "bydel"
  )

# Tjekker at tallene stemmer

conjoint_full %>%
  distinct(participant_id, bydel, andel_almen_pct, almen_bydel_gruppe) %>%
  count(almen_bydel_gruppe)

conjoint_full %>%
  distinct(participant_id, bydel, andel_almen_pct, almen_bydel_gruppe) %>%
  arrange(andel_almen_pct)

# Det gør de

# Laver MM grupperet på 20 % og mere eller under 20 %

mm_h3_bydel_almen <- cj(
  data = conjoint_full %>% filter(!is.na(almen_bydel_gruppe)),
  formula = chosen ~ andel_almene_boliger,
  id = ~ participant_id,
  estimate = "mm",
  h0 = 0.5,
  by = ~ almen_bydel_gruppe
)

mm_h3_bydel_almen

# Udskriver tabel
stargazer(
  mm_h3_bydel_almen,
  type = "html",
  summary = FALSE,
  rownames = FALSE,
  title = "Koncentration af almennyttige boliger i bydel x andel almennyttige boliger",
  out = "h3_bydel_almen.html"
)


# Plot

plot_h3_bydel_almen_mm <- mm_h3_bydel_almen %>%
  mutate(
    almen_bydel_gruppe = factor(
      BY,
      levels = c("Under 20 %", "20 % eller mere")
    ),
    level = factor(level, levels = c("100 %", "50 %", "25 %", "0 %"))
  ) %>%
  ggplot(aes(x = estimate, y = level, color = almen_bydel_gruppe, shape = almen_bydel_gruppe)) +
  geom_vline(xintercept = 0.5, linetype = "longdash", linewidth = 0.7, color = "black") +
  geom_errorbarh(
    aes(xmin = lower, xmax = upper),
    height = 0.15,
    linewidth = 0.8,
    position = position_dodge(width = 0.5)
  ) +
  geom_point(
    size = 2.8,
    position = position_dodge(width = 0.5)
  ) +
  scale_color_manual(
    values = c("Under 20 %" = "black", "20 % eller mere" = "grey50")
  ) +
  scale_shape_manual(
    values = c("Under 20 %" = 16, "20 % eller mere" = 17)
  ) +
  labs(
    title = "Andel almennyttige boliger efter bydelens koncentration af almennyttige boliger",
    x = "Sandsynlighed for at boligprojektet foretrækkes (marginal means)",
    y = NULL,
    color = NULL,
    shape = NULL
  ) +
  theme(
    text = element_text(family = "Times New Roman"),
    axis.text = element_text(size = 11),
    axis.title = element_text(size = 12),
    plot.title = element_text(size = 16, face = "bold"),
    legend.text = element_text(size = 11),
    legend.position = "bottom",
    panel.background = element_rect(fill = "grey92", colour = NA),
    plot.background = element_rect(fill = "white", colour = NA),
    panel.grid.major = element_line(colour = "white", linewidth = 0.4),
    panel.grid.minor = element_blank()
  )

plot_h3_bydel_almen_mm
ggsave("plot_h3_bydel_almen_mm.png", plot = plot_h3_bydel_almen_mm, width = 9, height = 6)

# Omnibus test
omnibus_h3_bydel_almen <- cj_anova(
  data = conjoint_full %>% filter(!is.na(almen_bydel_gruppe)),
  formula = chosen ~ andel_almene_boliger,
  id = ~ participant_id,
  by = ~ almen_bydel_gruppe
)

omnibus_h3_bydel_almen


### Hypotese 4 - højde og udseende

# Starter med højde

# MM

mm_h4_hojde <- cj(
  data = conjoint_full,
  formula = chosen ~ afstand + hojde + andel_almene_boliger + udseende,
  id = ~ participant_id,
  estimate = "mm",
  h0 = 0.5
) %>%
  dplyr::filter(feature == "hojde")

mm_h4_hojde

# LAver tabel

tab_h4_hojde_mm <- mm_h4_hojde %>%
  transmute(
    Niveau = level,
    estimate = round(estimate, 3),
    `95% KI` = sprintf("[%.3f, %.3f]", lower, upper),
    `p-værdi` = ifelse(
      is.na(p), "",
      ifelse(p < 0.001, "<0.001", sprintf("%.3f", p))
    )
  )

tab_h4_hojde_mm

# Udskriver tabel

stargazer(
  tab_h4_hojde_mm,
  type = "html",
  summary = FALSE,
  rownames = FALSE,
  title = "Højde - MM",
  out = "h4_hojde_mm.html"
)

# Visualisering

plot_h4_hojde_mm <- mm_h4_hojde %>%
  mutate(
    level = factor(level, levels = c("8 etager", "5 etager", "3 etager"))
  ) %>%
  ggplot(aes(x = estimate, y = level)) +
  geom_vline(xintercept = 0.5, linetype = "longdash", linewidth = 0.7, color = "black") +
  geom_errorbarh(aes(xmin = lower, xmax = upper), height = 0.15, linewidth = 0.8) +
  geom_point(size = 2.8) +
  labs(
    title = "Højde",
    x = "Sandsynlighed for at boligprojektet foretrækkes (marginal means)",
    y = NULL
  ) +
  theme(
    text = element_text(family = "Times New Roman"),
    axis.text = element_text(size = 11),
    axis.title = element_text(size = 12),
    plot.title = element_text(size = 16, face = "bold"),
    panel.background = element_rect(fill = "grey92", colour = NA),
    plot.background = element_rect(fill = "white", colour = NA),
    panel.grid.major = element_line(colour = "white", linewidth = 0.4),
    panel.grid.minor = element_blank()
  )

plot_h4_hojde_mm

ggsave("plot_h4_hojde_mm.png", plot = plot_h4_hojde_mm, width = 8, height = 6)

# Højde AMCE

amce_h4_hojde <- cj(
  data = conjoint_full,
  formula = chosen ~ afstand + hojde + andel_almene_boliger + udseende,
  id = ~ participant_id,
  estimate = "amce"
) %>%
  dplyr::filter(feature == "hojde")

amce_h4_hojde

# Laver tabel

tab_h4_hojde_amce <- amce_h4_hojde %>%
  transmute(
    Niveau = level,
    AMCE = round(estimate, 3),
    `95% KI` = sprintf("[%.3f, %.3f]", lower, upper),
    `p-værdi` = ifelse(
      is.na(p), "",
      ifelse(p < 0.001, "<0.001", sprintf("%.3f", p))
    )
  )

tab_h4_hojde_amce

# Udskriver tabel

stargazer(
  tab_h4_hojde_amce,
  type = "html",
  summary = FALSE,
  rownames = FALSE,
  title = "Højde - AMCE",
  out = "h4_hojde_amce.html"
)

# Visualisering

plot_h4_hojde_amce <- amce_h4_hojde %>%
  filter(!is.na(estimate)) %>%
  mutate(
    level = factor(level, levels = c("8 etager", "5 etager", "3 etager"))
  ) %>%
  ggplot(aes(x = estimate, y = level)) +
  geom_vline(xintercept = 0, linetype = "longdash", linewidth = 0.7, color = "black") +
  geom_errorbarh(aes(xmin = lower, xmax = upper), height = 0.15, linewidth = 0.8) +
  geom_point(size = 2.8) +
  labs(
    title = "Højde",
    x = "Ændring i sandsynligheden for at boligprojektet foretrækkes (AMCE)",
    y = NULL
  ) +
  theme(
    text = element_text(family = "Times New Roman"),
    axis.text = element_text(size = 11),
    axis.title = element_text(size = 12),
    plot.title = element_text(size = 16, face = "bold"),
    panel.background = element_rect(fill = "grey92", colour = NA),
    plot.background = element_rect(fill = "white", colour = NA),
    panel.grid.major = element_line(colour = "white", linewidth = 0.4),
    panel.grid.minor = element_blank()
  )

plot_h4_hojde_amce

ggsave("plot_h4_hojde_amce.png", plot = plot_h4_hojde_amce, width = 8, height = 6)


# Udseende

# Marginal means

mm_h4_udseende <- cj(
  data = conjoint_full,
  formula = chosen ~ afstand + hojde + andel_almene_boliger + udseende,
  id = ~ participant_id,
  estimate = "mm",
  h0 = 0.5
) %>%
  dplyr::filter(feature == "udseende")

mm_h4_udseende

# Laver tabel

tab_h4_udseende_mm <- mm_h4_udseende %>%
  transmute(
    Niveau = level,
    estimate = round(estimate, 3),
    `95% KI` = sprintf("[%.3f, %.3f]", lower, upper),
    `p-værdi` = ifelse(
      is.na(p), "",
      ifelse(p < 0.001, "<0.001", sprintf("%.3f", p))
    )
  )

tab_h4_udseende_mm

# Udskriver tabel

stargazer(
  tab_h4_udseende_mm,
  type = "html",
  summary = FALSE,
  rownames = FALSE,
  title = "Udseende - MM",
  out = "h4_udseende_mm.html"
)

# Visualisering

plot_h4_udseende_mm <- mm_h4_udseende %>%
  mutate(
    level = factor(level, levels = c("Passer dårligt", "Passer nogenlunde", "Passer godt"))
  ) %>%
  ggplot(aes(x = estimate, y = level)) +
  geom_vline(xintercept = 0.5, linetype = "longdash", linewidth = 0.7, color = "black") +
  geom_errorbarh(aes(xmin = lower, xmax = upper), height = 0.15, linewidth = 0.8) +
  geom_point(size = 2.8) +
  labs(
    title = "Udseende",
    x = "Sandsynlighed for at boligprojektet foretrækkes (marginal means)",
    y = NULL
  ) +
  theme(
    text = element_text(family = "Times New Roman"),
    axis.text = element_text(size = 11),
    axis.title = element_text(size = 12),
    plot.title = element_text(size = 16, face = "bold"),
    panel.background = element_rect(fill = "grey92", colour = NA),
    plot.background = element_rect(fill = "white", colour = NA),
    panel.grid.major = element_line(colour = "white", linewidth = 0.4),
    panel.grid.minor = element_blank()
  )

plot_h4_udseende_mm

ggsave("plot_h4_udseende_mm.png", plot = plot_h4_udseende_mm, width = 8, height = 6)

# AMCE

amce_h4_udseende <- cj(
  data = conjoint_full,
  formula = chosen ~ afstand + hojde + andel_almene_boliger + udseende,
  id = ~ participant_id,
  estimate = "amce"
) %>%
  dplyr::filter(feature == "udseende")

amce_h4_udseende

# Laver tabel

tab_h4_udseende_amce <- amce_h4_udseende %>%
  transmute(
    Niveau = level,
    AMCE = round(estimate, 3),
    `95% KI` = sprintf("[%.3f, %.3f]", lower, upper),
    `p-værdi` = ifelse(
      is.na(p), "",
      ifelse(p < 0.001, "<0.001", sprintf("%.3f", p))
    )
  )

tab_h4_udseende_amce

# Udskriver tabel

stargazer(
  tab_h4_udseende_amce,
  type = "html",
  summary = FALSE,
  rownames = FALSE,
  title = "Udseende - AMCE",
  out = "h4_udseende_amce.html"
)

# Visualisering

plot_h4_udseende_amce <- amce_h4_udseende %>%
  filter(!is.na(estimate)) %>%
  mutate(
    level = factor(level, levels = c("Passer dårligt", "Passer nogenlunde", "Passer godt"))
  ) %>%
  ggplot(aes(x = estimate, y = level)) +
  geom_vline(xintercept = 0, linetype = "longdash", linewidth = 0.7, color = "black") +
  geom_errorbarh(aes(xmin = lower, xmax = upper), height = 0.15, linewidth = 0.8) +
  geom_point(size = 2.8) +
  labs(
    title = "Udseende",
    x = "Ændring i sandsynligheden for at boligprojektet foretrækkes (AMCE)",
    y = NULL
  ) +
  theme(
    text = element_text(family = "Times New Roman"),
    axis.text = element_text(size = 11),
    axis.title = element_text(size = 12),
    plot.title = element_text(size = 16, face = "bold"),
    panel.background = element_rect(fill = "grey92", colour = NA),
    plot.background = element_rect(fill = "white", colour = NA),
    panel.grid.major = element_line(colour = "white", linewidth = 0.4),
    panel.grid.minor = element_blank()
  )

plot_h4_udseende_amce

ggsave("plot_h4_udseende_amce.png", plot = plot_h4_udseende_amce, width = 8, height = 6)

# Umiddelbart opbakning til H4!

# Sætter moderatorer op - stedtilknytningindeks og antal år i lokalområde

moderator_h4 <- conjoint_full %>%
  distinct(participant_id, stedtilknytning, år_i_lokalområdet) %>%
  mutate(
    stedtilknytning_gruppe = cut(
      stedtilknytning,
      breaks = c(1, 7/3, 11/3, 5), # Inddeler stedtilknytning i lav, mellem og høj ligesom med omfordeling og indvandring
      include.lowest = TRUE,
      labels = c("Lav", "Mellem", "Høj")
    ),
    år_i_lokalområdet = factor(
      år_i_lokalområdet,
      levels = c("0-2 år", "3-5 år", "6-11 år", "11-20 år", "Mere end 20 år")
    )
  )

conjoint_full <- conjoint_full %>%
  select(-any_of(c("stedtilknytning_gruppe", "år_i_lokalområdet"))) %>%
  left_join(
    moderator_h4 %>%
      select(participant_id, stedtilknytning_gruppe, år_i_lokalområdet),
    by = "participant_id"
  )

# Tjekker fordeling
moderator_h4 %>%
  count(stedtilknytning_gruppe)

moderator_h4 %>%
  count(år_i_lokalområdet)

# Højde x stedtilknytning - MM

mm_h4_hojde_tilknytning <- cj(
  data = conjoint_full %>% filter(!is.na(stedtilknytning_gruppe)),
  formula = chosen ~ hojde,
  id = ~ participant_id,
  estimate = "mm",
  h0 = 0.5,
  by = ~ stedtilknytning_gruppe
)

mm_h4_hojde_tilknytning

# Udskriver tabel

stargazer(
  mm_h4_hojde_tilknytning,
  type = "html",
  summary = FALSE,
  rownames = FALSE,
  title = "stedtilknytning x Højde - MM",
  out = "h4_tilknytning_hojde_mm.html"
)

# Visualisering

plot_h4_tilknytning_hojde_mm <- mm_h4_hojde_tilknytning %>%
  mutate(
    stedtilknytning_gruppe = factor(stedtilknytning_gruppe, levels = c("Lav", "Mellem", "Høj")),
    level = factor(level, levels = c("8 etager", "5 etager", "3 etager"))
  ) %>%
  ggplot(aes(x = estimate, y = level, color = stedtilknytning_gruppe, shape = stedtilknytning_gruppe)) +
  geom_vline(xintercept = 0.5, linetype = "longdash", linewidth = 0.7, color = "black") +
  geom_errorbarh(
    aes(xmin = lower, xmax = upper),
    height = 0.15,
    linewidth = 0.8,
    position = position_dodge(width = 0.5)
  ) +
  geom_point(size = 2.8, position = position_dodge(width = 0.5)) +
  scale_color_manual(values = c("Lav" = "black", "Mellem" = "grey40", "Høj" = "grey70")) +
  scale_shape_manual(values = c("Lav" = 16, "Mellem" = 17, "Høj" = 15)) +
  labs(
    title = "Højde efter stedtilknytning",
    x = "Sandsynlighed for at boligprojektet foretrækkes (marginal means)",
    y = NULL,
    color = NULL,
    shape = NULL
  ) +
  theme(
    text = element_text(family = "Times New Roman"),
    axis.text = element_text(size = 11),
    axis.title = element_text(size = 12),
    plot.title = element_text(size = 16, face = "bold"),
    legend.text = element_text(size = 11),
    legend.position = "bottom",
    panel.background = element_rect(fill = "grey92", colour = NA),
    plot.background = element_rect(fill = "white", colour = NA),
    panel.grid.major = element_line(colour = "white", linewidth = 0.4),
    panel.grid.minor = element_blank()
  )

plot_h4_tilknytning_hojde_mm

ggsave("plot_h4_tilknytning_hojde_mm.png", plot = plot_h4_tilknytning_hojde_mm, width = 8, height = 6)

# Omnibus test

omnibus_h4_tilknytning_hojde <- cj_anova(
  data = conjoint_full %>% filter(!is.na(stedtilknytning_gruppe)),
  formula = chosen ~ hojde,
  id = ~ participant_id,
  by = ~ stedtilknytning_gruppe
)

stargazer(
  omnibus_h4_tilknytning_hojde,
  type = "html",
  summary = FALSE,
  rownames = FALSE,
  title = "H4 omnibus-test, stedtilknytning x højde",
  out = "h4_omnibus_tilknytning_hojde.html"
)

# Udseende x Stedtilknytning - MM

mm_h4_udseende_tilknytning <- cj(
  data = conjoint_full %>% filter(!is.na(stedtilknytning_gruppe)),
  formula = chosen ~ udseende,
  id = ~ participant_id,
  estimate = "mm",
  h0 = 0.5,
  by = ~ stedtilknytning_gruppe
)

mm_h4_udseende_tilknytning

# Udskriver tabel

stargazer(
  mm_h4_udseende_tilknytning,
  type = "html",
  summary = FALSE,
  rownames = FALSE,
  title = "stedtilknytning x Udseende - MM",
  out = "h4_tilknytning_udseende_mm.html"
)

# Visualisering

plot_h4_tilknytning_udseende_mm <- mm_h4_udseende_tilknytning %>%
  mutate(
    stedtilknytning_gruppe = factor(stedtilknytning_gruppe, levels = c("Lav", "Mellem", "Høj")),
    level = factor(level, levels = c("Passer dårligt", "Passer nogenlunde", "Passer godt"))
  ) %>%
  ggplot(aes(x = estimate, y = level, color = stedtilknytning_gruppe, shape = stedtilknytning_gruppe)) +
  geom_vline(xintercept = 0.5, linetype = "longdash", linewidth = 0.7, color = "black") +
  geom_errorbarh(
    aes(xmin = lower, xmax = upper),
    height = 0.15,
    linewidth = 0.8,
    position = position_dodge(width = 0.5)
  ) +
  geom_point(size = 2.8, position = position_dodge(width = 0.5)) +
  scale_color_manual(values = c("Lav" = "black", "Mellem" = "grey40", "Høj" = "grey70")) +
  scale_shape_manual(values = c("Lav" = 16, "Mellem" = 17, "Høj" = 15)) +
  labs(
    title = "Udseende efter stedtilknytning",
    x = "Sandsynlighed for at boligprojektet foretrækkes (marginal means)",
    y = NULL,
    color = NULL,
    shape = NULL
  ) +
  theme(
    text = element_text(family = "Times New Roman"),
    axis.text = element_text(size = 11),
    axis.title = element_text(size = 12),
    plot.title = element_text(size = 16, face = "bold"),
    legend.text = element_text(size = 11),
    legend.position = "bottom",
    panel.background = element_rect(fill = "grey92", colour = NA),
    plot.background = element_rect(fill = "white", colour = NA),
    panel.grid.major = element_line(colour = "white", linewidth = 0.4),
    panel.grid.minor = element_blank()
  )

plot_h4_tilknytning_udseende_mm

ggsave("plot_h4_tilknytning_udseende_mm.png", plot = plot_h4_tilknytning_udseende_mm, width = 8, height = 6)

# Omnibus test

omnibus_h4_tilknytning_udseende <- cj_anova(
  data = conjoint_full %>% filter(!is.na(stedtilknytning_gruppe)),
  formula = chosen ~ udseende,
  id = ~ participant_id,
  by = ~ stedtilknytning_gruppe
)

stargazer(
  omnibus_h4_tilknytning_udseende,
  type = "html",
  summary = FALSE,
  rownames = FALSE,
  title = "H4 omnibus-test, stedtilknytning x udseende",
  out = "h4_omnibus_tilknytning_udseende.html"
)

# Højde x År i lokalområdet

mm_h4_år_hojde <- cj(
  data = conjoint_full %>% filter(!is.na(år_i_lokalområdet)),
  formula = chosen ~ hojde,
  id = ~ participant_id,
  estimate = "mm",
  h0 = 0.5,
  by = ~ år_i_lokalområdet
)

mm_h4_år_hojde

# Udskriver tabel

stargazer(
  mm_h4_år_hojde,
  type = "html",
  summary = FALSE,
  rownames = FALSE,
  title = "År i lokalområdet x Højde - MM",
  out = "h4_år_hojde_mm.html"
)

# Visualisering

plot_h4_år_hojde_mm <- mm_h4_år_hojde %>%
  mutate(
    BY = factor(
      BY,
      levels = c("0-2 år", "3-5 år", "6-11 år", "11-20 år", "Mere end 20 år")
    ),
    level = factor(level, levels = c("8 etager", "5 etager", "3 etager"))
  ) %>%
  ggplot(aes(x = estimate, y = level, color = BY, shape = BY)) +
  geom_vline(
    xintercept = 0.5,
    linetype = "longdash",
    linewidth = 0.7,
    color = "black"
  ) +
  geom_errorbarh(
    aes(xmin = lower, xmax = upper),
    height = 0.15,
    linewidth = 0.8,
    position = position_dodge(width = 0.6)
  ) +
  geom_point(
    size = 2.8,
    position = position_dodge(width = 0.6)
  ) +
  scale_color_manual(
    values = c(
      "0-2 år" = "black",
      "3-5 år" = "grey25",
      "6-11 år" = "grey40",
      "11-20 år" = "grey55",
      "Mere end 20 år" = "grey70"
    )
  ) +
  scale_shape_manual(
    values = c(
      "0-2 år" = 16,
      "3-5 år" = 17,
      "6-11 år" = 15,
      "11-20 år" = 18,
      "Mere end 20 år" = 8
    )
  ) +
  labs(
    title = "Højde efter antal år i lokalområdet",
    x = "Sandsynlighed for at boligprojektet foretrækkes (marginal means)",
    y = NULL,
    color = NULL,
    shape = NULL
  ) +
  theme(
    text = element_text(family = "Times New Roman"),
    axis.text = element_text(size = 11),
    axis.title = element_text(size = 12),
    plot.title = element_text(size = 16, face = "bold"),
    legend.text = element_text(size = 10),
    legend.position = "bottom",
    panel.background = element_rect(fill = "grey92", colour = NA),
    plot.background = element_rect(fill = "white", colour = NA),
    panel.grid.major = element_line(colour = "white", linewidth = 0.4),
    panel.grid.minor = element_blank()
  )

plot_h4_år_hojde_mm

ggsave("plot_h4_år_hojde_mm.png", plot = plot_h4_år_hojde_mm, width = 9, height = 6)

# Omnibus test
omnibus_h4_år_hojde <- cj_anova(
  data = conjoint_full %>% filter(!is.na(år_i_lokalområdet)),
  formula = chosen ~ hojde,
  id = ~ participant_id,
  by = ~ år_i_lokalområdet
)

stargazer(
  omnibus_h4_år_hojde,
  type = "html",
  summary = FALSE,
  rownames = FALSE,
  title = "H4 omnibus-test, år i lokalområdet x højde",
  out = "h4_omnibus_år_hojde.html"
)

# Udseende x år i lokalområdet - MM

mm_h4_år_udseende <- cj(
  data = conjoint_full %>% filter(!is.na(år_i_lokalområdet)),
  formula = chosen ~ udseende,
  id = ~ participant_id,
  estimate = "mm",
  h0 = 0.5,
  by = ~ år_i_lokalområdet
)

mm_h4_år_udseende

# Udskriver tabel

plot_h4_år_udseende_mm <- mm_h4_år_udseende %>%
  mutate(
    år_i_lokalområdet = factor(
      år_i_lokalområdet,
      levels = c("0-2 år", "3-5 år", "6-11 år", "11-20 år", "Mere end 20 år")
    ),
    level = factor(level, levels = c("Passer dårligt", "Passer nogenlunde", "Passer godt"))
  ) %>%
  ggplot(aes(x = estimate, y = level, color = år_i_lokalområdet, shape = år_i_lokalområdet)) +
  geom_vline(xintercept = 0.5, linetype = "longdash", linewidth = 0.7, color = "black") +
  geom_errorbarh(
    aes(xmin = lower, xmax = upper),
    height = 0.15,
    linewidth = 0.8,
    position = position_dodge(width = 0.6)
  ) +
  geom_point(size = 2.8, position = position_dodge(width = 0.6) )+

  scale_color_manual(
    values = c(
      "0-2 år" = "black",
      "3-5 år" = "grey25",
      "6-11 år" = "grey40",
      "11-20 år" = "grey55",
      "Mere end 20 år" = "grey70"
    )
  ) +
  scale_shape_manual(
    values = c(
      "0-2 år" = 16,
      "3-5 år" = 17,
      "6-11 år" = 15,
      "11-20 år" = 18,
      "Mere end 20 år" = 8
    )
  ) +
  labs(
    title = "Udseende efter antal år i lokalområdet",
    x = "Sandsynlighed for at boligprojektet foretrækkes (marginal means)",
    y = NULL,
    color = NULL,
    shape = NULL
  ) +
  theme(
    text = element_text(family = "Times New Roman"),
    axis.text = element_text(size = 11),
    axis.title = element_text(size = 12),
    plot.title = element_text(size = 16, face = "bold"),
    legend.text = element_text(size = 10),
    legend.position = "bottom",
    panel.background = element_rect(fill = "grey92", colour = NA),
    plot.background = element_rect(fill = "white", colour = NA),
    panel.grid.major = element_line(colour = "white", linewidth = 0.4),
    panel.grid.minor = element_blank()
  )

plot_h4_år_udseende_mm

ggsave("plot_h4_år_udseende_mm.png", plot = plot_h4_år_udseende_mm, width = 9, height = 6)

# Omnibus test

omnibus_h4_år_udseende <- cj_anova(
  data = conjoint_full %>% filter(!is.na(år_i_lokalområdet)),
  formula = chosen ~ udseende,
  id = ~ participant_id,
  by = ~ år_i_lokalområdet
)

stargazer(
  omnibus_h4_år_udseende,
  type = "html",
  summary = FALSE,
  rownames = FALSE,
  title = "H4 omnibus-test, år i lokalområdet x udseende",
  out = "h4_omnibus_år_udseende.html"
)

# Samler H4 MM plots i en figur

plotdata_h4_mm <- bind_rows(
  mm_h4_hojde %>%
    mutate(
      feature = "Højde",
      plot_level = factor(level, levels = c("8 etager", "5 etager", "3 etager"))
    ),
  mm_h4_udseende %>%
    mutate(
      feature = "Udseende",
      plot_level = factor(level, levels = c("Passer dårligt", "Passer nogenlunde", "Passer godt"))
    )
) %>%
  mutate(
    feature = factor(feature, levels = c("Højde", "Udseende"))
  )

plot_h4_mm_samlet <- plotdata_h4_mm %>%
  ggplot(aes(x = estimate, y = plot_level)) +
  geom_vline(
    xintercept = 0.5,
    linetype = "longdash",
    linewidth = 0.7,
    color = "black"
  ) +
  geom_errorbarh(
    aes(xmin = lower, xmax = upper),
    height = 0.15,
    linewidth = 0.8
  ) +
  geom_point(size = 2.8) +
  facet_grid(
    feature ~ .,
    scales = "free_y",
    space = "free_y"
  ) +
  scale_x_continuous(
    breaks = c(0.3, 0.4, 0.5, 0.6)
  ) +
  labs(
    title = "Boligprojektets påvirkning af de eksisterende rammer",
    x = "Sandsynlighed for at boligprojektet foretrækkes (marginal means)",
    y = NULL
  ) +
  theme(
    text = element_text(family = "Times New Roman"),
    axis.text = element_text(size = 11),
    axis.title = element_text(size = 12),
    strip.text = element_text(size = 12),
    plot.title = element_text(size = 16, face = "bold"),
    panel.background = element_rect(fill = "grey92", colour = NA),
    plot.background = element_rect(fill = "white", colour = NA),
    panel.grid.major = element_line(colour = "white", linewidth = 0.4),
    panel.grid.minor = element_blank()
  )

plot_h4_mm_samlet
ggsave("plot_h4_mm_samlet.png", plot = plot_h4_mm_samlet, width = 8, height = 6)

# Samler H4 amce i et plot

plotdata_h4_amce <- bind_rows(
  amce_h4_hojde %>%
    filter(!is.na(estimate)) %>%
    mutate(
      feature = "Højde",
      plot_level = factor(level, levels = c("8 etager", "5 etager", "3 etager"))
    ),
  amce_h4_udseende %>%
    filter(!is.na(estimate)) %>%
    mutate(
      feature = "Udseende",
      plot_level = factor(level, levels = c("Passer dårligt", "Passer nogenlunde", "Passer godt"))
    )
) %>%
  mutate(
    feature = factor(feature, levels = c("Højde", "Udseende"))
  )

plot_h4_amce_samlet <- plotdata_h4_amce %>%
  ggplot(aes(x = estimate, y = plot_level)) +
  geom_vline(
    xintercept = 0,
    linetype = "longdash",
    linewidth = 0.7,
    color = "black"
  ) +
  geom_errorbarh(
    aes(xmin = lower, xmax = upper),
    height = 0.15,
    linewidth = 0.8
  ) +
  geom_point(size = 2.8) +
  facet_grid(
    feature ~ .,
    scales = "free_y",
    space = "free_y"
  ) +
  labs(
    title = "Boligprojektets påvirkning af de eksisterende rammer",
    x = "Ændring i sandsynligheden for at boligprojektet foretrækkes (AMCE)",
    y = NULL
  ) +
  theme(
    text = element_text(family = "Times New Roman"),
    axis.text = element_text(size = 11),
    axis.title = element_text(size = 12),
    strip.text = element_text(size = 12),
    plot.title = element_text(size = 16, face = "bold"),
    panel.background = element_rect(fill = "grey92", colour = NA),
    plot.background = element_rect(fill = "white", colour = NA),
    panel.grid.major = element_line(colour = "white", linewidth = 0.4),
    panel.grid.minor = element_blank()
  )

plot_h4_amce_samlet
ggsave("plot_h4_amce_samlet.png", plot = plot_h4_amce_samlet, width = 8, height = 6)

# Samler MM for stedstilknytning i en figur

plotdata_h4_tilknytning_mm <- bind_rows(
  mm_h4_hojde_tilknytning %>%
    mutate(
      feature = "Højde",
      gruppe = factor(BY, levels = c("Lav", "Mellem", "Høj")),
      plot_level = factor(level, levels = c("8 etager", "5 etager", "3 etager"))
    ),
  mm_h4_udseende_tilknytning %>%
    mutate(
      feature = "Udseende",
      gruppe = factor(BY, levels = c("Lav", "Mellem", "Høj")),
      plot_level = factor(level, levels = c("Passer dårligt", "Passer nogenlunde", "Passer godt"))
    )
) %>%
  mutate(
    feature = factor(feature, levels = c("Højde", "Udseende"))
  )

plot_h4_tilknytning_mm_samlet <- plotdata_h4_tilknytning_mm %>%
  ggplot(aes(x = estimate, y = plot_level, color = gruppe, shape = gruppe)) +
  geom_vline(
    xintercept = 0.5,
    linetype = "longdash",
    linewidth = 0.7,
    color = "black"
  ) +
  geom_errorbarh(
    aes(xmin = lower, xmax = upper),
    height = 0.15,
    linewidth = 0.8,
    position = position_dodge(width = 0.5)
  ) +
  geom_point(
    size = 2.8,
    position = position_dodge(width = 0.5)
  ) +
  facet_grid(
    feature ~ .,
    scales = "free_y",
    space = "free_y"
  ) +
  scale_color_manual(
    values = c("Lav" = "black", "Mellem" = "grey40", "Høj" = "grey70")
  ) +
  scale_shape_manual(
    values = c("Lav" = 16, "Mellem" = 17, "Høj" = 15)
  ) +
  labs(
    title = "Boligprojektets påvirkning af de eksisterende rammer 
efter stedstilknytning",
    x = "Sandsynlighed for at boligprojektet foretrækkes (marginal means)",
    y = NULL,
    color = NULL,
    shape = NULL
  ) +
  theme(
    text = element_text(family = "Times New Roman"),
    axis.text = element_text(size = 11),
    axis.title = element_text(size = 12),
    strip.text = element_text(size = 12),
    plot.title = element_text(size = 16, face = "bold"),
    legend.text = element_text(size = 11),
    legend.position = "bottom",
    panel.background = element_rect(fill = "grey92", colour = NA),
    plot.background = element_rect(fill = "white", colour = NA),
    panel.grid.major = element_line(colour = "white", linewidth = 0.4),
    panel.grid.minor = element_blank()
  )

plot_h4_tilknytning_mm_samlet

ggsave( "plot_h4_tilknytning_mm_samlet.png", plot = plot_h4_tilknytning_mm_samlet,  width = 8,  height = 8)

#### Del 8 - robusthedstest ####

# Randomisering

# Afstand:
rand_afstand <- conjoint_full %>%
  filter(!is.na(afstand)) %>%
  count(afstand) %>%
  mutate(
    observeret_andel = n / sum(n),
    forventet_andel = 1/3,
    afvigelse_pctpoint = (observeret_andel - forventet_andel) * 100
  )

rand_afstand

chisq.test(rand_afstand$n, p = c(1/3, 1/3, 1/3)) # p = 0.9948

# Højde:
rand_hojde <- conjoint_full %>%
  filter(!is.na(hojde)) %>%
  count(hojde) %>%
  mutate(
    observeret_andel = n / sum(n),
    forventet_andel = 1/3,
    afvigelse_pctpoint = (observeret_andel - forventet_andel) * 100
  )

rand_hojde

chisq.test(rand_hojde$n, p = c(1/3, 1/3, 1/3)) # p = 1

# Udseende: 
rand_udseende <- conjoint_full %>%
  filter(!is.na(udseende)) %>%
  count(udseende) %>%
  mutate(
    observeret_andel = n / sum(n),
    forventet_andel = 1/3,
    afvigelse_pctpoint = (observeret_andel - forventet_andel) * 100
  )

rand_udseende

chisq.test(rand_udseende$n, p = c(1/3, 1/3, 1/3)) # p = 0.9994

# almennyttige boliger
rand_almene <- conjoint_full %>%
  filter(!is.na(andel_almene_boliger)) %>%
  count(andel_almene_boliger) %>%
  mutate(
    observeret_andel = n / sum(n),
    forventet_andel = 1/4,
    afvigelse_pctpoint = (observeret_andel - forventet_andel) * 100
  )

rand_almene

chisq.test(rand_almene$n, p = c(1/4, 1/4, 1/4, 1/4)) # p = 0.9999


randomisering_tabel <- bind_rows(
  rand_afstand %>% mutate(Attribut = "Afstand", Niveau = afstand) %>% select(Attribut, Niveau, n, observeret_andel, forventet_andel, afvigelse_pctpoint),
  rand_hojde %>% mutate(Attribut = "Højde", Niveau = hojde) %>% select(Attribut, Niveau, n, observeret_andel, forventet_andel, afvigelse_pctpoint),
  rand_udseende %>% mutate(Attribut = "Udseende", Niveau = udseende) %>% select(Attribut, Niveau, n, observeret_andel, forventet_andel, afvigelse_pctpoint),
  rand_almene %>% mutate(Attribut = "Andel almennyttige boliger", Niveau = andel_almene_boliger) %>% select(Attribut, Niveau, n, observeret_andel, forventet_andel, afvigelse_pctpoint)
) %>%
  mutate(
    observeret_andel = round(observeret_andel * 100, 1),
    forventet_andel = round(forventet_andel * 100, 1),
    afvigelse_pctpoint = round(afvigelse_pctpoint, 1)
  )

randomisering_tabel


stargazer(
  randomisering_tabel,
  type = "html",
  summary = FALSE,
  rownames = FALSE,
  title = "Randomisering tabel",
  out = "randomisering_tabel.html"
)

# Visualisering

rand_plot_data <- bind_rows(
  conjoint_full %>%
    filter(!is.na(afstand)) %>%
    count(level = afstand) %>%
    mutate(
      attribut = "Afstand",
      expected = 100/3
    ),
  
  conjoint_full %>%
    filter(!is.na(hojde)) %>%
    count(level = hojde) %>%
    mutate(
      attribut = "Højde",
      expected = 100/3
    ),
  
  conjoint_full %>%
    filter(!is.na(udseende)) %>%
    count(level = udseende) %>%
    mutate(
      attribut = "Udseende",
      expected = 100/3
    ),
  
  conjoint_full %>%
    filter(!is.na(andel_almene_boliger)) %>%
    count(level = andel_almene_boliger) %>%
    mutate(
      attribut = "Andel almennyttige boliger",
      expected = 25
    )
) %>%
  group_by(attribut) %>%
  mutate(
    pct = n / sum(n) * 100
  ) %>%
  ungroup()

rand_plot_data <- rand_plot_data %>%
  mutate(
    level = case_when(
      attribut == "Afstand" ~ factor(level, levels = c("2 km", "1 km", "300 m")),
      attribut == "Højde" ~ factor(level, levels = c("8 etager", "5 etager", "3 etager")),
      attribut == "Udseende" ~ factor(level, levels = c("Passer godt", "Passer nogenlunde", "Passer dårligt")),
      attribut == "Andel almennyttige boliger" ~ factor(level, levels = c("100 %", "50 %", "25 %", "0 %")),
      TRUE ~ factor(level)
    )
  )

ref_lines <- rand_plot_data %>%
  distinct(attribut, expected)

plot_randomisering <- ggplot(rand_plot_data, aes(x = pct, y = level)) +
  geom_vline(
    data = ref_lines,
    aes(xintercept = expected),
    linetype = "longdash",
    linewidth = 0.6,
    colour = "black",
    inherit.aes = FALSE
  ) +
  geom_col(fill = "grey20", width = 0.5) +
  geom_text(
    aes(label = round(pct, 1)),
    hjust = -0.15,
    size = 4
  ) +
  facet_grid(attribut ~ ., scales = "free_y") +
  scale_x_continuous(
    limits = c(0, 100),
    breaks = c(0, 25, 50, 75, 100)
  ) +
  labs(
    title = "Fordeling af attributniveauer i conjoint-eksperimentet",
    x = "Procent af profiler",
    y = NULL
  ) +
  theme_minimal(base_family = "Times New Roman") +
  theme(
    plot.title = element_text(size = 18),
    axis.text = element_text(size = 11),
    axis.title = element_text(size = 13),
    strip.text.y = element_text(size = 12, angle = -90),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.spacing = unit(0.8, "lines"),
    strip.background = element_rect(fill = "grey80", colour = "grey40"),
    panel.background = element_rect(fill = "grey92", colour = "grey40"),
    plot.background = element_rect(fill = "white", colour = NA)
  )

plot_randomisering

ggsave("randomisering.png", plot = plot_randomisering, height = 8, width = 8)

# Balancetest på køn, alder og uddannelse

# loader pakker
library(stringr)
library(fixest)

balance_data <- conjoint_full %>%
  mutate(
    kvinde = if_else(køn == "Kvinde", 1, 0),
    alder_mean = as.numeric(alder),
    videregaaende = if_else(
      str_detect(str_to_lower(uddannelse), "videregående|bachelor|kandidat"),
      1, 0
    )
  ) %>%
  select(
    participant_id,
    kvinde,
    alder_mean,
    videregaaende,
    afstand,
    hojde,
    udseende,
    andel_almene_boliger
  ) %>%
  pivot_longer(
    cols = c(afstand, hojde, udseende, andel_almene_boliger),
    names_to = "attribut",
    values_to = "niveau"
  ) %>%
  mutate(
    attribut = recode(
      attribut,
      "afstand" = "Afstand",
      "hojde" = "Højde",
      "udseende" = "Udseende",
      "andel_almene_boliger" = "Andel almennyttige boliger"
    ),
    niveau = factor(
      niveau,
      levels = c(
        "300 m", "1 km", "2 km",
        "3 etager", "5 etager", "8 etager",
        "Passer godt", "Passer nogenlunde", "Passer dårligt",
        "0 %", "25 %", "50 %", "100 %"
      )
    )
  ) %>%
  pivot_longer(
    cols = c(kvinde, alder_mean, videregaaende),
    names_to = "udfald",
    values_to = "y"
  ) %>%
  mutate(
    udfald = recode(
      udfald,
      "kvinde" = "Marginal mean (andel kvinder)",
      "alder_mean" = "Marginal mean (gennemsnitsalder)",
      "videregaaende" = "Marginal mean (videregående uddannelse)"
    )
  )

# Samlede gennemsnit til reference-linjer
overall_means <- balance_data %>%
  group_by(udfald) %>%
  summarise(
    overall = mean(y, na.rm = TRUE),
    .groups = "drop"
  )

# Estimater og klyngerobuste konfidensintervaller
balance_est <- balance_data %>%
  group_by(udfald, attribut) %>%
  group_modify(~{
    mod <- feols(y ~ 0 + niveau, data = .x, vcov = ~ participant_id)
    ci <- confint(mod)
    
    tibble(
      niveau = gsub("^niveau", "", names(coef(mod))),
      estimate = unname(coef(mod)),
      lower = ci[, 1],
      upper = ci[, 2]
    )
  }) %>%
  ungroup() %>%
  left_join(overall_means, by = "udfald")

# Sikrer rigtig rækkefølge
balance_est <- balance_est %>%
  mutate(
    niveau = factor(
      niveau,
      levels = c(
        "300 m", "1 km", "2 km",
        "0 %", "25 %", "50 %", "100 %",
        "3 etager", "5 etager", "8 etager",
        "Passer godt", "Passer nogenlunde", "Passer dårligt"
      )
    ),
    attribut = factor(
      attribut,
      levels = c("Afstand", "Andel almennyttige boliger", "Højde", "Udseende")
    )
  )

# Plot
plot_balancetest <- ggplot(balance_est, aes(x = niveau, y = estimate)) +
  geom_hline(
    aes(yintercept = overall),
    linetype = "longdash",
    linewidth = 0.7,
    color = "black"
  ) +
  geom_errorbar(
    aes(ymin = lower, ymax = upper),
    width = 0.15,
    linewidth = 0.7
  ) +
  geom_point(size = 2.4) +
  facet_grid(
    udfald ~ attribut,
    scales = "free",
    space = "free_x"
  ) +
  labs(
    title = "Balancetest på køn, alder og uddannelsesniveau",
    x = NULL,
    y = NULL
  ) +
  theme(
    text = element_text(family = "Times New Roman"),
    plot.title = element_text(size = 16, face = "bold"),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 11),
    axis.text.y = element_text(size = 11),
    strip.text.x = element_text(size = 12),
    strip.text.y = element_text(size = 12),
    panel.background = element_rect(fill = "grey92", colour = NA),
    plot.background = element_rect(fill = "white", colour = NA),
    panel.grid.major = element_line(colour = "white", linewidth = 0.4),
    panel.grid.minor = element_blank(),
    panel.spacing = unit(0.4, "lines")
  )

plot_balancetest
ggsave("plot_balancetest.png", plot = plot_balancetest, width = 12, height = 14)

# Carryover test

# Inddeler efter opgave nr - 6 opgaver i alt

conjoint_full <- conjoint_full %>%
  mutate(
    opgavenummer = factor(
      paste("Opgave", qes),
      levels = paste("Opgave", 1:6)
    )
  )

table(conjoint_full$opgavenummer)

# Omnibustest af carry over/task order
anova_carryover <- cj_anova(
  data = conjoint_full,
  formula = chosen ~ afstand + hojde + andel_almene_boliger + udseende,
  id = ~ participant_id,
  by = ~ opgavenummer
)

anova_carryover # F = 1,38, p = 0.041

# Laver tabel

anova_carryover_df <- as.data.frame(anova_carryover)

anova_carryover_tab <- anova_carryover_df %>%
  mutate(
    Deviance = round(Deviance, 3),
    F = round(F, 2),
    `Pr(>F)` = ifelse(`Pr(>F)` < 0.001, "<0.001", sprintf("%.3f", `Pr(>F)`))
  ) %>%
  rename(
    `Residual df` = `Resid. Df`,
    `Residual deviance` = `Resid. Dev`,
    `F-statistik` = F,
    `p-værdi` = `Pr(>F)`
  )

anova_carryover_tab 

stargazer(
  anova_carryover_tab,
  type = "html",
  summary = FALSE,
  rownames = FALSE,
  title = "Carryover tabel",
  out = "carryover_tabel.html"
)

# MM for hvert opgave

mm_opgavenummer <- cj(
  data = conjoint_full,
  formula = chosen ~ afstand + hojde + andel_almene_boliger + udseende,
  id = ~ participant_id,
  by = ~ opgavenummer,
  estimate = "mm",
  h0 = 0.5
)

mm_opgavenummer

# Plotter fordeling

plot_opgavenummer <- mm_opgavenummer %>%
  mutate(
    opgavenummer = factor(BY, levels = paste("Opgave", 1:6)),
    level = recode(
      level,
      "1 kilometer" = "1 km",
      "2 kilometer" = "2 km"
    ),
    level = case_when(
      feature == "afstand" ~ factor(level, levels = c("300 m", "1 km", "2 km")),
      feature == "hojde" ~ factor(level, levels = c("3 etager", "5 etager", "8 etager")),
      feature == "andel_almene_boliger" ~ factor(level, levels = c("0 %", "25 %", "50 %", "100 %")),
      feature == "udseende" ~ factor(level, levels = c("Passer godt", "Passer nogenlunde", "Passer dårligt")),
      TRUE ~ factor(level)
    ),
    feature = recode(
      feature,
      "afstand" = "Afstand",
      "hojde" = "Højde",
      "andel_almene_boliger" = "Andel almene boliger",
      "udseende" = "Udseende"
    ),
    feature = factor(feature, levels = c("Afstand", "Højde", "Andel almene boliger", "Udseende"))
  ) %>%
  ggplot(aes(
    x = estimate,
    y = level,
    color = opgavenummer,
    shape = opgavenummer,
    group = opgavenummer
  )) +
  geom_vline(
    xintercept = 0.5,
    linetype = "longdash",
    linewidth = 0.7,
    color = "black"
  ) +
  geom_errorbarh(
    aes(xmin = lower, xmax = upper),
    position = position_dodge(width = 0.6),
    height = 0.15,
    linewidth = 0.7
  ) +
  geom_point(
    position = position_dodge(width = 0.6),
    size = 2.5
  ) +
  facet_grid(
    feature ~ .,
    scales = "free_y",
    space = "free_y"
  ) +
  scale_shape_manual(
    values = c(16, 17, 15, 18, 8, 3)
  ) +
  scale_color_manual(
    values = c(
      "Opgave 1" = "black",
      "Opgave 2" = "grey15",
      "Opgave 3" = "grey30",
      "Opgave 4" = "grey45",
      "Opgave 5" = "grey60",
      "Opgave 6" = "grey70"
    )
  ) +
  labs(
    title = "Carry-over-effekter på tværs af conjoint-opgaver",
    x = "Sandsynlighed for at boligprojektet foretrækkes (marginal means)",
    y = NULL,
    color = NULL,
    shape = NULL
  ) +
  theme(
    text = element_text(family = "Times New Roman"),
    axis.text = element_text(size = 11),
    axis.title = element_text(size = 12),
    strip.text = element_text(size = 12),
    plot.title = element_text(size = 16, face = "bold"),
    legend.text = element_text(size = 10),
    legend.position = "bottom",
    panel.background = element_rect(fill = "grey92", colour = NA),
    plot.background = element_rect(fill = "white", colour = NA),
    panel.grid.major = element_line(colour = "white", linewidth = 0.4),
    panel.grid.minor = element_blank()
  )


plot_opgavenummer

ggsave(
  "plot_opgavenummer.png", plot = plot_opgavenummer, width = 10, height = 12)

# Profile order effekt - højre vs venstre

# Tjek først profilnummer
table(conjoint_full$alt)

# Hvis alt = 1 og 2, og 1 = venstre, 2 = højre:

conjoint_full <- conjoint_full %>%
  mutate(
    alt_profilnummer = factor(
      alt,
      levels = c(1, 2),
      labels = c("Profil til venstre", "Profil til højre")
    )
  )

table(conjoint_full$alt_profilnummer)

# Omnibus test
anova_profilnummer <- cj_anova(
  data = conjoint_full,
  formula = chosen ~ afstand + hojde + andel_almene_boliger + udseende,
  id = ~ participant_id,
  by = ~ alt_profilnummer
)

anova_profilnummer #  F = 0,695, p = 0,730

stargazer(
  anova_carryover_tab,
  type = "html",
  summary = FALSE,
  rownames = FALSE,
  title = "Omnibus-test af profile order-effekter",
  out = "profile_order_anova.html"
)

# Sætter op som MM

mm_profilnummer <- cj(
  data = conjoint_full,
  formula = chosen ~ afstand + hojde + andel_almene_boliger + udseende,
  id = ~ participant_id,
  by = ~ alt_profilnummer,
  estimate = "mm",
  h0 = 0.5
)

mm_profilnummer

# Visualisering

plot_profilnummer <- mm_profilnummer %>%
  mutate(
    feature = recode(
      feature,
      "afstand" = "Afstand",
      "hojde" = "Højde",
      "andel_almene_boliger" = "Andel almennyttige boliger",
      "udseende" = "Udseende"
    ),
    level = recode(
      level,
      "1 kilometer" = "1 km",
      "2 kilometer" = "2 km"
    )
  ) %>%
  ggplot(aes(
    x = estimate,
    y = reorder(level, desc(level)),
    color = alt_profilnummer,
    shape = alt_profilnummer
  )) +
  geom_point(
    position = position_dodge(width = 0.5),
    size = 2.5
  ) +
  geom_errorbarh(
    aes(xmin = lower, xmax = upper),
    position = position_dodge(width = 0.5),
    height = 0.4
  ) +
  geom_vline(
    xintercept = 0.5,
    linetype = "longdash",
    color = "black"
  ) +
  facet_grid(feature ~ ., scales = "free_y", space = "free_y") +
  scale_color_manual(
    values = c("Profil til venstre" = "black",
               "Profil til højre" = "grey50")
  ) +
  scale_shape_manual(
    values = c("Profil til venstre" = 16,
               "Profil til højre" = 17)
  ) +
  labs(
    title = "Profile order-effekter",
    x = "Sandsynlighed for at boligprojektet foretrækkes (marginal means)",
    y = NULL,
    color = NULL,
    shape = NULL
  ) +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    text = element_text(family = "Times New Roman"),
    axis.text = element_text(size = 11),
    axis.title = element_text(size = 12),
    plot.title = element_text(size = 16, face = "bold"),
    strip.text = element_text(size = 12),
    panel.background = element_rect(fill = "grey92", colour = NA),
    plot.background = element_rect(fill = "white", colour = NA),
    panel.grid.major = element_line(colour = "white", linewidth = 0.4),
    panel.grid.minor = element_blank()
  )

plot_profilnummer

ggsave(filename = "plot_profilnummer.png", plot = plot_profilnummer, width = 8, height = 9)

