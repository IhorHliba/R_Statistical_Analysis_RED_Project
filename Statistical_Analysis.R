# =========================
# Project: RED — socio-dem & support analysis
# =========================

# ---- Repro/Env (optional) ----
# install.packages("renv")
# renv::init()

# ---- Packages ----
suppressPackageStartupMessages({
  library(tidyverse)
  library(readxl)
  library(janitor)      # clean_names()
  library(broom)        # tidy(), glance()
  library(rstatix)      # shapiro_test, levene_test, identify_outliers
  library(effectsize)   # cramers_v, cohens_d, eta_squared, omega_squared
  library(forcats)      # fct_*
  library(here)         # robust paths
  library(ggplot2)
})

# ---- Paths ----
socdem_path  <- here::here("data", "Data_SOC_DEM_RED_3.xlsx")
support_path <- here::here("data", "Data_Rozsah_podpory_dle_specifikace_RED.xlsx")

# ---- Helpers ----

qq_plot <- function(x, title = "QQ-plot") {
  df <- tibble(x = x) %>% drop_na()
  ggplot(df, aes(sample = x)) +
    stat_qq() + stat_qq_line() +
    labs(title = title, x = "???????????????????? ????????????????", y = "???????????????????????????? ????????????????") +
    theme_minimal()
}

pct_bar <- function(df, x, fill, title, ylab = "Share") {
  ggplot(df, aes({{x}}, after_stat(prop), fill = {{fill}})) +
    geom_bar(position = "fill") +
    scale_y_continuous(labels = scales::percent) +
    labs(title = title, x = rlang::as_name(rlang::enexpr(x)), y = ylab, fill = rlang::as_name(rlang::enexpr(fill))) +
    theme_minimal()
}

# ---- Load & clean: socio-dem ----
socdem <- readxl::read_excel(socdem_path) %>% 
  janitor::clean_names()

# splnila_bagatelni_podporu: Yes/No (factor)
# pohlavi (factor)
# vek_pri_vstupu_do_projektu: numeric 

socdem <- socdem %>%
  mutate(
    across(c(splnila_bagatelni_podporu, pohlavi), ~ as.factor(.x)),
    vek_pri_vstupu_do_projektu = suppressWarnings(as.numeric(vek_pri_vstupu_do_projektu))
  )

# ---- 1) ??2: Gender × Support ----

soc_chi <- socdem %>%
  select(splnila_bagatelni_podporu, pohlavi) %>%
  drop_na()

tab_gender_support <- table(
  Support = soc_chi$splnila_bagatelni_podporu,
  Gender  = soc_chi$pohlavi
)

chisq_res <- chisq.test(tab_gender_support, correct = FALSE)
cv        <- effectsize::cramers_v(tab_gender_support)

message("\n--- Chi-square test: Support × Gender ---")
print(chisq_res)
print(cv)

soc_chi %>%
  mutate(
    pohlavi = fct_infreq(pohlavi),
    splnila_bagatelni_podporu = fct_infreq(splnila_bagatelni_podporu)
  ) %>%
  pct_bar(x = pohlavi, fill = splnila_bagatelni_podporu, title = "Support by Gender")

# ---- 2) Age by Support: t-test + cheks + fallback ----

soc_age <- socdem %>%
  select(splnila_bagatelni_podporu, vek_pri_vstupu_do_projektu) %>%
  drop_na()

# Normality per group 
norm_chk <- soc_age %>%
  group_by(splnila_bagatelni_podporu) %>%
  shapiro_test(vek_pri_vstupu_do_projektu)

message("\n--- Shapiro normality by group (age): ---")
print(norm_chk)

# Levene 
lev_chk <- rstatix::levene_test(
  data = soc_age,
  formula = vek_pri_vstupu_do_projektu ~ splnila_bagatelni_podporu
)

message("\n--- Levene test (homogeneity of variances): ---")
print(lev_chk)

# QQ-plot 
print(qq_plot(
  soc_age$vek_pri_vstupu_do_projektu[soc_age$splnila_bagatelni_podporu == levels(soc_age$splnila_bagatelni_podporu)[1]],
  paste0("QQ: age (group = ", levels(soc_age$splnila_bagatelni_podporu)[1], ")")
))
print(qq_plot(
  soc_age$vek_pri_vstupu_do_projektu[soc_age$splnila_bagatelni_podporu == levels(soc_age$splnila_bagatelni_podporu)[2]],
  paste0("QQ: age (group = ", levels(soc_age$splnila_bagatelni_podporu)[2], ")")
))

# Outliers scan 
outl <- soc_age %>%
  group_by(splnila_bagatelni_podporu) %>%
  identify_outliers(vek_pri_vstupu_do_projektu)
message("\n--- Potential outliers (age by support) ---")
print(outl)

equal_var <- lev_chk$p > 0.05
normalish <- all(norm_chk$p > 0.05)  

if (normalish) {
  t_res <- t.test(vek_pri_vstupu_do_projektu ~ splnila_bagatelni_podporu,
                  data = soc_age,
                  var.equal = equal_var)  # Welch ??????????????????????, ???????? FALSE
  d_res <- effectsize::cohens_d(vek_pri_vstupu_do_projektu ~ splnila_bagatelni_podporu,
                                data = soc_age,
                                pooled_sd = equal_var)
  message("\n--- t-test (age by support) ---")
  print(t_res)
  print(d_res)
} else {
  w_res <- wilcox.test(vek_pri_vstupu_do_projektu ~ splnila_bagatelni_podporu,
                       data = soc_age,
                       exact = FALSE)
  message("\n--- Wilcoxon rank-sum (age by support) ---")
  print(w_res)
}

# Boxplot
ggplot(soc_age,
       aes(splnila_bagatelni_podporu, vek_pri_vstupu_do_projektu,
           fill = splnila_bagatelni_podporu)) +
  geom_boxplot(alpha = 0.7, width = 0.6, outlier.alpha = 0.4) +
  labs(title = "Age by Support Status", x = "Support", y = "Age at Project Entry") +
  theme_minimal() +
  theme(legend.position = "none")

# ---- Load & clean: “Rozsah podpory dle specifikace” ----
support <- readxl::read_excel(support_path) %>%
  janitor::clean_names() %>%
  mutate(
    cilove_skupiny = as.factor(cilove_skupiny),
    rozsah_podpory_celkem = suppressWarnings(as.numeric(rozsah_podpory_celkem))
  ) %>%
  drop_na(cilove_skupiny, rozsah_podpory_celkem)

# ---- 3) ANOVA or Welch----

shp_sup <- shapiro_test(support$rozsah_podpory_celkem)
lev_sup <- rstatix::levene_test(rozsah_podpory_celkem ~ cilove_skupiny, data = support)

message("\n--- Shapiro (support total) ---")
print(shp_sup)
message("\n--- Levene (support total ~ group) ---")
print(lev_sup)

# 
if (lev_sup$p > 0.05) {
  fit_aov <- aov(rozsah_podpory_celkem ~ cilove_skupiny, data = support)
  message("\n--- Classical ANOVA ---")
  print(summary(fit_aov))
  
  # 
  eta  <- effectsize::eta_squared(fit_aov, partial = FALSE, ci = 0.95)
  omega <- effectsize::omega_squared(fit_aov, ci = 0.95)
  message("\n--- Effect sizes (ANOVA) ---")
  print(eta); print(omega)
  
  # Tukey post-hoc
  message("\n--- Tukey HSD ---")
  print(broom::tidy(TukeyHSD(fit_aov)))
  
} else {
  welch <- oneway.test(rozsah_podpory_celkem ~ cilove_skupiny, data = support)
  message("\n--- Welch ANOVA (heterogeneous variances) ---")
  print(welch)
  message("Note: ?????? post-hoc ?????? ???????????????? ???????????????????? ???????????????????????????? Games–Howell (???????? ?????????? R).")
  # print(rstatix::games_howell_test(support, rozsah_podpory_celkem ~ cilove_skupiny))
}

# Boxplot
support %>%
  mutate(cilove_skupiny = fct_reorder(cilove_skupiny, rozsah_podpory_celkem, .fun = median, .desc = TRUE)) %>%
  ggplot(aes(cilove_skupiny, rozsah_podpory_celkem, fill = cilove_skupiny)) +
  geom_boxplot(alpha = 0.7, outlier.alpha = 0.4) +
  coord_flip() +
  labs(title = "Total Support by Target Group", x = "Target Group (sorted by median)", y = "Total Support") +
  theme_minimal() +
  theme(legend.position = "none")

# ---- Compact reporting tables (optional) ----
# library(gt)
# broom::tidy(chisq_res) %>% gt::gt()
# broom::tidy(fit_aov)   %>% gt::gt()
