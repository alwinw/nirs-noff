source("_targets.R")

new.packages <-
  packages[!(packages %in% utils::installed.packages()[, "Package"])]

if (length(new.packages)) {
  install.packages(new.packages)
}
lapply(packages, library, character.only = TRUE)

csv_paths <- list.files("data", "[0-9]{1,2}_*.csv", full.names = TRUE)


orthogeri <- read_dta("data/NIRS_orthogeri_foranalysis.dta")
orthogeri_csv <- read.csv("data/NIRS_orthogeri_foranalysis.csv")


all.equal(orthogeri_csv, orthogeri, check.attributes = FALSE)

length(unique(orthogeri$id))

library(gtsummary)

demographics <- orthogeri_csv |>
  filter(idtag == 1) |>
  mutate(
    group = factor(
      group,
      levels = c("Orthogeriatric", "Minor surgery"),
      labels = c("Orthogeriatric Patients", "Control"),
      ordered = TRUE
    )
  )

# Issues
# orthogeri$fio2 has two entry errors, one is 43 and one is 47
# orthogeri$t one has day = 8 not day 8

# Table 1
demographics |>
  pivot_longer(
    cols = c(elective),
    names_to = "admission"
  ) |>
  select(
    group,
    Age = age,
    `Male gender` = male,
    `Right handed` = righthanded,
    `Height, cm` = height,
    `Weight, kg` = weight,
    Ethnicity = ethnic,
    `ASA class` = asa,
    `Admission type` = admission,
    # Preoperative LVEF, %
    # ef 20-70%
    `Coronary artery disease` = cad,
    `Previous myocardial infarction ` = mi_prior,
    `Arterial hypertension` = hypert,
    # Peripheral vascular disease
    # arteriopathy
    # History of stroke
    # stroke
    `Current smoker` = smoker,
    # Chronic pulmonary disease`
    # copd
    `Diabetes` = diab,
    `Obstructive sleep apnea` = osas,
    `History of neurosurgery` = neurosurg,
  ) |>
  tbl_summary(by = group) |>
  add_p()

# Table 2
demographics |>
  mutate(
    type_surg = case_when(
      # double check for overlaps
      abdominal_surg == 1 ~ "Abdominal surgery",
      ortho_surg == 1 ~ "Orthopedic surgery",
      TRUE ~ NA_character_
    ),
    type_anaest = case_when(
      general_anest == 1 ~ "General anesthesia",
      regional_anaest == 1 ~ "Regional anesthesia",
      TRUE ~ NA_character_
    ),
    premed_none = ifelse(premed == 1, 0, 1)
  ) |>
  select(
    group,
    `Type of surgery` = type_surg,
    `Duration of surgery, hours` = optime,
    `Type of anesthesia` = type_anaest,
    `Premed Benzodiazepines` = premed_benzo,
    `Premed Opioids` = premed_opioid,
    `Premed none` = premed_none,
    Propofol = propofol_op,
    Midazolam = midaz_op,
    Opioids = opioid_op,
    Sevoflurane = sevo_op,
    Isoflurane = iso_op,
    Vasopressors = vasopressor_op,
    Inotropes = inotropes_op,
    `Glyceryl trinitrate` = gtn_op, # Double check this name gtn_op is correct
  ) |>
  tbl_summary(by = group) |>
  add_p()

# Table 3
# ae_ = adverse effects
demographics |>
  select(
    group,
    # RBC transfusion
    # prbc_ever
    `ICU admission` = ae_icuadm,
    `Hospital length of stay` = hlos,
    `Hospital mortality` = hospmort,
    `Return to operating room` = ae_return_or,
    `Stroke` = ae_stroke,
    `Myocardial infarction` = ae_mi,
    `Delirium` = ae_delir,
    `Pulmonary embolism` = ae_pe,
  ) |>
  tbl_summary(by = group) |>
  add_p()


# Table 4
# Hemoglobin = Hb
# Supplemental oxygen (per 1 L/min increase) <- fio2? but in wrong units
# all.equal(orthogeri_csv$mbinirs, (orthogeri_csv$mlnirs + orthogeri_csv$mrnirs)/2)
#> TRUE
# all.equal(orthogeri_csv$mbinirs_discharge[orthogeri_csv$last_nirs == 1], orthogeri_csv$mbinirs[orthogeri_csv$last_nirs == 1])
#> TRUE
# How is cardio data used?

# See also: https://datascienceplus.com/r-for-publication-by-page-piccinini-lesson-6-part-2-linear-mixed-effects-models-lmem/
# https://stats.stackexchange.com/questions/468436/p-values-from-lmer-with-lmertest-why-reml-true

library(lme4)
library(lmerTest)

lmer_df <- orthogeri_csv |>
  mutate(
    age_10 = age / 10,
    height_10 = height / 10,
  ) |>
  mutate(
    time = parse_number(t)
  ) |>
  filter(!is.na(time))

model <- lmer(
  mbinirs ~ group + time + male + age_10 + height_10 + mi_prior + arteriopathy + fio2 + hb + prbc_ever + (1 | id),
  lmer_df
)

summary(model)
# anova(model)

test <- glm(
  mbinirs ~ group + time + male + age_10 + height_10 + mi_prior + arteriopathy + fio2 + hb + prbc_ever,
  data = lmer_df
)
summary(test)

theme_set(theme_bw())

plot_df <- orthogeri_csv |>
  select(group, id, t, mbinirs, mbinirs_discharge) |>
  mutate(t = ifelse(t == "8", "day 8", t)) |>
  pivot_wider(
    names_from = t,
    values_from = mbinirs
  ) |>
  rename(discharge = mbinirs_discharge) |>
  pivot_longer(
    cols = c(baseline:`day 1`, discharge),
    names_to = "time",
    values_to = "mbinirs"
  ) |>
  mutate(time = factor(
    time,
    levels = c("baseline", "intubated", "postop", paste("day", 1:8), "discharge"),
    ordered = TRUE
  )) |>
  mutate(
    group = factor(
      group,
      levels = c("Orthogeriatric", "Minor surgery"),
      labels = c("Orthogeriatric Patients", "Control"),
      ordered = TRUE
    )
  ) |>
  filter(!is.na(mbinirs))

#> plot_df |> pivot_wider(
#>   names_from = time,
#>   values_from = mbinirs
#> ) |> filter(is.na(`day 2`), is.na(`day 3`))

frequencyAnnotation <- function(x) {
  c(y = (quantile(x, .75, names = F) + median(x)) / 2, label = length(x))
}

ggplot(plot_df, aes(x = time, y = mbinirs, fill = time)) +
  stat_boxplot(geom = "errorbar", width = 0.2) +
  geom_boxplot() +
  stat_summary(
    fun.data = frequencyAnnotation, geom = "text",
    position = position_dodge(width = 0.75)
  ) +
  facet_wrap(~group) +
  scale_fill_manual(values = colorRampPalette(brewer.pal("BuPu", n = 4))(12)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
