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
    `Coronary artery disease` = cad,
    `Previous myocardial infarction ` = mi_prior,
    `Arterial hypertension` = hypert,
    # Peripheral vascular disease
    # History of stroke
    `Current smoker` = smoker,
    # Chronic pulmonary disease`
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
