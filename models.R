################################################################################
## Primary Analysis
################################################################################

data <- pfts %>%
  filter (fvc_z_score >= -1.645 & fev1_fvc_z_score >= -1.645)

model_cough <- get_model (data, "cough")
model_dyspnea <- get_model (data, "dyspnea")
model_wheeze <- get_model (data, "wheeze")
model_asthma <- get_model (data, "asthma")
model_bronchiectasis <- get_model (data, "bronchiectasis")
model_chest_wall <- get_model (data, "chest_wall")
model_copd <- get_model (data, "copd")
model_ild <- get_model (data, "ild")
model_neuromuscular <- get_model (data, "neuromuscular")
model_emphysema <- get_model (data, "emphysema")
model_honeycombing <- get_model (data, "honeycombing")
model_reticulation <- get_model (data, "reticulation")
model_thickening <- get_model (data, "thickening")
model_traction <- get_model (data, "traction")

model_ed <- get_model_ph (data, "ed")
model_death <- get_model_ph (data, "death")

################################################################################
## Subgroups
################################################################################

#### Sex ####

data <- pfts %>%
  filter (fvc_z_score >= -1.645 & fev1_fvc_z_score >= -1.645) %>%
  filter (sex == 1)

model_cough_sex_1 <- get_model (data, "cough")
model_dyspnea_sex_1 <- get_model (data, "dyspnea")
model_wheeze_sex_1 <- get_model (data, "wheeze")
model_asthma_sex_1 <- get_model (data, "asthma")
model_bronchiectasis_sex_1 <- get_model (data, "bronchiectasis")
model_chest_wall_sex_1 <- get_model (data, "chest_wall")
model_copd_sex_1 <- get_model (data, "copd")
model_ild_sex_1 <- get_model (data, "ild")
model_neuromuscular_sex_1 <- get_model (data, "neuromuscular")
model_emphysema_sex_1 <- get_model (data, "emphysema")
model_honeycombing_sex_1 <- get_model (data, "honeycombing")
model_reticulation_sex_1 <- get_model (data, "reticulation")
model_thickening_sex_1 <- get_model (data, "thickening")
model_traction_sex_1 <- get_model (data, "traction")

model_ed_sex_1 <- get_model_ph (data, "ed")
model_death_sex_1 <- get_model_ph (data, "death")

# Sex 2

data <- pfts %>%
  filter (fvc_z_score >= -1.645 & fev1_fvc_z_score >= -1.645) %>%
  filter (sex == 2)

model_cough_sex_2 <- get_model (data, "cough")
model_dyspnea_sex_2 <- get_model (data, "dyspnea")
model_wheeze_sex_2 <- get_model (data, "wheeze")
model_asthma_sex_2 <- get_model (data, "asthma")
model_bronchiectasis_sex_2 <- get_model (data, "bronchiectasis")
model_chest_wall_sex_2 <- get_model (data, "chest_wall")
model_copd_sex_2 <- get_model (data, "copd")
model_ild_sex_2 <- get_model (data, "ild")
model_neuromuscular_sex_2 <- get_model (data, "neuromuscular")
model_emphysema_sex_2 <- get_model (data, "emphysema")
model_honeycombing_sex_2 <- get_model (data, "honeycombing")
model_reticulation_sex_2 <- get_model (data, "reticulation")
model_thickening_sex_2 <- get_model (data, "thickening")
model_traction_sex_2 <- get_model (data, "traction")

model_ed_sex_2 <- get_model_ph (data, "ed")
model_death_sex_2 <- get_model_ph (data, "death")

#### Race ####

# Race 1

data <- pfts %>%
  filter (fvc_z_score >= -1.645 & fev1_fvc_z_score >= -1.645) %>%
  filter (race == 1)

model_cough_race_1 <- get_model (data, "cough")
model_dyspnea_race_1 <- get_model (data, "dyspnea")
model_wheeze_race_1 <- get_model (data, "wheeze")
model_asthma_race_1 <- get_model (data, "asthma")
model_bronchiectasis_race_1 <- get_model (data, "bronchiectasis")
model_chest_wall_race_1 <- get_model (data, "chest_wall")
model_copd_race_1 <- get_model (data, "copd")
model_ild_race_1 <- get_model (data, "ild")
model_neuromuscular_race_1 <- get_model (data, "neuromuscular")
model_emphysema_race_1 <- get_model (data, "emphysema")
model_honeycombing_race_1 <- get_model (data, "honeycombing")
model_reticulation_race_1 <- get_model (data, "reticulation")
model_thickening_race_1 <- get_model (data, "thickening")
model_traction_race_1 <- get_model (data, "traction")

model_ed_race_1 <- get_model_ph (data, "ed")
model_death_race_1 <- get_model_ph (data, "death")

# Race 2

data <- pfts %>%
  filter (fvc_z_score >= -1.645 & fev1_fvc_z_score >= -1.645) %>%
  filter (race == 2)

model_cough_race_2 <- get_model (data, "cough")
model_dyspnea_race_2 <- get_model (data, "dyspnea")
model_wheeze_race_2 <- get_model (data, "wheeze")
model_asthma_race_2 <- get_model (data, "asthma")
model_bronchiectasis_race_2 <- get_model (data, "bronchiectasis")
model_chest_wall_race_2 <- get_model (data, "chest_wall")
model_copd_race_2 <- get_model (data, "copd")
model_ild_race_2 <- get_model (data, "ild")
model_neuromuscular_race_2 <- get_model (data, "neuromuscular")
model_emphysema_race_2 <- get_model (data, "emphysema")
model_honeycombing_race_2 <- get_model (data, "honeycombing")
model_reticulation_race_2 <- get_model (data, "reticulation")
model_thickening_race_2 <- get_model (data, "thickening")
model_traction_race_2 <- get_model (data, "traction")

model_ed_race_2 <- get_model_ph (data, "ed")
model_death_race_2 <- get_model_ph (data, "death")

# Race 3

data <- pfts %>%
  filter (fvc_z_score >= -1.645 & fev1_fvc_z_score >= -1.645) %>%
  filter (race == 3)

model_cough_race_3 <- get_model (data, "cough")
model_dyspnea_race_3 <- get_model (data, "dyspnea")
model_wheeze_race_3 <- get_model (data, "wheeze")
model_asthma_race_3 <- get_model (data, "asthma")
model_bronchiectasis_race_3 <- get_model (data, "bronchiectasis")
model_chest_wall_race_3 <- get_model (data, "chest_wall")
model_copd_race_3 <- get_model (data, "copd")
model_ild_race_3 <- get_model (data, "ild")
model_neuromuscular_race_3 <- get_model (data, "neuromuscular")
model_emphysema_race_3 <- get_model (data, "emphysema")
model_honeycombing_race_3 <- get_model (data, "honeycombing")
model_reticulation_race_3 <- get_model (data, "reticulation")
model_thickening_race_3 <- get_model (data, "thickening")
model_traction_race_3 <- get_model (data, "traction")

model_ed_race_3 <- get_model_ph (data, "ed")
model_death_race_3 <- get_model_ph (data, "death")

# Race 4

data <- pfts %>%
  filter (fvc_z_score >= -1.645 & fev1_fvc_z_score >= -1.645) %>%
  filter (race == 4)

model_cough_race_4 <- get_model (data, "cough")
model_dyspnea_race_4 <- get_model (data, "dyspnea")
model_wheeze_race_4 <- get_model (data, "wheeze")
model_asthma_race_4 <- get_model (data, "asthma")
model_bronchiectasis_race_4 <- get_model (data, "bronchiectasis")
model_chest_wall_race_4 <- get_model (data, "chest_wall")
model_copd_race_4 <- get_model (data, "copd")
model_ild_race_4 <- get_model (data, "ild")
model_neuromuscular_race_4 <- get_model (data, "neuromuscular")
model_emphysema_race_4 <- get_model (data, "emphysema")
model_honeycombing_race_4 <- get_model (data, "honeycombing")
model_reticulation_race_4 <- get_model (data, "reticulation")
model_thickening_race_4 <- get_model (data, "thickening")
model_traction_race_4 <- get_model (data, "traction")

model_ed_race_4 <- get_model_ph (data, "ed")
model_death_race_4 <- get_model_ph (data, "death")

#### Pulmonary Diagnostic Lab ####

# Lab 1

data <- pfts %>%
  filter (fvc_z_score >= -1.645 & fev1_fvc_z_score >= -1.645) %>%
  filter (lab == 1)

model_cough_lab_1 <- get_model (data, "cough")
model_dyspnea_lab_1 <- get_model (data, "dyspnea")
model_wheeze_lab_1 <- get_model (data, "wheeze")
model_asthma_lab_1 <- get_model (data, "asthma")
model_bronchiectasis_lab_1 <- get_model (data, "bronchiectasis")
model_chest_wall_lab_1 <- get_model (data, "chest_wall")
model_copd_lab_1 <- get_model (data, "copd")
model_ild_lab_1 <- get_model (data, "ild")
model_neuromuscular_lab_1 <- get_model (data, "neuromuscular")
model_emphysema_lab_1 <- get_model (data, "emphysema")
model_honeycombing_lab_1 <- get_model (data, "honeycombing")
model_reticulation_lab_1 <- get_model (data, "reticulation")
model_thickening_lab_1 <- get_model (data, "thickening")
model_traction_lab_1 <- get_model (data, "traction")

model_ed_lab_1 <- get_model_ph (data, "ed")
model_death_lab_1 <- get_model_ph (data, "death")

# Lab 2

data <- pfts %>%
  filter (fvc_z_score >= -1.645 & fev1_fvc_z_score >= -1.645) %>%
  filter (lab == 2)

model_cough_lab_2 <- get_model (data, "cough")
model_dyspnea_lab_2 <- get_model (data, "dyspnea")
model_wheeze_lab_2 <- get_model (data, "wheeze")
model_asthma_lab_2 <- get_model (data, "asthma")
model_bronchiectasis_lab_2 <- get_model (data, "bronchiectasis")
model_chest_wall_lab_2 <- get_model (data, "chest_wall")
model_copd_lab_2 <- get_model (data, "copd")
model_ild_lab_2 <- get_model (data, "ild")
model_neuromuscular_lab_2 <- get_model (data, "neuromuscular")
model_emphysema_lab_2 <- get_model (data, "emphysema")
model_honeycombing_lab_2 <- get_model (data, "honeycombing")
model_reticulation_lab_2 <- get_model (data, "reticulation")
model_thickening_lab_2 <- get_model (data, "thickening")
model_traction_lab_2 <- get_model (data, "traction")

model_ed_lab_2 <- get_model_ph (data, "ed")
model_death_lab_2 <- get_model_ph (data, "death")

# Lab 3

data <- pfts %>%
  filter (fvc_z_score >= -1.645 & fev1_fvc_z_score >= -1.645) %>%
  filter (lab == 3)

model_cough_lab_3 <- get_model (data, "cough")
model_dyspnea_lab_3 <- get_model (data, "dyspnea")
model_wheeze_lab_3 <- get_model (data, "wheeze")
model_asthma_lab_3 <- get_model (data, "asthma")
model_bronchiectasis_lab_3 <- get_model (data, "bronchiectasis")
model_chest_wall_lab_3 <- get_model (data, "chest_wall")
model_copd_lab_3 <- get_model (data, "copd")
model_ild_lab_3 <- get_model (data, "ild")
model_neuromuscular_lab_3 <- get_model (data, "neuromuscular")
model_emphysema_lab_3 <- get_model (data, "emphysema")
model_honeycombing_lab_3 <- get_model (data, "honeycombing")
model_reticulation_lab_3 <- get_model (data, "reticulation")
model_thickening_lab_3 <- get_model (data, "thickening")
model_traction_lab_3 <- get_model (data, "traction")

model_ed_lab_3 <- get_model_ph (data, "ed")
model_death_lab_3 <- get_model_ph (data, "death")

# Lab 4

data <- pfts %>%
  filter (fvc_z_score >= -1.645 & fev1_fvc_z_score >= -1.645) %>%
  filter (lab == 4)

#model_cough_lab_4 <- get_model (data, "cough")
#model_dyspnea_lab_4 <- get_model (data, "dyspnea")
#model_wheeze_lab_4 <- get_model (data, "wheeze")
model_asthma_lab_4 <- get_model (data, "asthma")
model_bronchiectasis_lab_4 <- get_model (data, "bronchiectasis")
model_chest_wall_lab_4 <- get_model (data, "chest_wall")
model_copd_lab_4 <- get_model (data, "copd")
model_ild_lab_4 <- get_model (data, "ild")
model_neuromuscular_lab_4 <- get_model (data, "neuromuscular")
model_emphysema_lab_4 <- get_model (data, "emphysema")
model_honeycombing_lab_4 <- get_model (data, "honeycombing")
model_reticulation_lab_4 <- get_model (data, "reticulation")
model_thickening_lab_4 <- get_model (data, "thickening")
model_traction_lab_4 <- get_model (data, "traction")

model_ed_lab_4 <- get_model_ph (data, "ed")
model_death_lab_4 <- get_model_ph (data, "death")

#### Referring Specialty ####

# Pulmonology
data <- pfts %>%
  filter (fvc_z_score >= -1.645 & fev1_fvc_z_score >= -1.645) %>%
  filter (specialty == 1)

model_cough_specialty_1 <- get_model (data, "cough")
model_dyspnea_specialty_1 <- get_model (data, "dyspnea")
model_wheeze_specialty_1 <- get_model (data, "wheeze")
model_asthma_specialty_1 <- get_model (data, "asthma")
model_bronchiectasis_specialty_1 <- get_model (data, "bronchiectasis")
model_chest_wall_specialty_1 <- get_model (data, "chest_wall")
model_copd_specialty_1 <- get_model (data, "copd")
model_ild_specialty_1 <- get_model (data, "ild")
model_neuromuscular_specialty_1 <- get_model (data, "neuromuscular")
model_emphysema_specialty_1 <- get_model (data, "emphysema")
model_honeycombing_specialty_1 <- get_model (data, "honeycombing")
model_reticulation_specialty_1 <- get_model (data, "reticulation")
model_thickening_specialty_1 <- get_model (data, "thickening")
model_traction_specialty_1 <- get_model (data, "traction")

model_ed_specialty_1 <- get_model_ph (data, "ed")
model_death_specialty_1 <- get_model_ph (data, "death")

# Primary Care
data <- pfts %>%
  filter (fvc_z_score >= -1.645 & fev1_fvc_z_score >= -1.645) %>%
  filter (specialty == 2)

model_cough_specialty_2 <- get_model (data, "cough")
model_dyspnea_specialty_2 <- get_model (data, "dyspnea")
model_wheeze_specialty_2 <- get_model (data, "wheeze")
model_asthma_specialty_2 <- get_model (data, "asthma")
model_bronchiectasis_specialty_2 <- get_model (data, "bronchiectasis")
model_chest_wall_specialty_2 <- get_model (data, "chest_wall")
model_copd_specialty_2 <- get_model (data, "copd")
model_ild_specialty_2 <- get_model (data, "ild")
model_neuromuscular_specialty_2 <- get_model (data, "neuromuscular")
model_emphysema_specialty_2 <- get_model (data, "emphysema")
model_honeycombing_specialty_2 <- get_model (data, "honeycombing")
model_reticulation_specialty_2 <- get_model (data, "reticulation")
model_thickening_specialty_2 <- get_model (data, "thickening")
model_traction_specialty_2 <- get_model (data, "traction")

model_ed_specialty_2 <- get_model_ph (data, "ed")
model_death_specialty_2 <- get_model_ph (data, "death")

################################################################################
## Normal Spirometry among Patients with Restriction
################################################################################

data <- pfts %>%
  filter (tlc_z_score < -1.645) %>% 
  mutate (normal_spirometry = case_when (
    fvc_z_score >= -1.645 & fev1_fvc_z_score >= -1.645 ~ 1,
    TRUE ~ 0)
  ) %>% 
  mutate (normal_spirometry = as.factor (normal_spirometry))

model_cough_restriction <- get_model_restriction (data, "cough")
model_dyspnea_restriction <- get_model_restriction (data, "dyspnea")
model_wheeze_restriction <- get_model_restriction (data, "wheeze")
model_asthma_restriction <- get_model_restriction (data, "asthma")
model_bronchiectasis_restriction <- get_model_restriction (data, "bronchiectasis")
model_chest_wall_restriction <- get_model_restriction (data, "chest_wall")
model_copd_restriction <- get_model_restriction (data, "copd")
model_ild_restriction <- get_model_restriction (data, "ild")
model_neuromuscular_restriction <- get_model_restriction (data, "neuromuscular")
model_emphysema_restriction <- get_model_restriction (data, "emphysema")
model_honeycombing_restriction <- get_model_restriction (data, "honeycombing")
model_reticulation_restriction <- get_model_restriction (data, "reticulation")
model_thickening_restriction <- get_model_restriction (data, "thickening")
model_traction_restriction <- get_model_restriction (data, "traction")

model_ed_restriction <- get_model_ph_restriction (data, "ed")
model_death_restriction <- get_model_ph_restriction (data, "death")

################################################################################
## Diagnosis with One or More ICD Codes
################################################################################

data <- pfts %>%
  filter (fvc_z_score >= -1.645 & fev1_fvc_z_score >= -1.645)

model_asthma_icd <- get_model (data, "asthma_icd")
model_bronchiectasis_icd <- get_model (data, "bronchiectasis_icd")
model_chest_wall_icd <- get_model (data, "chest_wall_icd")
model_copd_icd <- get_model (data, "copd_icd")
model_ild_icd <- get_model (data, "ild_icd")
model_neuromuscular_icd <- get_model (data, "neuromuscular_icd")

################################################################################
## With Race-Specific Reference Equations
################################################################################

data <- pfts %>%
  filter (fvc_z_score_2012 >= -1.645 & fev1_fvc_z_score_2012 >= -1.645)

model_cough_specific <- get_model_specific (data, "cough")
model_dyspnea_specific <- get_model_specific (data, "dyspnea")
model_wheeze_specific <- get_model_specific (data, "wheeze")
model_asthma_specific <- get_model_specific (data, "asthma")
model_bronchiectasis_specific <- get_model_specific (data, "bronchiectasis")
model_chest_wall_specific <- get_model_specific (data, "chest_wall")
model_copd_specific <- get_model_specific (data, "copd")
model_ild_specific <- get_model_specific (data, "ild")
model_neuromuscular_specific <- get_model_specific (data, "neuromuscular")
model_emphysema_specific <- get_model_specific (data, "emphysema")
model_honeycombing_specific <- get_model_specific (data, "honeycombing")
model_reticulation_specific <- get_model_specific (data, "reticulation")
model_thickening_specific <- get_model_specific (data, "thickening")
model_traction_specific <- get_model_specific (data, "traction")

model_ed_specific <- get_model_specific_ph (data, "ed")
model_death_specific <- get_model_specific_ph (data, "death")

################################################################################
## With Race-Specific Reference Equations by Race
################################################################################

# White

data <- pfts %>%
  filter (race == 1) %>% 
  filter (fvc_z_score_2012 >= -1.645 & fev1_fvc_z_score_2012 >= -1.645)

model_cough_specific_race_1 <- get_model_specific (data, "cough")
model_dyspnea_specific_race_1 <- get_model_specific (data, "dyspnea")
model_wheeze_specific_race_1 <- get_model_specific (data, "wheeze")
model_asthma_specific_race_1 <- get_model_specific (data, "asthma")
model_bronchiectasis_specific_race_1 <- get_model_specific (data, "bronchiectasis")
model_chest_wall_specific_race_1 <- get_model_specific (data, "chest_wall")
model_copd_specific_race_1 <- get_model_specific (data, "copd")
model_ild_specific_race_1 <- get_model_specific (data, "ild")
model_neuromuscular_specific_race_1 <- get_model_specific (data, "neuromuscular")
model_emphysema_specific_race_1 <- get_model_specific (data, "emphysema")
model_honeycombing_specific_race_1 <- get_model_specific (data, "honeycombing")
model_reticulation_specific_race_1 <- get_model_specific (data, "reticulation")
model_thickening_specific_race_1 <- get_model_specific (data, "thickening")
model_traction_specific_race_1 <- get_model_specific (data, "traction")

model_ed_specific_race_1 <- get_model_specific_ph (data, "ed")
model_death_specific_race_1 <- get_model_specific_ph (data, "death")

# Black

data <- pfts %>%
  filter (race == 2) %>% 
  filter (fvc_z_score_2012 >= -1.645 & fev1_fvc_z_score_2012 >= -1.645)

model_cough_specific_race_2 <- get_model_specific (data, "cough")
model_dyspnea_specific_race_2 <- get_model_specific (data, "dyspnea")
model_wheeze_specific_race_2 <- get_model_specific (data, "wheeze")
model_asthma_specific_race_2 <- get_model_specific (data, "asthma")
model_bronchiectasis_specific_race_2 <- get_model_specific (data, "bronchiectasis")
model_chest_wall_specific_race_2 <- get_model_specific (data, "chest_wall")
model_copd_specific_race_2 <- get_model_specific (data, "copd")
model_ild_specific_race_2 <- get_model_specific (data, "ild")
model_neuromuscular_specific_race_2 <- get_model_specific (data, "neuromuscular")
model_emphysema_specific_race_2 <- get_model_specific (data, "emphysema")
model_honeycombing_specific_race_2 <- get_model_specific (data, "honeycombing")
model_reticulation_specific_race_2 <- get_model_specific (data, "reticulation")
model_thickening_specific_race_2 <- get_model_specific (data, "thickening")
model_traction_specific_race_2 <- get_model_specific (data, "traction")

model_ed_specific_race_2 <- get_model_specific_ph (data, "ed")
model_death_specific_race_2 <- get_model_specific_ph (data, "death")
