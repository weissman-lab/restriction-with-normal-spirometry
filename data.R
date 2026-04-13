################################################################################
## Get Breeze Data
################################################################################

pfts_breeze <- rbind (
  read_csv ("../data/breeze_2012.csv"),
  read_csv ("../data/breeze_2013.csv"),
  read_csv ("../data/breeze_2014.csv"),
  read_csv ("../data/breeze_2015.csv"),
  read_csv ("../data/breeze_2016.csv"),
  read_csv ("../data/breeze_2017.csv"),
  read_csv ("../data/breeze_2018.csv"),
  read_csv ("../data/breeze_2019.csv"),
  read_csv ("../data/breeze_2020.csv"),
  read_csv ("../data/breeze_2021.csv"),
  read_csv ("../data/breeze_2022.csv"),
  read_csv ("../data/breeze_2023.csv")
)

pfts_breeze <- pfts_breeze %>% 
  select (
    mrn,
    last_name,
    first_name,
    middle_name,
    dob,
    date,
    lab = site,
    age,
    sex,
    height,
    weight,
    pack_years,
    cough,
    dyspnea,
    wheeze,
    diagnosis,
    fev1,
    fvc,
    fev1_post,
    fvc_post,
    tlc,
    rv,
    dlco,
    kco,
    va,
    comments
  ) %>%
  mutate (pack_years = case_when (
    pack_years > 0 ~ pack_years,
    TRUE ~ 0)
  ) %>%
  mutate (height = height * 2.54) %>% # convert inches to centimeters
  mutate (weight = weight * 0.453592) %>% # convert pounds to kilograms  
  mutate (source = "breeze")

################################################################################
## Get Vyaire Data
################################################################################

pfts_reference_pre_2020 <- read_excel ("../data/pfts_reference_pre_2020.xlsx")
pfts_reference_2020 <- read_excel ("../data/pfts_reference_2020.xlsx")
pfts_reference_2021_part_1 <- read_excel ("../data/pfts_reference_2021_part_1.xlsx")
pfts_reference_2021_part_2 <- read_excel ("../data/pfts_reference_2021_part_2.xlsx")
pfts_reference_2022_part_1 <- read_excel ("../data/pfts_reference_2022_part_1.xlsx")
pfts_reference_2022_part_2 <- read_excel ("../data/pfts_reference_2022_part_2.xlsx")
pfts_reference_2023_part_1 <- read_excel ("../data/pfts_reference_2023_part_1.xlsx")
pfts_reference_2023_part_2 <- read_excel ("../data/pfts_reference_2023_part_2.xlsx")
pfts_reference_2024_part_1 <- read_excel ("../data/pfts_reference_2024_part_1.xlsx")
pfts_reference_2024_part_2 <- read_excel ("../data/pfts_reference_2024_part_2.xlsx")
pfts_reference_2025_part_1 <- read_excel ("../data/pfts_reference_2025_part_1.xlsx")
pfts_reference_2025_part_2 <- read_excel ("../data/pfts_reference_2025_part_2.xlsx")

pfts_reference <- rbind (
  pfts_reference_pre_2020,
  pfts_reference_2020,
  pfts_reference_2021_part_1,
  pfts_reference_2021_part_2,  
  pfts_reference_2022_part_1,
  pfts_reference_2022_part_2,  
  pfts_reference_2023_part_1,
  pfts_reference_2023_part_2,  
  pfts_reference_2024_part_1,
  pfts_reference_2024_part_2,  
  pfts_reference_2025_part_1,
  pfts_reference_2025_part_2  
)

rm (pfts_reference_pre_2020)
rm (pfts_reference_2020)
rm (pfts_reference_2021_part_1)
rm (pfts_reference_2021_part_2)
rm (pfts_reference_2022_part_1)
rm (pfts_reference_2022_part_2)
rm (pfts_reference_2023_part_1)
rm (pfts_reference_2023_part_2)
rm (pfts_reference_2024_part_1)
rm (pfts_reference_2024_part_2)
rm (pfts_reference_2025_part_1)
rm (pfts_reference_2025_part_2)

pfts_reference <- pfts_reference %>% 
  select (
    mrn = `Patient ID`,
    last_name = Name,
    first_name = Firstname,
    middle_name = Middlename,
    dob = Date_Of_Birth,
    date = `Test Date`,
    lab = Location,
    age = Age,
    sex = BiologicalGender,
    height = Height,
    weight = Weight,    
    pre = LevelType,
    fev1_predicted = `FEV 1 PRED`,
    fev1_percent_predicted = `FEV 1 %PRED`,
    fvc_predicted = `VC max PRED`,
    fvc_percent_predicted = `VC max %PRED`,
    fev1_fvc_predicted = `FEV 1 % FVC PRED`,
    fev1_fvc_percent_predicted = `FEV 1 % VC max %PRED`,
    tlc_predicted = `TLC_Pleth PRED`,
    tlc_percent_predicted = `TLC_Pleth %PRED`,
    frc_predicted = `FRC_Pleth PRED`,
    frc_percent_predicted = `FRC_Pleth %PRED`,
    rv_predicted = `RV_Pleth PRED`,
    rv_percent_predicted = `RV_Pleth %PRED`,
    dlco_predicted = `DLCO_SB PRED`,
    dlco_percent_predicted = `DLCO_SB %PRED`,
    kco_predicted = `DLCO_SB / VA PRED`,
    kco_percent_predicted = `DLCO_SB / VA %PRED`,
    va_predicted = `VA PRED`,
    va_percent_predicted = `VA %PRED`
  ) %>% 
  mutate (dob = mdy_hms (dob)) %>% 
  mutate (dob = as_date (dob)) %>%   
  mutate (last_name = str_to_lower (last_name)) %>%
  mutate (first_name = str_to_lower (first_name)) %>% 
  mutate (middle_name = str_to_lower (middle_name)) %>%
  mutate (age = as.integer (age)) %>%
  mutate (sex = case_when (sex == "male" ~ 1, sex == "female" ~ 2)) %>%
  mutate (sex = as.factor (sex)) %>% 
  mutate (height = as.double (height)) %>% 
  mutate (height = height * 100) %>%
  mutate (weight = as.double (weight)) %>%
  mutate (date = mdy_hms (date)) %>%
  mutate (date = as_date (date)) %>%
  mutate (fev1_predicted = as.double (fev1_predicted)) %>% 
  mutate (fev1_percent_predicted = as.double (fev1_percent_predicted)) %>% 
  mutate (fvc_predicted = as.double (fvc_predicted)) %>% 
  mutate (fvc_percent_predicted = as.double (fvc_percent_predicted)) %>% 
  mutate (fev1_fvc_predicted = as.double (fev1_fvc_predicted)) %>% 
  mutate (fev1_fvc_percent_predicted = as.double (fev1_fvc_percent_predicted)) %>% 
  mutate (tlc_predicted = as.double (tlc_predicted)) %>%
  mutate (tlc_percent_predicted = as.double (tlc_percent_predicted)) %>% 
  mutate (rv_predicted = as.double (rv_predicted)) %>%
  mutate (rv_percent_predicted = as.double (rv_percent_predicted)) %>% 
  mutate (dlco_predicted = as.double (dlco_predicted)) %>%
  mutate (dlco_percent_predicted = as.double (dlco_percent_predicted)) %>% 
  mutate (kco_predicted = as.double (kco_predicted)) %>%
  mutate (kco_percent_predicted = as.double (kco_percent_predicted)) %>% 
  mutate (va_predicted = as.double (va_predicted)) %>% 
  mutate (va_percent_predicted = as.double (va_percent_predicted)) %>% 
  mutate (fev1 = fev1_predicted * fev1_percent_predicted / 100) %>% 
  mutate (fvc = fvc_predicted * fvc_percent_predicted / 100) %>% 
  mutate (fev1_fvc = fev1_fvc_predicted * fev1_fvc_percent_predicted / 100) %>% 
  mutate (tlc = tlc_predicted * tlc_percent_predicted / 100) %>% 
  mutate (rv = rv_predicted * rv_percent_predicted / 100) %>% 
  mutate (dlco = dlco_predicted * 2.987 * dlco_percent_predicted / 100) %>% 
  mutate (kco = kco_predicted * 2.987 * kco_percent_predicted / 100) %>% 
  mutate (va = va_predicted * va_percent_predicted / 100) %>%
  mutate (fev1 = round (fev1, digits = 2)) %>% 
  mutate (fvc = round (fvc, digits = 2)) %>%
  mutate (fev1_fvc = round (fev1_fvc, digits = 2)) %>% 
  mutate (tlc = round (tlc, digits = 2)) %>% 
  mutate (rv = round (rv, digits = 2)) %>% 
  mutate (dlco = round (dlco, digits = 2)) %>% 
  mutate (kco = round (kco, digits = 2)) %>%   
  mutate (va = round (va, digits = 2))

# Pre-Bronchodilator Data

pfts_pre <- pfts_reference %>% 
  filter (pre == 1)

# Post-Bronchodilator Data

pfts_post <- pfts_reference %>% 
  filter (pre == 2) %>% 
  select (
    mrn,
    dob,
    last_name,
    first_name,
    middle_name,
    date,
    fev1_post = fev1,
    fvc_post = fvc,
    fev1_fvc_post = fev1_fvc
  )

rm (pfts_reference)

# Combine Pre- and Post-Bronchodilator Data

pfts_vyaire <- pfts_pre %>% 
  left_join (pfts_post, by = c (
    "mrn",
    "last_name",
    "first_name",
    "middle_name",
    "dob",
    "date")
  ) %>%
  mutate (pack_years = NA) %>% 
  mutate (cough = NA) %>% 
  mutate (dyspnea = NA) %>% 
  mutate (wheeze = NA) %>%
  mutate (diagnosis = NA) %>%
  mutate (comments = NA) %>% 
  mutate (source = "vyaire") %>% 
  select (
    mrn,
    last_name,
    first_name,
    middle_name,
    dob,
    date,
    lab,
    age,
    sex,
    height,
    weight,
    pack_years,
    cough,
    dyspnea,
    wheeze,
    diagnosis,
    fev1,
    fvc,
    fev1_post,
    fvc_post,
    tlc,
    rv,
    dlco,
    kco,
    va,
    comments,
    source
  )   

rm (pfts_pre)
rm (pfts_post)

# pfts_vyaire <- rbind (
#   read_xlsx ("../data/vyaire_pre_2020.xlsx"),
#   read_xlsx ("../data/vyaire_2020.xlsx"),
#   read_xlsx ("../data/vyaire_2021.xlsx"),
#   read_xlsx ("../data/vyaire_2022.xlsx"),
#   read_xlsx ("../data/vyaire_2023.xlsx"),
#   read_xlsx ("../data/vyaire_2024.xlsx"),
#   read_xlsx ("../data/vyaire_2025.xlsx")
# )
# 
# pfts_vyaire <- pfts_vyaire %>% 
#   select (
#     mrn = `Patient ID`,
#     last_name = Name,
#     first_name = Firstname,
#     middle_name = Middlename,
#     dob = Date_of_Birth,
#     date = `Visit Date`,
#     lab = Location,
#     age = Age,
#     sex = BiologicalGender,
#     height = Height,
#     weight = Weight,
#     diagnosis = Diagnosis,
#     fev1 = `FEV 1`,
#     fvc = `VC max`,
#     tlc = TLC_Pleth,
#     rv = RV_Pleth,
#     raw = Raw,
#     sgaw = sGaw,
#     dlco = DLCO_SB,
#     kco = `DLCO_SB / VA`,
#     va = VA
#   ) %>% 
#   mutate (pack_years = NA, .after = weight) %>% 
#   mutate (cough = NA, .after = pack_years) %>% 
#   mutate (dyspnea = NA, .after = cough) %>% 
#   mutate (wheeze = NA, .after = dyspnea) %>% 
#   mutate (comments = NA, .after = va)
# 
# pfts_vyaire_pre_post <- read_xlsx ("../data/vyaire_pre_post.xlsx") %>% 
#   select (
#     mrn = `Patient ID`,
#     last_name = Name,
#     first_name = Firstname,
#     middle_name = Middlename,
#     dob = `Date of Birth`,
#     date = `Visit Date`,
#     fev1_post = `Post FEV 1`,
#     fvc_post = `Post FVC`
#   ) %>% 
#   filter (is.na (fev1_post) == 0 | is.na (fvc_post) == 0) %>% 
#   group_by (mrn, last_name, first_name, middle_name, dob, date) %>% 
#   mutate (fev1_post = max (fev1_post)) %>% 
#   mutate (fvc_post = max (fvc_post)) %>% 
#   ungroup () %>% 
#   distinct ()
# 
# pfts_vyaire <- pfts_vyaire %>% 
#   left_join (pfts_vyaire_pre_post, by = c (
#     "mrn",
#     "last_name",
#     "first_name",
#     "middle_name",
#     "dob",
#     "date")
#   ) %>% 
#   relocate (fev1_post, .after = fvc) %>% 
#   relocate (fvc_post, .after = fev1_post) %>% 
#   mutate (dob = as.Date (mdy_hms (dob))) %>% 
#   mutate (date = as.Date (mdy_hms (date))) %>%
#   mutate (age = as.integer (age)) %>% 
#   mutate (sex = case_when (
#     sex == "male" ~ 1,
#     sex == "female" ~ 2)
#   ) %>%
#   mutate (sex = as.factor (sex)) %>% 
#   mutate (height = as.double (height)) %>% 
#   mutate (height = height * 39.3701) %>%
#   mutate (weight = as.double (weight)) %>% 
#   mutate (weight = weight * 2.20462) %>%
#   mutate (fev1 = as.double (fev1)) %>% 
#   mutate (fvc = as.double (fvc)) %>% 
#   mutate (fev1_post = as.double (fev1_post)) %>% 
#   mutate (fvc_post = as.double (fvc_post)) %>% 
#   mutate (tlc = as.double (tlc)) %>% 
#   mutate (rv = as.double (rv)) %>% 
#   mutate (raw = as.double (raw)) %>%
#   mutate (raw = raw * 10.197) %>% 
#   mutate (sgaw = as.double (sgaw)) %>%
#   mutate (sgaw = sgaw / 10.197) %>% 
#   mutate (dlco = as.double (dlco)) %>% 
#   mutate (dlco = dlco * 2.987) %>% 
#   mutate (kco = as.double (kco)) %>%
#   mutate (kco = kco * 2.987) %>% 
#   mutate (va = as.double (va)) %>% 
#   mutate (source = "vyaire")
  
################################################################################
## Combine Breeze and Vyaire Data
################################################################################

pfts_all <- rbind (pfts_breeze, pfts_vyaire)

################################################################################
## Update MRNs
################################################################################

# HUP MRNS
mrns <- read_csv ("../data/mrns.csv") %>% 
  select (last_name, first_name, dob, hup_mrn) %>% 
  mutate (last_name = tolower (last_name)) %>% 
  mutate (first_name = tolower (first_name)) %>% 
  distinct ()

pfts_all <- pfts_all %>%
  mutate (last_name = tolower (last_name)) %>% 
  mutate (first_name = tolower (first_name)) %>% 
  left_join (mrns, by = c ("last_name", "first_name", "dob")) %>% 
  filter (is.na (hup_mrn) == 0) %>% 
  mutate (mrn = hup_mrn) %>% 
  select (-hup_mrn) %>% 
  distinct (dob, last_name, first_name, date, .keep_all = TRUE)

################################################################################
## Apply Exclusion Criteria
################################################################################

pfts <- pfts_all %>%
  filter (age > 0 & (sex == 1 | sex == 2)) %>% 
  filter (height > 50 & weight > 25) %>% 
  filter (fev1 > 0 & fvc > 0) %>%
  filter (tlc > 0 & tlc < 20) %>%
  filter (age >= 18 & age <= 80)


################################################################################
## Add Race and Ethnicity Data
################################################################################

race <- read_parquet ("../data/race.parquet") %>% 
  select (
    mrn = pat_mrn_id,
    race = race1,
    ethnicity = ethnicity
  )

pfts <- pfts %>%
  left_join (race, by = "mrn")

################################################################################
## Interpret Data
################################################################################

pfts <- pfts %>% 
  mutate (fev1_fvc = fev1 / fvc, .after = fvc) %>%
  mutate (bmi = weight / (height / 100)^2) %>%
  mutate (sex = as.factor (sex)) %>%
  mutate (race = case_when (
    race == "White" |
    race == "HLW-Hispanic Latino/White" ~ 1, # "White"
    race == "Black or African American" |
    race == "HLB-Hispanic Latino/Black" ~ 2, # "Black"
    race == "Asian" ~ 3, # "Asian"
    race == "American Indian or Alaskan Native" |
    race == "East Indian" |
    race == "Native Hawaiian or Other Pacific Islander" |
    race == "Some Other Race" ~ 4) # "Other"
  ) %>%
  mutate (race = as.factor (race)) %>%
  mutate (ethnicity = case_when (
    ethnicity == "Hispanic Latino" ~ 1, # "Hispanic"
    ethnicity == "Not Hispanic or Latino" ~ 2) # Not Hispanic
  ) %>% 
  mutate (ethnicity = as.factor (ethnicity)) %>% 
  mutate (lab = case_when (
    lab == "HUP" |
    lab == "University of Pennsylvania Health System" |
    lab == "University of Pennsylvania Hospital" |
    lab == "UPHS" |
    lab == "UPHS Pulmonary" |
    lab == "UPHS Pulmonary Diagnostic Services" |
    lab == "UPHS Pulmonary Service" |
    lab == "UPHS Respiratory Care Services" ~ 1,
    lab == "Penn Presbyterian Allergy & Asthma" |
    lab == "Penn Presbyterian Hospital" |
    lab == "Penn Presbyterian Medical Center" |
    lab == "Penn Presbyterian Medical Center Pulmonary Lab" |
    lab == "Penn Presbyterian Medical Center." |
    lab == "Pmuc" |
    lab == "PMUC" |
    lab == "PPMC Harron Lung Center" |
    lab == "Presbyterian Medical Center" ~ 2,
    lab == "Pennsylvania Hospital" |
    lab == "PMWS" |
    lab == "PMWS 9th Floor" ~ 3,
    lab == "CCH" ~ 4)
  ) %>% 
  mutate (lab = as.factor (lab)) %>% 
  rowwise () %>%
  mutate (fev1_z_score = get_z_score (age, height, sex, "fev1", fev1), .after = fev1) %>%
  mutate (fev1_z_score_2012 = get_z_score_2012 (age, height, sex, race, "fev1", fev1), .after = fev1_z_score) %>% 
  mutate (fev1_predicted = get_mu (age, height, sex, "fev1"), .after = fev1_z_score_2012) %>%
  mutate (fev1_predicted_2012 = get_mu_2012 (age, height, sex, race, "fev1"), .after = fev1_predicted) %>% 
  mutate (fvc_z_score = get_z_score (age, height, sex, "fvc", fvc), .after = fvc) %>%
  mutate (fvc_z_score_2012 = get_z_score_2012 (age, height, sex, race, "fvc", fvc), .after = fvc_z_score) %>% 
  mutate (fvc_predicted = get_mu (age, height, sex, "fvc"), .after = fvc_z_score_2012) %>%
  mutate (fvc_predicted_2012 = get_mu_2012 (age, height, sex, race, "fvc"), .after = fvc_predicted) %>% 
  mutate (fev1_fvc_z_score = get_z_score (age, height, sex, "fev1/fvc", fev1_fvc), .after = fev1_fvc) %>%
  mutate (fev1_fvc_z_score_2012 = get_z_score_2012 (age, height, sex, race, "fev1/fvc", fev1_fvc), .after = fev1_fvc_z_score) %>% 
  mutate (tlc_z_score = get_z_score (age, height, sex, "tlc", tlc), .after = tlc) %>%
  mutate (rv_z_score = get_z_score (age, height, sex, "rv", rv), .after = rv) %>%
  mutate (dlco_z_score = get_z_score (age, height, sex, "dlco", dlco), .after = dlco) %>%
  mutate (kco_z_score = get_z_score (age, height, sex, "kco", kco), .after = kco) %>%
  mutate (va_z_score = get_z_score (age, height, sex, "va", va), .after = va) %>%
  mutate (interpretation = case_when (
    fev1_z_score < -1.645 & fvc_z_score < -1.645 & fev1_fvc_z_score >= -1.645 &
      tlc_z_score >= -1.645 ~ "Non-Specific",    
    fev1_fvc_z_score < -1.645 & tlc_z_score >= -1.645 ~ "Obstructive",
    fev1_fvc_z_score >= -1.645 & fvc_z_score >= -1.645 & 
      tlc_z_score < -1.645 ~ "Restrictive with Normal Spirometry",
    fev1_fvc_z_score >= -1.645 & fvc_z_score < -1.645 &
      tlc_z_score < -1.645 ~ "Restrictive with Abnormal Spirometry",
    fev1_fvc_z_score < -1.645 & tlc_z_score < -1.645 ~ "Mixed",
    fvc_z_score >= -1.645 & fev1_fvc_z_score >= -1.645 & tlc_z_score >= -1.645 ~ "Normal")
  ) %>%
  mutate (interpretation = factor (interpretation, levels = c (
    "Normal",
    "Non-Specific",
    "Obstructive",
    "Restrictive with Normal Spirometry",
    "Restrictive with Abnormal Spirometry",
    "Mixed"))
  ) %>%
  mutate (interpretation_2012 = case_when (
    fev1_z_score_2012 < -1.645 & fvc_z_score_2012 < -1.645 & fev1_fvc_z_score_2012 >= -1.645 &
      tlc_z_score >= -1.645 ~ "Non-Specific",    
    fev1_fvc_z_score_2012 < -1.645 & tlc_z_score >= -1.645 ~ "Obstructive",
    fev1_fvc_z_score_2012 >= -1.645 & fvc_z_score_2012 >= -1.645 & 
      tlc_z_score < -1.645 ~ "Restrictive with Normal Spirometry",
    fev1_fvc_z_score_2012 >= -1.645 & fvc_z_score_2012 < -1.645 &
      tlc_z_score < -1.645 ~ "Restrictive with Abnormal Spirometry",
    fev1_fvc_z_score_2012 < -1.645 & tlc_z_score < -1.645 ~ "Mixed",
    TRUE ~ "Normal")
  ) %>%
  mutate (interpretation_2012 = factor (interpretation_2012, levels = c (
    "Normal",
    "Non-Specific",
    "Obstructive",
    "Restrictive with Normal Spirometry",
    "Restrictive with Abnormal Spirometry",
    "Mixed"))
  ) %>%  
  ungroup () %>%
  group_by (mrn) %>% 
  mutate (pack_years = max (pack_years)) %>%
  ungroup () %>%
  mutate (restriction = case_when (tlc_z_score < -1.645 ~ 1, TRUE ~ 0)) %>%
  mutate (restriction = as.factor (restriction)) %>%
  rowwise () %>%
  mutate (response = get_response (fev1, fvc, fev1_post,
    fvc_post, fev1_predicted, fvc_predicted), .after = restriction
  ) %>%
  mutate (response_2012 = get_response (fev1, fvc, fev1_post, fvc_post,
    fev1_predicted_2012, fvc_predicted_2012), .after = response
  ) %>% 
  ungroup () %>%
  mutate (response = as.factor (response)) %>%
  mutate (response_2012 = as.factor (response_2012)) %>% 
  mutate (comments = str_to_lower (comments)) %>%
  mutate (comments = str_replace_all (comments, ",", "")) %>%
  mutate (effort = case_when(
    str_detect (comments, "efforts appeared inconsistent") |
    str_detect (comments, "effort could be stronger") |
    str_detect (comments, "effort is variable") |
    str_detect (comments, "efforts variable") |
    str_detect (comments, "efforts were marred") |
    str_detect (comments, "efforts were variable") |
    str_detect (comments, "effort was variable") |
    str_detect (comments, "fair effort") |
    str_detect (comments, "inconsistenteffort") |
    str_detect (comments, "poor effort") |
    str_detect (comments, "questionable effort") |
    str_detect (comments, "questionable patient effort") |
    str_detect (comments, "suboptimal effort") |
    str_detect (comments, "uneven effort") |
    str_detect (comments, "variable effort") |
    str_detect (comments, "weak effort") ~ 0,
    str_detect (comments, "adequate effort") |
    str_detect (comments, "adequate patient effort") |
    str_detect (comments, "best effort throughout") |
    str_detect (comments, "effort appeared good") |
    str_detect (comments, "effort was good") |
    str_detect (comments, "good effort") |
    str_detect (comments, "good patient coordination and effort") |
    str_detect (comments, "good patient effort") |
    str_detect (comments, "good pt effort") |
    str_detect (comments, "goodpatient comprehension and effort") |
    str_detect (comments, "great effort") |
    str_detect (comments, "maximal effort") |
    str_detect (comments, "repeatable effort") ~ 1)
  ) %>%
  mutate (effort = as.factor (effort)) %>% 
  mutate (cough = case_when (
    cough == 2 | # productive
    cough == 3 ~ 1, # non-productive
    cough == 4 ~ 0) # no cough
  ) %>%
  mutate (cough = as.factor (cough)) %>% 
  mutate (dyspnea = case_when (
    dyspnea == 3 | # on hills and stairs
    dyspnea == 4 | # walking more than 100 yards
    dyspnea == 5 | # walking less than 100 yards
    dyspnea == 6 ~ 1, # after any exertion
    dyspnea == 2 | # after severe exertion
    dyspnea == 7 ~ 0) # no dyspnea
  ) %>% 
  mutate (dyspnea = as.factor (dyspnea)) %>% 
  mutate (wheeze = case_when (
    wheeze == 2 | # rare
    wheeze == 3 | # frequent
    wheeze == 4 ~ 1, # constant
    wheeze == 5 ~ 0) # no wheeze
  ) %>%
  mutate (wheeze = as.factor (wheeze)) %>% 
  arrange (date) %>%
  arrange (mrn) %>% 
  rowid_to_column ("test")
 
################################################################################
## Label with Respiratory Diseases
################################################################################

##### Number of encounters by NLP ####

# reader <- ParquetFileReader$create ("../data/notes.parquet")
# 
# nlp <- tibble ()
# 
# for (i in seq_len (reader$num_row_groups)) {
# 
#   #print (i)
# 
#   batch <- reader$ReadRowGroup(i - 1)
# 
#   nlp_batch <- as_tibble (batch) %>%
#     select (mrn, pat_enc_csn_id, line = note_line, text = note_text) %>%
#     arrange (pat_enc_csn_id) %>%
#     filter (!(text == "" | text == " " | is.na (text))) %>%
#     arrange (line) %>%
#     group_by (pat_enc_csn_id) %>%
#     summarize (text = paste (text, collapse = " ")) %>%
#     mutate (text = str_squish (text)) %>%
#     mutate (text = str_to_lower (text)) %>%
#     mutate (asthma = case_when (
#       str_detect (text, paste (nlp_asthma, collapse = "|")) ~ 1,
#       TRUE ~ 0)
#     ) %>%
#     mutate (bronchiectasis = case_when (
#       str_detect (text, paste (nlp_bronchiectasis, collapse = "|")) ~ 1,
#       TRUE ~ 0)
#     ) %>%
#     mutate (chest_wall = case_when (
#       str_detect (text, paste (nlp_chest_wall, collapse = "|")) ~ 1,
#       TRUE ~ 0)
#     ) %>%
#     mutate (copd = case_when (
#       str_detect (text, paste (nlp_copd, collapse = "|")) ~ 1,
#       TRUE ~ 0)
#     ) %>%
#     mutate (ild = case_when (
#       str_detect (text, paste (nlp_ild, collapse = "|")) ~ 1,
#       TRUE ~ 0)
#     ) %>%
#     mutate (neuromuscular = case_when (
#       str_detect (text, paste (nlp_neuromuscular, collapse = "|")) ~ 1,
#       TRUE ~ 0)
#     ) %>%
#     mutate (ph = case_when (
#       str_detect (text, paste (nlp_ph, collapse = "|")) ~ 1,
#       TRUE ~ 0)
#     ) %>%         
#     select (
#       pat_enc_csn_id,
#       asthma,
#       bronchiectasis,
#       chest_wall,
#       copd,
#       ild,
#       neuromuscular,
#       ph
#     )
# 
#   nlp <- rbind (nlp, nlp_batch)
# 
#   rm (batch, nlp_batch)
#   gc ()
# 
# }
# 
# encounters <- read_parquet ("../data/encounters.parquet") %>%
#   filter (encounter_type == "Office Visit")
# 
# nlp <- encounters %>%
#   select (pat_mrn_id, pat_enc_csn_id) %>%
#   left_join (nlp, by = "pat_enc_csn_id") %>%
#   select (-pat_enc_csn_id) %>% 
#   group_by (pat_mrn_id) %>%
#   summarise(across(everything(), ~sum(.x, na.rm = TRUE)), .groups = "drop") %>% 
#   select (
#     pat_mrn_id,
#     asthma_nlp = asthma,
#     bronchiectasis_nlp = bronchiectasis,
#     chest_wall_nlp = chest_wall,
#     copd_nlp = copd,
#     ild_nlp = ild,
#     neuromuscular_nlp = neuromuscular,
#     ph_nlp = ph
#   )
#   
# rm (encounters)
# 
# gc ()
# 
# write_csv (nlp, "../data/nlp.csv")

nlp <- read_csv ("../data/nlp.csv")

##### Number of encounters by ICD code ####

# diagnoses <- read_parquet ("../data/diagnoses.parquet")
# 
# icd <- diagnoses %>%
#   rename (icd = code) %>%
#   mutate (asthma_icd = case_when (
#     icd %in% icd_asthma ~ 1,
#     TRUE ~ 0)
#   ) %>%
#   mutate (bronchiectasis_icd = case_when (
#     icd %in% icd_bronchiectasis ~ 1,
#     TRUE ~ 0)
#   ) %>%
#   mutate (chest_wall_icd = case_when (
#     icd %in% icd_chest_wall ~ 1,
#     TRUE ~ 0)
#   ) %>%
#   mutate (copd_icd = case_when (
#     icd %in% icd_copd ~ 1,
#     TRUE ~ 0)
#   ) %>%
#   mutate (ild_icd = case_when (
#     icd %in% icd_ild ~ 1,
#     TRUE ~ 0)
#   ) %>%
#   mutate (neuromuscular_icd = case_when (
#     icd %in% icd_neuromuscular ~ 1,
#     TRUE ~ 0)
#   ) %>%
#   mutate (ph_icd = case_when (
#     icd %in% icd_ph ~ 1,
#     TRUE ~ 0)
#   ) %>%     
#   select (
#     pat_enc_csn_id,
#     asthma_icd,
#     bronchiectasis_icd,
#     chest_wall_icd,
#     copd_icd,
#     ild_icd,
#     neuromuscular_icd,
#     ph_icd
#   ) %>%
#   group_by (pat_enc_csn_id) %>%
#   summarize (
#     asthma_icd = max (asthma_icd),
#     bronchiectasis_icd = max (bronchiectasis_icd),
#     chest_wall_icd = max (chest_wall_icd),
#     copd_icd = max (copd_icd),
#     ild_icd = max (ild_icd),
#     neuromuscular_icd = max (neuromuscular_icd),
#     ph_icd = max (ph_icd)
#   ) %>%
#   ungroup ()
# 
# encounters <- read_parquet ("../data/encounters.parquet") %>%
#   filter (encounter_type == "Office Visit")
# 
# icd <- encounters %>%
#   select (pat_mrn_id, pat_enc_csn_id) %>%
#   left_join (icd, by = "pat_enc_csn_id") %>%
#   group_by (pat_mrn_id) %>%
#   summarize (
#     asthma_icd = sum (asthma_icd, na.rm = TRUE),
#     bronchiectasis_icd = sum (bronchiectasis_icd, na.rm = TRUE),
#     chest_wall_icd = sum (chest_wall_icd, na.rm = TRUE),
#     copd_icd = sum (copd_icd, na.rm = TRUE),
#     ild_icd = sum (ild_icd, na.rm = TRUE),
#     neuromuscular_icd = sum (neuromuscular_icd, na.rm = TRUE),
#     ph_icd = sum (ph_icd, na.rm = TRUE),    
#   ) %>%
#   ungroup ()
# 
# rm (diagnoses)
# rm (encounters)
# 
# gc ()
# 
# write_csv (icd, "../data/icd.csv")

icd <- read_csv ("../data/icd.csv")

##### Number of office visits per patient ####

# encounters <- read_parquet ("../data/encounters.parquet") %>% 
#   filter (encounter_type == "Office Visit")  
# 
# visits <- encounters %>%
#   select (pat_mrn_id, pat_enc_csn_id) %>% 
#   group_by (pat_mrn_id) %>% 
#   summarize (visits = n ()) %>% 
#   ungroup ()
# 
# rm (encounters)
# 
# write_csv (visits, "../data/visits.csv")

visits <- read_csv ("../data/visits.csv")

#### Apply MAP algorithm ####

# map <- visits %>%
#   left_join (icd, by = "pat_mrn_id") %>% 
#   left_join (nlp, by = "pat_mrn_id")
# 
# visits <- map %>% 
#   pull (visits)
# 
# # Asthma
# 
# ICD <- map %>% 
#   pull (asthma_icd)
# 
# NLP <- map %>% 
#   pull (asthma_nlp)
# 
# mat <- Matrix (data = cbind (ICD, NLP), sparse = TRUE)
# 
# note <- Matrix (visits, ncol = 1, sparse = TRUE)
# 
# asthma_map <- MAP (
#   mat = mat,
#   note = note,
#   full.output = TRUE,
#   subset_sample = TRUE,
#   subset_sample_size = 10000,
#   verbose = TRUE
# )
# 
# map <- map %>% 
#   add_column (asthma = as.vector (asthma_map$scores)) %>% 
#   mutate (asthma = case_when (
#     asthma > asthma_map$cut.MAP ~ 1,
#     TRUE ~ 0)
#   )
# 
# # Bronchiectasis
# 
# ICD <- map %>% 
#   pull (bronchiectasis_icd)
# 
# NLP <- map %>% 
#   pull (bronchiectasis_nlp)
# 
# mat <- Matrix (data = cbind (ICD, NLP), sparse = TRUE)
# 
# note <- Matrix (visits, ncol = 1, sparse = TRUE)
# 
# bronchiectasis_map <- MAP (
#   mat = mat,
#   note = note,
#   full.output = TRUE,
#   subset_sample = TRUE,
#   subset_sample_size = 10000,
#   verbose = TRUE
# )
# 
# map <- map %>% 
#   add_column (bronchiectasis = as.vector (bronchiectasis_map$scores)) %>% 
#   mutate (bronchiectasis = case_when (
#     bronchiectasis > bronchiectasis_map$cut.MAP ~ 1,
#     TRUE ~ 0)
#   )
# 
# # Chest wall
# 
# ICD <- map %>% 
#   pull (chest_wall_icd)
# 
# NLP <- map %>% 
#   pull (chest_wall_nlp)
# 
# mat <- Matrix (data = cbind (ICD, NLP), sparse = TRUE)
# 
# note <- Matrix (visits, ncol = 1, sparse = TRUE)
# 
# chest_wall_map <- MAP (
#   mat = mat,
#   note = note,
#   full.output = TRUE,
#   subset_sample = TRUE,
#   subset_sample_size = 10000,
#   verbose = TRUE
# )
# 
# map <- map %>% 
#   add_column (chest_wall = as.vector (chest_wall_map$scores)) %>% 
#   mutate (chest_wall = case_when (
#     chest_wall > chest_wall_map$cut.MAP ~ 1,
#     TRUE ~ 0)
#   )
# 
# # COPD
# 
# ICD <- map %>% 
#   pull (copd_icd)
# 
# NLP <- map %>% 
#   pull (copd_nlp)
# 
# mat <- Matrix (data = cbind (ICD, NLP), sparse = TRUE)
# 
# note <- Matrix (visits, ncol = 1, sparse = TRUE)
# 
# copd_map <- MAP (
#   mat = mat,
#   note = note,
#   full.output = TRUE,
#   subset_sample = TRUE,
#   subset_sample_size = 10000,
#   verbose = TRUE
# )
# 
# map <- map %>% 
#   add_column (copd = as.vector (copd_map$scores)) %>% 
#   mutate (copd = case_when (
#     copd > copd_map$cut.MAP ~ 1,
#     TRUE ~ 0)
#   )
# 
# # ILD
# 
# ICD <- map %>% 
#   pull (ild_icd)
# 
# NLP <- map %>% 
#   pull (ild_nlp)
# 
# mat <- Matrix (data = cbind (ICD, NLP), sparse = TRUE)
# 
# note <- Matrix (visits, ncol = 1, sparse = TRUE)
# 
# ild_map <- MAP (
#   mat = mat,
#   note = note,
#   full.output = TRUE,
#   subset_sample = TRUE,
#   subset_sample_size = 10000,
#   verbose = TRUE
# )
# 
# map <- map %>% 
#   add_column (ild = as.vector (ild_map$scores)) %>% 
#   mutate (ild = case_when (
#     ild > ild_map$cut.MAP ~ 1,
#     TRUE ~ 0)
#   )
# 
# # Neuromuscular
# 
# ICD <- map %>% 
#   pull (neuromuscular_icd)
# 
# NLP <- map %>% 
#   pull (neuromuscular_nlp)
# 
# mat <- Matrix (data = cbind (ICD, NLP), sparse = TRUE)
# 
# note <- Matrix (visits, ncol = 1, sparse = TRUE)
# 
# neuromuscular_map <- MAP (
#   mat = mat,
#   note = note,
#   full.output = TRUE,
#   subset_sample = TRUE,
#   subset_sample_size = 10000,
#   verbose = TRUE
# )
# 
# map <- map %>% 
#   add_column (neuromuscular = as.vector (neuromuscular_map$scores)) %>% 
#   mutate (neuromuscular = case_when (
#     neuromuscular > neuromuscular_map$cut.MAP ~ 1,
#     TRUE ~ 0)
#   )
# 
# # Pulmonary hypertension
# 
# ICD <- map %>% 
#   pull (ph_icd)
# 
# NLP <- map %>% 
#   pull (ph_nlp)
# 
# mat <- Matrix (data = cbind (ICD, NLP), sparse = TRUE)
# 
# note <- Matrix (visits, ncol = 1, sparse = TRUE)
# 
# ph_map <- MAP (
#   mat = mat,
#   note = note,
#   full.output = TRUE,
#   subset_sample = TRUE,
#   subset_sample_size = 10000,
#   verbose = TRUE
# )
# 
# map <- map %>% 
#   add_column (ph = as.vector (ph_map$scores)) %>% 
#   mutate (ph = case_when (
#     ph > ph_map$cut.MAP ~ 1,
#     TRUE ~ 0)
#   )
# 
# map <- map %>% 
#   select (
#     mrn = pat_mrn_id,
#     asthma,
#     bronchiectasis,
#     chest_wall,
#     copd,
#     ild,
#     neuromuscular,
#     ph
#   )
# 
# write_csv (map, "../data/map.csv")

map <- read_csv ("../data/map.csv")

pfts <- pfts %>% 
  left_join (map, by = "mrn") %>%
  mutate (any = case_when (
    asthma == 1 |
    bronchiectasis == 1 |
    chest_wall == 1 |
    copd == 1 |
    ild == 1 |
    neuromuscular == 1 ~ 1,
    asthma == 0 &
    bronchiectasis == 0 &
    chest_wall == 0 &
    copd == 0 &
    ild == 0 &
    neuromuscular == 0 ~ 0)    
  ) %>% 
  mutate (none = case_when (
    any == 1 ~ 0,
    any == 0 ~ 1)
  ) %>% 
  mutate (asthma = as.factor (asthma)) %>% 
  mutate (bronchiectasis = as.factor (bronchiectasis)) %>% 
  mutate (chest_wall = as.factor (chest_wall)) %>% 
  mutate (copd = as.factor (copd)) %>% 
  mutate (ild = as.factor (ild)) %>% 
  mutate (neuromuscular = as.factor (neuromuscular)) %>% 
  mutate (none = as.factor (none))

# For sensitivity analysis, add diagnoses if at least 1 ICD Code Present

diagnoses <- read_parquet ("../data/diagnoses.parquet")

diagnoses <- diagnoses %>% 
  select (mrn, diagnosis = code)

diagnoses_asthma <- diagnoses %>% 
  filter (diagnosis %in% icd_asthma) %>% 
  select (mrn) %>% 
  distinct () %>% 
  mutate (asthma_icd = 1)

diagnoses_bronchiectasis <- diagnoses %>% 
  filter (diagnosis %in% icd_bronchiectasis) %>% 
  select (mrn) %>% 
  distinct () %>% 
  mutate (bronchiectasis_icd = 1)

diagnoses_chest_wall <- diagnoses %>% 
  filter (diagnosis %in% icd_chest_wall) %>% 
  select (mrn) %>% 
  distinct () %>% 
  mutate (chest_wall_icd = 1)

diagnoses_copd <- diagnoses %>% 
  filter (diagnosis %in% icd_copd)  %>% 
  select (mrn) %>% 
  distinct () %>% 
  mutate (copd_icd = 1)

diagnoses_ild <- diagnoses %>% 
  filter (diagnosis %in% icd_ild) %>% 
  select (mrn) %>% 
  distinct () %>% 
  mutate (ild_icd = 1)

diagnoses_neuromuscular <- diagnoses %>% 
  filter (diagnosis %in% icd_neuromuscular) %>% 
  select (mrn) %>% 
  distinct () %>% 
  mutate (neuromuscular_icd = 1)

pfts <- pfts %>% 
  left_join (diagnoses_asthma, by = "mrn") %>% 
  left_join (diagnoses_bronchiectasis, by = "mrn") %>% 
  left_join (diagnoses_chest_wall, by = "mrn") %>% 
  left_join (diagnoses_copd, by = "mrn") %>% 
  left_join (diagnoses_ild, by = "mrn") %>% 
  left_join (diagnoses_neuromuscular, by = "mrn") %>% 
  mutate (asthma_icd = replace_na (asthma_icd, 0)) %>% 
  mutate (bronchiectasis_icd = replace_na (bronchiectasis_icd, 0)) %>% 
  mutate (chest_wall_icd = replace_na (chest_wall_icd, 0)) %>% 
  mutate (copd_icd = replace_na (copd_icd, 0)) %>% 
  mutate (ild_icd = replace_na (ild_icd, 0)) %>% 
  mutate (neuromuscular_icd = replace_na (neuromuscular_icd, 0))

################################################################################
## Label with Mortality Data
################################################################################

mortality <- read_parquet ("../data/mortality.parquet")

mortality <- mortality %>%
  rename (mrn = pat_mrn_id) %>% 
  filter (is.na (most_recent_encounter_date) == 0) %>%
  mutate (dead = case_when (
    is.na (death_date) == 0 ~ 1,
    TRUE ~ 0)
  ) %>%
  mutate (date_last = case_when (
    dead == 1 ~ death_date,
    dead == 0 ~ most_recent_encounter_date)
  ) %>%
  select (
    mrn,
    dead,
    date_last
  )

pfts <- pfts %>%
  left_join (mortality, by = "mrn")

################################################################################
## Label with ED Encounters
################################################################################

encounters <- read_parquet ("../data/encounters.parquet") %>% 
  select (pat_mrn_id, pat_enc_csn_id)

ed <- read_parquet ("../data/ed.parquet") %>% 
  left_join (encounters, by = "pat_enc_csn_id") %>% 
  rename (mrn = pat_mrn_id) %>%
  rename (date_ed = adt_arrival_dttm) %>% 
  mutate (date_ed = date (date_ed)) %>%
  rename (reason = reason_visit_names) %>% 
  filter (
    str_detect (reason, "AIRWAY OBSTRUCTION") |
    str_detect (reason, "ASPIRATION") |
    str_detect (reason, "ASTHMA") |
    str_detect (reason, "BREATHING PROBLEM") |
    str_detect (reason, "COPD") |
    str_detect (reason, "COUGH") |
    str_detect (reason, "CYSTIC FIBROSIS") |
    str_detect (reason, "INFLUENZA") |
    str_detect (reason, "PNEUMONIA") |
    str_detect (reason, "RESPIRATORY ARREST") |
    str_detect (reason, "RESPIRATORY DISTRESS") |
    str_detect (reason, "SHORTNESS OF BREATH") |
    str_detect (reason, "TRACHEOSTOMY TUBE CHANGE") |
    str_detect (reason, "URI") |
    str_detect (reason, "WHEEZING")
  ) %>% 
  select (mrn, date_ed)

ed <- pfts %>% 
  left_join (ed, by = "mrn", relationship = "many-to-many") %>%
  filter (date_ed > date) %>% 
  group_by (test) %>% 
  slice_min (date_ed, n = 1, with_ties = FALSE) %>% 
  ungroup () %>% 
  select (test, date_ed)
  
pfts <- pfts %>% 
  left_join (ed, by = "test")

################################################################################
## Label with Imaging Data
################################################################################

imaging <- read_parquet ("../data/imaging.parquet")

imaging <- imaging %>%
  mutate (modality = case_when (
    str_detect (description, "CT CHEST") ~ "ct")
  ) %>%
  filter (modality == "ct") %>% 
  select (
    mrn,
    imaging_date = order_time,
    order = order_proc_id,
    modality
  ) %>%
  mutate (imaging_date = as.Date (imaging_date))

impressions <- read_parquet ("../data/impressions.parquet")
narratives <- read_parquet ("../data/narratives.parquet")

interpretations <- bind_rows (
  narratives %>% mutate (section = 1, text = narrative),
  impressions %>% mutate (section = 2, text = impression)
) %>%
arrange (mrn, order_proc_id, section, line) %>%
group_by (mrn, order_proc_id) %>%
summarise (interpretation = paste (text, collapse = " "), .groups = "drop") %>% 
select (order = order_proc_id, interpretation)

interpretations <- imaging %>%
  left_join (interpretations, by = "order") %>% 
  filter (is.na (interpretation) == 0) %>%
  mutate (interpretation = str_squish (interpretation)) %>% 
  mutate (interpretation = tolower (interpretation)) %>%
  mutate (emphysema = map_dbl (interpretation, ~get_finding (.x, expression_emphysema))) %>%
  mutate (honeycombing = map_dbl (interpretation, ~get_finding (.x, expression_honeycombing))) %>%
  mutate (reticulation = map_dbl (interpretation, ~get_finding (.x, expression_reticulation))) %>%  
  mutate (thickening = map_dbl (interpretation, ~get_finding (.x, expression_thickening))) %>% 
  mutate (traction = map_dbl (interpretation, ~get_finding (.x, expression_traction))) %>%
  group_by (mrn) %>% 
  summarize (
    across (
      c( emphysema, honeycombing, reticulation, thickening, traction),
      ~ max (.x, na.rm = TRUE)
    ),
    .groups = "drop"
  )

pfts <- pfts %>% 
  left_join (interpretations, by = "mrn")

write_csv (pfts, "../data/pfts.csv")

################################################################################
## Label with Provider Specialty
################################################################################

procedures <- read_parquet ("../data/procedures.parquet")
providers <- read_parquet ("../data/providers.parquet")

procedures <- procedures %>% 
  select (
    mrn = pat_mrn_id,
    procedure_date = order_time,
    provider = ord_creatr_user_id_did,
    description
  ) %>% 
  filter (
    description == "FLOW VOLUME LOOP" |
    description == "PLETHYSMOGRAPHY LUNG VOLUMES W/WO AIRWAY RESIST" |
    description == "PULMONARY FUNCTION TEST" |
    description == "PULMONARY FUNCTION TESTING" |
    description == "SPIROMETRY" |      
    description == "SPIROMETRY (OFFICE)" |
    description == "SPIROMETRY PRE/POST BRONCHODILATOR (OFFICE)"
  ) %>% 
  distinct ()

providers <- providers %>%
  filter (
    provider_type == "Physician"
  ) %>% 
  select (
    provider = visit_user_id_did,
    specialty = team_or_specialty
  ) %>%
  distinct ()

procedures <- procedures %>% 
  left_join (providers, by = "provider") %>% 
  select (mrn, procedure_date, specialty) %>% 
  distinct () %>%
  mutate (specialty = case_when (
    specialty == "Internal Medicine;Pulmonary Medicine" |
    specialty == "Pulmonary Medicine" |
    specialty == "Pulmonary Medicine;Internal Medicine" ~ 1, # Pulmonology
    specialty == "Family Medicine" |
    specialty == "Family Medicine;Internal Medicine" |
    specialty == "Geriatrics" |
    specialty == "Internal Medicine" |
    specialty == "Internal Medicine;Family Medicine" ~ 2, # Primary care
    is.na (specialty) == 0 ~ 3) # Other
  ) %>%
  distinct ()

pfts <- pfts %>% 
  left_join (procedures, by = join_by (mrn, closest (date >= procedure_date))) %>% 
  select (-procedure_date) %>% 
  distinct () %>%
  group_by (mrn, date) %>%
  slice (1) %>%
  ungroup ()

