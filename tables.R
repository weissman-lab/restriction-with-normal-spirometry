################################################################################
## Table 1
################################################################################

data <- pfts %>%
  mutate (spirometry = case_when (
    interpretation == "Normal" ~ 1,
    interpretation == "Non-Specific" ~ 2,
    interpretation == "Obstructive" ~ 3,
    interpretation == "Restrictive with Normal Spirometry" ~ 4,
    interpretation == "Restrictive with Abnormal Spirometry" ~ 5,
    interpretation == "Mixed" ~ 6)
  ) %>%
  mutate (spirometry = factor (spirometry, levels = c ("1", "2", "3", "4", "5", "6")))

table <- table1 (~
  age +
  sex +
  race +
  ethnicity +
  bmi +
  pack_years +
  cough +
  dyspnea +
  wheeze +
  asthma +
  bronchiectasis +
  chest_wall +
  copd +
  ild +
  neuromuscular +
  none +
  copd_imaging +
  ild_imaging +
  fev1_z_score +
  fvc_z_score +
  fev1_fvc_z_score +
  tlc_z_score +
  rv_z_score +
  dlco_z_score +
  kco_z_score |
  spirometry,
  data = data,
  big.mark =",",
  render.categorical = render_categorical,
  render.continuous = render_continuous
)

table <- as.data.frame (table)

table <- paste (
  "#table(\n\t",
  "inset: (x: 4pt, y: 2.5pt),\n\t ",
  "columns: 8,\n\t",
  "align: (left, center, center, center, center, center, center, center),\n\t",
  "table.hline(),\n\t",
  "[], table.cell(colspan: 7, [*Pulmonary Function Test Interpretation*]),\n\t",
	"table.hline(start:1, end:8),\n\t",
  "[], [], [], [], table.cell(colspan: 2, [*Restrictive*]), [], [],\n\t",
  "table.hline(start:4, end:6),\n\t",
  "[], [], [], [], [*Normal*], [*Abnormal*], [], [],\n\t",
  "[], [*Normal*], [*Non-Specific*], [*Obstructive*], [*Spirometry*], [*Spirometry*], [*Mixed*], [*All*],\n\t",
  "[],", 
  "[($n=",
  comma (parse_number (table [[1,2]])), 
  "$)], [($n=",
  comma (parse_number (table [[1,3]])),
  "$)], [($n=",
  comma (parse_number (table [[1,4]])),
  "$)], [($n=",
  comma (parse_number (table [[1,5]])),
  "$)], [($n=",
  comma (parse_number (table [[1,6]])),
  "$)], [($n=",
  comma (parse_number (table [[1,7]])),
  "$)], [($n=",
  comma (parse_number (table [[1,8]])),
  "$)],\n\t",
  "table.hline(),\n\t",
  "[*Age, yr*],",
  "[",
  table [[which (table [,1] == "age"), 2]],
  "], [",
  table [[which (table [,1] == "age"), 3]],
  "], [",
  table [[which (table [,1] == "age"), 4]],
  "], [",
  table [[which (table [,1] == "age"), 5]],
  "], [",
  table [[which (table [,1] == "age"), 6]],
  "], [",
  table [[which (table [,1] == "age"), 7]],
  "], [",
  table [[which (table [,1] == "age"), 8]],  
  "],\n\t",
  "[*Sex*], [], [], [], [], [], [], [],\n\t",
  "[#h(1em)Men],",
  "[",
  table [[which (table [,1] == "sex") + 1, 2]],
  "], [",
  table [[which (table [,1] == "sex") + 1, 3]],
  "], [",
  table [[which (table [,1] == "sex") + 1, 4]],
  "], [",
  table [[which (table [,1] == "sex") + 1, 5]],
  "], [",
  table [[which (table [,1] == "sex") + 1, 6]],
  "], [",
  table [[which (table [,1] == "sex") + 1, 7]],
  "], [",
  table [[which (table [,1] == "sex") + 1, 8]],  
  "],\n\t",
  "[#h(1em)Women],",
  "[",
  table [[which (table [,1] == "sex") + 2, 2]],
  "], [",
  table [[which (table [,1] == "sex") + 2,3]],
  "], [",
  table [[which (table [,1] == "sex") + 2,4]],
  "], [",
  table [[which (table [,1] == "sex") + 2,5]],
  "], [",
  table [[which (table [,1] == "sex") + 2,6]],
  "], [",
  table [[which (table [,1] == "sex") + 2,7]],
  "], [",
  table [[which (table [,1] == "sex") + 2,8]],  
  "],\n\t",
  "[*Race*], [], [], [], [], [], [], [],\n\t",
  "[#h(1em)White],",
  "[",
  table [[which (table [,1] == "race") + 1, 2]],
  "], [",
  table [[which (table [,1] == "race") + 1, 3]],
  "], [",
  table [[which (table [,1] == "race") + 1, 4]],
  "], [",
  table [[which (table [,1] == "race") + 1, 5]],
  "], [",
  table [[which (table [,1] == "race") + 1, 6]],
  "], [",
  table [[which (table [,1] == "race") + 1, 7]],
  "], [",
  table [[which (table [,1] == "race") + 1, 8]],  
  "],\n\t",
  "[#h(1em)Black],",
  "[",
  table [[which (table [,1] == "race") + 2, 2]],
  "], [",
  table [[which (table [,1] == "race") + 2, 3]],
  "], [",
  table [[which (table [,1] == "race") + 2, 4]],
  "], [",
  table [[which (table [,1] == "race") + 2, 5]],
  "], [",
  table [[which (table [,1] == "race") + 2, 6]],
  "], [",
  table [[which (table [,1] == "race") + 2, 7]],
  "], [",
  table [[which (table [,1] == "race") + 2, 8]],  
  "],\n\t",
  "[#h(1em)Asian],",
  "[",
  table [[which (table [,1] == "race") + 3, 2]],
  "], [",
  table [[which (table [,1] == "race") + 3, 3]],
  "], [",
  table [[which (table [,1] == "race") + 3, 4]],
  "], [",
  table [[which (table [,1] == "race") + 3 ,5]],
  "], [",
  table [[which (table [,1] == "race") + 3, 6]],
  "], [",
  table [[which (table [,1] == "race") + 3 ,7]],
  "], [",
  table [[which (table [,1] == "race") + 3, 8]],  
  "],\n\t",  
  "[#h(1em)Other],",
  "[",
  table [[which (table [,1] == "race") + 4, 2]],
  "], [",
  table [[which (table [,1] == "race") + 4, 3]],
  "], [",
  table [[which (table [,1] == "race") + 4, 4]],
  "], [",
  table [[which (table [,1] == "race") + 4, 5]],
  "], [",
  table [[which (table [,1] == "race") + 4, 6]],
  "], [",
  table [[which (table [,1] == "race") + 4, 7]],
  "], [",
  table [[which (table [,1] == "race") + 4, 8]],  
  "],\n\t",
  "[*Ethnicity*], [], [], [], [], [], [], [],\n\t",
  "[#h(1em)Hispanic],",
  "[",
  table [[which (table [,1] == "ethnicity") + 1, 2]],
  "], [",
  table [[which (table [,1] == "ethnicity") + 1, 3]],
  "], [",
  table [[which (table [,1] == "ethnicity") + 1, 4]],
  "], [",
  table [[which (table [,1] == "ethnicity") + 1, 5]],
  "], [",
  table [[which (table [,1] == "ethnicity") + 1, 6]],
  "], [",
  table [[which (table [,1] == "ethnicity") + 1, 7]],
  "], [",
  table [[which (table [,1] == "ethnicity") + 1, 8]],  
  "],\n\t",
  "[#h(1em)Not Hispanic],",
  "[",
  table [[which (table [,1] == "ethnicity") + 2, 2]],
  "], [",
  table [[which (table [,1] == "ethnicity") + 2, 3]],
  "], [",
  table [[which (table [,1] == "ethnicity") + 2, 4]],
  "], [",
  table [[which (table [,1] == "ethnicity") + 2, 5]],
  "], [",
  table [[which (table [,1] == "ethnicity") + 2, 6]],
  "], [",
  table [[which (table [,1] == "ethnicity") + 2, 7]],
  "], [",
  table [[which (table [,1] == "ethnicity") + 2, 8]],  
  "],\n\t",   
  "[*BMI, kg/m#super[2]*],",
  "[",
  table [[which (table [,1] == "bmi"), 2]],
  "], [",
  table [[which (table [,1] == "bmi"), 3]],
  "], [",
  table [[which (table [,1] == "bmi"), 4]],
  "], [",
  table [[which (table [,1] == "bmi"), 5]],
  "], [",
  table [[which (table [,1] == "bmi"), 6]],
  "], [",
  table [[which (table [,1] == "bmi"), 7]],
  "], [",
  table [[which (table [,1] == "bmi"), 8]],  
  "],\n\t",  
  "[*Pack Years, yr*],",
  "[",
  table [[which (table [,1] == "pack_years"), 2]],
  "], [",
  table [[which (table [,1] == "pack_years"), 3]],
  "], [",
  table [[which (table [,1] == "pack_years"), 4]],
  "], [",
  table [[which (table [,1] == "pack_years"), 5]],
  "], [",
  table [[which (table [,1] == "pack_years"), 6]],
  "], [",
  table [[which (table [,1] == "pack_years"), 7]],
  "], [",
  table [[which (table [,1] == "pack_years"), 8]],  
  "],\n\t",
  "[*Respiratory Symptoms*], [], [], [], [], [], [], [],\n\t",
  "[#h(1em)Cough],",
  "[",
  table [[which (table [,1] == "cough") + 2, 2]],
  "], [",
  table [[which (table [,1] == "cough") + 2, 3]],
  "], [",
  table [[which (table [,1] == "cough") + 2, 4]],
  "], [",
  table [[which (table [,1] == "cough") + 2, 5]],
  "], [",
  table [[which (table [,1] == "cough") + 2, 6]],
  "], [",
  table [[which (table [,1] == "cough") + 2, 7]],
  "], [",
  table [[which (table [,1] == "cough") + 2, 8]], 
  "],\n\t",
  "[#h(1em)Dyspnea],",
  "[",
  table [[which (table [,1] == "dyspnea") + 2, 2]],
  "], [",
  table [[which (table [,1] == "dyspnea") + 2, 3]],
  "], [",
  table [[which (table [,1] == "dyspnea") + 2, 4]],
  "], [",
  table [[which (table [,1] == "dyspnea") + 2, 5]],
  "], [",
  table [[which (table [,1] == "dyspnea") + 2, 6]],
  "], [",
  table [[which (table [,1] == "dyspnea") + 2, 7]],
  "], [",
  table [[which (table [,1] == "dyspnea") + 2, 8]], 
  "],\n\t",
  "[#h(1em)Wheeze],",
  "[",
  table [[which (table [,1] == "wheeze") + 2, 2]],
  "], [",
  table [[which (table [,1] == "wheeze") + 2, 3]],
  "], [",
  table [[which (table [,1] == "wheeze") + 2, 4]],
  "], [",
  table [[which (table [,1] == "wheeze") + 2, 5]],
  "], [",
  table [[which (table [,1] == "wheeze") + 2, 6]],
  "], [",
  table [[which (table [,1] == "wheeze") + 2, 7]],
  "], [",
  table [[which (table [,1] == "wheeze") + 2, 8]], 
  "],\n\t",     
  "[*Respiratory Disease*], [], [], [], [], [], [], [],\n\t",
  "[#h(1em)Asthma],",
  "[",
  table [[which (table [,1] == "asthma") + 2, 2]],
  "], [",
  table [[which (table [,1] == "asthma") + 2, 3]],
  "], [",
  table [[which (table [,1] == "asthma") + 2, 4]],
  "], [",
  table [[which (table [,1] == "asthma") + 2, 5]],
  "], [",
  table [[which (table [,1] == "asthma") + 2, 6]],
  "], [",
  table [[which (table [,1] == "asthma") + 2, 7]],
  "], [",
  table [[which (table [,1] == "asthma") + 2, 8]], 
  "],\n\t",
  "[#h(1em)Bronchiectasis],",
  "[",
  table [[which (table [,1] == "bronchiectasis") + 2, 2]],
  "], [",
  table [[which (table [,1] == "bronchiectasis") + 2, 3]],
  "], [",
  table [[which (table [,1] == "bronchiectasis") + 2, 4]],
  "], [",
  table [[which (table [,1] == "bronchiectasis") + 2, 5]],
  "], [",
  table [[which (table [,1] == "bronchiectasis") + 2, 6]],
  "], [",
  table [[which (table [,1] == "bronchiectasis") + 2, 7]],
  "], [",
  table [[which (table [,1] == "bronchiectasis") + 2, 8]], 
  "],\n\t",
  "[#h(1em)Chest Wall Disorder],",
  "[",
  table [[which (table [,1] == "chest_wall") + 2, 2]],
  "], [",
  table [[which (table [,1] == "chest_wall") + 2, 3]],
  "], [",
  table [[which (table [,1] == "chest_wall") + 2, 4]],
  "], [",
  table [[which (table [,1] == "chest_wall") + 2, 5]],
  "], [",
  table [[which (table [,1] == "chest_wall") + 2, 6]],
  "], [",
  table [[which (table [,1] == "chest_wall") + 2, 7]],
  "], [",
  table [[which (table [,1] == "chest_wall") + 2, 8]], 
  "],\n\t",    
  "[#h(1em)Chronic Obstructive Pulmonary Disease],",
  "[",
  table [[which (table [,1] == "copd") + 2, 2]],
  "], [",
  table [[which (table [,1] == "copd") + 2, 3]],
  "], [",
  table [[which (table [,1] == "copd") + 2, 4]],
  "], [",
  table [[which (table [,1] == "copd") + 2, 5]],
  "], [",
  table [[which (table [,1] == "copd") + 2, 6]],
  "], [",
  table [[which (table [,1] == "copd") + 2, 7]],
  "], [",
  table [[which (table [,1] == "copd") + 2, 8]], 
  "],\n\t",
  "[#h(1em)Interstitial Lung Disease],",
  "[",
  table [[which (table [,1] == "ild") + 2, 2]],
  "], [",
  table [[which (table [,1] == "ild") + 2, 3]],
  "], [",
  table [[which (table [,1] == "ild") + 2, 4]],
  "], [",
  table [[which (table [,1] == "ild") + 2, 5]],
  "], [",
  table [[which (table [,1] == "ild") + 2, 6]],
  "], [",
  table [[which (table [,1] == "ild") + 2, 7]],
  "], [",
  table [[which (table [,1] == "ild") + 2, 8]], 
  "],\n\t",
  "[#h(1em)Neuromuscular Disease],",
  "[",
  table [[which (table [,1] == "neuromuscular") + 2, 2]],
  "], [",
  table [[which (table [,1] == "neuromuscular") + 2, 3]],
  "], [",
  table [[which (table [,1] == "neuromuscular") + 2, 4]],
  "], [",
  table [[which (table [,1] == "neuromuscular") + 2, 5]],
  "], [",
  table [[which (table [,1] == "neuromuscular") + 2, 6]],
  "], [",
  table [[which (table [,1] == "neuromuscular") + 2, 7]],
  "], [",
  table [[which (table [,1] == "neuromuscular") + 2, 8]], 
  "],\n\t",
  "[#h(1em)None],",
  "[",
  table [[which (table [,1] == "none") + 2, 2]],
  "], [",
  table [[which (table [,1] == "none") + 2, 3]],
  "], [",
  table [[which (table [,1] == "none") + 2, 4]],
  "], [",
  table [[which (table [,1] == "none") + 2, 5]],
  "], [",
  table [[which (table [,1] == "none") + 2, 6]],
  "], [",
  table [[which (table [,1] == "none") + 2, 7]],
  "], [",
  table [[which (table [,1] == "none") + 2, 8]], 
  "],\n\t",
  "[*Computed Tomography*], [], [], [], [], [], [], [],\n\t",
  "[#h(1em)Chronic Obstructive Pulmonary Disease],",
  "[",
  table [[which (table [,1] == "copd_imaging") + 2, 2]],
  "], [",
  table [[which (table [,1] == "copd_imaging") + 2, 3]],
  "], [",
  table [[which (table [,1] == "copd_imaging") + 2, 4]],
  "], [",
  table [[which (table [,1] == "copd_imaging") + 2, 5]],
  "], [",
  table [[which (table [,1] == "copd_imaging") + 2, 6]],
  "], [",
  table [[which (table [,1] == "copd_imaging") + 2, 7]],
  "], [",
  table [[which (table [,1] == "copd_imaging") + 2, 8]], 
  "],\n\t",
  "[#h(1em)Interstitial Lung Disease],",
  "[",
  table [[which (table [,1] == "ild_imaging") + 2, 2]],
  "], [",
  table [[which (table [,1] == "ild_imaging") + 2, 3]],
  "], [",
  table [[which (table [,1] == "ild_imaging") + 2, 4]],
  "], [",
  table [[which (table [,1] == "ild_imaging") + 2, 5]],
  "], [",
  table [[which (table [,1] == "ild_imaging") + 2, 6]],
  "], [",
  table [[which (table [,1] == "ild_imaging") + 2, 7]],
  "], [",
  table [[which (table [,1] == "ild_imaging") + 2, 8]], 
  "],\n\t",  
  "[*Dynamic Lung Volumes, z-score*], [], [], [], [], [], [], [],\n\t",
  "[#h(1em)FEV#sub[1]],",
  "[",
  table [[which (table [,1] == "fev1_z_score"), 2]],
  "], [",
  table [[which (table [,1] == "fev1_z_score"), 3]],
  "], [",
  table [[which (table [,1] == "fev1_z_score"), 4]],
  "], [",
  table [[which (table [,1] == "fev1_z_score"), 5]],
  "], [",
  table [[which (table [,1] == "fev1_z_score"), 6]],
  "], [",
  table [[which (table [,1] == "fev1_z_score"), 7]],
  "], [",
  table [[which (table [,1] == "fev1_z_score"), 8]], 
  "],\n\t",
  "[#h(1em)FVC],",
  "[",
  table [[which (table [,1] == "fvc_z_score"), 2]],
  "], [",
  table [[which (table [,1] == "fvc_z_score"), 3]],
  "], [",
  table [[which (table [,1] == "fvc_z_score"), 4]],
  "], [",
  table [[which (table [,1] == "fvc_z_score"), 5]],
  "], [",
  table [[which (table [,1] == "fvc_z_score"), 6]],
  "], [",
  table [[which (table [,1] == "fvc_z_score"), 7]],
  "], [",
  table [[which (table [,1] == "fvc_z_score"), 8]], 
  "],\n\t",
  "[#h(1em)FEV#sub[1]/FVC],",
  "[",
  table [[which (table [,1] == "fev1_fvc_z_score"), 2]],
  "], [",
  table [[which (table [,1] == "fev1_fvc_z_score"), 3]],
  "], [",
  table [[which (table [,1] == "fev1_fvc_z_score"), 4]],
  "], [",
  table [[which (table [,1] == "fev1_fvc_z_score"), 5]],
  "], [",
  table [[which (table [,1] == "fev1_fvc_z_score"), 6]],
  "], [",
  table [[which (table [,1] == "fev1_fvc_z_score"), 7]],
  "], [",
  table [[which (table [,1] == "fev1_fvc_z_score"), 8]], 
  "],\n\t",
  "[*Static Lung Volumes, z-score*], [], [], [], [], [], [], [],\n\t",
  "[#h(1em)TLC],",
  "[",
  table [[which (table [,1] == "tlc_z_score"), 2]],
  "], [",
  table [[which (table [,1] == "tlc_z_score"), 3]],
  "], [",
  table [[which (table [,1] == "tlc_z_score"), 4]],
  "], [",
  table [[which (table [,1] == "tlc_z_score"), 5]],
  "], [",
  table [[which (table [,1] == "tlc_z_score"), 6]],
  "], [",
  table [[which (table [,1] == "tlc_z_score"), 7]],
  "], [",
  table [[which (table [,1] == "tlc_z_score"), 8]], 
  "],\n\t",
  "[#h(1em)RV],",
  "[",
  table [[which (table [,1] == "rv_z_score"), 2]],
  "], [",
  table [[which (table [,1] == "rv_z_score"), 3]],
  "], [",
  table [[which (table [,1] == "rv_z_score"), 4]],
  "], [",
  table [[which (table [,1] == "rv_z_score"), 5]],
  "], [",
  table [[which (table [,1] == "rv_z_score"), 6]],
  "], [",
  table [[which (table [,1] == "rv_z_score"), 7]],
  "], [",
  table [[which (table [,1] == "rv_z_score"), 8]], 
  "],\n\t",
  "[*Diffusing Capacity, z-score*], [], [], [], [], [], [], [],\n\t",
  "[#h(1em)D#sub[LCO]],",
  "[",
  table [[which (table [,1] == "dlco_z_score"), 2]],
  "], [",
  table [[which (table [,1] == "dlco_z_score"), 3]],
  "], [",
  table [[which (table [,1] == "dlco_z_score"), 4]],
  "], [",
  table [[which (table [,1] == "dlco_z_score"), 5]],
  "], [",
  table [[which (table [,1] == "dlco_z_score"), 6]],
  "], [",
  table [[which (table [,1] == "dlco_z_score"), 7]],
  "], [",
  table [[which (table [,1] == "dlco_z_score"), 8]], 
  "],\n\t",
  "[#h(1em)K#sub[CO]],",
  "[",
  table [[which (table [,1] == "kco_z_score"), 2]],
  "], [",
  table [[which (table [,1] == "kco_z_score"), 3]],
  "], [",
  table [[which (table [,1] == "kco_z_score"), 4]],
  "], [",
  table [[which (table [,1] == "kco_z_score"), 5]],
  "], [",
  table [[which (table [,1] == "kco_z_score"), 6]],
  "], [",
  table [[which (table [,1] == "kco_z_score"), 7]],
  "], [",
  table [[which (table [,1] == "kco_z_score"), 8]], 
  "],\n\t",
  "table.hline()\n",
  ")",
  sep = ""
)

write_lines (table, "../tables/table-1.txt")

################################################################################
## Table 2
################################################################################

data <- pfts %>%
  filter (fvc_z_score >= -1.645 & fev1_fvc_z_score >= -1.645)

# Unadjusted

model_age_unadjusted <- tidy (glm (
  formula = restriction ~ age,
  family = "binomial",
  data = data), conf.int = TRUE, exponentiate = TRUE
)

model_sex_unadjusted <- tidy (glm (
  formula = restriction ~ sex,
  family = "binomial",
  data = data), conf.int = TRUE, exponentiate = TRUE
)

model_bmi_unadjusted <- tidy (glm (
  formula = restriction ~ bmi,
  family = "binomial",
  data = data), conf.int = TRUE, exponentiate = TRUE
)

model_race_unadjusted <- tidy (glm (
  formula = restriction ~ race,
  family = "binomial",
  data = data), conf.int = TRUE, exponentiate = TRUE
)

model_ethnicity_unadjusted <- tidy (glm (
  formula = restriction ~ ethnicity,
  family = "binomial",
  data = data), conf.int = TRUE, exponentiate = TRUE
)

model_pack_years_unadjusted <- tidy (glm (
  formula = restriction ~ pack_years,
  family = "binomial",
  data = data), conf.int = TRUE, exponentiate = TRUE
)

model_cough_unadjusted <- tidy (glm (
  formula = restriction ~ cough,
  family = "binomial",
  data = data), conf.int = TRUE, exponentiate = TRUE
)

model_dyspnea_unadjusted <- tidy (glm (
  formula = restriction ~ dyspnea,
  family = "binomial",
  data = data), conf.int = TRUE, exponentiate = TRUE
)

model_wheeze_unadjusted <- tidy (glm (
  formula = restriction ~ wheeze,
  family = "binomial",
  data = data), conf.int = TRUE, exponentiate = TRUE
)

model_asthma_unadjusted <- tidy (glm (
  formula = restriction ~ asthma,
  family = "binomial",
  data = data), conf.int = TRUE, exponentiate = TRUE
)

model_bronchiectasis_unadjusted <- tidy (glm (
  formula = restriction ~ bronchiectasis,
  family = "binomial",
  data = data), conf.int = TRUE, exponentiate = TRUE
)

model_chest_wall_unadjusted <- tidy (glm (
  formula = restriction ~ chest_wall,
  family = "binomial",
  data = data), conf.int = TRUE, exponentiate = TRUE
)

model_copd_unadjusted <- tidy (glm (
  formula = restriction ~ copd,
  family = "binomial",
  data = data), conf.int = TRUE, exponentiate = TRUE
)

model_ild_unadjusted <- tidy (glm (
  formula = restriction ~ ild,
  family = "binomial",
  data = data), conf.int = TRUE, exponentiate = TRUE
)

model_neuromuscular_unadjusted <- tidy (glm (
  formula = restriction ~ neuromuscular,
  family = "binomial",
  data = data), conf.int = TRUE, exponentiate = TRUE
)

model_none_unadjusted <- tidy (glm (
  formula = restriction ~ none,
  family = "binomial",
  data = data), conf.int = TRUE, exponentiate = TRUE
)

model_copd_imaging_unadjusted <- tidy (glm (
  formula = restriction ~ copd_imaging,
  family = "binomial",
  data = data), conf.int = TRUE, exponentiate = TRUE
)

model_ild_imaging_unadjusted <- tidy (glm (
  formula = restriction ~ ild_imaging,
  family = "binomial",
  data = data), conf.int = TRUE, exponentiate = TRUE
)

model_fev1_unadjusted <- tidy (glm (
  formula = restriction ~ fev1_z_score,
  family = "binomial",
  data = data), conf.int = TRUE, exponentiate = TRUE
)

model_fvc_unadjusted <- tidy (glm (
  formula = restriction ~ fvc_z_score,
  family = "binomial",
  data = data), conf.int = TRUE, exponentiate = TRUE
)

model_fev1_fvc_unadjusted <- tidy (glm (
  formula = restriction ~ fev1_fvc_z_score,
  family = "binomial",
  data = data), conf.int = TRUE, exponentiate = TRUE
)

model_response_unadjusted <- tidy (glm (
  formula = restriction ~ response,
  family = "binomial",
  data = data), conf.int = TRUE, exponentiate = TRUE
)

model_effort_unadjusted <- tidy (glm (
  formula = restriction ~ effort,
  family = "binomial",
  data = data), conf.int = TRUE, exponentiate = TRUE
)

model_rv_unadjusted <- tidy (glm (
  formula = restriction ~ rv_z_score,
  family = "binomial",
  data = data), conf.int = TRUE, exponentiate = TRUE
)

model_dlco_unadjusted <- tidy (glm (
  formula = restriction ~ dlco_z_score,
  family = "binomial",
  data = data), conf.int = TRUE, exponentiate = TRUE
)

model_kco_unadjusted <- tidy (glm (
  formula = restriction ~ kco_z_score,
  family = "binomial",
  data = data), conf.int = TRUE, exponentiate = TRUE
)

# Adjusted

model_age_adjusted <- tidy (glm (
  formula = restriction ~ age + sex + fev1_z_score + fvc_z_score +
    fev1_fvc_z_score,
  family = "binomial",
  data = data), conf.int = TRUE, exponentiate = TRUE
)

model_sex_adjusted <- tidy (glm (
  formula = restriction ~ sex + age + fev1_z_score + fvc_z_score +
    fev1_fvc_z_score,
  family = "binomial",
  data = data), conf.int = TRUE, exponentiate = TRUE
)

model_bmi_adjusted <- tidy (glm (
  formula = restriction ~ bmi + age + sex + fev1_z_score + fvc_z_score +
    fev1_fvc_z_score,
  family = "binomial",
  data = data), conf.int = TRUE, exponentiate = TRUE
)

model_race_adjusted <- tidy (glm (
  formula = restriction ~ race + age + sex + fev1_z_score + fvc_z_score +
    fev1_fvc_z_score,
  family = "binomial",
  data = data), conf.int = TRUE, exponentiate = TRUE
)

model_ethnicity_adjusted <- tidy (glm (
  formula = restriction ~ ethnicity + age + sex + fev1_z_score + fvc_z_score +
    fev1_fvc_z_score,
  family = "binomial",
  data = data), conf.int = TRUE, exponentiate = TRUE
)

model_pack_years_adjusted <- tidy (glm (
  formula = restriction ~ pack_years + age + sex + fev1_z_score + fvc_z_score +
    fev1_fvc_z_score,
  family = "binomial",
  data = data), conf.int = TRUE, exponentiate = TRUE
)

model_cough_adjusted <- tidy (glm (
  formula = restriction ~ cough + age + sex + fev1_z_score + fvc_z_score +
    fev1_fvc_z_score,
  family = "binomial",
  data = data), conf.int = TRUE, exponentiate = TRUE
)

model_dyspnea_adjusted <- tidy (glm (
  formula = restriction ~ dyspnea + age + sex + fev1_z_score + fvc_z_score +
    fev1_fvc_z_score,
  family = "binomial",
  data = data), conf.int = TRUE, exponentiate = TRUE
)

model_wheeze_adjusted <- tidy (glm (
  formula = restriction ~ wheeze + age + sex + fev1_z_score + fvc_z_score +
    fev1_fvc_z_score,
  family = "binomial",
  data = data), conf.int = TRUE, exponentiate = TRUE
)

model_asthma_adjusted <- tidy (glm (
  formula = restriction ~ asthma + age + sex + fev1_z_score + fvc_z_score +
    fev1_fvc_z_score,
  family = "binomial",
  data = data), conf.int = TRUE, exponentiate = TRUE
)

model_bronchiectasis_adjusted <- tidy (glm (
  formula = restriction ~ bronchiectasis + age + sex + fev1_z_score + fvc_z_score +
    fev1_fvc_z_score,
  family = "binomial",
  data = data), conf.int = TRUE, exponentiate = TRUE
)

model_chest_wall_adjusted <- tidy (glm (
  formula = restriction ~ chest_wall + age + sex + fev1_z_score + fvc_z_score +
    fev1_fvc_z_score,
  family = "binomial",
  data = data), conf.int = TRUE, exponentiate = TRUE
)

model_copd_adjusted <- tidy (glm (
  formula = restriction ~ copd + age + sex + fev1_z_score + fvc_z_score +
    fev1_fvc_z_score,
  family = "binomial",
  data = data), conf.int = TRUE, exponentiate = TRUE
)

model_ild_adjusted <- tidy (glm (
  formula = restriction ~ ild + age + sex + fev1_z_score + fvc_z_score +
    fev1_fvc_z_score,
  family = "binomial",
  data = data), conf.int = TRUE, exponentiate = TRUE
)

model_neuromuscular_adjusted <- tidy (glm (
  formula = restriction ~ neuromuscular + age + sex + fev1_z_score + fvc_z_score +
    fev1_fvc_z_score,
  family = "binomial",
  data = data), conf.int = TRUE, exponentiate = TRUE
)

model_none_adjusted <- tidy (glm (
  formula = restriction ~ none + age + sex + fev1_z_score + fvc_z_score +
    fev1_fvc_z_score,
  family = "binomial",
  data = data), conf.int = TRUE, exponentiate = TRUE
)

model_copd_imaging_adjusted <- tidy (glm (
  formula = restriction ~ copd_imaging + age + sex  + fev1_z_score + fvc_z_score +
    fev1_fvc_z_score,
  family = "binomial",
  data = data), conf.int = TRUE, exponentiate = TRUE
)

model_ild_imaging_adjusted <- tidy (glm (
  formula = restriction ~ ild_imaging + age + sex + fev1_z_score + fvc_z_score +
    fev1_fvc_z_score,
  family = "binomial",
  data = data), conf.int = TRUE, exponentiate = TRUE
)

model_fev1_adjusted <- tidy (glm (
  formula = restriction ~ fev1_z_score + age + sex + fvc_z_score +
    fev1_fvc_z_score,
  family = "binomial",
  data = data), conf.int = TRUE, exponentiate = TRUE
)

model_fvc_adjusted <- tidy (glm (
  formula = restriction ~ fvc_z_score + age + sex + fev1_z_score +
    fev1_fvc_z_score,
  family = "binomial",
  data = data), conf.int = TRUE, exponentiate = TRUE
)

model_fev1_fvc_adjusted <- tidy (glm (
  formula = restriction ~ fev1_fvc_z_score + age + sex + fev1_z_score +
    fvc_z_score,
  family = "binomial",
  data = data), conf.int = TRUE, exponentiate = TRUE
)

model_response_adjusted <- tidy (glm (
  formula = restriction ~ response + age + sex + fev1_z_score + fvc_z_score +
    fev1_fvc_z_score,
  family = "binomial",
  data = data), conf.int = TRUE, exponentiate = TRUE
)

model_effort_adjusted <- tidy (glm (
  formula = restriction ~ effort + age + sex + fev1_z_score + fvc_z_score +
    fev1_fvc_z_score,
  family = "binomial",
  data = data), conf.int = TRUE, exponentiate = TRUE
)

model_rv_adjusted <- tidy (glm (
  formula = restriction ~ rv_z_score + age + sex + fev1_z_score + fvc_z_score +
    fev1_fvc_z_score,
  family = "binomial",
  data = data), conf.int = TRUE, exponentiate = TRUE
)

model_dlco_adjusted <- tidy (glm (
  formula = restriction ~ dlco_z_score + age + sex + fev1_z_score + fvc_z_score +
    fev1_fvc_z_score,
  family = "binomial",
  data = data), conf.int = TRUE, exponentiate = TRUE
)

model_kco_adjusted <- tidy (glm (
  formula = restriction ~ kco_z_score + age + sex + fev1_z_score + fvc_z_score +
    fev1_fvc_z_score,
  family = "binomial",
  data = data), conf.int = TRUE, exponentiate = TRUE
)

table <- paste (
  "#table(\n\t",
	"inset: (x: 4pt, y: 2.5pt),\n\t",
  "columns: 6,\n\t",
	"align: (left, center, right, center, center, right),\n\t",
	"table.hline(),\n\t",
  "[], table.cell(colspan: 5, [*Association with Restriction*]),\n\t",
  "table.hline(start:1, end:6),\n\t",
	"[], table.cell(colspan: 2, [*Unadjusted*]), [], table.cell(colspan: 2,[*Adjusted#super[a]*]),\n\t",
	"table.hline(start:1, end:3), table.hline(start:4, end:6),\n\t",
	"[*Characteristic*], [*Odds Ratio (95% CI)*], [*$P$ Value*], [], [*Odds Ratio (95% CI)*], [*$P$ Value*],\n\t",
	"table.hline(),\n\t",
  "[*Age, yr*], [",
  format (round (model_age_unadjusted [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_age_unadjusted [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_age_unadjusted [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  print_p_value (model_age_unadjusted [[2, "p.value"]]),
  "], [], [",
  format (round (model_age_adjusted [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_age_adjusted [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_age_adjusted [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  print_p_value (model_age_adjusted [[2, "p.value"]]),
  "],\n\t",
  "[*Sex*], [], [], [], [], [],\n\t",
  "[#h(1em)Male], [1.00 (Reference)], [---], [], [1.00 (Reference)], [---],\n\t",
  "[#h(1em)Female], [",
  format (round (model_sex_unadjusted [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_sex_unadjusted [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_sex_unadjusted [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  print_p_value (model_sex_unadjusted [[2, "p.value"]]),
  "], [], [",
  format (round (model_sex_adjusted [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_sex_adjusted [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_sex_adjusted [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  print_p_value (model_sex_adjusted [[2, "p.value"]]),  
  "],\n\t",  
  "[*Race*], [], [], [], [], [],\n\t",
  "[#h(1em)White], [1.00 (Reference)], [---], [], [1.00 (Reference)], [---],\n\t",
  "[#h(1em)Black], [",
  format (round (model_race_unadjusted [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_race_unadjusted [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_race_unadjusted [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  print_p_value (model_race_unadjusted [[2, "p.value"]]),
  "], [], [",
  format (round (model_race_adjusted [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_race_adjusted [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_race_adjusted [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  print_p_value (model_race_adjusted [[2, "p.value"]]),  
  "],\n\t",
  "[#h(1em)Asian], [",
  format (round (model_race_unadjusted [[3, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_race_unadjusted [[3, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_race_unadjusted [[3, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  print_p_value (model_race_unadjusted [[3, "p.value"]]),
  "], [], [",
  format (round (model_race_adjusted [[3, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_race_adjusted [[3, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_race_adjusted [[3, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  print_p_value (model_race_adjusted [[3, "p.value"]]),  
  "],\n\t",  
  "[#h(1em)Other], [",
  format (round (model_race_unadjusted [[4, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_race_unadjusted [[4, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_race_unadjusted [[4, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  print_p_value (model_race_unadjusted [[4, "p.value"]]),
  "], [], [",
  format (round (model_race_adjusted [[4, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_race_adjusted [[4, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_race_adjusted [[4, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  print_p_value (model_race_adjusted [[4, "p.value"]]),  
  "],\n\t",
  "[*Ethnicity*], [], [], [], [], [],\n\t",
  "[#h(1em)Hispanic], [1.00 (Reference)], [---], [], [1.00 (Reference)], [---],\n\t",
  "[#h(1em)Not Hispanic], [",
  format (round (model_ethnicity_unadjusted [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_ethnicity_unadjusted [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_ethnicity_unadjusted [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  print_p_value (model_ethnicity_unadjusted [[2, "p.value"]]),
  "], [], [",
  format (round (model_ethnicity_adjusted [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_ethnicity_adjusted [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_ethnicity_adjusted [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  print_p_value (model_ethnicity_adjusted [[2, "p.value"]]),  
  "],\n\t",
  "[*BMI, kg/m#super[2]*], [",
  format (round (model_bmi_unadjusted [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_bmi_unadjusted [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_bmi_unadjusted [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  print_p_value (model_bmi_unadjusted [[2, "p.value"]]),
  "], [], [",
  format (round (model_bmi_adjusted [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_bmi_adjusted [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_bmi_adjusted [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  print_p_value (model_bmi_adjusted [[2, "p.value"]]),
  "],\n\t",    
  "[*Pack Years, yr*], [",
  format (round (model_pack_years_unadjusted [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_pack_years_unadjusted [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_pack_years_unadjusted [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  print_p_value (model_pack_years_unadjusted [[2, "p.value"]]),
  "], [], [",
  format (round (model_pack_years_adjusted [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_pack_years_adjusted [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_pack_years_adjusted [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  print_p_value (model_pack_years_adjusted [[2, "p.value"]]),
  "],\n\t",  
  "[*Respiratory Symptoms*], [], [], [], [], [],\n\t",
  "[#h(1em)Cough], [",
  format (round (model_cough_unadjusted [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_cough_unadjusted [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_cough_unadjusted [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  print_p_value (model_cough_unadjusted [[2, "p.value"]]),
  "], [], [",
  format (round (model_cough_adjusted [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_cough_adjusted [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_cough_adjusted [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  print_p_value (model_cough_adjusted [[2, "p.value"]]),  
  "],\n\t",
  "[#h(1em)Dyspnea], [",
  format (round (model_dyspnea_unadjusted [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_dyspnea_unadjusted [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_dyspnea_unadjusted [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  print_p_value (model_dyspnea_unadjusted [[2, "p.value"]]),
  "], [], [",
  format (round (model_dyspnea_adjusted [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_dyspnea_adjusted [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_dyspnea_adjusted [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  print_p_value (model_dyspnea_adjusted [[2, "p.value"]]),  
  "],\n\t",
  "[#h(1em)Wheeze], [",
  format (round (model_wheeze_unadjusted [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_wheeze_unadjusted [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_wheeze_unadjusted [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  print_p_value (model_wheeze_unadjusted [[2, "p.value"]]),
  "], [], [",
  format (round (model_wheeze_adjusted [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_wheeze_adjusted [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_wheeze_adjusted [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  print_p_value (model_wheeze_adjusted [[2, "p.value"]]),  
  "],\n\t",   
  "[*Respiratory Diseases*], [], [], [], [], [],\n\t",
  "[#h(1em)Asthma], [",
  format (round (model_asthma_unadjusted [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_asthma_unadjusted [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_asthma_unadjusted [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  print_p_value (model_asthma_unadjusted [[2, "p.value"]]),
  "], [], [",
  format (round (model_asthma_adjusted [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_asthma_adjusted [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_asthma_adjusted [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  print_p_value (model_asthma_adjusted [[2, "p.value"]]),  
  "],\n\t",
  "[#h(1em)Bronchiectasis], [",
  format (round (model_bronchiectasis_unadjusted [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_bronchiectasis_unadjusted [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_bronchiectasis_unadjusted [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  print_p_value (model_bronchiectasis_unadjusted [[2, "p.value"]]),
  "], [], [",
  format (round (model_bronchiectasis_adjusted [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_bronchiectasis_adjusted [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_bronchiectasis_adjusted [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  print_p_value (model_bronchiectasis_adjusted [[2, "p.value"]]),  
  "],\n\t",
  "[#h(1em)Chest Wall Disorder], [",
  format (round (model_chest_wall_unadjusted [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_chest_wall_unadjusted [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_chest_wall_unadjusted [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  print_p_value (model_chest_wall_unadjusted [[2, "p.value"]]),
  "], [], [",
  format (round (model_chest_wall_adjusted [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_chest_wall_adjusted [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_chest_wall_adjusted [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  print_p_value (model_chest_wall_adjusted [[2, "p.value"]]),  
  "],\n\t",
  "[#h(1em)Chronic Obstructive Pulmonary Disease], [",
  format (round (model_copd_unadjusted [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_copd_unadjusted [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_copd_unadjusted [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  print_p_value (model_copd_unadjusted [[2, "p.value"]]),
  "], [], [",
  format (round (model_copd_adjusted [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_copd_adjusted [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_copd_adjusted [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  print_p_value (model_copd_adjusted [[2, "p.value"]]),  
  "],\n\t",
  "[#h(1em)Interstitial Lung Disease], [",
  format (round (model_ild_unadjusted [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_ild_unadjusted [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_ild_unadjusted [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  print_p_value (model_ild_unadjusted [[2, "p.value"]]),
  "], [], [",
  format (round (model_ild_adjusted [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_ild_adjusted [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_ild_adjusted [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  print_p_value (model_ild_adjusted [[2, "p.value"]]),  
  "],\n\t",
  "[#h(1em)Neuromuscular Disease], [",
  format (round (model_neuromuscular_unadjusted [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_neuromuscular_unadjusted [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_neuromuscular_unadjusted [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  print_p_value (model_neuromuscular_unadjusted [[2, "p.value"]]),
  "], [], [",
  format (round (model_neuromuscular_adjusted [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_neuromuscular_adjusted [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_neuromuscular_adjusted [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  print_p_value (model_neuromuscular_adjusted [[2, "p.value"]]),  
  "],\n\t",  
  "[#h(1em)None], [",
  format (round (model_none_unadjusted [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_none_unadjusted [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_none_unadjusted [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  print_p_value (model_none_unadjusted [[2, "p.value"]]),
  "], [], [",
  format (round (model_none_adjusted [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_none_adjusted [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_none_adjusted [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  print_p_value (model_none_adjusted [[2, "p.value"]]),  
  "],\n\t",    
  "[*Computed Tomography*], [], [], [], [], [],\n\t",
  "[#h(1em)Chronic Obstructive Pulmonary Disease], [",
  format (round (model_copd_imaging_unadjusted [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_copd_imaging_unadjusted [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_copd_imaging_unadjusted [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  print_p_value (model_copd_imaging_unadjusted [[2, "p.value"]]),
  "], [], [",
  format (round (model_copd_imaging_adjusted [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_copd_imaging_adjusted [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_copd_imaging_adjusted [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  print_p_value (model_copd_imaging_adjusted [[2, "p.value"]]),  
  "],\n\t",
  "[#h(1em)Interstitial Lung Disease], [",
  format (round (model_ild_imaging_unadjusted [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_ild_imaging_unadjusted [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_ild_imaging_unadjusted [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  print_p_value (model_ild_imaging_unadjusted [[2, "p.value"]]),
  "], [], [",
  format (round (model_ild_imaging_adjusted [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_ild_imaging_adjusted [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_ild_imaging_adjusted [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  print_p_value (model_ild_imaging_adjusted [[2, "p.value"]]),  
  "],\n\t",     
  "[*Dynamic Lung Volumes*], [], [], [], [], [],\n\t",
  "[#h(1em)FEV#sub[1], z-score], [",
  format (round (model_fev1_unadjusted [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_fev1_unadjusted [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_fev1_unadjusted [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  print_p_value (model_fev1_unadjusted [[2, "p.value"]]),
  "], [], [",
  format (round (model_fev1_adjusted [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_fev1_adjusted [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_fev1_adjusted [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  print_p_value (model_fev1_adjusted [[2, "p.value"]]),  
  "],\n\t",
  "[#h(1em)FVC, z-score], [",
  format (round (model_fvc_unadjusted [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_fvc_unadjusted [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_fvc_unadjusted [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  print_p_value (model_fvc_unadjusted [[2, "p.value"]]),
  "], [], [",
  format (round (model_fvc_adjusted [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_fvc_adjusted [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_fvc_adjusted [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  print_p_value (model_fvc_adjusted [[2, "p.value"]]),  
  "],\n\t",
  "[#h(1em)FEV#sub[1]/FVC, z-score], [",
  format (round (model_fev1_fvc_unadjusted [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_fev1_fvc_unadjusted [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_fev1_fvc_unadjusted [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  print_p_value (model_fev1_fvc_unadjusted [[2, "p.value"]]),
  "], [], [",
  format (round (model_fev1_fvc_adjusted [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_fev1_fvc_adjusted [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_fev1_fvc_adjusted [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  print_p_value (model_fev1_fvc_adjusted [[2, "p.value"]]),  
  "],\n\t",
  "[#h(1em)Bronchodilator Response], [",
  format (round (model_response_unadjusted [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_response_unadjusted [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_response_unadjusted [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  print_p_value (model_response_unadjusted [[2, "p.value"]]),
  "], [], [",
  format (round (model_response_adjusted [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_response_adjusted [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_response_adjusted [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  print_p_value (model_response_adjusted [[2, "p.value"]]),  
  "],\n\t",
  "[#h(1em)Adequate Effort], [",
  format (round (model_effort_unadjusted [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_effort_unadjusted [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_effort_unadjusted [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  print_p_value (model_effort_unadjusted [[2, "p.value"]]),
  "], [], [",
  format (round (model_effort_adjusted [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_effort_adjusted [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_effort_adjusted [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  print_p_value (model_effort_adjusted [[2, "p.value"]]),  
  "],\n\t",     
  "[*Static Lung Volumes*], [], [], [], [], [],\n\t",
  "[#h(1em)RV, z-score], [",
  format (round (model_rv_unadjusted [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_rv_unadjusted [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_rv_unadjusted [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  print_p_value (model_rv_unadjusted [[2, "p.value"]]),
  "], [], [",
  format (round (model_rv_adjusted [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_rv_adjusted [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_rv_adjusted [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  print_p_value (model_rv_adjusted [[2, "p.value"]]),  
  "],\n\t",      
  "[*Diffusing Capacity*], [], [], [], [], [],\n\t",
  "[#h(1em)D#sub[LCO], z-score], [",
  format (round (model_dlco_unadjusted [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_dlco_unadjusted [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_dlco_unadjusted [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  print_p_value (model_dlco_unadjusted [[2, "p.value"]]),
  "], [], [",
  format (round (model_dlco_adjusted [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_dlco_adjusted [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_dlco_adjusted [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  print_p_value (model_dlco_adjusted [[2, "p.value"]]),  
  "],\n\t",
  "[#h(1em)K#sub[CO], z-score], [",
  format (round (model_kco_unadjusted [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_kco_unadjusted [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_kco_unadjusted [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  print_p_value (model_kco_unadjusted [[2, "p.value"]]),
  "], [], [",
  format (round (model_kco_adjusted [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_kco_adjusted [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_kco_adjusted [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  print_p_value (model_kco_adjusted [[2, "p.value"]]),  
  "],\n\t",
  "table.hline()\n",
  ")", 
  sep = ""
)  

write_lines (table, "../tables/table-2.txt")

################################################################################
## Table 3
################################################################################

# ED visit with respiratory symptoms

data <- pfts %>% 
  filter (fev1_fvc_z_score >= -1.645 & fvc_z_score >= -1.645) %>% 
  mutate (event = case_when (
    is.na (date_ed) == 0 ~ 1,
    dead == 1 ~ 2,
    TRUE ~ 0)
  ) %>%
  mutate (time = case_when (
    event == 1 ~ interval (date, date_ed) %/% months (1),
    TRUE ~ interval (date, date_last) %/% months (1))
  ) %>% 
  select (
    age,
    sex,
    fev1_z_score,
    fvc_z_score,
    fev1_fvc_z_score,
    restriction,
    event,
    time
  ) %>% 
  drop_na () %>% 
  mutate (time = pmax (time, 0))

model_ed_unadjusted <- tidy (
  CSC (
    Hist (time, event) ~ restriction,
    data = data
  )$models[[1]],
  conf.int = TRUE,
  exponentiate = TRUE
)

model_ed_adjusted <- tidy (
  CSC (
    Hist (time, event) ~ restriction + age + sex + fev1_z_score + fvc_z_score + 
      fev1_fvc_z_score,
    data = data
  )$models[[1]],
  conf.int = TRUE,
  exponentiate = TRUE
)

# Death

data <- pfts %>%
  filter (fev1_fvc_z_score >= -1.645 & fvc_z_score >= -1.645) %>%
  mutate (time = interval (date, date_last) %/% months (1)) %>%
  filter (time > 0) %>%
  select (
    age,
    sex,
    race,
    fev1_z_score,
    fvc_z_score,
    fev1_fvc_z_score,
    restriction,
    event = dead,
    time
  ) %>% 
  drop_na ()

model_death_unadjusted <- tidy (
  coxph (Surv (time, event) ~ restriction, data = data, x = TRUE),
  conf.int = TRUE,
  exponentiate = TRUE
)

model_death_adjusted <- tidy (
  coxph (Surv (time, event) ~ restriction + age + sex + fev1_z_score +
    fvc_z_score + fev1_fvc_z_score, data = data, x = TRUE),
  conf.int = TRUE,
  exponentiate = TRUE
)

table <- paste (
  "#table(\n\t",
	"inset: (x: 4pt, y: 3pt),\n\t",
  "columns: 6,\n\t",
	"align: (left, center, right, center, center, right),\n\t",
	"table.hline(),\n\t",
  "[], table.cell(colspan: 5, [*Association with Restriction*]),\n\t",
  "table.hline(start:1, end:6),\n\t",
	"[], table.cell(colspan: 2, [*Unadjusted*]), [], table.cell(colspan: 2,[*Adjusted#super[a]*]),\n\t",
	"table.hline(start:1, end:3), table.hline(start:4, end:6),\n\t",
	"[*Event*], [*Hazard Ratio (95% CI)*], [*$P$ Value*], [], [*Hazard Ratio (95% CI)*], [*$P$ Value*],\n\t",
	"table.hline(),\n\t",
  "[Emergency Department Visit with], table.cell(rowspan: 2, align: horizon)[",
  format (round (model_ed_unadjusted [[1, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_ed_unadjusted [[1, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_ed_unadjusted [[1, "conf.high"]], digits = 2), nsmall = 2),
  ")], table.cell(rowspan: 2, align: horizon)[",
  print_p_value (model_ed_unadjusted [[1, "p.value"]]),
  "], [], table.cell(rowspan: 2, align: horizon)[",
  format (round (model_ed_adjusted [[1, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_ed_adjusted [[1, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_ed_adjusted [[1, "conf.high"]], digits = 2), nsmall = 2),
  ")], table.cell(rowspan: 2, align: horizon)[",
  print_p_value (model_ed_adjusted [[1, "p.value"]]),
  "],\n\t",
  "[#h(1em)Respiratory Complaint], [],\n\t",
  "[Death from Any Cause], [",
  format (round (model_death_unadjusted [[1, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_death_unadjusted [[1, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_death_unadjusted [[1, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  print_p_value (model_death_unadjusted [[1, "p.value"]]),
  "], [], [",
  format (round (model_death_adjusted [[1, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_death_adjusted [[1, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_death_adjusted [[1, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  print_p_value (model_death_adjusted [[1, "p.value"]]),
  "],\n\t",
  "table.hline()\n",
  ")", 
  sep = ""
)  

write_lines (table, "../tables/table-3.txt")

################################################################################
## e-table 12 - Baseline Characteristics by Age
################################################################################

data <- pfts %>%
  mutate (spirometry = case_when (
    interpretation == "Normal" ~ 1,
    interpretation == "Non-Specific" ~ 2,
    interpretation == "Obstructive" ~ 3,
    interpretation == "Restrictive with Normal Spirometry" ~ 4,
    interpretation == "Restrictive with Abnormal Spirometry" ~ 5,
    interpretation == "Mixed" ~ 6)
  ) %>%
  mutate (spirometry = factor (
    spirometry,
    levels = c ("1", "2", "3", "4", "5", "6"))
  ) %>% 
  mutate (race = factor (
    race,
    levels = c ("1", "2", "3", "4", "5"))
  ) %>% 
  mutate (copd_imaging = as.factor (copd_imaging)) %>% 
  mutate (ild_imaging = as.factor (ild_imaging)) %>% 
  mutate (age_group = case_when (
    age >= 18 & age <= 40 ~ 1,
    age >= 41 & age <= 64 ~ 2,
    age >= 65 & age <= 80 ~ 3)
  ) %>%
  mutate (age_group = factor (
    age_group,
    levels = c ("1", "2", "3"))
  )

table <- table1 (~
  sex +
  race +
  ethnicity +
  bmi +
  pack_years +
  cough +
  dyspnea +
  wheeze +
  asthma +
  bronchiectasis +
  chest_wall +
  copd +
  ild +
  neuromuscular +
  none +
  copd_imaging +
  ild_imaging +    
  spirometry +
  fev1_z_score +
  fvc_z_score +
  fev1_fvc_z_score +
  tlc_z_score +
  rv_z_score +
  dlco_z_score +
  kco_z_score |
  age_group,
  data = data,
  big.mark =",",
  render.categorical = render_categorical,
  render.continuous = render_continuous
)

table <- as.data.frame (table)

table <- paste (
  "#table(\n\t",
  "inset: (x: 10pt, y: 3pt),\n\t",
  "columns: 4,\n\t",
  "align: (left, center, center, center),\n\t",
  "table.hline(),\n\t",
  "[], [*Age 18--40*], [*Age 41--64*], [*Age 65--80*],\n\t",
  "[],", 
  "[($n=",
  comma (parse_number (table [[1,2]])), 
  "$)], [($n=",
  comma (parse_number (table [[1,3]])),
  "$)], [($n=",
  comma (parse_number (table [[1,4]])),   
  "$)],\n\t",
  "table.hline(),\n\t",
  "[*Sex*], [], [], [],\n\t",
  "[#h(1em)Male], [",
  table [[which (table [,1] == "sex") + 1, 2]],
  "], [",
  table [[which (table [,1] == "sex") + 1, 3]],
  "], [",
  table [[which (table [,1] == "sex") + 1, 4]],  
  "],\n\t",
  "[#h(1em)Female], [",
  table [[which (table [,1] == "sex") + 2, 2]],
  "], [",
  table [[which (table [,1] == "sex") + 2, 3]],
  "], [",
  table [[which (table [,1] == "sex") + 2, 4]],  
  "],\n\t",  
  "[*Race*], [], [], [],\n\t",
  "[#h(1em)White], [",
  table [[which (table [,1] == "race") + 1, 2]],
  "], [",
  table [[which (table [,1] == "race") + 1, 3]],
  "], [",
  table [[which (table [,1] == "race") + 1, 4]],  
  "],\n\t",
  "[#h(1em)Black], [",
  table [[which (table [,1] == "race") + 2, 2]],
  "], [",
  table [[which (table [,1] == "race") + 2, 3]],
  "], [",
  table [[which (table [,1] == "race") + 2, 4]],  
  "],\n\t",
  "[#h(1em)Asian], [",
  table [[which (table [,1] == "race") + 3, 2]],
  "], [",
  table [[which (table [,1] == "race") + 3, 3]],
  "], [",
  table [[which (table [,1] == "race") + 3, 4]],  
  "],\n\t",
  "[#h(1em)Other], [",
  table [[which (table [,1] == "race") + 4, 2]],
  "], [",
  table [[which (table [,1] == "race") + 4, 3]],
  "], [",
  table [[which (table [,1] == "race") + 4, 4]],  
  "],\n\t",
  "[*Ethnicity*], [], [], [],\n\t",
  "[#h(1em)Hispanic], [",
  table [[which (table [,1] == "ethnicity") + 1, 2]],
  "], [",
  table [[which (table [,1] == "ethnicity") + 1, 3]],
  "], [",
  table [[which (table [,1] == "ethnicity") + 1, 4]],  
  "],\n\t",
  "[#h(1em)Not Hispanic], [",
  table [[which (table [,1] == "ethnicity") + 2, 2]],
  "], [",
  table [[which (table [,1] == "ethnicity") + 2, 3]],
  "], [",
  table [[which (table [,1] == "ethnicity") + 2, 4]],  
  "],\n\t",   
  "[*BMI, kg/m#super[2]*],",
  "[",
  table [[which (table [,1] == "bmi"), 2]],
  "], [",
  table [[which (table [,1] == "bmi"), 3]],
  "], [",
  table [[which (table [,1] == "bmi"), 4]],  
  "],\n\t",  
  "[*Pack Years, yr*],",
  "[",
  table [[which (table [,1] == "pack_years"), 2]],
  "], [",
  table [[which (table [,1] == "pack_years"), 3]],
  "], [",
  table [[which (table [,1] == "pack_years"), 4]],  
  "],\n\t",
  "[*Respiratory Symptoms*], [], [], [],\n\t",
  "[#h(1em)Cough], [",
  table [[which (table [,1] == "cough") + 2, 2]],
  "], [",
  table [[which (table [,1] == "cough") + 2, 3]],
  "], [",
  table [[which (table [,1] == "cough") + 2, 4]],  
  "],\n\t",
  "[#h(1em)Dyspnea], [",
  table [[which (table [,1] == "dyspnea") + 2, 2]],
  "], [",
  table [[which (table [,1] == "dyspnea") + 2, 3]],
  "], [",
  table [[which (table [,1] == "dyspnea") + 2, 4]],  
  "],\n\t",
  "[#h(1em)Wheeze], [",
  table [[which (table [,1] == "wheeze") + 2, 2]],
  "], [",
  table [[which (table [,1] == "wheeze") + 2, 3]],
  "], [",
  table [[which (table [,1] == "wheeze") + 2, 4]],  
  "],\n\t",   
  "[*Respiratory Diseases*], [], [], [],\n\t",
  "[#h(1em)Asthma], [",
  table [[which (table [,1] == "asthma") + 2, 2]],
  "], [",
  table [[which (table [,1] == "asthma") + 2, 3]],
  "], [",
  table [[which (table [,1] == "asthma") + 2, 4]],  
  "],\n\t",
  "[#h(1em)Bronchiectasis], [",
  table [[which (table [,1] == "bronchiectasis") + 2, 2]],
  "], [",
  table [[which (table [,1] == "bronchiectasis") + 2, 3]],
  "], [",
  table [[which (table [,1] == "bronchiectasis") + 2, 4]],  
  "],\n\t",
  "[#h(1em)Chest Wall Disorder], [",
  table [[which (table [,1] == "chest_wall") + 2, 2]],
  "], [",
  table [[which (table [,1] == "chest_wall") + 2, 3]],
  "], [",
  table [[which (table [,1] == "chest_wall") + 2, 4]],  
  "],\n\t",
  "[#h(1em)Chronic Obstructive Pulmonary Disease], [",
  table [[which (table [,1] == "copd") + 2, 2]],
  "], [",
  table [[which (table [,1] == "copd") + 2, 3]],
  "], [",
  table [[which (table [,1] == "copd") + 2, 4]],  
  "],\n\t",
  "[#h(1em)Interstitial Lung Disease], [",
  table [[which (table [,1] == "ild") + 2, 2]],
  "], [",
  table [[which (table [,1] == "ild") + 2, 3]],
  "], [",
  table [[which (table [,1] == "ild") + 2, 4]],  
  "],\n\t",
  "[#h(1em)Neuromuscular Disease], [",
  table [[which (table [,1] == "neuromuscular") + 2, 2]],
  "], [",
  table [[which (table [,1] == "neuromuscular") + 2, 3]],
  "], [",
  table [[which (table [,1] == "neuromuscular") + 2, 4]],  
  "],\n\t",
  "[#h(1em)None], [",
  table [[which (table [,1] == "none") + 2, 2]],
  "], [",
  table [[which (table [,1] == "none") + 2, 3]],
  "], [",
  table [[which (table [,1] == "none") + 2, 4]],  
  "],\n\t",
  "[*Computed Tomography*], [], [], [],\n\t",
  "[#h(1em)Chronic Obstructive Pulmonary Disease], [",
  table [[which (table [,1] == "copd_imaging") + 2, 2]],
  "], [",
  table [[which (table [,1] == "copd_imaging") + 2, 3]],
  "], [",
  table [[which (table [,1] == "copd_imaging") + 2, 4]],  
  "],\n\t",
  "[#h(1em)Interstitial Lung Disease], [",
  table [[which (table [,1] == "ild_imaging") + 2, 2]],
  "], [",
  table [[which (table [,1] == "ild_imaging") + 2, 3]],
  "], [",
  table [[which (table [,1] == "ild_imaging") + 2, 4]],  
  "],\n\t",  
  "[*Dynamic Lung Volumes, z-score*], [], [], [],\n\t",
  "[#h(1em)FEV#sub[1]],",
  "[",
  table [[which (table [,1] == "fev1_z_score"), 2]],
  "], [",
  table [[which (table [,1] == "fev1_z_score"), 3]],
  "], [",
  table [[which (table [,1] == "fev1_z_score"), 4]],  
  "],\n\t",
  "[#h(1em)FVC],",
  "[",
  table [[which (table [,1] == "fvc_z_score"), 2]],
  "], [",
  table [[which (table [,1] == "fvc_z_score"), 3]],
  "], [",
  table [[which (table [,1] == "fvc_z_score"), 4]],  
  "],\n\t",
  "[#h(1em)FEV#sub[1]/FVC],",
  "[",
  table [[which (table [,1] == "fev1_fvc_z_score"), 2]],
  "], [",
  table [[which (table [,1] == "fev1_fvc_z_score"), 3]],
  "], [",
  table [[which (table [,1] == "fev1_fvc_z_score"), 4]],  
  "],\n\t",
  "[*Static Lung Volumes, z-score*], [], [], [],\n\t",
  "[#h(1em)TLC],",
  "[",
  table [[which (table [,1] == "tlc_z_score"), 2]],
  "], [",
  table [[which (table [,1] == "tlc_z_score"), 3]],
  "], [",
  table [[which (table [,1] == "tlc_z_score"), 4]],  
  "],\n\t",
  "[#h(1em)RV],",
  "[",
  table [[which (table [,1] == "rv_z_score"), 2]],
  "], [",
  table [[which (table [,1] == "rv_z_score"), 3]],
  "], [",
  table [[which (table [,1] == "rv_z_score"), 4]],  
  "],\n\t",
  "[*Diffusing Capacity, z-score*], [], [], [],\n\t",
  "[#h(1em)D#sub[LCO]],",
  "[",
  table [[which (table [,1] == "dlco_z_score"), 2]],
  "], [",
  table [[which (table [,1] == "dlco_z_score"), 3]],
  "], [",
  table [[which (table [,1] == "dlco_z_score"), 4]],  
  "],\n\t",
  "[#h(1em)K#sub[CO]],",
  "[",
  table [[which (table [,1] == "kco_z_score"), 2]],
  "], [",
  table [[which (table [,1] == "kco_z_score"), 3]],
  "], [",
  table [[which (table [,1] == "kco_z_score"), 4]],  
  "],\n\t",
  "[*Interpretation*], [], [], [],\n\t",
  "[#h(1em)Normal],",
  "[",
  table [[which (table [,1] == "spirometry") + 1, 2]],
  "], [",
  table [[which (table [,1] == "spirometry") + 1, 3]],
  "], [",
  table [[which (table [,1] == "spirometry") + 1, 4]],  
  "],\n\t",
  "[#h(1em)Non-Specific],",
  "[",
  table [[which (table [,1] == "spirometry") + 2, 2]],
  "], [",
  table [[which (table [,1] == "spirometry") + 2, 3]],
  "], [",
  table [[which (table [,1] == "spirometry") + 2, 4]],  
  "],\n\t",
  "[#h(1em)Obstructive],",
  "[",
  table [[which (table [,1] == "spirometry") + 3, 2]],
  "], [",
  table [[which (table [,1] == "spirometry") + 3, 3]],
  "], [",
  table [[which (table [,1] == "spirometry") + 3, 4]],  
  "],\n\t",
  "[#h(1em)Restrictive with Normal Spirometry],",
  "[",
  table [[which (table [,1] == "spirometry") + 4, 2]],
  "], [",
  table [[which (table [,1] == "spirometry") + 4, 3]],
  "], [",
  table [[which (table [,1] == "spirometry") + 4, 4]],  
  "],\n\t",
  "[#h(1em)Restrictive with Abnormal Spirometry],",
  "[",
  table [[which (table [,1] == "spirometry") + 5, 2]],
  "], [",
  table [[which (table [,1] == "spirometry") + 5, 3]],
  "], [",
  table [[which (table [,1] == "spirometry") + 5, 4]],  
  "],\n\t",
  "[#h(1em)Mixed],",
  "[",
  table [[which (table [,1] == "spirometry") + 6, 2]],
  "], [",
  table [[which (table [,1] == "spirometry") + 6, 3]],
  "], [",
  table [[which (table [,1] == "spirometry") + 6, 4]],  
  "],\n\t",    
  "table.hline()\n",
  ")",
  sep = ""
)

write_lines (table, "../tables/e-table-12.txt")

################################################################################
## e-table 13 - Baseline Characteristics by Sex
################################################################################

data <- pfts %>%
  mutate (spirometry = case_when (
    interpretation == "Normal" ~ 1,
    interpretation == "Non-Specific" ~ 2,
    interpretation == "Obstructive" ~ 3,
    interpretation == "Restrictive with Normal Spirometry" ~ 4,
    interpretation == "Restrictive with Abnormal Spirometry" ~ 5,
    interpretation == "Mixed" ~ 6)
  ) %>%
  mutate (spirometry = factor (
    spirometry,
    levels = c ("1", "2", "3", "4", "5", "6"))
  ) %>% 
  filter (is.na (race) == 0) %>% 
  mutate (race = factor (
    race,
    levels = c ("1", "2", "3", "4", "5"))
  ) %>% 
  mutate (copd_imaging = as.factor (copd_imaging)) %>% 
  mutate (ild_imaging = as.factor (ild_imaging))

table <- table1 (~
  age +
  race +
  ethnicity +
  bmi +
  pack_years +
  asthma +
  bronchiectasis +
  chest_wall +
  copd +
  ild +
  neuromuscular +
  none +
  copd_imaging +
  ild_imaging +    
  spirometry +
  fev1_z_score +
  fvc_z_score +
  fev1_fvc_z_score +
  tlc_z_score +
  rv_z_score +
  dlco_z_score +
  kco_z_score |
  sex,
  data = data,
  big.mark =",",
  render.categorical = render_categorical,
  render.continuous = render_continuous
)

table <- as.data.frame (table)

table <- paste (
  "#table(\n\t",
  "inset: (x: 10pt, y: 4pt),\n\t",
  "columns: 3,\n\t",
  "align: (left, center, center),\n\t",
  "table.hline(),\n\t",
  "[], [*Male*], [*Female*],\n\t",
  "[],", 
  "[($n=",
  comma (parse_number (table [[1,2]])), 
  "$)], [($n=",
  comma (parse_number (table [[1,3]])), 
  "$)],\n\t",
  "table.hline(),\n\t",
  "[*Age, yrs*], [",
  table [[which (table [,1] == "age"), 2]],
  "], [",
  table [[which (table [,1] == "age"), 3]],
  "],\n\t",
  "[*Race*], [], [],\n\t",
  "[#h(1em)White], [",
  table [[which (table [,1] == "race") + 1, 2]],
  "], [",
  table [[which (table [,1] == "race") + 1, 3]],
  "],\n\t",
  "[#h(1em)Black], [",
  table [[which (table [,1] == "race") + 2, 2]],
  "], [",
  table [[which (table [,1] == "race") + 2, 3]],
  "],\n\t",
  "[#h(1em)Asian], [",
  table [[which (table [,1] == "race") + 3, 2]],
  "], [",
  table [[which (table [,1] == "race") + 3, 3]],
  "],\n\t",
  "[#h(1em)Other], [",
  table [[which (table [,1] == "race") + 4, 2]],
  "], [",
  table [[which (table [,1] == "race") + 4, 3]],
  "],\n\t",
  "[*Ethnicity*], [], [],\n\t",
  "[#h(1em)Hispanic], [",
  table [[which (table [,1] == "ethnicity") + 1, 2]],
  "], [",
  table [[which (table [,1] == "ethnicity") + 1, 3]],
  "],\n\t",
  "[#h(1em)Not Hispanic], [",
  table [[which (table [,1] == "ethnicity") + 2, 2]],
  "], [",
  table [[which (table [,1] == "ethnicity") + 2, 3]],
  "],\n\t",  
  "[*BMI, kg/m#super[2]*],",
  "[",
  table [[which (table [,1] == "bmi"), 2]],
  "], [",
  table [[which (table [,1] == "bmi"), 3]],
  "],\n\t",  
  "[*Pack Years, yr*],",
  "[",
  table [[which (table [,1] == "pack_years"), 2]],
  "], [",
  table [[which (table [,1] == "pack_years"), 3]],
  "],\n\t",
  "[*Respiratory Disease Diagnoses*], [], [],\n\t",
  "[#h(1em)Asthma], [",
  table [[which (table [,1] == "asthma") + 2, 2]],
  "], [",
  table [[which (table [,1] == "asthma") + 2, 3]],
  "],\n\t",
  "[#h(1em)Bronchiectasis], [",
  table [[which (table [,1] == "bronchiectasis") + 2, 2]],
  "], [",
  table [[which (table [,1] == "bronchiectasis") + 2, 3]],
  "],\n\t",
  "[#h(1em)Chest Wall Disorder], [",
  table [[which (table [,1] == "chest_wall") + 2, 2]],
  "], [",
  table [[which (table [,1] == "chest_wall") + 2, 3]],
  "],\n\t",
  "[#h(1em)Chronic Obstructive Pulmonary Disease], [",
  table [[which (table [,1] == "copd") + 2, 2]],
  "], [",
  table [[which (table [,1] == "copd") + 2, 3]],
  "],\n\t",
  "[#h(1em)Interstitial Lung Disease], [",
  table [[which (table [,1] == "ild") + 2, 2]],
  "], [",
  table [[which (table [,1] == "ild") + 2, 3]],
  "],\n\t",
  "[#h(1em)Neuromuscular Disease], [",
  table [[which (table [,1] == "neuromuscular") + 2, 2]],
  "], [",
  table [[which (table [,1] == "neuromuscular") + 2, 3]],
  "],\n\t",
  "[#h(1em)None], [",
  table [[which (table [,1] == "none") + 2, 2]],
  "], [",
  table [[which (table [,1] == "none") + 2, 3]],
  "],\n\t",
  "[*Computed Tomography Findings*], [], [],\n\t",
  "[#h(1em)Chronic Obstructive Pulmonary Disease], [",
  table [[which (table [,1] == "copd_imaging") + 2, 2]],
  "], [",
  table [[which (table [,1] == "copd_imaging") + 2, 3]],
  "],\n\t",
  "[#h(1em)Interstitial Lung Disease], [",
  table [[which (table [,1] == "ild_imaging") + 2, 2]],
  "], [",
  table [[which (table [,1] == "ild_imaging") + 2, 3]],
  "],\n\t",  
  "[*Dynamic Lung Volumes, z-score*], [], [],\n\t",
  "[#h(1em)FEV#sub[1]],",
  "[",
  table [[which (table [,1] == "fev1_z_score"), 2]],
  "], [",
  table [[which (table [,1] == "fev1_z_score"), 3]],
  "],\n\t",
  "[#h(1em)FVC],",
  "[",
  table [[which (table [,1] == "fvc_z_score"), 2]],
  "], [",
  table [[which (table [,1] == "fvc_z_score"), 3]],
  "],\n\t",
  "[#h(1em)FEV#sub[1]/FVC],",
  "[",
  table [[which (table [,1] == "fev1_fvc_z_score"), 2]],
  "], [",
  table [[which (table [,1] == "fev1_fvc_z_score"), 3]],
  "],\n\t",
  "[*Static Lung Volumes, z-score*], [], [],\n\t",
  "[#h(1em)TLC],",
  "[",
  table [[which (table [,1] == "tlc_z_score"), 2]],
  "], [",
  table [[which (table [,1] == "tlc_z_score"), 3]],
  "],\n\t",
  "[#h(1em)RV],",
  "[",
  table [[which (table [,1] == "rv_z_score"), 2]],
  "], [",
  table [[which (table [,1] == "rv_z_score"), 3]],
  "],\n\t",
  "[*Diffusing Capacity, z-score*], [], [],\n\t",
  "[#h(1em)D#sub[LCO]],",
  "[",
  table [[which (table [,1] == "dlco_z_score"), 2]],
  "], [",
  table [[which (table [,1] == "dlco_z_score"), 3]],
  "],\n\t",
  "[#h(1em)K#sub[CO]],",
  "[",
  table [[which (table [,1] == "kco_z_score"), 2]],
  "], [",
  table [[which (table [,1] == "kco_z_score"), 3]],
  "],\n\t",
  "[*Interpretation*], [], [],\n\t",
  "[#h(1em)Normal],",
  "[",
  table [[which (table [,1] == "spirometry") + 1, 2]],
  "], [",
  table [[which (table [,1] == "spirometry") + 1, 3]],
  "],\n\t",
  "[#h(1em)Non-Specific],",
  "[",
  table [[which (table [,1] == "spirometry") + 2, 2]],
  "], [",
  table [[which (table [,1] == "spirometry") + 2, 3]],
  "],\n\t",
  "[#h(1em)Obstructive],",
  "[",
  table [[which (table [,1] == "spirometry") + 3, 2]],
  "], [",
  table [[which (table [,1] == "spirometry") + 3, 3]],
  "],\n\t",
  "[#h(1em)Restrictive with Normal Spirometry],",
  "[",
  table [[which (table [,1] == "spirometry") + 4, 2]],
  "], [",
  table [[which (table [,1] == "spirometry") + 4, 3]],
  "],\n\t",
  "[#h(1em)Restrictive with Abnormal Spirometry],",
  "[",
  table [[which (table [,1] == "spirometry") + 5, 2]],
  "], [",
  table [[which (table [,1] == "spirometry") + 5, 3]],
  "],\n\t",
  "[#h(1em)Mixed],",
  "[",
  table [[which (table [,1] == "spirometry") + 6, 2]],
  "], [",
  table [[which (table [,1] == "spirometry") + 6, 3]],
  "],\n\t",
  "table.hline()\n",
  ")",
  sep = ""
)

write_lines (table, "../tables/e-table-13.txt")

################################################################################
## e-table 14 - Baseline Characteristics by Race
################################################################################

data <- pfts %>%
  mutate (spirometry = case_when (
    interpretation == "Normal" ~ 1,
    interpretation == "Non-Specific" ~ 2,
    interpretation == "Obstructive" ~ 3,
    interpretation == "Restrictive with Normal Spirometry" ~ 4,
    interpretation == "Restrictive with Abnormal Spirometry" ~ 5,
    interpretation == "Mixed" ~ 6)
  ) %>%
  mutate (spirometry = factor (
    spirometry,
    levels = c ("1", "2", "3", "4", "5", "6"))
  ) %>% 
  filter (is.na (race) == 0) %>% 
  mutate (race = factor (
    race,
    levels = c ("1", "2", "3", "4"))
  ) %>% 
  mutate (copd_imaging = as.factor (copd_imaging)) %>% 
  mutate (ild_imaging = as.factor (ild_imaging))

table <- table1 (~
  age +
  sex +
  ethnicity +
  bmi +
  pack_years +
  cough +
  dyspnea +
  wheeze +
  asthma +
  bronchiectasis +
  chest_wall +
  copd +
  ild +
  neuromuscular +
  none +
  copd_imaging +
  ild_imaging +    
  spirometry +
  fev1_z_score +
  fvc_z_score +
  fev1_fvc_z_score +
  tlc_z_score +
  rv_z_score +
  dlco_z_score +
  kco_z_score |
  race,
  data = data,
  big.mark =",",
  render.categorical = render_categorical,
  render.continuous = render_continuous
)

table <- as.data.frame (table)

table <- paste (
  "#table(\n\t",
  "inset: (x: 10pt, y: 3.5pt),\n\t",
  "columns: 5,\n\t",
  "align: (left, center, center, center, center),\n\t",
  "table.hline(),\n\t",
  "[], [*White*], [*Black*], [*Asian*], [*Other*],\n\t",
  "[],", 
  "[($n=",
  comma (parse_number (table [[1,2]])), 
  "$)], [($n=",
  comma (parse_number (table [[1,3]])), 
  "$)], [($n=",
  comma (parse_number (table [[1,4]])), 
  "$)], [($n=",
  comma (parse_number (table [[1,5]])), 
  "$)],\n\t",
  "table.hline(),\n\t",
  "[*Age, yrs*], [",
  table [[which (table [,1] == "age"), 2]],
  "], [",
  table [[which (table [,1] == "age"), 3]],
  "], [",
  table [[which (table [,1] == "age"), 4]],
  "], [",
  table [[which (table [,1] == "age"), 5]],
  "],\n\t",
  "[*Sex*], [], [], [], [],\n\t",
  "[#h(1em)Male], [",
  table [[which (table [,1] == "sex") + 1, 2]],
  "], [",
  table [[which (table [,1] == "sex") + 1, 3]],
  "], [",  
  table [[which (table [,1] == "sex") + 1, 4]],
  "], [",  
  table [[which (table [,1] == "sex") + 1, 5]],
  "],\n\t",
  "[#h(1em)Female], [",
  table [[which (table [,1] == "sex") + 2, 2]],
  "], [",
  table [[which (table [,1] == "sex") + 2, 3]],
  "], [",  
  table [[which (table [,1] == "sex") + 2, 4]],
  "], [",  
  table [[which (table [,1] == "sex") + 2, 5]],
  "],\n\t",
  "[*Ethnicity*], [], [], [], [],\n\t",
  "[#h(1em)Hispanic], [",
  table [[which (table [,1] == "ethnicity") + 1, 2]],
  "], [",
  table [[which (table [,1] == "ethnicity") + 1, 3]],
  "], [",  
  table [[which (table [,1] == "ethnicity") + 1, 4]],
  "], [",  
  table [[which (table [,1] == "ethnicity") + 1, 5]],
  "],\n\t",
  "[#h(1em)Not Hispanic], [",
  table [[which (table [,1] == "ethnicity") + 2, 2]],
  "], [",
  table [[which (table [,1] == "ethnicity") + 2, 3]],
  "], [",  
  table [[which (table [,1] == "ethnicity") + 2, 4]],
  "], [",  
  table [[which (table [,1] == "ethnicity") + 2, 5]],
  "],\n\t",  
  "[*BMI, kg/m#super[2]*],",
  "[",
  table [[which (table [,1] == "bmi"), 2]],
  "], [",
  table [[which (table [,1] == "bmi"), 3]],
  "], [",
  table [[which (table [,1] == "bmi"), 4]],
  "], [",
  table [[which (table [,1] == "bmi"), 5]],
  "],\n\t",  
  "[*Pack Years, yr*],",
  "[",
  table [[which (table [,1] == "pack_years"), 2]],
  "], [",
  table [[which (table [,1] == "pack_years"), 3]],
  "], [",
  table [[which (table [,1] == "pack_years"), 4]],
  "], [",
  table [[which (table [,1] == "pack_years"), 5]],
  "],\n\t",
  "[*Respiratory Symptoms*], [], [], [], [],\n\t",
  "[#h(1em)Cough], [",
  table [[which (table [,1] == "cough") + 2, 2]],
  "], [",
  table [[which (table [,1] == "cough") + 2, 3]],
  "], [",  
  table [[which (table [,1] == "cough") + 2, 4]],
  "], [",  
  table [[which (table [,1] == "cough") + 2, 5]],
  "],\n\t",
  "[#h(1em)Dyspnea], [",
  table [[which (table [,1] == "dyspnea") + 2, 2]],
  "], [",
  table [[which (table [,1] == "dyspnea") + 2, 3]],
  "], [",  
  table [[which (table [,1] == "dyspnea") + 2, 4]],
  "], [",  
  table [[which (table [,1] == "dyspnea") + 2, 5]],
  "],\n\t",
  "[#h(1em)Wheeze], [",
  table [[which (table [,1] == "wheeze") + 2, 2]],
  "], [",
  table [[which (table [,1] == "wheeze") + 2, 3]],
  "], [",  
  table [[which (table [,1] == "wheeze") + 2, 4]],
  "], [",  
  table [[which (table [,1] == "wheeze") + 2, 5]],
  "],\n\t",   
  "[*Respiratory Diseases*], [], [], [], [],\n\t",
  "[#h(1em)Asthma], [",
  table [[which (table [,1] == "asthma") + 2, 2]],
  "], [",
  table [[which (table [,1] == "asthma") + 2, 3]],
  "], [",  
  table [[which (table [,1] == "asthma") + 2, 4]],
  "], [",  
  table [[which (table [,1] == "asthma") + 2, 5]],
  "],\n\t",
  "[#h(1em)Bronchiectasis], [",
  table [[which (table [,1] == "bronchiectasis") + 2, 2]],
  "], [",
  table [[which (table [,1] == "bronchiectasis") + 2, 3]],
  "], [",  
  table [[which (table [,1] == "bronchiectasis") + 2, 4]],
  "], [",  
  table [[which (table [,1] == "bronchiectasis") + 2, 5]],
  "],\n\t",
  "[#h(1em)Chest Wall Disorder], [",
  table [[which (table [,1] == "chest_wall") + 2, 2]],
  "], [",
  table [[which (table [,1] == "chest_wall") + 2, 3]],
  "], [",  
  table [[which (table [,1] == "chest_wall") + 2, 4]],
  "], [",  
  table [[which (table [,1] == "chest_wall") + 2, 5]],
  "],\n\t",
  "[#h(1em)Chronic Obstructive Pulmonary Disease], [",
  table [[which (table [,1] == "copd") + 2, 2]],
  "], [",
  table [[which (table [,1] == "copd") + 2, 3]],
  "], [",  
  table [[which (table [,1] == "copd") + 2, 4]],
  "], [",  
  table [[which (table [,1] == "copd") + 2, 5]],
  "],\n\t",
  "[#h(1em)Interstitial Lung Disease], [",
  table [[which (table [,1] == "ild") + 2, 2]],
  "], [",
  table [[which (table [,1] == "ild") + 2, 3]],
  "], [",  
  table [[which (table [,1] == "ild") + 2, 4]],
  "], [",  
  table [[which (table [,1] == "ild") + 2, 5]],
  "],\n\t",
  "[#h(1em)Neuromuscular Disease], [",
  table [[which (table [,1] == "neuromuscular") + 2, 2]],
  "], [",
  table [[which (table [,1] == "neuromuscular") + 2, 3]],
  "], [",  
  table [[which (table [,1] == "neuromuscular") + 2, 4]],
  "], [",  
  table [[which (table [,1] == "neuromuscular") + 2, 5]],
  "],\n\t",
  "[#h(1em)None], [",
  table [[which (table [,1] == "none") + 2, 2]],
  "], [",
  table [[which (table [,1] == "none") + 2, 3]],
  "], [",  
  table [[which (table [,1] == "none") + 2, 4]],
  "], [",  
  table [[which (table [,1] == "none") + 2, 5]],
  "],\n\t",
  "[*Computed Tomography*], [], [], [], [],\n\t",
  "[#h(1em)Chronic Obstructive Pulmonary Disease], [",
  table [[which (table [,1] == "copd_imaging") + 2, 2]],
  "], [",
  table [[which (table [,1] == "copd_imaging") + 2, 3]],
  "], [",  
  table [[which (table [,1] == "copd_imaging") + 2, 4]],
  "], [",  
  table [[which (table [,1] == "copd_imaging") + 2, 5]],
  "],\n\t",
  "[#h(1em)Interstitial Lung Disease], [",
  table [[which (table [,1] == "ild_imaging") + 2, 2]],
  "], [",
  table [[which (table [,1] == "ild_imaging") + 2, 3]],
  "], [",  
  table [[which (table [,1] == "ild_imaging") + 2, 4]],
  "], [",  
  table [[which (table [,1] == "ild_imaging") + 2, 5]],
  "],\n\t",  
  "[*Dynamic Lung Volumes, z-score*], [], [], [], [],\n\t",
  "[#h(1em)FEV#sub[1]],",
  "[",
  table [[which (table [,1] == "fev1_z_score"), 2]],
  "], [",
  table [[which (table [,1] == "fev1_z_score"), 3]],
  "], [",
  table [[which (table [,1] == "fev1_z_score"), 4]],
  "], [",
  table [[which (table [,1] == "fev1_z_score"), 5]],
  "],\n\t",
  "[#h(1em)FVC],",
  "[",
  table [[which (table [,1] == "fvc_z_score"), 2]],
  "], [",
  table [[which (table [,1] == "fvc_z_score"), 3]],
  "], [",
  table [[which (table [,1] == "fvc_z_score"), 4]],
  "], [",
  table [[which (table [,1] == "fvc_z_score"), 5]],
  "],\n\t",
  "[#h(1em)FEV#sub[1]/FVC],",
  "[",
  table [[which (table [,1] == "fev1_fvc_z_score"), 2]],
  "], [",
  table [[which (table [,1] == "fev1_fvc_z_score"), 3]],
  "], [",
  table [[which (table [,1] == "fev1_fvc_z_score"), 4]],
  "], [",
  table [[which (table [,1] == "fev1_fvc_z_score"), 5]],
  "],\n\t",
  "[*Static Lung Volumes, z-score*], [], [], [], [],\n\t",
  "[#h(1em)TLC],",
  "[",
  table [[which (table [,1] == "tlc_z_score"), 2]],
  "], [",
  table [[which (table [,1] == "tlc_z_score"), 3]],
  "], [",
  table [[which (table [,1] == "tlc_z_score"), 4]],
  "], [",
  table [[which (table [,1] == "tlc_z_score"), 5]],
  "],\n\t",
  "[#h(1em)RV],",
  "[",
  table [[which (table [,1] == "rv_z_score"), 2]],
  "], [",
  table [[which (table [,1] == "rv_z_score"), 3]],
  "], [",
  table [[which (table [,1] == "rv_z_score"), 4]],
  "], [",
  table [[which (table [,1] == "rv_z_score"), 5]],
  "],\n\t",
  "[*Diffusing Capacity, z-score*], [], [], [], [],\n\t",
  "[#h(1em)D#sub[LCO]],",
  "[",
  table [[which (table [,1] == "dlco_z_score"), 2]],
  "], [",
  table [[which (table [,1] == "dlco_z_score"), 3]],
  "], [",
  table [[which (table [,1] == "dlco_z_score"), 4]],
  "], [",
  table [[which (table [,1] == "dlco_z_score"), 5]],
  "],\n\t",
  "[#h(1em)K#sub[CO]],",
  "[",
  table [[which (table [,1] == "kco_z_score"), 2]],
  "], [",
  table [[which (table [,1] == "kco_z_score"), 3]],
  "], [",
  table [[which (table [,1] == "kco_z_score"), 4]],
  "], [",
  table [[which (table [,1] == "kco_z_score"), 5]],
  "],\n\t",
  "[*Interpretation*], [], [], [], [],\n\t",
  "[#h(1em)Normal],",
  "[",
  table [[which (table [,1] == "spirometry") + 1, 2]],
  "], [",
  table [[which (table [,1] == "spirometry") + 1, 3]],
  "], [",
  table [[which (table [,1] == "spirometry") + 1, 4]],
  "], [",
  table [[which (table [,1] == "spirometry") + 1, 5]],
  "],\n\t",
  "[#h(1em)Non-Specific],",
  "[",
  table [[which (table [,1] == "spirometry") + 2, 2]],
  "], [",
  table [[which (table [,1] == "spirometry") + 2, 3]],
  "], [",
  table [[which (table [,1] == "spirometry") + 2, 4]],
  "], [",
  table [[which (table [,1] == "spirometry") + 2, 5]],
  "],\n\t",
  "[#h(1em)Obstructive],",
  "[",
  table [[which (table [,1] == "spirometry") + 3, 2]],
  "], [",
  table [[which (table [,1] == "spirometry") + 3, 3]],
  "], [",
  table [[which (table [,1] == "spirometry") + 3, 4]],
  "], [",
  table [[which (table [,1] == "spirometry") + 3, 5]],
  "],\n\t",
  "[#h(1em)Restrictive with Normal Spirometry],",
  "[",
  table [[which (table [,1] == "spirometry") + 4, 2]],
  "], [",
  table [[which (table [,1] == "spirometry") + 4, 3]],
  "], [",
  table [[which (table [,1] == "spirometry") + 4, 4]],
  "], [",
  table [[which (table [,1] == "spirometry") + 4, 5]],
  "],\n\t",
  "[#h(1em)Restrictive with Abnormal Spirometry],",
  "[",
  table [[which (table [,1] == "spirometry") + 5, 2]],
  "], [",
  table [[which (table [,1] == "spirometry") + 5, 3]],
  "], [", 
  table [[which (table [,1] == "spirometry") + 5, 4]],
  "], [",
  table [[which (table [,1] == "spirometry") + 5, 5]],
  "],\n\t",
  "[#h(1em)Mixed],",
  "[",
  table [[which (table [,1] == "spirometry") + 6, 2]],
  "], [",
  table [[which (table [,1] == "spirometry") + 6, 3]],
  "], [",
  table [[which (table [,1] == "spirometry") + 6, 4]],
  "], [",
  table [[which (table [,1] == "spirometry") + 6, 5]],
  "],\n\t",    
  "table.hline()\n",
  ")",
  sep = ""
)

write_lines (table, "../tables/e-table-14.txt")

################################################################################
## e-Table 15 - Baseline Characteristics by Ethnicity
################################################################################

data <- pfts %>%
  mutate (spirometry = case_when (
    interpretation == "Normal" ~ 1,
    interpretation == "Non-Specific" ~ 2,
    interpretation == "Obstructive" ~ 3,
    interpretation == "Restrictive with Normal Spirometry" ~ 4,
    interpretation == "Restrictive with Abnormal Spirometry" ~ 5,
    interpretation == "Mixed" ~ 6)
  ) %>%
  mutate (spirometry = factor (
    spirometry,
    levels = c ("1", "2", "3", "4", "5", "6"))
  ) %>% 
  filter (is.na (ethnicity) == 0) %>% 
  mutate (race = factor (
    race,
    levels = c ("1", "2", "3", "4"))
  ) %>% 
  mutate (copd_imaging = as.factor (copd_imaging)) %>% 
  mutate (ild_imaging = as.factor (ild_imaging))

table <- table1 (~
  age +
  sex +
  race +
  bmi +
  pack_years +
  cough +
  dyspnea +
  wheeze +
  asthma +
  bronchiectasis +
  chest_wall +
  copd +
  ild +
  neuromuscular +
  none +
  copd_imaging +
  ild_imaging +    
  spirometry +
  fev1_z_score +
  fvc_z_score +
  fev1_fvc_z_score +
  tlc_z_score +
  rv_z_score +
  dlco_z_score +
  kco_z_score |
  ethnicity,
  data = data,
  big.mark =",",
  render.categorical = render_categorical,
  render.continuous = render_continuous
)

table <- as.data.frame (table)

table <- paste (
  "#table(\n\t",
  "inset: (x: 10pt, y: 3.5pt),\n\t",
  "columns: 3,\n\t",
  "align: (left, center, center),\n\t",
  "table.hline(),\n\t",
  "[], [*Hispanic*], [*Not Hispanic*],\n\t",
  "[],", 
  "[($n=",
  comma (parse_number (table [[1,2]])), 
  "$)], [($n=",
  comma (parse_number (table [[1,3]])), 
  "$)],\n\t",
  "table.hline(),\n\t",
  "[*Age, yrs*], [",
  table [[which (table [,1] == "age"), 2]],
  "], [",
  table [[which (table [,1] == "age"), 3]],
  "],\n\t",
  "[*Sex*], [], [],\n\t",
  "[#h(1em)Male], [",
  table [[which (table [,1] == "sex") + 1, 2]],
  "], [",
  table [[which (table [,1] == "sex") + 1, 3]],
  "],\n\t",
  "[#h(1em)Female], [",
  table [[which (table [,1] == "sex") + 2, 2]],
  "], [",
  table [[which (table [,1] == "sex") + 2, 3]],
  "],\n\t",    
  "[*Race*], [], [],\n\t",
  "[#h(1em)White], [",
  table [[which (table [,1] == "race") + 1, 2]],
  "], [",
  table [[which (table [,1] == "race") + 1, 3]],
  "],\n\t",
  "[#h(1em)Black], [",
  table [[which (table [,1] == "race") + 2, 2]],
  "], [",
  table [[which (table [,1] == "race") + 2, 3]],
  "],\n\t",
  "[#h(1em)Asian], [",
  table [[which (table [,1] == "race") + 3, 2]],
  "], [",
  table [[which (table [,1] == "race") + 3, 3]],
  "],\n\t",
  "[#h(1em)Other], [",
  table [[which (table [,1] == "race") + 4, 2]],
  "], [",
  table [[which (table [,1] == "race") + 4, 3]],
  "],\n\t",
  "[*BMI, kg/m#super[2]*],",
  "[",
  table [[which (table [,1] == "bmi"), 2]],
  "], [",
  table [[which (table [,1] == "bmi"), 3]],
  "],\n\t",  
  "[*Pack Years, yr*],",
  "[",
  table [[which (table [,1] == "pack_years"), 2]],
  "], [",
  table [[which (table [,1] == "pack_years"), 3]],
  "],\n\t",
  "[*Respiratory Symptoms*], [], [],\n\t",
  "[#h(1em)Cough], [",
  table [[which (table [,1] == "cough") + 2, 2]],
  "], [",
  table [[which (table [,1] == "cough") + 2, 3]],
  "],\n\t",
  "[#h(1em)Dyspnea], [",
  table [[which (table [,1] == "dyspnea") + 2, 2]],
  "], [",
  table [[which (table [,1] == "dyspnea") + 2, 3]],
  "],\n\t",
  "[#h(1em)Wheeze], [",
  table [[which (table [,1] == "wheeze") + 2, 2]],
  "], [",
  table [[which (table [,1] == "wheeze") + 2, 3]],
  "],\n\t",   
  "[*Respiratory Diseases*], [], [],\n\t",
  "[#h(1em)Asthma], [",
  table [[which (table [,1] == "asthma") + 2, 2]],
  "], [",
  table [[which (table [,1] == "asthma") + 2, 3]],
  "],\n\t",
  "[#h(1em)Bronchiectasis], [",
  table [[which (table [,1] == "bronchiectasis") + 2, 2]],
  "], [",
  table [[which (table [,1] == "bronchiectasis") + 2, 3]],
  "],\n\t",
  "[#h(1em)Chest Wall Disorder], [",
  table [[which (table [,1] == "chest_wall") + 2, 2]],
  "], [",
  table [[which (table [,1] == "chest_wall") + 2, 3]],
  "],\n\t",
  "[#h(1em)Chronic Obstructive Pulmonary Disease], [",
  table [[which (table [,1] == "copd") + 2, 2]],
  "], [",
  table [[which (table [,1] == "copd") + 2, 3]],
  "],\n\t",
  "[#h(1em)Interstitial Lung Disease], [",
  table [[which (table [,1] == "ild") + 2, 2]],
  "], [",
  table [[which (table [,1] == "ild") + 2, 3]],
  "],\n\t",
  "[#h(1em)Neuromuscular Disease], [",
  table [[which (table [,1] == "neuromuscular") + 2, 2]],
  "], [",
  table [[which (table [,1] == "neuromuscular") + 2, 3]],
  "],\n\t",
  "[#h(1em)None], [",
  table [[which (table [,1] == "none") + 2, 2]],
  "], [",
  table [[which (table [,1] == "none") + 2, 3]],
  "],\n\t",
  "[*Computed Tomography Findings*], [], [],\n\t",
  "[#h(1em)Chronic Obstructive Pulmonary Disease], [",
  table [[which (table [,1] == "copd_imaging") + 2, 2]],
  "], [",
  table [[which (table [,1] == "copd_imaging") + 2, 3]],
  "],\n\t",
  "[#h(1em)Interstitial Lung Disease], [",
  table [[which (table [,1] == "ild_imaging") + 2, 2]],
  "], [",
  table [[which (table [,1] == "ild_imaging") + 2, 3]],
  "],\n\t",  
  "[*Dynamic Lung Volumes, z-score*], [], [],\n\t",
  "[#h(1em)FEV#sub[1]],",
  "[",
  table [[which (table [,1] == "fev1_z_score"), 2]],
  "], [",
  table [[which (table [,1] == "fev1_z_score"), 3]],
  "],\n\t",
  "[#h(1em)FVC],",
  "[",
  table [[which (table [,1] == "fvc_z_score"), 2]],
  "], [",
  table [[which (table [,1] == "fvc_z_score"), 3]],
  "],\n\t",
  "[#h(1em)FEV#sub[1]/FVC],",
  "[",
  table [[which (table [,1] == "fev1_fvc_z_score"), 2]],
  "], [",
  table [[which (table [,1] == "fev1_fvc_z_score"), 3]],
  "],\n\t",
  "[*Static Lung Volumes, z-score*], [], [],\n\t",
  "[#h(1em)TLC],",
  "[",
  table [[which (table [,1] == "tlc_z_score"), 2]],
  "], [",
  table [[which (table [,1] == "tlc_z_score"), 3]],
  "],\n\t",
  "[#h(1em)RV],",
  "[",
  table [[which (table [,1] == "rv_z_score"), 2]],
  "], [",
  table [[which (table [,1] == "rv_z_score"), 3]],
  "],\n\t",
  "[*Diffusing Capacity, z-score*], [], [],\n\t",
  "[#h(1em)D#sub[LCO]],",
  "[",
  table [[which (table [,1] == "dlco_z_score"), 2]],
  "], [",
  table [[which (table [,1] == "dlco_z_score"), 3]],
  "],\n\t",
  "[#h(1em)K#sub[CO]],",
  "[",
  table [[which (table [,1] == "kco_z_score"), 2]],
  "], [",
  table [[which (table [,1] == "kco_z_score"), 3]],
  "],\n\t",
  "[*Interpretation*], [], [],\n\t",
  "[#h(1em)Normal],",
  "[",
  table [[which (table [,1] == "spirometry") + 1, 2]],
  "], [",
  table [[which (table [,1] == "spirometry") + 1, 3]],
  "],\n\t",
  "[#h(1em)Non-Specific],",
  "[",
  table [[which (table [,1] == "spirometry") + 2, 2]],
  "], [",
  table [[which (table [,1] == "spirometry") + 2, 3]],
  "],\n\t",
  "[#h(1em)Obstructive],",
  "[",
  table [[which (table [,1] == "spirometry") + 3, 2]],
  "], [",
  table [[which (table [,1] == "spirometry") + 3, 3]],
  "],\n\t",
  "[#h(1em)Restrictive with Normal Spirometry],",
  "[",
  table [[which (table [,1] == "spirometry") + 4, 2]],
  "], [",
  table [[which (table [,1] == "spirometry") + 4, 3]],
  "],\n\t",
  "[#h(1em)Restrictive with Abnormal Spirometry],",
  "[",
  table [[which (table [,1] == "spirometry") + 5, 2]],
  "], [",
  table [[which (table [,1] == "spirometry") + 5, 3]],
  "],\n\t",
  "[#h(1em)Mixed],",
  "[",
  table [[which (table [,1] == "spirometry") + 6, 2]],
  "], [",
  table [[which (table [,1] == "spirometry") + 6, 3]],
  "],\n\t",
  "table.hline()\n",
  ")",
  sep = ""
)

write_lines (table, "../tables/e-table-15.txt")

################################################################################
## e-Table 16 - PFT Characteristics by Pulmonary Diagnostic Lab
################################################################################

data <- pfts %>%
  mutate (spirometry = case_when (
    interpretation == "Normal" ~ 1,
    interpretation == "Non-Specific" ~ 2,
    interpretation == "Obstructive" ~ 3,
    interpretation == "Restrictive with Normal Spirometry" ~ 4,
    interpretation == "Restrictive with Abnormal Spirometry" ~ 5,
    interpretation == "Mixed" ~ 6)
  ) %>%
  mutate (spirometry = factor (
    spirometry,
    levels = c ("1", "2", "3", "4", "5", "6"))
  ) %>% 
  filter (is.na (location) == 0) %>% 
  mutate (copd_imaging = factor (copd_imaging)) %>% 
  mutate (ild_imaging = factor (ild_imaging))

table <- table1 (~
  age +
  sex +
  race +
  ethnicity +
  bmi +
  pack_years +
  cough +
  dyspnea +
  wheeze +
  asthma +
  bronchiectasis +
  chest_wall +
  copd +
  ild +
  neuromuscular +
  none +
  copd_imaging +
  ild_imaging +    
  spirometry +
  fev1_z_score +
  fvc_z_score +
  fev1_fvc_z_score +
  tlc_z_score +
  rv_z_score +
  dlco_z_score +
  kco_z_score |
  location,
  data = data,
  big.mark =",",
  render.categorical = render_categorical,
  render.continuous = render_continuous 
)

table <- as.data.frame (table)

table <- paste (
  "#table(\n\t",
  "inset: (x: 10pt, y: 3pt),\n\t",
  "columns: 5,\n\t",
  "align: (left, center, center, center, center),\n\t",
  "table.hline(),\n\t",
  "[], [*Lab 1*], [*Lab 2*], [*Lab 3*], [*Lab 4*],\n\t",
  "[],", 
  "[($n=",
  comma (parse_number (table [[1,2]])), 
  "$)], [($n=",
  comma (parse_number (table [[1,3]])), 
  "$)], [($n=",
  comma (parse_number (table [[1,4]])), 
  "$)], [($n=",
  comma (parse_number (table [[1,5]])), 
  "$)],\n\t",
  "table.hline(),\n\t",
  "[*Age, yrs*], [",
  table [[which (table [,1] == "age"), 2]],
  "], [",
  table [[which (table [,1] == "age"), 3]],
  "], [",
  table [[which (table [,1] == "age"), 4]],
  "], [",
  table [[which (table [,1] == "age"), 5]],
  "],\n\t",
  "[*Sex*], [], [], [], [],\n\t",
  "[#h(1em)Male], [",
  table [[which (table [,1] == "sex") + 1, 2]],
  "], [",
  table [[which (table [,1] == "sex") + 1, 3]],
  "], [",  
  table [[which (table [,1] == "sex") + 1, 4]],
  "], [",  
  table [[which (table [,1] == "sex") + 1, 5]],
  "],\n\t",
  "[#h(1em)Female], [",
  table [[which (table [,1] == "sex") + 2, 2]],
  "], [",
  table [[which (table [,1] == "sex") + 2, 3]],
  "], [",  
  table [[which (table [,1] == "sex") + 2, 4]],
  "], [",  
  table [[which (table [,1] == "sex") + 2, 5]],
  "],\n\t",
  "[*Race*], [], [], [], [],\n\t",
  "[#h(1em)White], [",
  table [[which (table [,1] == "race") + 1, 2]],
  "], [",
  table [[which (table [,1] == "race") + 1, 3]],
  "], [",  
  table [[which (table [,1] == "race") + 1, 4]],
  "], [",  
  table [[which (table [,1] == "race") + 1, 5]],
  "],\n\t",
  "[#h(1em)Black], [",
  table [[which (table [,1] == "race") + 2, 2]],
  "], [",
  table [[which (table [,1] == "race") + 2, 3]],
  "], [",  
  table [[which (table [,1] == "race") + 2, 4]],
  "], [",  
  table [[which (table [,1] == "race") + 2, 5]],
  "],\n\t",
  "[#h(1em)Asian], [",
  table [[which (table [,1] == "race") + 3, 2]],
  "], [",
  table [[which (table [,1] == "race") + 3, 3]],
  "], [",  
  table [[which (table [,1] == "race") + 3, 4]],
  "], [",  
  table [[which (table [,1] == "race") + 3, 5]],
  "],\n\t",
  "[#h(1em)Other], [",
  table [[which (table [,1] == "race") + 4, 2]],
  "], [",
  table [[which (table [,1] == "race") + 4, 3]],
  "], [",  
  table [[which (table [,1] == "race") + 4, 4]],
  "], [",  
  table [[which (table [,1] == "race") + 4, 5]],
  "],\n\t",  
  "[*Ethnicity*], [], [], [], [],\n\t",
  "[#h(1em)Hispanic], [",
  table [[which (table [,1] == "ethnicity") + 1, 2]],
  "], [",
  table [[which (table [,1] == "ethnicity") + 1, 3]],
  "], [",  
  table [[which (table [,1] == "ethnicity") + 1, 4]],
  "], [",  
  table [[which (table [,1] == "ethnicity") + 1, 5]],
  "],\n\t",
  "[#h(1em)Not Hispanic], [",
  table [[which (table [,1] == "ethnicity") + 2, 2]],
  "], [",
  table [[which (table [,1] == "ethnicity") + 2, 3]],
  "], [",  
  table [[which (table [,1] == "ethnicity") + 2, 4]],
  "], [",  
  table [[which (table [,1] == "ethnicity") + 2, 5]],
  "],\n\t",  
  "[*BMI, kg/m#super[2]*],",
  "[",
  table [[which (table [,1] == "bmi"), 2]],
  "], [",
  table [[which (table [,1] == "bmi"), 3]],
  "], [",
  table [[which (table [,1] == "bmi"), 4]],
  "], [",
  table [[which (table [,1] == "bmi"), 5]],
  "],\n\t",  
  "[*Pack Years, yr*],",
  "[",
  table [[which (table [,1] == "pack_years"), 2]],
  "], [",
  table [[which (table [,1] == "pack_years"), 3]],
  "], [",
  table [[which (table [,1] == "pack_years"), 4]],
  "], [---],\n\t",
  # table [[which (table [,1] == "pack_years"), 5]],
  # "],\n\t",
  "[*Respiratory Symptoms*], [], [], [], [],\n\t",
  "[#h(1em)Cough], [",
  table [[which (table [,1] == "cough") + 2, 2]],
  "], [",
  table [[which (table [,1] == "cough") + 2, 3]],
  "], [",  
  table [[which (table [,1] == "cough") + 2, 4]],
  "], [---],\n\t",  
  #table [[which (table [,1] == "cough") + 2, 5]],
  #"],\n\t",
  "[#h(1em)Dyspnea], [",
  table [[which (table [,1] == "dyspnea") + 2, 2]],
  "], [",
  table [[which (table [,1] == "dyspnea") + 2, 3]],
  "], [",  
  table [[which (table [,1] == "dyspnea") + 2, 4]],
  "], [---],\n\t",  
  # table [[which (table [,1] == "dyspnea") + 2, 5]],
  # "],\n\t",
  "[#h(1em)Wheeze], [",
  table [[which (table [,1] == "wheeze") + 2, 2]],
  "], [",
  table [[which (table [,1] == "wheeze") + 2, 3]],
  "], [",  
  table [[which (table [,1] == "wheeze") + 2, 4]],
  "], [---],\n\t",  
  # table [[which (table [,1] == "wheeze") + 2, 5]],
  # "],\n\t",   
  "[*Respiratory Disease*], [], [], [], [],\n\t",
  "[#h(1em)Asthma], [",
  table [[which (table [,1] == "asthma") + 2, 2]],
  "], [",
  table [[which (table [,1] == "asthma") + 2, 3]],
  "], [",  
  table [[which (table [,1] == "asthma") + 2, 4]],
  "], [",  
  table [[which (table [,1] == "asthma") + 2, 5]],
  "],\n\t",
  "[#h(1em)Bronchiectasis], [",
  table [[which (table [,1] == "bronchiectasis") + 2, 2]],
  "], [",
  table [[which (table [,1] == "bronchiectasis") + 2, 3]],
  "], [",  
  table [[which (table [,1] == "bronchiectasis") + 2, 4]],
  "], [",  
  table [[which (table [,1] == "bronchiectasis") + 2, 5]],
  "],\n\t",
  "[#h(1em)Chest Wall Disorder], [",
  table [[which (table [,1] == "chest_wall") + 2, 2]],
  "], [",
  table [[which (table [,1] == "chest_wall") + 2, 3]],
  "], [",  
  table [[which (table [,1] == "chest_wall") + 2, 4]],
  "], [",  
  table [[which (table [,1] == "chest_wall") + 2, 5]],
  "],\n\t",
  "[#h(1em)Chronic Obstructive Pulmonary Disease], [",
  table [[which (table [,1] == "copd") + 2, 2]],
  "], [",
  table [[which (table [,1] == "copd") + 2, 3]],
  "], [",  
  table [[which (table [,1] == "copd") + 2, 4]],
  "], [",  
  table [[which (table [,1] == "copd") + 2, 5]],
  "],\n\t",
  "[#h(1em)Interstitial Lung Disease], [",
  table [[which (table [,1] == "ild") + 2, 2]],
  "], [",
  table [[which (table [,1] == "ild") + 2, 3]],
  "], [",  
  table [[which (table [,1] == "ild") + 2, 4]],
  "], [",  
  table [[which (table [,1] == "ild") + 2, 5]],
  "],\n\t",
  "[#h(1em)Neuromuscular Disease], [",
  table [[which (table [,1] == "neuromuscular") + 2, 2]],
  "], [",
  table [[which (table [,1] == "neuromuscular") + 2, 3]],
  "], [",  
  table [[which (table [,1] == "neuromuscular") + 2, 4]],
  "], [",  
  table [[which (table [,1] == "neuromuscular") + 2, 5]],
  "],\n\t",
  "[#h(1em)None], [",
  table [[which (table [,1] == "none") + 2, 2]],
  "], [",
  table [[which (table [,1] == "none") + 2, 3]],
  "], [",  
  table [[which (table [,1] == "none") + 2, 4]],
  "], [",  
  table [[which (table [,1] == "none") + 2, 5]],
  "],\n\t",
  "[*Computed Tomography*], [], [], [], [],\n\t",
  "[#h(1em)Chronic Obstructive Pulmonary Disease], [",
  table [[which (table [,1] == "copd_imaging") + 2, 2]],
  "], [",
  table [[which (table [,1] == "copd_imaging") + 2, 3]],
  "], [",  
  table [[which (table [,1] == "copd_imaging") + 2, 4]],
  "], [",  
  table [[which (table [,1] == "copd_imaging") + 2, 5]],
  "],\n\t",
  "[#h(1em)Interstitial Lung Disease], [",
  table [[which (table [,1] == "ild_imaging") + 2, 2]],
  "], [",
  table [[which (table [,1] == "ild_imaging") + 2, 3]],
  "], [",  
  table [[which (table [,1] == "ild_imaging") + 2, 4]],
  "], [",  
  table [[which (table [,1] == "ild_imaging") + 2, 5]],
  "],\n\t",    
  "[*Dynamic Lung Volumes, z-score*], [], [], [], [],\n\t",
  "[#h(1em)FEV#sub[1]],",
  "[",
  table [[which (table [,1] == "fev1_z_score"), 2]],
  "], [",
  table [[which (table [,1] == "fev1_z_score"), 3]],
  "], [",
  table [[which (table [,1] == "fev1_z_score"), 4]],
  "], [",
  table [[which (table [,1] == "fev1_z_score"), 5]],
  "],\n\t",
  "[#h(1em)FVC],",
  "[",
  table [[which (table [,1] == "fvc_z_score"), 2]],
  "], [",
  table [[which (table [,1] == "fvc_z_score"), 3]],
  "], [",
  table [[which (table [,1] == "fvc_z_score"), 4]],
  "], [",
  table [[which (table [,1] == "fvc_z_score"), 5]],
  "],\n\t",
  "[#h(1em)FEV#sub[1]/FVC],",
  "[",
  table [[which (table [,1] == "fev1_fvc_z_score"), 2]],
  "], [",
  table [[which (table [,1] == "fev1_fvc_z_score"), 3]],
  "], [",
  table [[which (table [,1] == "fev1_fvc_z_score"), 4]],
  "], [",
  table [[which (table [,1] == "fev1_fvc_z_score"), 5]],
  "],\n\t",
  "[*Static Lung Volumes, z-score*], [], [], [], [],\n\t",
  "[#h(1em)TLC],",
  "[",
  table [[which (table [,1] == "tlc_z_score"), 2]],
  "], [",
  table [[which (table [,1] == "tlc_z_score"), 3]],
  "], [",
  table [[which (table [,1] == "tlc_z_score"), 4]],
  "], [",
  table [[which (table [,1] == "tlc_z_score"), 5]],
  "],\n\t",
  "[#h(1em)RV],",
  "[",
  table [[which (table [,1] == "rv_z_score"), 2]],
  "], [",
  table [[which (table [,1] == "rv_z_score"), 3]],
  "], [",
  table [[which (table [,1] == "rv_z_score"), 4]],
  "], [",
  table [[which (table [,1] == "rv_z_score"), 5]],
  "],\n\t",
  "[*Diffusing Capacity, z-score*], [], [], [], [],\n\t",
  "[#h(1em)D#sub[LCO]],",
  "[",
  table [[which (table [,1] == "dlco_z_score"), 2]],
  "], [",
  table [[which (table [,1] == "dlco_z_score"), 3]],
  "], [",
  table [[which (table [,1] == "dlco_z_score"), 4]],
  "], [",
  table [[which (table [,1] == "dlco_z_score"), 5]],
  "],\n\t",
  "[#h(1em)K#sub[CO]],",
  "[",
  table [[which (table [,1] == "kco_z_score"), 2]],
  "], [",
  table [[which (table [,1] == "kco_z_score"), 3]],
  "], [",
  table [[which (table [,1] == "kco_z_score"), 4]],
  "], [",
  table [[which (table [,1] == "kco_z_score"), 5]],
  "],\n\t",
  "[*Interpretation*], [], [], [], [],\n\t",
  "[#h(1em)Normal],",
  "[",
  table [[which (table [,1] == "spirometry") + 1, 2]],
  "], [",
  table [[which (table [,1] == "spirometry") + 1, 3]],
  "], [",
  table [[which (table [,1] == "spirometry") + 1, 4]],
  "], [",
  table [[which (table [,1] == "spirometry") + 1, 5]],
  "],\n\t",
  "[#h(1em)Non-Specific],",
  "[",
  table [[which (table [,1] == "spirometry") + 2, 2]],
  "], [",
  table [[which (table [,1] == "spirometry") + 2, 3]],
  "], [",
  table [[which (table [,1] == "spirometry") + 2, 4]],
  "], [",
  table [[which (table [,1] == "spirometry") + 2, 5]],
  "],\n\t",
  "[#h(1em)Obstructive],",
  "[",
  table [[which (table [,1] == "spirometry") + 3, 2]],
  "], [",
  table [[which (table [,1] == "spirometry") + 3, 3]],
  "], [",
  table [[which (table [,1] == "spirometry") + 3, 4]],
  "], [",
  table [[which (table [,1] == "spirometry") + 3, 5]],
  "],\n\t",
  "[#h(1em)Restrictive with Normal Spirometry],",
  "[",
  table [[which (table [,1] == "spirometry") + 4, 2]],
  "], [",
  table [[which (table [,1] == "spirometry") + 4, 3]],
  "], [",
  table [[which (table [,1] == "spirometry") + 4, 4]],
  "], [",
  table [[which (table [,1] == "spirometry") + 4, 5]],
  "],\n\t",
  "[#h(1em)Restrictive with Abnormal Spirometry],",
  "[",
  table [[which (table [,1] == "spirometry") + 5, 2]],
  "], [",
  table [[which (table [,1] == "spirometry") + 5, 3]],
  "], [", 
  table [[which (table [,1] == "spirometry") + 5, 4]],
  "], [",
  table [[which (table [,1] == "spirometry") + 5, 5]],
  "],\n\t",
  "[#h(1em)Mixed],",
  "[",
  table [[which (table [,1] == "spirometry") + 6, 2]],
  "], [",
  table [[which (table [,1] == "spirometry") + 6, 3]],
  "], [",
  table [[which (table [,1] == "spirometry") + 6, 4]],
  "], [",
  table [[which (table [,1] == "spirometry") + 6, 5]],
  "],\n\t",    
  "table.hline()\n",
  ")",
  sep = ""
)

write_lines (table, "../tables/e-table-16.txt")

################################################################################
## e-Table 17 - Missing Data
################################################################################

data <- matrix (nrow = 13, ncol = 2)

# Race

data [1, 1] <- pfts %>% 
  filter (is.na (race)) %>% 
  nrow () %>% 
  comma ()

data [1, 2] <- pfts %>% 
  filter (is.na (race)) %>% 
  nrow () %>% 
  divide_by (nrow (pfts)) %>%
  multiply_by (100) %>% 
  round (digits = 1) %>% 
  format (nsmall = 1)

# Pack Years

data [2, 1] <- pfts %>% 
  filter (is.na (pack_years)) %>% 
  nrow () %>% 
  comma ()

data [2, 2] <- pfts %>% 
  filter (is.na (pack_years)) %>% 
  nrow () %>% 
  divide_by (nrow (pfts)) %>%
  multiply_by (100) %>% 
  round (digits = 1) %>% 
  format (nsmall = 1)

# Cough

data [3, 1] <- pfts %>% 
  filter (is.na (cough)) %>% 
  nrow () %>% 
  comma ()

data [3, 2] <- pfts %>% 
  filter (is.na (cough)) %>% 
  nrow () %>% 
  divide_by (nrow (pfts)) %>%
  multiply_by (100) %>% 
  round (digits = 1) %>% 
  format (nsmall = 1)

# Dyspnea

data [4, 1] <- pfts %>% 
  filter (is.na (dyspnea)) %>% 
  nrow () %>% 
  comma ()

data [4, 2] <- pfts %>% 
  filter (is.na (dyspnea)) %>% 
  nrow () %>% 
  divide_by (nrow (pfts)) %>%
  multiply_by (100) %>% 
  round (digits = 1) %>% 
  format (nsmall = 1)

# Wheeze

data [5, 1] <- pfts %>% 
  filter (is.na (wheeze)) %>% 
  nrow () %>% 
  comma ()

data [5, 2] <- pfts %>% 
  filter (is.na (wheeze)) %>% 
  nrow () %>% 
  divide_by (nrow (pfts)) %>%
  multiply_by (100) %>% 
  round (digits = 1) %>% 
  format (nsmall = 1)

# Respiratory Disease

data [6, 1] <- pfts %>% 
  filter (is.na (copd)) %>% 
  nrow () %>% 
  comma ()

data [6, 2] <- pfts %>% 
  filter (is.na (copd)) %>% 
  nrow () %>% 
  divide_by (nrow (pfts)) %>%
  multiply_by (100) %>% 
  round (digits = 1) %>% 
  format (nsmall = 1)

# CT Findings

data [7, 1] <- pfts %>% 
  filter (is.na (copd_imaging)) %>% 
  nrow () %>% 
  comma ()

data [7, 2] <- pfts %>% 
  filter (is.na (copd_imaging)) %>% 
  nrow () %>% 
  divide_by (nrow (pfts)) %>%
  multiply_by (100) %>% 
  round (digits = 1) %>% 
  format (nsmall = 1)

# Bronchodilator Response

data [8, 1] <- pfts %>% 
  filter (is.na (response)) %>% 
  nrow () %>% 
  comma ()

data [8, 2] <- pfts %>% 
  filter (is.na (response)) %>% 
  nrow () %>% 
  divide_by (nrow (pfts)) %>%
  multiply_by (100) %>% 
  round (digits = 1) %>% 
  format (nsmall = 1)

# Effort

data [9, 1] <- pfts %>% 
  filter (is.na (effort)) %>% 
  nrow () %>% 
  comma ()

data [9, 2] <- pfts %>% 
  filter (is.na (effort)) %>% 
  nrow () %>% 
  divide_by (nrow (pfts)) %>%
  multiply_by (100) %>% 
  round (digits = 1) %>% 
  format (nsmall = 1)

# RV

data [10, 1] <- pfts %>% 
  filter (is.na (rv_z_score)) %>% 
  nrow () %>% 
  comma ()

data [10, 2] <- pfts %>% 
  filter (is.na (rv_z_score)) %>% 
  nrow () %>% 
  divide_by (nrow (pfts)) %>%
  multiply_by (100) %>% 
  round (digits = 1) %>% 
  format (nsmall = 1)

# DLCO

data [11, 1] <- pfts %>% 
  filter (is.na (dlco_z_score)) %>% 
  nrow () %>% 
  comma ()

data [11, 2] <- pfts %>% 
  filter (is.na (dlco_z_score)) %>% 
  nrow () %>% 
  divide_by (nrow (pfts)) %>%
  multiply_by (100) %>% 
  round (digits = 1) %>% 
  format (nsmall = 1)

# KCO

data [12, 1] <- pfts %>% 
  filter (is.na (kco_z_score)) %>% 
  nrow () %>% 
  comma ()

data [12, 2] <- pfts %>% 
  filter (is.na (kco_z_score)) %>% 
  nrow () %>% 
  divide_by (nrow (pfts)) %>%
  multiply_by (100) %>% 
  round (digits = 1) %>% 
  format (nsmall = 1)

# Location

data [13, 1] <- pfts %>% 
  filter (is.na (location)) %>% 
  nrow () %>% 
  comma ()

data [13, 2] <- pfts %>% 
  filter (is.na (location)) %>% 
  nrow () %>% 
  divide_by (nrow (pfts)) %>%
  multiply_by (100) %>% 
  round (digits = 1) %>% 
  format (nsmall = 1)

table <- paste (
  "#table(\n\t",
  "columns: 3,\n\t",
	"align: (left, center, center),\n\t",
	"table.hline(),\n\t",
	"[], [*Number of Tests \\ with Observation Missing*], [*Percentage of Tests \\ with Observation Missing*],\n\t",
	"table.hline(),\n\t",
  "[Pulmonary Diagnostic Lab], [",
  data [13, 1],
  "], [",
  data [13, 2],
  "%],\n\t",
  "[Race and Ethnicity], [",
  data [1, 1],
  "], [",
  data [1, 2],
  "%],\n\t",
  "[Pack Years], [",
  data [2, 1],
  "], [",
  data [2, 2],
  "%],\n\t",
  "[Cough], [",
  data [3, 1],
  "], [",
  data [3, 2],
  "%],\n\t",
  "[Dyspnea], [",
  data [4, 1],
  "], [",
  data [4, 2],
  "%],\n\t",
  "[Wheeze], [",
  data [5, 1],
  "], [",
  data [5, 2],
  "%],\n\t",
  "[Respiratory Disease], [",
  data [6, 1],
  "], [",
  data [6, 2],
  "%],\n\t",
  "[Computed Tomography], [",
  data [7, 1],
  "], [",
  data [7, 2],
  "%],\n\t",
  "[Bronchodilator Response], [",
  data [8, 1],
  "], [",
  data [8, 2],
  "%],\n\t",
  "[Spirometry Effort], [",
  data [9, 1],
  "], [",
  data [9, 2],
  "%],\n\t", 
  "[Residual Volume], [",
  data [10, 1],
  "], [",
  data [10, 2],
  "%],\n\t",
  "[Diffusing Capacity for Carbon Monoxide], [",
  data [11, 1],
  "], [",
  data [11, 2],
  "%],\n\t",
  "[Transfer Factor], [",
  data [12, 1],
  "], [",
  data [12, 2],
  "%],\n\t",
  "table.hline()\n",
  ")", 
  sep = ""
)  
  
write_lines (table, "../tables/e-table-17.txt")

################################################################################
## e-Table 18 - Characteristics by Age
################################################################################

data_18_40 <- pfts %>%
  filter (age >= 18 & age <= 40) %>% 
  filter (fvc_z_score >= -1.645 & fev1_fvc_z_score >= -1.645)

data_41_64 <- pfts %>%
  filter (age >= 41 & age <= 64) %>% 
  filter (fvc_z_score >= -1.645 & fev1_fvc_z_score >= -1.645)

data_65_80 <- pfts %>%
  filter (age >= 65 & age <= 80) %>% 
  filter (fvc_z_score >= -1.645 & fev1_fvc_z_score >= -1.645)

# Age 18 to 40

model_sex_18_40 <- tidy (glm (
  formula = restriction ~ sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_18_40), conf.int = TRUE, exponentiate = TRUE
)

model_bmi_18_40 <- tidy (glm (
  formula = restriction ~ bmi + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_18_40), conf.int = TRUE, exponentiate = TRUE
)

model_race_18_40 <- tidy (glm (
  formula = restriction ~ race + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_18_40), conf.int = TRUE, exponentiate = TRUE
)

model_ethnicity_18_40 <- tidy (glm (
  formula = restriction ~ ethnicity + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_18_40), conf.int = TRUE, exponentiate = TRUE
)

model_pack_years_18_40 <- tidy (glm (
  formula = restriction ~ pack_years + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_18_40), conf.int = TRUE, exponentiate = TRUE
)

model_cough_18_40 <- tidy (glm (
  formula = restriction ~ cough + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_18_40), conf.int = TRUE, exponentiate = TRUE
)

model_dyspnea_18_40 <- tidy (glm (
  formula = restriction ~ dyspnea + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_18_40), conf.int = TRUE, exponentiate = TRUE
)

model_wheeze_18_40 <- tidy (glm (
  formula = restriction ~ wheeze + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_18_40), conf.int = TRUE, exponentiate = TRUE
)

model_asthma_18_40 <- tidy (glm (
  formula = restriction ~ asthma + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_18_40), conf.int = TRUE, exponentiate = TRUE
)

model_bronchiectasis_18_40 <- tidy (glm (
  formula = restriction ~ bronchiectasis + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_18_40), conf.int = TRUE, exponentiate = TRUE
)

model_chest_wall_18_40 <- tidy (glm (
  formula = restriction ~ chest_wall + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_18_40), conf.int = TRUE, exponentiate = TRUE
)

model_copd_18_40 <- tidy (glm (
  formula = restriction ~ copd + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_18_40), conf.int = TRUE, exponentiate = TRUE
)

model_ild_18_40 <- tidy (glm (
  formula = restriction ~ ild + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_18_40), conf.int = TRUE, exponentiate = TRUE
)

model_neuromuscular_18_40 <- tidy (glm (
  formula = restriction ~ neuromuscular + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_18_40), conf.int = TRUE, exponentiate = TRUE
)

model_none_18_40 <- tidy (glm (
  formula = restriction ~ none + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_18_40), conf.int = TRUE, exponentiate = TRUE
)

model_copd_imaging_18_40 <- tidy (glm (
  formula = restriction ~ copd_imaging + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_18_40), conf.int = TRUE, exponentiate = TRUE
)

model_ild_imaging_18_40 <- tidy (glm (
  formula = restriction ~ ild_imaging + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_18_40), conf.int = TRUE, exponentiate = TRUE
)

model_fev1_18_40 <- tidy (glm (
  formula = restriction ~ fev1_z_score + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_18_40), conf.int = TRUE, exponentiate = TRUE
)

model_fvc_18_40 <- tidy (glm (
  formula = restriction ~ fvc_z_score + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_18_40), conf.int = TRUE, exponentiate = TRUE
)

model_fev1_fvc_18_40 <- tidy (glm (
  formula = restriction ~ fev1_fvc_z_score + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_18_40), conf.int = TRUE, exponentiate = TRUE
)

model_effort_18_40 <- tidy (glm (
  formula = restriction ~ effort + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_18_40), conf.int = TRUE, exponentiate = TRUE
)

model_response_18_40 <- tidy (glm (
  formula = restriction ~ response + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_18_40), conf.int = TRUE, exponentiate = TRUE
)

model_rv_18_40 <- tidy (glm (
  formula = restriction ~ rv_z_score + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_18_40), conf.int = TRUE, exponentiate = TRUE
)

model_dlco_18_40 <- tidy (glm (
  formula = restriction ~ dlco_z_score + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_18_40), conf.int = TRUE, exponentiate = TRUE
)

model_kco_18_40 <- tidy (glm (
  formula = restriction ~ kco_z_score + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_18_40), conf.int = TRUE, exponentiate = TRUE
)

# Age 41 to 64

model_sex_41_64 <- tidy (glm (
  formula = restriction ~ sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_41_64), conf.int = TRUE, exponentiate = TRUE
)

model_bmi_41_64 <- tidy (glm (
  formula = restriction ~ bmi + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_41_64), conf.int = TRUE, exponentiate = TRUE
)

model_race_41_64 <- tidy (glm (
  formula = restriction ~ race + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_41_64), conf.int = TRUE, exponentiate = TRUE
)

model_ethnicity_41_64 <- tidy (glm (
  formula = restriction ~ ethnicity + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_41_64), conf.int = TRUE, exponentiate = TRUE
)

model_pack_years_41_64 <- tidy (glm (
  formula = restriction ~ pack_years + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_41_64), conf.int = TRUE, exponentiate = TRUE
)

model_cough_41_64 <- tidy (glm (
  formula = restriction ~ cough + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_41_64), conf.int = TRUE, exponentiate = TRUE
)

model_dyspnea_41_64 <- tidy (glm (
  formula = restriction ~ dyspnea + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_41_64), conf.int = TRUE, exponentiate = TRUE
)

model_wheeze_41_64 <- tidy (glm (
  formula = restriction ~ wheeze + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_41_64), conf.int = TRUE, exponentiate = TRUE
)

model_asthma_41_64 <- tidy (glm (
  formula = restriction ~ asthma + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_41_64), conf.int = TRUE, exponentiate = TRUE
)

model_bronchiectasis_41_64 <- tidy (glm (
  formula = restriction ~ bronchiectasis + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_41_64), conf.int = TRUE, exponentiate = TRUE
)

model_chest_wall_41_64 <- tidy (glm (
  formula = restriction ~ chest_wall + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_41_64), conf.int = TRUE, exponentiate = TRUE
)

model_copd_41_64 <- tidy (glm (
  formula = restriction ~ copd + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_41_64), conf.int = TRUE, exponentiate = TRUE
)

model_ild_41_64 <- tidy (glm (
  formula = restriction ~ ild + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_41_64), conf.int = TRUE, exponentiate = TRUE
)

model_neuromuscular_41_64 <- tidy (glm (
  formula = restriction ~ neuromuscular + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_41_64), conf.int = TRUE, exponentiate = TRUE
)

model_none_41_64 <- tidy (glm (
  formula = restriction ~ none + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_41_64), conf.int = TRUE, exponentiate = TRUE
)

model_copd_imaging_41_64 <- tidy (glm (
  formula = restriction ~ copd_imaging + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_41_64), conf.int = TRUE, exponentiate = TRUE
)

model_ild_imaging_41_64 <- tidy (glm (
  formula = restriction ~ ild_imaging + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_41_64), conf.int = TRUE, exponentiate = TRUE
)

model_fev1_41_64 <- tidy (glm (
  formula = restriction ~ fev1_z_score + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_41_64), conf.int = TRUE, exponentiate = TRUE
)

model_fvc_41_64 <- tidy (glm (
  formula = restriction ~ fvc_z_score + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_41_64), conf.int = TRUE, exponentiate = TRUE
)

model_fev1_fvc_41_64 <- tidy (glm (
  formula = restriction ~ fev1_fvc_z_score + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_41_64), conf.int = TRUE, exponentiate = TRUE
)

model_effort_41_64 <- tidy (glm (
  formula = restriction ~ effort + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_41_64), conf.int = TRUE, exponentiate = TRUE
)

model_response_41_64 <- tidy (glm (
  formula = restriction ~ response + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_41_64), conf.int = TRUE, exponentiate = TRUE
)

model_rv_41_64 <- tidy (glm (
  formula = restriction ~ rv_z_score + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_41_64), conf.int = TRUE, exponentiate = TRUE
)

model_dlco_41_64 <- tidy (glm (
  formula = restriction ~ dlco_z_score + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_41_64), conf.int = TRUE, exponentiate = TRUE
)

model_kco_41_64 <- tidy (glm (
  formula = restriction ~ kco_z_score + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_41_64), conf.int = TRUE, exponentiate = TRUE
)

# age 65 to 80

model_sex_65_80 <- tidy (glm (
  formula = restriction ~ sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_65_80), conf.int = TRUE, exponentiate = TRUE
)

model_bmi_65_80 <- tidy (glm (
  formula = restriction ~ bmi + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_65_80), conf.int = TRUE, exponentiate = TRUE
)

model_race_65_80 <- tidy (glm (
  formula = restriction ~ race + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_65_80), conf.int = TRUE, exponentiate = TRUE
)

model_ethnicity_65_80 <- tidy (glm (
  formula = restriction ~ ethnicity + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_65_80), conf.int = TRUE, exponentiate = TRUE
)

model_pack_years_65_80 <- tidy (glm (
  formula = restriction ~ pack_years + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_65_80), conf.int = TRUE, exponentiate = TRUE
)

model_cough_65_80 <- tidy (glm (
  formula = restriction ~ cough + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_65_80), conf.int = TRUE, exponentiate = TRUE
)

model_dyspnea_65_80 <- tidy (glm (
  formula = restriction ~ dyspnea + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_65_80), conf.int = TRUE, exponentiate = TRUE
)

model_wheeze_65_80 <- tidy (glm (
  formula = restriction ~ wheeze + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_65_80), conf.int = TRUE, exponentiate = TRUE
)

model_asthma_65_80 <- tidy (glm (
  formula = restriction ~ asthma + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_65_80), conf.int = TRUE, exponentiate = TRUE
)

model_bronchiectasis_65_80 <- tidy (glm (
  formula = restriction ~ bronchiectasis + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_65_80), conf.int = TRUE, exponentiate = TRUE
)

model_chest_wall_65_80 <- tidy (glm (
  formula = restriction ~ chest_wall + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_65_80), conf.int = TRUE, exponentiate = TRUE
)

model_copd_65_80 <- tidy (glm (
  formula = restriction ~ copd + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_65_80), conf.int = TRUE, exponentiate = TRUE
)

model_ild_65_80 <- tidy (glm (
  formula = restriction ~ ild + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_65_80), conf.int = TRUE, exponentiate = TRUE
)

model_neuromuscular_65_80 <- tidy (glm (
  formula = restriction ~ neuromuscular + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_65_80), conf.int = TRUE, exponentiate = TRUE
)

model_none_65_80 <- tidy (glm (
  formula = restriction ~ none + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_65_80), conf.int = TRUE, exponentiate = TRUE
)

model_copd_imaging_65_80 <- tidy (glm (
  formula = restriction ~ copd_imaging + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_65_80), conf.int = TRUE, exponentiate = TRUE
)

model_ild_imaging_65_80 <- tidy (glm (
  formula = restriction ~ ild_imaging + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_65_80), conf.int = TRUE, exponentiate = TRUE
)

model_fev1_65_80 <- tidy (glm (
  formula = restriction ~ fev1_z_score + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_65_80), conf.int = TRUE, exponentiate = TRUE
)

model_fvc_65_80 <- tidy (glm (
  formula = restriction ~ fvc_z_score + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_65_80), conf.int = TRUE, exponentiate = TRUE
)

model_fev1_fvc_65_80 <- tidy (glm (
  formula = restriction ~ fev1_fvc_z_score + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_65_80), conf.int = TRUE, exponentiate = TRUE
)

model_effort_65_80 <- tidy (glm (
  formula = restriction ~ effort + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_65_80), conf.int = TRUE, exponentiate = TRUE
)

model_response_65_80 <- tidy (glm (
  formula = restriction ~ response + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_65_80), conf.int = TRUE, exponentiate = TRUE
)

model_rv_65_80 <- tidy (glm (
  formula = restriction ~ rv_z_score + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_65_80), conf.int = TRUE, exponentiate = TRUE
)

model_dlco_65_80 <- tidy (glm (
  formula = restriction ~ dlco_z_score + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_65_80), conf.int = TRUE, exponentiate = TRUE
)

model_kco_65_80 <- tidy (glm (
  formula = restriction ~ kco_z_score + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_65_80), conf.int = TRUE, exponentiate = TRUE
)

table <- paste (
  
  "#table(\n\t",
  "inset: (x: 4pt, y: 3.5pt),\n\t",
	"columns: 6,\n\t",
	"align: (left, center, center, center, center, center),\n\t",
	"table.hline(),\n\t",
	"[], table.cell(colspan: 5, [*Adjusted Odds Ratio (95% CI)#super[a]*]),\n\t",
	"table.hline(start:1, end:6),\n\t",
	"[], [*18--40 Years*], [], [*41--64 Years*], [], [*65--80 Years*],\n\t",
  "table.hline(),\n\t",
  "[*Sex*], [], [], [], [], [],\n\t",
  "[#h(1em)Male], [1.00 (Reference)], [], [1.00 (Reference)], [], [1.00 (Reference)],\n\t",
  "[#h(1em)Female], [",
  format (round (model_sex_18_40 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_sex_18_40 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_sex_18_40 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [], [",
  format (round (model_sex_41_64 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_sex_41_64 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_sex_41_64 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [], [",
  format (round (model_sex_65_80 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_sex_65_80 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_sex_65_80 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")],\n\t",
  "[*Race*], [], [], [], [], [],\n\t",
  "[#h(1em)White], [1.00 (Reference)], [], [1.00 (Reference)], [], [1.00 (Reference)],\n\t",
  "[#h(1em)Black], [",
  format (round (model_race_18_40 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_race_18_40 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_race_18_40 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [], [",
  format (round (model_race_41_64 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_race_41_64 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_race_41_64 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [], [",
  format (round (model_race_65_80 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_race_65_80 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_race_65_80 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")],\n\t",
  "[#h(1em)Asian], [",
  format (round (model_race_18_40 [[3, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_race_18_40 [[3, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_race_18_40 [[3, "conf.high"]], digits = 2), nsmall = 2),
  ")], [], [",
  format (round (model_race_41_64 [[3, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_race_41_64 [[3, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_race_41_64 [[3, "conf.high"]], digits = 2), nsmall = 2),
  ")], [], [",
  format (round (model_race_65_80 [[3, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_race_65_80 [[3, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_race_65_80 [[3, "conf.high"]], digits = 2), nsmall = 2),
  ")],\n\t",
  "[#h(1em)Other], [",
  format (round (model_race_18_40 [[4, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_race_18_40 [[4, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_race_18_40 [[4, "conf.high"]], digits = 2), nsmall = 2),
  ")], [], [",
  format (round (model_race_41_64 [[4, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_race_41_64 [[4, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_race_41_64 [[4, "conf.high"]], digits = 2), nsmall = 2),
  ")], [], [",
  format (round (model_race_65_80 [[4, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_race_65_80 [[4, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_race_65_80 [[4, "conf.high"]], digits = 2), nsmall = 2),
  ")],\n\t",
  "[*Ethnicity*], [], [], [], [], [],\n\t",
  "[#h(1em)Hispanic], [1.00 (Reference)], [], [1.00 (Reference)], [], [1.00 (Reference)],\n\t",
  "[#h(1em)Not Hispanic], [",
  format (round (model_ethnicity_18_40 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_ethnicity_18_40 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_ethnicity_18_40 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [], [",
  format (round (model_ethnicity_41_64 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_ethnicity_41_64 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_ethnicity_41_64 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [], [",
  format (round (model_ethnicity_65_80 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_ethnicity_65_80 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_ethnicity_65_80 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")],\n\t",  
  "[*Pack Years, yrs*], [",
  format (round (model_pack_years_18_40 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_pack_years_18_40 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_pack_years_18_40 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [], [",
  format (round (model_pack_years_41_64 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_pack_years_41_64 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_pack_years_41_64 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [], [",
  format (round (model_pack_years_65_80 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_pack_years_65_80 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_pack_years_65_80 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")],\n\t",  
  "[*Respiratory Symptoms*], [], [], [], [], [],\n\t",
  "[#h(1em)Cough], [",
  format (round (model_cough_18_40 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_cough_18_40 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_cough_18_40 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [], [",
  format (round (model_cough_41_64 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_cough_41_64 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_cough_41_64 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [], [",
  format (round (model_cough_65_80 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_cough_65_80 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_cough_65_80 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")],\n\t",
  "[#h(1em)Dyspnea], [",
  format (round (model_dyspnea_18_40 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_dyspnea_18_40 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_dyspnea_18_40 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [], [",
  format (round (model_dyspnea_41_64 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_dyspnea_41_64 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_dyspnea_41_64 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [], [",
  format (round (model_dyspnea_65_80 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_dyspnea_65_80 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_dyspnea_65_80 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")],\n\t",
  "[#h(1em)Wheeze], [",
  format (round (model_wheeze_18_40 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_wheeze_18_40 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_wheeze_18_40 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [], [",
  format (round (model_wheeze_41_64 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_wheeze_41_64 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_wheeze_41_64 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [], [",
  format (round (model_wheeze_65_80 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_wheeze_65_80 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_wheeze_65_80 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")],\n\t",    
  "[*Respiratory Diseases*], [], [], [], [], [],\n\t",
  "[#h(1em)Asthma], [",
  format (round (model_asthma_18_40 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_asthma_18_40 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_asthma_18_40 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [], [",
  format (round (model_asthma_41_64 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_asthma_41_64 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_asthma_41_64 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [], [",
  format (round (model_asthma_65_80 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_asthma_65_80 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_asthma_65_80 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")],\n\t",
  "[#h(1em)Bronchiectasis], [",
  format (round (model_bronchiectasis_18_40 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_bronchiectasis_18_40 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_bronchiectasis_18_40 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [], [",
  format (round (model_bronchiectasis_41_64 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_bronchiectasis_41_64 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_bronchiectasis_41_64 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [], [",
  format (round (model_bronchiectasis_65_80 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_bronchiectasis_65_80 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_bronchiectasis_65_80 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")],\n\t",
  "[#h(1em)Chest Wall Disorder], [",
  format (round (model_chest_wall_18_40 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_chest_wall_18_40 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_chest_wall_18_40 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [], [",
  format (round (model_chest_wall_41_64 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_chest_wall_41_64 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_chest_wall_41_64 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [], [",
  format (round (model_chest_wall_65_80 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_chest_wall_65_80 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_chest_wall_65_80 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")],\n\t",
  "[#h(1em)Chronic Obstructive Pulmonary Disease], [",
  format (round (model_copd_18_40 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_copd_18_40 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_copd_18_40 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [], [",
  format (round (model_copd_41_64 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_copd_41_64 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_copd_41_64 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [], [",
  format (round (model_copd_65_80 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_copd_65_80 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_copd_65_80 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")],\n\t",
  "[#h(1em)Interstitial Lung Disease], [",
  format (round (model_ild_18_40 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_ild_18_40 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_ild_18_40 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [], [",
  format (round (model_ild_41_64 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_ild_41_64 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_ild_41_64 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [], [",
  format (round (model_ild_65_80 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_ild_65_80 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_ild_65_80 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")],\n\t",
  "[#h(1em)Neuromuscular Disease], [",
  format (round (model_neuromuscular_18_40 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_neuromuscular_18_40 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_neuromuscular_18_40 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [], [",
  format (round (model_neuromuscular_41_64 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_neuromuscular_41_64 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_neuromuscular_41_64 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [], [",
  format (round (model_neuromuscular_65_80 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_neuromuscular_65_80 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_neuromuscular_65_80 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")],\n\t",     
  "[#h(1em)None], [",
  format (round (model_none_18_40 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_none_18_40 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_none_18_40 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [], [",
  format (round (model_none_41_64 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_none_41_64 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_none_41_64 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [], [",
  format (round (model_none_65_80 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_none_65_80 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_none_65_80 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")],\n\t",     
  "[*Computed Tomography Findings*], [], [], [], [], [],\n\t",
  "[#h(1em)Chronic Obstructive Pulmonary Disease], [",
  format (round (model_copd_imaging_18_40 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_copd_imaging_18_40 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_copd_imaging_18_40 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [], [",
  format (round (model_copd_imaging_41_64 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_copd_imaging_41_64 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_copd_imaging_41_64 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [], [",
  format (round (model_copd_imaging_65_80 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_copd_imaging_65_80 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_copd_imaging_65_80 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")],\n\t",
  "[#h(1em)Interstitial Lung Disease], [",
  format (round (model_ild_imaging_18_40 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_ild_imaging_18_40 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_ild_imaging_18_40 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [], [",
  format (round (model_ild_imaging_41_64 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_ild_imaging_41_64 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_ild_imaging_41_64 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [], [",
  format (round (model_ild_imaging_65_80 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_ild_imaging_65_80 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_ild_imaging_65_80 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")],\n\t",
  "[*Dynamic Lung Volumes*], [], [], [], [], [],\n\t",
  "[#h(1em)FEV#sub[1], z-score], [",
  format (round (model_fev1_18_40 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_fev1_18_40 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_fev1_18_40 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [], [",
  format (round (model_fev1_41_64 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_fev1_41_64 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_fev1_41_64 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [], [",
  format (round (model_fev1_65_80 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_fev1_65_80 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_fev1_65_80 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")],\n\t",
  "[#h(1em)FVC, z-score], [",
  format (round (model_fvc_18_40 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_fvc_18_40 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_fvc_18_40 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [], [",
  format (round (model_fvc_41_64 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_fvc_41_64 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_fvc_41_64 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [], [",
  format (round (model_fvc_65_80 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_fvc_65_80 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_fvc_65_80 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")],\n\t",
  "[#h(1em)FEV#sub[1]/FVC, z-score], [",
  format (round (model_fev1_fvc_18_40 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_fev1_fvc_18_40 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_fev1_fvc_18_40 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [], [",
  format (round (model_fev1_fvc_41_64 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_fev1_fvc_41_64 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_fev1_fvc_41_64 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [], [",
  format (round (model_fev1_fvc_65_80 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_fev1_fvc_65_80 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_fev1_fvc_65_80 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")],\n\t",   
  "[#h(1em)Bronchodilator Response], [",
  format (round (model_response_18_40 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_response_18_40 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_response_18_40 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [], [",
  format (round (model_response_41_64 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_response_41_64 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_response_41_64 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [], [",
  format (round (model_response_65_80 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_response_65_80 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_response_65_80 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")],\n\t",
  "[#h(1em)Adequate Effort], [",
  format (round (model_effort_18_40 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_effort_18_40 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_effort_18_40 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [], [",
  format (round (model_effort_41_64 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_effort_41_64 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_effort_41_64 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [], [",
  format (round (model_effort_65_80 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_effort_65_80 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_effort_65_80 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")],\n\t",     
  "[*Static Lung Volumes*], [], [], [], [], [],\n\t",
  "[#h(1em)RV, z-score], [",
  format (round (model_rv_18_40 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_rv_18_40 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_rv_18_40 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [], [",
  format (round (model_rv_41_64 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_rv_41_64 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_rv_41_64 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [], [",
  format (round (model_rv_65_80 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_rv_65_80 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_rv_65_80 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")],\n\t",
  "[*Diffusing Capacity*], [], [], [], [], [],\n\t",
  "[#h(1em)D#sub[LCO], z-score], [",
  format (round (model_dlco_18_40 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_dlco_18_40 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_dlco_18_40 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [], [",
  format (round (model_dlco_41_64 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_dlco_41_64 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_dlco_41_64 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [], [",
  format (round (model_dlco_65_80 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_dlco_65_80 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_dlco_65_80 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")],\n\t",
  "[#h(1em)K#sub[CO], z-score], [",
  format (round (model_kco_18_40 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_kco_18_40 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_kco_18_40 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [], [",
  format (round (model_kco_41_64 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_kco_41_64 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_kco_41_64 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [], [",
  format (round (model_kco_65_80 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_kco_65_80 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_kco_65_80 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")],\n\t",   
	"table.hline(),\n",
  ")", 
  sep = ""
  
)

write_lines (table, "../tables/e-table-18.txt")

################################################################################
## e-Table 19 - Odds of Characteristics by Sex
################################################################################

data_male <- pfts %>%
  filter (sex == 1) %>% 
  filter (fvc_z_score >= -1.645 & fev1_fvc_z_score >= -1.645)

data_female <- pfts %>%
  filter (sex == 2) %>% 
  filter (fvc_z_score >= -1.645 & fev1_fvc_z_score >= -1.645)

# Male

model_age_male <- tidy (glm (
  formula = restriction ~ age + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_male), conf.int = TRUE, exponentiate = TRUE
)

model_bmi_male <- tidy (glm (
  formula = restriction ~ bmi + age + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_male), conf.int = TRUE, exponentiate = TRUE
)

model_race_male <- tidy (glm (
  formula = restriction ~ race + age + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_male), conf.int = TRUE, exponentiate = TRUE
)

model_ethnicity_male <- tidy (glm (
  formula = restriction ~ ethnicity + age + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_male), conf.int = TRUE, exponentiate = TRUE
)

model_pack_years_male <- tidy (glm (
  formula = restriction ~ pack_years + age + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_male), conf.int = TRUE, exponentiate = TRUE
)

model_cough_male <- tidy (glm (
  formula = restriction ~ cough + age + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_male), conf.int = TRUE, exponentiate = TRUE
)

model_dyspnea_male <- tidy (glm (
  formula = restriction ~ dyspnea + age + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_male), conf.int = TRUE, exponentiate = TRUE
)

model_wheeze_male <- tidy (glm (
  formula = restriction ~ wheeze + age + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_male), conf.int = TRUE, exponentiate = TRUE
)

model_asthma_male <- tidy (glm (
  formula = restriction ~ asthma + age + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_male), conf.int = TRUE, exponentiate = TRUE
)

model_bronchiectasis_male <- tidy (glm (
  formula = restriction ~ bronchiectasis + age + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_male), conf.int = TRUE, exponentiate = TRUE
)

model_chest_wall_male <- tidy (glm (
  formula = restriction ~ chest_wall + age + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_male), conf.int = TRUE, exponentiate = TRUE
)

model_copd_male <- tidy (glm (
  formula = restriction ~ copd + age + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_male), conf.int = TRUE, exponentiate = TRUE
)

model_ild_male <- tidy (glm (
  formula = restriction ~ ild + age + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_male), conf.int = TRUE, exponentiate = TRUE
)

model_neuromuscular_male <- tidy (glm (
  formula = restriction ~ neuromuscular + age + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_male), conf.int = TRUE, exponentiate = TRUE
)

model_none_male <- tidy (glm (
  formula = restriction ~ none + age + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_male), conf.int = TRUE, exponentiate = TRUE
)

model_copd_imaging_male <- tidy (glm (
  formula = restriction ~ copd_imaging + age + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_male), conf.int = TRUE, exponentiate = TRUE
)

model_ild_imaging_male <- tidy (glm (
  formula = restriction ~ ild_imaging + age + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_male), conf.int = TRUE, exponentiate = TRUE
)

model_fev1_male <- tidy (glm (
  formula = restriction ~ fev1_z_score + age + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_male), conf.int = TRUE, exponentiate = TRUE
)

model_fvc_male <- tidy (glm (
  formula = restriction ~ fvc_z_score + age + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_male), conf.int = TRUE, exponentiate = TRUE
)

model_fev1_fvc_male <- tidy (glm (
  formula = restriction ~ fev1_fvc_z_score + age + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_male), conf.int = TRUE, exponentiate = TRUE
)

model_effort_male <- tidy (glm (
  formula = restriction ~ effort + age + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_male), conf.int = TRUE, exponentiate = TRUE
)

model_response_male <- tidy (glm (
  formula = restriction ~ response + age + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_male), conf.int = TRUE, exponentiate = TRUE
)

model_rv_male <- tidy (glm (
  formula = restriction ~ rv_z_score + age + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_male), conf.int = TRUE, exponentiate = TRUE
)

model_dlco_male <- tidy (glm (
  formula = restriction ~ dlco_z_score + age + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_male), conf.int = TRUE, exponentiate = TRUE
)

model_kco_male <- tidy (glm (
  formula = restriction ~ kco_z_score + age + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_male), conf.int = TRUE, exponentiate = TRUE
)

# Female

model_age_female <- tidy (glm (
  formula = restriction ~ age + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_female), conf.int = TRUE, exponentiate = TRUE
)

model_bmi_female <- tidy (glm (
  formula = restriction ~ bmi + age + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_female), conf.int = TRUE, exponentiate = TRUE
)

model_race_female <- tidy (glm (
  formula = restriction ~ race + age + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_female), conf.int = TRUE, exponentiate = TRUE
)

model_ethnicity_female <- tidy (glm (
  formula = restriction ~ ethnicity + age + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_female), conf.int = TRUE, exponentiate = TRUE
)

model_pack_years_female <- tidy (glm (
  formula = restriction ~ pack_years + age + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_female), conf.int = TRUE, exponentiate = TRUE
)

model_cough_female <- tidy (glm (
  formula = restriction ~ cough + age + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_female), conf.int = TRUE, exponentiate = TRUE
)

model_dyspnea_female <- tidy (glm (
  formula = restriction ~ dyspnea + age + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_female), conf.int = TRUE, exponentiate = TRUE
)

model_wheeze_female <- tidy (glm (
  formula = restriction ~ wheeze + age + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_female), conf.int = TRUE, exponentiate = TRUE
)

model_asthma_female <- tidy (glm (
  formula = restriction ~ asthma + age + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_female), conf.int = TRUE, exponentiate = TRUE
)

model_bronchiectasis_female <- tidy (glm (
  formula = restriction ~ bronchiectasis + age + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_female), conf.int = TRUE, exponentiate = TRUE
)

model_chest_wall_female <- tidy (glm (
  formula = restriction ~ chest_wall + age + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_female), conf.int = TRUE, exponentiate = TRUE
)

model_copd_female <- tidy (glm (
  formula = restriction ~ copd + age + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_female), conf.int = TRUE, exponentiate = TRUE
)

model_ild_female <- tidy (glm (
  formula = restriction ~ ild + age + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_female), conf.int = TRUE, exponentiate = TRUE
)

model_neuromuscular_female <- tidy (glm (
  formula = restriction ~ neuromuscular + age + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_female), conf.int = TRUE, exponentiate = TRUE
)

model_none_female <- tidy (glm (
  formula = restriction ~ none + age + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_female), conf.int = TRUE, exponentiate = TRUE
)

model_copd_imaging_female <- tidy (glm (
  formula = restriction ~ copd_imaging + age + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_female), conf.int = TRUE, exponentiate = TRUE
)

model_ild_imaging_female <- tidy (glm (
  formula = restriction ~ ild_imaging + age + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_female), conf.int = TRUE, exponentiate = TRUE
)

model_fev1_female <- tidy (glm (
  formula = restriction ~ fev1_z_score + age + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_female), conf.int = TRUE, exponentiate = TRUE
)

model_fvc_female <- tidy (glm (
  formula = restriction ~ fvc_z_score + age + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_female), conf.int = TRUE, exponentiate = TRUE
)

model_fev1_fvc_female <- tidy (glm (
  formula = restriction ~ fev1_fvc_z_score + age + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_female), conf.int = TRUE, exponentiate = TRUE
)

model_effort_female <- tidy (glm (
  formula = restriction ~ effort + age + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_female), conf.int = TRUE, exponentiate = TRUE
)

model_response_female <- tidy (glm (
  formula = restriction ~ response + age + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_female), conf.int = TRUE, exponentiate = TRUE
)

model_rv_female <- tidy (glm (
  formula = restriction ~ rv_z_score + age + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_female), conf.int = TRUE, exponentiate = TRUE
)

model_dlco_female <- tidy (glm (
  formula = restriction ~ dlco_z_score + age + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_female), conf.int = TRUE, exponentiate = TRUE
)

model_kco_female <- tidy (glm (
  formula = restriction ~ kco_z_score + age + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_female), conf.int = TRUE, exponentiate = TRUE
)

table <- paste (
  
  "#table(\n\t",
  "inset: (x: 4pt, y: 4pt),\n\t",
	"columns: 3,\n\t",
	"align: (left, center, center),\n\t",
	"table.hline(),\n\t",
	"[], table.cell(colspan: 2, [*Adjusted Odds Ratio (95% CI)#super[a]*]),\n\t",
	"table.hline(start:1, end:3),\n\t",
	"[], [*Male*], [*Female*],\n\t",
  "table.hline(),\n\t",
  "[*Age, yrs*], [",
  format (round (model_pack_years_male [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_pack_years_male [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_pack_years_male [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_pack_years_female [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_pack_years_female [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_pack_years_female [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")],\n\t",  
  "[*Race*], [], [],\n\t",
  "[#h(1em)White], [1.00 (Reference)], [1.00 (Reference)],\n\t",
  "[#h(1em)Black], [",
  format (round (model_race_male [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_race_male [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_race_male [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_race_female [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_race_female [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_race_female [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")],\n\t",
  "[#h(1em)Asian], [",
  format (round (model_race_male [[3, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_race_male [[3, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_race_male [[3, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_race_female [[3, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_race_female [[3, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_race_female [[3, "conf.high"]], digits = 2), nsmall = 2),
  ")],\n\t",
  "[#h(1em)Other], [",
  format (round (model_race_male [[4, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_race_male [[4, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_race_male [[4, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_race_female [[4, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_race_female [[4, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_race_female [[4, "conf.high"]], digits = 2), nsmall = 2),
  ")],\n\t",
  "[*Ethniicty*], [], [],\n\t",
  "[#h(1em)Hispanic], [1.00 (Reference)], [1.00 (Reference)],\n\t",
  "[#h(1em)Not Hispanic], [",
  format (round (model_race_male [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_race_male [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_race_male [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_race_female [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_race_female [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_race_female [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")],\n\t",  
  "[*Pack Years, yrs*], [",
  format (round (model_pack_years_male [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_pack_years_male [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_pack_years_male [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_pack_years_female [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_pack_years_female [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_pack_years_female [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")],\n\t",  
  "[*Respiratory Symptoms*], [], [],\n\t",
  "[#h(1em)Cough], [",
  format (round (model_cough_male [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_cough_male [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_cough_male [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_cough_female [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_cough_female [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_cough_female [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")],\n\t",
  "[#h(1em)Dyspnea], [",
  format (round (model_dyspnea_male [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_dyspnea_male [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_dyspnea_male [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_dyspnea_female [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_dyspnea_female [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_dyspnea_female [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")],\n\t",
  "[#h(1em)Wheeze], [",
  format (round (model_wheeze_male [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_wheeze_male [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_wheeze_male [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_wheeze_female [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_wheeze_female [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_wheeze_female [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")],\n\t",    
  "[*Respiratory Diseases*], [], [],\n\t",
  "[#h(1em)Asthma], [",
  format (round (model_asthma_male [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_asthma_male [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_asthma_male [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_asthma_female [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_asthma_female [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_asthma_female [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")],\n\t",
  "[#h(1em)Bronchiectasis], [",
  format (round (model_bronchiectasis_male [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_bronchiectasis_male [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_bronchiectasis_male [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_bronchiectasis_female [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_bronchiectasis_female [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_bronchiectasis_female [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")],\n\t",
  "[#h(1em)Chest Wall Disorder], [",
  format (round (model_chest_wall_male [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_chest_wall_male [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_chest_wall_male [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_chest_wall_female [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_chest_wall_female [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_chest_wall_female [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")],\n\t",
  "[#h(1em)Chronic Obstructive Pulmonary Disease], [",
  format (round (model_copd_male [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_copd_male [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_copd_male [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_copd_female [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_copd_female [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_copd_female [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")],\n\t",
  "[#h(1em)Interstitial Lung Disease], [",
  format (round (model_ild_male [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_ild_male [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_ild_male [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_ild_female [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_ild_female [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_ild_female [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")],\n\t",
  "[#h(1em)Neuromuscular Disease], [",
  format (round (model_neuromuscular_male [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_neuromuscular_male [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_neuromuscular_male [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_neuromuscular_female [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_neuromuscular_female [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_neuromuscular_female [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")],\n\t",     
  "[#h(1em)None], [",
  format (round (model_none_male [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_none_male [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_none_male [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_none_female [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_none_female [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_none_female [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")],\n\t",
  "[*Computed Tomography Findings*], [], [],\n\t",
  "[#h(1em)Chronic Obstructive Pulmonary Disease], [",
  format (round (model_copd_imaging_male [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_copd_imaging_male [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_copd_imaging_male [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_copd_imaging_female [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_copd_imaging_female [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_copd_imaging_female [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")],\n\t",
  "[#h(1em)Interstitial Lung Disease], [",
  format (round (model_ild_imaging_male [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_ild_imaging_male [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_ild_imaging_male [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_ild_imaging_female [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_ild_imaging_female [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_ild_imaging_female [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")],\n\t",     
  "[*Dynamic Lung Volumes*], [], [],\n\t",
  "[#h(1em)FEV#sub[1], z-score], [",
  format (round (model_fev1_male [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_fev1_male [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_fev1_male [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_fev1_female [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_fev1_female [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_fev1_female [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")],\n\t",
  "[#h(1em)FVC, z-score], [",
  format (round (model_fvc_male [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_fvc_male [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_fvc_male [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_fvc_female [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_fvc_female [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_fvc_female [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")],\n\t",
  "[#h(1em)FEV#sub[1]/FVC, z-score], [",
  format (round (model_fev1_fvc_male [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_fev1_fvc_male [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_fev1_fvc_male [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_fev1_fvc_female [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_fev1_fvc_female [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_fev1_fvc_female [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")],\n\t",   
  "[#h(1em)Bronchodilator Response], [",
  format (round (model_response_male [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_response_male [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_response_male [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_response_female [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_response_female [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_response_female [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")],\n\t",
  "[#h(1em)Adequate Effort], [",
  format (round (model_effort_male [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_effort_male [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_effort_male [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_effort_female [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_effort_female [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_effort_female [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")],\n\t",     
  "[*Static Lung Volumes*], [], [],\n\t",
  "[#h(1em)RV, z-score], [",
  format (round (model_rv_male [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_rv_male [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_rv_male [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_rv_female [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_rv_female [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_rv_female [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")],\n\t",
  "[*Diffusing Capacity*], [], [],\n\t",
  "[#h(1em)D#sub[LCO], z-score], [",
  format (round (model_dlco_male [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_dlco_male [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_dlco_male [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_dlco_female [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_dlco_female [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_dlco_female [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")],\n\t",
  "[#h(1em)K#sub[CO], z-score], [",
  format (round (model_kco_male [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_kco_male [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_kco_male [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_kco_female [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_kco_female [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_kco_female [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")],\n\t",   
	"table.hline(),\n",
  ")", 
  sep = ""
  
)

write_lines (table, "../tables/e-table-19.txt")

################################################################################
## e-Table 20 - Odds of Characteristics by Race
################################################################################

data_white <- pfts %>%
  filter (race == 1) %>% 
  filter (fvc_z_score >= -1.645 & fev1_fvc_z_score >= -1.645)

data_black <- pfts %>%
  filter (race == 2) %>% 
  filter (fvc_z_score >= -1.645 & fev1_fvc_z_score >= -1.645)

data_asian <- pfts %>%
  filter (race == 3) %>% 
  filter (fvc_z_score >= -1.645 & fev1_fvc_z_score >= -1.645)

data_other <- pfts %>%
  filter (race == 4) %>% 
  filter (fvc_z_score >= -1.645 & fev1_fvc_z_score >= -1.645)

# White

model_age_white <- tidy (glm (
  formula = restriction ~ age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_white), conf.int = TRUE, exponentiate = TRUE
)

model_sex_white <- tidy (glm (
  formula = restriction ~ sex + age + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_white), conf.int = TRUE, exponentiate = TRUE
)

model_ethnicity_white <- tidy (glm (
  formula = restriction ~ ethnicity + age + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_white), conf.int = TRUE, exponentiate = TRUE
)

model_bmi_white <- tidy (glm (
  formula = restriction ~ bmi + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_white), conf.int = TRUE, exponentiate = TRUE
)

model_pack_years_white <- tidy (glm (
  formula = restriction ~ pack_years + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_white), conf.int = TRUE, exponentiate = TRUE
)

model_cough_white <- tidy (glm (
  formula = restriction ~ cough + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_white), conf.int = TRUE, exponentiate = TRUE
)

model_dyspnea_white <- tidy (glm (
  formula = restriction ~ dyspnea + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_white), conf.int = TRUE, exponentiate = TRUE
)

model_wheeze_white <- tidy (glm (
  formula = restriction ~ wheeze + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_white), conf.int = TRUE, exponentiate = TRUE
)

model_asthma_white <- tidy (glm (
  formula = restriction ~ asthma + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_white), conf.int = TRUE, exponentiate = TRUE
)

model_bronchiectasis_white <- tidy (glm (
  formula = restriction ~ bronchiectasis + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_white), conf.int = TRUE, exponentiate = TRUE
)

model_chest_wall_white <- tidy (glm (
  formula = restriction ~ chest_wall + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_white), conf.int = TRUE, exponentiate = TRUE
)

model_copd_white <- tidy (glm (
  formula = restriction ~ copd + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_white), conf.int = TRUE, exponentiate = TRUE
)

model_ild_white <- tidy (glm (
  formula = restriction ~ ild + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_white), conf.int = TRUE, exponentiate = TRUE
)

model_neuromuscular_white <- tidy (glm (
  formula = restriction ~ neuromuscular + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_white), conf.int = TRUE, exponentiate = TRUE
)

model_none_white <- tidy (glm (
  formula = restriction ~ neuromuscular + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_white), conf.int = TRUE, exponentiate = TRUE
)

model_copd_imaging_white <- tidy (glm (
  formula = restriction ~ copd_imaging + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_white), conf.int = TRUE, exponentiate = TRUE
)

model_ild_imaging_white <- tidy (glm (
  formula = restriction ~ ild_imaging + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_white), conf.int = TRUE, exponentiate = TRUE
)

model_fev1_white <- tidy (glm (
  formula = restriction ~ fev1_z_score + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_white), conf.int = TRUE, exponentiate = TRUE
)

model_fvc_white <- tidy (glm (
  formula = restriction ~ fvc_z_score + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_white), conf.int = TRUE, exponentiate = TRUE
)

model_fev1_fvc_white <- tidy (glm (
  formula = restriction ~ fev1_fvc_z_score + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_white), conf.int = TRUE, exponentiate = TRUE
)

model_effort_white <- tidy (glm (
  formula = restriction ~ effort + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_white), conf.int = TRUE, exponentiate = TRUE
)

model_response_white <- tidy (glm (
  formula = restriction ~ response + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_white), conf.int = TRUE, exponentiate = TRUE
)

model_rv_white <- tidy (glm (
  formula = restriction ~ rv_z_score + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_white), conf.int = TRUE, exponentiate = TRUE
)

model_dlco_white <- tidy (glm (
  formula = restriction ~ dlco_z_score + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_white), conf.int = TRUE, exponentiate = TRUE
)

model_kco_white <- tidy (glm (
  formula = restriction ~ kco_z_score + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_white), conf.int = TRUE, exponentiate = TRUE
)

# Black

model_age_black <- tidy (glm (
  formula = restriction ~ age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_black), conf.int = TRUE, exponentiate = TRUE
)

model_sex_black <- tidy (glm (
  formula = restriction ~ sex + age + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_black), conf.int = TRUE, exponentiate = TRUE
)

model_ethnicity_black <- tidy (glm (
  formula = restriction ~ ethnicity + age + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_black), conf.int = TRUE, exponentiate = TRUE
)

model_bmi_black <- tidy (glm (
  formula = restriction ~ bmi + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_black), conf.int = TRUE, exponentiate = TRUE
)

model_pack_years_black <- tidy (glm (
  formula = restriction ~ pack_years + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_black), conf.int = TRUE, exponentiate = TRUE
)

model_cough_black <- tidy (glm (
  formula = restriction ~ cough + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_black), conf.int = TRUE, exponentiate = TRUE
)

model_dyspnea_black <- tidy (glm (
  formula = restriction ~ dyspnea + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_black), conf.int = TRUE, exponentiate = TRUE
)

model_wheeze_black <- tidy (glm (
  formula = restriction ~ wheeze + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_black), conf.int = TRUE, exponentiate = TRUE
)

model_asthma_black <- tidy (glm (
  formula = restriction ~ asthma + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_black), conf.int = TRUE, exponentiate = TRUE
)

model_bronchiectasis_black <- tidy (glm (
  formula = restriction ~ bronchiectasis + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_black), conf.int = TRUE, exponentiate = TRUE
)

model_chest_wall_black <- tidy (glm (
  formula = restriction ~ chest_wall + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_black), conf.int = TRUE, exponentiate = TRUE
)

model_copd_black <- tidy (glm (
  formula = restriction ~ copd + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_black), conf.int = TRUE, exponentiate = TRUE
)

model_ild_black <- tidy (glm (
  formula = restriction ~ ild + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_black), conf.int = TRUE, exponentiate = TRUE
)

model_neuromuscular_black <- tidy (glm (
  formula = restriction ~ neuromuscular + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_black), conf.int = TRUE, exponentiate = TRUE
)

model_copd_imaging_black <- tidy (glm (
  formula = restriction ~ copd_imaging + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_black), conf.int = TRUE, exponentiate = TRUE
)

model_ild_imaging_black <- tidy (glm (
  formula = restriction ~ ild_imaging + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_black), conf.int = TRUE, exponentiate = TRUE
)

model_fev1_black <- tidy (glm (
  formula = restriction ~ fev1_z_score + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_black), conf.int = TRUE, exponentiate = TRUE
)

model_fvc_black <- tidy (glm (
  formula = restriction ~ fvc_z_score + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_black), conf.int = TRUE, exponentiate = TRUE
)

model_fev1_fvc_black <- tidy (glm (
  formula = restriction ~ fev1_fvc_z_score + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_black), conf.int = TRUE, exponentiate = TRUE
)

model_effort_black <- tidy (glm (
  formula = restriction ~ effort + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_black), conf.int = TRUE, exponentiate = TRUE
)

model_response_black <- tidy (glm (
  formula = restriction ~ response + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_black), conf.int = TRUE, exponentiate = TRUE
)

model_rv_black <- tidy (glm (
  formula = restriction ~ rv_z_score + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_black), conf.int = TRUE, exponentiate = TRUE
)

model_dlco_black <- tidy (glm (
  formula = restriction ~ dlco_z_score + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_black), conf.int = TRUE, exponentiate = TRUE
)

model_kco_black <- tidy (glm (
  formula = restriction ~ kco_z_score + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_black), conf.int = TRUE, exponentiate = TRUE
)

# Asian

model_age_asian <- tidy (glm (
  formula = restriction ~ age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_asian), conf.int = TRUE, exponentiate = TRUE
)

model_sex_asian <- tidy (glm (
  formula = restriction ~ sex + age + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_asian), conf.int = TRUE, exponentiate = TRUE
)

model_ethnicity_asian <- tidy (glm (
  formula = restriction ~ ethnicity + age + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_asian), conf.int = TRUE, exponentiate = TRUE
)

model_bmi_asian <- tidy (glm (
  formula = restriction ~ bmi + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_asian), conf.int = TRUE, exponentiate = TRUE
)

model_pack_years_asian <- tidy (glm (
  formula = restriction ~ pack_years + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_asian), conf.int = TRUE, exponentiate = TRUE
)

model_cough_asian <- tidy (glm (
  formula = restriction ~ cough + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_asian), conf.int = TRUE, exponentiate = TRUE
)

model_dyspnea_asian <- tidy (glm (
  formula = restriction ~ dyspnea + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_asian), conf.int = TRUE, exponentiate = TRUE
)

model_wheeze_asian <- tidy (glm (
  formula = restriction ~ wheeze + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_asian), conf.int = TRUE, exponentiate = TRUE
)

model_asthma_asian <- tidy (glm (
  formula = restriction ~ asthma + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_asian), conf.int = TRUE, exponentiate = TRUE
)

model_bronchiectasis_asian <- tidy (glm (
  formula = restriction ~ bronchiectasis + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_asian), conf.int = TRUE, exponentiate = TRUE
)

# model_chest_wall_asian <- tidy (glm (
#   formula = restriction ~ chest_wall + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
#   family = "binomial",
#   data = data_asian), conf.int = TRUE, exponentiate = TRUE
# )

model_copd_asian <- tidy (glm (
  formula = restriction ~ copd + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_asian), conf.int = TRUE, exponentiate = TRUE
)

model_ild_asian <- tidy (glm (
  formula = restriction ~ ild + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_asian), conf.int = TRUE, exponentiate = TRUE
)

model_neuromuscular_asian <- tidy (glm (
  formula = restriction ~ neuromuscular + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_asian), conf.int = TRUE, exponentiate = TRUE
)

model_copd_imaging_asian <- tidy (glm (
  formula = restriction ~ copd_imaging + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_asian), conf.int = TRUE, exponentiate = TRUE
)

model_ild_imaging_asian <- tidy (glm (
  formula = restriction ~ ild_imaging + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_asian), conf.int = TRUE, exponentiate = TRUE
)

model_fev1_asian <- tidy (glm (
  formula = restriction ~ fev1_z_score + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_asian), conf.int = TRUE, exponentiate = TRUE
)

model_fvc_asian <- tidy (glm (
  formula = restriction ~ fvc_z_score + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_asian), conf.int = TRUE, exponentiate = TRUE
)

model_fev1_fvc_asian <- tidy (glm (
  formula = restriction ~ fev1_fvc_z_score + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_asian), conf.int = TRUE, exponentiate = TRUE
)

# model_effort_asian <- tidy (glm (
#   formula = restriction ~ effort + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
#   family = "binomial",
#   data = data_asian), conf.int = TRUE, exponentiate = TRUE
# )

model_response_asian <- tidy (glm (
  formula = restriction ~ response + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_asian), conf.int = TRUE, exponentiate = TRUE
)

model_rv_asian <- tidy (glm (
  formula = restriction ~ rv_z_score + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_asian), conf.int = TRUE, exponentiate = TRUE
)

model_dlco_asian <- tidy (glm (
  formula = restriction ~ dlco_z_score + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_asian), conf.int = TRUE, exponentiate = TRUE
)

model_kco_asian <- tidy (glm (
  formula = restriction ~ kco_z_score + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_asian), conf.int = TRUE, exponentiate = TRUE
)

# Other

model_age_other <- tidy (glm (
  formula = restriction ~ age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_other), conf.int = TRUE, exponentiate = TRUE
)

model_sex_other <- tidy (glm (
  formula = restriction ~ sex + age + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_other), conf.int = TRUE, exponentiate = TRUE
)

model_ethnicity_other <- tidy (glm (
  formula = restriction ~ ethnicity + age + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_other), conf.int = TRUE, exponentiate = TRUE
)

model_bmi_other <- tidy (glm (
  formula = restriction ~ bmi + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_other), conf.int = TRUE, exponentiate = TRUE
)

model_pack_years_other <- tidy (glm (
  formula = restriction ~ pack_years + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_other), conf.int = TRUE, exponentiate = TRUE
)

model_cough_other <- tidy (glm (
  formula = restriction ~ cough + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_other), conf.int = TRUE, exponentiate = TRUE
)

model_dyspnea_other <- tidy (glm (
  formula = restriction ~ dyspnea + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_other), conf.int = TRUE, exponentiate = TRUE
)

model_wheeze_other <- tidy (glm (
  formula = restriction ~ wheeze + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_other), conf.int = TRUE, exponentiate = TRUE
)

model_asthma_other <- tidy (glm (
  formula = restriction ~ asthma + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_other), conf.int = TRUE, exponentiate = TRUE
)

model_bronchiectasis_other <- tidy (glm (
  formula = restriction ~ bronchiectasis + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_other), conf.int = TRUE, exponentiate = TRUE
)

model_chest_wall_other <- tidy (glm (
  formula = restriction ~ chest_wall + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_other), conf.int = TRUE, exponentiate = TRUE
)

model_copd_other <- tidy (glm (
  formula = restriction ~ copd + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_other), conf.int = TRUE, exponentiate = TRUE
)

model_ild_other <- tidy (glm (
  formula = restriction ~ ild + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_other), conf.int = TRUE, exponentiate = TRUE
)

model_neuromuscular_other <- tidy (glm (
  formula = restriction ~ neuromuscular + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_other), conf.int = TRUE, exponentiate = TRUE
)

model_copd_imaging_other <- tidy (glm (
  formula = restriction ~ copd_imaging + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_other), conf.int = TRUE, exponentiate = TRUE
)

model_ild_imaging_other <- tidy (glm (
  formula = restriction ~ ild_imaging + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_other), conf.int = TRUE, exponentiate = TRUE
)

model_fev1_other <- tidy (glm (
  formula = restriction ~ fev1_z_score + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_other), conf.int = TRUE, exponentiate = TRUE
)

model_fvc_other <- tidy (glm (
  formula = restriction ~ fvc_z_score + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_other), conf.int = TRUE, exponentiate = TRUE
)

model_fev1_fvc_other <- tidy (glm (
  formula = restriction ~ fev1_fvc_z_score + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_other), conf.int = TRUE, exponentiate = TRUE
)

model_effort_other <- tidy (glm (
  formula = restriction ~ effort + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_other), conf.int = TRUE, exponentiate = TRUE
)

model_response_other <- tidy (glm (
  formula = restriction ~ response + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_other), conf.int = TRUE, exponentiate = TRUE
)

model_rv_other <- tidy (glm (
  formula = restriction ~ rv_z_score + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_other), conf.int = TRUE, exponentiate = TRUE
)

model_dlco_other <- tidy (glm (
  formula = restriction ~ dlco_z_score + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_other), conf.int = TRUE, exponentiate = TRUE
)

model_kco_other <- tidy (glm (
  formula = restriction ~ kco_z_score + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_other), conf.int = TRUE, exponentiate = TRUE
)

table <- paste (
  "#table(\n\t",
  "inset: (x: 8pt, y: 4pt),\n\t",  
  "columns: 5,\n\t",
	"align: (left, center, center, center, center),\n\t",
	"table.hline(),\n\t",
  "[], table.cell(colspan: 4, [*Adjusted Odds Ratio (95% CI)#super[a]*]),\n\t",
  "table.hline(start:1, end:6),\n\t",
	"[], [*White*], [*Black*], [*Asian*], [*Other*],\n\t",
	"table.hline(),\n\t",
  "[*Age, yr*], [",
  format (round (model_age_white [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_age_white [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_age_white [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_age_black [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_age_black [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_age_black [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_age_asian [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_age_asian [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_age_asian [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_age_other [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_age_other [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_age_other [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")],\n\t",
  "[*Sex*], [], [], [], [],\n\t",
  "[#h(1em)Men], [1.00 (Reference)], [1.00 (Reference)], [1.00 (Reference)], [1.00 (Reference)],\n\t",
  "[#h(1em)Women], [",  
  format (round (model_sex_white [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_sex_white [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_sex_white [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_sex_black [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_sex_black [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_sex_black [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_sex_asian [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_sex_asian [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_sex_asian [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_sex_other [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_sex_other [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_sex_other [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")],\n\t",
  "[*Ethnicity*], [], [], [], [],\n\t",
  "[#h(1em)Hispanic], [1.00 (Reference)], [1.00 (Reference)], [1.00 (Reference)], [1.00 (Reference)],\n\t",
  "[#h(1em)Not Hispanic], [",  
  format (round (model_ethnicity_white [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_ethnicity_white [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_ethnicity_white [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_ethnicity_black [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_ethnicity_black [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_ethnicity_black [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_ethnicity_asian [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_ethnicity_asian [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_ethnicity_asian [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_ethnicity_other [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_ethnicity_other [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_ethnicity_other [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")],\n\t",  
  "[*Pack Years, yr*], [",
  format (round (model_pack_years_white [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_pack_years_white [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_pack_years_white [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_pack_years_black [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_pack_years_black [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_pack_years_black [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_pack_years_asian [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_pack_years_asian [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_pack_years_asian [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_pack_years_other [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_pack_years_other [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_pack_years_other [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")],\n\t",
  "[*Respiratory Symptoms*], [], [], [], [],\n\t",
  "[#h(1em)Cough], [",
  format (round (model_cough_white [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_cough_white [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_cough_white [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_cough_black [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_cough_black [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_cough_black [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_cough_asian [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_cough_asian [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_cough_asian [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_cough_other [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_cough_other [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_cough_other [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")],\n\t",
  "[#h(1em)Dyspnea], [",
  format (round (model_dyspnea_white [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_dyspnea_white [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_dyspnea_white [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_dyspnea_black [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_dyspnea_black [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_dyspnea_black [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_dyspnea_asian [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_dyspnea_asian [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_dyspnea_asian [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_dyspnea_other [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_dyspnea_other [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_dyspnea_other [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")],\n\t",
  "[#h(1em)Wheeze], [",
  format (round (model_wheeze_white [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_wheeze_white [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_wheeze_white [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_wheeze_black [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_wheeze_black [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_wheeze_black [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_wheeze_asian [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_wheeze_asian [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_wheeze_asian [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_wheeze_other [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_wheeze_other [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_wheeze_other [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")],\n\t",     
  "[*Respiratory Diseases*], [], [], [], [],\n\t",
  "[#h(1em)Asthma], [",
  format (round (model_asthma_white [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_asthma_white [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_asthma_white [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_asthma_black [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_asthma_black [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_asthma_black [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_asthma_asian [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_asthma_asian [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_asthma_asian [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_asthma_other [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_asthma_other [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_asthma_other [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")],\n\t",
  "[#h(1em)Bronchiectasis], [",
  format (round (model_bronchiectasis_white [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_bronchiectasis_white [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_bronchiectasis_white [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_bronchiectasis_black [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_bronchiectasis_black [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_bronchiectasis_black [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_bronchiectasis_asian [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_bronchiectasis_asian [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_bronchiectasis_asian [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_bronchiectasis_other [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_bronchiectasis_other [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_bronchiectasis_other [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")],\n\t",
  "[#h(1em)Chest Wall Disorder], [",
  format (round (model_chest_wall_white [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_chest_wall_white [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_chest_wall_white [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_chest_wall_black [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_chest_wall_black [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_chest_wall_black [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [---], [",
  # format (round (model_chest_wall_asian [[2, "estimate"]], digits = 2), nsmall = 2),
  # " (",
  # format (round (model_chest_wall_asian [[2, "conf.low"]], digits = 2), nsmall = 2),
  # "--",
  # format (round (model_chest_wall_asian [[2, "conf.high"]], digits = 2), nsmall = 2),
  # ")], [",
  format (round (model_chest_wall_other [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_chest_wall_other [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_chest_wall_other [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")],\n\t",
  "[#h(1em)Chronic Obstructive Pulmonary Disease], [",
  format (round (model_copd_white [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_copd_white [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_copd_white [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_copd_black [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_copd_black [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_copd_black [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_copd_asian [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_copd_asian [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_copd_asian [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_copd_other [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_copd_other [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_copd_other [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")],\n\t",  
  "[#h(1em)Interstitial Lung Disease], [",
  format (round (model_ild_white [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_ild_white [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_ild_white [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_ild_black [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_ild_black [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_ild_black [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_ild_asian [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_ild_asian [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_ild_asian [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_ild_other [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_ild_other [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_ild_other [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")],\n\t",
  "[#h(1em)Neuromuscular Disease], [",
  format (round (model_neuromuscular_white [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_neuromuscular_white [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_neuromuscular_white [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_neuromuscular_black [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_neuromuscular_black [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_neuromuscular_black [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_neuromuscular_asian [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_neuromuscular_asian [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_neuromuscular_asian [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_neuromuscular_other [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_neuromuscular_other [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_neuromuscular_other [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")],\n\t", 
  "[*Computed Tomography*], [], [], [], [],\n\t",
  "[#h(1em)Chronic Obstructive Pulmonary Disease], [",
  format (round (model_copd_imaging_white [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_copd_imaging_white [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_copd_imaging_white [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_copd_imaging_black [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_copd_imaging_black [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_copd_imaging_black [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_copd_imaging_asian [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_copd_imaging_asian [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_copd_imaging_asian [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_copd_imaging_other [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_copd_imaging_other [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_copd_imaging_other [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")],\n\t", 
  "[#h(1em)Interstitial Lung Disease], [",
  format (round (model_ild_imaging_white [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_ild_imaging_white [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_ild_imaging_white [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_ild_imaging_black [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_ild_imaging_black [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_ild_imaging_black [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_ild_imaging_asian [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_ild_imaging_asian [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_ild_imaging_asian [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_ild_imaging_other [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_ild_imaging_other [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_ild_imaging_other [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")],\n\t",    
  "[*Dynamic Lung Volumes*], [], [], [], [],\n\t",
  "[#h(1em)FEV#sub[1], z-score], [",
  format (round (model_fev1_white [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_fev1_white [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_fev1_white [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_fev1_black [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_fev1_black [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_fev1_black [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_fev1_asian [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_fev1_asian [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_fev1_asian [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_fev1_other [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_fev1_other [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_fev1_other [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")],\n\t",
  "[#h(1em)FVC, z-score], [",
  format (round (model_fvc_white [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_fvc_white [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_fvc_white [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_fvc_black [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_fvc_black [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_fvc_black [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_fvc_asian [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_fvc_asian [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_fvc_asian [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_fvc_other [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_fvc_other [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_fvc_other [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")],\n\t",  
  "[#h(1em)FEV#sub[1]/FVC, z-score], [",
  format (round (model_fev1_fvc_white [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_fev1_fvc_white [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_fev1_fvc_white [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_fev1_fvc_black [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_fev1_fvc_black [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_fev1_fvc_black [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_fev1_fvc_asian [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_fev1_fvc_asian [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_fev1_fvc_asian [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_fev1_fvc_other [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_fev1_fvc_other [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_fev1_fvc_other [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")],\n\t",
  "[#h(1em)Bronchodilator Response], [",
  format (round (model_response_white [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_response_white [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_response_white [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_response_black [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_response_black [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_response_black [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_response_asian [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_response_asian [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_response_asian [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_response_other [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_response_other [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_response_other [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")],\n\t",
  "[#h(1em)Adequate Effort], [",
  format (round (model_effort_white [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_effort_white [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_effort_white [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_effort_black [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_effort_black [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_effort_black [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  "---",
  "], [",
  format (round (model_effort_other [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_effort_other [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_effort_other [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")],\n\t",      
  "[*Static Lung Volumes*], [], [], [], [],\n\t",
  "[#h(1em)RV, z-score], [",
  format (round (model_rv_white [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_rv_white [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_rv_white [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_rv_black [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_rv_black [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_rv_black [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_rv_asian [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_rv_asian [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_rv_asian [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_rv_other [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_rv_other [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_rv_other [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")],\n\t",   
  "[*Diffusing Capacity*], [], [], [], [],\n\t",
  "[#h(1em)D#sub[LCO], z-score], [",
  format (round (model_dlco_white [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_dlco_white [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_dlco_white [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_dlco_black [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_dlco_black [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_dlco_black [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_dlco_asian [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_dlco_asian [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_dlco_asian [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_dlco_other [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_dlco_other [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_dlco_other [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")],\n\t",
  "[#h(1em)K#sub[CO], z-score], [",
  format (round (model_kco_white [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_kco_white [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_kco_white [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_kco_black [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_kco_black [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_kco_black [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_kco_asian [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_kco_asian [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_kco_asian [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_kco_other [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_kco_other [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_kco_other [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")],\n\t",  
  "table.hline()\n",
  ")", 
  sep = ""
)  

write_lines (table, "../tables/e-table-20.txt")

################################################################################
## e-Table 21 - Characteristics by Ethnicity
################################################################################

data_hispanic <- pfts %>%
  filter (ethnicity == 1) %>% 
  filter (fvc_z_score >= -1.645 & fev1_fvc_z_score >= -1.645)

data_not_hispanic <- pfts %>%
  filter (ethnicity == 2) %>% 
  filter (fvc_z_score >= -1.645 & fev1_fvc_z_score >= -1.645)

# Hispanic

model_age_hispanic <- tidy (glm (
  formula = restriction ~ age + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_hispanic), conf.int = TRUE, exponentiate = TRUE
)

model_sex_hispanic <- tidy (glm (
  formula = restriction ~ sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_hispanic), conf.int = TRUE, exponentiate = TRUE
)

model_bmi_hispanic <- tidy (glm (
  formula = restriction ~ bmi + age + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_hispanic), conf.int = TRUE, exponentiate = TRUE
)

model_race_hispanic <- tidy (glm (
  formula = restriction ~ race + age + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_hispanic), conf.int = TRUE, exponentiate = TRUE
)

model_pack_years_hispanic <- tidy (glm (
  formula = restriction ~ pack_years + age + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_hispanic), conf.int = TRUE, exponentiate = TRUE
)

model_cough_hispanic <- tidy (glm (
  formula = restriction ~ cough + age + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_hispanic), conf.int = TRUE, exponentiate = TRUE
)

model_dyspnea_hispanic <- tidy (glm (
  formula = restriction ~ dyspnea + age + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_hispanic), conf.int = TRUE, exponentiate = TRUE
)

model_wheeze_hispanic <- tidy (glm (
  formula = restriction ~ wheeze + age + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_hispanic), conf.int = TRUE, exponentiate = TRUE
)

model_asthma_hispanic <- tidy (glm (
  formula = restriction ~ asthma + age + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_hispanic), conf.int = TRUE, exponentiate = TRUE
)

model_bronchiectasis_hispanic <- tidy (glm (
  formula = restriction ~ bronchiectasis + age + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_hispanic), conf.int = TRUE, exponentiate = TRUE
)

model_chest_wall_hispanic <- tidy (glm (
  formula = restriction ~ chest_wall + age + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_hispanic), conf.int = TRUE, exponentiate = TRUE
)

model_copd_hispanic <- tidy (glm (
  formula = restriction ~ copd + age + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_hispanic), conf.int = TRUE, exponentiate = TRUE
)

model_ild_hispanic <- tidy (glm (
  formula = restriction ~ ild + age + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_hispanic), conf.int = TRUE, exponentiate = TRUE
)

model_neuromuscular_hispanic <- tidy (glm (
  formula = restriction ~ neuromuscular + age + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_hispanic), conf.int = TRUE, exponentiate = TRUE
)

model_none_hispanic <- tidy (glm (
  formula = restriction ~ none + age + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_hispanic), conf.int = TRUE, exponentiate = TRUE
)

model_copd_imaging_hispanic <- tidy (glm (
  formula = restriction ~ copd_imaging + age + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_hispanic), conf.int = TRUE, exponentiate = TRUE
)

model_ild_imaging_hispanic <- tidy (glm (
  formula = restriction ~ ild_imaging + age + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_hispanic), conf.int = TRUE, exponentiate = TRUE
)

model_fev1_hispanic <- tidy (glm (
  formula = restriction ~ fev1_z_score + age + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_hispanic), conf.int = TRUE, exponentiate = TRUE
)

model_fvc_hispanic <- tidy (glm (
  formula = restriction ~ fvc_z_score + age + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_hispanic), conf.int = TRUE, exponentiate = TRUE
)

model_fev1_fvc_hispanic <- tidy (glm (
  formula = restriction ~ fev1_fvc_z_score + age + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_hispanic), conf.int = TRUE, exponentiate = TRUE
)

model_effort_hispanic <- tidy (glm (
  formula = restriction ~ effort + age + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_hispanic), conf.int = TRUE, exponentiate = TRUE
)

model_response_hispanic <- tidy (glm (
  formula = restriction ~ response + age + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_hispanic), conf.int = TRUE, exponentiate = TRUE
)

model_rv_hispanic <- tidy (glm (
  formula = restriction ~ rv_z_score + age + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_hispanic), conf.int = TRUE, exponentiate = TRUE
)

model_dlco_hispanic <- tidy (glm (
  formula = restriction ~ dlco_z_score + age + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_hispanic), conf.int = TRUE, exponentiate = TRUE
)

model_kco_hispanic <- tidy (glm (
  formula = restriction ~ kco_z_score + age + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_hispanic), conf.int = TRUE, exponentiate = TRUE
)

# Not Hispanic

model_age_not_hispanic <- tidy (glm (
  formula = restriction ~ age + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_not_hispanic), conf.int = TRUE, exponentiate = TRUE
)

model_sex_not_hispanic <- tidy (glm (
  formula = restriction ~ sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_not_hispanic), conf.int = TRUE, exponentiate = TRUE
)

model_bmi_not_hispanic <- tidy (glm (
  formula = restriction ~ bmi + age + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_not_hispanic), conf.int = TRUE, exponentiate = TRUE
)

model_race_not_hispanic <- tidy (glm (
  formula = restriction ~ race + age + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_not_hispanic), conf.int = TRUE, exponentiate = TRUE
)

model_pack_years_not_hispanic <- tidy (glm (
  formula = restriction ~ pack_years + age + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_not_hispanic), conf.int = TRUE, exponentiate = TRUE
)

model_cough_not_hispanic <- tidy (glm (
  formula = restriction ~ cough + age + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_not_hispanic), conf.int = TRUE, exponentiate = TRUE
)

model_dyspnea_not_hispanic <- tidy (glm (
  formula = restriction ~ dyspnea + age + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_not_hispanic), conf.int = TRUE, exponentiate = TRUE
)

model_wheeze_not_hispanic <- tidy (glm (
  formula = restriction ~ wheeze + age + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_not_hispanic), conf.int = TRUE, exponentiate = TRUE
)

model_asthma_not_hispanic <- tidy (glm (
  formula = restriction ~ asthma + age + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_not_hispanic), conf.int = TRUE, exponentiate = TRUE
)

model_bronchiectasis_not_hispanic <- tidy (glm (
  formula = restriction ~ bronchiectasis + age + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_not_hispanic), conf.int = TRUE, exponentiate = TRUE
)

model_chest_wall_not_hispanic <- tidy (glm (
  formula = restriction ~ chest_wall + age + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_not_hispanic), conf.int = TRUE, exponentiate = TRUE
)

model_copd_not_hispanic <- tidy (glm (
  formula = restriction ~ copd + age + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_not_hispanic), conf.int = TRUE, exponentiate = TRUE
)

model_ild_not_hispanic <- tidy (glm (
  formula = restriction ~ ild + age + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_not_hispanic), conf.int = TRUE, exponentiate = TRUE
)

model_neuromuscular_not_hispanic <- tidy (glm (
  formula = restriction ~ neuromuscular + age + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_not_hispanic), conf.int = TRUE, exponentiate = TRUE
)

model_none_not_hispanic <- tidy (glm (
  formula = restriction ~ none + age + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_not_hispanic), conf.int = TRUE, exponentiate = TRUE
)

model_copd_imaging_not_hispanic <- tidy (glm (
  formula = restriction ~ copd_imaging + age + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_not_hispanic), conf.int = TRUE, exponentiate = TRUE
)

model_ild_imaging_not_hispanic <- tidy (glm (
  formula = restriction ~ ild_imaging + age + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_not_hispanic), conf.int = TRUE, exponentiate = TRUE
)

model_fev1_not_hispanic <- tidy (glm (
  formula = restriction ~ fev1_z_score + age + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_not_hispanic), conf.int = TRUE, exponentiate = TRUE
)

model_fvc_not_hispanic <- tidy (glm (
  formula = restriction ~ fvc_z_score + age + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_not_hispanic), conf.int = TRUE, exponentiate = TRUE
)

model_fev1_fvc_not_hispanic <- tidy (glm (
  formula = restriction ~ fev1_fvc_z_score + age + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_not_hispanic), conf.int = TRUE, exponentiate = TRUE
)

model_effort_not_hispanic <- tidy (glm (
  formula = restriction ~ effort + age + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_not_hispanic), conf.int = TRUE, exponentiate = TRUE
)

model_response_not_hispanic <- tidy (glm (
  formula = restriction ~ response + age + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_not_hispanic), conf.int = TRUE, exponentiate = TRUE
)

model_rv_not_hispanic <- tidy (glm (
  formula = restriction ~ rv_z_score + age + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_not_hispanic), conf.int = TRUE, exponentiate = TRUE
)

model_dlco_not_hispanic <- tidy (glm (
  formula = restriction ~ dlco_z_score + age + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_not_hispanic), conf.int = TRUE, exponentiate = TRUE
)

model_kco_not_hispanic <- tidy (glm (
  formula = restriction ~ kco_z_score + age + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_not_hispanic), conf.int = TRUE, exponentiate = TRUE
)

table <- paste (
  
  "#table(\n\t",
  "inset: (x: 4pt, y: 3.5pt),\n\t",
	"columns: 3,\n\t",
	"align: (left, center, center),\n\t",
	"table.hline(),\n\t",
	"[], table.cell(colspan: 2, [*Adjusted Odds Ratio (95% CI)#super[a]*]),\n\t",
	"table.hline(start:1, end:3),\n\t",
	"[], [*Hispanic*], [*Not-Hispanic*],\n\t",
  "table.hline(),\n\t",
  "[*Age, yrs*], [",
  format (round (model_pack_years_hispanic [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_pack_years_hispanic [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_pack_years_hispanic [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_pack_years_not_hispanic [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_pack_years_not_hispanic [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_pack_years_not_hispanic [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")],\n\t",
  "[*Sex*], [], [],\n\t",
  "[#h(1em)Male], [1.00 (Reference)], [1.00 (Reference)],\n\t",
  "[#h(1em)Female], [",
  format (round (model_sex_hispanic [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_sex_hispanic [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_sex_hispanic [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_sex_not_hispanic [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_sex_not_hispanic [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_sex_not_hispanic [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")],\n\t",  
  "[*Race*], [], [],\n\t",
  "[#h(1em)White], [1.00 (Reference)], [1.00 (Reference)],\n\t",
  "[#h(1em)Black], [",
  format (round (model_race_hispanic [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_race_hispanic [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_race_hispanic [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_race_not_hispanic [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_race_not_hispanic [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_race_not_hispanic [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")],\n\t",
  "[#h(1em)Asian], [",
  format (round (model_race_hispanic [[3, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_race_hispanic [[3, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_race_hispanic [[3, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_race_not_hispanic [[3, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_race_not_hispanic [[3, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_race_not_hispanic [[3, "conf.high"]], digits = 2), nsmall = 2),
  ")],\n\t",
  "[#h(1em)Other], [",
  format (round (model_race_hispanic [[4, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_race_hispanic [[4, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_race_hispanic [[4, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_race_not_hispanic [[4, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_race_not_hispanic [[4, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_race_not_hispanic [[4, "conf.high"]], digits = 2), nsmall = 2),
  ")],\n\t",
  "[*Ethniicty*], [], [],\n\t",
  "[#h(1em)Hispanic], [1.00 (Reference)], [1.00 (Reference)],\n\t",
  "[#h(1em)Not Hispanic], [",
  format (round (model_race_hispanic [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_race_hispanic [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_race_hispanic [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_race_not_hispanic [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_race_not_hispanic [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_race_not_hispanic [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")],\n\t",  
  "[*Pack Years, yrs*], [",
  format (round (model_pack_years_hispanic [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_pack_years_hispanic [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_pack_years_hispanic [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_pack_years_not_hispanic [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_pack_years_not_hispanic [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_pack_years_not_hispanic [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")],\n\t",  
  "[*Respiratory Symptoms*], [], [],\n\t",
  "[#h(1em)Cough], [",
  format (round (model_cough_hispanic [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_cough_hispanic [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_cough_hispanic [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_cough_not_hispanic [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_cough_not_hispanic [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_cough_not_hispanic [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")],\n\t",
  "[#h(1em)Dyspnea], [",
  format (round (model_dyspnea_hispanic [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_dyspnea_hispanic [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_dyspnea_hispanic [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_dyspnea_not_hispanic [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_dyspnea_not_hispanic [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_dyspnea_not_hispanic [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")],\n\t",
  "[#h(1em)Wheeze], [",
  format (round (model_wheeze_hispanic [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_wheeze_hispanic [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_wheeze_hispanic [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_wheeze_not_hispanic [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_wheeze_not_hispanic [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_wheeze_not_hispanic [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")],\n\t",    
  "[*Respiratory Diseases*], [], [],\n\t",
  "[#h(1em)Asthma], [",
  format (round (model_asthma_hispanic [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_asthma_hispanic [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_asthma_hispanic [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_asthma_not_hispanic [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_asthma_not_hispanic [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_asthma_not_hispanic [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")],\n\t",
  "[#h(1em)Bronchiectasis], [",
  format (round (model_bronchiectasis_hispanic [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_bronchiectasis_hispanic [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_bronchiectasis_hispanic [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_bronchiectasis_not_hispanic [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_bronchiectasis_not_hispanic [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_bronchiectasis_not_hispanic [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")],\n\t",
  "[#h(1em)Chest Wall Disorder], [",
  format (round (model_chest_wall_hispanic [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_chest_wall_hispanic [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_chest_wall_hispanic [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_chest_wall_not_hispanic [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_chest_wall_not_hispanic [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_chest_wall_not_hispanic [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")],\n\t",
  "[#h(1em)Chronic Obstructive Pulmonary Disease], [",
  format (round (model_copd_hispanic [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_copd_hispanic [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_copd_hispanic [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_copd_not_hispanic [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_copd_not_hispanic [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_copd_not_hispanic [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")],\n\t",
  "[#h(1em)Interstitial Lung Disease], [",
  format (round (model_ild_hispanic [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_ild_hispanic [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_ild_hispanic [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_ild_not_hispanic [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_ild_not_hispanic [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_ild_not_hispanic [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")],\n\t",
  "[#h(1em)Neuromuscular Disease], [",
  format (round (model_neuromuscular_hispanic [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_neuromuscular_hispanic [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_neuromuscular_hispanic [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_neuromuscular_not_hispanic [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_neuromuscular_not_hispanic [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_neuromuscular_not_hispanic [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")],\n\t",     
  "[#h(1em)None], [",
  format (round (model_none_hispanic [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_none_hispanic [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_none_hispanic [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_none_not_hispanic [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_none_not_hispanic [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_none_not_hispanic [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")],\n\t",
  "[*Computed Tomography Findings*], [], [],\n\t",
  "[#h(1em)Chronic Obstructive Pulmonary Disease], [",
  format (round (model_copd_imaging_hispanic [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_copd_imaging_hispanic [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_copd_imaging_hispanic [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_copd_imaging_not_hispanic [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_copd_imaging_not_hispanic [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_copd_imaging_not_hispanic [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")],\n\t",
  "[#h(1em)Interstitial Lung Disease], [",
  format (round (model_ild_imaging_hispanic [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_ild_imaging_hispanic [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_ild_imaging_hispanic [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_ild_imaging_not_hispanic [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_ild_imaging_not_hispanic [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_ild_imaging_not_hispanic [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")],\n\t",     
  "[*Dynamic Lung Volumes*], [], [],\n\t",
  "[#h(1em)FEV#sub[1], z-score], [",
  format (round (model_fev1_hispanic [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_fev1_hispanic [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_fev1_hispanic [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_fev1_not_hispanic [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_fev1_not_hispanic [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_fev1_not_hispanic [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")],\n\t",
  "[#h(1em)FVC, z-score], [",
  format (round (model_fvc_hispanic [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_fvc_hispanic [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_fvc_hispanic [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_fvc_not_hispanic [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_fvc_not_hispanic [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_fvc_not_hispanic [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")],\n\t",
  "[#h(1em)FEV#sub[1]/FVC, z-score], [",
  format (round (model_fev1_fvc_hispanic [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_fev1_fvc_hispanic [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_fev1_fvc_hispanic [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_fev1_fvc_not_hispanic [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_fev1_fvc_not_hispanic [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_fev1_fvc_not_hispanic [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")],\n\t",   
  "[#h(1em)Bronchodilator Response], [",
  format (round (model_response_hispanic [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_response_hispanic [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_response_hispanic [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_response_not_hispanic [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_response_not_hispanic [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_response_not_hispanic [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")],\n\t",
  "[#h(1em)Adequate Effort], [",
  format (round (model_effort_hispanic [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_effort_hispanic [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_effort_hispanic [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_effort_not_hispanic [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_effort_not_hispanic [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_effort_not_hispanic [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")],\n\t",     
  "[*Static Lung Volumes*], [], [],\n\t",
  "[#h(1em)RV, z-score], [",
  format (round (model_rv_hispanic [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_rv_hispanic [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_rv_hispanic [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_rv_not_hispanic [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_rv_not_hispanic [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_rv_not_hispanic [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")],\n\t",
  "[*Diffusing Capacity*], [], [],\n\t",
  "[#h(1em)D#sub[LCO], z-score], [",
  format (round (model_dlco_hispanic [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_dlco_hispanic [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_dlco_hispanic [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_dlco_not_hispanic [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_dlco_not_hispanic [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_dlco_not_hispanic [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")],\n\t",
  "[#h(1em)K#sub[CO], z-score], [",
  format (round (model_kco_hispanic [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_kco_hispanic [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_kco_hispanic [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_kco_not_hispanic [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_kco_not_hispanic [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_kco_not_hispanic [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")],\n\t",   
	"table.hline(),\n",
  ")", 
  sep = ""
  
)

write_lines (table, "../tables/e-table-21.txt")

################################################################################
## e-Table 22 - Characteristics by BMI
################################################################################

data_bmi_1 <- pfts %>%
  filter (bmi >= 18.5 & bmi < 25) %>% 
  filter (fvc_z_score >= -1.645 & fev1_fvc_z_score >= -1.645)

data_bmi_2 <- pfts %>%
  filter (bmi >= 25 & bmi < 30) %>% 
  filter (fvc_z_score >= -1.645 & fev1_fvc_z_score >= -1.645)

data_bmi_3 <- pfts %>%
  filter (bmi >= 30 & bmi < 35) %>% 
  filter (fvc_z_score >= -1.645 & fev1_fvc_z_score >= -1.645)

data_bmi_4 <- pfts %>%
  filter (bmi >= 35 & bmi < 40) %>% 
  filter (fvc_z_score >= -1.645 & fev1_fvc_z_score >= -1.645)

data_bmi_5 <- pfts %>%
  filter (bmi >= 40) %>% 
  filter (fvc_z_score >= -1.645 & fev1_fvc_z_score >= -1.645)

# BMI 1 - Normal weight

model_age_bmi_1 <- tidy (glm (
  formula = restriction ~ age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_bmi_1), conf.int = TRUE, exponentiate = TRUE
)

model_sex_bmi_1 <- tidy (glm (
  formula = restriction ~ sex + age + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_bmi_1), conf.int = TRUE, exponentiate = TRUE
)

model_race_bmi_1 <- tidy (glm (
  formula = restriction ~ race + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_bmi_1), conf.int = TRUE, exponentiate = TRUE
)

model_ethnicity_bmi_1 <- tidy (glm (
  formula = restriction ~ ethnicity + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_bmi_1), conf.int = TRUE, exponentiate = TRUE
)

model_pack_years_bmi_1 <- tidy (glm (
  formula = restriction ~ pack_years + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_bmi_1), conf.int = TRUE, exponentiate = TRUE
)

model_cough_bmi_1 <- tidy (glm (
  formula = restriction ~ cough + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_bmi_1), conf.int = TRUE, exponentiate = TRUE
)

model_dyspnea_bmi_1 <- tidy (glm (
  formula = restriction ~ dyspnea + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_bmi_1), conf.int = TRUE, exponentiate = TRUE
)

model_wheeze_bmi_1 <- tidy (glm (
  formula = restriction ~ wheeze + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_bmi_1), conf.int = TRUE, exponentiate = TRUE
)

model_asthma_bmi_1 <- tidy (glm (
  formula = restriction ~ asthma + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_bmi_1), conf.int = TRUE, exponentiate = TRUE
)

model_bronchiectasis_bmi_1 <- tidy (glm (
  formula = restriction ~ bronchiectasis + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_bmi_1), conf.int = TRUE, exponentiate = TRUE
)

model_chest_wall_bmi_1 <- tidy (glm (
  formula = restriction ~ chest_wall + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_bmi_1), conf.int = TRUE, exponentiate = TRUE
)

model_copd_bmi_1 <- tidy (glm (
  formula = restriction ~ copd + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_bmi_1), conf.int = TRUE, exponentiate = TRUE
)

model_ild_bmi_1 <- tidy (glm (
  formula = restriction ~ ild + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_bmi_1), conf.int = TRUE, exponentiate = TRUE
)

model_neuromuscular_bmi_1 <- tidy (glm (
  formula = restriction ~ neuromuscular + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_bmi_1), conf.int = TRUE, exponentiate = TRUE
)

model_none_bmi_1 <- tidy (glm (
  formula = restriction ~ none + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_bmi_1), conf.int = TRUE, exponentiate = TRUE
)

model_copd_imaging_bmi_1 <- tidy (glm (
  formula = restriction ~ copd_imaging + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_bmi_1), conf.int = TRUE, exponentiate = TRUE
)

model_ild_imaging_bmi_1 <- tidy (glm (
  formula = restriction ~ ild_imaging + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_bmi_1), conf.int = TRUE, exponentiate = TRUE
)

model_fev1_bmi_1 <- tidy (glm (
  formula = restriction ~ fev1_z_score + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_bmi_1), conf.int = TRUE, exponentiate = TRUE
)

model_fvc_bmi_1 <- tidy (glm (
  formula = restriction ~ fvc_z_score + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_bmi_1), conf.int = TRUE, exponentiate = TRUE
)

model_fev1_fvc_bmi_1 <- tidy (glm (
  formula = restriction ~ fev1_fvc_z_score + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_bmi_1), conf.int = TRUE, exponentiate = TRUE
)

model_effort_bmi_1 <- tidy (glm (
  formula = restriction ~ effort + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_bmi_1), conf.int = TRUE, exponentiate = TRUE
)

model_response_bmi_1 <- tidy (glm (
  formula = restriction ~ response + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_bmi_1), conf.int = TRUE, exponentiate = TRUE
)

model_rv_bmi_1 <- tidy (glm (
  formula = restriction ~ rv_z_score + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_bmi_1), conf.int = TRUE, exponentiate = TRUE
)

model_dlco_bmi_1 <- tidy (glm (
  formula = restriction ~ dlco_z_score + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_bmi_1), conf.int = TRUE, exponentiate = TRUE
)

model_kco_bmi_1 <- tidy (glm (
  formula = restriction ~ kco_z_score + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_bmi_1), conf.int = TRUE, exponentiate = TRUE
)

# BMI 2 - Overweight

model_age_bmi_2 <- tidy (glm (
  formula = restriction ~ age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_bmi_2), conf.int = TRUE, exponentiate = TRUE
)

model_sex_bmi_2 <- tidy (glm (
  formula = restriction ~ sex + age + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_bmi_2), conf.int = TRUE, exponentiate = TRUE
)

model_race_bmi_2 <- tidy (glm (
  formula = restriction ~ race + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_bmi_2), conf.int = TRUE, exponentiate = TRUE
)

model_ethnicity_bmi_2 <- tidy (glm (
  formula = restriction ~ ethnicity + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_bmi_2), conf.int = TRUE, exponentiate = TRUE
)

model_pack_years_bmi_2 <- tidy (glm (
  formula = restriction ~ pack_years + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_bmi_2), conf.int = TRUE, exponentiate = TRUE
)

model_cough_bmi_2 <- tidy (glm (
  formula = restriction ~ cough + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_bmi_2), conf.int = TRUE, exponentiate = TRUE
)

model_dyspnea_bmi_2 <- tidy (glm (
  formula = restriction ~ dyspnea + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_bmi_2), conf.int = TRUE, exponentiate = TRUE
)

model_wheeze_bmi_2 <- tidy (glm (
  formula = restriction ~ wheeze + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_bmi_2), conf.int = TRUE, exponentiate = TRUE
)

model_asthma_bmi_2 <- tidy (glm (
  formula = restriction ~ asthma + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_bmi_2), conf.int = TRUE, exponentiate = TRUE
)

model_bronchiectasis_bmi_2 <- tidy (glm (
  formula = restriction ~ bronchiectasis + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_bmi_2), conf.int = TRUE, exponentiate = TRUE
)

model_chest_wall_bmi_2 <- tidy (glm (
  formula = restriction ~ chest_wall + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_bmi_2), conf.int = TRUE, exponentiate = TRUE
)

model_copd_bmi_2 <- tidy (glm (
  formula = restriction ~ copd + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_bmi_2), conf.int = TRUE, exponentiate = TRUE
)

model_ild_bmi_2 <- tidy (glm (
  formula = restriction ~ ild + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_bmi_2), conf.int = TRUE, exponentiate = TRUE
)

model_neuromuscular_bmi_2 <- tidy (glm (
  formula = restriction ~ neuromuscular + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_bmi_2), conf.int = TRUE, exponentiate = TRUE
)

model_none_bmi_2 <- tidy (glm (
  formula = restriction ~ none + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_bmi_2), conf.int = TRUE, exponentiate = TRUE
)

model_copd_imaging_bmi_2 <- tidy (glm (
  formula = restriction ~ copd_imaging + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_bmi_2), conf.int = TRUE, exponentiate = TRUE
)

model_ild_imaging_bmi_2 <- tidy (glm (
  formula = restriction ~ ild_imaging + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_bmi_2), conf.int = TRUE, exponentiate = TRUE
)

model_fev1_bmi_2 <- tidy (glm (
  formula = restriction ~ fev1_z_score + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_bmi_2), conf.int = TRUE, exponentiate = TRUE
)

model_fvc_bmi_2 <- tidy (glm (
  formula = restriction ~ fvc_z_score + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_bmi_2), conf.int = TRUE, exponentiate = TRUE
)

model_fev1_fvc_bmi_2 <- tidy (glm (
  formula = restriction ~ fev1_fvc_z_score + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_bmi_2), conf.int = TRUE, exponentiate = TRUE
)

model_effort_bmi_2 <- tidy (glm (
  formula = restriction ~ effort + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_bmi_2), conf.int = TRUE, exponentiate = TRUE
)

model_response_bmi_2 <- tidy (glm (
  formula = restriction ~ response + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_bmi_2), conf.int = TRUE, exponentiate = TRUE
)

model_rv_bmi_2 <- tidy (glm (
  formula = restriction ~ rv_z_score + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_bmi_2), conf.int = TRUE, exponentiate = TRUE
)

model_dlco_bmi_2 <- tidy (glm (
  formula = restriction ~ dlco_z_score + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_bmi_2), conf.int = TRUE, exponentiate = TRUE
)

model_kco_bmi_2 <- tidy (glm (
  formula = restriction ~ kco_z_score + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_bmi_2), conf.int = TRUE, exponentiate = TRUE
)

# BMI 3 - Class I

model_age_bmi_3 <- tidy (glm (
  formula = restriction ~ age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_bmi_3), conf.int = TRUE, exponentiate = TRUE
)

model_sex_bmi_3 <- tidy (glm (
  formula = restriction ~ sex + age + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_bmi_3), conf.int = TRUE, exponentiate = TRUE
)

model_race_bmi_3 <- tidy (glm (
  formula = restriction ~ race + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_bmi_3), conf.int = TRUE, exponentiate = TRUE
)

model_ethnicity_bmi_3 <- tidy (glm (
  formula = restriction ~ ethnicity + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_bmi_3), conf.int = TRUE, exponentiate = TRUE
)

model_pack_years_bmi_3 <- tidy (glm (
  formula = restriction ~ pack_years + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_bmi_3), conf.int = TRUE, exponentiate = TRUE
)

model_cough_bmi_3 <- tidy (glm (
  formula = restriction ~ cough + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_bmi_3), conf.int = TRUE, exponentiate = TRUE
)

model_dyspnea_bmi_3 <- tidy (glm (
  formula = restriction ~ dyspnea + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_bmi_3), conf.int = TRUE, exponentiate = TRUE
)

model_wheeze_bmi_3 <- tidy (glm (
  formula = restriction ~ wheeze + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_bmi_3), conf.int = TRUE, exponentiate = TRUE
)

model_asthma_bmi_3 <- tidy (glm (
  formula = restriction ~ asthma + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_bmi_3), conf.int = TRUE, exponentiate = TRUE
)

model_bronchiectasis_bmi_3 <- tidy (glm (
  formula = restriction ~ bronchiectasis + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_bmi_3), conf.int = TRUE, exponentiate = TRUE
)

model_chest_wall_bmi_3 <- tidy (glm (
  formula = restriction ~ chest_wall + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_bmi_3), conf.int = TRUE, exponentiate = TRUE
)

model_copd_bmi_3 <- tidy (glm (
  formula = restriction ~ copd + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_bmi_3), conf.int = TRUE, exponentiate = TRUE
)

model_ild_bmi_3 <- tidy (glm (
  formula = restriction ~ ild + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_bmi_3), conf.int = TRUE, exponentiate = TRUE
)

model_neuromuscular_bmi_3 <- tidy (glm (
  formula = restriction ~ neuromuscular + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_bmi_3), conf.int = TRUE, exponentiate = TRUE
)

model_none_bmi_3 <- tidy (glm (
  formula = restriction ~ none + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_bmi_3), conf.int = TRUE, exponentiate = TRUE
)

model_copd_imaging_bmi_3 <- tidy (glm (
  formula = restriction ~ copd_imaging + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_bmi_3), conf.int = TRUE, exponentiate = TRUE
)

model_ild_imaging_bmi_3 <- tidy (glm (
  formula = restriction ~ ild_imaging + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_bmi_3), conf.int = TRUE, exponentiate = TRUE
)

model_fev1_bmi_3 <- tidy (glm (
  formula = restriction ~ fev1_z_score + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_bmi_3), conf.int = TRUE, exponentiate = TRUE
)

model_fvc_bmi_3 <- tidy (glm (
  formula = restriction ~ fvc_z_score + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_bmi_3), conf.int = TRUE, exponentiate = TRUE
)

model_fev1_fvc_bmi_3 <- tidy (glm (
  formula = restriction ~ fev1_fvc_z_score + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_bmi_3), conf.int = TRUE, exponentiate = TRUE
)

model_effort_bmi_3 <- tidy (glm (
  formula = restriction ~ effort + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_bmi_3), conf.int = TRUE, exponentiate = TRUE
)

model_response_bmi_3 <- tidy (glm (
  formula = restriction ~ response + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_bmi_3), conf.int = TRUE, exponentiate = TRUE
)

model_rv_bmi_3 <- tidy (glm (
  formula = restriction ~ rv_z_score + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_bmi_3), conf.int = TRUE, exponentiate = TRUE
)

model_dlco_bmi_3 <- tidy (glm (
  formula = restriction ~ dlco_z_score + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_bmi_3), conf.int = TRUE, exponentiate = TRUE
)

model_kco_bmi_3 <- tidy (glm (
  formula = restriction ~ kco_z_score + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_bmi_3), conf.int = TRUE, exponentiate = TRUE
)

# BMI 4 - Class II

model_age_bmi_4 <- tidy (glm (
  formula = restriction ~ age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_bmi_4), conf.int = TRUE, exponentiate = TRUE
)

model_sex_bmi_4 <- tidy (glm (
  formula = restriction ~ sex + age + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_bmi_4), conf.int = TRUE, exponentiate = TRUE
)

model_race_bmi_4 <- tidy (glm (
  formula = restriction ~ race + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_bmi_4), conf.int = TRUE, exponentiate = TRUE
)

model_ethnicity_bmi_4 <- tidy (glm (
  formula = restriction ~ ethnicity + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_bmi_4), conf.int = TRUE, exponentiate = TRUE
)

model_pack_years_bmi_4 <- tidy (glm (
  formula = restriction ~ pack_years + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_bmi_4), conf.int = TRUE, exponentiate = TRUE
)

model_cough_bmi_4 <- tidy (glm (
  formula = restriction ~ cough + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_bmi_4), conf.int = TRUE, exponentiate = TRUE
)

model_dyspnea_bmi_4 <- tidy (glm (
  formula = restriction ~ dyspnea + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_bmi_4), conf.int = TRUE, exponentiate = TRUE
)

model_wheeze_bmi_4 <- tidy (glm (
  formula = restriction ~ wheeze + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_bmi_4), conf.int = TRUE, exponentiate = TRUE
)

model_asthma_bmi_4 <- tidy (glm (
  formula = restriction ~ asthma + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_bmi_4), conf.int = TRUE, exponentiate = TRUE
)

model_bronchiectasis_bmi_4 <- tidy (glm (
  formula = restriction ~ bronchiectasis + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_bmi_4), conf.int = TRUE, exponentiate = TRUE
)

model_chest_wall_bmi_4 <- tidy (glm (
  formula = restriction ~ chest_wall + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_bmi_4), conf.int = TRUE, exponentiate = TRUE
)

model_copd_bmi_4 <- tidy (glm (
  formula = restriction ~ copd + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_bmi_4), conf.int = TRUE, exponentiate = TRUE
)

model_ild_bmi_4 <- tidy (glm (
  formula = restriction ~ ild + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_bmi_4), conf.int = TRUE, exponentiate = TRUE
)

model_neuromuscular_bmi_4 <- tidy (glm (
  formula = restriction ~ neuromuscular + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_bmi_4), conf.int = TRUE, exponentiate = TRUE
)

model_none_bmi_4 <- tidy (glm (
  formula = restriction ~ none + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_bmi_4), conf.int = TRUE, exponentiate = TRUE
)

model_copd_imaging_bmi_4 <- tidy (glm (
  formula = restriction ~ copd_imaging + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_bmi_4), conf.int = TRUE, exponentiate = TRUE
)

model_ild_imaging_bmi_4 <- tidy (glm (
  formula = restriction ~ ild_imaging + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_bmi_4), conf.int = TRUE, exponentiate = TRUE
)

model_fev1_bmi_4 <- tidy (glm (
  formula = restriction ~ fev1_z_score + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_bmi_4), conf.int = TRUE, exponentiate = TRUE
)

model_fvc_bmi_4 <- tidy (glm (
  formula = restriction ~ fvc_z_score + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_bmi_4), conf.int = TRUE, exponentiate = TRUE
)

model_fev1_fvc_bmi_4 <- tidy (glm (
  formula = restriction ~ fev1_fvc_z_score + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_bmi_4), conf.int = TRUE, exponentiate = TRUE
)

model_effort_bmi_4 <- tidy (glm (
  formula = restriction ~ effort + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_bmi_4), conf.int = TRUE, exponentiate = TRUE
)

model_response_bmi_4 <- tidy (glm (
  formula = restriction ~ response + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_bmi_4), conf.int = TRUE, exponentiate = TRUE
)

model_rv_bmi_4 <- tidy (glm (
  formula = restriction ~ rv_z_score + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_bmi_4), conf.int = TRUE, exponentiate = TRUE
)

model_dlco_bmi_4 <- tidy (glm (
  formula = restriction ~ dlco_z_score + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_bmi_4), conf.int = TRUE, exponentiate = TRUE
)

model_kco_bmi_4 <- tidy (glm (
  formula = restriction ~ kco_z_score + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_bmi_4), conf.int = TRUE, exponentiate = TRUE
)

# BMI 5 - Class III

model_age_bmi_5 <- tidy (glm (
  formula = restriction ~ age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_bmi_5), conf.int = TRUE, exponentiate = TRUE
)

model_sex_bmi_5 <- tidy (glm (
  formula = restriction ~ sex + age + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_bmi_5), conf.int = TRUE, exponentiate = TRUE
)

model_race_bmi_5 <- tidy (glm (
  formula = restriction ~ race + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_bmi_5), conf.int = TRUE, exponentiate = TRUE
)

model_ethnicity_bmi_5 <- tidy (glm (
  formula = restriction ~ ethnicity + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_bmi_5), conf.int = TRUE, exponentiate = TRUE
)

model_pack_years_bmi_5 <- tidy (glm (
  formula = restriction ~ pack_years + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_bmi_5), conf.int = TRUE, exponentiate = TRUE
)

model_cough_bmi_5 <- tidy (glm (
  formula = restriction ~ cough + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_bmi_5), conf.int = TRUE, exponentiate = TRUE
)

model_dyspnea_bmi_5 <- tidy (glm (
  formula = restriction ~ dyspnea + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_bmi_5), conf.int = TRUE, exponentiate = TRUE
)

model_wheeze_bmi_5 <- tidy (glm (
  formula = restriction ~ wheeze + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_bmi_5), conf.int = TRUE, exponentiate = TRUE
)

model_asthma_bmi_5 <- tidy (glm (
  formula = restriction ~ asthma + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_bmi_5), conf.int = TRUE, exponentiate = TRUE
)

model_bronchiectasis_bmi_5 <- tidy (glm (
  formula = restriction ~ bronchiectasis + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_bmi_5), conf.int = TRUE, exponentiate = TRUE
)

model_chest_wall_bmi_5 <- tidy (glm (
  formula = restriction ~ chest_wall + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_bmi_5), conf.int = TRUE, exponentiate = TRUE
)

model_copd_bmi_5 <- tidy (glm (
  formula = restriction ~ copd + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_bmi_5), conf.int = TRUE, exponentiate = TRUE
)

model_ild_bmi_5 <- tidy (glm (
  formula = restriction ~ ild + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_bmi_5), conf.int = TRUE, exponentiate = TRUE
)

model_neuromuscular_bmi_5 <- tidy (glm (
  formula = restriction ~ neuromuscular + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_bmi_5), conf.int = TRUE, exponentiate = TRUE
)

model_none_bmi_5 <- tidy (glm (
  formula = restriction ~ none + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_bmi_5), conf.int = TRUE, exponentiate = TRUE
)

model_copd_imaging_bmi_5 <- tidy (glm (
  formula = restriction ~ copd_imaging + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_bmi_5), conf.int = TRUE, exponentiate = TRUE
)

model_ild_imaging_bmi_5 <- tidy (glm (
  formula = restriction ~ ild_imaging + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_bmi_5), conf.int = TRUE, exponentiate = TRUE
)

model_fev1_bmi_5 <- tidy (glm (
  formula = restriction ~ fev1_z_score + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_bmi_5), conf.int = TRUE, exponentiate = TRUE
)

model_fvc_bmi_5 <- tidy (glm (
  formula = restriction ~ fvc_z_score + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_bmi_5), conf.int = TRUE, exponentiate = TRUE
)

model_fev1_fvc_bmi_5 <- tidy (glm (
  formula = restriction ~ fev1_fvc_z_score + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_bmi_5), conf.int = TRUE, exponentiate = TRUE
)

model_effort_bmi_5 <- tidy (glm (
  formula = restriction ~ effort + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_bmi_5), conf.int = TRUE, exponentiate = TRUE
)

model_response_bmi_5 <- tidy (glm (
  formula = restriction ~ response + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_bmi_5), conf.int = TRUE, exponentiate = TRUE
)

model_rv_bmi_5 <- tidy (glm (
  formula = restriction ~ rv_z_score + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_bmi_5), conf.int = TRUE, exponentiate = TRUE
)

model_dlco_bmi_5 <- tidy (glm (
  formula = restriction ~ dlco_z_score + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_bmi_5), conf.int = TRUE, exponentiate = TRUE
)

model_kco_bmi_5 <- tidy (glm (
  formula = restriction ~ kco_z_score + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_bmi_5), conf.int = TRUE, exponentiate = TRUE
)

table <- paste (
  "#table(\n\t",
  "inset: (x: 4pt, y: 4pt),\n\t",  
  "columns: 6,\n\t",
	"align: (left, center, center, center, center, center),\n\t",
	"table.hline(),\n\t",
  "[], table.cell(colspan: 5, [*Adjusted Odds Ratio (95% CI)#super[a]*]),\n\t",
  "table.hline(start:1, end:7),\n\t",
	"[], [*Normal Weight*], [*Overweight*], [*Class I Obesity*], [*Class II Obesity*], [*Class III Obesity*],\n\t",
	"table.hline(),\n\t",
  "[*Age, yr*], [",
  format (round (model_age_bmi_1 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_age_bmi_1 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_age_bmi_1 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_age_bmi_2 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_age_bmi_2 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_age_bmi_2 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_age_bmi_3 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_age_bmi_3 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_age_bmi_3 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_age_bmi_4 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_age_bmi_4 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_age_bmi_4 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_age_bmi_5 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_age_bmi_5 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_age_bmi_5 [[2, "conf.high"]], digits = 2), nsmall = 2),  
  ")],\n\t",
  "[*Sex*], [], [], [], [], [],\n\t",
  "[#h(1em)Men], [1.00 (Reference)], [1.00 (Reference)], [1.00 (Reference)], [1.00 (Reference)], [1.00 (Reference)],\n\t",
  "[#h(1em)Women], [",
  format (round (model_sex_bmi_1 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_sex_bmi_1 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_sex_bmi_1 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_sex_bmi_2 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_sex_bmi_2 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_sex_bmi_2 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_sex_bmi_3 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_sex_bmi_3 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_sex_bmi_3 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_sex_bmi_4 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_sex_bmi_4 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_sex_bmi_4 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_sex_bmi_5 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_sex_bmi_5 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_sex_bmi_5 [[2, "conf.high"]], digits = 2), nsmall = 2),  
  ")],\n\t",
  "[*Race*], [], [], [], [], [],\n\t",
  "[#h(1em)White], [1.00 (Reference)], [1.00 (Reference)], [1.00 (Reference)], [1.00 (Reference)], [1.00 (Reference)],\n\t",
  "[#h(1em)Black], [",
  format (round (model_race_bmi_1 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_race_bmi_1 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_race_bmi_1 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_race_bmi_2 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_race_bmi_2 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_race_bmi_2 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_race_bmi_3 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_race_bmi_3 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_race_bmi_3 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_race_bmi_4 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_race_bmi_4 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_race_bmi_4 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_race_bmi_5 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_race_bmi_5 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_race_bmi_5 [[2, "conf.high"]], digits = 2), nsmall = 2),  
  ")],\n\t",
  "[#h(1em)Asian], [",
  format (round (model_race_bmi_1 [[3, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_race_bmi_1 [[3, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_race_bmi_1 [[3, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_race_bmi_2 [[3, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_race_bmi_2 [[3, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_race_bmi_2 [[3, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_race_bmi_3 [[3, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_race_bmi_3 [[3, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_race_bmi_3 [[3, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_race_bmi_4 [[3, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_race_bmi_4 [[3, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_race_bmi_4 [[3, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_race_bmi_5 [[3, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_race_bmi_5 [[3, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_race_bmi_5 [[3, "conf.high"]], digits = 2), nsmall = 2),  
  ")],\n\t",
  "[#h(1em)Other], [",
  format (round (model_race_bmi_1 [[4, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_race_bmi_1 [[4, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_race_bmi_1 [[4, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_race_bmi_2 [[4, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_race_bmi_2 [[4, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_race_bmi_2 [[4, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_race_bmi_3 [[4, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_race_bmi_3 [[4, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_race_bmi_3 [[4, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_race_bmi_4 [[4, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_race_bmi_4 [[4, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_race_bmi_4 [[4, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_race_bmi_5 [[4, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_race_bmi_5 [[4, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_race_bmi_5 [[4, "conf.high"]], digits = 2), nsmall = 2),  
  ")],\n\t",     
  "[*Ethnicity*], [], [], [], [], [],\n\t",
  "[#h(1em)Hispanic], [1.00 (Reference)], [1.00 (Reference)], [1.00 (Reference)], [1.00 (Reference)], [1.00 (Reference)],\n\t",
  "[#h(1em)Not Hispanic], [",
  format (round (model_ethnicity_bmi_1 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_ethnicity_bmi_1 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_ethnicity_bmi_1 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_ethnicity_bmi_2 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_ethnicity_bmi_2 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_ethnicity_bmi_2 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_ethnicity_bmi_3 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_ethnicity_bmi_3 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_ethnicity_bmi_3 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_ethnicity_bmi_4 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_ethnicity_bmi_4 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_ethnicity_bmi_4 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_ethnicity_bmi_5 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_ethnicity_bmi_5 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_ethnicity_bmi_5 [[2, "conf.high"]], digits = 2), nsmall = 2),  
  ")],\n\t",
  "[*Pack Years, yr*], [",
  format (round (model_pack_years_bmi_1 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_pack_years_bmi_1 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_pack_years_bmi_1 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_pack_years_bmi_2 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_pack_years_bmi_2 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_pack_years_bmi_2 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_pack_years_bmi_3 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_pack_years_bmi_3 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_pack_years_bmi_3 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_pack_years_bmi_4 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_pack_years_bmi_4 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_pack_years_bmi_4 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_pack_years_bmi_5 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_pack_years_bmi_5 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_pack_years_bmi_5 [[2, "conf.high"]], digits = 2), nsmall = 2),  
  ")],\n\t",
  "[*Respiratory Symptoms*], [], [], [], [], [],\n\t",
  "[#h(1em)Cough], [",
  format (round (model_cough_bmi_1 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_cough_bmi_1 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_cough_bmi_1 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_cough_bmi_2 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_cough_bmi_2 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_cough_bmi_2 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_cough_bmi_3 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_cough_bmi_3 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_cough_bmi_3 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_cough_bmi_4 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_cough_bmi_4 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_cough_bmi_4 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_cough_bmi_5 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_cough_bmi_5 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_cough_bmi_5 [[2, "conf.high"]], digits = 2), nsmall = 2),  
  ")],\n\t",
  "[#h(1em)Dyspnea], [",
  format (round (model_dyspnea_bmi_1 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_dyspnea_bmi_1 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_dyspnea_bmi_1 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_dyspnea_bmi_2 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_dyspnea_bmi_2 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_dyspnea_bmi_2 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_dyspnea_bmi_3 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_dyspnea_bmi_3 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_dyspnea_bmi_3 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_dyspnea_bmi_4 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_dyspnea_bmi_4 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_dyspnea_bmi_4 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_dyspnea_bmi_5 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_dyspnea_bmi_5 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_dyspnea_bmi_5 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")],\n\t",
  "[#h(1em)Wheeze], [",
  format (round (model_wheeze_bmi_1 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_wheeze_bmi_1 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_wheeze_bmi_1 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_wheeze_bmi_2 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_wheeze_bmi_2 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_wheeze_bmi_2 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_wheeze_bmi_3 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_wheeze_bmi_3 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_wheeze_bmi_3 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_wheeze_bmi_4 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_wheeze_bmi_4 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_wheeze_bmi_4 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_wheeze_bmi_5 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_wheeze_bmi_5 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_wheeze_bmi_5 [[2, "conf.high"]], digits = 2), nsmall = 2),  
  ")],\n\t",
  "[*Respiratory Diseases*], [], [], [], [], [],\n\t",
  "[#h(1em)Asthma], [",
  format (round (model_asthma_bmi_1 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_asthma_bmi_1 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_asthma_bmi_1 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_asthma_bmi_2 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_asthma_bmi_2 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_asthma_bmi_2 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_asthma_bmi_3 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_asthma_bmi_3 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_asthma_bmi_3 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_asthma_bmi_4 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_asthma_bmi_4 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_asthma_bmi_4 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_asthma_bmi_5 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_asthma_bmi_5 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_asthma_bmi_5 [[2, "conf.high"]], digits = 2), nsmall = 2),  
  ")],\n\t",
  "[#h(1em)Bronchiectasis], [",
  format (round (model_bronchiectasis_bmi_1 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_bronchiectasis_bmi_1 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_bronchiectasis_bmi_1 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_bronchiectasis_bmi_2 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_bronchiectasis_bmi_2 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_bronchiectasis_bmi_2 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_bronchiectasis_bmi_3 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_bronchiectasis_bmi_3 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_bronchiectasis_bmi_3 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_bronchiectasis_bmi_4 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_bronchiectasis_bmi_4 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_bronchiectasis_bmi_4 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_bronchiectasis_bmi_5 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_bronchiectasis_bmi_5 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_bronchiectasis_bmi_5 [[2, "conf.high"]], digits = 2), nsmall = 2),  
  ")],\n\t",
  "[#h(1em)Chest Wall Disorder], [",
  format (round (model_chest_wall_bmi_1 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_chest_wall_bmi_1 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_chest_wall_bmi_1 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_chest_wall_bmi_2 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_chest_wall_bmi_2 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_chest_wall_bmi_2 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_chest_wall_bmi_3 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_chest_wall_bmi_3 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_chest_wall_bmi_3 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_chest_wall_bmi_4 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_chest_wall_bmi_4 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_chest_wall_bmi_4 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_chest_wall_bmi_5 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_chest_wall_bmi_5 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_chest_wall_bmi_5 [[2, "conf.high"]], digits = 2), nsmall = 2),  
  ")],\n\t",
  "[#h(1em)Chronic Obstructive Pulmonary Disease], [",
  format (round (model_copd_bmi_1 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_copd_bmi_1 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_copd_bmi_1 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_copd_bmi_2 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_copd_bmi_2 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_copd_bmi_2 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_copd_bmi_3 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_copd_bmi_3 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_copd_bmi_3 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_copd_bmi_4 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_copd_bmi_4 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_copd_bmi_4 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_copd_bmi_5 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_copd_bmi_5 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_copd_bmi_5 [[2, "conf.high"]], digits = 2), nsmall = 2),  
  ")],\n\t",
  "[#h(1em)Interstitial Lung Disease], [",
  format (round (model_ild_bmi_1 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_ild_bmi_1 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_ild_bmi_1 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_ild_bmi_2 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_ild_bmi_2 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_ild_bmi_2 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_ild_bmi_3 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_ild_bmi_3 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_ild_bmi_3 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_ild_bmi_4 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_ild_bmi_4 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_ild_bmi_4 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_ild_bmi_5 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_ild_bmi_5 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_ild_bmi_5 [[2, "conf.high"]], digits = 2), nsmall = 2),  
  ")],\n\t",
  "[#h(1em)Neuromuscular Disease], [",
  format (round (model_neuromuscular_bmi_1 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_neuromuscular_bmi_1 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_neuromuscular_bmi_1 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_neuromuscular_bmi_2 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_neuromuscular_bmi_2 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_neuromuscular_bmi_2 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_neuromuscular_bmi_3 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_neuromuscular_bmi_3 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_neuromuscular_bmi_3 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_neuromuscular_bmi_4 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_neuromuscular_bmi_4 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_neuromuscular_bmi_4 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_neuromuscular_bmi_5 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_neuromuscular_bmi_5 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_neuromuscular_bmi_5 [[2, "conf.high"]], digits = 2), nsmall = 2),  
  ")],\n\t",
  "[#h(1em)None], [",
  format (round (model_none_bmi_1 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_none_bmi_1 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_none_bmi_1 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_none_bmi_2 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_none_bmi_2 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_none_bmi_2 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_none_bmi_3 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_none_bmi_3 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_none_bmi_3 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_none_bmi_4 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_none_bmi_4 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_none_bmi_4 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_none_bmi_5 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_none_bmi_5 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_none_bmi_5 [[2, "conf.high"]], digits = 2), nsmall = 2),  
  ")],\n\t",  
  "[*Computed Tomography*], [], [], [], [], [],\n\t",
  "[#h(1em)Chronic Obstructive Pulmonary Disease], [",
  format (round (model_copd_imaging_bmi_1 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_copd_imaging_bmi_1 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_copd_imaging_bmi_1 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_copd_imaging_bmi_2 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_copd_imaging_bmi_2 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_copd_imaging_bmi_2 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_copd_imaging_bmi_3 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_copd_imaging_bmi_3 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_copd_imaging_bmi_3 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_copd_imaging_bmi_4 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_copd_imaging_bmi_4 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_copd_imaging_bmi_4 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_copd_imaging_bmi_5 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_copd_imaging_bmi_5 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_copd_imaging_bmi_5 [[2, "conf.high"]], digits = 2), nsmall = 2),  
  ")],\n\t",
  "[#h(1em)Interstitial Lung Disease], [",
  format (round (model_ild_imaging_bmi_1 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_ild_imaging_bmi_1 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_ild_imaging_bmi_1 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_ild_imaging_bmi_2 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_ild_imaging_bmi_2 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_ild_imaging_bmi_2 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_ild_imaging_bmi_3 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_ild_imaging_bmi_3 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_ild_imaging_bmi_3 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_ild_imaging_bmi_4 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_ild_imaging_bmi_4 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_ild_imaging_bmi_4 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_ild_imaging_bmi_5 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_ild_imaging_bmi_5 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_ild_imaging_bmi_5 [[2, "conf.high"]], digits = 2), nsmall = 2),  
  ")],\n\t",
  "[*Dynamic Lung Volumes*], [], [], [], [], [],\n\t",
  "[#h(1em)FEV#sub[1], z-score], [",
  format (round (model_fev1_bmi_1 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_fev1_bmi_1 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_fev1_bmi_1 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_fev1_bmi_2 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_fev1_bmi_2 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_fev1_bmi_2 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_fev1_bmi_3 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_fev1_bmi_3 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_fev1_bmi_3 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_fev1_bmi_4 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_fev1_bmi_4 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_fev1_bmi_4 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_fev1_bmi_5 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_fev1_bmi_5 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_fev1_bmi_5 [[2, "conf.high"]], digits = 2), nsmall = 2),  
  ")],\n\t",
  "[#h(1em)FVC, z-score], [",
  format (round (model_fvc_bmi_1 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_fvc_bmi_1 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_fvc_bmi_1 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_fvc_bmi_2 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_fvc_bmi_2 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_fvc_bmi_2 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_fvc_bmi_3 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_fvc_bmi_3 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_fvc_bmi_3 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_fvc_bmi_4 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_fvc_bmi_4 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_fvc_bmi_4 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_fvc_bmi_5 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_fvc_bmi_5 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_fvc_bmi_5 [[2, "conf.high"]], digits = 2), nsmall = 2),  
  ")],\n\t",
  "[#h(1em)FEV#sub[1]/FVC, z-score], [",
  format (round (model_fev1_fvc_bmi_1 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_fev1_fvc_bmi_1 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_fev1_fvc_bmi_1 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_fev1_fvc_bmi_2 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_fev1_fvc_bmi_2 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_fev1_fvc_bmi_2 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_fev1_fvc_bmi_3 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_fev1_fvc_bmi_3 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_fev1_fvc_bmi_3 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_fev1_fvc_bmi_4 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_fev1_fvc_bmi_4 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_fev1_fvc_bmi_4 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_fev1_fvc_bmi_5 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_fev1_fvc_bmi_5 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_fev1_fvc_bmi_5 [[2, "conf.high"]], digits = 2), nsmall = 2),  
  ")],\n\t",
  "[#h(1em)Bronchodilator Response], [",
  format (round (model_response_bmi_1 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_response_bmi_1 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_response_bmi_1 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_response_bmi_2 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_response_bmi_2 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_response_bmi_2 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_response_bmi_3 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_response_bmi_3 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_response_bmi_3 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_response_bmi_4 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_response_bmi_4 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_response_bmi_4 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_response_bmi_5 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_response_bmi_5 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_response_bmi_5 [[2, "conf.high"]], digits = 2), nsmall = 2),  
  ")],\n\t",
  "[#h(1em)Adequate Effort], [",
  format (round (model_effort_bmi_1 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_effort_bmi_1 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_effort_bmi_1 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_effort_bmi_2 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_effort_bmi_2 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_effort_bmi_2 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_effort_bmi_3 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_effort_bmi_3 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_effort_bmi_3 [[2, "conf.high"]], digits = 2), nsmall = 2),  
  "], [",
  format (round (model_effort_bmi_4 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_effort_bmi_4 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_effort_bmi_4 [[2, "conf.high"]], digits = 2), nsmall = 2),
  "], [",
  format (round (model_effort_bmi_5 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_effort_bmi_5 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_effort_bmi_5 [[2, "conf.high"]], digits = 2), nsmall = 2),  
  ")],\n\t",
  "[*Static Lung Volumes*], [], [], [], [], [],\n\t",
  "[#h(1em)RV, z-score], [",
  format (round (model_rv_bmi_1 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_rv_bmi_1 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_rv_bmi_1 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_rv_bmi_2 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_rv_bmi_2 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_rv_bmi_2 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_rv_bmi_3 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_rv_bmi_3 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_rv_bmi_3 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_rv_bmi_4 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_rv_bmi_4 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_rv_bmi_4 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_rv_bmi_5 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_rv_bmi_5 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_rv_bmi_5 [[2, "conf.high"]], digits = 2), nsmall = 2),  
  ")],\n\t",
  "[*Diffusing Capacity*], [], [], [], [], [],\n\t",
  "[#h(1em)D#sub[LCO], z-score], [",
  format (round (model_dlco_bmi_1 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_dlco_bmi_1 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_dlco_bmi_1 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_dlco_bmi_2 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_dlco_bmi_2 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_dlco_bmi_2 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_dlco_bmi_3 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_dlco_bmi_3 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_dlco_bmi_3 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_dlco_bmi_4 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_dlco_bmi_4 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_dlco_bmi_4 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_dlco_bmi_5 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_dlco_bmi_5 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_dlco_bmi_5 [[2, "conf.high"]], digits = 2), nsmall = 2),  
  ")],\n\t",
  "[#h(1em)K#sub[CO], z-score], [",
  format (round (model_kco_bmi_1 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_kco_bmi_1 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_kco_bmi_1 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_kco_bmi_2 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_kco_bmi_2 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_kco_bmi_2 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_kco_bmi_3 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_kco_bmi_3 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_kco_bmi_3 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_kco_bmi_4 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_kco_bmi_4 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_kco_bmi_4 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_kco_bmi_5 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_kco_bmi_5 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_kco_bmi_5 [[2, "conf.high"]], digits = 2), nsmall = 2),  
  ")],\n\t",
  "table.hline()\n",
  ")", 
  sep = ""
)  

write_lines (table, "../tables/e-table-22.txt")

################################################################################
## e-Table 23 - Characteristics by Pulmonary Diagnostic Lab
################################################################################

data_lab_1 <- pfts %>%
  filter (location == 1) %>% 
  filter (fvc_z_score >= -1.645 & fev1_fvc_z_score >= -1.645)

data_lab_2 <- pfts %>%
  filter (location == 2) %>% 
  filter (fvc_z_score >= -1.645 & fev1_fvc_z_score >= -1.645)

data_lab_3 <- pfts %>%
  filter (location == 3) %>% 
  filter (fvc_z_score >= -1.645 & fev1_fvc_z_score >= -1.645)

data_lab_4 <- pfts %>%
  filter (location == 4) %>% 
  filter (fvc_z_score >= -1.645 & fev1_fvc_z_score >= -1.645)

# Lab 1

model_age_lab_1 <- tidy (glm (
  formula = restriction ~ age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_lab_1), conf.int = TRUE, exponentiate = TRUE
)

model_sex_lab_1 <- tidy (glm (
  formula = restriction ~ sex + age + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_lab_1), conf.int = TRUE, exponentiate = TRUE
)

model_bmi_lab_1 <- tidy (glm (
  formula = restriction ~ bmi + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_lab_1), conf.int = TRUE, exponentiate = TRUE
)

model_race_lab_1 <- tidy (glm (
  formula = restriction ~ race + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_lab_1), conf.int = TRUE, exponentiate = TRUE
)

model_ethnicity_lab_1 <- tidy (glm (
  formula = restriction ~ ethnicity + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_lab_1), conf.int = TRUE, exponentiate = TRUE
)

model_pack_years_lab_1 <- tidy (glm (
  formula = restriction ~ pack_years + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_lab_1), conf.int = TRUE, exponentiate = TRUE
)

model_cough_lab_1 <- tidy (glm (
  formula = restriction ~ cough + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_lab_1), conf.int = TRUE, exponentiate = TRUE
)

model_dyspnea_lab_1 <- tidy (glm (
  formula = restriction ~ dyspnea + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_lab_1), conf.int = TRUE, exponentiate = TRUE
)

model_wheeze_lab_1 <- tidy (glm (
  formula = restriction ~ wheeze + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_lab_1), conf.int = TRUE, exponentiate = TRUE
)

model_asthma_lab_1 <- tidy (glm (
  formula = restriction ~ asthma + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_lab_1), conf.int = TRUE, exponentiate = TRUE
)

model_bronchiectasis_lab_1 <- tidy (glm (
  formula = restriction ~ bronchiectasis + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_lab_1), conf.int = TRUE, exponentiate = TRUE
)

model_chest_wall_lab_1 <- tidy (glm (
  formula = restriction ~ chest_wall + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_lab_1), conf.int = TRUE, exponentiate = TRUE
)

model_copd_lab_1 <- tidy (glm (
  formula = restriction ~ copd + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_lab_1), conf.int = TRUE, exponentiate = TRUE
)

model_ild_lab_1 <- tidy (glm (
  formula = restriction ~ ild + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_lab_1), conf.int = TRUE, exponentiate = TRUE
)

model_neuromuscular_lab_1 <- tidy (glm (
  formula = restriction ~ neuromuscular + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_lab_1), conf.int = TRUE, exponentiate = TRUE
)

model_none_lab_1 <- tidy (glm (
  formula = restriction ~ none + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_lab_1), conf.int = TRUE, exponentiate = TRUE
)

model_copd_imaging_lab_1 <- tidy (glm (
  formula = restriction ~ copd_imaging + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_lab_1), conf.int = TRUE, exponentiate = TRUE
)

model_ild_imaging_lab_1 <- tidy (glm (
  formula = restriction ~ ild_imaging + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_lab_1), conf.int = TRUE, exponentiate = TRUE
)

model_fev1_lab_1 <- tidy (glm (
  formula = restriction ~ fev1_z_score + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_lab_1), conf.int = TRUE, exponentiate = TRUE
)

model_fvc_lab_1 <- tidy (glm (
  formula = restriction ~ fvc_z_score + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_lab_1), conf.int = TRUE, exponentiate = TRUE
)

model_fev1_fvc_lab_1 <- tidy (glm (
  formula = restriction ~ fev1_fvc_z_score + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_lab_1), conf.int = TRUE, exponentiate = TRUE
)

model_effort_lab_1 <- tidy (glm (
  formula = restriction ~ effort + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_lab_1), conf.int = TRUE, exponentiate = TRUE
)

model_response_lab_1 <- tidy (glm (
  formula = restriction ~ response + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_lab_1), conf.int = TRUE, exponentiate = TRUE
)

model_rv_lab_1 <- tidy (glm (
  formula = restriction ~ rv_z_score + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_lab_1), conf.int = TRUE, exponentiate = TRUE
)

model_dlco_lab_1 <- tidy (glm (
  formula = restriction ~ dlco_z_score + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_lab_1), conf.int = TRUE, exponentiate = TRUE
)

model_kco_lab_1 <- tidy (glm (
  formula = restriction ~ kco_z_score + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_lab_1), conf.int = TRUE, exponentiate = TRUE
)

# Lab 2

model_age_lab_2 <- tidy (glm (
  formula = restriction ~ age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_lab_2), conf.int = TRUE, exponentiate = TRUE
)

model_sex_lab_2 <- tidy (glm (
  formula = restriction ~ sex + age + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_lab_2), conf.int = TRUE, exponentiate = TRUE
)

model_bmi_lab_2 <- tidy (glm (
  formula = restriction ~ bmi + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_lab_2), conf.int = TRUE, exponentiate = TRUE
)

model_race_lab_2 <- tidy (glm (
  formula = restriction ~ race + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_lab_2), conf.int = TRUE, exponentiate = TRUE
)

model_ethnicity_lab_2 <- tidy (glm (
  formula = restriction ~ ethnicity + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_lab_2), conf.int = TRUE, exponentiate = TRUE
)

model_pack_years_lab_2 <- tidy (glm (
  formula = restriction ~ pack_years + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_lab_2), conf.int = TRUE, exponentiate = TRUE
)

model_cough_lab_2 <- tidy (glm (
  formula = restriction ~ cough + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_lab_2), conf.int = TRUE, exponentiate = TRUE
)

model_dyspnea_lab_2 <- tidy (glm (
  formula = restriction ~ dyspnea + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_lab_2), conf.int = TRUE, exponentiate = TRUE
)

model_wheeze_lab_2 <- tidy (glm (
  formula = restriction ~ wheeze + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_lab_2), conf.int = TRUE, exponentiate = TRUE
)

model_asthma_lab_2 <- tidy (glm (
  formula = restriction ~ asthma + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_lab_2), conf.int = TRUE, exponentiate = TRUE
)

model_bronchiectasis_lab_2 <- tidy (glm (
  formula = restriction ~ bronchiectasis + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_lab_2), conf.int = TRUE, exponentiate = TRUE
)

model_chest_wall_lab_2 <- tidy (glm (
  formula = restriction ~ chest_wall + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_lab_2), conf.int = TRUE, exponentiate = TRUE
)

model_copd_lab_2 <- tidy (glm (
  formula = restriction ~ copd + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_lab_2), conf.int = TRUE, exponentiate = TRUE
)

model_ild_lab_2 <- tidy (glm (
  formula = restriction ~ ild + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_lab_2), conf.int = TRUE, exponentiate = TRUE
)

model_neuromuscular_lab_2 <- tidy (glm (
  formula = restriction ~ neuromuscular + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_lab_2), conf.int = TRUE, exponentiate = TRUE
)

model_none_lab_2 <- tidy (glm (
  formula = restriction ~ none + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_lab_2), conf.int = TRUE, exponentiate = TRUE
)

model_copd_imaging_lab_2 <- tidy (glm (
  formula = restriction ~ copd_imaging + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_lab_2), conf.int = TRUE, exponentiate = TRUE
)

model_ild_imaging_lab_2 <- tidy (glm (
  formula = restriction ~ ild_imaging + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_lab_2), conf.int = TRUE, exponentiate = TRUE
)

model_fev1_lab_2 <- tidy (glm (
  formula = restriction ~ fev1_z_score + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_lab_2), conf.int = TRUE, exponentiate = TRUE
)

model_fvc_lab_2 <- tidy (glm (
  formula = restriction ~ fvc_z_score + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_lab_2), conf.int = TRUE, exponentiate = TRUE
)

model_fev1_fvc_lab_2 <- tidy (glm (
  formula = restriction ~ fev1_fvc_z_score + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_lab_2), conf.int = TRUE, exponentiate = TRUE
)

model_effort_lab_2 <- tidy (glm (
  formula = restriction ~ effort + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_lab_2), conf.int = TRUE, exponentiate = TRUE
)

model_response_lab_2 <- tidy (glm (
  formula = restriction ~ response + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_lab_2), conf.int = TRUE, exponentiate = TRUE
)

model_rv_lab_2 <- tidy (glm (
  formula = restriction ~ rv_z_score + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_lab_2), conf.int = TRUE, exponentiate = TRUE
)

model_dlco_lab_2 <- tidy (glm (
  formula = restriction ~ dlco_z_score + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_lab_2), conf.int = TRUE, exponentiate = TRUE
)

model_kco_lab_2 <- tidy (glm (
  formula = restriction ~ kco_z_score + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_lab_2), conf.int = TRUE, exponentiate = TRUE
)

# Lab 3

model_age_lab_3 <- tidy (glm (
  formula = restriction ~ age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_lab_3), conf.int = TRUE, exponentiate = TRUE
)

model_sex_lab_3 <- tidy (glm (
  formula = restriction ~ sex + age + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_lab_3), conf.int = TRUE, exponentiate = TRUE
)

model_bmi_lab_3 <- tidy (glm (
  formula = restriction ~ bmi + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_lab_3), conf.int = TRUE, exponentiate = TRUE
)

model_race_lab_3 <- tidy (glm (
  formula = restriction ~ race + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_lab_3), conf.int = TRUE, exponentiate = TRUE
)

model_ethnicity_lab_3 <- tidy (glm (
  formula = restriction ~ ethnicity + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_lab_3), conf.int = TRUE, exponentiate = TRUE
)

model_pack_years_lab_3 <- tidy (glm (
  formula = restriction ~ pack_years + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_lab_3), conf.int = TRUE, exponentiate = TRUE
)

model_cough_lab_3 <- tidy (glm (
  formula = restriction ~ cough + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_lab_3), conf.int = TRUE, exponentiate = TRUE
)

model_dyspnea_lab_3 <- tidy (glm (
  formula = restriction ~ dyspnea + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_lab_3), conf.int = TRUE, exponentiate = TRUE
)

model_wheeze_lab_3 <- tidy (glm (
  formula = restriction ~ wheeze + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_lab_3), conf.int = TRUE, exponentiate = TRUE
)

model_asthma_lab_3 <- tidy (glm (
  formula = restriction ~ asthma + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_lab_3), conf.int = TRUE, exponentiate = TRUE
)

model_bronchiectasis_lab_3 <- tidy (glm (
  formula = restriction ~ bronchiectasis + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_lab_3), conf.int = TRUE, exponentiate = TRUE
)

model_chest_wall_lab_3 <- tidy (glm (
  formula = restriction ~ chest_wall + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_lab_3), conf.int = TRUE, exponentiate = TRUE
)

model_copd_lab_3 <- tidy (glm (
  formula = restriction ~ copd + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_lab_3), conf.int = TRUE, exponentiate = TRUE
)

model_ild_lab_3 <- tidy (glm (
  formula = restriction ~ ild + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_lab_3), conf.int = TRUE, exponentiate = TRUE
)

model_neuromuscular_lab_3 <- tidy (glm (
  formula = restriction ~ neuromuscular + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_lab_3), conf.int = TRUE, exponentiate = TRUE
)

model_none_lab_3 <- tidy (glm (
  formula = restriction ~ none + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_lab_3), conf.int = TRUE, exponentiate = TRUE
)

model_copd_imaging_lab_3 <- tidy (glm (
  formula = restriction ~ copd_imaging + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_lab_3), conf.int = TRUE, exponentiate = TRUE
)

model_ild_imaging_lab_3 <- tidy (glm (
  formula = restriction ~ ild_imaging + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_lab_3), conf.int = TRUE, exponentiate = TRUE
)

model_fev1_lab_3 <- tidy (glm (
  formula = restriction ~ fev1_z_score + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_lab_3), conf.int = TRUE, exponentiate = TRUE
)

model_fvc_lab_3 <- tidy (glm (
  formula = restriction ~ fvc_z_score + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_lab_3), conf.int = TRUE, exponentiate = TRUE
)

model_fev1_fvc_lab_3 <- tidy (glm (
  formula = restriction ~ fev1_fvc_z_score + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_lab_3), conf.int = TRUE, exponentiate = TRUE
)

model_effort_lab_3 <- tidy (glm (
  formula = restriction ~ effort + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_lab_3), conf.int = TRUE, exponentiate = TRUE
)

model_response_lab_3 <- tidy (glm (
  formula = restriction ~ response + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_lab_3), conf.int = TRUE, exponentiate = TRUE
)

model_rv_lab_3 <- tidy (glm (
  formula = restriction ~ rv_z_score + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_lab_3), conf.int = TRUE, exponentiate = TRUE
)

model_dlco_lab_3 <- tidy (glm (
  formula = restriction ~ dlco_z_score + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_lab_3), conf.int = TRUE, exponentiate = TRUE
)

model_kco_lab_3 <- tidy (glm (
  formula = restriction ~ kco_z_score + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_lab_3), conf.int = TRUE, exponentiate = TRUE
)

# Lab 4

model_age_lab_4 <- tidy (glm (
  formula = restriction ~ age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_lab_4), conf.int = TRUE, exponentiate = TRUE
)

model_sex_lab_4 <- tidy (glm (
  formula = restriction ~ sex + age + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_lab_4), conf.int = TRUE, exponentiate = TRUE
)

model_bmi_lab_4 <- tidy (glm (
  formula = restriction ~ bmi + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_lab_4), conf.int = TRUE, exponentiate = TRUE
)

model_race_lab_4 <- tidy (glm (
  formula = restriction ~ race + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_lab_4), conf.int = TRUE, exponentiate = TRUE
)

model_ethnicity_lab_4 <- tidy (glm (
  formula = restriction ~ ethnicity + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_lab_4), conf.int = TRUE, exponentiate = TRUE
)

# model_pack_years_lab_4 <- tidy (glm (
#   formula = restriction ~ pack_years + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
#   family = "binomial",
#   data = data_lab_4), conf.int = TRUE, exponentiate = TRUE
# )

# model_cough_lab_4 <- tidy (glm (
#   formula = restriction ~ cough + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
#   family = "binomial",
#   data = data_lab_4), conf.int = TRUE, exponentiate = TRUE
# )

# model_dyspnea_lab_4 <- tidy (glm (
#   formula = restriction ~ dyspnea + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
#   family = "binomial",
#   data = data_lab_4), conf.int = TRUE, exponentiate = TRUE
# )
# 
# model_wheeze_lab_4 <- tidy (glm (
#   formula = restriction ~ wheeze + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
#   family = "binomial",
#   data = data_lab_4), conf.int = TRUE, exponentiate = TRUE
# )

model_asthma_lab_4 <- tidy (glm (
  formula = restriction ~ asthma + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_lab_4), conf.int = TRUE, exponentiate = TRUE
)

model_bronchiectasis_lab_4 <- tidy (glm (
  formula = restriction ~ bronchiectasis + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_lab_4), conf.int = TRUE, exponentiate = TRUE
)

model_chest_wall_lab_4 <- tidy (glm (
  formula = restriction ~ chest_wall + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_lab_4), conf.int = TRUE, exponentiate = TRUE
)

model_copd_lab_4 <- tidy (glm (
  formula = restriction ~ copd + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_lab_4), conf.int = TRUE, exponentiate = TRUE
)

model_ild_lab_4 <- tidy (glm (
  formula = restriction ~ ild + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_lab_4), conf.int = TRUE, exponentiate = TRUE
)

model_neuromuscular_lab_4 <- tidy (glm (
  formula = restriction ~ neuromuscular + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_lab_4), conf.int = TRUE, exponentiate = TRUE
)

model_none_lab_4 <- tidy (glm (
  formula = restriction ~ none + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_lab_4), conf.int = TRUE, exponentiate = TRUE
)

model_copd_imaging_lab_4 <- tidy (glm (
  formula = restriction ~ copd_imaging + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_lab_4), conf.int = TRUE, exponentiate = TRUE
)

model_ild_imaging_lab_4 <- tidy (glm (
  formula = restriction ~ ild_imaging + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_lab_4), conf.int = TRUE, exponentiate = TRUE
)

model_fev1_lab_4 <- tidy (glm (
  formula = restriction ~ fev1_z_score + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_lab_4), conf.int = TRUE, exponentiate = TRUE
)

model_fvc_lab_4 <- tidy (glm (
  formula = restriction ~ fvc_z_score + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_lab_4), conf.int = TRUE, exponentiate = TRUE
)

model_fev1_fvc_lab_4 <- tidy (glm (
  formula = restriction ~ fev1_fvc_z_score + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_lab_4), conf.int = TRUE, exponentiate = TRUE
)

# model_effort_lab_4 <- tidy (glm (
#   formula = restriction ~ effort + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
#   family = "binomial",
#   data = data_lab_4), conf.int = TRUE, exponentiate = TRUE
# )

model_response_lab_4 <- tidy (glm (
  formula = restriction ~ response + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_lab_4), conf.int = TRUE, exponentiate = TRUE
)

model_rv_lab_4 <- tidy (glm (
  formula = restriction ~ rv_z_score + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_lab_4), conf.int = TRUE, exponentiate = TRUE
)

model_dlco_lab_4 <- tidy (glm (
  formula = restriction ~ dlco_z_score + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_lab_4), conf.int = TRUE, exponentiate = TRUE
)

model_kco_lab_4 <- tidy (glm (
  formula = restriction ~ kco_z_score + age + sex + fev1_z_score + fvc_z_score + fev1_fvc_z_score,
  family = "binomial",
  data = data_lab_4), conf.int = TRUE, exponentiate = TRUE
)

table <- paste (
  
  "#table(\n\t",
  "inset: (x: 4pt, y: 3.5pt),\n\t",
	"columns: 5,\n\t",
	"align: (left, center, center, center, center),\n\t",
	"table.hline(),\n\t",
	"[], table.cell(colspan: 4, [*Adjusted Odds Ratio (95% CI)#super[a]*]),\n\t",
	"table.hline(start:1, end:5),\n\t",
	"[], [*Lab 1*], [*Lab 2*], [*Lab 3*], [*Lab 4*],\n\t",
  "table.hline(),\n\t",
  "[*Age, yrs*], [",
  format (round (model_age_lab_1 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_age_lab_1 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_age_lab_1 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_age_lab_2 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_age_lab_2 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_age_lab_2 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_age_lab_3 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_age_lab_3 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_age_lab_3 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_age_lab_4 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_age_lab_4 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_age_lab_4 [[2, "conf.high"]], digits = 2), nsmall = 2),  
  ")],\n\t",    
  "[*Sex*], [], [], [], [],\n\t",
  "[#h(1em)Male], [1.00 (Reference)], [1.00 (Reference)], [1.00 (Reference)], [1.00 (Reference)],\n\t",
  "[#h(1em)Female], [",
  format (round (model_sex_lab_1 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_sex_lab_1 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_sex_lab_1 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_sex_lab_2 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_sex_lab_2 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_sex_lab_2 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_sex_lab_3 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_sex_lab_3 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_sex_lab_3 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_sex_lab_4 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_sex_lab_4 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_sex_lab_4 [[2, "conf.high"]], digits = 2), nsmall = 2),  
  ")],\n\t",
  "[*Race*], [], [], [], [],\n\t",
  "[#h(1em)White], [1.00 (Reference)], [1.00 (Reference)], [1.00 (Reference)], [1.00 (Reference)],\n\t",
  "[#h(1em)Black], [",
  format (round (model_race_lab_1 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_race_lab_1 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_race_lab_1 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_race_lab_2 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_race_lab_2 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_race_lab_2 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_race_lab_3 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_race_lab_3 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_race_lab_3 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_race_lab_4 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_race_lab_4 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_race_lab_4 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")],\n\t",
  "[#h(1em)Asian], [",
  format (round (model_race_lab_1 [[3, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_race_lab_1 [[3, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_race_lab_1 [[3, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_race_lab_2 [[3, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_race_lab_2 [[3, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_race_lab_2 [[3, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_race_lab_3 [[3, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_race_lab_3 [[3, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_race_lab_3 [[3, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_race_lab_4 [[3, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_race_lab_4 [[3, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_race_lab_4 [[3, "conf.high"]], digits = 2), nsmall = 2),
  ")],\n\t",
  "[#h(1em)Other], [",
  format (round (model_race_lab_1 [[4, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_race_lab_1 [[4, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_race_lab_1 [[4, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_race_lab_2 [[4, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_race_lab_2 [[4, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_race_lab_2 [[4, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_race_lab_3 [[4, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_race_lab_3 [[4, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_race_lab_3 [[4, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_race_lab_4 [[4, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_race_lab_4 [[4, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_race_lab_4 [[4, "conf.high"]], digits = 2), nsmall = 2),
  ")],\n\t",
  "[*Ethnicity*], [], [], [], [],\n\t",
  "[#h(1em)Hispanic], [1.00 (Reference)], [1.00 (Reference)], [1.00 (Reference)], [1.00 (Reference)],\n\t",
  "[#h(1em)Not Hispanic], [",
  format (round (model_ethnicity_lab_1 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_ethnicity_lab_1 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_ethnicity_lab_1 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_ethnicity_lab_2 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_ethnicity_lab_2 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_ethnicity_lab_2 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_ethnicity_lab_3 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_ethnicity_lab_3 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_ethnicity_lab_3 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_ethnicity_lab_4 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_ethnicity_lab_4 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_ethnicity_lab_4 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")],\n\t",  
  "[*Pack Years, yrs*], [",
  format (round (model_pack_years_lab_1 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_pack_years_lab_1 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_pack_years_lab_1 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_pack_years_lab_2 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_pack_years_lab_2 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_pack_years_lab_2 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_pack_years_lab_3 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_pack_years_lab_3 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_pack_years_lab_3 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [---],\n\t",
  # format (round (model_pack_years_lab_4 [[2, "estimate"]], digits = 2), nsmall = 2),
  # " (",
  # format (round (model_pack_years_lab_4 [[2, "conf.low"]], digits = 2), nsmall = 2),
  # "--",
  # format (round (model_pack_years_lab_4 [[2, "conf.high"]], digits = 2), nsmall = 2),
  # ")],\n\t",
  "[*Respiratory Symptoms*], [], [], [], [],\n\t",
  "[#h(1em)Cough], [",
  format (round (model_cough_lab_1 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_cough_lab_1 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_cough_lab_1 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_cough_lab_2 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_cough_lab_2 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_cough_lab_2 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_cough_lab_3 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_cough_lab_3 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_cough_lab_3 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [---],\n\t",
  # format (round (model_cough_lab_4 [[2, "estimate"]], digits = 2), nsmall = 2),
  # " (",
  # format (round (model_cough_lab_4 [[2, "conf.low"]], digits = 2), nsmall = 2),
  # "--",
  # format (round (model_cough_lab_4 [[2, "conf.high"]], digits = 2), nsmall = 2),
  # ")],\n\t",
  "[#h(1em)Dyspnea], [",
  format (round (model_dyspnea_lab_1 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_dyspnea_lab_1 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_dyspnea_lab_1 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_dyspnea_lab_2 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_dyspnea_lab_2 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_dyspnea_lab_2 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_dyspnea_lab_3 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_dyspnea_lab_3 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_dyspnea_lab_3 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [---],\n\t",
  # format (round (model_dyspnea_lab_4 [[2, "estimate"]], digits = 2), nsmall = 2),
  # " (",
  # format (round (model_dyspnea_lab_4 [[2, "conf.low"]], digits = 2), nsmall = 2),
  # "--",
  # format (round (model_dyspnea_lab_4 [[2, "conf.high"]], digits = 2), nsmall = 2),
  # ")],\n\t",
  "[#h(1em)Wheeze], [",
  format (round (model_wheeze_lab_1 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_wheeze_lab_1 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_wheeze_lab_1 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_wheeze_lab_2 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_wheeze_lab_2 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_wheeze_lab_2 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_wheeze_lab_3 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_wheeze_lab_3 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_wheeze_lab_3 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [---],\n\t",
  # format (round (model_wheeze_lab_4 [[2, "estimate"]], digits = 2), nsmall = 2),
  # " (",
  # format (round (model_wheeze_lab_4 [[2, "conf.low"]], digits = 2), nsmall = 2),
  # "--",
  # format (round (model_wheeze_lab_4 [[2, "conf.high"]], digits = 2), nsmall = 2),
  # ")],\n\t",
  "[*Respiratory Diseases*], [], [], [], [],\n\t",
  "[#h(1em)Asthma], [",
  format (round (model_asthma_lab_1 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_asthma_lab_1 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_asthma_lab_1 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_asthma_lab_2 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_asthma_lab_2 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_asthma_lab_2 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_asthma_lab_3 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_asthma_lab_3 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_asthma_lab_3 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_asthma_lab_4 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_asthma_lab_4 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_asthma_lab_4 [[2, "conf.high"]], digits = 2), nsmall = 2),  
  ")],\n\t",
  "[#h(1em)Bronchiectasis], [",
  format (round (model_bronchiectasis_lab_1 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_bronchiectasis_lab_1 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_bronchiectasis_lab_1 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_bronchiectasis_lab_2 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_bronchiectasis_lab_2 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_bronchiectasis_lab_2 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_bronchiectasis_lab_3 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_bronchiectasis_lab_3 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_bronchiectasis_lab_3 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_bronchiectasis_lab_4 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_bronchiectasis_lab_4 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_bronchiectasis_lab_4 [[2, "conf.high"]], digits = 2), nsmall = 2),  
  ")],\n\t",
  "[#h(1em)Chest Wall Disorder], [",
  format (round (model_chest_wall_lab_1 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_chest_wall_lab_1 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_chest_wall_lab_1 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_chest_wall_lab_2 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_chest_wall_lab_2 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_chest_wall_lab_2 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [---],",
  # format (round (model_chest_wall_lab_3 [[2, "estimate"]], digits = 2), nsmall = 2),
  # " (",
  # format (round (model_chest_wall_lab_3 [[2, "conf.low"]], digits = 2), nsmall = 2),
  # "--",
  # format (round (model_chest_wall_lab_3 [[2, "conf.high"]], digits = 2), nsmall = 2),
  # ")], [",
  "[---],\n\t",
  # format (round (model_chest_wall_lab_4 [[2, "estimate"]], digits = 2), nsmall = 2),
  # " (",
  # format (round (model_chest_wall_lab_4 [[2, "conf.low"]], digits = 2), nsmall = 2),
  # "--",
  # format (round (model_chest_wall_lab_4 [[2, "conf.high"]], digits = 2), nsmall = 2),  
  # ")],\n\t",
  "[#h(1em)Chronic Obstructive Pulmonary Disease], [",
  format (round (model_copd_lab_1 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_copd_lab_1 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_copd_lab_1 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_copd_lab_2 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_copd_lab_2 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_copd_lab_2 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_copd_lab_3 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_copd_lab_3 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_copd_lab_3 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_copd_lab_4 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_copd_lab_4 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_copd_lab_4 [[2, "conf.high"]], digits = 2), nsmall = 2),  
  ")],\n\t",
  "[#h(1em)Interstitial Lung Disease], [",
  format (round (model_ild_lab_1 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_ild_lab_1 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_ild_lab_1 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_ild_lab_2 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_ild_lab_2 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_ild_lab_2 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_ild_lab_3 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_ild_lab_3 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_ild_lab_3 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_ild_lab_4 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_ild_lab_4 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_ild_lab_4 [[2, "conf.high"]], digits = 2), nsmall = 2),  
  ")],\n\t",
  "[#h(1em)Neuromuscular Disease], [",
  format (round (model_neuromuscular_lab_1 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_neuromuscular_lab_1 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_neuromuscular_lab_1 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_neuromuscular_lab_2 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_neuromuscular_lab_2 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_neuromuscular_lab_2 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_neuromuscular_lab_3 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_neuromuscular_lab_3 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_neuromuscular_lab_3 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_neuromuscular_lab_4 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_neuromuscular_lab_4 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_neuromuscular_lab_4 [[2, "conf.high"]], digits = 2), nsmall = 2),  
  ")],\n\t",
  "[#h(1em)None], [",
  format (round (model_none_lab_1 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_none_lab_1 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_none_lab_1 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_none_lab_2 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_none_lab_2 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_none_lab_2 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_none_lab_3 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_none_lab_3 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_none_lab_3 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_none_lab_4 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_none_lab_4 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_none_lab_4 [[2, "conf.high"]], digits = 2), nsmall = 2),  
  ")],\n\t",  
  "[*Computed Tomography Findings*], [], [], [], [],\n\t",
  "[#h(1em)Chronic Obstructive Pulmonary Disease], [",
  format (round (model_copd_imaging_lab_1 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_copd_imaging_lab_1 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_copd_imaging_lab_1 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_copd_imaging_lab_2 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_copd_imaging_lab_2 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_copd_imaging_lab_2 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_copd_imaging_lab_3 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_copd_imaging_lab_3 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_copd_imaging_lab_3 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_copd_imaging_lab_4 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_copd_imaging_lab_4 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_copd_imaging_lab_4 [[2, "conf.high"]], digits = 2), nsmall = 2),  
  ")],\n\t",
  "[#h(1em)Interstitial Lung Disease], [",
  format (round (model_ild_imaging_lab_1 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_ild_imaging_lab_1 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_ild_imaging_lab_1 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_ild_imaging_lab_2 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_ild_imaging_lab_2 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_ild_imaging_lab_2 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_ild_imaging_lab_3 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_ild_imaging_lab_3 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_ild_imaging_lab_3 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_ild_imaging_lab_4 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_ild_imaging_lab_4 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_ild_imaging_lab_4 [[2, "conf.high"]], digits = 2), nsmall = 2),  
  ")],\n\t",     
  "[*Dynamic Lung Volumes*], [], [], [], [],\n\t",
  "[#h(1em)FEV#sub[1], z-score], [",
  format (round (model_fev1_lab_1 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_fev1_lab_1 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_fev1_lab_1 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_fev1_lab_2 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_fev1_lab_2 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_fev1_lab_2 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_fev1_lab_3 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_fev1_lab_3 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_fev1_lab_3 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_fev1_lab_4 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_fev1_lab_4 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_fev1_lab_4 [[2, "conf.high"]], digits = 2), nsmall = 2),  
  ")],\n\t",
  "[#h(1em)FVC, z-score], [",
  format (round (model_fvc_lab_1 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_fvc_lab_1 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_fvc_lab_1 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_fvc_lab_2 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_fvc_lab_2 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_fvc_lab_2 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_fvc_lab_3 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_fvc_lab_3 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_fvc_lab_3 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_fvc_lab_4 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_fvc_lab_4 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_fvc_lab_4 [[2, "conf.high"]], digits = 2), nsmall = 2),  
  ")],\n\t",
  "[#h(1em)FEV#sub[1]/FVC, z-score], [",
  format (round (model_fev1_fvc_lab_1 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_fev1_fvc_lab_1 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_fev1_fvc_lab_1 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_fev1_fvc_lab_2 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_fev1_fvc_lab_2 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_fev1_fvc_lab_2 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_fev1_fvc_lab_3 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_fev1_fvc_lab_3 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_fev1_fvc_lab_3 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_fev1_fvc_lab_4 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_fev1_fvc_lab_4 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_fev1_fvc_lab_4 [[2, "conf.high"]], digits = 2), nsmall = 2),  
  ")],\n\t",   
  "[#h(1em)Bronchodilator Response], [",
  format (round (model_response_lab_1 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_response_lab_1 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_response_lab_1 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_response_lab_2 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_response_lab_2 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_response_lab_2 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_response_lab_3 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_response_lab_3 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_response_lab_3 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [---],\n\t",
  # format (round (model_response_lab_4 [[2, "estimate"]], digits = 2), nsmall = 2),
  # " (",
  # format (round (model_response_lab_4 [[2, "conf.low"]], digits = 2), nsmall = 2),
  # "--",
  # format (round (model_response_lab_4 [[2, "conf.high"]], digits = 2), nsmall = 2),  
  # ")],\n\t",
  "[#h(1em)Adequate Effort], [",
  format (round (model_effort_lab_1 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_effort_lab_1 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_effort_lab_1 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_effort_lab_2 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_effort_lab_2 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_effort_lab_2 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [---],\n\t",
  # format (round (model_effort_lab_3 [[2, "estimate"]], digits = 2), nsmall = 2),
  # " (",
  # format (round (model_effort_lab_3 [[2, "conf.low"]], digits = 2), nsmall = 2),
  # "--",
  # format (round (model_effort_lab_3 [[2, "conf.high"]], digits = 2), nsmall = 2),
  "[---],\n\t",
  # format (round (model_effort_lab_4 [[2, "estimate"]], digits = 2), nsmall = 2),
  # " (",
  # format (round (model_effort_lab_4 [[2, "conf.low"]], digits = 2), nsmall = 2),
  # "--",
  # format (round (model_effort_lab_4 [[2, "conf.high"]], digits = 2), nsmall = 2),
  # ")],\n\t",
  "[*Static Lung Volumes*], [], [], [], [],\n\t",
  "[#h(1em)RV, z-score], [",
  format (round (model_rv_lab_1 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_rv_lab_1 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_rv_lab_1 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_rv_lab_2 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_rv_lab_2 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_rv_lab_2 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_rv_lab_3 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_rv_lab_3 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_rv_lab_3 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_rv_lab_4 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_rv_lab_4 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_rv_lab_4 [[2, "conf.high"]], digits = 2), nsmall = 2),  
  ")],\n\t",
  "[*Diffusing Capacity*], [], [], [], [],\n\t",
  "[#h(1em)D#sub[LCO], z-score], [",
  format (round (model_dlco_lab_1 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_dlco_lab_1 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_dlco_lab_1 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_dlco_lab_2 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_dlco_lab_2 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_dlco_lab_2 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_dlco_lab_3 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_dlco_lab_3 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_dlco_lab_3 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_dlco_lab_4 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_dlco_lab_4 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_dlco_lab_4 [[2, "conf.high"]], digits = 2), nsmall = 2),  
  ")],\n\t",
  "[#h(1em)K#sub[CO], z-score], [",
  format (round (model_kco_lab_1 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_kco_lab_1 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_kco_lab_1 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_kco_lab_2 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_kco_lab_2 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_kco_lab_2 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_kco_lab_3 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_kco_lab_3 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_kco_lab_3 [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  format (round (model_kco_lab_4 [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_kco_lab_4 [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_kco_lab_4 [[2, "conf.high"]], digits = 2), nsmall = 2),  
  ")],\n\t",   
	"table.hline(),\n",
  ")", 
  sep = ""
  
)

write_lines (table, "../tables/e-table-23.txt")

################################################################################
## e-Table 24 - Characteristics Associated with Normal Spirometry in PFTs with
## Restriction
################################################################################

data <- pfts %>%
  filter (tlc_z_score < -1.645) %>% 
  mutate (normal_spirometry = case_when (
    fvc_z_score >= -1.645 & fev1_fvc_z_score >= -1.645 ~ 1,
    TRUE ~ 0)
  ) %>% 
  mutate (normal_spirometry = as.factor (normal_spirometry))

# Unadjusted

model_age_unadjusted <- tidy (glm (
  formula = normal_spirometry ~ age,
  family = "binomial",
  data = data), conf.int = TRUE, exponentiate = TRUE
)

model_sex_unadjusted <- tidy (glm (
  formula = normal_spirometry ~ sex,
  family = "binomial",
  data = data), conf.int = TRUE, exponentiate = TRUE
)

model_bmi_unadjusted <- tidy (glm (
  formula = normal_spirometry ~ bmi,
  family = "binomial",
  data = data), conf.int = TRUE, exponentiate = TRUE
)

model_race_unadjusted <- tidy (glm (
  formula = normal_spirometry ~ race,
  family = "binomial",
  data = data), conf.int = TRUE, exponentiate = TRUE
)

model_ethnicity_unadjusted <- tidy (glm (
  formula = normal_spirometry ~ ethnicity,
  family = "binomial",
  data = data), conf.int = TRUE, exponentiate = TRUE
)

model_pack_years_unadjusted <- tidy (glm (
  formula = normal_spirometry ~ pack_years,
  family = "binomial",
  data = data), conf.int = TRUE, exponentiate = TRUE
)

model_cough_unadjusted <- tidy (glm (
  formula = normal_spirometry ~ cough,
  family = "binomial",
  data = data), conf.int = TRUE, exponentiate = TRUE
)

model_dyspnea_unadjusted <- tidy (glm (
  formula = normal_spirometry ~ dyspnea,
  family = "binomial",
  data = data), conf.int = TRUE, exponentiate = TRUE
)

model_wheeze_unadjusted <- tidy (glm (
  formula = normal_spirometry ~ wheeze,
  family = "binomial",
  data = data), conf.int = TRUE, exponentiate = TRUE
)

model_asthma_unadjusted <- tidy (glm (
  formula = normal_spirometry ~ asthma,
  family = "binomial",
  data = data), conf.int = TRUE, exponentiate = TRUE
)

model_bronchiectasis_unadjusted <- tidy (glm (
  formula = normal_spirometry ~ bronchiectasis,
  family = "binomial",
  data = data), conf.int = TRUE, exponentiate = TRUE
)

model_chest_wall_unadjusted <- tidy (glm (
  formula = normal_spirometry ~ chest_wall,
  family = "binomial",
  data = data), conf.int = TRUE, exponentiate = TRUE
)

model_copd_unadjusted <- tidy (glm (
  formula = normal_spirometry ~ copd,
  family = "binomial",
  data = data), conf.int = TRUE, exponentiate = TRUE
)

model_ild_unadjusted <- tidy (glm (
  formula = normal_spirometry ~ ild,
  family = "binomial",
  data = data), conf.int = TRUE, exponentiate = TRUE
)

model_neuromuscular_unadjusted <- tidy (glm (
  formula = normal_spirometry ~ neuromuscular,
  family = "binomial",
  data = data), conf.int = TRUE, exponentiate = TRUE
)

model_none_unadjusted <- tidy (glm (
  formula = normal_spirometry ~ none,
  family = "binomial",
  data = data), conf.int = TRUE, exponentiate = TRUE
)

model_copd_imaging_unadjusted <- tidy (glm (
  formula = normal_spirometry ~ copd_imaging,
  family = "binomial",
  data = data), conf.int = TRUE, exponentiate = TRUE
)

model_ild_imaging_unadjusted <- tidy (glm (
  formula = normal_spirometry ~ ild_imaging,
  family = "binomial",
  data = data), conf.int = TRUE, exponentiate = TRUE
)

model_fev1_unadjusted <- tidy (glm (
  formula = normal_spirometry ~ fev1_z_score,
  family = "binomial",
  data = data), conf.int = TRUE, exponentiate = TRUE
)

model_fvc_unadjusted <- tidy (glm (
  formula = normal_spirometry ~ fvc_z_score,
  family = "binomial",
  data = data), conf.int = TRUE, exponentiate = TRUE
)

model_fev1_fvc_unadjusted <- tidy (glm (
  formula = normal_spirometry ~ fev1_fvc_z_score,
  family = "binomial",
  data = data), conf.int = TRUE, exponentiate = TRUE
)

model_response_unadjusted <- tidy (glm (
  formula = normal_spirometry ~ response,
  family = "binomial",
  data = data), conf.int = TRUE, exponentiate = TRUE
)

model_effort_unadjusted <- tidy (glm (
  formula = normal_spirometry ~ effort,
  family = "binomial",
  data = data), conf.int = TRUE, exponentiate = TRUE
)

model_rv_unadjusted <- tidy (glm (
  formula = normal_spirometry ~ rv_z_score,
  family = "binomial",
  data = data), conf.int = TRUE, exponentiate = TRUE
)

model_dlco_unadjusted <- tidy (glm (
  formula = normal_spirometry ~ dlco_z_score,
  family = "binomial",
  data = data), conf.int = TRUE, exponentiate = TRUE
)

model_kco_unadjusted <- tidy (glm (
  formula = normal_spirometry ~ kco_z_score,
  family = "binomial",
  data = data), conf.int = TRUE, exponentiate = TRUE
)

# Adjusted

model_age_adjusted <- tidy (glm (
  formula = normal_spirometry ~ age + sex + tlc_z_score,
  family = "binomial",
  data = data), conf.int = TRUE, exponentiate = TRUE
)

model_sex_adjusted <- tidy (glm (
  formula = normal_spirometry ~ sex + age + tlc_z_score,
  family = "binomial",
  data = data), conf.int = TRUE, exponentiate = TRUE
)

model_bmi_adjusted <- tidy (glm (
  formula = normal_spirometry ~ bmi + age + sex + tlc_z_score,
  family = "binomial",
  data = data), conf.int = TRUE, exponentiate = TRUE
)

model_race_adjusted <- tidy (glm (
  formula = normal_spirometry ~ race + age + sex + tlc_z_score,
  family = "binomial",
  data = data), conf.int = TRUE, exponentiate = TRUE
)

model_ethnicity_adjusted <- tidy (glm (
  formula = normal_spirometry ~ ethnicity + age + sex + tlc_z_score,
  family = "binomial",
  data = data), conf.int = TRUE, exponentiate = TRUE
)

model_pack_years_adjusted <- tidy (glm (
  formula = normal_spirometry ~ pack_years + age + sex + tlc_z_score,
  family = "binomial",
  data = data), conf.int = TRUE, exponentiate = TRUE
)

model_cough_adjusted <- tidy (glm (
  formula = normal_spirometry ~ cough + age + sex + tlc_z_score,
  family = "binomial",
  data = data), conf.int = TRUE, exponentiate = TRUE
)

model_dyspnea_adjusted <- tidy (glm (
  formula = normal_spirometry ~ dyspnea + age + sex + tlc_z_score,
  family = "binomial",
  data = data), conf.int = TRUE, exponentiate = TRUE
)

model_wheeze_adjusted <- tidy (glm (
  formula = normal_spirometry ~ wheeze + age + sex + tlc_z_score,
  family = "binomial",
  data = data), conf.int = TRUE, exponentiate = TRUE
)

model_asthma_adjusted <- tidy (glm (
  formula = normal_spirometry ~ asthma + age + sex + tlc_z_score,
  family = "binomial",
  data = data), conf.int = TRUE, exponentiate = TRUE
)

model_bronchiectasis_adjusted <- tidy (glm (
  formula = normal_spirometry ~ bronchiectasis + age + sex + tlc_z_score,
  family = "binomial",
  data = data), conf.int = TRUE, exponentiate = TRUE
)

model_chest_wall_adjusted <- tidy (glm (
  formula = normal_spirometry ~ chest_wall + age + sex + tlc_z_score,
  family = "binomial",
  data = data), conf.int = TRUE, exponentiate = TRUE
)

model_copd_adjusted <- tidy (glm (
  formula = normal_spirometry ~ copd + age + sex + tlc_z_score,
  family = "binomial",
  data = data), conf.int = TRUE, exponentiate = TRUE
)

model_ild_adjusted <- tidy (glm (
  formula = normal_spirometry ~ ild + age + sex + tlc_z_score,
  family = "binomial",
  data = data), conf.int = TRUE, exponentiate = TRUE
)

model_neuromuscular_adjusted <- tidy (glm (
  formula = normal_spirometry ~ neuromuscular + age + sex + tlc_z_score,
  family = "binomial",
  data = data), conf.int = TRUE, exponentiate = TRUE
)

model_none_adjusted <- tidy (glm (
  formula = normal_spirometry ~ none + age + sex + tlc_z_score,
  family = "binomial",
  data = data), conf.int = TRUE, exponentiate = TRUE
)

model_copd_imaging_adjusted <- tidy (glm (
  formula = normal_spirometry ~ copd_imaging + age + sex  + tlc_z_score,
  family = "binomial",
  data = data), conf.int = TRUE, exponentiate = TRUE
)

model_ild_imaging_adjusted <- tidy (glm (
  formula = normal_spirometry ~ ild_imaging + age + sex + tlc_z_score,
  family = "binomial",
  data = data), conf.int = TRUE, exponentiate = TRUE
)

model_fev1_adjusted <- tidy (glm (
  formula = normal_spirometry ~ fev1_z_score + age + sex + tlc_z_score,
  family = "binomial",
  data = data), conf.int = TRUE, exponentiate = TRUE
)

model_fvc_adjusted <- tidy (glm (
  formula = normal_spirometry ~ fvc_z_score + age + sex + tlc_z_score,
  family = "binomial",
  data = data), conf.int = TRUE, exponentiate = TRUE
)

model_fev1_fvc_adjusted <- tidy (glm (
  formula = normal_spirometry ~ fev1_fvc_z_score + age + sex + tlc_z_score,
  family = "binomial",
  data = data), conf.int = TRUE, exponentiate = TRUE
)

model_response_adjusted <- tidy (glm (
  formula = normal_spirometry ~ response + age + sex + tlc_z_score,
  family = "binomial",
  data = data), conf.int = TRUE, exponentiate = TRUE
)

model_effort_adjusted <- tidy (glm (
  formula = normal_spirometry ~ effort + age + sex + tlc_z_score,
  family = "binomial",
  data = data), conf.int = TRUE, exponentiate = TRUE
)

model_rv_adjusted <- tidy (glm (
  formula = normal_spirometry ~ rv_z_score + age + sex + tlc_z_score,
  family = "binomial",
  data = data), conf.int = TRUE, exponentiate = TRUE
)

model_dlco_adjusted <- tidy (glm (
  formula = normal_spirometry ~ dlco_z_score + age + sex + tlc_z_score,
  family = "binomial",
  data = data), conf.int = TRUE, exponentiate = TRUE
)

model_kco_adjusted <- tidy (glm (
  formula = normal_spirometry ~ kco_z_score + age + sex + tlc_z_score,
  family = "binomial",
  data = data), conf.int = TRUE, exponentiate = TRUE
)

table <- paste (
  "#table(\n\t",
	"inset: (x: 4pt, y: 3.5pt),\n\t",
  "columns: 6,\n\t",
	"align: (left, center, right, center, center, right),\n\t",
	"table.hline(),\n\t",
  "[], table.cell(colspan: 5, [*Association with Normal Spirometry*]),\n\t",
  "table.hline(start:1, end:6),\n\t",
	"[], table.cell(colspan: 2, [*Unadjusted*]), [], table.cell(colspan: 2,[*Adjusted#super[a]*]),\n\t",
	"table.hline(start:1, end:3), table.hline(start:4, end:6),\n\t",
	"[*Characteristic*], [*Odds Ratio (95% CI)*], [*$P$ Value*], [], [*Odds Ratio (95% CI)*], [*$P$ Value*],\n\t",
	"table.hline(),\n\t",
  "[*Age, yr*], [",
  format (round (model_age_unadjusted [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_age_unadjusted [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_age_unadjusted [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  print_p_value (model_age_unadjusted [[2, "p.value"]]),
  "], [], [",
  format (round (model_age_adjusted [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_age_adjusted [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_age_adjusted [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  print_p_value (model_age_adjusted [[2, "p.value"]]),
  "],\n\t",
  "[*Sex*], [], [], [], [], [],\n\t",
  "[#h(1em)Male], [1.00 (Reference)], [---], [], [1.00 (Reference)], [---],\n\t",
  "[#h(1em)Female], [",
  format (round (model_sex_unadjusted [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_sex_unadjusted [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_sex_unadjusted [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  print_p_value (model_sex_unadjusted [[2, "p.value"]]),
  "], [], [",
  format (round (model_sex_adjusted [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_sex_adjusted [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_sex_adjusted [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  print_p_value (model_sex_adjusted [[2, "p.value"]]),  
  "],\n\t",  
  "[*Race*], [], [], [], [], [],\n\t",
  "[#h(1em)White], [1.00 (Reference)], [---], [], [1.00 (Reference)], [---],\n\t",
  "[#h(1em)Black], [",
  format (round (model_race_unadjusted [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_race_unadjusted [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_race_unadjusted [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  print_p_value (model_race_unadjusted [[2, "p.value"]]),
  "], [], [",
  format (round (model_race_adjusted [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_race_adjusted [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_race_adjusted [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  print_p_value (model_race_adjusted [[2, "p.value"]]),  
  "],\n\t",
  "[#h(1em)Asian], [",
  format (round (model_race_unadjusted [[3, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_race_unadjusted [[3, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_race_unadjusted [[3, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  print_p_value (model_race_unadjusted [[3, "p.value"]]),
  "], [], [",
  format (round (model_race_adjusted [[3, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_race_adjusted [[3, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_race_adjusted [[3, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  print_p_value (model_race_adjusted [[3, "p.value"]]),  
  "],\n\t",  
  "[#h(1em)Other], [",
  format (round (model_race_unadjusted [[4, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_race_unadjusted [[4, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_race_unadjusted [[4, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  print_p_value (model_race_unadjusted [[4, "p.value"]]),
  "], [], [",
  format (round (model_race_adjusted [[4, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_race_adjusted [[4, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_race_adjusted [[4, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  print_p_value (model_race_adjusted [[4, "p.value"]]),  
  "],\n\t",
  "[*Ethnicity*], [], [], [], [], [],\n\t",
  "[#h(1em)Hispanic], [1.00 (Reference)], [---], [], [1.00 (Reference)], [---],\n\t",
  "[#h(1em)Not Hispanic], [",
  format (round (model_ethnicity_unadjusted [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_ethnicity_unadjusted [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_ethnicity_unadjusted [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  print_p_value (model_ethnicity_unadjusted [[2, "p.value"]]),
  "], [], [",
  format (round (model_ethnicity_adjusted [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_ethnicity_adjusted [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_ethnicity_adjusted [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  print_p_value (model_ethnicity_adjusted [[2, "p.value"]]),  
  "],\n\t",
  "[*BMI, kg/m#super[2]*], [",
  format (round (model_bmi_unadjusted [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_bmi_unadjusted [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_bmi_unadjusted [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  print_p_value (model_bmi_unadjusted [[2, "p.value"]]),
  "], [], [",
  format (round (model_bmi_adjusted [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_bmi_adjusted [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_bmi_adjusted [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  print_p_value (model_bmi_adjusted [[2, "p.value"]]),
  "],\n\t",    
  "[*Pack Years, yr*], [",
  format (round (model_pack_years_unadjusted [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_pack_years_unadjusted [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_pack_years_unadjusted [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  print_p_value (model_pack_years_unadjusted [[2, "p.value"]]),
  "], [], [",
  format (round (model_pack_years_adjusted [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_pack_years_adjusted [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_pack_years_adjusted [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  print_p_value (model_pack_years_adjusted [[2, "p.value"]]),
  "],\n\t",  
  "[*Respiratory Symptoms*], [], [], [], [], [],\n\t",
  "[#h(1em)Cough], [",
  format (round (model_cough_unadjusted [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_cough_unadjusted [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_cough_unadjusted [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  print_p_value (model_cough_unadjusted [[2, "p.value"]]),
  "], [], [",
  format (round (model_cough_adjusted [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_cough_adjusted [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_cough_adjusted [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  print_p_value (model_cough_adjusted [[2, "p.value"]]),  
  "],\n\t",
  "[#h(1em)Dyspnea], [",
  format (round (model_dyspnea_unadjusted [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_dyspnea_unadjusted [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_dyspnea_unadjusted [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  print_p_value (model_dyspnea_unadjusted [[2, "p.value"]]),
  "], [], [",
  format (round (model_dyspnea_adjusted [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_dyspnea_adjusted [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_dyspnea_adjusted [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  print_p_value (model_dyspnea_adjusted [[2, "p.value"]]),  
  "],\n\t",
  "[#h(1em)Wheeze], [",
  format (round (model_wheeze_unadjusted [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_wheeze_unadjusted [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_wheeze_unadjusted [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  print_p_value (model_wheeze_unadjusted [[2, "p.value"]]),
  "], [], [",
  format (round (model_wheeze_adjusted [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_wheeze_adjusted [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_wheeze_adjusted [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  print_p_value (model_wheeze_adjusted [[2, "p.value"]]),  
  "],\n\t",   
  "[*Respiratory Diseases*], [], [], [], [], [],\n\t",
  "[#h(1em)Asthma], [",
  format (round (model_asthma_unadjusted [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_asthma_unadjusted [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_asthma_unadjusted [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  print_p_value (model_asthma_unadjusted [[2, "p.value"]]),
  "], [], [",
  format (round (model_asthma_adjusted [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_asthma_adjusted [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_asthma_adjusted [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  print_p_value (model_asthma_adjusted [[2, "p.value"]]),  
  "],\n\t",
  "[#h(1em)Bronchiectasis], [",
  format (round (model_bronchiectasis_unadjusted [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_bronchiectasis_unadjusted [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_bronchiectasis_unadjusted [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  print_p_value (model_bronchiectasis_unadjusted [[2, "p.value"]]),
  "], [], [",
  format (round (model_bronchiectasis_adjusted [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_bronchiectasis_adjusted [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_bronchiectasis_adjusted [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  print_p_value (model_bronchiectasis_adjusted [[2, "p.value"]]),  
  "],\n\t",
  "[#h(1em)Chest Wall Disorder], [",
  format (round (model_chest_wall_unadjusted [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_chest_wall_unadjusted [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_chest_wall_unadjusted [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  print_p_value (model_chest_wall_unadjusted [[2, "p.value"]]),
  "], [], [",
  format (round (model_chest_wall_adjusted [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_chest_wall_adjusted [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_chest_wall_adjusted [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  print_p_value (model_chest_wall_adjusted [[2, "p.value"]]),  
  "],\n\t",
  "[#h(1em)Chronic Obstructive Pulmonary Disease], [",
  format (round (model_copd_unadjusted [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_copd_unadjusted [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_copd_unadjusted [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  print_p_value (model_copd_unadjusted [[2, "p.value"]]),
  "], [], [",
  format (round (model_copd_adjusted [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_copd_adjusted [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_copd_adjusted [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  print_p_value (model_copd_adjusted [[2, "p.value"]]),  
  "],\n\t",
  "[#h(1em)Interstitial Lung Disease], [",
  format (round (model_ild_unadjusted [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_ild_unadjusted [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_ild_unadjusted [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  print_p_value (model_ild_unadjusted [[2, "p.value"]]),
  "], [], [",
  format (round (model_ild_adjusted [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_ild_adjusted [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_ild_adjusted [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  print_p_value (model_ild_adjusted [[2, "p.value"]]),  
  "],\n\t",
  "[#h(1em)Neuromuscular Disease], [",
  format (round (model_neuromuscular_unadjusted [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_neuromuscular_unadjusted [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_neuromuscular_unadjusted [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  print_p_value (model_neuromuscular_unadjusted [[2, "p.value"]]),
  "], [], [",
  format (round (model_neuromuscular_adjusted [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_neuromuscular_adjusted [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_neuromuscular_adjusted [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  print_p_value (model_neuromuscular_adjusted [[2, "p.value"]]),  
  "],\n\t",  
  "[#h(1em)None], [",
  format (round (model_none_unadjusted [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_none_unadjusted [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_none_unadjusted [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  print_p_value (model_none_unadjusted [[2, "p.value"]]),
  "], [], [",
  format (round (model_none_adjusted [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_none_adjusted [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_none_adjusted [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  print_p_value (model_none_adjusted [[2, "p.value"]]),  
  "],\n\t",    
  "[*Computed Tomography*], [], [], [], [], [],\n\t",
  "[#h(1em)Chronic Obstructive Pulmonary Disease], [",
  format (round (model_copd_imaging_unadjusted [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_copd_imaging_unadjusted [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_copd_imaging_unadjusted [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  print_p_value (model_copd_imaging_unadjusted [[2, "p.value"]]),
  "], [], [",
  format (round (model_copd_imaging_adjusted [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_copd_imaging_adjusted [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_copd_imaging_adjusted [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  print_p_value (model_copd_imaging_adjusted [[2, "p.value"]]),  
  "],\n\t",
  "[#h(1em)Interstitial Lung Disease], [",
  format (round (model_ild_imaging_unadjusted [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_ild_imaging_unadjusted [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_ild_imaging_unadjusted [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  print_p_value (model_ild_imaging_unadjusted [[2, "p.value"]]),
  "], [], [",
  format (round (model_ild_imaging_adjusted [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_ild_imaging_adjusted [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_ild_imaging_adjusted [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  print_p_value (model_ild_imaging_adjusted [[2, "p.value"]]),  
  "],\n\t",     
  "[*Dynamic Lung Volumes*], [], [], [], [], [],\n\t",
  "[#h(1em)FEV#sub[1], z-score], [",
  format (round (model_fev1_unadjusted [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_fev1_unadjusted [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_fev1_unadjusted [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  print_p_value (model_fev1_unadjusted [[2, "p.value"]]),
  "], [], [",
  format (round (model_fev1_adjusted [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_fev1_adjusted [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_fev1_adjusted [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  print_p_value (model_fev1_adjusted [[2, "p.value"]]),  
  "],\n\t",
  "[#h(1em)FVC, z-score], [",
  format (round (model_fvc_unadjusted [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_fvc_unadjusted [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_fvc_unadjusted [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  print_p_value (model_fvc_unadjusted [[2, "p.value"]]),
  "], [], [",
  format (round (model_fvc_adjusted [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_fvc_adjusted [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_fvc_adjusted [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  print_p_value (model_fvc_adjusted [[2, "p.value"]]),  
  "],\n\t",
  "[#h(1em)FEV#sub[1]/FVC, z-score], [",
  format (round (model_fev1_fvc_unadjusted [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_fev1_fvc_unadjusted [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_fev1_fvc_unadjusted [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  print_p_value (model_fev1_fvc_unadjusted [[2, "p.value"]]),
  "], [], [",
  format (round (model_fev1_fvc_adjusted [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_fev1_fvc_adjusted [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_fev1_fvc_adjusted [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  print_p_value (model_fev1_fvc_adjusted [[2, "p.value"]]),  
  "],\n\t",
  "[#h(1em)Bronchodilator Response], [",
  format (round (model_response_unadjusted [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_response_unadjusted [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_response_unadjusted [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  print_p_value (model_response_unadjusted [[2, "p.value"]]),
  "], [], [",
  format (round (model_response_adjusted [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_response_adjusted [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_response_adjusted [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  print_p_value (model_response_adjusted [[2, "p.value"]]),  
  "],\n\t",
  "[#h(1em)Adequate Effort], [",
  format (round (model_effort_unadjusted [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_effort_unadjusted [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_effort_unadjusted [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  print_p_value (model_effort_unadjusted [[2, "p.value"]]),
  "], [], [",
  format (round (model_effort_adjusted [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_effort_adjusted [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_effort_adjusted [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  print_p_value (model_effort_adjusted [[2, "p.value"]]),  
  "],\n\t",     
  "[*Static Lung Volumes*], [], [], [], [], [],\n\t",
  "[#h(1em)RV, z-score], [",
  format (round (model_rv_unadjusted [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_rv_unadjusted [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_rv_unadjusted [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  print_p_value (model_rv_unadjusted [[2, "p.value"]]),
  "], [], [",
  format (round (model_rv_adjusted [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_rv_adjusted [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_rv_adjusted [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  print_p_value (model_rv_adjusted [[2, "p.value"]]),  
  "],\n\t",      
  "[*Diffusing Capacity*], [], [], [], [], [],\n\t",
  "[#h(1em)D#sub[LCO], z-score], [",
  format (round (model_dlco_unadjusted [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_dlco_unadjusted [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_dlco_unadjusted [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  print_p_value (model_dlco_unadjusted [[2, "p.value"]]),
  "], [], [",
  format (round (model_dlco_adjusted [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_dlco_adjusted [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_dlco_adjusted [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  print_p_value (model_dlco_adjusted [[2, "p.value"]]),  
  "],\n\t",
  "[#h(1em)K#sub[CO], z-score], [",
  format (round (model_kco_unadjusted [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_kco_unadjusted [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_kco_unadjusted [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  print_p_value (model_kco_unadjusted [[2, "p.value"]]),
  "], [], [",
  format (round (model_kco_adjusted [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_kco_adjusted [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_kco_adjusted [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  print_p_value (model_kco_adjusted [[2, "p.value"]]),  
  "],\n\t",
  "table.hline()\n",
  ")", 
  sep = ""
)  

write_lines (table, "../tables/e-table-24.txt")

################################################################################
## e-Table 25 - Race Specific Reference Equations and Characteristics
################################################################################

data <- pfts %>%
  filter (fvc_z_score_2012 >= -1.645 & fev1_fvc_z_score_2012 >= -1.645)

# Unadjusted

model_age_unadjusted <- tidy (glm (
  formula = restriction ~ age,
  family = "binomial",
  data = data), conf.int = TRUE, exponentiate = TRUE
)

model_sex_unadjusted <- tidy (glm (
  formula = restriction ~ sex,
  family = "binomial",
  data = data), conf.int = TRUE, exponentiate = TRUE
)

model_bmi_unadjusted <- tidy (glm (
  formula = restriction ~ bmi,
  family = "binomial",
  data = data), conf.int = TRUE, exponentiate = TRUE
)

model_race_unadjusted <- tidy (glm (
  formula = restriction ~ race,
  family = "binomial",
  data = data), conf.int = TRUE, exponentiate = TRUE
)

model_ethnicity_unadjusted <- tidy (glm (
  formula = restriction ~ ethnicity,
  family = "binomial",
  data = data), conf.int = TRUE, exponentiate = TRUE
)

model_pack_years_unadjusted <- tidy (glm (
  formula = restriction ~ pack_years,
  family = "binomial",
  data = data), conf.int = TRUE, exponentiate = TRUE
)

model_cough_unadjusted <- tidy (glm (
  formula = restriction ~ cough,
  family = "binomial",
  data = data), conf.int = TRUE, exponentiate = TRUE
)

model_dyspnea_unadjusted <- tidy (glm (
  formula = restriction ~ dyspnea,
  family = "binomial",
  data = data), conf.int = TRUE, exponentiate = TRUE
)

model_wheeze_unadjusted <- tidy (glm (
  formula = restriction ~ wheeze,
  family = "binomial",
  data = data), conf.int = TRUE, exponentiate = TRUE
)

model_asthma_unadjusted <- tidy (glm (
  formula = restriction ~ asthma,
  family = "binomial",
  data = data), conf.int = TRUE, exponentiate = TRUE
)

model_bronchiectasis_unadjusted <- tidy (glm (
  formula = restriction ~ bronchiectasis,
  family = "binomial",
  data = data), conf.int = TRUE, exponentiate = TRUE
)

model_chest_wall_unadjusted <- tidy (glm (
  formula = restriction ~ chest_wall,
  family = "binomial",
  data = data), conf.int = TRUE, exponentiate = TRUE
)

model_copd_unadjusted <- tidy (glm (
  formula = restriction ~ copd,
  family = "binomial",
  data = data), conf.int = TRUE, exponentiate = TRUE
)

model_ild_unadjusted <- tidy (glm (
  formula = restriction ~ ild,
  family = "binomial",
  data = data), conf.int = TRUE, exponentiate = TRUE
)

model_neuromuscular_unadjusted <- tidy (glm (
  formula = restriction ~ neuromuscular,
  family = "binomial",
  data = data), conf.int = TRUE, exponentiate = TRUE
)

model_none_unadjusted <- tidy (glm (
  formula = restriction ~ none,
  family = "binomial",
  data = data), conf.int = TRUE, exponentiate = TRUE
)

model_copd_imaging_unadjusted <- tidy (glm (
  formula = restriction ~ copd_imaging,
  family = "binomial",
  data = data), conf.int = TRUE, exponentiate = TRUE
)

model_ild_imaging_unadjusted <- tidy (glm (
  formula = restriction ~ ild_imaging,
  family = "binomial",
  data = data), conf.int = TRUE, exponentiate = TRUE
)

model_fev1_unadjusted <- tidy (glm (
  formula = restriction ~ fev1_z_score_2012,
  family = "binomial",
  data = data), conf.int = TRUE, exponentiate = TRUE
)

model_fvc_unadjusted <- tidy (glm (
  formula = restriction ~ fvc_z_score_2012,
  family = "binomial",
  data = data), conf.int = TRUE, exponentiate = TRUE
)

model_fev1_fvc_unadjusted <- tidy (glm (
  formula = restriction ~ fev1_fvc_z_score_2012,
  family = "binomial",
  data = data), conf.int = TRUE, exponentiate = TRUE
)

model_response_unadjusted <- tidy (glm (
  formula = restriction ~ response_2012,
  family = "binomial",
  data = data), conf.int = TRUE, exponentiate = TRUE
)

model_effort_unadjusted <- tidy (glm (
  formula = restriction ~ effort,
  family = "binomial",
  data = data), conf.int = TRUE, exponentiate = TRUE
)

model_rv_unadjusted <- tidy (glm (
  formula = restriction ~ rv_z_score,
  family = "binomial",
  data = data), conf.int = TRUE, exponentiate = TRUE
)

model_dlco_unadjusted <- tidy (glm (
  formula = restriction ~ dlco_z_score,
  family = "binomial",
  data = data), conf.int = TRUE, exponentiate = TRUE
)

model_kco_unadjusted <- tidy (glm (
  formula = restriction ~ kco_z_score,
  family = "binomial",
  data = data), conf.int = TRUE, exponentiate = TRUE
)

# Adjusted

model_age_adjusted <- tidy (glm (
  formula = restriction ~ age + sex + fev1_z_score_2012 + fvc_z_score_2012 +
    fev1_fvc_z_score_2012,
  family = "binomial",
  data = data), conf.int = TRUE, exponentiate = TRUE
)

model_sex_adjusted <- tidy (glm (
  formula = restriction ~ sex + age + fev1_z_score_2012 + fvc_z_score_2012 +
    fev1_fvc_z_score_2012,
  family = "binomial",
  data = data), conf.int = TRUE, exponentiate = TRUE
)

model_bmi_adjusted <- tidy (glm (
  formula = restriction ~ bmi + age + sex + fev1_z_score_2012 + fvc_z_score_2012 +
    fev1_fvc_z_score_2012,
  family = "binomial",
  data = data), conf.int = TRUE, exponentiate = TRUE
)

model_race_adjusted <- tidy (glm (
  formula = restriction ~ race + age + sex + fev1_z_score_2012 + fvc_z_score_2012 +
    fev1_fvc_z_score_2012,
  family = "binomial",
  data = data), conf.int = TRUE, exponentiate = TRUE
)

model_ethnicity_adjusted <- tidy (glm (
  formula = restriction ~ ethnicity + age + sex + fev1_z_score_2012 + fvc_z_score_2012 +
    fev1_fvc_z_score_2012,
  family = "binomial",
  data = data), conf.int = TRUE, exponentiate = TRUE
)

model_pack_years_adjusted <- tidy (glm (
  formula = restriction ~ pack_years + age + sex + fev1_z_score_2012 + fvc_z_score_2012 +
    fev1_fvc_z_score_2012,
  family = "binomial",
  data = data), conf.int = TRUE, exponentiate = TRUE
)

model_cough_adjusted <- tidy (glm (
  formula = restriction ~ cough + age + sex + fev1_z_score_2012 + fvc_z_score_2012 +
    fev1_fvc_z_score_2012,
  family = "binomial",
  data = data), conf.int = TRUE, exponentiate = TRUE
)

model_dyspnea_adjusted <- tidy (glm (
  formula = restriction ~ dyspnea + age + sex + fev1_z_score_2012 + fvc_z_score_2012 +
    fev1_fvc_z_score_2012,
  family = "binomial",
  data = data), conf.int = TRUE, exponentiate = TRUE
)

model_wheeze_adjusted <- tidy (glm (
  formula = restriction ~ wheeze + age + sex + fev1_z_score_2012 + fvc_z_score_2012 +
    fev1_fvc_z_score_2012,
  family = "binomial",
  data = data), conf.int = TRUE, exponentiate = TRUE
)

model_asthma_adjusted <- tidy (glm (
  formula = restriction ~ asthma + age + sex + fev1_z_score_2012 + fvc_z_score_2012 +
    fev1_fvc_z_score_2012,
  family = "binomial",
  data = data), conf.int = TRUE, exponentiate = TRUE
)

model_bronchiectasis_adjusted <- tidy (glm (
  formula = restriction ~ bronchiectasis + age + sex + fev1_z_score_2012 + fvc_z_score_2012 +
    fev1_fvc_z_score_2012,
  family = "binomial",
  data = data), conf.int = TRUE, exponentiate = TRUE
)

model_chest_wall_adjusted <- tidy (glm (
  formula = restriction ~ chest_wall + age + sex + fev1_z_score_2012 + fvc_z_score_2012 +
    fev1_fvc_z_score_2012,
  family = "binomial",
  data = data), conf.int = TRUE, exponentiate = TRUE
)

model_copd_adjusted <- tidy (glm (
  formula = restriction ~ copd + age + sex + fev1_z_score_2012 + fvc_z_score_2012 +
    fev1_fvc_z_score_2012,
  family = "binomial",
  data = data), conf.int = TRUE, exponentiate = TRUE
)

model_ild_adjusted <- tidy (glm (
  formula = restriction ~ ild + age + sex + fev1_z_score_2012 + fvc_z_score_2012 +
    fev1_fvc_z_score_2012,
  family = "binomial",
  data = data), conf.int = TRUE, exponentiate = TRUE
)

model_neuromuscular_adjusted <- tidy (glm (
  formula = restriction ~ neuromuscular + age + sex + fev1_z_score_2012 + fvc_z_score_2012 +
    fev1_fvc_z_score_2012,
  family = "binomial",
  data = data), conf.int = TRUE, exponentiate = TRUE
)

model_none_adjusted <- tidy (glm (
  formula = restriction ~ none + age + sex + fev1_z_score_2012 + fvc_z_score_2012 +
    fev1_fvc_z_score_2012,
  family = "binomial",
  data = data), conf.int = TRUE, exponentiate = TRUE
)

model_copd_imaging_adjusted <- tidy (glm (
  formula = restriction ~ copd_imaging + age + sex  + fev1_z_score_2012 + fvc_z_score_2012 +
    fev1_fvc_z_score_2012,
  family = "binomial",
  data = data), conf.int = TRUE, exponentiate = TRUE
)

model_ild_imaging_adjusted <- tidy (glm (
  formula = restriction ~ ild_imaging + age + sex + fev1_z_score_2012 + fvc_z_score_2012 +
    fev1_fvc_z_score_2012,
  family = "binomial",
  data = data), conf.int = TRUE, exponentiate = TRUE
)

model_fev1_adjusted <- tidy (glm (
  formula = restriction ~ fev1_z_score_2012 + age + sex + fvc_z_score_2012 +
    fev1_fvc_z_score_2012,
  family = "binomial",
  data = data), conf.int = TRUE, exponentiate = TRUE
)

model_fvc_adjusted <- tidy (glm (
  formula = restriction ~ fvc_z_score_2012 + age + sex + fev1_z_score_2012 +
    fev1_fvc_z_score_2012,
  family = "binomial",
  data = data), conf.int = TRUE, exponentiate = TRUE
)

model_fev1_fvc_adjusted <- tidy (glm (
  formula = restriction ~ fev1_fvc_z_score_2012 + age + sex + fev1_z_score_2012 +
    fvc_z_score_2012,
  family = "binomial",
  data = data), conf.int = TRUE, exponentiate = TRUE
)

model_response_adjusted <- tidy (glm (
  formula = restriction ~ response_2012 + age + sex + fev1_z_score_2012 + fvc_z_score_2012 +
    fev1_fvc_z_score_2012,
  family = "binomial",
  data = data), conf.int = TRUE, exponentiate = TRUE
)

model_effort_adjusted <- tidy (glm (
  formula = restriction ~ effort + age + sex + fev1_z_score_2012 + fvc_z_score_2012 +
    fev1_fvc_z_score_2012,
  family = "binomial",
  data = data), conf.int = TRUE, exponentiate = TRUE
)

model_rv_adjusted <- tidy (glm (
  formula = restriction ~ rv_z_score + age + sex + fev1_z_score_2012 + fvc_z_score_2012 +
    fev1_fvc_z_score_2012,
  family = "binomial",
  data = data), conf.int = TRUE, exponentiate = TRUE
)

model_dlco_adjusted <- tidy (glm (
  formula = restriction ~ dlco_z_score + age + sex + fev1_z_score_2012 + fvc_z_score_2012 +
    fev1_fvc_z_score_2012,
  family = "binomial",
  data = data), conf.int = TRUE, exponentiate = TRUE
)

model_kco_adjusted <- tidy (glm (
  formula = restriction ~ kco_z_score + age + sex + fev1_z_score_2012 + fvc_z_score_2012 +
    fev1_fvc_z_score_2012,
  family = "binomial",
  data = data), conf.int = TRUE, exponentiate = TRUE
)

table <- paste (
  "#table(\n\t",
  "columns: 6,\n\t",
  "inset: (x: 4pt, y: 3pt),\n\t",
	"align: (left, center, right, center, center, right),\n\t",
	"table.hline(),\n\t",
  "[], table.cell(colspan: 5, [*Association with Restriction*]),\n\t",
  "table.hline(start:1, end:6),\n\t",
  "[], table.cell(colspan: 2, [*Unadjusted*]), [], table.cell(colspan: 2,[*Adjusted#super[a]*]),\n\t",
  "table.hline(start:1, end:3), table.hline(start:4, end:6),\n\t",
	"[*Characteristic*], [*Odds Ratio (95% CI)*], [*$P$ Value*], [], [*Odds Ratio (95% CI)*], [*$P$ Value*],\n\t",
	"table.hline(),\n\t",  
  "[*Age, yr*], [",
  format (round (model_age_unadjusted [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_age_unadjusted [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_age_unadjusted [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  print_p_value (model_age_unadjusted [[2, "p.value"]]),
  "], [], [",
  format (round (model_age_adjusted [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_age_adjusted [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_age_adjusted [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  print_p_value (model_age_adjusted [[2, "p.value"]]),
  "],\n\t",
  "[*Sex*], [], [], [], [], [],\n\t",
  "[#h(1em)Male], [1.00 (Reference)], [---], [], [1.00 (Reference)], [---],\n\t",
  "[#h(1em)Female], [",
  format (round (model_sex_unadjusted [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_sex_unadjusted [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_sex_unadjusted [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  print_p_value (model_sex_unadjusted [[2, "p.value"]]),
  "], [], [",
  format (round (model_sex_adjusted [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_sex_adjusted [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_sex_adjusted [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  print_p_value (model_sex_adjusted [[2, "p.value"]]),  
  "],\n\t",  
  "[*Race*], [], [], [], [], [],\n\t",
  "[#h(1em)White], [1.00 (Reference)], [---], [], [1.00 (Reference)], [---],\n\t",
  "[#h(1em)Black], [",
  format (round (model_race_unadjusted [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_race_unadjusted [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_race_unadjusted [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  print_p_value (model_race_unadjusted [[2, "p.value"]]),
  "], [], [",
  format (round (model_race_adjusted [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_race_adjusted [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_race_adjusted [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  print_p_value (model_race_adjusted [[2, "p.value"]]),  
  "],\n\t",
  "[#h(1em)Asian], [",
  format (round (model_race_unadjusted [[3, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_race_unadjusted [[3, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_race_unadjusted [[3, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  print_p_value (model_race_unadjusted [[3, "p.value"]]),
  "], [], [",
  format (round (model_race_adjusted [[3, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_race_adjusted [[3, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_race_adjusted [[3, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  print_p_value (model_race_adjusted [[3, "p.value"]]),  
  "],\n\t",  
  "[#h(1em)Other], [",
  format (round (model_race_unadjusted [[4, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_race_unadjusted [[4, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_race_unadjusted [[4, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  print_p_value (model_race_unadjusted [[4, "p.value"]]),
  "], [], [",
  format (round (model_race_adjusted [[4, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_race_adjusted [[4, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_race_adjusted [[4, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  print_p_value (model_race_adjusted [[4, "p.value"]]),  
  "],\n\t",
  "[*Ethnicity*], [], [], [], [], [],\n\t",
  "[#h(1em)Hispanic], [1.00 (Reference)], [---], [], [1.00 (Reference)], [---],\n\t",
  "[#h(1em)Not Hispanic], [",
  format (round (model_ethnicity_unadjusted [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_ethnicity_unadjusted [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_ethnicity_unadjusted [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  print_p_value (model_ethnicity_unadjusted [[2, "p.value"]]),
  "], [], [",
  format (round (model_ethnicity_adjusted [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_ethnicity_adjusted [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_ethnicity_adjusted [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  print_p_value (model_ethnicity_adjusted [[2, "p.value"]]),  
  "],\n\t",
  "[*BMI, kg/m#super[2]*], [",
  format (round (model_bmi_unadjusted [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_bmi_unadjusted [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_bmi_unadjusted [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  print_p_value (model_bmi_unadjusted [[2, "p.value"]]),
  "], [], [",
  format (round (model_bmi_adjusted [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_bmi_adjusted [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_bmi_adjusted [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  print_p_value (model_bmi_adjusted [[2, "p.value"]]),
  "],\n\t",    
  "[*Pack Years, yr*], [",
  format (round (model_bmi_unadjusted [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_bmi_unadjusted [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_bmi_unadjusted [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  print_p_value (model_bmi_unadjusted [[2, "p.value"]]),
  "], [], [",
  format (round (model_bmi_adjusted [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_bmi_adjusted [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_bmi_adjusted [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  print_p_value (model_bmi_adjusted [[2, "p.value"]]),
  "],\n\t",  
  "[*Respiratory Symptoms*], [], [], [], [], [],\n\t",
  "[#h(1em)Cough], [",
  format (round (model_cough_unadjusted [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_cough_unadjusted [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_cough_unadjusted [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  print_p_value (model_cough_unadjusted [[2, "p.value"]]),
  "], [], [",
  format (round (model_cough_adjusted [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_cough_adjusted [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_cough_adjusted [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  print_p_value (model_cough_adjusted [[2, "p.value"]]),  
  "],\n\t",
  "[#h(1em)Dyspnea], [",
  format (round (model_dyspnea_unadjusted [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_dyspnea_unadjusted [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_dyspnea_unadjusted [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  print_p_value (model_dyspnea_unadjusted [[2, "p.value"]]),
  "], [], [",
  format (round (model_dyspnea_adjusted [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_dyspnea_adjusted [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_dyspnea_adjusted [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  print_p_value (model_dyspnea_adjusted [[2, "p.value"]]),  
  "],\n\t",
  "[#h(1em)Wheeze], [",
  format (round (model_wheeze_unadjusted [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_wheeze_unadjusted [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_wheeze_unadjusted [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  print_p_value (model_wheeze_unadjusted [[2, "p.value"]]),
  "], [], [",
  format (round (model_wheeze_adjusted [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_wheeze_adjusted [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_wheeze_adjusted [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  print_p_value (model_wheeze_adjusted [[2, "p.value"]]),  
  "],\n\t",   
  "[*Respiratory Diseases*], [], [], [], [], [],\n\t",
  "[#h(1em)Asthma], [",
  format (round (model_asthma_unadjusted [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_asthma_unadjusted [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_asthma_unadjusted [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  print_p_value (model_asthma_unadjusted [[2, "p.value"]]),
  "], [], [",
  format (round (model_asthma_adjusted [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_asthma_adjusted [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_asthma_adjusted [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  print_p_value (model_asthma_adjusted [[2, "p.value"]]),  
  "],\n\t",
  "[#h(1em)Bronchiectasis], [",
  format (round (model_bronchiectasis_unadjusted [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_bronchiectasis_unadjusted [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_bronchiectasis_unadjusted [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  print_p_value (model_bronchiectasis_unadjusted [[2, "p.value"]]),
  "], [], [",
  format (round (model_bronchiectasis_adjusted [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_bronchiectasis_adjusted [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_bronchiectasis_adjusted [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  print_p_value (model_bronchiectasis_adjusted [[2, "p.value"]]),  
  "],\n\t",
  "[#h(1em)Chest Wall Disorder], [",
  format (round (model_chest_wall_unadjusted [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_chest_wall_unadjusted [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_chest_wall_unadjusted [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  print_p_value (model_chest_wall_unadjusted [[2, "p.value"]]),
  "], [], [",
  format (round (model_chest_wall_adjusted [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_chest_wall_adjusted [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_chest_wall_adjusted [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  print_p_value (model_chest_wall_adjusted [[2, "p.value"]]),  
  "],\n\t",
  "[#h(1em)Chronic Obstructive Pulmonary Disease], [",
  format (round (model_copd_unadjusted [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_copd_unadjusted [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_copd_unadjusted [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  print_p_value (model_copd_unadjusted [[2, "p.value"]]),
  "], [], [",
  format (round (model_copd_adjusted [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_copd_adjusted [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_copd_adjusted [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  print_p_value (model_copd_adjusted [[2, "p.value"]]),  
  "],\n\t",
  "[#h(1em)Interstitial Lung Disease], [",
  format (round (model_ild_unadjusted [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_ild_unadjusted [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_ild_unadjusted [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  print_p_value (model_ild_unadjusted [[2, "p.value"]]),
  "], [], [",
  format (round (model_ild_adjusted [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_ild_adjusted [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_ild_adjusted [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  print_p_value (model_ild_adjusted [[2, "p.value"]]),  
  "],\n\t",
  "[#h(1em)Neuromuscular Disease], [",
  format (round (model_neuromuscular_unadjusted [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_neuromuscular_unadjusted [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_neuromuscular_unadjusted [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  print_p_value (model_neuromuscular_unadjusted [[2, "p.value"]]),
  "], [], [",
  format (round (model_neuromuscular_adjusted [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_neuromuscular_adjusted [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_neuromuscular_adjusted [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  print_p_value (model_neuromuscular_adjusted [[2, "p.value"]]),  
  "],\n\t",
  "[#h(1em)None], [",
  format (round (model_none_unadjusted [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_none_unadjusted [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_none_unadjusted [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  print_p_value (model_none_unadjusted [[2, "p.value"]]),
  "], [], [",
  format (round (model_none_adjusted [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_none_adjusted [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_none_adjusted [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  print_p_value (model_none_adjusted [[2, "p.value"]]),  
  "],\n\t",   
  "[*Computed Tomography Findings*], [], [], [], [], [],\n\t",
  "[#h(1em)Chronic Obstructive Pulmonary Disease], [",
  format (round (model_copd_imaging_unadjusted [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_copd_imaging_unadjusted [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_copd_imaging_unadjusted [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  print_p_value (model_copd_imaging_unadjusted [[2, "p.value"]]),
  "], [], [",
  format (round (model_copd_imaging_adjusted [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_copd_imaging_adjusted [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_copd_imaging_adjusted [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  print_p_value (model_copd_imaging_adjusted [[2, "p.value"]]),  
  "],\n\t",
  "[#h(1em)Interstitial Lung Disease], [",
  format (round (model_ild_imaging_unadjusted [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_ild_imaging_unadjusted [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_ild_imaging_unadjusted [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  print_p_value (model_ild_imaging_unadjusted [[2, "p.value"]]),
  "], [], [",
  format (round (model_ild_imaging_adjusted [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_ild_imaging_adjusted [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_ild_imaging_adjusted [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  print_p_value (model_ild_imaging_adjusted [[2, "p.value"]]),  
  "],\n\t",     
  "[*Dynamic Lung Volumes*], [], [], [], [], [],\n\t",
  "[#h(1em)FEV#sub[1], z-score], [",
  format (round (model_fev1_unadjusted [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_fev1_unadjusted [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_fev1_unadjusted [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  print_p_value (model_fev1_unadjusted [[2, "p.value"]]),
  "], [], [",
  format (round (model_fev1_adjusted [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_fev1_adjusted [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_fev1_adjusted [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  print_p_value (model_fev1_adjusted [[2, "p.value"]]),  
  "],\n\t",
  "[#h(1em)FVC, z-score], [",
  format (round (model_fvc_unadjusted [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_fvc_unadjusted [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_fvc_unadjusted [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  print_p_value (model_fvc_unadjusted [[2, "p.value"]]),
  "], [], [",
  format (round (model_fvc_adjusted [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_fvc_adjusted [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_fvc_adjusted [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  print_p_value (model_fvc_adjusted [[2, "p.value"]]),  
  "],\n\t",
  "[#h(1em)FEV#sub[1]/FVC, z-score], [",
  format (round (model_fev1_fvc_unadjusted [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_fev1_fvc_unadjusted [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_fev1_fvc_unadjusted [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  print_p_value (model_fev1_fvc_unadjusted [[2, "p.value"]]),
  "], [], [",
  format (round (model_fev1_fvc_adjusted [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_fev1_fvc_adjusted [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_fev1_fvc_adjusted [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  print_p_value (model_fev1_fvc_adjusted [[2, "p.value"]]),  
  "],\n\t",
  "[#h(1em)Bronchodilator Response], [",
  format (round (model_response_unadjusted [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_response_unadjusted [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_response_unadjusted [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  print_p_value (model_response_unadjusted [[2, "p.value"]]),
  "], [], [",
  format (round (model_response_adjusted [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_response_adjusted [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_response_adjusted [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  print_p_value (model_response_adjusted [[2, "p.value"]]),  
  "],\n\t",
  "[#h(1em)Adequate Effort], [",
  format (round (model_effort_unadjusted [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_effort_unadjusted [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_effort_unadjusted [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  print_p_value (model_effort_unadjusted [[2, "p.value"]]),
  "], [], [",
  format (round (model_effort_adjusted [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_effort_adjusted [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_effort_adjusted [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  print_p_value (model_effort_adjusted [[2, "p.value"]]),  
  "],\n\t",     
  "[*Static Lung Volumes*], [], [], [], [], [],\n\t",
  "[#h(1em)RV, z-score], [",
  format (round (model_rv_unadjusted [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_rv_unadjusted [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_rv_unadjusted [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  print_p_value (model_rv_unadjusted [[2, "p.value"]]),
  "], [], [",
  format (round (model_rv_adjusted [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_rv_adjusted [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_rv_adjusted [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  print_p_value (model_rv_adjusted [[2, "p.value"]]),  
  "],\n\t",      
  "[*Diffusing Capacity*], [], [], [], [], [],\n\t",
  "[#h(1em)D#sub[LCO], z-score], [",
  format (round (model_dlco_unadjusted [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_dlco_unadjusted [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_dlco_unadjusted [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  print_p_value (model_dlco_unadjusted [[2, "p.value"]]),
  "], [], [",
  format (round (model_dlco_adjusted [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_dlco_adjusted [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_dlco_adjusted [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  print_p_value (model_dlco_adjusted [[2, "p.value"]]),  
  "],\n\t",
  "[#h(1em)K#sub[CO], z-score], [",
  format (round (model_kco_unadjusted [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_kco_unadjusted [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_kco_unadjusted [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  print_p_value (model_kco_unadjusted [[2, "p.value"]]),
  "], [], [",
  format (round (model_kco_adjusted [[2, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_kco_adjusted [[2, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_kco_adjusted [[2, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  print_p_value (model_kco_adjusted [[2, "p.value"]]),  
  "],\n\t",
  "table.hline()\n",
  ")", 
  sep = ""
)  

write_lines (table, "../tables/e-table-25.txt")

################################################################################
## e-Table 26 - Race Specific Reference Equations and Events Associated
## with Restriction with Normal Spirometry
################################################################################

# ED visit with respiratory symptoms

data <- pfts %>% 
  filter (fev1_fvc_z_score_2012 >= -1.645 & fvc_z_score_2012 >= -1.645) %>% 
  mutate (event = case_when (
    is.na (date_ed) == 0 ~ 1,
    dead == 1 ~ 2,
    TRUE ~ 0)
  ) %>%
  mutate (time = case_when (
    event == 1 ~ interval (date, date_ed) %/% months (1),
    TRUE ~ interval (date, date_last) %/% months (1))
  ) %>% 
  select (
    age,
    sex,
    fev1_z_score_2012,
    fvc_z_score_2012,
    fev1_fvc_z_score_2012,
    restriction,
    event,
    time
  ) %>% 
  drop_na () %>% 
  mutate (time = pmax (time, 0))

model_ed_unadjusted <- tidy (
  CSC (
    Hist (time, event) ~ restriction,
    data = data
  )$models[[1]],
  conf.int = TRUE,
  exponentiate = TRUE
)

model_ed_adjusted <- tidy (
  CSC (
    Hist (time, event) ~ restriction + age + sex + fev1_z_score_2012 + 
      fvc_z_score_2012 + fev1_fvc_z_score_2012,
    data = data
  )$models[[1]],
  conf.int = TRUE,
  exponentiate = TRUE
)

# Death

data <- pfts %>%
  filter (fev1_fvc_z_score_2012 >= -1.645 & fvc_z_score_2012 >= -1.645) %>%
  mutate (time = interval (date, date_last) %/% months (1)) %>%
  filter (time > 0) %>%
  select (
    age,
    sex,
    race,
    fev1_z_score_2012,
    fvc_z_score_2012,
    fev1_fvc_z_score_2012,
    restriction,
    event = dead,
    time
  ) %>% 
  drop_na ()

model_death_unadjusted <- tidy (
  coxph (Surv (time, event) ~ restriction, data = data, x = TRUE),
  conf.int = TRUE,
  exponentiate = TRUE
)

model_death_adjusted <- tidy (
  coxph (Surv (time, event) ~ restriction + age + sex + fev1_z_score_2012 +
    fvc_z_score_2012 + fev1_fvc_z_score_2012, data = data, x = TRUE),
  conf.int = TRUE,
  exponentiate = TRUE
)

table <- paste (
  "#table(\n\t",
	"inset: (x: 4pt, y: 4pt),\n\t",
  "columns: 6,\n\t",
	"align: (left, center, right, center, center, right),\n\t",
	"table.hline(),\n\t",
  "[], table.cell(colspan: 5, [*Association with Restriction*]),\n\t",
  "table.hline(start:1, end:6),\n\t",
	"[], table.cell(colspan: 2, [*Unadjusted*]), [], table.cell(colspan: 2,[*Adjusted#super[a]*]),\n\t",
	"table.hline(start:1, end:3), table.hline(start:4, end:6),\n\t",
	"[*Event*], [*Hazard Ratio (95% CI)*], [*$P$ Value*], [], [*Hazard Ratio (95% CI)*], [*$P$ Value*],\n\t",
	"table.hline(),\n\t",
  "[Emergency Department Visit with], table.cell(rowspan: 2, align: horizon)[",
  format (round (model_ed_unadjusted [[1, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_ed_unadjusted [[1, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_ed_unadjusted [[1, "conf.high"]], digits = 2), nsmall = 2),
  ")], table.cell(rowspan: 2, align: horizon)[",
  print_p_value (model_ed_unadjusted [[1, "p.value"]]),
  "], [], table.cell(rowspan: 2, align: horizon)[",
  format (round (model_ed_adjusted [[1, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_ed_adjusted [[1, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_ed_adjusted [[1, "conf.high"]], digits = 2), nsmall = 2),
  ")], table.cell(rowspan: 2, align: horizon)[",
  print_p_value (model_ed_adjusted [[1, "p.value"]]),
  "],\n\t",
  "[#h(1em)Respiratory Complaint], [],\n\t",
  "[Death from Any Cause], [",
  format (round (model_death_unadjusted [[1, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_death_unadjusted [[1, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_death_unadjusted [[1, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  print_p_value (model_death_unadjusted [[1, "p.value"]]),
  "], [], [",
  format (round (model_death_adjusted [[1, "estimate"]], digits = 2), nsmall = 2),
  " (",
  format (round (model_death_adjusted [[1, "conf.low"]], digits = 2), nsmall = 2),
  "--",
  format (round (model_death_adjusted [[1, "conf.high"]], digits = 2), nsmall = 2),
  ")], [",
  print_p_value (model_death_adjusted [[1, "p.value"]]),
  "],\n\t",
  "table.hline()\n",
  ")", 
  sep = ""
)  

write_lines (table, "../tables/e-table-26.txt")

