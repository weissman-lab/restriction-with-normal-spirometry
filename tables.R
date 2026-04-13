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
    interpretation == "Mixed" ~ 6,
    TRUE ~ 7)
  ) %>%
  mutate (spirometry = factor (spirometry, levels = c ("1", "2", "3", "4", "5", "6", "7"))) %>% 
  mutate (thickening = as.factor (thickening)) %>% 
  mutate (emphysema = as.factor (emphysema)) %>% 
  mutate (honeycombing = as.factor (honeycombing)) %>% 
  mutate (reticulation = as.factor (reticulation)) %>% 
  mutate (traction = as.factor (traction))

table <- table1 (~
  age +
  height +
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
  thickening +
  emphysema +
  honeycombing +
  reticulation +
  traction +
  fev1_z_score +
  fvc_z_score +
  fev1_fvc_z_score +
  tlc_z_score +
  rv_z_score +
  dlco_z_score |
  spirometry,
  data = data,
  big.mark =",",
  render.categorical = render_categorical,
  render.continuous = render_continuous
)

table <- as.data.frame (table)

table <- paste (
  "#table(\n\t",
  "inset: (x: 4pt, y: 2.5pt),\n\t",
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
  comma (parse_number (table [[1,9]])),
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
  table [[which (table [,1] == "age"), 9]],  
  "],\n\t",
  "[*Height, cm*],",
  "[",
  table [[which (table [,1] == "height"), 2]],
  "], [",
  table [[which (table [,1] == "height"), 3]],
  "], [",
  table [[which (table [,1] == "height"), 4]],
  "], [",
  table [[which (table [,1] == "height"), 5]],
  "], [",
  table [[which (table [,1] == "height"), 6]],
  "], [",
  table [[which (table [,1] == "height"), 7]],
  "], [",
  table [[which (table [,1] == "height"), 9]],  
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
  table [[which (table [,1] == "sex") + 1, 9]],  
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
  table [[which (table [,1] == "sex") + 2,9]],  
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
  table [[which (table [,1] == "race") + 1, 9]],  
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
  table [[which (table [,1] == "race") + 2, 9]],  
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
  table [[which (table [,1] == "race") + 3, 9]],  
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
  table [[which (table [,1] == "race") + 4, 9]],  
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
  table [[which (table [,1] == "ethnicity") + 1, 9]],  
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
  table [[which (table [,1] == "ethnicity") + 2, 9]],  
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
  table [[which (table [,1] == "bmi"), 9]],  
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
  table [[which (table [,1] == "pack_years"), 9]],  
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
  table [[which (table [,1] == "cough") + 2, 9]], 
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
  table [[which (table [,1] == "dyspnea") + 2, 9]], 
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
  table [[which (table [,1] == "wheeze") + 2, 9]], 
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
  table [[which (table [,1] == "asthma") + 2, 9]], 
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
  table [[which (table [,1] == "bronchiectasis") + 2, 9]], 
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
  table [[which (table [,1] == "chest_wall") + 2, 9]], 
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
  table [[which (table [,1] == "copd") + 2, 9]], 
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
  table [[which (table [,1] == "ild") + 2, 9]], 
  "],\n\t",
  "[#h(1em)Neuromuscular Disorder],",
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
  table [[which (table [,1] == "neuromuscular") + 2, 9]], 
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
  table [[which (table [,1] == "none") + 2, 9]], 
  "],\n\t",
  "[*Computed Tomography*], [], [], [], [], [], [], [],\n\t",
  "[#h(1em)Bronchial Wall Thickening],",
  "[",
  table [[which (table [,1] == "thickening") + 2, 2]],
  "], [",
  table [[which (table [,1] == "thickening") + 2, 3]],
  "], [",
  table [[which (table [,1] == "thickening") + 2, 4]],
  "], [",
  table [[which (table [,1] == "thickening") + 2, 5]],
  "], [",
  table [[which (table [,1] == "thickening") + 2, 6]],
  "], [",
  table [[which (table [,1] == "thickening") + 2, 7]],
  "], [",
  table [[which (table [,1] == "thickening") + 2, 9]], 
  "],\n\t",
  "[#h(1em)Emphysema],",
  "[",
  table [[which (table [,1] == "emphysema") + 2, 2]],
  "], [",
  table [[which (table [,1] == "emphysema") + 2, 3]],
  "], [",
  table [[which (table [,1] == "emphysema") + 2, 4]],
  "], [",
  table [[which (table [,1] == "emphysema") + 2, 5]],
  "], [",
  table [[which (table [,1] == "emphysema") + 2, 6]],
  "], [",
  table [[which (table [,1] == "emphysema") + 2, 7]],
  "], [",
  table [[which (table [,1] == "emphysema") + 2, 9]], 
  "],\n\t",
  "[#h(1em)Honeycombing],",
  "[",
  table [[which (table [,1] == "honeycombing") + 2, 2]],
  "], [",
  table [[which (table [,1] == "honeycombing") + 2, 3]],
  "], [",
  table [[which (table [,1] == "honeycombing") + 2, 4]],
  "], [",
  table [[which (table [,1] == "honeycombing") + 2, 5]],
  "], [",
  table [[which (table [,1] == "honeycombing") + 2, 6]],
  "], [",
  table [[which (table [,1] == "honeycombing") + 2, 7]],
  "], [",
  table [[which (table [,1] == "honeycombing") + 2, 9]], 
  "],\n\t",
  "[#h(1em)Reticulation],",
  "[",
  table [[which (table [,1] == "reticulation") + 2, 2]],
  "], [",
  table [[which (table [,1] == "reticulation") + 2, 3]],
  "], [",
  table [[which (table [,1] == "reticulation") + 2, 4]],
  "], [",
  table [[which (table [,1] == "reticulation") + 2, 5]],
  "], [",
  table [[which (table [,1] == "reticulation") + 2, 6]],
  "], [",
  table [[which (table [,1] == "reticulation") + 2, 7]],
  "], [",
  table [[which (table [,1] == "reticulation") + 2, 9]], 
  "],\n\t",
  "[#h(1em)Traction Bronchiectasis],",
  "[",
  table [[which (table [,1] == "traction") + 2, 2]],
  "], [",
  table [[which (table [,1] == "traction") + 2, 3]],
  "], [",
  table [[which (table [,1] == "traction") + 2, 4]],
  "], [",
  table [[which (table [,1] == "traction") + 2, 5]],
  "], [",
  table [[which (table [,1] == "traction") + 2, 6]],
  "], [",
  table [[which (table [,1] == "traction") + 2, 7]],
  "], [",
  table [[which (table [,1] == "traction") + 2, 9]], 
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
  table [[which (table [,1] == "fev1_z_score"), 9]], 
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
  table [[which (table [,1] == "fvc_z_score"), 9]], 
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
  table [[which (table [,1] == "fev1_fvc_z_score"), 9]], 
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
  table [[which (table [,1] == "tlc_z_score"), 9]], 
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
  table [[which (table [,1] == "rv_z_score"), 9]], 
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
  table [[which (table [,1] == "dlco_z_score"), 9]], 
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


# # Unadjusted
# 
# model_age_unadjusted <- tidy (glm (
#   formula = restriction ~ age,
#   family = "binomial",
#   data = data), conf.int = TRUE, exponentiate = TRUE
# )
# 
# model_sex_unadjusted <- tidy (glm (
#   formula = restriction ~ sex,
#   family = "binomial",
#   data = data), conf.int = TRUE, exponentiate = TRUE
# )
# 
# model_bmi_unadjusted <- tidy (glm (
#   formula = restriction ~ bmi,
#   family = "binomial",
#   data = data), conf.int = TRUE, exponentiate = TRUE
# )
# 
# model_race_unadjusted <- tidy (glm (
#   formula = restriction ~ race,
#   family = "binomial",
#   data = data), conf.int = TRUE, exponentiate = TRUE
# )
# 
# model_ethnicity_unadjusted <- tidy (glm (
#   formula = restriction ~ ethnicity,
#   family = "binomial",
#   data = data), conf.int = TRUE, exponentiate = TRUE
# )
# 
# model_pack_years_unadjusted <- tidy (glm (
#   formula = restriction ~ pack_years,
#   family = "binomial",
#   data = data), conf.int = TRUE, exponentiate = TRUE
# )
# 
# model_cough_unadjusted <- tidy (glm (
#   formula = restriction ~ cough,
#   family = "binomial",
#   data = data), conf.int = TRUE, exponentiate = TRUE
# )
# 
# model_dyspnea_unadjusted <- tidy (glm (
#   formula = restriction ~ dyspnea,
#   family = "binomial",
#   data = data), conf.int = TRUE, exponentiate = TRUE
# )
# 
# model_wheeze_unadjusted <- tidy (glm (
#   formula = restriction ~ wheeze,
#   family = "binomial",
#   data = data), conf.int = TRUE, exponentiate = TRUE
# )
# 
# model_asthma_unadjusted <- tidy (glm (
#   formula = restriction ~ asthma,
#   family = "binomial",
#   data = data), conf.int = TRUE, exponentiate = TRUE
# )
# 
# model_bronchiectasis_unadjusted <- tidy (glm (
#   formula = restriction ~ bronchiectasis,
#   family = "binomial",
#   data = data), conf.int = TRUE, exponentiate = TRUE
# )
# 
# model_chest_wall_unadjusted <- tidy (glm (
#   formula = restriction ~ chest_wall,
#   family = "binomial",
#   data = data), conf.int = TRUE, exponentiate = TRUE
# )
# 
# model_copd_unadjusted <- tidy (glm (
#   formula = restriction ~ copd,
#   family = "binomial",
#   data = data), conf.int = TRUE, exponentiate = TRUE
# )
# 
# model_ild_unadjusted <- tidy (glm (
#   formula = restriction ~ ild,
#   family = "binomial",
#   data = data), conf.int = TRUE, exponentiate = TRUE
# )
# 
# model_neuromuscular_unadjusted <- tidy (glm (
#   formula = restriction ~ neuromuscular,
#   family = "binomial",
#   data = data), conf.int = TRUE, exponentiate = TRUE
# )
# 
# model_none_unadjusted <- tidy (glm (
#   formula = restriction ~ none,
#   family = "binomial",
#   data = data), conf.int = TRUE, exponentiate = TRUE
# )
# 
# model_copd_imaging_unadjusted <- tidy (glm (
#   formula = restriction ~ copd_imaging,
#   family = "binomial",
#   data = data), conf.int = TRUE, exponentiate = TRUE
# )
# 
# model_ild_imaging_unadjusted <- tidy (glm (
#   formula = restriction ~ ild_imaging,
#   family = "binomial",
#   data = data), conf.int = TRUE, exponentiate = TRUE
# )
# 
# model_fev1_unadjusted <- tidy (glm (
#   formula = restriction ~ fev1_z_score,
#   family = "binomial",
#   data = data), conf.int = TRUE, exponentiate = TRUE
# )
# 
# model_fvc_unadjusted <- tidy (glm (
#   formula = restriction ~ fvc_z_score,
#   family = "binomial",
#   data = data), conf.int = TRUE, exponentiate = TRUE
# )
# 
# model_fev1_fvc_unadjusted <- tidy (glm (
#   formula = restriction ~ fev1_fvc_z_score,
#   family = "binomial",
#   data = data), conf.int = TRUE, exponentiate = TRUE
# )
# 
# model_response_unadjusted <- tidy (glm (
#   formula = restriction ~ response,
#   family = "binomial",
#   data = data), conf.int = TRUE, exponentiate = TRUE
# )
# 
# model_effort_unadjusted <- tidy (glm (
#   formula = restriction ~ effort,
#   family = "binomial",
#   data = data), conf.int = TRUE, exponentiate = TRUE
# )
# 
# model_rv_unadjusted <- tidy (glm (
#   formula = restriction ~ rv_z_score,
#   family = "binomial",
#   data = data), conf.int = TRUE, exponentiate = TRUE
# )
# 
# model_dlco_unadjusted <- tidy (glm (
#   formula = restriction ~ dlco_z_score,
#   family = "binomial",
#   data = data), conf.int = TRUE, exponentiate = TRUE
# )
# 
# model_kco_unadjusted <- tidy (glm (
#   formula = restriction ~ kco_z_score,
#   family = "binomial",
#   data = data), conf.int = TRUE, exponentiate = TRUE
# )
# 
# # Adjusted
# 
# model_age_adjusted <- tidy (glm (
#   formula = restriction ~ age + sex + fev1_z_score + fvc_z_score +
#     fev1_fvc_z_score,
#   family = "binomial",
#   data = data), conf.int = TRUE, exponentiate = TRUE
# )
# 
# model_sex_adjusted <- tidy (glm (
#   formula = restriction ~ sex + age + fev1_z_score + fvc_z_score +
#     fev1_fvc_z_score,
#   family = "binomial",
#   data = data), conf.int = TRUE, exponentiate = TRUE
# )
# 
# model_bmi_adjusted <- tidy (glm (
#   formula = restriction ~ bmi + age + sex + fev1_z_score + fvc_z_score +
#     fev1_fvc_z_score,
#   family = "binomial",
#   data = data), conf.int = TRUE, exponentiate = TRUE
# )
# 
# model_race_adjusted <- tidy (glm (
#   formula = restriction ~ race + age + sex + fev1_z_score + fvc_z_score +
#     fev1_fvc_z_score,
#   family = "binomial",
#   data = data), conf.int = TRUE, exponentiate = TRUE
# )
# 
# model_ethnicity_adjusted <- tidy (glm (
#   formula = restriction ~ ethnicity + age + sex + fev1_z_score + fvc_z_score +
#     fev1_fvc_z_score,
#   family = "binomial",
#   data = data), conf.int = TRUE, exponentiate = TRUE
# )
# 
# model_pack_years_adjusted <- tidy (glm (
#   formula = restriction ~ pack_years + age + sex + fev1_z_score + fvc_z_score +
#     fev1_fvc_z_score,
#   family = "binomial",
#   data = data), conf.int = TRUE, exponentiate = TRUE
# )
# 
# model_cough_adjusted <- tidy (glm (
#   formula = restriction ~ cough + age + sex + fev1_z_score + fvc_z_score +
#     fev1_fvc_z_score,
#   family = "binomial",
#   data = data), conf.int = TRUE, exponentiate = TRUE
# )
# 
# model_dyspnea_adjusted <- tidy (glm (
#   formula = restriction ~ dyspnea + age + sex + fev1_z_score + fvc_z_score +
#     fev1_fvc_z_score,
#   family = "binomial",
#   data = data), conf.int = TRUE, exponentiate = TRUE
# )
# 
# model_wheeze_adjusted <- tidy (glm (
#   formula = restriction ~ wheeze + age + sex + fev1_z_score + fvc_z_score +
#     fev1_fvc_z_score,
#   family = "binomial",
#   data = data), conf.int = TRUE, exponentiate = TRUE
# )
# 
# model_asthma_adjusted <- tidy (glm (
#   formula = restriction ~ asthma + age + sex + fev1_z_score + fvc_z_score +
#     fev1_fvc_z_score,
#   family = "binomial",
#   data = data), conf.int = TRUE, exponentiate = TRUE
# )
# 
# model_bronchiectasis_adjusted <- tidy (glm (
#   formula = restriction ~ bronchiectasis + age + sex + fev1_z_score + fvc_z_score +
#     fev1_fvc_z_score,
#   family = "binomial",
#   data = data), conf.int = TRUE, exponentiate = TRUE
# )
# 
# model_chest_wall_adjusted <- tidy (glm (
#   formula = restriction ~ chest_wall + age + sex + fev1_z_score + fvc_z_score +
#     fev1_fvc_z_score,
#   family = "binomial",
#   data = data), conf.int = TRUE, exponentiate = TRUE
# )
# 
# model_copd_adjusted <- tidy (glm (
#   formula = restriction ~ copd + age + sex + fev1_z_score + fvc_z_score +
#     fev1_fvc_z_score,
#   family = "binomial",
#   data = data), conf.int = TRUE, exponentiate = TRUE
# )
# 
# model_ild_adjusted <- tidy (glm (
#   formula = restriction ~ ild + age + sex + fev1_z_score + fvc_z_score +
#     fev1_fvc_z_score,
#   family = "binomial",
#   data = data), conf.int = TRUE, exponentiate = TRUE
# )
# 
# model_neuromuscular_adjusted <- tidy (glm (
#   formula = restriction ~ neuromuscular + age + sex + fev1_z_score + fvc_z_score +
#     fev1_fvc_z_score,
#   family = "binomial",
#   data = data), conf.int = TRUE, exponentiate = TRUE
# )
# 
# model_none_adjusted <- tidy (glm (
#   formula = restriction ~ none + age + sex + fev1_z_score + fvc_z_score +
#     fev1_fvc_z_score,
#   family = "binomial",
#   data = data), conf.int = TRUE, exponentiate = TRUE
# )
# 
# model_copd_imaging_adjusted <- tidy (glm (
#   formula = restriction ~ copd_imaging + age + sex  + fev1_z_score + fvc_z_score +
#     fev1_fvc_z_score,
#   family = "binomial",
#   data = data), conf.int = TRUE, exponentiate = TRUE
# )
# 
# model_ild_imaging_adjusted <- tidy (glm (
#   formula = restriction ~ ild_imaging + age + sex + fev1_z_score + fvc_z_score +
#     fev1_fvc_z_score,
#   family = "binomial",
#   data = data), conf.int = TRUE, exponentiate = TRUE
# )
# 
# model_fev1_adjusted <- tidy (glm (
#   formula = restriction ~ fev1_z_score + age + sex + fvc_z_score +
#     fev1_fvc_z_score,
#   family = "binomial",
#   data = data), conf.int = TRUE, exponentiate = TRUE
# )
# 
# model_fvc_adjusted <- tidy (glm (
#   formula = restriction ~ fvc_z_score + age + sex + fev1_z_score +
#     fev1_fvc_z_score,
#   family = "binomial",
#   data = data), conf.int = TRUE, exponentiate = TRUE
# )
# 
# model_fev1_fvc_adjusted <- tidy (glm (
#   formula = restriction ~ fev1_fvc_z_score + age + sex + fev1_z_score +
#     fvc_z_score,
#   family = "binomial",
#   data = data), conf.int = TRUE, exponentiate = TRUE
# )
# 
# model_response_adjusted <- tidy (glm (
#   formula = restriction ~ response + age + sex + fev1_z_score + fvc_z_score +
#     fev1_fvc_z_score,
#   family = "binomial",
#   data = data), conf.int = TRUE, exponentiate = TRUE
# )
# 
# model_effort_adjusted <- tidy (glm (
#   formula = restriction ~ effort + age + sex + fev1_z_score + fvc_z_score +
#     fev1_fvc_z_score,
#   family = "binomial",
#   data = data), conf.int = TRUE, exponentiate = TRUE
# )
# 
# model_rv_adjusted <- tidy (glm (
#   formula = restriction ~ rv_z_score + age + sex + fev1_z_score + fvc_z_score +
#     fev1_fvc_z_score,
#   family = "binomial",
#   data = data), conf.int = TRUE, exponentiate = TRUE
# )
# 
# model_dlco_adjusted <- tidy (glm (
#   formula = restriction ~ dlco_z_score + age + sex + fev1_z_score + fvc_z_score +
#     fev1_fvc_z_score,
#   family = "binomial",
#   data = data), conf.int = TRUE, exponentiate = TRUE
# )
# 
# model_kco_adjusted <- tidy (glm (
#   formula = restriction ~ kco_z_score + age + sex + fev1_z_score + fvc_z_score +
#     fev1_fvc_z_score,
#   family = "binomial",
#   data = data), conf.int = TRUE, exponentiate = TRUE
# )
# 
# table <- paste (
#   "#table(\n\t",
# 	"inset: (x: 4pt, y: 2.5pt),\n\t",
#   "columns: 6,\n\t",
# 	"align: (left, center, right, center, center, right),\n\t",
# 	"table.hline(),\n\t",
#   "[], table.cell(colspan: 5, [*Association with Restriction*]),\n\t",
#   "table.hline(start:1, end:6),\n\t",
# 	"[], table.cell(colspan: 2, [*Unadjusted*]), [], table.cell(colspan: 2,[*Adjusted#super[a]*]),\n\t",
# 	"table.hline(start:1, end:3), table.hline(start:4, end:6),\n\t",
# 	"[*Characteristic*], [*Odds Ratio (95% CI)*], [*$P$ Value*], [], [*Odds Ratio (95% CI)*], [*$P$ Value*],\n\t",
# 	"table.hline(),\n\t",
#   "[*Age, yr*], [",
#   format (round (model_age_unadjusted [[2, "estimate"]], digits = 2), nsmall = 2),
#   " (",
#   format (round (model_age_unadjusted [[2, "conf.low"]], digits = 2), nsmall = 2),
#   "--",
#   format (round (model_age_unadjusted [[2, "conf.high"]], digits = 2), nsmall = 2),
#   ")], [",
#   print_p_value (model_age_unadjusted [[2, "p.value"]]),
#   "], [], [",
#   format (round (model_age_adjusted [[2, "estimate"]], digits = 2), nsmall = 2),
#   " (",
#   format (round (model_age_adjusted [[2, "conf.low"]], digits = 2), nsmall = 2),
#   "--",
#   format (round (model_age_adjusted [[2, "conf.high"]], digits = 2), nsmall = 2),
#   ")], [",
#   print_p_value (model_age_adjusted [[2, "p.value"]]),
#   "],\n\t",
#   "[*Sex*], [], [], [], [], [],\n\t",
#   "[#h(1em)Male], [1.00 (Reference)], [---], [], [1.00 (Reference)], [---],\n\t",
#   "[#h(1em)Female], [",
#   format (round (model_sex_unadjusted [[2, "estimate"]], digits = 2), nsmall = 2),
#   " (",
#   format (round (model_sex_unadjusted [[2, "conf.low"]], digits = 2), nsmall = 2),
#   "--",
#   format (round (model_sex_unadjusted [[2, "conf.high"]], digits = 2), nsmall = 2),
#   ")], [",
#   print_p_value (model_sex_unadjusted [[2, "p.value"]]),
#   "], [], [",
#   format (round (model_sex_adjusted [[2, "estimate"]], digits = 2), nsmall = 2),
#   " (",
#   format (round (model_sex_adjusted [[2, "conf.low"]], digits = 2), nsmall = 2),
#   "--",
#   format (round (model_sex_adjusted [[2, "conf.high"]], digits = 2), nsmall = 2),
#   ")], [",
#   print_p_value (model_sex_adjusted [[2, "p.value"]]),  
#   "],\n\t",  
#   "[*Race*], [], [], [], [], [],\n\t",
#   "[#h(1em)White], [1.00 (Reference)], [---], [], [1.00 (Reference)], [---],\n\t",
#   "[#h(1em)Black], [",
#   format (round (model_race_unadjusted [[2, "estimate"]], digits = 2), nsmall = 2),
#   " (",
#   format (round (model_race_unadjusted [[2, "conf.low"]], digits = 2), nsmall = 2),
#   "--",
#   format (round (model_race_unadjusted [[2, "conf.high"]], digits = 2), nsmall = 2),
#   ")], [",
#   print_p_value (model_race_unadjusted [[2, "p.value"]]),
#   "], [], [",
#   format (round (model_race_adjusted [[2, "estimate"]], digits = 2), nsmall = 2),
#   " (",
#   format (round (model_race_adjusted [[2, "conf.low"]], digits = 2), nsmall = 2),
#   "--",
#   format (round (model_race_adjusted [[2, "conf.high"]], digits = 2), nsmall = 2),
#   ")], [",
#   print_p_value (model_race_adjusted [[2, "p.value"]]),  
#   "],\n\t",
#   "[#h(1em)Asian], [",
#   format (round (model_race_unadjusted [[3, "estimate"]], digits = 2), nsmall = 2),
#   " (",
#   format (round (model_race_unadjusted [[3, "conf.low"]], digits = 2), nsmall = 2),
#   "--",
#   format (round (model_race_unadjusted [[3, "conf.high"]], digits = 2), nsmall = 2),
#   ")], [",
#   print_p_value (model_race_unadjusted [[3, "p.value"]]),
#   "], [], [",
#   format (round (model_race_adjusted [[3, "estimate"]], digits = 2), nsmall = 2),
#   " (",
#   format (round (model_race_adjusted [[3, "conf.low"]], digits = 2), nsmall = 2),
#   "--",
#   format (round (model_race_adjusted [[3, "conf.high"]], digits = 2), nsmall = 2),
#   ")], [",
#   print_p_value (model_race_adjusted [[3, "p.value"]]),  
#   "],\n\t",  
#   "[#h(1em)Other], [",
#   format (round (model_race_unadjusted [[4, "estimate"]], digits = 2), nsmall = 2),
#   " (",
#   format (round (model_race_unadjusted [[4, "conf.low"]], digits = 2), nsmall = 2),
#   "--",
#   format (round (model_race_unadjusted [[4, "conf.high"]], digits = 2), nsmall = 2),
#   ")], [",
#   print_p_value (model_race_unadjusted [[4, "p.value"]]),
#   "], [], [",
#   format (round (model_race_adjusted [[4, "estimate"]], digits = 2), nsmall = 2),
#   " (",
#   format (round (model_race_adjusted [[4, "conf.low"]], digits = 2), nsmall = 2),
#   "--",
#   format (round (model_race_adjusted [[4, "conf.high"]], digits = 2), nsmall = 2),
#   ")], [",
#   print_p_value (model_race_adjusted [[4, "p.value"]]),  
#   "],\n\t",
#   "[*Ethnicity*], [], [], [], [], [],\n\t",
#   "[#h(1em)Hispanic], [1.00 (Reference)], [---], [], [1.00 (Reference)], [---],\n\t",
#   "[#h(1em)Not Hispanic], [",
#   format (round (model_ethnicity_unadjusted [[2, "estimate"]], digits = 2), nsmall = 2),
#   " (",
#   format (round (model_ethnicity_unadjusted [[2, "conf.low"]], digits = 2), nsmall = 2),
#   "--",
#   format (round (model_ethnicity_unadjusted [[2, "conf.high"]], digits = 2), nsmall = 2),
#   ")], [",
#   print_p_value (model_ethnicity_unadjusted [[2, "p.value"]]),
#   "], [], [",
#   format (round (model_ethnicity_adjusted [[2, "estimate"]], digits = 2), nsmall = 2),
#   " (",
#   format (round (model_ethnicity_adjusted [[2, "conf.low"]], digits = 2), nsmall = 2),
#   "--",
#   format (round (model_ethnicity_adjusted [[2, "conf.high"]], digits = 2), nsmall = 2),
#   ")], [",
#   print_p_value (model_ethnicity_adjusted [[2, "p.value"]]),  
#   "],\n\t",
#   "[*BMI, kg/m#super[2]*], [",
#   format (round (model_bmi_unadjusted [[2, "estimate"]], digits = 2), nsmall = 2),
#   " (",
#   format (round (model_bmi_unadjusted [[2, "conf.low"]], digits = 2), nsmall = 2),
#   "--",
#   format (round (model_bmi_unadjusted [[2, "conf.high"]], digits = 2), nsmall = 2),
#   ")], [",
#   print_p_value (model_bmi_unadjusted [[2, "p.value"]]),
#   "], [], [",
#   format (round (model_bmi_adjusted [[2, "estimate"]], digits = 2), nsmall = 2),
#   " (",
#   format (round (model_bmi_adjusted [[2, "conf.low"]], digits = 2), nsmall = 2),
#   "--",
#   format (round (model_bmi_adjusted [[2, "conf.high"]], digits = 2), nsmall = 2),
#   ")], [",
#   print_p_value (model_bmi_adjusted [[2, "p.value"]]),
#   "],\n\t",    
#   "[*Pack Years, yr*], [",
#   format (round (model_pack_years_unadjusted [[2, "estimate"]], digits = 2), nsmall = 2),
#   " (",
#   format (round (model_pack_years_unadjusted [[2, "conf.low"]], digits = 2), nsmall = 2),
#   "--",
#   format (round (model_pack_years_unadjusted [[2, "conf.high"]], digits = 2), nsmall = 2),
#   ")], [",
#   print_p_value (model_pack_years_unadjusted [[2, "p.value"]]),
#   "], [], [",
#   format (round (model_pack_years_adjusted [[2, "estimate"]], digits = 2), nsmall = 2),
#   " (",
#   format (round (model_pack_years_adjusted [[2, "conf.low"]], digits = 2), nsmall = 2),
#   "--",
#   format (round (model_pack_years_adjusted [[2, "conf.high"]], digits = 2), nsmall = 2),
#   ")], [",
#   print_p_value (model_pack_years_adjusted [[2, "p.value"]]),
#   "],\n\t",  
#   "[*Respiratory Symptoms*], [], [], [], [], [],\n\t",
#   "[#h(1em)Cough], [",
#   format (round (model_cough_unadjusted [[2, "estimate"]], digits = 2), nsmall = 2),
#   " (",
#   format (round (model_cough_unadjusted [[2, "conf.low"]], digits = 2), nsmall = 2),
#   "--",
#   format (round (model_cough_unadjusted [[2, "conf.high"]], digits = 2), nsmall = 2),
#   ")], [",
#   print_p_value (model_cough_unadjusted [[2, "p.value"]]),
#   "], [], [",
#   format (round (model_cough_adjusted [[2, "estimate"]], digits = 2), nsmall = 2),
#   " (",
#   format (round (model_cough_adjusted [[2, "conf.low"]], digits = 2), nsmall = 2),
#   "--",
#   format (round (model_cough_adjusted [[2, "conf.high"]], digits = 2), nsmall = 2),
#   ")], [",
#   print_p_value (model_cough_adjusted [[2, "p.value"]]),  
#   "],\n\t",
#   "[#h(1em)Dyspnea], [",
#   format (round (model_dyspnea_unadjusted [[2, "estimate"]], digits = 2), nsmall = 2),
#   " (",
#   format (round (model_dyspnea_unadjusted [[2, "conf.low"]], digits = 2), nsmall = 2),
#   "--",
#   format (round (model_dyspnea_unadjusted [[2, "conf.high"]], digits = 2), nsmall = 2),
#   ")], [",
#   print_p_value (model_dyspnea_unadjusted [[2, "p.value"]]),
#   "], [], [",
#   format (round (model_dyspnea_adjusted [[2, "estimate"]], digits = 2), nsmall = 2),
#   " (",
#   format (round (model_dyspnea_adjusted [[2, "conf.low"]], digits = 2), nsmall = 2),
#   "--",
#   format (round (model_dyspnea_adjusted [[2, "conf.high"]], digits = 2), nsmall = 2),
#   ")], [",
#   print_p_value (model_dyspnea_adjusted [[2, "p.value"]]),  
#   "],\n\t",
#   "[#h(1em)Wheeze], [",
#   format (round (model_wheeze_unadjusted [[2, "estimate"]], digits = 2), nsmall = 2),
#   " (",
#   format (round (model_wheeze_unadjusted [[2, "conf.low"]], digits = 2), nsmall = 2),
#   "--",
#   format (round (model_wheeze_unadjusted [[2, "conf.high"]], digits = 2), nsmall = 2),
#   ")], [",
#   print_p_value (model_wheeze_unadjusted [[2, "p.value"]]),
#   "], [], [",
#   format (round (model_wheeze_adjusted [[2, "estimate"]], digits = 2), nsmall = 2),
#   " (",
#   format (round (model_wheeze_adjusted [[2, "conf.low"]], digits = 2), nsmall = 2),
#   "--",
#   format (round (model_wheeze_adjusted [[2, "conf.high"]], digits = 2), nsmall = 2),
#   ")], [",
#   print_p_value (model_wheeze_adjusted [[2, "p.value"]]),  
#   "],\n\t",   
#   "[*Respiratory Diseases*], [], [], [], [], [],\n\t",
#   "[#h(1em)Asthma], [",
#   format (round (model_asthma_unadjusted [[2, "estimate"]], digits = 2), nsmall = 2),
#   " (",
#   format (round (model_asthma_unadjusted [[2, "conf.low"]], digits = 2), nsmall = 2),
#   "--",
#   format (round (model_asthma_unadjusted [[2, "conf.high"]], digits = 2), nsmall = 2),
#   ")], [",
#   print_p_value (model_asthma_unadjusted [[2, "p.value"]]),
#   "], [], [",
#   format (round (model_asthma_adjusted [[2, "estimate"]], digits = 2), nsmall = 2),
#   " (",
#   format (round (model_asthma_adjusted [[2, "conf.low"]], digits = 2), nsmall = 2),
#   "--",
#   format (round (model_asthma_adjusted [[2, "conf.high"]], digits = 2), nsmall = 2),
#   ")], [",
#   print_p_value (model_asthma_adjusted [[2, "p.value"]]),  
#   "],\n\t",
#   "[#h(1em)Bronchiectasis], [",
#   format (round (model_bronchiectasis_unadjusted [[2, "estimate"]], digits = 2), nsmall = 2),
#   " (",
#   format (round (model_bronchiectasis_unadjusted [[2, "conf.low"]], digits = 2), nsmall = 2),
#   "--",
#   format (round (model_bronchiectasis_unadjusted [[2, "conf.high"]], digits = 2), nsmall = 2),
#   ")], [",
#   print_p_value (model_bronchiectasis_unadjusted [[2, "p.value"]]),
#   "], [], [",
#   format (round (model_bronchiectasis_adjusted [[2, "estimate"]], digits = 2), nsmall = 2),
#   " (",
#   format (round (model_bronchiectasis_adjusted [[2, "conf.low"]], digits = 2), nsmall = 2),
#   "--",
#   format (round (model_bronchiectasis_adjusted [[2, "conf.high"]], digits = 2), nsmall = 2),
#   ")], [",
#   print_p_value (model_bronchiectasis_adjusted [[2, "p.value"]]),  
#   "],\n\t",
#   "[#h(1em)Chest Wall Disorder], [",
#   format (round (model_chest_wall_unadjusted [[2, "estimate"]], digits = 2), nsmall = 2),
#   " (",
#   format (round (model_chest_wall_unadjusted [[2, "conf.low"]], digits = 2), nsmall = 2),
#   "--",
#   format (round (model_chest_wall_unadjusted [[2, "conf.high"]], digits = 2), nsmall = 2),
#   ")], [",
#   print_p_value (model_chest_wall_unadjusted [[2, "p.value"]]),
#   "], [], [",
#   format (round (model_chest_wall_adjusted [[2, "estimate"]], digits = 2), nsmall = 2),
#   " (",
#   format (round (model_chest_wall_adjusted [[2, "conf.low"]], digits = 2), nsmall = 2),
#   "--",
#   format (round (model_chest_wall_adjusted [[2, "conf.high"]], digits = 2), nsmall = 2),
#   ")], [",
#   print_p_value (model_chest_wall_adjusted [[2, "p.value"]]),  
#   "],\n\t",
#   "[#h(1em)Chronic Obstructive Pulmonary Disease], [",
#   format (round (model_copd_unadjusted [[2, "estimate"]], digits = 2), nsmall = 2),
#   " (",
#   format (round (model_copd_unadjusted [[2, "conf.low"]], digits = 2), nsmall = 2),
#   "--",
#   format (round (model_copd_unadjusted [[2, "conf.high"]], digits = 2), nsmall = 2),
#   ")], [",
#   print_p_value (model_copd_unadjusted [[2, "p.value"]]),
#   "], [], [",
#   format (round (model_copd_adjusted [[2, "estimate"]], digits = 2), nsmall = 2),
#   " (",
#   format (round (model_copd_adjusted [[2, "conf.low"]], digits = 2), nsmall = 2),
#   "--",
#   format (round (model_copd_adjusted [[2, "conf.high"]], digits = 2), nsmall = 2),
#   ")], [",
#   print_p_value (model_copd_adjusted [[2, "p.value"]]),  
#   "],\n\t",
#   "[#h(1em)Interstitial Lung Disease], [",
#   format (round (model_ild_unadjusted [[2, "estimate"]], digits = 2), nsmall = 2),
#   " (",
#   format (round (model_ild_unadjusted [[2, "conf.low"]], digits = 2), nsmall = 2),
#   "--",
#   format (round (model_ild_unadjusted [[2, "conf.high"]], digits = 2), nsmall = 2),
#   ")], [",
#   print_p_value (model_ild_unadjusted [[2, "p.value"]]),
#   "], [], [",
#   format (round (model_ild_adjusted [[2, "estimate"]], digits = 2), nsmall = 2),
#   " (",
#   format (round (model_ild_adjusted [[2, "conf.low"]], digits = 2), nsmall = 2),
#   "--",
#   format (round (model_ild_adjusted [[2, "conf.high"]], digits = 2), nsmall = 2),
#   ")], [",
#   print_p_value (model_ild_adjusted [[2, "p.value"]]),  
#   "],\n\t",
#   "[#h(1em)Neuromuscular Disorder], [",
#   format (round (model_neuromuscular_unadjusted [[2, "estimate"]], digits = 2), nsmall = 2),
#   " (",
#   format (round (model_neuromuscular_unadjusted [[2, "conf.low"]], digits = 2), nsmall = 2),
#   "--",
#   format (round (model_neuromuscular_unadjusted [[2, "conf.high"]], digits = 2), nsmall = 2),
#   ")], [",
#   print_p_value (model_neuromuscular_unadjusted [[2, "p.value"]]),
#   "], [], [",
#   format (round (model_neuromuscular_adjusted [[2, "estimate"]], digits = 2), nsmall = 2),
#   " (",
#   format (round (model_neuromuscular_adjusted [[2, "conf.low"]], digits = 2), nsmall = 2),
#   "--",
#   format (round (model_neuromuscular_adjusted [[2, "conf.high"]], digits = 2), nsmall = 2),
#   ")], [",
#   print_p_value (model_neuromuscular_adjusted [[2, "p.value"]]),  
#   "],\n\t",  
#   "[#h(1em)None], [",
#   format (round (model_none_unadjusted [[2, "estimate"]], digits = 2), nsmall = 2),
#   " (",
#   format (round (model_none_unadjusted [[2, "conf.low"]], digits = 2), nsmall = 2),
#   "--",
#   format (round (model_none_unadjusted [[2, "conf.high"]], digits = 2), nsmall = 2),
#   ")], [",
#   print_p_value (model_none_unadjusted [[2, "p.value"]]),
#   "], [], [",
#   format (round (model_none_adjusted [[2, "estimate"]], digits = 2), nsmall = 2),
#   " (",
#   format (round (model_none_adjusted [[2, "conf.low"]], digits = 2), nsmall = 2),
#   "--",
#   format (round (model_none_adjusted [[2, "conf.high"]], digits = 2), nsmall = 2),
#   ")], [",
#   print_p_value (model_none_adjusted [[2, "p.value"]]),  
#   "],\n\t",    
#   "[*Computed Tomography*], [], [], [], [], [],\n\t",
#   "[#h(1em)Chronic Obstructive Pulmonary Disease], [",
#   format (round (model_copd_imaging_unadjusted [[2, "estimate"]], digits = 2), nsmall = 2),
#   " (",
#   format (round (model_copd_imaging_unadjusted [[2, "conf.low"]], digits = 2), nsmall = 2),
#   "--",
#   format (round (model_copd_imaging_unadjusted [[2, "conf.high"]], digits = 2), nsmall = 2),
#   ")], [",
#   print_p_value (model_copd_imaging_unadjusted [[2, "p.value"]]),
#   "], [], [",
#   format (round (model_copd_imaging_adjusted [[2, "estimate"]], digits = 2), nsmall = 2),
#   " (",
#   format (round (model_copd_imaging_adjusted [[2, "conf.low"]], digits = 2), nsmall = 2),
#   "--",
#   format (round (model_copd_imaging_adjusted [[2, "conf.high"]], digits = 2), nsmall = 2),
#   ")], [",
#   print_p_value (model_copd_imaging_adjusted [[2, "p.value"]]),  
#   "],\n\t",
#   "[#h(1em)Interstitial Lung Disease], [",
#   format (round (model_ild_imaging_unadjusted [[2, "estimate"]], digits = 2), nsmall = 2),
#   " (",
#   format (round (model_ild_imaging_unadjusted [[2, "conf.low"]], digits = 2), nsmall = 2),
#   "--",
#   format (round (model_ild_imaging_unadjusted [[2, "conf.high"]], digits = 2), nsmall = 2),
#   ")], [",
#   print_p_value (model_ild_imaging_unadjusted [[2, "p.value"]]),
#   "], [], [",
#   format (round (model_ild_imaging_adjusted [[2, "estimate"]], digits = 2), nsmall = 2),
#   " (",
#   format (round (model_ild_imaging_adjusted [[2, "conf.low"]], digits = 2), nsmall = 2),
#   "--",
#   format (round (model_ild_imaging_adjusted [[2, "conf.high"]], digits = 2), nsmall = 2),
#   ")], [",
#   print_p_value (model_ild_imaging_adjusted [[2, "p.value"]]),  
#   "],\n\t",     
#   "[*Dynamic Lung Volumes*], [], [], [], [], [],\n\t",
#   "[#h(1em)FEV#sub[1], z-score], [",
#   format (round (model_fev1_unadjusted [[2, "estimate"]], digits = 2), nsmall = 2),
#   " (",
#   format (round (model_fev1_unadjusted [[2, "conf.low"]], digits = 2), nsmall = 2),
#   "--",
#   format (round (model_fev1_unadjusted [[2, "conf.high"]], digits = 2), nsmall = 2),
#   ")], [",
#   print_p_value (model_fev1_unadjusted [[2, "p.value"]]),
#   "], [], [",
#   format (round (model_fev1_adjusted [[2, "estimate"]], digits = 2), nsmall = 2),
#   " (",
#   format (round (model_fev1_adjusted [[2, "conf.low"]], digits = 2), nsmall = 2),
#   "--",
#   format (round (model_fev1_adjusted [[2, "conf.high"]], digits = 2), nsmall = 2),
#   ")], [",
#   print_p_value (model_fev1_adjusted [[2, "p.value"]]),  
#   "],\n\t",
#   "[#h(1em)FVC, z-score], [",
#   format (round (model_fvc_unadjusted [[2, "estimate"]], digits = 2), nsmall = 2),
#   " (",
#   format (round (model_fvc_unadjusted [[2, "conf.low"]], digits = 2), nsmall = 2),
#   "--",
#   format (round (model_fvc_unadjusted [[2, "conf.high"]], digits = 2), nsmall = 2),
#   ")], [",
#   print_p_value (model_fvc_unadjusted [[2, "p.value"]]),
#   "], [], [",
#   format (round (model_fvc_adjusted [[2, "estimate"]], digits = 2), nsmall = 2),
#   " (",
#   format (round (model_fvc_adjusted [[2, "conf.low"]], digits = 2), nsmall = 2),
#   "--",
#   format (round (model_fvc_adjusted [[2, "conf.high"]], digits = 2), nsmall = 2),
#   ")], [",
#   print_p_value (model_fvc_adjusted [[2, "p.value"]]),  
#   "],\n\t",
#   "[#h(1em)FEV#sub[1]/FVC, z-score], [",
#   format (round (model_fev1_fvc_unadjusted [[2, "estimate"]], digits = 2), nsmall = 2),
#   " (",
#   format (round (model_fev1_fvc_unadjusted [[2, "conf.low"]], digits = 2), nsmall = 2),
#   "--",
#   format (round (model_fev1_fvc_unadjusted [[2, "conf.high"]], digits = 2), nsmall = 2),
#   ")], [",
#   print_p_value (model_fev1_fvc_unadjusted [[2, "p.value"]]),
#   "], [], [",
#   format (round (model_fev1_fvc_adjusted [[2, "estimate"]], digits = 2), nsmall = 2),
#   " (",
#   format (round (model_fev1_fvc_adjusted [[2, "conf.low"]], digits = 2), nsmall = 2),
#   "--",
#   format (round (model_fev1_fvc_adjusted [[2, "conf.high"]], digits = 2), nsmall = 2),
#   ")], [",
#   print_p_value (model_fev1_fvc_adjusted [[2, "p.value"]]),  
#   "],\n\t",
#   "[#h(1em)Bronchodilator Response], [",
#   format (round (model_response_unadjusted [[2, "estimate"]], digits = 2), nsmall = 2),
#   " (",
#   format (round (model_response_unadjusted [[2, "conf.low"]], digits = 2), nsmall = 2),
#   "--",
#   format (round (model_response_unadjusted [[2, "conf.high"]], digits = 2), nsmall = 2),
#   ")], [",
#   print_p_value (model_response_unadjusted [[2, "p.value"]]),
#   "], [], [",
#   format (round (model_response_adjusted [[2, "estimate"]], digits = 2), nsmall = 2),
#   " (",
#   format (round (model_response_adjusted [[2, "conf.low"]], digits = 2), nsmall = 2),
#   "--",
#   format (round (model_response_adjusted [[2, "conf.high"]], digits = 2), nsmall = 2),
#   ")], [",
#   print_p_value (model_response_adjusted [[2, "p.value"]]),  
#   "],\n\t",
#   "[#h(1em)Adequate Effort], [",
#   format (round (model_effort_unadjusted [[2, "estimate"]], digits = 2), nsmall = 2),
#   " (",
#   format (round (model_effort_unadjusted [[2, "conf.low"]], digits = 2), nsmall = 2),
#   "--",
#   format (round (model_effort_unadjusted [[2, "conf.high"]], digits = 2), nsmall = 2),
#   ")], [",
#   print_p_value (model_effort_unadjusted [[2, "p.value"]]),
#   "], [], [",
#   format (round (model_effort_adjusted [[2, "estimate"]], digits = 2), nsmall = 2),
#   " (",
#   format (round (model_effort_adjusted [[2, "conf.low"]], digits = 2), nsmall = 2),
#   "--",
#   format (round (model_effort_adjusted [[2, "conf.high"]], digits = 2), nsmall = 2),
#   ")], [",
#   print_p_value (model_effort_adjusted [[2, "p.value"]]),  
#   "],\n\t",     
#   "[*Static Lung Volumes*], [], [], [], [], [],\n\t",
#   "[#h(1em)RV, z-score], [",
#   format (round (model_rv_unadjusted [[2, "estimate"]], digits = 2), nsmall = 2),
#   " (",
#   format (round (model_rv_unadjusted [[2, "conf.low"]], digits = 2), nsmall = 2),
#   "--",
#   format (round (model_rv_unadjusted [[2, "conf.high"]], digits = 2), nsmall = 2),
#   ")], [",
#   print_p_value (model_rv_unadjusted [[2, "p.value"]]),
#   "], [], [",
#   format (round (model_rv_adjusted [[2, "estimate"]], digits = 2), nsmall = 2),
#   " (",
#   format (round (model_rv_adjusted [[2, "conf.low"]], digits = 2), nsmall = 2),
#   "--",
#   format (round (model_rv_adjusted [[2, "conf.high"]], digits = 2), nsmall = 2),
#   ")], [",
#   print_p_value (model_rv_adjusted [[2, "p.value"]]),  
#   "],\n\t",      
#   "[*Diffusing Capacity*], [], [], [], [], [],\n\t",
#   "[#h(1em)D#sub[LCO], z-score], [",
#   format (round (model_dlco_unadjusted [[2, "estimate"]], digits = 2), nsmall = 2),
#   " (",
#   format (round (model_dlco_unadjusted [[2, "conf.low"]], digits = 2), nsmall = 2),
#   "--",
#   format (round (model_dlco_unadjusted [[2, "conf.high"]], digits = 2), nsmall = 2),
#   ")], [",
#   print_p_value (model_dlco_unadjusted [[2, "p.value"]]),
#   "], [], [",
#   format (round (model_dlco_adjusted [[2, "estimate"]], digits = 2), nsmall = 2),
#   " (",
#   format (round (model_dlco_adjusted [[2, "conf.low"]], digits = 2), nsmall = 2),
#   "--",
#   format (round (model_dlco_adjusted [[2, "conf.high"]], digits = 2), nsmall = 2),
#   ")], [",
#   print_p_value (model_dlco_adjusted [[2, "p.value"]]),  
#   "],\n\t",
#   "[#h(1em)K#sub[CO], z-score], [",
#   format (round (model_kco_unadjusted [[2, "estimate"]], digits = 2), nsmall = 2),
#   " (",
#   format (round (model_kco_unadjusted [[2, "conf.low"]], digits = 2), nsmall = 2),
#   "--",
#   format (round (model_kco_unadjusted [[2, "conf.high"]], digits = 2), nsmall = 2),
#   ")], [",
#   print_p_value (model_kco_unadjusted [[2, "p.value"]]),
#   "], [], [",
#   format (round (model_kco_adjusted [[2, "estimate"]], digits = 2), nsmall = 2),
#   " (",
#   format (round (model_kco_adjusted [[2, "conf.low"]], digits = 2), nsmall = 2),
#   "--",
#   format (round (model_kco_adjusted [[2, "conf.high"]], digits = 2), nsmall = 2),
#   ")], [",
#   print_p_value (model_kco_adjusted [[2, "p.value"]]),  
#   "],\n\t",
#   "table.hline()\n",
#   ")", 
#   sep = ""
# )  
# 
# write_lines (table, "../tables/table-2.txt")

################################################################################
## e-table 7 - Baseline Characteristics by Age
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
  mutate (emphysema = as.factor (emphysema)) %>% 
  mutate (honeycombing = as.factor (honeycombing)) %>% 
  mutate (reticulation = as.factor (reticulation)) %>% 
  mutate (thickening = as.factor (thickening)) %>% 
  mutate (traction = as.factor (traction))

table <- table1 (~
  age +
  height +
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
  thickening +
  emphysema +
  honeycombing +
  reticulation +
  traction +
  spirometry +
  fev1_z_score +
  fvc_z_score +
  fev1_fvc_z_score +
  tlc_z_score +
  rv_z_score +
  dlco_z_score |
  sex,
  data = data,
  big.mark =",",
  render.categorical = render_categorical,
  render.continuous = render_continuous
)

table <- as.data.frame (table)

table <- paste (
  "#table(\n\t",
  "inset: (x: 10pt, y: 2pt),\n\t",
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
  "[*Age, yrs*],",
  "[",
  table [[which (table [,1] == "age"), 2]],
  "], [",
  table [[which (table [,1] == "age"), 3]],
  "],\n\t",
  "[*Height, cm*],",
  "[",
  table [[which (table [,1] == "height"), 2]],
  "], [",
  table [[which (table [,1] == "height"), 3]],
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
  "[#h(1em)Neuromuscular Disorder], [",
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
  "[#h(1em)Bronchial Wall Thickening], [",
  table [[which (table [,1] == "thickening") + 2, 2]],
  "], [",
  table [[which (table [,1] == "thickening") + 2, 3]],
  "],\n\t",
  "[#h(1em)Emphysema], [",
  table [[which (table [,1] == "emphysema") + 2, 2]],
  "], [",
  table [[which (table [,1] == "emphysema") + 2, 3]],
  "],\n\t",
  "[#h(1em)Honeycombing], [",
  table [[which (table [,1] == "honeycombing") + 2, 2]],
  "], [",
  table [[which (table [,1] == "honeycombing") + 2, 3]],
  "],\n\t",
  "[#h(1em)Reticulation], [",
  table [[which (table [,1] == "reticulation") + 2, 2]],
  "], [",
  table [[which (table [,1] == "reticulation") + 2, 3]],
  "],\n\t",
  "[#h(1em)Traction Bronchiectasis], [",
  table [[which (table [,1] == "traction") + 2, 2]],
  "], [",
  table [[which (table [,1] == "traction") + 2, 3]],
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

write_lines (table, "../tables/e-table-7.txt")

################################################################################
## e-table 8 - Baseline Characteristics by Race
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
  mutate (emphysema = as.factor (emphysema)) %>% 
  mutate (honeycombing = as.factor (honeycombing)) %>% 
  mutate (reticulation = as.factor (reticulation)) %>% 
  mutate (thickening = as.factor (thickening)) %>% 
  mutate (traction = as.factor (traction))

table <- table1 (~
  age +
  height +
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
  emphysema +
  honeycombing +
  reticulation +
  thickening +
  traction +
  spirometry +
  fev1_z_score +
  fvc_z_score +
  fev1_fvc_z_score +
  tlc_z_score +
  rv_z_score +
  dlco_z_score |
  race,
  data = data,
  big.mark =",",
  render.categorical = render_categorical,
  render.continuous = render_continuous
)

table <- as.data.frame (table)

table <- paste (
  "#table(\n\t",
  "inset: (x: 10pt, y: 2pt),\n\t",
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
  "[*Height, cm*], [",
  table [[which (table [,1] == "height"), 2]],
  "], [",
  table [[which (table [,1] == "height"), 3]],
  "], [",
  table [[which (table [,1] == "height"), 4]],
  "], [",
  table [[which (table [,1] == "height"), 5]],
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
  "[#h(1em)Neuromuscular Disorder], [",
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
  "[*Computed Tomography Findings*], [], [], [], [],\n\t",
  "[#h(1em)Bronchial Wall Thickening], [",
  table [[which (table [,1] == "thickening") + 2, 2]],
  "], [",
  table [[which (table [,1] == "thickening") + 2, 3]],
  "], [",  
  table [[which (table [,1] == "thickening") + 2, 4]],
  "], [",  
  table [[which (table [,1] == "thickening") + 2, 5]],
  "],\n\t",
  "[#h(1em)Emphysema], [",
  table [[which (table [,1] == "emphysema") + 2, 2]],
  "], [",
  table [[which (table [,1] == "emphysema") + 2, 3]],
  "], [",  
  table [[which (table [,1] == "emphysema") + 2, 4]],
  "], [",  
  table [[which (table [,1] == "emphysema") + 2, 5]],
  "],\n\t",
  "[#h(1em)Honeycombing], [",
  table [[which (table [,1] == "honeycombing") + 2, 2]],
  "], [",
  table [[which (table [,1] == "honeycombing") + 2, 3]],
  "], [",  
  table [[which (table [,1] == "honeycombing") + 2, 4]],
  "], [",  
  table [[which (table [,1] == "honeycombing") + 2, 5]],
  "],\n\t",
  "[#h(1em)Reticulation], [",
  table [[which (table [,1] == "reticulation") + 2, 2]],
  "], [",
  table [[which (table [,1] == "reticulation") + 2, 3]],
  "], [",  
  table [[which (table [,1] == "reticulation") + 2, 4]],
  "], [",  
  table [[which (table [,1] == "reticulation") + 2, 5]],
  "],\n\t",
  "[#h(1em)Traction], [",
  table [[which (table [,1] == "traction") + 2, 2]],
  "], [",
  table [[which (table [,1] == "traction") + 2, 3]],
  "], [",  
  table [[which (table [,1] == "traction") + 2, 4]],
  "], [",  
  table [[which (table [,1] == "traction") + 2, 5]],
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

write_lines (table, "../tables/e-table-8.txt")

################################################################################
## e-Table 9 - PFT Characteristics by Pulmonary Diagnostic Lab
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
  filter (is.na (lab) == 0) %>%
  mutate (lab = as.factor (lab)) %>% 
  mutate (emphysema = as.factor (emphysema)) %>% 
  mutate (honeycombing = as.factor (honeycombing)) %>% 
  mutate (reticulation = as.factor (reticulation)) %>% 
  mutate (thickening = as.factor (thickening)) %>% 
  mutate (traction = as.factor (traction))

table <- table1 (~
  age +
  height +
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
  thickening +
  emphysema +
  honeycombing +
  reticulation +
  traction +    
  spirometry +
  fev1_z_score +
  fvc_z_score +
  fev1_fvc_z_score +
  tlc_z_score +
  rv_z_score +
  dlco_z_score |
  lab,
  data = data,
  big.mark =",",
  render.categorical = render_categorical,
  render.continuous = render_continuous 
)

table <- as.data.frame (table)

table <- paste (
  "#table(\n\t",
  "inset: (x: 10pt, y: 2pt),\n\t",
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
  "[*Height, cm*], [",
  table [[which (table [,1] == "height"), 2]],
  "], [",
  table [[which (table [,1] == "height"), 3]],
  "], [",
  table [[which (table [,1] == "height"), 4]],
  "], [",
  table [[which (table [,1] == "height"), 5]],
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
  "[#h(1em)Neuromuscular Disorder], [",
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
  "[*Computed Tomography Findings*], [], [], [], [],\n\t",
  "[#h(1em)Bronchial Wall Thickening], [",
  table [[which (table [,1] == "thickening") + 2, 2]],
  "], [",
  table [[which (table [,1] == "thickening") + 2, 3]],
  "], [",  
  table [[which (table [,1] == "thickening") + 2, 4]],
  "], [",  
  table [[which (table [,1] == "thickening") + 2, 5]],
  "],\n\t",
  "[#h(1em)Emphysema], [",
  table [[which (table [,1] == "emphysema") + 2, 2]],
  "], [",
  table [[which (table [,1] == "emphysema") + 2, 3]],
  "], [",  
  table [[which (table [,1] == "emphysema") + 2, 4]],
  "], [",  
  table [[which (table [,1] == "emphysema") + 2, 5]],
  "],\n\t",
  "[#h(1em)Honeycombing], [",
  table [[which (table [,1] == "honeycombing") + 2, 2]],
  "], [",
  table [[which (table [,1] == "honeycombing") + 2, 3]],
  "], [",  
  table [[which (table [,1] == "honeycombing") + 2, 4]],
  "], [",  
  table [[which (table [,1] == "honeycombing") + 2, 5]],
  "],\n\t",
  "[#h(1em)Reticulation], [",
  table [[which (table [,1] == "reticulation") + 2, 2]],
  "], [",
  table [[which (table [,1] == "reticulation") + 2, 3]],
  "], [",  
  table [[which (table [,1] == "reticulation") + 2, 4]],
  "], [",  
  table [[which (table [,1] == "reticulation") + 2, 5]],
  "],\n\t",
  "[#h(1em)Traction Bronchiectasis], [",
  table [[which (table [,1] == "traction") + 2, 2]],
  "], [",
  table [[which (table [,1] == "traction") + 2, 3]],
  "], [",  
  table [[which (table [,1] == "traction") + 2, 4]],
  "], [",  
  table [[which (table [,1] == "traction") + 2, 5]],
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

write_lines (table, "../tables/e-table-9.txt")

################################################################################
## e-table 10 - Baseline Characteristics by Ordering Specialty
################################################################################

data <- pfts %>%
  filter (specialty == 1 | specialty == 2) %>% 
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
  mutate (sex = as.factor (sex)) %>% 
  mutate (specialty = as.factor (specialty)) %>%
  mutate (cough = as.factor (cough)) %>% 
  mutate (dyspnea = as.factor (dyspnea)) %>% 
  mutate (wheeze = as.factor (wheeze)) %>% 
  mutate (emphysema = as.factor (emphysema)) %>% 
  mutate (honeycombing = as.factor (honeycombing)) %>% 
  mutate (reticulation = as.factor (reticulation)) %>% 
  mutate (thickening = as.factor (thickening)) %>% 
  mutate (traction = as.factor (traction))

table <- table1 (~
  age +
  height +
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
  thickening +
  emphysema +
  honeycombing +
  reticulation +
  traction +
  spirometry +
  fev1_z_score +
  fvc_z_score +
  fev1_fvc_z_score +
  tlc_z_score +
  rv_z_score +
  dlco_z_score |
  specialty,
  data = data,
  big.mark =",",
  render.categorical = render_categorical,
  render.continuous = render_continuous
)

table <- as.data.frame (table)

table <- paste (
  "#table(\n\t",
  "inset: (x: 10pt, y: 2pt),\n\t",
  "columns: 3,\n\t",
  "align: (left, center, center),\n\t",
  "table.hline(),\n\t",
  "[], [*Pulmonology*], [*Primary Care*],\n\t",
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
  "[*Height, cm*], [",
  table [[which (table [,1] == "height"), 2]],
  "], [",
  table [[which (table [,1] == "height"), 3]],
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
  "[#h(1em)Neuromuscular Disorder], [",
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
  "[#h(1em)Bronchial Wall Thickening], [",
  table [[which (table [,1] == "thickening") + 2, 2]],
  "], [",
  table [[which (table [,1] == "thickening") + 2, 3]],
  "],\n\t",
  "[#h(1em)Emphysema], [",
  table [[which (table [,1] == "emphysema") + 2, 2]],
  "], [",
  table [[which (table [,1] == "emphysema") + 2, 3]],
  "],\n\t",
  "[#h(1em)Honeycombing], [",
  table [[which (table [,1] == "honeycombing") + 2, 2]],
  "], [",
  table [[which (table [,1] == "honeycombing") + 2, 3]],
  "],\n\t",
  "[#h(1em)Reticulation], [",
  table [[which (table [,1] == "reticulation") + 2, 2]],
  "], [",
  table [[which (table [,1] == "reticulation") + 2, 3]],
  "],\n\t",   
  "[#h(1em)Traction Bronchiectasis], [",
  table [[which (table [,1] == "traction") + 2, 2]],
  "], [",
  table [[which (table [,1] == "traction") + 2, 3]],
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

write_lines (table, "../tables/e-table-10.txt")

################################################################################
## e-Table 11 - Missing Data
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
  filter (
    is.na (emphysema) &
    is.na (honeycombing) &
    is.na (reticulation) &
    is.na (thickening) &
    is.na (traction)
  ) %>% 
  nrow () %>% 
  comma ()

data [7, 2] <- pfts %>% 
  filter (
    is.na (emphysema) &
    is.na (honeycombing) &
    is.na (reticulation) &
    is.na (thickening) &
    is.na (traction)
  ) %>% 
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

# Location

data [12, 1] <- pfts %>% 
  filter (is.na (lab)) %>% 
  nrow () %>% 
  comma ()

data [12, 2] <- pfts %>% 
  filter (is.na (lab)) %>% 
  nrow () %>% 
  divide_by (nrow (pfts)) %>%
  multiply_by (100) %>% 
  round (digits = 1) %>% 
  format (nsmall = 1)

# Specialty

data [13, 1] <- pfts %>% 
  filter (is.na (specialty)) %>% 
  nrow () %>% 
  comma ()

data [13, 2] <- pfts %>% 
  filter (is.na (specialty)) %>% 
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
  "[Pulmonary Diagnostic Lab], [",
  data [12, 1],
  "], [",
  data [12, 2],
  "%],\n\t",
  "[Specialty of Referring Provider], [",
  data [13, 1],
  "], [",
  data [13, 2],
  "%],\n\t",  
  "table.hline()\n",
  ")", 
  sep = ""
)  
  
write_lines (table, "../tables/e-table-11.txt")

################################################################################
## e-Table 12 - Characteristics Associated with Restriction with Normal
## Spirometry
################################################################################

data <- pfts %>%
  filter (fvc_z_score >= -1.645 & fev1_fvc_z_score >= -1.645)

# Age

model_age_unadjusted <- tidy (
  glm (
    formula = "restriction ~ age",
    family = "binomial",
    data = data
  ),
  conf.int = TRUE,
  exponentiate = TRUE
)

model_age_adjusted <- tidy (
  glm (
    formula = "restriction ~ age + sex + height + fev1_z_score + fvc_z_score + fev1_fvc_z_score",
    family = "binomial",
    data = data
  ),
  conf.int = TRUE,
  exponentiate = TRUE
)

# Sex

model_sex_unadjusted <- tidy (
  glm (
    formula = "restriction ~ sex",
    family = "binomial",
    data = data
  ),
  conf.int = TRUE,
  exponentiate = TRUE
)

model_sex_adjusted <- tidy (
  glm (
    formula = "restriction ~ sex + age + height + fev1_z_score + fvc_z_score + fev1_fvc_z_score",
    family = "binomial",
    data = data
  ),
  conf.int = TRUE,
  exponentiate = TRUE
)

# Race

model_race_unadjusted <- tidy (
  glm (
    formula = "restriction ~ race",
    family = "binomial",
    data = data
  ),
  conf.int = TRUE,
  exponentiate = TRUE
)

model_race_adjusted <- tidy (
  glm (
    formula = "restriction ~ race + sex + age + height + fev1_z_score + fvc_z_score + fev1_fvc_z_score",
    family = "binomial",
    data = data
  ),
  conf.int = TRUE,
  exponentiate = TRUE
)

# Ethnicity

model_ethnicity_unadjusted <- tidy (
  glm (
    formula = "restriction ~ ethnicity",
    family = "binomial",
    data = data
  ),
  conf.int = TRUE,
  exponentiate = TRUE
)

model_ethnicity_adjusted <- tidy (
  glm (
    formula = "restriction ~ ethnicity + sex + age + height + fev1_z_score + fvc_z_score + fev1_fvc_z_score",
    family = "binomial",
    data = data
  ),
  conf.int = TRUE,
  exponentiate = TRUE
)

# BMI

model_bmi_unadjusted <- tidy (
  glm (
    formula = "restriction ~ bmi",
    family = "binomial",
    data = data
  ),
  conf.int = TRUE,
  exponentiate = TRUE
)

model_bmi_adjusted <- tidy (
  glm (
    formula = "restriction ~ bmi + sex + age + height + fev1_z_score + fvc_z_score + fev1_fvc_z_score",
    family = "binomial",
    data = data
  ),
  conf.int = TRUE,
  exponentiate = TRUE
)

# Pack Years

model_pack_years_unadjusted <- tidy (
  glm (
    formula = "restriction ~ pack_years",
    family = "binomial",
    data = data
  ),
  conf.int = TRUE,
  exponentiate = TRUE
)

model_pack_years_adjusted <- tidy (
  glm (
    formula = "restriction ~ pack_years + sex + age + height + fev1_z_score + fvc_z_score + fev1_fvc_z_score",
    family = "binomial",
    data = data
  ),
  conf.int = TRUE,
  exponentiate = TRUE
)

# Bronchodilator Response

model_response_unadjusted <- tidy (
  glm (
    formula = "restriction ~ response",
    family = "binomial",
    data = data
  ),
  conf.int = TRUE,
  exponentiate = TRUE
)

model_response_adjusted <- tidy (
  glm (
    formula = "restriction ~ response + sex + age + height + fev1_z_score + fvc_z_score + fev1_fvc_z_score",
    family = "binomial",
    data = data
  ),
  conf.int = TRUE,
  exponentiate = TRUE
)

# Adequate Effort

model_effort_unadjusted <- tidy (
  glm (
    formula = "restriction ~ effort",
    family = "binomial",
    data = data
  ),
  conf.int = TRUE,
  exponentiate = TRUE
)

model_effort_adjusted <- tidy (
  glm (
    formula = "restriction ~ effort + sex + age + height + fev1_z_score + fvc_z_score + fev1_fvc_z_score",
    family = "binomial",
    data = data
  ),
  conf.int = TRUE,
  exponentiate = TRUE
)

# Residual Volume

model_rv_unadjusted <- tidy (
  glm (
    formula = "restriction ~ rv_z_score",
    family = "binomial",
    data = data
  ),
  conf.int = TRUE,
  exponentiate = TRUE
)

model_rv_adjusted <- tidy (
  glm (
    formula = "restriction ~ rv_z_score + sex + age + height + fev1_z_score + fvc_z_score + fev1_fvc_z_score",
    family = "binomial",
    data = data
  ),
  conf.int = TRUE,
  exponentiate = TRUE
)

# Diffusing Capacity

model_dlco_unadjusted <- tidy (
  glm (
    formula = "restriction ~ dlco_z_score",
    family = "binomial",
    data = data
  ),
  conf.int = TRUE,
  exponentiate = TRUE
)

model_dlco_adjusted <- tidy (
  glm (
    formula = "restriction ~ dlco_z_score + sex + age + height + fev1_z_score + fvc_z_score + fev1_fvc_z_score",
    family = "binomial",
    data = data
  ),
  conf.int = TRUE,
  exponentiate = TRUE
)

table <- paste (
  "#table(\n\t",
	"inset: (x: 10pt, y: 4pt),\n\t",
	"columns: 3,\n\t",
	"align: (left, center, center),\n\t",
	"table.hline(),\n\t",
	"[*Characteristic*], [*Unadjusted Odds Ratio (95% CI)*], [*Adjusted Odds Ratio (95% CI)*],\n\t",
	"table.hline(),\n\t",
  "[*Age, yr*],[", print_model (model_age_unadjusted), "],[", print_model (model_age_adjusted), "],\n\t",
  "[*Sex*],[],[],\n\t",
  "[#h(1em)Male],[1.00 (Reference)],[1.00 (Reference)],\n\t",
  "[#h(1em)Female],[", print_model (model_sex_unadjusted), "],[", print_model (model_sex_adjusted), "],\n\t",
  "[*Race*],[],[],\n\t",
  "[#h(1em)White],[1.00 (Reference)],[1.00 (Reference)],\n\t",
  "[#h(1em)Black],[", print_model_line (model_race_unadjusted, 2), "],[", print_model_line (model_race_adjusted, 2), "],\n\t",
  "[#h(1em)Asian],[", print_model_line (model_race_unadjusted, 3), "],[", print_model_line (model_race_adjusted, 3), "],\n\t",
  "[#h(1em)Other],[", print_model_line (model_race_unadjusted, 4), "],[", print_model_line (model_race_adjusted, 4), "],\n\t",
  "[*Ethnicity*],[],[],\n\t",
  "[#h(1em)Hispanic],[1.00 (Reference)],[1.00 (Reference)],\n\t",
  "[#h(1em)Not Hispanic],[", print_model_line (model_ethnicity_unadjusted, 2), "],[", print_model_line (model_ethnicity_adjusted, 2), "],\n\t",  
  "[*BMI, kg/m#super[2]*],[", print_model (model_bmi_unadjusted), "],[", print_model (model_bmi_adjusted), "],\n\t",
  "[*Pack Years, yr*],[", print_model (model_pack_years_unadjusted), "],[", print_model (model_pack_years_adjusted), "],\n\t",
  "[*Spirometry*],[],[],\n\t",
  "[#h(1em)Bronchodilator Response],[", print_model (model_response_unadjusted), "],[", print_model (model_response_adjusted), "],\n\t",
  "[#h(1em)Adequate Effort],[", print_model (model_effort_unadjusted), "],[", print_model (model_effort_adjusted), "],\n\t",
  "[*Static Lung Volumes*],[],[],\n\t",
  "[#h(1em)RV, z-score],[", print_model (model_rv_unadjusted), "],[", print_model (model_rv_adjusted), "],\n\t",
  "[*Diffusing Capacity*],[],[],\n\t",
  "[#h(1em)D#sub[LCO], z-score],[", print_model (model_dlco_unadjusted), "],[", print_model (model_dlco_adjusted), "],\n\t",
  "table.hline()\n",
  ")",
  sep = ""
)

write_lines (table, "../tables/e-table-12.txt")

################################################################################
## e-Table 13 - Clinical Implications of Restriction in PFTs with Normal
## Spirometry by Sex
################################################################################

table <- paste (
  "#table(\n\t",
  "inset: (x: 10pt, y: 3pt),\n\t",
  "columns: 3,\n\t",
  "align: (left, center, center),\n\t",
  "table.hline(),\n\t",
  "[], table.cell(colspan: 2, [*Patient Sex*]),\n\t",
	"table.hline(start:1, end:3),\n\t",
  "[], [*Male*], [*Female*],\n\t",
  "[*Outcome*], [$n = ",
  comma (nrow (filter (pfts, fvc_z_score >= -1.645 & fev1_fvc_z_score >= -1.645 & sex == 1))),
  "$], [$n = ",
  comma (nrow (filter (pfts, fvc_z_score >= -1.645 & fev1_fvc_z_score >= -1.645 & sex == 2))),
  "$],\n\t",
  "table.hline(),\n\t",
  "[*Respiratory Symptoms*], [], [],\n\t",
  "[#h(1em)Cough],",
  "[",print_model (model_cough_sex_1),"],",
  "[",print_model (model_cough_sex_2),"],\n\t",
  "[#h(1em)Dyspnea],",
  "[",print_model (model_dyspnea_sex_1),"],",
  "[",print_model (model_dyspnea_sex_2),"],\n\t",
  "[#h(1em)Wheeze],",
  "[",print_model (model_wheeze_sex_1),"],",
  "[",print_model (model_wheeze_sex_2),"],\n\t",
  "[*Respiratory Disease Diagnoses*], [], [],\n\t",
  "[#h(1em)Asthma],",
  "[",print_model (model_asthma_sex_1),"],",
  "[",print_model (model_asthma_sex_2),"],\n\t",
  "[#h(1em)Broncheictasis],",
  "[",print_model (model_bronchiectasis_sex_1),"],",
  "[",print_model (model_bronchiectasis_sex_2),"],\n\t",
  "[#h(1em)Chest Wall Disorder],",
  "[",print_model (model_chest_wall_sex_1),"],",
  "[",print_model (model_chest_wall_sex_2),"],\n\t",
  "[#h(1em)Chronic Obstructive Pulmonary Disease],",
  "[",print_model (model_copd_sex_1),"],",
  "[",print_model (model_copd_sex_2),"],\n\t",
  "[#h(1em)Interstitial Lung Disease],",
  "[",print_model (model_ild_sex_1),"],",
  "[",print_model (model_ild_sex_2),"],\n\t",
  "[#h(1em)Neuromuscular Disorder],",
  "[",print_model (model_neuromuscular_sex_1),"],",
  "[",print_model (model_neuromuscular_sex_2),"],\n\t",
  "[*Chest Computed Tomography Findings*], [], [],\n\t",
  "[#h(1em)Bronchial Wall Thickening],",
  "[",print_model (model_thickening_sex_1),"],",
  "[",print_model (model_thickening_sex_2),"],\n\t",
  "[#h(1em)Emphysema],",
  "[",print_model (model_emphysema_sex_1),"],",
  "[",print_model (model_emphysema_sex_2),"],\n\t",
  "[#h(1em)Honeycombing],",
  "[",print_model (model_honeycombing_sex_1),"],",
  "[",print_model (model_honeycombing_sex_2),"],\n\t",
  "[#h(1em)Reticulation],",
  "[",print_model (model_reticulation_sex_1),"],",
  "[",print_model (model_reticulation_sex_2),"],\n\t",
  "[#h(1em)Traction Bronchiectasis],",
  "[",print_model (model_traction_sex_1),"],",
  "[",print_model (model_traction_sex_2),"],\n\t",    
  "[*Event*], [], [],\n\t",
  "[#h(1em)ED Visit with Respiratory Complaint],",
  "[",print_model_ph (model_ed_sex_1),"],",
  "[",print_model_ph (model_ed_sex_2),"],\n\t",
  "[#h(1em)Death from Any Cause],",
  "[",print_model_ph (model_death_sex_1),"],",
  "[",print_model_ph (model_death_sex_2),"],\n\t",  
  "table.hline()\n",
  ")",
  sep = ""
)

write_lines (table, "../tables/e-table-13.txt")

################################################################################
## e-Table 14 - Clinical Implications of Restriction in PFTs with Normal
## Spirometry by Race
################################################################################

table <- paste (
  "#table(\n\t",
  "inset: (x: 4pt, y: 4pt),\n\t",
  "columns: 5,\n\t",
  "align: (left, center, center, center, center),\n\t",
  "table.hline(),\n\t",
  "[], table.cell(colspan: 4, [*Adjusted Association (95% CI)*]),\n\t",
	"table.hline(start:1, end:6),\n\t",
  "[], [*White*], [*Black*], [*Asian*], [*Other*],\n\t",
  "[*Outcome*], [*($n = ",
  comma (nrow (filter (pfts, fvc_z_score >= -1.645 & fev1_fvc_z_score >= -1.645 & race == 1))),
  "$)*], [*($n = ",
  comma (nrow (filter (pfts, fvc_z_score >= -1.645 & fev1_fvc_z_score >= -1.645 & race == 2))),
  "$)*], [*($n = ",
  comma (nrow (filter (pfts, fvc_z_score >= -1.645 & fev1_fvc_z_score >= -1.645 & race == 3))),
  "$)*], [*($n = ",
  comma (nrow (filter (pfts, fvc_z_score >= -1.645 & fev1_fvc_z_score >= -1.645 & race == 4))),  
  "$)*],\n\t",
  "table.hline(),\n\t",
  "[*Respiratory Symptoms*], [], [], [], [],\n\t",
  "[#h(1em)Cough],",
  "[",print_model (model_cough_race_1),"],",
  "[",print_model (model_cough_race_2),"],",
  "[",print_model (model_cough_race_3),"],",
  "[",print_model (model_cough_race_4),"],\n\t",  
  "[#h(1em)Dyspnea],",
  "[",print_model (model_dyspnea_race_1),"],",
  "[",print_model (model_dyspnea_race_2),"],",
  "[",print_model (model_dyspnea_race_3),"],",
  "[",print_model (model_dyspnea_race_4),"],\n\t", 
  "[#h(1em)Wheeze],",
  "[",print_model (model_wheeze_race_1),"],",
  "[",print_model (model_wheeze_race_2),"],",
  "[",print_model (model_wheeze_race_3),"],",
  "[",print_model (model_wheeze_race_4),"],\n\t", 
  "[*Respiratory Disease Diagnoses*], [], [], [], [],\n\t",
  "[#h(1em)Asthma],",
  "[",print_model (model_asthma_race_1),"],",
  "[",print_model (model_asthma_race_2),"],",
  "[",print_model (model_asthma_race_3),"],",
  "[",print_model (model_asthma_race_4),"],\n\t",
  "[#h(1em)Broncheictasis],",
  "[",print_model (model_bronchiectasis_race_1),"],",
  "[",print_model (model_bronchiectasis_race_2),"],",
  "[",print_model (model_bronchiectasis_race_3),"],",
  "[",print_model (model_bronchiectasis_race_4),"],\n\t",
  "[#h(1em)Chest Wall Disorder],",
  "[",print_model (model_chest_wall_race_1),"],",
  "[",print_model (model_chest_wall_race_2),"],",
  "[",print_model (model_chest_wall_race_3),"],",
  "[",print_model (model_chest_wall_race_4),"],\n\t",
  "[#h(1em)Chronic Obstructive Pulmonary Disease],",
  "[",print_model (model_copd_race_1),"],",
  "[",print_model (model_copd_race_2),"],",
  "[",print_model (model_copd_race_3),"],",
  "[",print_model (model_copd_race_4),"],\n\t",
  "[#h(1em)Interstitial Lung Disease],",
  "[",print_model (model_ild_race_1),"],",
  "[",print_model (model_ild_race_2),"],",
  "[",print_model (model_ild_race_3),"],",
  "[",print_model (model_ild_race_4),"],\n\t",
  "[#h(1em)Neuromuscular Disorder],",
  "[",print_model (model_neuromuscular_race_1),"],",
  "[",print_model (model_neuromuscular_race_2),"],",
  "[",print_model (model_neuromuscular_race_3),"],",
  "[",print_model (model_neuromuscular_race_4),"],\n\t",
  "[*Chest Computed Tomography Findings*], [], [], [], [],\n\t",
  "[#h(1em)Bronchial Wall Thickening],",
  "[",print_model (model_thickening_race_1),"],",
  "[",print_model (model_thickening_race_2),"],",
  "[",print_model (model_thickening_race_3),"],",
  "[",print_model (model_thickening_race_4),"],\n\t",
  "[#h(1em)Emphysema],",
  "[",print_model (model_emphysema_race_1),"],",
  "[",print_model (model_emphysema_race_2),"],",
  "[",print_model (model_emphysema_race_3),"],",
  "[",print_model (model_emphysema_race_4),"],\n\t",
  "[#h(1em)Honeycombing],",
  "[",print_model (model_honeycombing_race_1),"],",
  "[",print_model (model_honeycombing_race_2),"],",
  "[",print_model (model_honeycombing_race_3),"],",
  "[",print_model (model_honeycombing_race_4),"],\n\t",
  "[#h(1em)Reticulation],",
  "[",print_model (model_reticulation_race_1),"],",
  "[",print_model (model_reticulation_race_2),"],",
  "[",print_model (model_reticulation_race_3),"],",
  "[",print_model (model_reticulation_race_4),"],\n\t",
  "[#h(1em)Traction Bronchiectasis],",
  "[",print_model (model_traction_race_1),"],",
  "[",print_model (model_traction_race_2),"],",
  "[",print_model (model_traction_race_3),"],",
  "[",print_model (model_traction_race_4),"],\n\t",
  "[*Event*], [], [], [], [],\n\t",
  "[#h(1em)ED Visit with Respiratory Complaint],",
  "[",print_model_ph (model_ed_race_1),"],",
  "[",print_model_ph (model_ed_race_2),"],",
  "[",print_model_ph (model_ed_race_3),"],",
  "[",print_model_ph (model_ed_race_4),"],\n\t",  
  "[#h(1em)Death from Any Cause],",
  "[",print_model_ph (model_death_race_1),"],",
  "[",print_model_ph (model_death_race_2),"],",
  "[",print_model_ph (model_death_race_3),"],",
  "[",print_model_ph (model_death_race_4),"],\n\t",  
  "table.hline()\n",
  ")",
  sep = ""
)

write_lines (table, "../tables/e-table-14.txt")

################################################################################
## e-Table 15 - Clinical Implications of Restriction in PFTs with
## Normal Spirometry by Lab
################################################################################

table <- paste (
  "#table(\n\t",
  "inset: (x: 4pt, y: 4pt),\n\t",
  "columns: 5,\n\t",
  "align: (left, center, center, center, center),\n\t",
  "table.hline(),\n\t",
  "[], table.cell(colspan: 4, [*Adjusted Association (95% CI)*]),\n\t",
	"table.hline(start:1, end:6),\n\t",
  "[], [*Lab 1*], [*Lab 2*], [*Lab 3*], [*Lab 4*],\n\t",
  "[*Outcome*], [*($n = ",
  comma (nrow (filter (pfts, fvc_z_score >= -1.645 & fev1_fvc_z_score >= -1.645 & lab == 1))),
  "$)*], [*($n = ",
  comma (nrow (filter (pfts, fvc_z_score >= -1.645 & fev1_fvc_z_score >= -1.645 & lab == 2))),
  "$)*], [*($n = ",
  comma (nrow (filter (pfts, fvc_z_score >= -1.645 & fev1_fvc_z_score >= -1.645 & lab == 3))),
  "$)*], [*($n = ",
  comma (nrow (filter (pfts, fvc_z_score >= -1.645 & fev1_fvc_z_score >= -1.645 & lab == 4))),  
  "$)*],\n\t",
  "table.hline(),\n\t",
  "[*Respiratory Symptoms*], [], [], [], [],\n\t",
  "[#h(1em)Cough],",
  "[",print_model (model_cough_lab_1),"],",
  "[",print_model (model_cough_lab_2),"],",
  "[",print_model (model_cough_lab_3),"],",
  "[","---","],\n\t",  
  "[#h(1em)Dyspnea],",
  "[",print_model (model_dyspnea_lab_1),"],",
  "[",print_model (model_dyspnea_lab_2),"],",
  "[",print_model (model_dyspnea_lab_3),"],",
  "[","---","],\n\t", 
  "[#h(1em)Wheeze],",
  "[",print_model (model_wheeze_lab_1),"],",
  "[",print_model (model_wheeze_lab_2),"],",
  "[",print_model (model_wheeze_lab_3),"],",
  "[","---","],\n\t", 
  "[*Respiratory Disease Diagnoses*], [], [], [], [],\n\t",
  "[#h(1em)Asthma],",
  "[",print_model (model_asthma_lab_1),"],",
  "[",print_model (model_asthma_lab_2),"],",
  "[",print_model (model_asthma_lab_3),"],",
  "[",print_model (model_asthma_lab_4),"],\n\t",
  "[#h(1em)Broncheictasis],",
  "[",print_model (model_bronchiectasis_lab_1),"],",
  "[",print_model (model_bronchiectasis_lab_2),"],",
  "[",print_model (model_bronchiectasis_lab_3),"],",
  "[",print_model (model_bronchiectasis_lab_4),"],\n\t",
  "[#h(1em)Chest Wall Disorder],",
  "[",print_model (model_chest_wall_lab_1),"],",
  "[",print_model (model_chest_wall_lab_2),"],",
  "[","---","],",
  "[","---","],\n\t",
  "[#h(1em)Chronic Obstructive Pulmonary Disease],",
  "[",print_model (model_copd_lab_1),"],",
  "[",print_model (model_copd_lab_2),"],",
  "[",print_model (model_copd_lab_3),"],",
  "[",print_model (model_copd_lab_4),"],\n\t",
  "[#h(1em)Interstitial Lung Disease],",
  "[",print_model (model_ild_lab_1),"],",
  "[",print_model (model_ild_lab_2),"],",
  "[",print_model (model_ild_lab_3),"],",
  "[",print_model (model_ild_lab_4),"],\n\t",
  "[#h(1em)Neuromuscular Disorder],",
  "[",print_model (model_neuromuscular_lab_1),"],",
  "[",print_model (model_neuromuscular_lab_2),"],",
  "[",print_model (model_neuromuscular_lab_3),"],",
  "[",print_model (model_neuromuscular_lab_4),"],\n\t",
  "[*Chest Computed Tomography Findings*], [], [], [], [],\n\t",
  "[#h(1em)Bronchial Wall Thickening],",
  "[",print_model (model_thickening_lab_1),"],",
  "[",print_model (model_thickening_lab_2),"],",
  "[",print_model (model_thickening_lab_3),"],",
  "[",print_model (model_thickening_lab_4),"],\n\t",
  "[#h(1em)Emphysema],",
  "[",print_model (model_emphysema_lab_1),"],",
  "[",print_model (model_emphysema_lab_2),"],",
  "[",print_model (model_emphysema_lab_3),"],",
  "[",print_model (model_emphysema_lab_4),"],\n\t",
  "[#h(1em)Honeycombing],",
  "[",print_model (model_honeycombing_lab_1),"],",
  "[",print_model (model_honeycombing_lab_2),"],",
  "[",print_model (model_honeycombing_lab_3),"],",
  "[",print_model (model_honeycombing_lab_4),"],\n\t",
  "[#h(1em)Reticulation],",
  "[",print_model (model_reticulation_lab_1),"],",
  "[",print_model (model_reticulation_lab_2),"],",
  "[",print_model (model_reticulation_lab_3),"],",
  "[",print_model (model_reticulation_lab_4),"],\n\t",
  "[#h(1em)Traction Bronchiectasis],",
  "[",print_model (model_traction_lab_1),"],",
  "[",print_model (model_traction_lab_2),"],",
  "[",print_model (model_traction_lab_3),"],",
  "[",print_model (model_traction_lab_4),"],\n\t",
  "[*Event*], [], [], [], [],\n\t",
  "[#h(1em)ED Visit with Respiratory Complaint],",
  "[",print_model_ph (model_ed_lab_1),"],",
  "[",print_model_ph (model_ed_lab_2),"],",
  "[",print_model_ph (model_ed_lab_3),"],",
  "[",print_model_ph (model_ed_lab_4),"],\n\t",  
  "[#h(1em)Death from Any Cause],",
  "[",print_model_ph (model_death_lab_1),"],",
  "[",print_model_ph (model_death_lab_2),"],",
  "[",print_model_ph (model_death_lab_3),"],",
  "[",print_model_ph (model_death_lab_4),"],\n\t",  
  "table.hline()\n",
  ")",
  sep = ""
)

write_lines (table, "../tables/e-table-15.txt")

################################################################################
## e-Table 16 - Clinical Implications of Restriction in PFTs with Normal
## Spirometry by Referring Specialty
################################################################################

table <- paste (
  "#table(\n\t",
  "inset: (x: 10pt, y: 3pt),\n\t",
  "columns: 3,\n\t",
  "align: (left, center, center),\n\t",
  "table.hline(),\n\t",
  "[], table.cell(colspan: 2, [*Adjusted Association (95% CI)*]),\n\t",
	"table.hline(start:1, end:3),\n\t",
  "[], [*Pulmonology*], [*Primary Care*],\n\t",
  "[*Outcome*], [*($n = ",
  comma (nrow (filter (pfts, fvc_z_score >= -1.645 & fev1_fvc_z_score >= -1.645 & specialty == 1))),
  "$*)], [*($n =",
  comma (nrow (filter (pfts, fvc_z_score >= -1.645 & fev1_fvc_z_score >= -1.645 & specialty == 2))),
  "$*)],\n\t",
  "table.hline(),\n\t",
  "[*Respiratory Symptoms*], [], [],\n\t",
  "[#h(1em)Cough],",
  "[",print_model (model_cough_specialty_1),"],",
  "[",print_model (model_cough_specialty_2),"],\n\t",
  "[#h(1em)Dyspnea],",
  "[",print_model (model_dyspnea_specialty_1),"],",
  "[",print_model (model_dyspnea_specialty_2),"],\n\t",
  "[#h(1em)Wheeze],",
  "[",print_model (model_wheeze_specialty_1),"],",
  "[",print_model (model_wheeze_specialty_2),"],\n\t",
  "[*Respiratory Disease Diagnoses*], [], [],\n\t",
  "[#h(1em)Asthma],",
  "[",print_model (model_asthma_specialty_1),"],",
  "[",print_model (model_asthma_specialty_2),"],\n\t",
  "[#h(1em)Broncheictasis],",
  "[",print_model (model_bronchiectasis_specialty_1),"],",
  "[",print_model (model_bronchiectasis_specialty_2),"],\n\t",
  "[#h(1em)Chest Wall Disorder],",
  "[",print_model (model_chest_wall_specialty_1),"],",
  "[",print_model (model_chest_wall_specialty_2),"],\n\t",
  "[#h(1em)Chronic Obstructive Pulmonary Disease],",
  "[",print_model (model_copd_specialty_1),"],",
  "[",print_model (model_copd_specialty_2),"],\n\t",
  "[#h(1em)Interstitial Lung Disease],",
  "[",print_model (model_ild_specialty_1),"],",
  "[",print_model (model_ild_specialty_2),"],\n\t",
  "[#h(1em)Neuromuscular Disorder],",
  "[",print_model (model_neuromuscular_specialty_1),"],",
  "[",print_model (model_neuromuscular_specialty_2),"],\n\t",
  "[*Chest Computed Tomography Findings*], [], [],\n\t",
  "[#h(1em)Bronchial Wall Thickening],",
  "[",print_model (model_thickening_specialty_1),"],",
  "[",print_model (model_thickening_specialty_2),"],\n\t",
  "[#h(1em)Emphysema],",
  "[",print_model (model_emphysema_specialty_1),"],",
  "[",print_model (model_emphysema_specialty_2),"],\n\t",
  "[#h(1em)Honeycombing],",
  "[",print_model (model_honeycombing_specialty_1),"],",
  "[",print_model (model_honeycombing_specialty_2),"],\n\t",
  "[#h(1em)Reticulation],",
  "[",print_model (model_reticulation_specialty_1),"],",
  "[",print_model (model_reticulation_specialty_2),"],\n\t",
  "[#h(1em)Traction Bronchiectasis],",
  "[",print_model (model_traction_specialty_1),"],",
  "[",print_model (model_traction_specialty_2),"],\n\t",    
  "[*Event*], [], [],\n\t",
  "[#h(1em)ED Visit with Respiratory Complaint],",
  "[",print_model_ph (model_ed_specialty_1),"],",
  "[",print_model_ph (model_ed_specialty_2),"],\n\t",
  "[#h(1em)Death from Any Cause],",
  "[",print_model_ph (model_death_specialty_1),"],",
  "[",print_model_ph (model_death_specialty_2),"],\n\t",  
  "table.hline()\n",
  ")",
  sep = ""
)

write_lines (table, "../tables/e-table-16.txt")

################################################################################
## e-Table 17 - Clinical Implications of Normal Spirometry in PFTs with
## Restriction
################################################################################

table <- paste (
  "#table(\n\t",
  "inset: (x: 4pt, y: 4pt),\n\t",
  "columns: 2,\n\t",
  "align: (left, center),\n\t",
  "table.hline(),\n\t",
  "[*Outcome*], [*Adjusted Association (95% CI)*],\n\t",
  "table.hline(),\n\t",
  "[*Respiratory Symptoms*], [],\n\t",
  "[#h(1em)Cough],",
  "[",print_model (model_cough_restriction),"],\n\t",  
  "[#h(1em)Dyspnea],",
  "[",print_model (model_dyspnea_restriction),"],\n\t", 
  "[#h(1em)Wheeze],",
  "[",print_model (model_wheeze_restriction),"],\n\t", 
  "[*Respiratory Disease Diagnoses*], [],\n\t",
  "[#h(1em)Asthma],",
  "[",print_model (model_asthma_restriction),"],\n\t", 
  "[#h(1em)Broncheictasis],",
  "[",print_model (model_bronchiectasis_restriction),"],\n\t", 
  "[#h(1em)Chest Wall Disorder],",
  "[",print_model (model_chest_wall_restriction),"],\n\t", 
  "[#h(1em)Chronic Obstructive Pulmonary Disease],",
  "[",print_model (model_copd_restriction),"],\n\t", 
  "[#h(1em)Interstitial Lung Disease],",
  "[",print_model (model_ild_restriction),"],\n\t", 
  "[#h(1em)Neuromuscular Disorder],",
  "[",print_model (model_neuromuscular_restriction),"],\n\t", 
  "[*Chest Computed Tomography Findings*], [],\n\t",
  "[#h(1em)Bronchial Wall Thickening],",
  "[",print_model (model_thickening_restriction),"],\n\t", 
  "[#h(1em)Emphysema],",
  "[",print_model (model_emphysema_restriction),"],\n\t", 
  "[#h(1em)Honeycombing],",
  "[",print_model (model_honeycombing_restriction),"],\n\t",
  "[#h(1em)Reticulation],",
  "[",print_model (model_reticulation_restriction),"],\n\t", 
  "[#h(1em)Traction Bronchiectasis],",
  "[",print_model (model_traction_restriction),"],\n\t", 
  "[*Events*], [],\n\t",
  "[#h(1em)ED Visit with Respiratory Complaint],",
  "[",print_model_ph (model_ed_restriction),"],\n\t",  
  "[#h(1em)Death from Any Cause],",
  "[",print_model_ph (model_death_restriction),"],\n\t",   
  "table.hline()\n",
  ")",
  sep = ""
)

write_lines (table, "../tables/e-table-17.txt")

################################################################################
## e-Table 18 - Differences in the Association of Respiratory Diseases with
## Restriction in PFTs with Normal Spirometry Using the MAP Algorithm versus the
## Presence of at least One ICD-10 Code
################################################################################

table <- paste (
  "#table(\n\t",
  "inset: (x: 10pt, y: 3pt),\n\t",
  "columns: 3,\n\t",
  "align: (left, center, center),\n\t",
  "table.hline(),\n\t",
  "[], table.cell(colspan: 2, [*Adjusted Odds Ratio (95% CI)*]),\n\t",
	"table.hline(start:1, end:3),\n\t",
  "[*Respiratory Disease*], [*MAP Algorithm*], [*$>=$ 1 ICD Code*],\n\t",
  "table.hline(),\n\t",
  "[#h(1em)Asthma],",
  "[",print_model (model_asthma),"],",
  "[",print_model (model_asthma_icd),"],\n\t",
  "[#h(1em)Broncheictasis],",
  "[",print_model (model_bronchiectasis),"],",
  "[",print_model (model_bronchiectasis_icd),"],\n\t",
  "[#h(1em)Chest Wall Disorder],",
  "[",print_model (model_chest_wall),"],",
  "[",print_model (model_chest_wall_icd),"],\n\t",
  "[#h(1em)Chronic Obstructive Pulmonary Disease],",
  "[",print_model (model_copd),"],",
  "[",print_model (model_copd_icd),"],\n\t",
  "[#h(1em)Interstitial Lung Disease],",
  "[",print_model (model_ild),"],",
  "[",print_model (model_ild_icd),"],\n\t",
  "[#h(1em)Neuromuscular Disorder],",
  "[",print_model (model_neuromuscular),"],",
  "[",print_model (model_neuromuscular_icd),"],\n\t",
  "table.hline()\n",
  ")",
  sep = ""
)

write_lines (table, "../tables/e-table-18.txt")

################################################################################
## e-Table 19 - Differences in Outcomes Associated with Restriction in PFTs with
## Normal Spirometry when Normal Spirometry Is Defined with Race-Specific versus
## Race-Neutral Reference Equations
################################################################################

table <- paste (
  "#table(\n\t",
  "inset: (x: 10pt, y: 4pt),\n\t",
  "columns: 3,\n\t",
  "align: (left, center, center),\n\t",
  "table.hline(),\n\t",
  "[], table.cell(colspan: 2, [*Adjusted Association (95% CI)*]),\n\t",
	"table.hline(start:1, end:3),\n\t",
  "[*Outcome*], [*Race-Neutral Equations*], [*Race-Specific Equations*],\n\t",
  "table.hline(),\n\t",
  "[*Respiratory Symptoms*], [], [],\n\t",
  "[#h(1em)Cough],",
  "[",print_model (model_cough),"],",
  "[",print_model (model_cough_specific),"],\n\t",
  "[#h(1em)Dyspnea],",
  "[",print_model (model_dyspnea),"],",
  "[",print_model (model_dyspnea_specific),"],\n\t",
  "[#h(1em)Wheeze],",
  "[",print_model (model_wheeze),"],",
  "[",print_model (model_wheeze_specific),"],\n\t",
  "[*Respiratory Disease Diagnoses*], [], [],\n\t",
  "[#h(1em)Asthma],",
  "[",print_model (model_asthma),"],",
  "[",print_model (model_asthma_specific),"],\n\t",
  "[#h(1em)Broncheictasis],",
  "[",print_model (model_bronchiectasis),"],",
  "[",print_model (model_bronchiectasis_specific),"],\n\t",
  "[#h(1em)Chest Wall Disorder],",
  "[",print_model (model_chest_wall),"],",
  "[",print_model (model_chest_wall_specific),"],\n\t",
  "[#h(1em)Chronic Obstructive Pulmonary Disease],",
  "[",print_model (model_copd),"],",
  "[",print_model (model_copd_specific),"],\n\t",
  "[#h(1em)Interstitial Lung Disease],",
  "[",print_model (model_ild),"],",
  "[",print_model (model_ild_specific),"],\n\t",
  "[#h(1em)Neuromuscular Disorder],",
  "[",print_model (model_neuromuscular),"],",
  "[",print_model (model_neuromuscular_specific),"],\n\t",
  "[*Chest Computed Tomography Findings*], [], [],\n\t",
  "[#h(1em)Bronchial Wall Thickening],",
  "[",print_model (model_thickening),"],",
  "[",print_model (model_thickening_specific),"],\n\t",
  "[#h(1em)Emphysema],",
  "[",print_model (model_emphysema),"],",
  "[",print_model (model_emphysema_specific),"],\n\t",
  "[#h(1em)Honeycombing],",
  "[",print_model (model_honeycombing),"],",
  "[",print_model (model_honeycombing_specific),"],\n\t",
  "[#h(1em)Reticulation],",
  "[",print_model (model_reticulation),"],",
  "[",print_model (model_reticulation_specific),"],\n\t",
  "[#h(1em)Traction Bronchiectasis],",
  "[",print_model (model_traction),"],",
  "[",print_model (model_traction_specific),"],\n\t",    
  "[*Event*], [], [],\n\t",
  "[#h(1em)ED Visit with Respiratory Complaint],",
  "[",print_model_ph (model_ed),"],",
  "[",print_model_ph (model_ed_specific),"],\n\t",
  "[#h(1em)Death from Any Cause],",
  "[",print_model_ph (model_death),"],",
  "[",print_model_ph (model_death_specific),"],\n\t",  
  "table.hline()\n",
  ")",
  sep = ""
)

write_lines (table, "../tables/e-table-19.txt")

################################################################################
## e-Table 20 - Differences in Outcomes Associated with Restriction in PFTs with
## Normal Spirometry when Normal Spirometry Is Defined with Race-Specific versus
## Race-Neutral Reference Equations, by Race
################################################################################

table <- paste (
  "#table(\n\t",
	"inset: (x: 6pt, y: 4pt),\n\t",
	"columns: 6,\n\t",
	"align: (left, center, center, center, center, center),\n\t",
	"table.hline(),\n\t",
	"[], table.cell(colspan: 5, [*Adjusted Association (95% CI)*]),\n\t",
	"table.hline(start:1, end:6),\n\t",
  "[],table.cell(colspan: 2, [*White Patients*]),[],table.cell(colspan: 2, [*Black Patients*]),\n\t",
  "table.hline(start:1, end:3), table.hline(start:4, end:6),\n\t",
  "[*Outcome*],[*Race-Neutral*],[*Race-Specific*],[],[*Race-Neutral*],[*Race-Specific*],\n\t",
	"table.hline(),\n\t",
  "[*Respiratory Symptoms*],[],[],[],[],[],\n\t",
  "[#h(1em)Cough],",
  "[",print_model (model_cough_race_1),"],",
  "[",print_model (model_cough_specific_race_1),"],",
  "[],",
  "[",print_model (model_cough_race_2),"],",
  "[",print_model (model_cough_specific_race_2),"],\n\t",
  "[#h(1em)Dyspnea],",
  "[",print_model (model_dyspnea_race_1),"],",
  "[",print_model (model_dyspnea_specific_race_1),"],",
  "[],",
  "[",print_model (model_dyspnea_race_2),"],",
  "[",print_model (model_dyspnea_specific_race_2),"],\n\t",
  "[#h(1em)Wheeze],",
  "[",print_model (model_wheeze_race_1),"],",
  "[",print_model (model_wheeze_specific_race_1),"],",
  "[],",
  "[",print_model (model_wheeze_race_2),"],",
  "[",print_model (model_wheeze_specific_race_2),"],\n\t",
  "[*Respiratory Diseases*],[],[],[],[],[],\n\t",
  "[#h(1em)Asthma],",
  "[",print_model (model_asthma_race_1),"],",
  "[",print_model (model_asthma_specific_race_1),"],",
  "[],",
  "[",print_model (model_asthma_race_2),"],",
  "[",print_model (model_asthma_specific_race_2),"],\n\t",
  "[#h(1em)Bronchiectasis],",
  "[",print_model (model_bronchiectasis_race_1),"],",
  "[",print_model (model_bronchiectasis_specific_race_1),"],",
  "[],",
  "[",print_model (model_bronchiectasis_race_2),"],",
  "[",print_model (model_bronchiectasis_specific_race_2),"],\n\t",
  "[#h(1em)Chest Wall Disorder],",
  "[",print_model (model_chest_wall_race_1),"],",
  "[",print_model (model_chest_wall_specific_race_1),"],",
  "[],",
  "[",print_model (model_chest_wall_race_2),"],",
  "[",print_model (model_chest_wall_specific_race_2),"],\n\t",
  "[#h(1em)Chronic Obstructive Pulmonary Disease],",
  "[",print_model (model_copd_race_1),"],",
  "[",print_model (model_copd_specific_race_1),"],",
  "[],",
  "[",print_model (model_copd_race_2),"],",
  "[",print_model (model_copd_specific_race_2),"],\n\t",
  "[#h(1em)Interstitial Lung Disease],",
  "[",print_model (model_ild_race_1),"],",
  "[",print_model (model_ild_specific_race_1),"],",
  "[],",
  "[",print_model (model_ild_race_2),"],",
  "[",print_model (model_ild_specific_race_2),"],\n\t",
  "[#h(1em)Neuromuscular Disorder],",
  "[",print_model (model_neuromuscular_race_1),"],",
  "[",print_model (model_neuromuscular_specific_race_1),"],",
  "[],",
  "[",print_model (model_neuromuscular_race_2),"],",
  "[",print_model (model_neuromuscular_specific_race_2),"],\n\t",
  "[*Computed Tomography Findings*],[],[],[],[],[],\n\t",
  "[#h(1em)Bronchial Wall Thickening],",
  "[",print_model (model_thickening_race_1),"],",
  "[",print_model (model_thickening_specific_race_1),"],",
  "[],",
  "[",print_model (model_thickening_race_2),"],",
  "[",print_model (model_thickening_specific_race_2),"],\n\t",
  "[#h(1em)Emphysema],",
  "[",print_model (model_emphysema_race_1),"],",
  "[",print_model (model_emphysema_specific_race_1),"],",
  "[],",
  "[",print_model (model_emphysema_race_2),"],",
  "[",print_model (model_emphysema_specific_race_2),"],\n\t",
  "[#h(1em)Honeycombing],",
  "[",print_model (model_honeycombing_race_1),"],",
  "[",print_model (model_honeycombing_specific_race_1),"],",
  "[],",
  "[",print_model (model_honeycombing_race_2),"],",
  "[",print_model (model_honeycombing_specific_race_2),"],\n\t",
  "[#h(1em)Reticulation],",
  "[",print_model (model_reticulation_race_1),"],",
  "[",print_model (model_reticulation_specific_race_1),"],",
  "[],",
  "[",print_model (model_reticulation_race_2),"],",
  "[",print_model (model_reticulation_specific_race_2),"],\n\t",
  "[#h(1em)Traction Bronchiectasis],",
  "[",print_model (model_traction_race_1),"],",
  "[",print_model (model_traction_specific_race_1),"],",
  "[],",
  "[",print_model (model_traction_race_2),"],",
  "[",print_model (model_traction_specific_race_2),"],\n\t",
  "[*Events*],[],[],[],[],[],\n\t",
  "[#h(1em)ED Visit with Respiratory Complaint],",
  "[",print_model_ph (model_ed_race_1),"],",
  "[",print_model_ph (model_ed_specific_race_1),"],",
  "[],",
  "[",print_model_ph (model_ed_race_2),"],",
  "[",print_model_ph (model_ed_specific_race_2),"],\n\t",
  "[#h(1em)Death from Any Cause],",
  "[",print_model_ph (model_death_race_1),"],",
  "[",print_model_ph (model_death_specific_race_1),"],",
  "[],",
  "[",print_model_ph (model_death_race_2),"],",
  "[",print_model_ph (model_death_specific_race_2),"],\n\t",    
  "table.hline()\n",
  ")",
  sep = ""
)

write_lines (table, "../tables/e-table-20.txt")
