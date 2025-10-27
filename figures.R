################################################################################
## Figure 1
################################################################################

data <- pfts %>% 
  mutate (restriction = case_when (
    tlc_z_score < -1.645 ~ 1,
    TRUE ~ 0)
  ) %>% 
  mutate (fvc_bin = cut (
    fvc_z_score,
    breaks = seq (from = -5.645, to = 3.355, by = 0.2))
  ) %>% 
  mutate (fev1_fvc_bin = cut (
    fev1_fvc_z_score,
    breaks = seq (from = -5.645, to = 3.355, by = 0.2))
  ) %>%
  group_by (fvc_bin, fev1_fvc_bin) %>%
  summarise(
    probability_restriction = mean (restriction),
    observations = n (),
    .groups = "drop"
  ) %>%
  filter (observations > 10) %>% 
  ungroup () %>%
  drop_na () %>%
  rowwise () %>% 
  mutate (fvc_z_score = str_extract_all (fvc_bin, "-?\\d+\\.?\\d*")[[1]] %>% 
    as.numeric () %>% 
    mean ()
  ) %>%
  mutate (fev1_fvc_z_score = str_extract_all (fev1_fvc_bin, "-?\\d+\\.?\\d*")[[1]] %>% 
    as.numeric () %>% 
    mean ()
  ) %>%   
  ungroup () %>% 
  select (fvc_z_score, fev1_fvc_z_score, probability_restriction)
  
figure <- ggplot (data, aes (x = fvc_z_score, y = fev1_fvc_z_score, color = probability_restriction)) +
  geom_point (shape = 15, size = 2) +
  scale_color_gradient (low = "#374E55FF", high = "#DF8F44FF", name = "Probability of Restriction") +
  theme_classic (base_size = 10) +
  theme (text = element_text (family = "Arial")) +
  coord_fixed () +
  xlab ("FVC Z-Score") +
  ylab (expression ("FEV"["1"]~"/FVC Z-Score")) +
  xlim (-5.645, 3.355) +
  ylim (-5.645, 3.355) +
  geom_hline (yintercept = -1.645, linetype = 2) +
  geom_vline (xintercept = -1.645, linetype = 2) +
  annotate ("text", x = 1, y = 2.5, label = "Normal Spirometry", size = unit (2, "pt"), family = "Arial") +
  annotate ("text", x = 1, y = -5, label = "FVC Normal and", size = unit (2, "pt"), family = "Arial") +
  annotate ("text", x = 1, y = -5.25, label = expression ("FEV"["1"]~"/FVC Abnormal"), size = unit (2, "pt"), family = "Arial") +
  annotate ("text", x = -4, y = -5.25, label = "FVC Abnormal and", size = unit (2, "pt"), family = "Arial") +
  annotate ("text", x = -4, y = -5.5, label = expression ("FEV"["1"]~"/FVC Abnormal"), size = unit (2, "pt"), family = "Arial")+
  annotate ("text", x = -4.75, y = 3.35, label = "FVC Abnormal and", size = unit (2, "pt"), family = "Arial") +
  annotate ("text", x = -4.5, y = 3.1, label = expression ("FEV"["1"]~"/FVC Normal"), size = unit (2, "pt"), family = "Arial")  

ggsave ("../figures/figure-1.png", dpi = 1200, width = 20, height = 10, units = "cm")

################################################################################
## Figure 2
################################################################################

# Figure 2a

mrns <- pfts %>%
  arrange (date) %>%
  group_by (mrn) %>%
  filter (row_number () == 1) %>%
  filter (fvc_z_score >= -1.645 & fev1_fvc_z_score >= -1.645 & tlc_z_score < -1.645) %>%
  pull (mrn)

data_1 <- pfts %>%
  filter (mrn %in% mrns) %>%
  arrange (date) %>%
  group_by (mrn) %>%
  filter (row_number () == 1) %>%
  mutate (test = 1) %>%
  mutate (interpretation = case_when (
    fev1_z_score < -1.645 & fvc_z_score < -1.645 & fev1_fvc_z_score >= -1.645 &
      tlc_z_score >= -1.645 ~ "Non-Specific",
    fev1_fvc_z_score < -1.645 & tlc_z_score >= -1.645 ~ "Obstructive",
    fev1_fvc_z_score >= -1.645 & fvc_z_score >= -1.645 &
      tlc_z_score < -1.645 ~ "Restrictive with Normal Spirometry",
    fev1_fvc_z_score >= -1.645 & fvc_z_score < -1.645 &
      tlc_z_score < -1.645 ~ "Restrictive with Abnormal Spirometry",
    fev1_fvc_z_score < -1.645 & tlc_z_score < -1.645 ~ "Mixed",
    TRUE ~ "Normal")
  ) %>%
  select (mrn, test, interpretation)

data_2 <- pfts %>%
  filter (mrn %in% mrns) %>%
  arrange (date) %>%
  group_by (mrn) %>%
  filter (row_number () == 2) %>%
  mutate (test = 2) %>%
  mutate (interpretation = case_when (
    fev1_z_score < -1.645 & fvc_z_score < -1.645 & fev1_fvc_z_score >= -1.645 &
      tlc_z_score >= -1.645 ~ "Non-Specific",
    fev1_fvc_z_score < -1.645 & tlc_z_score >= -1.645 ~ "Obstructive",
    fev1_fvc_z_score >= -1.645 & fvc_z_score >= -1.645 &
      tlc_z_score < -1.645 ~ "Restrictive with Normal Spirometry",
    fev1_fvc_z_score >= -1.645 & fvc_z_score < -1.645 &
      tlc_z_score < -1.645 ~ "Restrictive with Abnormal Spirometry",
    fev1_fvc_z_score < -1.645 & tlc_z_score < -1.645 ~ "Mixed",
    TRUE ~ "Normal")
  ) %>%
  select (mrn, test, interpretation)

data_3 <- pfts %>%
  filter (mrn %in% mrns) %>%
  arrange (date) %>%
  group_by (mrn) %>%
  filter (row_number () == 3) %>%
  mutate (test = 3) %>%
  mutate (interpretation = case_when (
    fev1_z_score < -1.645 & fvc_z_score < -1.645 & fev1_fvc_z_score >= -1.645 &
      tlc_z_score >= -1.645 ~ "Non-Specific",
    fev1_fvc_z_score < -1.645 & tlc_z_score >= -1.645 ~ "Obstructive",
    fev1_fvc_z_score >= -1.645 & fvc_z_score >= -1.645 &
      tlc_z_score < -1.645 ~ "Restrictive with Normal Spirometry",
    fev1_fvc_z_score >= -1.645 & fvc_z_score < -1.645 &
      tlc_z_score < -1.645 ~ "Restrictive with Abnormal Spirometry",
    fev1_fvc_z_score < -1.645 & tlc_z_score < -1.645 ~ "Mixed",
    TRUE ~ "Normal")
  ) %>%
  select (mrn, test, interpretation)

data_4 <- pfts %>%
  filter (mrn %in% mrns) %>%
  arrange (date) %>%
  group_by (mrn) %>%
  filter (row_number () == 4) %>%
  mutate (test = 4) %>%
  mutate (interpretation = case_when (
    fev1_z_score < -1.645 & fvc_z_score < -1.645 & fev1_fvc_z_score >= -1.645 &
      tlc_z_score >= -1.645 ~ "Non-Specific",
    fev1_fvc_z_score < -1.645 & tlc_z_score >= -1.645 ~ "Obstructive",
    fev1_fvc_z_score >= -1.645 & fvc_z_score >= -1.645 &
      tlc_z_score < -1.645 ~ "Restrictive with Normal Spirometry",
    fev1_fvc_z_score >= -1.645 & fvc_z_score < -1.645 &
      tlc_z_score < -1.645 ~ "Restrictive with Abnormal Spirometry",
    fev1_fvc_z_score < -1.645 & tlc_z_score < -1.645 ~ "Mixed",
    TRUE ~ "Normal")
  ) %>%
  select (mrn, test, interpretation)

data_5 <- pfts %>%
  filter (mrn %in% mrns) %>%
  arrange (date) %>%
  group_by (mrn) %>%
  filter (row_number () == 5) %>%
  mutate (test = 5) %>%
  mutate (interpretation = case_when (
    fev1_z_score < -1.645 & fvc_z_score < -1.645 & fev1_fvc_z_score >= -1.645 &
      tlc_z_score >= -1.645 ~ "Non-Specific",
    fev1_fvc_z_score < -1.645 & tlc_z_score >= -1.645 ~ "Obstructive",
    fev1_fvc_z_score >= -1.645 & fvc_z_score >= -1.645 &
      tlc_z_score < -1.645 ~ "Restrictive with Normal Spirometry",
    fev1_fvc_z_score >= -1.645 & fvc_z_score < -1.645 &
      tlc_z_score < -1.645 ~ "Restrictive with Abnormal Spirometry",
    fev1_fvc_z_score < -1.645 & tlc_z_score < -1.645 ~ "Mixed",
    TRUE ~ "Normal")
  ) %>%
  select (mrn, test, interpretation)

data <- rbind (
  data_1,
  data_2,
  data_3,
  data_4,
  data_5
)

data <- data %>%
  mutate (interpretation = as.factor (interpretation)) %>%
  mutate (interpretation = fct_relevel (interpretation,
    "Normal",
    "Non-Specific",
    "Obstructive",
    "Restrictive with Normal Spirometry",
    "Restrictive with Abnormal Spirometry",
    "Mixed")
  )

figure_3a <- ggplot (data,
  aes (x = test, stratum = interpretation, alluvium = mrn,
    label = interpretation, fill = interpretation)) +
  #geom_alluvium (fill = "darkgrey", na.rm = TRUE) +
  geom_flow (alpha = 0.5) +
  geom_stratum () +
  theme_classic (base_size = 10) +
  theme (text = element_text (family = "Arial")) +
  scale_x_continuous (
    name = "Pulmonary Function Test",
    breaks = c (1, 2, 3, 4, 5),
    labels = c ("First", "Second", "Third", "Fourth", "Fifth")
  ) +
  scale_y_continuous (
    name = "Number of Pulmonary Function Tests"
  ) +
  scale_color_manual (
    name = "Interpretation",
    breaks = c (
      "Normal",
      "Non-Specific",
      "Obstructive",
      "Restrictive with Normal Spirometry",
      "Restrictive with Abnormal Spirometry",
      "Mixed"
    ),
    labels = c (
      "Normal",
      "Non-Specific",
      "Obstructive",
      "Restrictive with Normal Spirometry",
      "Restrictive with Abnormal Spirometry",
      "Mixed"
    ),
    values = c (
      "#79AF97FF",
      "#89796BFF",
      "#DF8F44FF",
      "#B24745FF",
      "#374E55FF",
      "#6A6599FF"
    )
  ) +
  scale_fill_manual (
    name = "Interpretation",
    breaks = c (
      "Normal",
      "Non-Specific",
      "Obstructive",
      "Restrictive with Normal Spirometry",
      "Restrictive with Abnormal Spirometry",
      "Mixed"
    ),
    labels = c (
      "Normal",
      "Non-Specific",
      "Obstructive",
      "Restrictive with Normal Spirometry",
      "Restrictive with Abnormal Spirometry",
      "Mixed"
    ),
    values = c (
      "#79AF97FF",
      "#89796BFF",
      "#DF8F44FF",
      "#B24745FF",
      "#374E55FF",
      "#6A6599FF"
    )
  )

ggsave ("../figures/figure-2a.png", dpi = 1200, height = 15, width = 20, units = "cm")

#Figure 2b

data_1 <- pfts %>%
  group_by (mrn) %>%
  select (
    test_1 = test,
    date_1 = date,
    mrn,
    age,
    sex,
    race,
    fev1_z_score,
    fvc_z_score,
    fev1_fvc_z_score,
    interpretation_1 = interpretation
  ) %>%
  mutate (rank = row_number ()) %>%
  mutate (total = n ()) %>%
  mutate (test_2 = case_when (
    rank < total ~ test_1 + 1)
  )

data_2 <- pfts %>%
  select (
    test_2 = test,
    date_2 = date,
    interpretation_2 = interpretation
  )

data <- data_1 %>%
  left_join (data_2, by = "test_2") %>%
  mutate (time = difftime (date_2, date_1, units = "days")) %>%
  mutate (time = as.double (time)) %>%
  select (-rank) %>%
  select (-total) %>%
  filter (is.na (interpretation_2) == 0) %>%
  mutate (interpretation_2 = as.factor (interpretation_2))

table <- matrix (data = NA, nrow = 6, ncol = 6)

# First test normal

table [1,1] <- paste (format (round (100 *
  nrow (filter (data,
    interpretation_1 == "Normal" &
    interpretation_2 == "Normal")) /
  nrow (filter (data, interpretation_1 == "Normal")),
  digits = 1), nsmall = 1), "%", sep = ""
)

table [1,2] <- paste (format (round (100 *
  nrow (filter (data,
    interpretation_1 == "Normal" &
    interpretation_2 == "Non-Specific")) /
  nrow (filter (data, interpretation_1 == "Normal")),
  digits = 1), nsmall = 1), "%", sep = ""
)

table [1,3] <- paste (format (round (100 *
  nrow (filter (data,
    interpretation_1 == "Normal" &
    interpretation_2 == "Obstructive")) /
  nrow (filter (data, interpretation_1 == "Normal")),
  digits = 1), nsmall = 1), "%", sep = ""
)

table [1,4] <- paste (format (round (100 *
  nrow (filter (data,
    interpretation_1 == "Normal" &
    interpretation_2 == "Restrictive with Normal Spirometry")) /
  nrow (filter (data, interpretation_1 == "Normal")),
  digits = 1), nsmall = 1), "%", sep = ""
)

table [1,5] <- paste (format (round (100 *
  nrow (filter (data,
    interpretation_1 == "Normal" &
    interpretation_2 == "Restrictive with Abnormal Spirometry")) /
  nrow (filter (data, interpretation_1 == "Normal")),
  digits = 1), nsmall = 1), "%", sep = ""
)

table [1,6] <- paste (format (round (100 *
  nrow (filter (data,
    interpretation_1 == "Normal" &
    interpretation_2 == "Mixed")) /
  nrow (filter (data, interpretation_1 == "Normal")),
  digits = 1), nsmall = 1), "%", sep = ""
)

# First test non-specific

table [2,1] <- paste (format (round (100 *
  nrow (filter (data,
    interpretation_1 == "Non-Specific" &
    interpretation_2 == "Normal")) /
  nrow (filter (data, interpretation_1 == "Non-Specific")),
  digits = 1), nsmall = 1), "%", sep = ""
)

table [2,2] <- paste (format (round (100 *
  nrow (filter (data,
    interpretation_1 == "Non-Specific" &
    interpretation_2 == "Non-Specific")) /
  nrow (filter (data, interpretation_1 == "Non-Specific")),
  digits = 1), nsmall = 1), "%", sep = ""
)

table [2,3] <- paste (format (round (100 *
  nrow (filter (data,
    interpretation_1 == "Non-Specific" &
    interpretation_2 == "Obstructive")) /
  nrow (filter (data, interpretation_1 == "Non-Specific")),
  digits = 1), nsmall = 1), "%", sep = ""
)

table [2,4] <- paste (format (round (100 *
  nrow (filter (data,
    interpretation_1 == "Non-Specific" &
    interpretation_2 == "Restrictive with Normal Spirometry")) /
  nrow (filter (data, interpretation_1 == "Non-Specific")),
  digits = 1), nsmall = 1), "%", sep = ""
)

table [2,5] <- paste (format (round (100 *
  nrow (filter (data,
    interpretation_1 == "Non-Specific" &
    interpretation_2 == "Restrictive with Abnormal Spirometry")) /
  nrow (filter (data, interpretation_1 == "Non-Specific")),
  digits = 1), nsmall = 1), "%", sep = ""
)

table [2,6] <- paste (format (round (100 *
  nrow (filter (data,
    interpretation_1 == "Non-Specific" &
    interpretation_2 == "Mixed")) /
  nrow (filter (data, interpretation_1 == "Non-Specific")),
  digits = 1), nsmall = 1), "%", sep = ""
)

# First test obstructive

table [3,1] <- paste (format (round (100 *
  nrow (filter (data,
    interpretation_1 == "Obstructive" &
    interpretation_2 == "Normal")) /
  nrow (filter (data, interpretation_1 == "Obstructive")),
  digits = 1), nsmall = 1), "%", sep = ""
)

table [3,2] <- paste (format (round (100 *
  nrow (filter (data,
    interpretation_1 == "Obstructive" &
    interpretation_2 == "Non-Specific")) /
  nrow (filter (data, interpretation_1 == "Obstructive")),
  digits = 1), nsmall = 1), "%", sep = ""
)

table [3,3] <- paste (format (round (100 *
  nrow (filter (data,
    interpretation_1 == "Obstructive" &
    interpretation_2 == "Obstructive")) /
  nrow (filter (data, interpretation_1 == "Obstructive")),
  digits = 1), nsmall = 1), "%", sep = ""
)

table [3,4] <- paste (format (round (100 *
  nrow (filter (data,
    interpretation_1 == "Obstructive" &
    interpretation_2 == "Restrictive with Normal Spirometry")) /
  nrow (filter (data, interpretation_1 == "Obstructive")),
  digits = 1), nsmall = 1), "%", sep = ""
)

table [3,5] <- paste (format (round (100 *
  nrow (filter (data,
    interpretation_1 == "Obstructive" &
    interpretation_2 == "Restrictive with Abnormal Spirometry")) /
  nrow (filter (data, interpretation_1 == "Obstructive")),
  digits = 1), nsmall = 1), "%", sep = ""
)

table [3,6] <- paste (format (round (100 *
  nrow (filter (data,
    interpretation_1 == "Obstructive" &
    interpretation_2 == "Mixed")) /
  nrow (filter (data, interpretation_1 == "Obstructive")),
  digits = 1), nsmall = 1), "%", sep = ""
)

# First test restrictive with normal spirometry

table [4,1] <- paste (format (round (100 *
  nrow (filter (data,
    interpretation_1 == "Restrictive with Normal Spirometry" &
    interpretation_2 == "Normal")) /
  nrow (filter (data, interpretation_1 == "Restrictive with Normal Spirometry")),
  digits = 1), nsmall = 1), "%", sep = ""
)

table [4,2] <- paste (format (round (100 *
  nrow (filter (data,
    interpretation_1 == "Restrictive with Normal Spirometry" &
    interpretation_2 == "Non-Specific")) /
  nrow (filter (data, interpretation_1 == "Restrictive with Normal Spirometry")),
  digits = 1), nsmall = 1), "%", sep = ""
)

table [4,3] <- paste (format (round (100 *
  nrow (filter (data,
    interpretation_1 == "Restrictive with Normal Spirometry" &
    interpretation_2 == "Obstructive")) /
  nrow (filter (data, interpretation_1 == "Restrictive with Normal Spirometry")),
  digits = 1), nsmall = 1), "%", sep = ""
)

table [4,4] <- paste (format (round (100 *
  nrow (filter (data,
    interpretation_1 == "Restrictive with Normal Spirometry" &
    interpretation_2 == "Restrictive with Normal Spirometry")) /
  nrow (filter (data, interpretation_1 == "Restrictive with Normal Spirometry")),
  digits = 1), nsmall = 1), "%", sep = ""
)

table [4,5] <- paste (format (round (100 *
  nrow (filter (data,
    interpretation_1 == "Restrictive with Normal Spirometry" &
    interpretation_2 == "Restrictive with Abnormal Spirometry")) /
  nrow (filter (data, interpretation_1 == "Restrictive with Normal Spirometry")),
  digits = 1), nsmall = 1), "%", sep = ""
)

table [4,6] <- paste (format (round (100 *
  nrow (filter (data,
    interpretation_1 == "Restrictive with Normal Spirometry" &
    interpretation_2 == "Mixed")) /
  nrow (filter (data, interpretation_1 == "Restrictive with Normal Spirometry")),
  digits = 1), nsmall = 1), "%", sep = ""
)

# First test restrictive with abnormal spirometry

table [5,1] <- paste (format (round (100 *
  nrow (filter (data,
    interpretation_1 == "Restrictive with Abnormal Spirometry" &
    interpretation_2 == "Normal")) /
  nrow (filter (data, interpretation_1 == "Restrictive with Abnormal Spirometry")),
  digits = 1), nsmall = 1), "%", sep = ""
)

table [5,2] <- paste (format (round (100 *
  nrow (filter (data,
    interpretation_1 == "Restrictive with Abnormal Spirometry" &
    interpretation_2 == "Non-Specific")) /
  nrow (filter (data, interpretation_1 == "Restrictive with Abnormal Spirometry")),
  digits = 1), nsmall = 1), "%", sep = ""
)

table [5,3] <- paste (format (round (100 *
  nrow (filter (data,
    interpretation_1 == "Restrictive with Abnormal Spirometry" &
    interpretation_2 == "Obstructive")) /
  nrow (filter (data, interpretation_1 == "Restrictive with Abnormal Spirometry")),
  digits = 1), nsmall = 1), "%", sep = ""
)

table [5,4] <- paste (format (round (100 *
  nrow (filter (data,
    interpretation_1 == "Restrictive with Abnormal Spirometry" &
    interpretation_2 == "Restrictive with Normal Spirometry")) /
  nrow (filter (data, interpretation_1 == "Restrictive with Abnormal Spirometry")),
  digits = 1), nsmall = 1), "%", sep = ""
)

table [5,5] <- paste (format (round (100 *
  nrow (filter (data,
    interpretation_1 == "Restrictive with Abnormal Spirometry" &
    interpretation_2 == "Restrictive with Abnormal Spirometry")) /
  nrow (filter (data, interpretation_1 == "Restrictive with Abnormal Spirometry")),
  digits = 1), nsmall = 1), "%", sep = ""
)

table [5,6] <- paste (format (round (100 *
  nrow (filter (data,
    interpretation_1 == "Restrictive with Abnormal Spirometry" &
    interpretation_2 == "Mixed")) /
  nrow (filter (data, interpretation_1 == "Restrictive with Abnormal Spirometry")),
  digits = 1), nsmall = 1), "%", sep = ""
)

# First test mixed

table [6,1] <- paste (format (round (100 *
  nrow (filter (data,
    interpretation_1 == "Mixed" &
    interpretation_2 == "Normal")) /
  nrow (filter (data, interpretation_1 == "Mixed")),
  digits = 1), nsmall = 1), "%", sep = ""
)

table [6,2] <- paste (format (round (100 *
  nrow (filter (data,
    interpretation_1 == "Mixed" &
    interpretation_2 == "Non-Specific")) /
  nrow (filter (data, interpretation_1 == "Mixed")),
  digits = 1), nsmall = 1), "%", sep = ""
)

table [6,3] <- paste (format (round (100 *
  nrow (filter (data,
    interpretation_1 == "Mixed" &
    interpretation_2 == "Obstructive")) /
  nrow (filter (data, interpretation_1 == "Mixed")),
  digits = 1), nsmall = 1), "%", sep = ""
)

table [6,4] <- paste (format (round (100 *
  nrow (filter (data,
    interpretation_1 == "Mixed" &
    interpretation_2 == "Restrictive with Normal Spirometry")) /
  nrow (filter (data, interpretation_1 == "Mixed")),
  digits = 1), nsmall = 1), "%", sep = ""
)

table [6,5] <- paste (format (round (100 *
  nrow (filter (data,
    interpretation_1 == "Mixed" &
    interpretation_2 == "Restrictive with Abnormal Spirometry")) /
  nrow (filter (data, interpretation_1 == "Mixed")),
  digits = 1), nsmall = 1), "%", sep = ""
)

table [6,6] <- paste (format (round (100 *
  nrow (filter (data,
    interpretation_1 == "Mixed" &
    interpretation_2 == "Mixed")) /
  nrow (filter (data, interpretation_1 == "Mixed")),
  digits = 1), nsmall = 1), "%", sep = ""
)

figure <- ggplot () +
  theme_void () +
  theme (plot.margin = unit (c (5, 5, 5, 5), "cm")) +
  geom_segment (aes (x = 0.005, y = 0, xend = 0.005, yend = 6)) +
  geom_segment (aes (x = 0, y = 0, xend = 6, yend = 0)) +
  geom_segment (aes (x = 6, y = 6, xend = 0, yend = 6)) +
  geom_segment (aes (x = 5.995, y = 0, xend = 5.995, yend = 6)) +
  geom_segment (aes (x = 1, y = 0, xend = 1, yend = 6)) +
  geom_segment (aes (x = 2, y = 0, xend = 2, yend = 6)) +
  geom_segment (aes (x = 3, y = 0, xend = 3, yend = 6)) +
  geom_segment (aes (x = 4, y = 0, xend = 4, yend = 6)) +
  geom_segment (aes (x = 5, y = 0, xend = 5, yend = 6)) +
  geom_segment (aes (x = 0, y = 1, xend = 6, yend = 1)) +
  geom_segment (aes (x = 0, y = 2, xend = 6, yend = 2)) +
  geom_segment (aes (x = 0, y = 3, xend = 6, yend = 3)) +
  geom_segment (aes (x = 0, y = 4, xend = 6, yend = 4)) +
  geom_segment (aes (x = 0, y = 5, xend = 6, yend = 5)) +
  annotate ("text", x = -3.5, y = 3, label = "First Pulmonary Function Test", size = 4, family = "Helvetica", fontface = 2, angle = 90) +
  annotate ("text", x = 3, y = 8.5, label = "Second Pulmonary Function Test", size = 4, family = "Helvetica", fontface = 2) +
  annotate ("text", x = -0.65, y = 5.5, label = "Normal", size = 3, family = "Helvetica") +
  annotate ("text", x = -0.95, y = 4.5, label = "Non-Specific", size = 3, family = "Helvetica") +
  annotate ("text", x = -0.85, y = 3.5, label = "Obstructive", size = 3, family = "Helvetica") +
  annotate ("text", x = -1.15, y = 2.625, label = "Restrictive with", size = 3, family = "Helvetica") +
  annotate ("text", x = -1.35, y = 2.375, label = "Normal Spirometry", size = 3, family = "Helvetica") +
  annotate ("text", x = -1.15, y = 1.625, label = "Restrictive with", size = 3, family = "Helvetica") +
  annotate ("text", x = -1.5, y = 1.4, label = "Abnormal Spirometry", size = 3, family = "Helvetica") +
  annotate ("text", x = -0.55, y = 0.5, label = "Mixed", size = 3, family = "Helvetica") +
  annotate ("text", x = 0.65, y = 6.5, label = "Normal", size = 3, family = "Helvetica", angle = 45) +
  annotate ("text", x = 1.85, y = 6.7, label = "Non-Specific", size = 3, family = "Helvetica", angle = 45) +
  annotate ("text", x = 2.85, y = 6.7, label = "Obstructive", size = 3, family = "Helvetica", angle = 45) +
  annotate ("text", x = 3.8, y = 6.75, label = "Restrictive with", size = 3, family = "Helvetica", angle = 45) +
  annotate ("text", x = 4.5, y = 7.0, label = "Normal Spirometry", size = 3, family = "Helvetica", angle = 45) +
  annotate ("text", x = 4.9, y = 6.75, label = "Restrictive with", size = 3, family = "Helvetica", angle = 45) +
  annotate ("text", x = 5.7, y = 7.1, label = "Abnormal Spirometry", size = 3, family = "Helvetica", angle = 45) +
  annotate ("text", x = 5.65, y = 6.5, label = "Mixed", size = 3, family = "Helvetica", angle = 45) +
  annotate ("text", x = 0.5, y = 5.5, label = table[[1,1]], size = 3, family = "Helvetica", fontface = 2) +
  annotate ("text", x = 0.5, y = 4.5, label = table[[2,1]], size = 3, family = "Helvetica") +
  annotate ("text", x = 0.5, y = 3.5, label = table[[3,1]], size = 3, family = "Helvetica") +
  annotate ("text", x = 0.5, y = 2.5, label = table[[4,1]], size = 3, family = "Helvetica") +
  annotate ("text", x = 0.5, y = 1.5, label = table[[5,1]], size = 3, family = "Helvetica") +
  annotate ("text", x = 0.5, y = 0.5, label = table[[6,1]], size = 3, family = "Helvetica") +
  annotate ("text", x = 1.5, y = 5.5, label = table[[1,2]], size = 3, family = "Helvetica") +
  annotate ("text", x = 1.5, y = 4.5, label = table[[2,2]], size = 3, family = "Helvetica", fontface = 2) +
  annotate ("text", x = 1.5, y = 3.5, label = table[[3,2]], size = 3, family = "Helvetica") +
  annotate ("text", x = 1.5, y = 2.5, label = table[[4,2]], size = 3, family = "Helvetica") +
  annotate ("text", x = 1.5, y = 1.5, label = table[[5,2]], size = 3, family = "Helvetica") +
  annotate ("text", x = 1.5, y = 0.5, label = table[[6,2]], size = 3, family = "Helvetica") +
  annotate ("text", x = 2.5, y = 5.5, label = table[[1,3]], size = 3, family = "Helvetica") +
  annotate ("text", x = 2.5, y = 4.5, label = table[[2,3]], size = 3, family = "Helvetica") +
  annotate ("text", x = 2.5, y = 3.5, label = table[[3,3]], size = 3, family = "Helvetica", fontface = 2) +
  annotate ("text", x = 2.5, y = 2.5, label = table[[4,3]], size = 3, family = "Helvetica") +
  annotate ("text", x = 2.5, y = 1.5, label = table[[5,3]], size = 3, family = "Helvetica") +
  annotate ("text", x = 2.5, y = 0.5, label = table[[6,3]], size = 3, family = "Helvetica") +
  annotate ("text", x = 3.5, y = 5.5, label = table[[1,4]], size = 3, family = "Helvetica") +
  annotate ("text", x = 3.5, y = 4.5, label = table[[2,4]], size = 3, family = "Helvetica") +
  annotate ("text", x = 3.5, y = 3.5, label = table[[3,4]], size = 3, family = "Helvetica") +
  annotate ("text", x = 3.5, y = 2.5, label = table[[4,4]], size = 3, family = "Helvetica", fontface = 2) +
  annotate ("text", x = 3.5, y = 1.5, label = table[[5,4]], size = 3, family = "Helvetica") +
  annotate ("text", x = 3.5, y = 0.5, label = table[[6,4]], size = 3, family = "Helvetica") +
  annotate ("text", x = 4.5, y = 5.5, label = table[[1,5]], size = 3, family = "Helvetica") +
  annotate ("text", x = 4.5, y = 4.5, label = table[[2,5]], size = 3, family = "Helvetica") +
  annotate ("text", x = 4.5, y = 3.5, label = table[[3,5]], size = 3, family = "Helvetica") +
  annotate ("text", x = 4.5, y = 2.5, label = table[[4,5]], size = 3, family = "Helvetica") +
  annotate ("text", x = 4.5, y = 1.5, label = table[[5,5]], size = 3, family = "Helvetica", fontface = 2) +
  annotate ("text", x = 4.5, y = 0.5, label = table[[6,5]], size = 3, family = "Helvetica") +
  annotate ("text", x = 5.5, y = 5.5, label = table[[1,6]], size = 3, family = "Helvetica") +
  annotate ("text", x = 5.5, y = 4.5, label = table[[2,6]], size = 3, family = "Helvetica") +
  annotate ("text", x = 5.5, y = 3.5, label = table[[3,6]], size = 3, family = "Helvetica") +
  annotate ("text", x = 5.5, y = 2.5, label = table[[4,6]], size = 3, family = "Helvetica") +
  annotate ("text", x = 5.5, y = 1.5, label = table[[5,6]], size = 3, family = "Helvetica") +
  annotate ("text", x = 5.5, y = 0.5, label = table[[6,6]], size = 3, family = "Helvetica", fontface = 2) +
  coord_cartesian(clip = "off")

ggsave ("../figures/figure-2b.png", dpi = 1200, width = 20, height = 20, units = "cm")


################################################################################
## e-Figure 1 - Flow Diagram
################################################################################

pfts_all %>% 
  nrow ()

pfts_all %>% 
  filter (age > 0 & (sex == 1 | sex == 2)) %>%
  nrow ()

pfts_all %>% 
  filter (age > 0 & (sex == 1 | sex == 2)) %>%
  filter (height > 50 & weight > 25) %>% 
  nrow ()

pfts_all %>% 
  filter (age > 0 & (sex == 1 | sex == 2)) %>%
  filter (height > 50 & weight > 25) %>% 
  filter (fev1 > 0 & fvc > 0) %>%  
  nrow ()

pfts_all %>% 
  filter (age > 0 & (sex == 1 | sex == 2)) %>%
  filter (height > 50 & weight > 25) %>% 
  filter (fev1 > 0 & fvc > 0) %>% 
  filter (tlc > 0 & tlc < 20) %>%
  nrow ()

pfts_all %>%
  filter (age > 0 & (sex == 1 | sex == 2)) %>% 
  filter (height > 50 & weight > 25) %>% 
  filter (fev1 > 0 & fvc > 0) %>%
  filter (tlc > 0 & tlc < 20) %>%
  filter (age >= 18 & age <= 80) %>% 
  nrow ()

################################################################################
## e-Figure 2
################################################################################

data <- pfts %>%
  select (tlc_z_score, fvc_z_score) %>% 
  mutate (interpretation = case_when (
    fvc_z_score < -1.645 ~ "FVC Abnormal",
    TRUE ~ "FVC Normal")
  ) %>% 
  mutate (interpretation = as.factor (interpretation))

figure <- ggplot (data, aes (x = tlc_z_score, fill = interpretation)) +
  geom_histogram (bins = 30, alpha = 0.7, position = "identity", color = "black") +
  xlab ("TLC Z-Score") +
  ylab ("Number of PFTs") +
  xlim (-7.5, 5) +
  geom_segment (aes(x = -1.645, xend = -1.645, y = 0, yend = 11000), linetype = "dashed") +
  theme_classic (base_size = 10) +
  scale_fill_manual (
    name = "",  # Custom legend title
    values = c("FVC Normal" = "#374E55FF", "FVC Abnormal" = "#DF8F44FF"),
    labels = c("FVC Normal" = "FVC Normal", "FVC Abnormal" = "FVC Abnormal")
  ) +
  theme (text = element_text (family = "Arial")) +
  annotate ("text", x = -2.75, y = 10500, label = "Z-score = -1.645", size = unit (3, "pt"), family = "Arial")  
  

ggsave ("../figures/e-figure-2.png", dpi = 1200, width = 20, height = 10, units = "cm")

################################################################################
## e-Figure 3
################################################################################

# e-Figure 3a

mrns <- pfts %>%
  arrange (date) %>%
  group_by (mrn) %>%
  filter (row_number () == 1) %>%
  filter (fvc_z_score_2012 >= -1.645 & fev1_fvc_z_score_2012 >= -1.645 & tlc_z_score < -1.645) %>%
  pull (mrn)

data_1 <- pfts %>%
  filter (mrn %in% mrns) %>%
  arrange (date) %>%
  group_by (mrn) %>%
  filter (row_number () == 1) %>%
  mutate (test = 1) %>%
  mutate (interpretation = case_when (
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
  select (mrn, test, interpretation)

data_2 <- pfts %>%
  filter (mrn %in% mrns) %>%
  arrange (date) %>%
  group_by (mrn) %>%
  filter (row_number () == 2) %>%
  mutate (test = 2) %>%
  mutate (interpretation = case_when (
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
  select (mrn, test, interpretation)

data_3 <- pfts %>%
  filter (mrn %in% mrns) %>%
  arrange (date) %>%
  group_by (mrn) %>%
  filter (row_number () == 3) %>%
  mutate (test = 3) %>%
  mutate (interpretation = case_when (
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
  select (mrn, test, interpretation)

data_4 <- pfts %>%
  filter (mrn %in% mrns) %>%
  arrange (date) %>%
  group_by (mrn) %>%
  filter (row_number () == 4) %>%
  mutate (test = 4) %>%
  mutate (interpretation = case_when (
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
  select (mrn, test, interpretation)

data_5 <- pfts %>%
  filter (mrn %in% mrns) %>%
  arrange (date) %>%
  group_by (mrn) %>%
  filter (row_number () == 5) %>%
  mutate (test = 5) %>%
  mutate (interpretation = case_when (
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
  select (mrn, test, interpretation)

data <- rbind (
  data_1,
  data_2,
  data_3,
  data_4,
  data_5
)

data <- data %>%
  mutate (interpretation = as.factor (interpretation)) %>%
  mutate (interpretation = fct_relevel (interpretation,
    "Normal",
    "Non-Specific",
    "Obstructive",
    "Restrictive with Normal Spirometry",
    "Restrictive with Abnormal Spirometry",
    "Mixed")
  )

figure_3a <- ggplot (data,
  aes (x = test, stratum = interpretation, alluvium = mrn,
    label = interpretation, fill = interpretation)) +
  #geom_alluvium (fill = "darkgrey", na.rm = TRUE) +
  geom_flow (alpha = 0.5) +
  geom_stratum () +
  theme_classic (base_size = 10) +
  theme (text = element_text (family = "Arial")) +
  scale_x_continuous (
    name = "Pulmonary Function Test",
    breaks = c (1, 2, 3, 4, 5),
    labels = c ("First", "Second", "Third", "Fourth", "Fifth")
  ) +
  scale_y_continuous (
    name = "Number of Pulmonary Function Tests"
  ) +
  scale_color_manual (
    name = "Interpretation",
    breaks = c (
      "Normal",
      "Non-Specific",
      "Obstructive",
      "Restrictive with Normal Spirometry",
      "Restrictive with Abnormal Spirometry",
      "Mixed"
    ),
    labels = c (
      "Normal",
      "Non-Specific",
      "Obstructive",
      "Restrictive with Normal Spirometry",
      "Restrictive with Abnormal Spirometry",
      "Mixed"
    ),
    values = c (
      "#79AF97FF",
      "#89796BFF",
      "#DF8F44FF",
      "#B24745FF",
      "#374E55FF",
      "#6A6599FF"
    )
  ) +
  scale_fill_manual (
    name = "Interpretation",
    breaks = c (
      "Normal",
      "Non-Specific",
      "Obstructive",
      "Restrictive with Normal Spirometry",
      "Restrictive with Abnormal Spirometry",
      "Mixed"
    ),
    labels = c (
      "Normal",
      "Non-Specific",
      "Obstructive",
      "Restrictive with Normal Spirometry",
      "Restrictive with Abnormal Spirometry",
      "Mixed"
    ),
    values = c (
      "#79AF97FF",
      "#89796BFF",
      "#DF8F44FF",
      "#B24745FF",
      "#374E55FF",
      "#6A6599FF"
    )
  )

ggsave ("../figures/e-figure-3a.png", dpi = 1200, height = 15, width = 20, units = "cm")

#e-Figure 3b

data_1 <- pfts %>%
  group_by (mrn) %>%
  select (
    test_1 = test,
    date_1 = date,
    mrn,
    age,
    sex,
    race,
    fev1_z_score_2012,
    fvc_z_score_2012,
    fev1_fvc_z_score_2012,
    tlc_z_score
  ) %>%
  mutate (interpretation_1 = case_when (
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
  mutate (rank = row_number ()) %>%
  mutate (total = n ()) %>%
  mutate (test_2 = case_when (
    rank < total ~ test_1 + 1)
  )

data_2 <- pfts %>%
  mutate (interpretation_2 = case_when (
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
  select (
    test_2 = test,
    date_2 = date,
    interpretation_2
  )

data <- data_1 %>%
  left_join (data_2, by = "test_2") %>%
  mutate (time = difftime (date_2, date_1, units = "days")) %>%
  mutate (time = as.double (time)) %>%
  select (-rank) %>%
  select (-total) %>%
  filter (is.na (interpretation_2) == 0) %>%
  mutate (interpretation_2 = as.factor (interpretation_2))

table <- matrix (data = NA, nrow = 6, ncol = 6)

# First test normal

table [1,1] <- paste (format (round (100 *
  nrow (filter (data,
    interpretation_1 == "Normal" &
    interpretation_2 == "Normal")) /
  nrow (filter (data, interpretation_1 == "Normal")),
  digits = 1), nsmall = 1), "%", sep = ""
)

table [1,2] <- paste (format (round (100 *
  nrow (filter (data,
    interpretation_1 == "Normal" &
    interpretation_2 == "Non-Specific")) /
  nrow (filter (data, interpretation_1 == "Normal")),
  digits = 1), nsmall = 1), "%", sep = ""
)

table [1,3] <- paste (format (round (100 *
  nrow (filter (data,
    interpretation_1 == "Normal" &
    interpretation_2 == "Obstructive")) /
  nrow (filter (data, interpretation_1 == "Normal")),
  digits = 1), nsmall = 1), "%", sep = ""
)

table [1,4] <- paste (format (round (100 *
  nrow (filter (data,
    interpretation_1 == "Normal" &
    interpretation_2 == "Restrictive with Normal Spirometry")) /
  nrow (filter (data, interpretation_1 == "Normal")),
  digits = 1), nsmall = 1), "%", sep = ""
)

table [1,5] <- paste (format (round (100 *
  nrow (filter (data,
    interpretation_1 == "Normal" &
    interpretation_2 == "Restrictive with Abnormal Spirometry")) /
  nrow (filter (data, interpretation_1 == "Normal")),
  digits = 1), nsmall = 1), "%", sep = ""
)

table [1,6] <- paste (format (round (100 *
  nrow (filter (data,
    interpretation_1 == "Normal" &
    interpretation_2 == "Mixed")) /
  nrow (filter (data, interpretation_1 == "Normal")),
  digits = 1), nsmall = 1), "%", sep = ""
)

# First test non-specific

table [2,1] <- paste (format (round (100 *
  nrow (filter (data,
    interpretation_1 == "Non-Specific" &
    interpretation_2 == "Normal")) /
  nrow (filter (data, interpretation_1 == "Non-Specific")),
  digits = 1), nsmall = 1), "%", sep = ""
)

table [2,2] <- paste (format (round (100 *
  nrow (filter (data,
    interpretation_1 == "Non-Specific" &
    interpretation_2 == "Non-Specific")) /
  nrow (filter (data, interpretation_1 == "Non-Specific")),
  digits = 1), nsmall = 1), "%", sep = ""
)

table [2,3] <- paste (format (round (100 *
  nrow (filter (data,
    interpretation_1 == "Non-Specific" &
    interpretation_2 == "Obstructive")) /
  nrow (filter (data, interpretation_1 == "Non-Specific")),
  digits = 1), nsmall = 1), "%", sep = ""
)

table [2,4] <- paste (format (round (100 *
  nrow (filter (data,
    interpretation_1 == "Non-Specific" &
    interpretation_2 == "Restrictive with Normal Spirometry")) /
  nrow (filter (data, interpretation_1 == "Non-Specific")),
  digits = 1), nsmall = 1), "%", sep = ""
)

table [2,5] <- paste (format (round (100 *
  nrow (filter (data,
    interpretation_1 == "Non-Specific" &
    interpretation_2 == "Restrictive with Abnormal Spirometry")) /
  nrow (filter (data, interpretation_1 == "Non-Specific")),
  digits = 1), nsmall = 1), "%", sep = ""
)

table [2,6] <- paste (format (round (100 *
  nrow (filter (data,
    interpretation_1 == "Non-Specific" &
    interpretation_2 == "Mixed")) /
  nrow (filter (data, interpretation_1 == "Non-Specific")),
  digits = 1), nsmall = 1), "%", sep = ""
)

# First test obstructive

table [3,1] <- paste (format (round (100 *
  nrow (filter (data,
    interpretation_1 == "Obstructive" &
    interpretation_2 == "Normal")) /
  nrow (filter (data, interpretation_1 == "Obstructive")),
  digits = 1), nsmall = 1), "%", sep = ""
)

table [3,2] <- paste (format (round (100 *
  nrow (filter (data,
    interpretation_1 == "Obstructive" &
    interpretation_2 == "Non-Specific")) /
  nrow (filter (data, interpretation_1 == "Obstructive")),
  digits = 1), nsmall = 1), "%", sep = ""
)

table [3,3] <- paste (format (round (100 *
  nrow (filter (data,
    interpretation_1 == "Obstructive" &
    interpretation_2 == "Obstructive")) /
  nrow (filter (data, interpretation_1 == "Obstructive")),
  digits = 1), nsmall = 1), "%", sep = ""
)

table [3,4] <- paste (format (round (100 *
  nrow (filter (data,
    interpretation_1 == "Obstructive" &
    interpretation_2 == "Restrictive with Normal Spirometry")) /
  nrow (filter (data, interpretation_1 == "Obstructive")),
  digits = 1), nsmall = 1), "%", sep = ""
)

table [3,5] <- paste (format (round (100 *
  nrow (filter (data,
    interpretation_1 == "Obstructive" &
    interpretation_2 == "Restrictive with Abnormal Spirometry")) /
  nrow (filter (data, interpretation_1 == "Obstructive")),
  digits = 1), nsmall = 1), "%", sep = ""
)

table [3,6] <- paste (format (round (100 *
  nrow (filter (data,
    interpretation_1 == "Obstructive" &
    interpretation_2 == "Mixed")) /
  nrow (filter (data, interpretation_1 == "Obstructive")),
  digits = 1), nsmall = 1), "%", sep = ""
)

# First test restrictive with normal spirometry

table [4,1] <- paste (format (round (100 *
  nrow (filter (data,
    interpretation_1 == "Restrictive with Normal Spirometry" &
    interpretation_2 == "Normal")) /
  nrow (filter (data, interpretation_1 == "Restrictive with Normal Spirometry")),
  digits = 1), nsmall = 1), "%", sep = ""
)

table [4,2] <- paste (format (round (100 *
  nrow (filter (data,
    interpretation_1 == "Restrictive with Normal Spirometry" &
    interpretation_2 == "Non-Specific")) /
  nrow (filter (data, interpretation_1 == "Restrictive with Normal Spirometry")),
  digits = 1), nsmall = 1), "%", sep = ""
)

table [4,3] <- paste (format (round (100 *
  nrow (filter (data,
    interpretation_1 == "Restrictive with Normal Spirometry" &
    interpretation_2 == "Obstructive")) /
  nrow (filter (data, interpretation_1 == "Restrictive with Normal Spirometry")),
  digits = 1), nsmall = 1), "%", sep = ""
)

table [4,4] <- paste (format (round (100 *
  nrow (filter (data,
    interpretation_1 == "Restrictive with Normal Spirometry" &
    interpretation_2 == "Restrictive with Normal Spirometry")) /
  nrow (filter (data, interpretation_1 == "Restrictive with Normal Spirometry")),
  digits = 1), nsmall = 1), "%", sep = ""
)

table [4,5] <- paste (format (round (100 *
  nrow (filter (data,
    interpretation_1 == "Restrictive with Normal Spirometry" &
    interpretation_2 == "Restrictive with Abnormal Spirometry")) /
  nrow (filter (data, interpretation_1 == "Restrictive with Normal Spirometry")),
  digits = 1), nsmall = 1), "%", sep = ""
)

table [4,6] <- paste (format (round (100 *
  nrow (filter (data,
    interpretation_1 == "Restrictive with Normal Spirometry" &
    interpretation_2 == "Mixed")) /
  nrow (filter (data, interpretation_1 == "Restrictive with Normal Spirometry")),
  digits = 1), nsmall = 1), "%", sep = ""
)

# First test restrictive with abnormal spirometry

table [5,1] <- paste (format (round (100 *
  nrow (filter (data,
    interpretation_1 == "Restrictive with Abnormal Spirometry" &
    interpretation_2 == "Normal")) /
  nrow (filter (data, interpretation_1 == "Restrictive with Abnormal Spirometry")),
  digits = 1), nsmall = 1), "%", sep = ""
)

table [5,2] <- paste (format (round (100 *
  nrow (filter (data,
    interpretation_1 == "Restrictive with Abnormal Spirometry" &
    interpretation_2 == "Non-Specific")) /
  nrow (filter (data, interpretation_1 == "Restrictive with Abnormal Spirometry")),
  digits = 1), nsmall = 1), "%", sep = ""
)

table [5,3] <- paste (format (round (100 *
  nrow (filter (data,
    interpretation_1 == "Restrictive with Abnormal Spirometry" &
    interpretation_2 == "Obstructive")) /
  nrow (filter (data, interpretation_1 == "Restrictive with Abnormal Spirometry")),
  digits = 1), nsmall = 1), "%", sep = ""
)

table [5,4] <- paste (format (round (100 *
  nrow (filter (data,
    interpretation_1 == "Restrictive with Abnormal Spirometry" &
    interpretation_2 == "Restrictive with Normal Spirometry")) /
  nrow (filter (data, interpretation_1 == "Restrictive with Abnormal Spirometry")),
  digits = 1), nsmall = 1), "%", sep = ""
)

table [5,5] <- paste (format (round (100 *
  nrow (filter (data,
    interpretation_1 == "Restrictive with Abnormal Spirometry" &
    interpretation_2 == "Restrictive with Abnormal Spirometry")) /
  nrow (filter (data, interpretation_1 == "Restrictive with Abnormal Spirometry")),
  digits = 1), nsmall = 1), "%", sep = ""
)

table [5,6] <- paste (format (round (100 *
  nrow (filter (data,
    interpretation_1 == "Restrictive with Abnormal Spirometry" &
    interpretation_2 == "Mixed")) /
  nrow (filter (data, interpretation_1 == "Restrictive with Abnormal Spirometry")),
  digits = 1), nsmall = 1), "%", sep = ""
)

# First test mixed

table [6,1] <- paste (format (round (100 *
  nrow (filter (data,
    interpretation_1 == "Mixed" &
    interpretation_2 == "Normal")) /
  nrow (filter (data, interpretation_1 == "Mixed")),
  digits = 1), nsmall = 1), "%", sep = ""
)

table [6,2] <- paste (format (round (100 *
  nrow (filter (data,
    interpretation_1 == "Mixed" &
    interpretation_2 == "Non-Specific")) /
  nrow (filter (data, interpretation_1 == "Mixed")),
  digits = 1), nsmall = 1), "%", sep = ""
)

table [6,3] <- paste (format (round (100 *
  nrow (filter (data,
    interpretation_1 == "Mixed" &
    interpretation_2 == "Obstructive")) /
  nrow (filter (data, interpretation_1 == "Mixed")),
  digits = 1), nsmall = 1), "%", sep = ""
)

table [6,4] <- paste (format (round (100 *
  nrow (filter (data,
    interpretation_1 == "Mixed" &
    interpretation_2 == "Restrictive with Normal Spirometry")) /
  nrow (filter (data, interpretation_1 == "Mixed")),
  digits = 1), nsmall = 1), "%", sep = ""
)

table [6,5] <- paste (format (round (100 *
  nrow (filter (data,
    interpretation_1 == "Mixed" &
    interpretation_2 == "Restrictive with Abnormal Spirometry")) /
  nrow (filter (data, interpretation_1 == "Mixed")),
  digits = 1), nsmall = 1), "%", sep = ""
)

table [6,6] <- paste (format (round (100 *
  nrow (filter (data,
    interpretation_1 == "Mixed" &
    interpretation_2 == "Mixed")) /
  nrow (filter (data, interpretation_1 == "Mixed")),
  digits = 1), nsmall = 1), "%", sep = ""
)

figure <- ggplot () +
  theme_void () +
  theme (plot.margin = unit (c (5, 5, 5, 5), "cm")) +
  geom_segment (aes (x = 0.005, y = 0, xend = 0.005, yend = 6)) +
  geom_segment (aes (x = 0, y = 0, xend = 6, yend = 0)) +
  geom_segment (aes (x = 6, y = 6, xend = 0, yend = 6)) +
  geom_segment (aes (x = 5.995, y = 0, xend = 5.995, yend = 6)) +
  geom_segment (aes (x = 1, y = 0, xend = 1, yend = 6)) +
  geom_segment (aes (x = 2, y = 0, xend = 2, yend = 6)) +
  geom_segment (aes (x = 3, y = 0, xend = 3, yend = 6)) +
  geom_segment (aes (x = 4, y = 0, xend = 4, yend = 6)) +
  geom_segment (aes (x = 5, y = 0, xend = 5, yend = 6)) +
  geom_segment (aes (x = 0, y = 1, xend = 6, yend = 1)) +
  geom_segment (aes (x = 0, y = 2, xend = 6, yend = 2)) +
  geom_segment (aes (x = 0, y = 3, xend = 6, yend = 3)) +
  geom_segment (aes (x = 0, y = 4, xend = 6, yend = 4)) +
  geom_segment (aes (x = 0, y = 5, xend = 6, yend = 5)) +
  annotate ("text", x = -3.5, y = 3, label = "First Pulmonary Function Test", size = 4, family = "Helvetica", fontface = 2, angle = 90) +
  annotate ("text", x = 3, y = 8.5, label = "Second Pulmonary Function Test", size = 4, family = "Helvetica", fontface = 2) +
  annotate ("text", x = -0.65, y = 5.5, label = "Normal", size = 3, family = "Helvetica") +
  annotate ("text", x = -0.95, y = 4.5, label = "Non-Specific", size = 3, family = "Helvetica") +
  annotate ("text", x = -0.85, y = 3.5, label = "Obstructive", size = 3, family = "Helvetica") +
  annotate ("text", x = -1.15, y = 2.625, label = "Restrictive with", size = 3, family = "Helvetica") +
  annotate ("text", x = -1.35, y = 2.375, label = "Normal Spirometry", size = 3, family = "Helvetica") +
  annotate ("text", x = -1.15, y = 1.625, label = "Restrictive with", size = 3, family = "Helvetica") +
  annotate ("text", x = -1.5, y = 1.4, label = "Abnormal Spirometry", size = 3, family = "Helvetica") +
  annotate ("text", x = -0.55, y = 0.5, label = "Mixed", size = 3, family = "Helvetica") +
  annotate ("text", x = 0.65, y = 6.5, label = "Normal", size = 3, family = "Helvetica", angle = 45) +
  annotate ("text", x = 1.85, y = 6.7, label = "Non-Specific", size = 3, family = "Helvetica", angle = 45) +
  annotate ("text", x = 2.85, y = 6.7, label = "Obstructive", size = 3, family = "Helvetica", angle = 45) +
  annotate ("text", x = 3.8, y = 6.75, label = "Restrictive with", size = 3, family = "Helvetica", angle = 45) +
  annotate ("text", x = 4.5, y = 7.0, label = "Normal Spirometry", size = 3, family = "Helvetica", angle = 45) +
  annotate ("text", x = 4.9, y = 6.75, label = "Restrictive with", size = 3, family = "Helvetica", angle = 45) +
  annotate ("text", x = 5.7, y = 7.1, label = "Abnormal Spirometry", size = 3, family = "Helvetica", angle = 45) +
  annotate ("text", x = 5.65, y = 6.5, label = "Mixed", size = 3, family = "Helvetica", angle = 45) +
  annotate ("text", x = 0.5, y = 5.5, label = table[[1,1]], size = 3, family = "Helvetica", fontface = 2) +
  annotate ("text", x = 0.5, y = 4.5, label = table[[2,1]], size = 3, family = "Helvetica") +
  annotate ("text", x = 0.5, y = 3.5, label = table[[3,1]], size = 3, family = "Helvetica") +
  annotate ("text", x = 0.5, y = 2.5, label = table[[4,1]], size = 3, family = "Helvetica") +
  annotate ("text", x = 0.5, y = 1.5, label = table[[5,1]], size = 3, family = "Helvetica") +
  annotate ("text", x = 0.5, y = 0.5, label = table[[6,1]], size = 3, family = "Helvetica") +
  annotate ("text", x = 1.5, y = 5.5, label = table[[1,2]], size = 3, family = "Helvetica") +
  annotate ("text", x = 1.5, y = 4.5, label = table[[2,2]], size = 3, family = "Helvetica", fontface = 2) +
  annotate ("text", x = 1.5, y = 3.5, label = table[[3,2]], size = 3, family = "Helvetica") +
  annotate ("text", x = 1.5, y = 2.5, label = table[[4,2]], size = 3, family = "Helvetica") +
  annotate ("text", x = 1.5, y = 1.5, label = table[[5,2]], size = 3, family = "Helvetica") +
  annotate ("text", x = 1.5, y = 0.5, label = table[[6,2]], size = 3, family = "Helvetica") +
  annotate ("text", x = 2.5, y = 5.5, label = table[[1,3]], size = 3, family = "Helvetica") +
  annotate ("text", x = 2.5, y = 4.5, label = table[[2,3]], size = 3, family = "Helvetica") +
  annotate ("text", x = 2.5, y = 3.5, label = table[[3,3]], size = 3, family = "Helvetica", fontface = 2) +
  annotate ("text", x = 2.5, y = 2.5, label = table[[4,3]], size = 3, family = "Helvetica") +
  annotate ("text", x = 2.5, y = 1.5, label = table[[5,3]], size = 3, family = "Helvetica") +
  annotate ("text", x = 2.5, y = 0.5, label = table[[6,3]], size = 3, family = "Helvetica") +
  annotate ("text", x = 3.5, y = 5.5, label = table[[1,4]], size = 3, family = "Helvetica") +
  annotate ("text", x = 3.5, y = 4.5, label = table[[2,4]], size = 3, family = "Helvetica") +
  annotate ("text", x = 3.5, y = 3.5, label = table[[3,4]], size = 3, family = "Helvetica") +
  annotate ("text", x = 3.5, y = 2.5, label = table[[4,4]], size = 3, family = "Helvetica", fontface = 2) +
  annotate ("text", x = 3.5, y = 1.5, label = table[[5,4]], size = 3, family = "Helvetica") +
  annotate ("text", x = 3.5, y = 0.5, label = table[[6,4]], size = 3, family = "Helvetica") +
  annotate ("text", x = 4.5, y = 5.5, label = table[[1,5]], size = 3, family = "Helvetica") +
  annotate ("text", x = 4.5, y = 4.5, label = table[[2,5]], size = 3, family = "Helvetica") +
  annotate ("text", x = 4.5, y = 3.5, label = table[[3,5]], size = 3, family = "Helvetica") +
  annotate ("text", x = 4.5, y = 2.5, label = table[[4,5]], size = 3, family = "Helvetica") +
  annotate ("text", x = 4.5, y = 1.5, label = table[[5,5]], size = 3, family = "Helvetica", fontface = 2) +
  annotate ("text", x = 4.5, y = 0.5, label = table[[6,5]], size = 3, family = "Helvetica") +
  annotate ("text", x = 5.5, y = 5.5, label = table[[1,6]], size = 3, family = "Helvetica") +
  annotate ("text", x = 5.5, y = 4.5, label = table[[2,6]], size = 3, family = "Helvetica") +
  annotate ("text", x = 5.5, y = 3.5, label = table[[3,6]], size = 3, family = "Helvetica") +
  annotate ("text", x = 5.5, y = 2.5, label = table[[4,6]], size = 3, family = "Helvetica") +
  annotate ("text", x = 5.5, y = 1.5, label = table[[5,6]], size = 3, family = "Helvetica") +
  annotate ("text", x = 5.5, y = 0.5, label = table[[6,6]], size = 3, family = "Helvetica", fontface = 2) +
  coord_cartesian(clip = "off")

ggsave ("../figures/e-figure-3b.png", dpi = 1200, width = 20, height = 20, units = "cm")
