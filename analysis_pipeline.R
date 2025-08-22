setwd("C:/Users/xingy/OneDrive/2024 Fall/CR4CR/PSM/DIF/HLM")
library(haven)
library(lme4)
library(magrittr)
library(tidyr)
library(haven)
library(dplyr)
library(ordinal)
require(foreign)
require(nnet)
require(ggplot2)
require(reshape2)
library(effectsize)
library(performance)
library(lmerTest)
library(parameters)
library(dplyr)
library(broom.mixed)

data <- read_sav("Fall22_Spr23_HSMS_Proficiencies_long.sav")

data$HS <- as.factor(data$HS)
data <- data %>%
  mutate(HS = case_when(
    HS == "0" ~ "MS",
    HS == "1" ~ "HS"
  ))
data <- data %>%
  group_by(HS) %>%
  mutate(
    WLE_standardized = scale(WLE, center = TRUE, scale = TRUE) %>% as.vector()
  ) %>%
  ungroup()
#######################################Clean Data####################################
## Gender differences
data <- data %>%
  mutate(D05_gender = case_when(
    D05_gender == "a" ~ "Female",
    D05_gender == "b" ~ "Male",
    D05_gender == "c" ~ "Transgender",
    D05_gender == "d" ~ "Non-binary",
    D05_gender == "e" ~ "Prefer not to answer",
    is.na(D05_gender) ~ "Missing",
    TRUE ~ "Other/Unspecified" # All other values that are not the expected gender categories
  ))%>%
  
  # Create a new variable to combine "Prefer not to answer", Missing, and unexpected values
  mutate(D05_gender_combined = case_when(
    D05_gender == "Female" ~ "Female",
    D05_gender == "Male" ~ "Male",
    D05_gender %in% c("Transgender", "Non-binary") ~ "Gender Minority", # Combine smaller groups
    D05_gender %in% c("Prefer not to answer", "Missing", "Other/Unspecified") | TRUE ~ "No Response/Unspecified" # Combine 'Prefer not to answer', 'Missing', unexpected values
  ))

## Hispanic
data <- data %>%
  mutate(D08_hispanic_YN = case_when(
    D08_hispanic_YN == "a" ~ "Yes",
    D08_hispanic_YN == "b" ~ "No",
    D08_hispanic_YN %in% c("Prefer not to answer", "Missing", "Other/Unspecified") | TRUE ~ "No Response/Unspecified"
  ))

data <- data %>%
  mutate(D15_Native_Language = case_when(
    D15_Native_Language == "a" ~ "Yes",
    D15_Native_Language == "b" ~ "No",
    D15_Native_Language %in% c("Prefer not to answer", "Missing", "Other/Unspecified") | TRUE ~ "No Response/Unspecified"
  ))




# Filter the data for Female and Male only
data_filtered <- data %>%
  filter(D05_gender %in% c("Female", "Male"))

# Convert Gender to a factor with only two levels
data_filtered$Gender <- factor(data_filtered$D05_gender, levels = c("Female", "Male"))


data_filtered <- data_filtered %>%
  mutate(Hispanic = factor(
    case_when(
      D08_hispanic_YN == "Yes" ~ "Hispanic",
      D08_hispanic_YN == "No" ~ "Non-Hispanic"
    ),
    levels = c("Hispanic", "Non-Hispanic")
  ))

data_filtered <- data_filtered %>%
  filter(D08_hispanic_YN %in% c("Yes", "No"))


data_filtered <- data_filtered %>%
  mutate(ELL = factor(
    case_when(
      D15_Native_Language == "Yes" ~ "Non-ELL",
      D15_Native_Language == "No" ~ "ELL"
    ),
    levels = c("Non-ELL", "ELL")
  ))

data_filtered <- data_filtered %>%
  filter(D15_Native_Language %in% c("Yes", "No"))


data_filtered%>%
  group_by(Administration) %>%
  summarise(
    count = n(),
    valid_n_proficiency = sum(!is.na(WLE)),
    avg_proficiency = mean(WLE, na.rm = TRUE),
    sd_proficiency = sd(WLE, na.rm = TRUE),
    valid_n_error = sum(!is.na(WLE_error)),
    avg_error = mean(WLE_error, na.rm = TRUE),
    sd_error = sd(WLE_error, na.rm = TRUE)
  )

# Set reference groups
data_filtered <- data_filtered %>%
  mutate(
    Administration = relevel(as.factor(Administration), ref = "1"),  # Assuming "1" is the reference for Pre
    HS = relevel(as.factor(HS), ref = "MS"),  
    Gender = relevel(as.factor(Gender), ref = "Male"),               # Setting "Male" as reference
    Hispanic = relevel(as.factor(Hispanic), ref = "Non-Hispanic"),   # Setting "Non-Hispanic" as reference
    ELL = relevel(as.factor(ELL), ref = "Non-ELL") # 
    #Class = relevel(as.factor(Class), ref = "AP Calculus BC")        # Setting "AP Calculus BC" as reference
  )

# Categorize classes into AP, Non-AP High School, and Middle School using the HS variable
data_filtered <- data_filtered %>%
  mutate(ClassType = case_when(
    grepl("AP", Class, ignore.case = TRUE) ~ "AP",
    HS == "HS" & !grepl("AP", Class, ignore.case = TRUE) ~ "Non-AP HS",
    HS == "MS" ~ "Middle School",
    TRUE ~ "Other"
  ))


# Step 1: Fit the full model using ML
data_filtered$ClassType <- relevel(as.factor(data_filtered$ClassType), ref = "Middle School")

# Centering STEM perception predictors
data_filtered <- data_filtered %>%
  mutate(
    STEMperception1_c = scale(STEMperception1, center = TRUE, scale = FALSE),
    STEMperception2_c = scale(STEMperception2, center = TRUE, scale = FALSE),
    STEMperception3_c = scale(STEMperception3, center = TRUE, scale = FALSE),
    STEMperception4_c = scale(STEMperception4, center = TRUE, scale = FALSE)
  )



# Fit the null model (random intercept only, no predictors)
hlm_null_model <- lmer(WLE ~ 1 + Administration + (1 | RespondentId), data = data_filtered)

# Summary of the model
summary(hlm_null_model)

variance_components <- as.data.frame(VarCorr(hlm_null_model))
between_group_variance <- variance_components[1, "vcov"]
within_group_variance <- attr(VarCorr(hlm_null_model), "sc")^2

# Calculate ICC
icc <- between_group_variance / (between_group_variance + within_group_variance)
icc

hlm_stem_model_full <- lmer(WLE ~ Administration * ClassType + ELL  + 
                              STEMperception1_c * Gender * Hispanic * ClassType + 
                              STEMperception2_c * Gender * Hispanic * ClassType + 
                              STEMperception3_c * Gender * Hispanic * ClassType + 
                              STEMperception4_c * Gender * Hispanic * ClassType + 
                              (1 | RespondentId), data = data_filtered, REML = FALSE)
summary(hlm_stem_model_full)

# Fit a reduced model (only significant interactions)

hlm_stem_model_reduced <- lmer(
  WLE ~ Administration * ClassType + ELL + STEMperception4_c +
    ClassType*STEMperception1_c*Hispanic +
    ClassType*STEMperception1_c*Gender +
    ClassType*STEMperception3_c*Gender +
    ClassType*Hispanic*STEMperception2_c +
    (1 | RespondentId),
  data = data_filtered,
  REML = FALSE
)

summary(hlm_stem_model_reduced)
# Compare models
anova(hlm_stem_model_full, hlm_stem_model_reduced)
variance_components <- as.data.frame(VarCorr(hlm_stem_model_reduced))
between_group_variance <- variance_components[1, "vcov"]
within_group_variance <- attr(VarCorr(hlm_stem_model_reduced), "sc")^2

# Calculate ICC
icc <- between_group_variance / (between_group_variance + within_group_variance)
icc

##### NEW!!! ############################################

# Split the dataset
data_ms <- data_filtered %>% filter(ClassType == "Middle School")
data_hs <- data_filtered %>% 
  filter(ClassType %in% c("AP", "Non-AP HS")) %>%
  droplevels()

levels(data_hs$ClassType)
data_ms <- data_ms %>%
  mutate(across(starts_with("STEMperception"), 
                ~ .x - mean(.x, na.rm = TRUE), 
                .names = "{.col}_c"))

data_hs <- data_hs %>%
  mutate(across(starts_with("STEMperception"), 
                ~ .x - mean(.x, na.rm = TRUE), 
                .names = "{.col}_c"))

###### ---- new ----
hlm_null_model <- lmer(WLE ~ 1 + Administration + (1 | RespondentId), data = data_hs) # 

# Summary of the model
summary(hlm_null_model)

variance_components <- as.data.frame(VarCorr(hlm_null_model))
between_group_variance <- variance_components[1, "vcov"]
within_group_variance <- attr(VarCorr(hlm_null_model), "sc")^2

# Calculate ICC
icc <- between_group_variance / (between_group_variance + within_group_variance)
icc

model_ms_full <- lmer(
  WLE ~ Administration + ELL + 
    STEMperception1_c * Gender * Hispanic + 
    STEMperception2_c * Gender * Hispanic + 
    STEMperception3_c * Gender * Hispanic + 
    STEMperception4_c * Gender * Hispanic + 
    (1 | RespondentId),
  data = data_ms,
  REML = FALSE
)
summary(model_ms_full)

model_ms_reduced <- lmer(
  WLE ~ Administration + Gender + Hispanic + ELL +
    STEMperception1_c + STEMperception2_c + STEMperception3_c + STEMperception4_c +
    (1 | RespondentId),
  data = data_ms,
  REML = FALSE
)
summary(model_ms_reduced)

model_hs_full <- lmer(
  WLE ~ Administration * ClassType + ELL + 
    STEMperception1_c * Gender * Hispanic * ClassType + 
    STEMperception2_c * Gender * Hispanic * ClassType + 
    STEMperception3_c * Gender * Hispanic * ClassType + 
    STEMperception4_c * Gender * Hispanic * ClassType + 
    (1 | RespondentId),
  data = data_hs,
  REML = FALSE
)
summary(model_hs_full)



model_hs_reduced <- lmer(
  WLE ~ Administration + ClassType + ELL +
    STEMperception1_c * Gender +
    STEMperception1_c * Hispanic +
    STEMperception3_c * Gender +
    STEMperception3_c * Hispanic +
    STEMperception2_c + STEMperception4_c +
    (1 | RespondentId),
  data = data_hs,
  REML = FALSE
)
summary(model_hs_reduced)



hs_reduced_std_betas <- standardize_parameters(model_hs_reduced, method = "refit")
ms_reduced_std_betas <- standardize_parameters(model_ms_reduced, method = "refit")
combined_reduced_std_betas <- standardize_parameters(hlm_stem_model_reduced, method = "refit")

# View results
print(hs_reduced_std_betas)
print(ms_reduced_std_betas)
print(combined_reduced_std_betas)



extract_std_with_p <- function(model, method = "refit", digits = 3) {
  
  # Step 1: Extract unstandardized estimates and p-values
  unstd <- tidy(model, effects = "fixed", conf.int = TRUE) %>%
    mutate(term = as.character(term)) %>%
    select(term, estimate, std.error, statistic, p.value) %>%
    mutate(p.value = round(p.value, digits))
  
  # Step 2: Extract standardized coefficients with CI
  std <- standardize_parameters(model, method = method) %>%
    rename(Std_Beta = Std_Coefficient, CI_low = CI_low, CI_high = CI_high) %>%
    mutate(term = as.character(Parameter)) %>%
    select(term, Std_Beta, CI_low, CI_high)
  
  # Step 3: Merge on term
  result <- left_join(unstd, std, by = "term") %>%
    mutate(across(c(estimate, std.error, statistic, Std_Beta, CI_low, CI_high), 
                  ~ round(.x, digits))) %>%
    select(term, estimate, std.error, statistic, p.value, Std_Beta, CI_low, CI_high)
  
  return(result)
}
hs_table <- extract_std_with_p(model_hs_reduced)
ms_table <- extract_std_with_p(model_ms_reduced)
combined_table <- extract_std_with_p(hlm_stem_model_reduced)


hs_table
ms_table
print(combined_table, n=35)

r2(model_ms_reduced)
r2(model_hs_reduced)
r2(hlm_stem_model_reduced)

# ---- Plot: Main Effect of STEM Confidence (STEMperception1) on PSM ----

library(interactions)
# Reorder ClassType factor levels as desired: Middle School, AP, Non-AP HS
data_hs$ClassType <- factor(data_hs$ClassType, 
                                       levels = c("Non-AP HS", "AP"))
interact_plot(model_hs_reduced, 
                                  pred = STEMperception1_c, 
                                  modx = Hispanic, 
                                  #mod2 = ClassType, 
                                  plot.points = F,
                                  interval = TRUE,
                                  int.alpha = 0.5,    # lighter bands
                                  int.width = 0.95) +
  # Change x-axis title
  xlab("STEM Confidence Level") +
  # Change y-axis title
  ylab("WLE Standardized Score") +
  # Improve plot appearance
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    axis.title.x = element_text(face = "bold", size = 14),
    axis.title.y = element_text(face = "bold", size = 14),
    legend.title = element_text(face = "bold", size = 12),
    legend.position = "top"
  ) +
  # Add a customized plot title
  ggtitle("Interaction of STEM Confidence, Hispanic, and Class Type on WLE Score") +
  # Modify colors for better distinction
  scale_color_brewer(palette = "Set1")


# Display the customized interaction plot
print(interaction_plot2_1)
library(ggplot2)
library(dplyr)
library(ggeffects)  # for prediction
library(patchwork)  # optional: to combine plots
# Generate predictions from stratified models
pred_ms_1 <- ggpredict(model_ms_reduced, terms = "STEMperception1_c [all]") %>%
  mutate(School = "Middle School", STEM_perception = "Confidence")
pred_hs_1 <- ggpredict(model_hs_reduced, terms = "STEMperception1_c [all]") %>%
  mutate(School = "High School", STEM_perception = "Confidence")

pred_ms_2 <- ggpredict(model_ms_reduced, terms = "STEMperception2_c [all]") %>%
  mutate(School = "Middle School", STEM_perception = "Interest")
pred_hs_2 <- ggpredict(model_hs_reduced, terms = "STEMperception2_c [all]") %>%
  mutate(School = "High School", STEM_perception = "Interest")

pred_ms_3 <- ggpredict(model_ms_reduced, terms = "STEMperception3_c [all]") %>%
  mutate(School = "Middle School", STEM_perception = "Opportunity")
pred_hs_3 <- ggpredict(model_hs_reduced, terms = "STEMperception3_c [all]") %>%
  mutate(School = "High School", STEM_perception = "Opportunity")

pred_ms_4 <- ggpredict(model_ms_reduced, terms = "STEMperception4_c [all]") %>%
  mutate(School = "Middle School", STEM_perception = "Career")
pred_hs_4 <- ggpredict(model_hs_reduced, terms = "STEMperception4_c [all]") %>%
  mutate(School = "High School", STEM_perception = "Career")

# Combine all
pred_all <- bind_rows(
  pred_ms_1, pred_hs_1,
  pred_ms_2, pred_hs_2,
  pred_ms_3, pred_hs_3,
  pred_ms_4, pred_hs_4
)

# Plot
ggplot(pred_all, aes(x = x, y = predicted, color = School, fill = School)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, color = NA) +
  facet_wrap(~ STEM_perception, scales = "free_x", ncol = 2) +
  labs(
    title = "Predicted Math Proficiency by STEM Perceptions and School Level",
    subtitle = "Stratified Models: Middle vs. High School",
    x = "STEM Perception (z-score)",
    y = "Predicted Problem-Solving Proficiency (WLE)",
    color = "School Level",
    fill = "School Level"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    strip.text = element_text(face = "bold", size = 13),
    legend.position = "bottom"
  )
