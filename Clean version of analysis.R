#libraries ----
library(readxl)
library(writexl)
library(dplyr)
library(interactions)
library(leaps)
library(ggplot2)
library(emmeans)
library(car)
library(report)
library(psych)
library(apaTables)
library("table1")
library(scales)
library(pscl)
library(tidyr)
library(gt)
library(lm.beta)

#loading upd data file with fixed scoring in Home Math and others ----
data_upd <- read_excel("cross_cultural_full.xlsx")

#deleting outliers -----
variables_to_clean <- c( 
  "Number_Identification_Accuracy_", 
  "Highest_Number", 
  "Counting",
  "Give_a_Number_Accuracy", 
  "DC_Total", 
  "NC_Total",
  "early_numeracy_index", 
  "iq_tscore", 
  "HME", 
  "HMA",
  "HMATT",
  "PE_Total_", 
  "PO_Total", 
  "SES_Total_", 
  "Weekly_attendance_days", 
  "Total_attendance_months", 
  "Total_language", 
  "N_of_siblings",
  "pens_raw",
  "bas_raw")

# Function to replace outliers with NA -----
clean_data <- function(df, vars) {
  df %>%
    group_by(GROUP) %>%  # Replace 'group' with your actual grouping variable
    mutate(across(all_of(vars), 
                  ~ ifelse(abs((.-mean(., na.rm = TRUE)) / sd(., na.rm = TRUE)) > 2, NA, .),
                  .names = "{.col}"))  # Replaces outliers with NA
}

# Clean the data ----
data_cleaned <- clean_data(data_upd, variables_to_clean)
write.csv(data_cleaned, "cross_cultural_full_afteroutliers_1.csv", row.names = FALSE)

#summary stats -----

summary_stats <- data_cleaned %>%
  group_by(GROUP) %>%
  summarise(across(where(is.numeric),
                   list(mean = ~ mean(., na.rm = TRUE),
                        sd   = ~ sd(., na.rm = TRUE)),
                   .names = "{.col}_{.fn}"),
            .groups = "drop")



# Moderation analysis after ANCOVA -----

#Non-symbolic task
moderation_DC <-lm(DC_Total ~ GROUP * Age, data = data_cleaned)
slopes<-sim_slopes(moderation_DC, pred = "Age", modx = "GROUP") #we do simple slopes before beta because lm.beta breaks the file and makes it unsuitable for simple slopes
slopes$slopes$p
slopes
moderation_DC<-lm.beta(moderation_DC)
summary(moderation_DC)

#Counting
moderation_Counting <-lm(Counting ~ GROUP * Age, data = data_cleaned)
slopes<-sim_slopes(moderation_Counting, pred = "Age", modx = "GROUP") 
slopes$slopes$p

moderation_Counting<-lm.beta(moderation_Counting)
summary(moderation_Counting)

#Number Identification
moderation_NI <-lm(Number_Identification_Accuracy_ ~ GROUP * Age, data = data_cleaned)
slopes<-sim_slopes(moderation_NI, pred = "Age", modx = "GROUP") 
slopes$slopes$p

moderation_NI<-lm.beta(moderation_NI)
summary(moderation_NI)

#Give_a_Number_Accuracy
moderation_gan <-lm(Give_a_Number_Accuracy ~ GROUP * Age, data = data_cleaned)
slopes<-sim_slopes(moderation_gan, pred = "Age", modx = "GROUP") 
slopes$slopes$p

moderation_gan<-lm.beta(moderation_gan)
summary(moderation_gan)

#pens_raw
moderation_pens <- lm(pens_raw ~ GROUP * Age, data = data_cleaned)
slopes<-sim_slopes(moderation_pens, pred = "Age", modx = "GROUP")
slopes$slopes$p


moderation_pens<-lm.beta(moderation_pens)
summary(moderation_pens)

#bas_raw
moderation_bas <- lm(bas_raw ~ GROUP * Age, data = data_cleaned)
slopes<-sim_slopes(moderation_bas, pred = "Age", modx = "GROUP")
slopes$slopes$p

moderation_bas<-lm.beta(moderation_bas)
summary(moderation_bas)

#NI_zero
moderation_zero <- glm(NI_zero ~ GROUP * Age, data = data_cleaned, family = binomial, na.action = na.exclude)
summary(moderation_zero) #estimates here are log odds, careful
exp(coef(moderation_zero))  # converts log-odds to odds ratios, analogy of betas as to report size effect

slopes<-sim_slopes(moderation_zero, pred = Age, modx = GROUP)
slopes$slopes$p




#Linear regressions ----
# splitting the dataset

UK_data <- subset(data_cleaned, GROUP == 'UK')
SA_data <- subset(data_cleaned, GROUP == 'SouthA')

UK_data$Gender_bin <- as.factor(UK_data$Gender_bin)
SA_data$Gender_bin <- as.factor(SA_data$Gender_bin)


# Number_Identification_Accuracy_ UK -----------

best_subset <- regsubsets(Number_Identification_Accuracy_ ~ Age+ Gender_bin  +  HME + HMA + HMATT + PO_Total + PE_Total_ + Total_language + Weekly_attendance_days + Total_attendance_months + N_of_siblings, data = UK_data, nbest = 1)
summary(best_subset)
reg_summary <- summary(best_subset)
which.max(reg_summary$adjr2)
which.min(reg_summary$bic)
which.min(reg_summary$cp)
best_model_nia_uk <- lm(Number_Identification_Accuracy_ ~ Age + HME + HMATT+PE_Total_+Total_language +Total_attendance_months, data = UK_data)
best_model_nia_uk <- lm.beta(best_model_nia_uk)
summary(best_model_nia_uk)



# Counting -----
best_subset <- regsubsets(Counting ~ Gender_bin + Age + HME + HMA + HMATT + PO_Total + PE_Total_  + Total_language + Weekly_attendance_days + Total_attendance_months + N_of_siblings, data = UK_data, nbest = 1)
summary(best_subset)
reg_summary <- summary(best_subset)
which.max(reg_summary$adjr2)
which.min(reg_summary$bic)
which.min(reg_summary$cp)
best_model_c_uk <- lm(Counting ~ Age+HME+HMATT+PO_Total+PE_Total_+N_of_siblings, data = UK_data)
best_model_c_uk <- lm.beta(best_model_c_uk)
summary(best_model_c_uk)

#NI_zero ----
best_subset <- regsubsets(NI_zero ~  Gender_bin + Age + HME + HMA + HMATT + PO_Total + PE_Total_  + Total_language + Weekly_attendance_days + Total_attendance_months + N_of_siblings, data = UK_data, nbest = 1)
summary(best_subset)
reg_summary <- summary(best_subset)
which.max(reg_summary$adjr2)
which.min(reg_summary$bic)
which.min(reg_summary$cp)
best_model_NI_zero_uk <- glm(NI_zero ~ Age+HME+HMA+N_of_siblings, data = UK_data, family = binomial)
summary(best_model_NI_zero_uk)

odds_ratios <- exp(coef(best_model_NI_zero_uk))
print(odds_ratios)
p_value <- 1 - pchisq(8.0193, df = 4)
p_value
pseudo_r2 <- pR2(best_model_NI_zero_uk)
pseudo_r2
# Remove rows with missing values in the predictor variables
UK_data_complete <- UK_data %>% drop_na(Age, HME, HMA, N_of_siblings)

# Fit the null model (using the same filtered dataset)
null_model <- glm(NI_zero ~ 1, data = UK_data_complete, family = binomial(link = "logit"))

# Fit the full model
best_model_NI_zero_uk <- glm(NI_zero ~ Age + HME + HMA+N_of_siblings, 
                          data = UK_data_complete, 
                          family = binomial(link = "logit"))

# Perform likelihood ratio test
anova(null_model, best_model_NI_zero_uk, test = "Chisq")




#Give a number ----
best_subset <- regsubsets(Give_a_Number_Accuracy ~  Gender_bin + Age + HME + HMA + HMATT + PO_Total + PE_Total_  + Total_language + Weekly_attendance_days + Total_attendance_months + N_of_siblings, data = UK_data, nbest = 1)
summary(best_subset)
reg_summary <- summary(best_subset)
which.max(reg_summary$adjr2)
which.min(reg_summary$bic)
which.min(reg_summary$cp)
best_model_Give_a_Number_Accuracy_uk <- lm(Give_a_Number_Accuracy ~ Age+HMA+HMATT+Total_language, data = UK_data)
best_model_Give_a_Number_Accuracy_uk <- lm.beta(best_model_Give_a_Number_Accuracy_uk)
summary(best_model_Give_a_Number_Accuracy_uk)


#DC total-------
best_subset <- regsubsets(DC_Total ~ Gender_bin + Age + HME + HMA + HMATT + PO_Total + PE_Total_  + Total_language + Weekly_attendance_days + Total_attendance_months + N_of_siblings, data = UK_data, nbest = 1)
summary(best_subset)
reg_summary <- summary(best_subset)
which.max(reg_summary$adjr2)
which.min(reg_summary$bic)
which.min(reg_summary$cp)
best_model_DC_Total_uk <- lm(DC_Total ~ Age+HME+HMA+PO_Total+Total_attendance_months+N_of_siblings, data = UK_data)
best_model_DC_Total_uk <- lm.beta(best_model_DC_Total_uk)
summary(best_model_DC_Total_uk)



#NC total -----
best_subset <- regsubsets(NC_Total ~ Gender_bin + Age + HME + HMA + HMATT + PO_Total + PE_Total_  + Total_language + Weekly_attendance_days + Total_attendance_months + N_of_siblings, data = UK_data, nbest = 1)
summary(best_subset)
reg_summary <- summary(best_subset)
which.max(reg_summary$adjr2)
which.min(reg_summary$bic)
which.min(reg_summary$cp)
best_model_NC_Total_uk <- lm(NC_Total ~ Total_attendance_months, data = UK_data)
best_model_NC_Total_uk <- lm.beta(best_model_NC_Total_uk)
summary(best_model_NC_Total_uk)



# Create the ggplot
ggplot(UK_data, aes(x = SES_Total_, y = NC_Total, color = GROUP)) +
  geom_point() +                             # Scatter plot points
  geom_smooth(method = "lm", se = TRUE) +    # Add the regression line with confidence interval
  labs(title = "Regression of NC Total by Group, and Gender_bin",
       x = "Gender_bin",
       y = "NC Total") +
  theme_minimal() 
#PENS -----
best_subset <- regsubsets(pens_raw ~ Gender_bin + Age + HME + HMA + HMATT + PO_Total + PE_Total_  + Total_language + Weekly_attendance_days + Total_attendance_months + N_of_siblings, data = UK_data, nbest = 1)
summary(best_subset)
reg_summary <- summary(best_subset)
which.max(reg_summary$adjr2)
which.min(reg_summary$bic)
which.min(reg_summary$cp)
best_model_pens_uk <- lm(pens_raw ~ Gender_bin+Age+HME+N_of_siblings, data = UK_data)
best_model_pens_uk <- lm.beta(best_model_pens_uk)
summary(best_model_pens_uk)



#SA lin reg
# Number_Identification_Accuracy_SA -----------
best_subset <- regsubsets(Number_Identification_Accuracy_ ~ Gender_bin + Age + HME + HMA + HMATT + PO_Total + PE_Total_+ Total_language + Weekly_attendance_days + Total_attendance_months + N_of_siblings, data = SA_data, nbest = 1)
summary(best_subset)
reg_summary <- summary(best_subset)
which.max(reg_summary$adjr2)
which.min(reg_summary$bic)
which.min(reg_summary$cp)
best_model_nia <- lm(Number_Identification_Accuracy_ ~ HME+HMA+PO_Total+PE_Total_+Total_language, data = SA_data)
best_model_nia <- lm.beta(best_model_nia)
summary(best_model_nia)


# Create the ggplot
ggplot(UK_data, aes(x = Total_attendance_months, y = Number_Identification_Accuracy_)) +
  geom_point() +                             # Scatter plot points
  geom_smooth(method = "lm", se = TRUE) +    # Add the regression line with confidence interval
  labs(title = "Regression of Number Identification Accuracy by Age and Group",
       x = "Age",
       y = "Number Identification Accuracy") +
  theme_minimal()     



# Counting -----
best_subset <- regsubsets(Counting ~ Gender_bin + Age + HME + HMA + HMATT + PO_Total + PE_Total_ + Total_language + Weekly_attendance_days + Total_attendance_months + N_of_siblings, data = SA_data, nbest = 1)
summary(best_subset)
reg_summary <- summary(best_subset)
which.max(reg_summary$adjr2)
which.min(reg_summary$bic)
which.min(reg_summary$cp)
best_model_c <- lm(Counting ~ Age+HMA, data = SA_data)
best_model_c <- lm.beta(best_model_c)
summary(best_model_c)


#NI_zero ----
best_subset <- regsubsets(NI_zero ~ Gender_bin + Age + HME + HMA + HMATT + PO_Total + PE_Total_  + Total_language + Weekly_attendance_days + Total_attendance_months + N_of_siblings, data = SA_data, nbest = 1)
summary(best_subset)
reg_summary <- summary(best_subset)
which.max(reg_summary$adjr2)
which.min(reg_summary$bic)
which.min(reg_summary$cp)
best_model_NI_zero <- glm(NI_zero ~ Gender_bin+HME+HMA++PO_Total+PE_Total_+Total_language, data = SA_data,family = binomial(link = "logit"))
summary(best_model_NI_zero)
odds_ratios <- exp(coef(best_model_NI_zero))
pseudo_r2 <- pR2(best_model_NI_zero)


# Remove rows with missing values in the predictor variables
SA_data_complete <- SA_data %>% drop_na(Gender_bin,HME,HMA,PO_Total,PE_Total_,Total_language)

# Fit the null model (using the same filtered dataset)
null_model <- glm(NI_zero ~ 1, data = SA_data_complete, family = binomial(link = "logit"))

# Fit the full model
best_model_NI_zero <- glm(NI_zero ~ Gender_bin+HME+HMA+PO_Total+PE_Total_+Total_language, 
                          data = SA_data_complete, 
                          family = binomial(link = "logit"))

# Perform likelihood ratio test
anova(null_model, best_model_NI_zero, test = "Chisq")       

library(pscl)
 pR2(best_model_NI_zero)

#Give a number ----
best_subset <- regsubsets(Give_a_Number_Accuracy ~ Gender_bin + Age +HME + HMA + HMATT + PO_Total + PE_Total_ + Total_language + Weekly_attendance_days + Total_attendance_months + N_of_siblings, data = SA_data, nbest = 1)
summary(best_subset)
reg_summary <- summary(best_subset)
which.max(reg_summary$adjr2)
which.min(reg_summary$bic)
which.min(reg_summary$cp)
best_model_Give_a_Number_Accuracy <- lm(Give_a_Number_Accuracy ~ Age+PE_Total_+Total_language, data = SA_data)
best_model_Give_a_Number_Accuracy <- lm.beta(best_model_Give_a_Number_Accuracy)
summary(best_model_Give_a_Number_Accuracy)


# Create the ggplot
ggplot(SA_data, aes(x = Age, y = Give_a_Number_Accuracy)) +
  geom_point() +                             # Scatter plot points
  geom_smooth(method = "lm", se = TRUE) +    # Add the regression line with confidence interval
  labs(title = "Regression of Give a Number by Age",
       x = "Age",
       y = "Give_a_Number_Accuracy") +
  theme_minimal() 

#DC total-------
best_subset <- regsubsets(DC_Total ~ Gender_bin +Age + HME + HMA + HMATT + PO_Total + PE_Total_ + Total_language + Weekly_attendance_days + Total_attendance_months + N_of_siblings, data = SA_data, nbest = 1)
summary(best_subset)
reg_summary <- summary(best_subset)
which.max(reg_summary$adjr2)
which.min(reg_summary$bic)
which.min(reg_summary$cp)
best_model_DC_Total <- lm(DC_Total ~ Age+HME+Total_language, data = SA_data)
best_model_DC_Total <- lm.beta(best_model_DC_Total)

summary(best_model_DC_Total)


# Create the ggplot
ggplot(data_cleaned, aes(x = Age, y = DC_Total, color = GROUP)) +
  geom_point() +                             # Scatter plot points
  geom_smooth(method = "lm", se = TRUE) +    # Add the regression line with confidence interval
  labs(title = "Regression of DC Total by Group, and Age",
       x = "Age",
       y = "DC Total") +
  theme_minimal() 

#NC total -----
best_subset <- regsubsets(NC_Total ~ Gender_bin +Age + HME + HMA + HMATT + PO_Total + PE_Total_  + Total_language + Weekly_attendance_days + Total_attendance_months + N_of_siblings, data = SA_data, nbest = 1)
summary(best_subset)
reg_summary <- summary(best_subset)
which.max(reg_summary$adjr2)
which.min(reg_summary$bic)
which.min(reg_summary$cp)
best_model_NC_Total <- lm(NC_Total ~ Gender_bin + PO_Total + PE_Total_  + Total_attendance_months + N_of_siblings, data = SA_data)
best_model_NC_Total <- lm.beta(best_model_NC_Total)
summary(best_model_NC_Total)


# Create the ggplot
ggplot(UK_data, aes(x = SES_Total_, y = NC_Total, color = GROUP)) +
  geom_point() +                             # Scatter plot points
  geom_smooth(method = "lm", se = TRUE) +    # Add the regression line with confidence interval
  labs(title = "Regression of NC Total by Group, and Gender_bin",
       x = "Gender_bin",
       y = "NC Total") +
  theme_minimal() 
#PENS -----
best_subset <- regsubsets(pens_raw ~ Gender_bin + HME + HMA + HMATT + PO_Total + PE_Total_  + Total_language + Weekly_attendance_days + Total_attendance_months + N_of_siblings, data = SA_data, nbest = 1)
summary(best_subset)
reg_summary <- summary(best_subset)
which.max(reg_summary$adjr2)
which.min(reg_summary$bic)
which.min(reg_summary$cp)
best_model_pens <- lm(pens_raw ~ HME+HMA+HMATT+PE_Total_+Total_attendance_months+N_of_siblings, data = SA_data)
best_model_pens <- lm.beta(best_model_pens)
summary(best_model_pens)




# Create the ggplot----
ggplot(data_cleaned, aes(x = Age, y = pens_raw, color = GROUP)) +
  geom_point() +                             # Scatter plot points
  geom_smooth(method = "lm", se = TRUE) +    # Add the regression line with confidence interval
  labs(title = "PENS",
       x = "Age",
       y = "Raw score") +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 14),  # Increase x-axis title size
    axis.title.y = element_text(size = 14),  # Increase y-axis title size
    plot.title = element_text(size = 16, hjust = 0.5)  # Center and increase the title size
  )