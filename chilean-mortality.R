library(tidyverse)
library(readxl)
library(ggplot2)
library(survival)
library(KMsurv)
library(flexsurv)
library(splines)
library(survminer)
library(scales)
library(broom)

#Read mortality data
mortality_data <- read.csv("ChileanMortality.csv")

life_tables <- read_xlsx("ChileanLifeTables.xlsx")

##PART B) DATA EXPLORATION##
str(mortality_data)
sum(is.na(mortality_data))
summary(mortality_data)

mortality_data$BIRTHDATE <- as.Date(mortality_data$BIRTHDATE)
mortality_data$DATE_START <- as.Date(mortality_data$DATE_START)
mortality_data$DATE_END <- as.Date(mortality_data$DATE_END)

# Creating new variable: age_at_start
mortality_data$age_at_start <- round(difftime(mortality_data$DATE_START, 
                                              mortality_data$BIRTHDATE, units = "auto") / 365.25, digits = 0)

table(mortality_data$HEALTH)

##PART D) ANALYSIS##
# Analysis 1: Age distribution of annuitants

mean_age <- mean(mortality_data$age_at_start, na.rm = TRUE)

age_distribution <- ggplot(mortality_data, aes(x = age_at_start)) +
  geom_histogram(binwidth = 5, fill = "#4D85BD", color = "#1C3C5A") +
  geom_vline(aes(xintercept = mean_age), color = "red", linetype = "dashed", size = 1) +
  scale_y_continuous(expand = c(0, 0), labels = label_number(scale = 1e-6)) +
  labs(title = "Age Distribution of Annuitants", 
       x = "Age at Start", 
       y = "Frequency (Millions)") +
  theme_classic(base_size = 12) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
        axis.text = element_text(size = 10),
        axis.line = element_line(size = 0.5),
        plot.margin = margin(1, 1, 1, 1, "cm"),
        plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_line(color = "black"))

# Print the plot
age_distribution

###Analysis 2: Health proportions of genders##########
prop_table <- table(mortality_data$SEX, mortality_data$HEALTH)
prop_table <- prop_table / rowSums(prop_table)

prop_df <- as.data.frame.matrix(prop_table)

prop_df <- prop_df * 100
prop_df <- round(prop_df, digits = 2)

print(prop_df)

health_status_gender <- ggplot(mortality_data, aes(x = SEX, fill = HEALTH)) +
  geom_bar(position = "fill") +  # Stacks the bar to show proportions
  scale_fill_manual(values = c("Disabled" = "#E27D60", "Healthy" = "#41B3A3")) +
  labs(y = "Proportion", title = "Health Status by Gender") +
  theme_classic() +  # Provides a plain background with no gridlines
  theme(axis.title = element_text(size = 12),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),  # Centers the title
        axis.text = element_text(size = 10),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10),
        axis.ticks = element_line(color = "black"))

health_status_gender

# Analysis 3: Distribution of annuitants by age and gender
histogram_plot <- ggplot(mortality_data, aes(x = age_at_start, fill = SEX)) +
  geom_histogram(binwidth = 1, position = "identity", alpha = 0.6) +
  scale_fill_brewer(palette = "Set1") +
  labs(x = "Age at Start", y = "Count", title = "Histogram of Annuitants by Age and Gender") +
  theme_minimal() +
  theme(legend.position = "top",
        plot.title = element_text(hjust = 0.5),
        legend.title = element_text(size = 10),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_rect(fill = "white", colour = NA))

histogram_plot

cum_freq_plot <- ggplot(mortality_data, aes(x = age_at_start, colour = SEX)) +
  geom_freqpoly(binwidth = 1) +
  scale_colour_brewer(palette = "Set1") +
  labs(x = "Age at Start", y = "Cumulative Count", title = "Cumulative Distribution of Annuitants by Age and Gender") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        legend.title = element_text(size = 10),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white", colour = NA))

cum_freq_plot

cum_freq_plot_health <- ggplot(mortality_data, aes(x = age_at_start, colour = HEALTH)) +
  geom_freqpoly(binwidth = 1) +
  scale_y_continuous(labels = scales::label_number(scale = 1e-6)) +
  scale_colour_brewer(palette = "Set1") +
  labs(x = "Age at Start", y = "Cumulative Count (Millions)", title = "Cumulative Distribution of Annuitants by Age and Health") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        legend.title = element_text(size = 10),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white", colour = NA))

cum_freq_plot_health

###4.2.2 Survival analysis###
data(mortality_data)
head(mortality_data)
attach(mortality_data)

###KM Estimate###  
mortality_data$DATE_START <- as.Date(mortality_data$DATE_START, format="%Y-%m-%d")
mortality_data$DATE_END <- as.Date(mortality_data$DATE_END, format="%Y-%m-%d")

mortality_data$AGE_AT_ENTRY <- as.numeric(difftime(mortality_data$DATE_START, 
                                                   mortality_data$BIRTHDATE, units = "days")) / 365.25
mortality_data$AGE_AT_EXIT <- as.numeric(difftime(mortality_data$DATE_END, 
                                                  mortality_data$BIRTHDATE, units = "days")) / 365.25
mortality_data$DAYS_IN_STUDY <- as.numeric(difftime(mortality_data$DATE_END, 
                                                    mortality_data$DATE_START, units = "days"))

mortality_data$AGE_AT_EXIT_floor <- floor(mortality_data$AGE_AT_EXIT)
mortality_data$AGE_AT_ENTRY_floor <- floor(mortality_data$AGE_AT_ENTRY)

###On-study time###
mortality_data$time <- as.numeric(mortality_data$DATE_END - mortality_data$DATE_START)

###Death to delta where delta = 0 is alive and delta = 1 is dead###
mortality_data$delta <- as.numeric(mortality_data$DEATH)

###Adding column for type where MA is 1 and Beneficiary is 2###
mortality_data$type <- ifelse(mortality_data$PERSON_TYPE == 'Main Annuitant', 1, 2)

###KM Estimation###
filtered_mortality_data <- subset(mortality_data, AGE_AT_ENTRY != AGE_AT_EXIT)
filtered_mortality_data <- filtered_mortality_data %>%
  mutate(bd_plus_60 = BIRTHDATE + years(60)) 
filtered_mortality_data$bd_plus_60 <- as.Date(filtered_mortality_data$bd_plus_60)

filtered_mortality_data <- filtered_mortality_data %>% 
  mutate(bd_plus_60 = year(bd_plus_60) - year(BIRTHDATE))

filtered_mortality_data <- filtered_mortality_data %>%
  mutate(AGE_AT_ENTRY = case_when(
    AGE_AT_ENTRY < 60 ~ bd_plus_60,
    TRUE ~ AGE_AT_ENTRY
))

surv_obj <- with(filtered_mortality_data, Surv(AGE_AT_ENTRY, AGE_AT_EXIT, DEATH))

km_sex <- survfit(surv_obj ~ SEX, data = filtered_mortality_data)
plot(km_sex, col = c("blue", "red"), lty = 1, 
     xlim = c(60, max(filtered_mortality_data$AGE_AT_EXIT)), 
     xlab = "Age (Years)", ylab = "Survival Probability", 
     main = "Kaplan-Meier Survival Curves by Sex", conf.int = TRUE)
legend("bottomleft", legend = c("Female", "Male"), col = c("blue", "red"), lty = 1)

# Plot KM survival curves for HEALTH with confidence intervals
km_health <- survfit(surv_obj ~ HEALTH, data = filtered_mortality_data)
plot(km_health, col = c("green", "orange"), lty = 1, 
     xlim = c(60, max(filtered_mortality_data$AGE_AT_EXIT)), 
     xlab = "Age (Years)", ylab = "Survival Probability", 
     main = "Kaplan-Meier Survival Curves by Health Status", conf.int = TRUE)
legend("bottomleft", legend = c("Disabled", "Healthy"), col = c("green", "orange"), lty = 1)


km_type <- survfit(surv_obj ~ PERSON_TYPE, data = filtered_mortality_data)
plot(km_type, col = c("purple", "brown"), lty = 1, 
     xlim = c(60, max(filtered_mortality_data$AGE_AT_EXIT)), 
     xlab = "Age (Years)", ylab = "Survival Probability", 
     main = "Kaplan-Meier Survival Curves by Type of Annuitant", conf.int = TRUE)
legend("bottomleft", legend = c("Main Annuitant", "Beneficiary"), col = c("purple", "brown"), lty = 1)


km_sex_health <- survfit(surv_obj ~ SEX + HEALTH, data = filtered_mortality_data)

km_sex_health_type <- survfit(surv_obj ~ SEX + HEALTH + PERSON_TYPE, data = filtered_mortality_data)

km_sex_health_plot <- plot(km_sex_health, col = c("blue", "red", "green", "purple"), 
                             xlim = c(60, max(filtered_mortality_data$AGE_AT_EXIT)),
                             lty = c(1, 2, 3, 4), xlab = "Time", ylab = "Survival Probability", 
                             main = "Kaplan-Meier Survival Curves by Sex and Health")
legend("topright", legend = c("M - Healthy", "M - Disabled", "F - Healthy", "F - Disabled"), 
       col = c("blue", "red", "green", "purple"), lty = c(1, 2, 3, 4))

km_sex_health_plot

plot(km_sex_health_type, col = c("blue", "red", "green", "purple", "orange", "brown", "cyan", "magenta"), 
     xlim = c(60, max(filtered_mortality_data$AGE_AT_EXIT)),
     lty = c(1, 2, 3, 4, 5, 6, 7, 8), xlab = "Time", ylab = "Survival Probability",
     main = "Kaplan-Meier Survival Curves by Sex, Health, and Person Type")
legend("topright", 
       legend = c("F - Disabled - Beneficiary", "F - Disabled - Main Annuitant",
                  "F - Healthy - Beneficiary", "F - Healthy - Main Annuitant",
                  "M - Disabled - Beneficiary", "M - Disabled - Main Annuitant",
                  "M - Healthy - Beneficiary", "M - Healthy - Main Annuitant"), 
       col = c("blue", "red", "green", "purple", "orange", "brown", "cyan", "magenta"), 
       lty = c(1, 2, 3, 4, 5, 6, 7, 8), cex = 0.7)

cens.mortality_MA <- Surv(time = mortality_data$time[mortality_data$type == 1], 
                            event = mortality_data$delta[mortality_data$type == 1])

###Log-Rank and Peto Peto###
G2 <- Surv(mortality_data$time, mortality_data$delta)

###SINGLE VARIABLE LOG_RANK###
logrank_sex <- survdiff(G2 ~ mortality_data$SEX, rho = 0)
logrank_sex

logrank_health <- survdiff(G2 ~ mortality_data$HEALTH, rho = 0)
logrank_health

logrank_type <- survdiff(G2 ~ mortality_data$PERSON_TYPE, rho = 0)
logrank_type

###Multiple VARIABLE LOG_RANK###
logrank_sex_health <- survdiff(G2 ~ mortality_data$SEX + mortality_data$HEALTH, rho = 0)
logrank_sex_health

logrank_sex_health_type <- survdiff(G2 ~ mortality_data$SEX + mortality_data$HEALTH + 
                                      mortality_data$PERSON_TYPE, rho = 0)
logrank_sex_health_type

###SINGLE VARIABLE PETO###
peto_sex <- survdiff(G2 ~ mortality_data$SEX, rho = 1)
peto_sex

peto_health <- survdiff(G2 ~ mortality_data$HEALTH, rho = 1)
peto_health

peto_type <- survdiff(G2 ~ mortality_data$PERSON_TYPE, rho = 1)
peto_type

###Multiple VARIABLE PETO###
peto_sex_health <- survdiff(G2 ~ mortality_data$SEX + mortality_data$HEALTH, rho = 1)
peto_sex_health

peto_sex_health_type <- survdiff(G2 ~ mortality_data$SEX + mortality_data$HEALTH + 
                                      mortality_data$PERSON_TYPE, rho = 1)
peto_sex_health_type

####TABLES FOR LOG-RANK AND PETO####

logrank_chi_sex <- logrank_sex$chisq
logrank_p_sex <- 1 - pchisq(logrank_sex$chisq, df = length(logrank_sex$n) - 1)

logrank_chi_health <- logrank_health$chisq
logrank_p_health <- 1 - pchisq(logrank_health$chisq, df = length(logrank_health$n) - 1)

logrank_chi_type <- logrank_type$chisq
logrank_p_type <- 1 - pchisq(logrank_type$chisq, df = length(logrank_type$n) - 1)

logrank_chi_sex_health <- logrank_sex_health$chisq
logrank_p_sex_health <- 1 - pchisq(logrank_sex_health$chisq, df = length(logrank_sex_health$n) - 1)

logrank_chi_sex_health_type <- logrank_sex_health_type$chisq
logrank_p_sex_health_type <- 1 - pchisq(logrank_sex_health_type$chisq, df = length(logrank_sex_health_type$n) - 1)

# Extract chi-squared and p-value for the Peto tests
peto_chi_sex <- peto_sex$chisq
peto_p_sex <- 1 - pchisq(peto_sex$chisq, df = length(peto_sex$n) - 1)

peto_chi_health <- peto_health$chisq
peto_p_health <- 1 - pchisq(peto_health$chisq, df = length(peto_health$n) - 1)

peto_chi_type <- peto_type$chisq
peto_p_type <- 1 - pchisq(peto_type$chisq, df = length(peto_type$n) - 1)

peto_chi_sex_health <- peto_sex_health$chisq
peto_p_sex_health <- 1 - pchisq(peto_sex_health$chisq, df = length(peto_sex_health$n) - 1)

peto_chi_sex_health_type <- peto_sex_health_type$chisq
peto_p_sex_health_type <- 1 - pchisq(peto_sex_health_type$chisq, df = length(peto_sex_health_type$n) - 1)

# Create summary table for Single Variable
summary_table_single <- data.frame(
  Group_Comparison = c("Sex", "Health", "Type"),
  LogRank_ChiSquared = c(logrank_chi_sex, logrank_chi_health, logrank_chi_type),
  LogRank_P_Value = c(logrank_p_sex, logrank_p_health, logrank_p_type),
  Peto_ChiSquared = c(peto_chi_sex, peto_chi_health, peto_chi_type),
  Peto_P_Value = c(peto_p_sex, peto_p_health, peto_p_type)
)
# Create summary table for Multiple Variable
summary_table <- data.frame(
  Group_Comparison = c("Sex", "Sex + Health", "Sex + Health + Type"),
  LogRank_ChiSquared = c(logrank_chi_sex, logrank_chi_sex_health, logrank_chi_sex_health_type),
  LogRank_P_Value = c(logrank_p_sex, logrank_p_sex_health, logrank_p_sex_health_type),
  Peto_ChiSquared = c(peto_chi_sex, peto_chi_sex_health, peto_chi_sex_health_type),
  Peto_P_Value = c(peto_p_sex, peto_p_sex_health, peto_p_sex_health_type)
)

print(summary_table)

###Cox Model Single###
cox_model.1 <- coxph(surv_obj ~ SEX, data = filtered_mortality_data, method = "breslow")
cox_model.2 <- coxph(surv_obj ~ HEALTH, data = filtered_mortality_data, method = "breslow")
cox_model.3 <- coxph(surv_obj ~ PERSON_TYPE, data = filtered_mortality_data, method = "breslow")

###Cox Model Multiple###
cox_model.4 <- coxph(surv_obj ~ SEX + HEALTH, data = filtered_mortality_data, method = "breslow")
cox_model.5 <- coxph(surv_obj ~ SEX + HEALTH + PERSON_TYPE, data = filtered_mortality_data, method = "breslow")

###COX SUMMARY TABLE###
models <- list(
  cox_model.1 = coxph(surv_obj ~ SEX, data = filtered_mortality_data, method = "breslow"),
  cox_model.2 = coxph(surv_obj ~ HEALTH, data = filtered_mortality_data, method = "breslow"),
  cox_model.3 = coxph(surv_obj ~ PERSON_TYPE, data = filtered_mortality_data, method = "breslow"),
  cox_model.4 = coxph(surv_obj ~ SEX + HEALTH, data = filtered_mortality_data, method = "breslow"),
  cox_model.5 = coxph(surv_obj ~ SEX + HEALTH + PERSON_TYPE, data = filtered_mortality_data, method = "breslow")
)

summary_table <- data.frame()

for (i in seq_along(models)) {
  model_summary <- tidy(models[[i]])
  model_summary$exp_coef <- exp(model_summary$estimate)
  model_summary$Model <- names(models)[i]
  summary_table <- rbind(summary_table, model_summary)
}

summary_table <- summary_table[, c("Model", "term", "estimate", "exp_coef", "std.error", "statistic", "p.value")]

print(summary_table)

###Obtaining baseline hazard function###
Base.H <- basehaz(cox_model.1, centered = FALSE)

plot(Base.H$time, Base.H$hazard, xlab = "t", ylab = "H_0(t)",
     main = "Baseline Hazard Rate", type = "s")

###Graduation of unisex life table for healthy annuitants###
healthy_data <- subset(mortality_data, HEALTH == "Healthy" & age_at_start >= 60 & age_at_start <= 100)

healthy_data$days_in_study <- pmax(as.numeric(as.Date(healthy_data$DATE_END) - as.Date(healthy_data$DATE_START)), 0)

healthy_data$adjusted_days_in_study <- ifelse(healthy_data$DEATH, 
                                              healthy_data$days_in_study - 0.5 * 365.25, 
                                              healthy_data$days_in_study)

Dx <- tapply(healthy_data$delta, healthy_data$AGE_AT_ENTRY_floor, sum, na.rm = TRUE)

Ex <- tapply(healthy_data$adjusted_days_in_study / 365.25, 
             healthy_data$AGE_AT_ENTRY_floor, sum, na.rm = TRUE)

mx <- Dx / Ex

x <- 60:100
Ex <- as.numeric(Ex[as.character(x)])
Dx <- as.numeric(Dx[as.character(x)])
mx <- as.numeric(mx[as.character(x)])
#########MAKE SURE THE Ex, DX and mx are correct###########
####CHECKING COEFFICIENTS####
log_mx <- log(mx)
gompertz_lm <- lm(log_mx ~ x)
summary(gompertz_lm)
b0_lm <- coef(gompertz_lm)[1] 
b1_lm <- coef(gompertz_lm)[2]
B_start <- exp(b0_lm)
C_start <- exp(b1_lm)
####CHECKING COEFFICIENTS####

####GOMPERTZ####
gompertz <- nls(mx ~ exp(b0 + b1*x), start = list(b0 = 1 , b1 =  0), weights = Ex/mx)
gompertz

mx_gompertz <- fitted(gompertz)
plot(x, log(mx), pch = 20, xlab = "age", ylab =
       "Central mortality rate (log scale)",
        main = "Chilean Mortality: Gompertz law",
        ylim = range(-6, 0))
        lines(x, log(mx_gompertz), col ='blue')


####MAKEHAM####
makeham <- nls(mx ~ A + exp(b0 + b1*x), start
               = list(A = 0, b0 = coef(gompertz)[1], b1 = coef(gompertz)[2]),
                      weights = Ex/mx)
makeham

mx_makeham <- fitted(makeham)
plot(x, log(mx), pch = 20, xlab = "age", ylab =
       "Central mortality rate (log scale)",
     main = "Chilean Mortality: Makeham law",
     ylim = range(-6, 0))
lines(x,log(mx_makeham), col = 'blue')

####SPLINES####

###Cubic splines####

knots <- quantile(x, probs = c(0.25, 0.5, 0.75))

cubSpline <- lm(mx ~ ns(x, knots = knots), weights = Ex/mx)
cubSpline

x_seq <- seq(min(x), max(x), length.out = 200)
mx_cubSpline <- fitted(cubSpline)

plot(x, log(mx), pch = 20, xlab = "Age", ylab = "Log Central mortality rate",
     main = "Chilean Mortality: Cubic Spline", ylim = range(log(mx), 
                                                            log(mx_cubSpline)))
lines(x, log(mx_cubSpline), col = 'blue')

###Smoothing splines####

###choosing the spar value###
plot(x, mx, main="Choosing Smoothing Parameter Value", xlab="Age", ylab="Mortality rate")
for(s in seq(0.2, 2, by=0.4)) {
  fit <- smooth.spline(x, mx, spar=s)
  lines(fit, col=rainbow(5)[which(s==seq(0.2, 2, by=0.4))], lwd=2)
}
legend("topleft", legend=seq(0.2, 2, by=0.4), col=rainbow(5), lwd=2, title="spar values")

fit <- smooth.spline(x, mx, cv=TRUE)
print(fit$spar) # Optimal spar value
##include in appendix#####

smSpline <- smooth.spline(x, mx, spar = 0.7) ####Find out the spar value###
smSpline

mx_smSpline <- fitted(smSpline)
plot(x, log(mx), pch = 20,  xlab = "age", ylab
     = "Central mortality rate (log scale)",
     main = "Graduated Mortality Rates using Smoothing Spline Model",
     ylim = range(-6, 0))
lines(x, log(mx_smSpline), col = 'blue')

####COMPARISON OF GRADUATIONS####
plot(x, log(mx), pch = 20,  xlab = "Age", ylab
     = "Central mortality rate (log scale)",main =
       "Comparison of Mortality Graduation Methods for Healthy Annuitants in Chile", ylim = range(-6,0))
lines(x, log(mx_gompertz), col = 2)
lines(x, log(mx_makeham), col = 3)
lines(x, log(mx_cubSpline), col = 4)
lines(x, log(mx_smSpline), col = 5)
legend("topleft", legend = c("Gompertz", "Makeham", "Cubic Spline", "Smoothing Spline"),  
       col = 2:5, lty = 1)

####STATISTICAL TESTS######
zx_makeham <- (Dx - Ex * mx_makeham) / sqrt(Ex * mx_makeham)
zx_gompertz <- (Dx - Ex * mx_gompertz) / sqrt(Ex * mx_gompertz)
zx_cubSpline <- (Dx - Ex * mx_cubSpline) / sqrt(Ex * mx_cubSpline)
zx_smSpline <- (Dx - Ex * mx_smSpline) / sqrt(Ex * mx_smSpline)

chi2Test <- function(O, E, npar, alpha = 0.05){
  chi2 <- sum((O - E)^2 / E)
  df <- length(O) - npar
  chi2_alpha <- qchisq(1 - alpha, df)
  p.value <- 1 - pchisq(chi2, df)
  list(statistic = chi2, c.value = chi2_alpha,
       df = df, p.value = p.value)
}

####Chi2Test###
chi2Test(Dx, Ex * mx_gompertz, 2)
chi2Test(Dx, Ex * mx_makeham, 3)
chi2Test(Dx, Ex * mx_cubSpline, 7) ###no. of knots + degrees of freedom
chi2Test(Dx, Ex * mx_smSpline, 6) ###no. of parameters for smoothing spline rounded up from 
#degrees of freedom for practical purposes


###SD Test###
stdTest <- function(zx, breaks = c(-Inf, -1, 0,
                                   1, Inf)){
  observed <- table(cut(zx, breaks))
  expected.p <- diff(pnorm(breaks))
  chisq.test(observed, p = expected.p)
}

stdTest_gompertz <- stdTest(zx_gompertz)
stdTest_makeham <- stdTest(zx_makeham)
stdTest_cubSpline <- stdTest(zx_cubSpline)
stdTest_smSpline <- stdTest(zx_smSpline)

###Signs Test###
nages <- length(x)
signTest_gompertz <- binom.test(sum(zx_gompertz > 0), nages)
signTest_makeham <- binom.test(sum(zx_makeham > 0), nages)
signTest_cubSpline <- binom.test(sum(zx_cubSpline > 0), nages)
signTest_smSpline <- binom.test(sum(zx_smSpline > 0), nages)

###Cum Deviations Test###
cumDevTest <- function(A, E, alpha = 0.05){
  cumDev <- sum(A - E) / sqrt(sum(E)) 
  z_alpha <- qnorm(1 - alpha/2) 
  p.value <- 2 *(1 - pnorm(cumDev)) 
list(statistic = cumDev, c.value = z_alpha,
     p.value = p.value)
}

cumDevTest_gompertz <- cumDevTest(Dx, Ex * mx_gompertz)
cumDevTest_makeham <- cumDevTest(Dx, Ex * mx_makeham)
cumDevTest_cubSpline <- cumDevTest(Dx, Ex * mx_cubSpline)
cumDevTest_smSpline <- cumDevTest(Dx, Ex * mx_smSpline)

###Grouping of Signs Test
groupSignTest <- function(zx, alpha = 0.05){
  signs <- sign(zx)
  n1 <- sum(signs == 1)
  n2 <- sum(signs == -1)
  y <- c(-1, sign(zx))
  G <- sum((y[-1] != y[-(n1 + n2 + 1)]) & y[-1] != -1) 
  mu <- n1 * (n2 + 1) / (n1 + n2)
  s2 <- (n1 * n2)^2 / (n1 + n2)^3
  G_alpha <- qnorm(alpha, mean = mu, sd = sqrt(s2)) 
  p.value <- (pnorm(G + 0.5, mean = mu, sd = sqrt(s2))) 
  list(statistic = G, c.value = G_alpha, p.value = p.value) 
}

groupSignTest_gompertz <- groupSignTest(zx_gompertz) ### reject H_0 since test stat is less than crit val
groupSignTest_makeham <- groupSignTest(zx_makeham)
groupSignTest_cubSpline <- groupSignTest(zx_cubSpline)
groupSignTest_smSpline <- groupSignTest(zx_smSpline)

###Serial Correlations test###
acf(zx_gompertz)
acf(zx_makeham)
acf(zx_cubSpline)
acf(zx_smSpline)

###Comparing Smoothing Spline Graduated Rates to Mortality rates from life table###
names(life_tables) <- c('Age', 'RV_M', 'MI_M', 'B_M', 'CB_H', 'MI_H')

life_tables$mx_RV_M <- -log(1 - life_tables$RV_M)
life_tables$mx_MI_M <- -log(1 - life_tables$MI_M)
life_tables$mx_B_M <- -log(1 - life_tables$B_M)
life_tables$mx_CB_H <- -log(1 - life_tables$CB_H)
life_tables$mx_MI_H <- -log(1 - life_tables$MI_H)

life_tables$log_mx_RV_M <- log(life_tables$mx_RV_M)
life_tables$log_mx_MI_M <- log(life_tables$mx_MI_M)
life_tables$log_mx_B_M <- log(life_tables$mx_B_M)
life_tables$log_mx_CB_H <- log(life_tables$mx_CB_H)
life_tables$log_mx_MI_H <- log(life_tables$mx_MI_H)

plot(life_tables$Age, life_tables$log_mx_RV_M, type='l', col='red', 
     ylim=c(-6, 0), xlim=c(60, 100), xlab="Age", ylab="log(m_x)",
     main="Comparison of Smoothed Spline Mortality Rates with Life Tables")
lines(life_tables$Age, life_tables$log_mx_B_M, col='green')
lines(life_tables$Age, life_tables$log_mx_CB_H, col='purple')
lines(x, log(mx_smSpline), col='black', lwd=2) # Only if mx_smSpline is in log scale

legend("topleft", legend=c("RV-M-2020", "B-M-2020", "CB-H-2020", "Smoothing Spline"),
       col=c("red", "green", "purple", "black"), lty=1, cex=0.8)

points(x, log_mx, pch=20, col='black')

# Calculate residuals for RV-M-2020
residuals_RV_M <- life_tables$RV_M - mx_smSpline

# Calculate residuals for B-M-2020
residuals_B_M <- life_tables$B_M - mx_smSpline

# Calculate residuals for CB-H-2020
residuals_CB_H <- life_tables$CB_H - mx_smSpline

# Plot residuals
plot(life_tables$Age, residuals_RV_M, type='b', pch=20, col='red', ylab='Residuals', xlab='Age',
     main='Residuals of Smoothed Spline vs Life Tables')
points(life_tables$Age, residuals_B_M, type='b', pch=20, col='green')
points(life_tables$Age, residuals_CB_H, type='b', pch=20, col='blue')
abline(h=0, lty=2) 

mse_RV_M <- mean(residuals_RV_M^2)
mse_B_M <- mean(residuals_B_M^2)
mse_CB_H <- mean(residuals_CB_H^2)

# Print MSE to console
print(paste('MSE for RV-M-2020:', mse_RV_M))
print(paste('MSE for B-M-2020:', mse_B_M))
print(paste('MSE for CB-H-2020:', mse_CB_H))

# Add a legend to the plot
legend("topleft", inset=c(0.02, 0.02), 
       legend=c(paste('RV-M-2020 (MSE:', round(mse_RV_M, 6), ')'), 
                paste('B-M-2020 (MSE:', round(mse_B_M, 6), ')'), 
                paste('CB-H-2020 (MSE:', round(mse_CB_H, 6), ')')), 
       col=c("red", "green", "blue"), pch=20, cex=0.8)

###DENSITY Plot of Residuals: 
names(life_tables) <- c('Age', 'RV_M', 'MI_M', 'B_M', 'CB_H', 'MI_H')

life_tables$mx_RV_M <- -log(1 - life_tables$RV_M)
life_tables$mx_MI_M <- -log(1 - life_tables$MI_M)
life_tables$mx_B_M <- -log(1 - life_tables$B_M)
life_tables$mx_CB_H <- -log(1 - life_tables$CB_H)
life_tables$mx_MI_H <- -log(1 - life_tables$MI_H)

life_tables$log_mx_RV_M <- log(life_tables$mx_RV_M)
life_tables$log_mx_MI_M <- log(life_tables$mx_MI_M)
life_tables$log_mx_B_M <- log(life_tables$mx_B_M)
life_tables$log_mx_CB_H <- log(life_tables$mx_CB_H)
life_tables$log_mx_MI_H <- log(life_tables$mx_MI_H)

plot(life_tables$Age, life_tables$log_mx_RV_M, type='l', col='red', 
     ylim=c(-6, 0), xlim=c(60, 100), xlab="Age", ylab="log(m_x)",
     main="Comparison of Smoothed Spline Mortality Rates with Life Tables")
lines(life_tables$Age, life_tables$log_mx_B_M, col='green')
lines(life_tables$Age, life_tables$log_mx_CB_H, col='purple')
lines(x, log(mx_smSpline), col='black', lwd=2) # Only if mx_smSpline is in log scale

legend("topleft", legend=c("RV-M-2020", "B-M-2020", "CB-H-2020", "Smoothing Spline"),
       col=c("red", "green", "purple", "black"), lty=1, cex=0.8)

points(x, log_mx, pch=20, col='black')

# Calculate residuals for RV-M-2020
residuals_RV_M <- life_tables$RV_M - mx_smSpline

# Calculate residuals for B-M-2020
residuals_B_M <- life_tables$B_M - mx_smSpline

# Calculate residuals for CB-H-2020
residuals_CB_H <- life_tables$CB_H - mx_smSpline

# Plot residuals
plot(life_tables$Age, residuals_RV_M, type='b', pch=20, col='red', ylab='Residuals', xlab='Age',
     main='Residuals of Smoothed Spline vs Life Tables')
points(life_tables$Age, residuals_B_M, type='b', pch=20, col='green')
points(life_tables$Age, residuals_CB_H, type='b', pch=20, col='blue')
abline(h=0, lty=2) 

mse_RV_M <- mean(residuals_RV_M^2)
mse_B_M <- mean(residuals_B_M^2)
mse_CB_H <- mean(residuals_CB_H^2)

# Print MSE to console
print(paste('MSE for RV-M-2020:', mse_RV_M))
print(paste('MSE for B-M-2020:', mse_B_M))
print(paste('MSE for CB-H-2020:', mse_CB_H))

# Add a legend to the plot
legend("topleft", inset=c(0.02, 0.02), 
       legend=c(paste('RV-M-2020 (MSE:', round(mse_RV_M, 6), ')'), 
                paste('B-M-2020 (MSE:', round(mse_B_M, 6), ')'), 
                paste('CB-H-2020 (MSE:', round(mse_CB_H, 6), ')')), 
       col=c("red", "green", "blue"), pch=20, cex=0.8)

# Density plot for RV-M-2020 residuals
plot(density(residuals_RV_M), main="Density Plot of Residuals", xlab="Residuals", ylab="Density", col='red')
lines(density(residuals_B_M), col='green')
lines(density(residuals_CB_H), col='blue')
legend("topright", legend=c("RV-M-2020", "B-M-2020", "CB-H-2020"), fill=c("red", "green", "blue"))

# Ratio plot for life table rates to spline rates
plot(life_tables$Age, life_tables$RV_M / mx_smSpline, type='b', pch=20, col='red', ylab='Ratio', xlab='Age', main='Ratio of Life Table Rates to Spline Rates')
points(life_tables$Age, life_tables$B_M / mx_smSpline, type='b', pch=20, col='green')
points(life_tables$Age, life_tables$CB_H / mx_smSpline, type='b', pch=20, col='blue')
abline(h=1, lty=2)


# Regression analysis with life table rates as dependent variable and spline rates as independent variable
spline_data <- data.frame(Age = 60:100, mx_smSpline = mx_smSpline)  
life_tables <- merge(life_tables, spline_data, by = "Age")

lm_RV_M <- lm(RV_M ~ mx_smSpline, data = life_tables)

summary(lm_RV_M)

lm_RV_M <- lm(RV_M ~ mx_smSpline, data=life_tables)
lm_B_M <- lm(B_M ~ mx_smSpline, data=life_tables)
lm_CB_H <- lm(CB_H ~ mx_smSpline, data=life_tables)

summary(lm_RV_M) # Output summary of regression for RV-M-2020
summary(lm_B_M)  # Output summary of regression for B-M-2020
summary(lm_CB_H) # Output summary of regression for CB-H-2020
