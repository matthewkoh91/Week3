# Week 2

# Summaries and Statistical tests 
viz_data <-  readr::read_csv("data/viz_data.csv")
head(viz_data)

mean(viz_data$weight)
median(viz_data$weight)
min(viz_data$weight)
max(viz_data$weight)
quantile(viz_data$weight)
sd(viz_data$weight)

cov(viz_data$height, viz_data$weight)
cor(viz_data$height, viz_data$weight)

group_outcome_table <- table(viz_data$group, viz_data$outcome)
chisq.test(group_outcome_table)
fisher.test(group_outcome_table)

t.test(response ~ group,
       data = viz_data, 
       subset = group %in% c("High dose", "Low dose"))
wilcox.test(response ~ group,
            data = viz_data,
            subset = group %in% c("High dose", "Low dose"))

response_site_aov <- aov(response ~ site, data = viz_data)
summary(response_site_aov)

response_ttest <- t.test(response ~group, 
                         data = viz_data,
                         subset = group %in% c ("High dose", "Low dose"))
str(response_ttest)
response_ttest$p.value
response_ttest$conf.int

weight_summary <- summary(viz_data$weight)
str(weight_summary)
weight_summary["Median"]

# Exercise 2.1
bmiquantile <- quantile(viz_data$bmi, probs = c(0.1, 0.9))
bmiquantile[2] - bmiquantile[1]

t.test(height ~ site,
                  data = viz_data,
                  subset = site %in% c("Site A", "Site B"))
t.test(height ~ site,
       data = viz_data,
       subset = site %in% c("Site A", "Site B"),
       alternative = "greater")

# Regression
viz_data <- readr::read_csv("data/viz_data.csv")
followup_data <- readr::read_csv(('data/followup_data.csv'))
head(viz_data)
head(followup_data)
followup_data <- merge(viz_data, followup_data, by = 'id')

lm_fit <- lm(response ~ bmi + group, data = viz_data)
summary(lm_fit)

viz_data$improved <- as.integer(viz_data$outcome == "Improved")
logistic_fit <- glm(improved ~ bmi + group, data = viz_data, family = binomial)
summary(logistic_fit)

negbin_fit <- MASS::glm.nb(clinic_visits ~ group, data = followup_data)
summary(negbin_fit)

library(survival)
followup_surv <- Surv(followup_data$survival_months, followup_data$event)
head(followup_surv)

cox_fit <- coxph(Surv(survival_months, event) ~ group,
                data = followup_data)
summary(cox_fit)

coef(lm_fit)
confint(lm_fit)
summary(lm_fit)$coefficients

plot(lm_fit, which = 1)
plot(logistic_fit, which = 1)
plot(negbin_fit, which = 1)
cox_survfit <- survfit(cox_fit, newdata = data.frame(group = unique(followup_data$group)))
plot(cox_survfit, 
     xlab = "Time (months)",
     ylab = "Estimated survival probability")

library(ggplot2)
ggplot(viz_data, aes(x = bmi, y = response)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE)

ggplot(viz_data, aes(x = bmi, y = as.numeric(improved))) +
  geom_point() +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = TRUE)

ggplot(followup_data, aes(x = group, y = clinic_visits)) + 
  geom_jitter(width = 0.1, height = 0) + 
  stat_summary (fun = mean, geom = "point", size = 3, colour = "red")

cox_survfit_df <- broom::tidy(cox_survfit)

ggplot(cox_survfit_df, aes (x = time)) + 
  geom_step(aes(y = estimate.1), colour = "black") + 
  geom_step(aes(y = estimate.2), colour = "red") + 
  geom_step(aes(y = estimate.3), colour = "blue") + 
  labs (x = "Time (months)", y = "Estimated survival probability", colour = "Group")

# Data wrangling
library(tidyverse)
clinic_raw = read_csv("data/clinic_raw.csv", skip = 2)
head(clinic_raw)
str(clinic_raw)

sum(is.na(clinic_raw$age))
range(clinic_raw$age, na.rm = TRUE)
summary(clinic_raw)

library(ggplot2)
ggplot(clinic_raw, aes(x = age)) + 
  geom_histogram(bandwith = 5, na.rm = TRUE)

clinic_clean <- clinic_raw |> rename(id = patient_id)
clinic_clean <- clinic_clean |> select(id, site, sex, age, visit_date, blood_pressure, weight_kg, notes)
clinic_clean <- clinic_clean |> filter(age >= 18 | is.na(age))

clinic_clean |> arrange(age) |> head()
clinic_clean |> arrange(desc(age)) |> head()

clinic_clean |> group_by(site) |> summarise(mean_age = mean(age, na.rm = TRUE),
                                            n = n())
clinic_clean <- clinic_clean |> mutate(site = tolower(trimws(site)),
                                       site = gsub("_", " ", site))
table(clinic_clean$site)

substring("PT-0001", 4, 7)

library(stringr)
clinic_clean <- clinic_clean |>
  mutate (bp_upper = as.numeric(str_extract(blood_pressure, "^[0-9]+")),
          bp_lower = as.numeric(str_extract(blood_pressure, "(?<=/)[0-9]+")))
head(clinic_clean[,c("blood_pressure", "bp_upper", "bp_lower")])
gsub("[^0-9.]", "", "72.5kg")

grep("follow", clinic_clean$notes, ignore.case = TRUE)
clinic_clean |> filter(grepl("follow", notes, ignore.case = TRUE))

clinic_clean <- clinic_clean %>% 
  mutate(
    weight_kg = na_if(weight_kg, "-"),
    weight_kg = as.numeric(weight_kg),
    visit_date = as.Date(visit_date, format = "%d/%m%Y")
  )
head(clinic_clean)

clinic_clean <- clinic_clean %>%
  mutate(
    sex = ifelse(
      toupper(sex) == "M", "Male", 
      ifelse(toupper(sex) == "F", "Female", NA)
    )
  )

clinic_clean <- clinic_clean %>%
  mutate(
    bp_category = case_when (
      is.na(bp_upper) ~ NA_character_ , 
      bp_upper < 120 ~ "Normal", 
      bp_upper < 140 ~ "Elevated",
      TRUE ~ "High"
    )
  )
table(clinic_clean$bp_category, useNA = "always")

clean_text = function(x) {
  x = trimws(x)
  x = tolower(x)
  x
}

clean_text("  FOLLOW UP")

clinic_clean <- clinic_clean %>% 
  mutate(notes = clean_text(notes))
table(clinic_clean$notes, useNA = "always")

clinic_clean %>%
  select(id, bp_upper, bp_lower) %>%
  head(4) %>%
  tidyr::pivot_longer(
    cols = c(bp_upper, bp_lower), 
    names_to = "reading",
    values_to = "value"
  )

clinic_clean %>%
  group_by(site, sex) %>%
  summarise(mean_age = mean(age, na.rm = TRUE), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = sex, values_from = mean_age)

site_lookup <- data.frame(
  site = c("site a", "site b", "site c", "site d"),
  region = c("North", "North", "South", "South")
)

clinic_clean <- clinic_clean %>%
  left_join(site_lookup, by = "site")
head(clinic_clean)

write_csv(clinic_clean, "data/clinic_clean.csv")
