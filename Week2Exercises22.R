# Week2, Exercise 2.2

clinic_raw <- read_csv("data/clinic_raw.csv", skip = 2)
str(clinic_raw)
summary(clinic_raw)

clinic_clean <- clinic_raw |>
  # Clean weight_kg column and change to numeric
  mutate(weight_kg = na_if(weight_kg, "-"),
         weight_kg = as.numeric(weight_kg)) |>
  # Clean sex column
  mutate(sex = toupper(sex)) |>
  # Clean site column 
  mutate(site = tolower(trimws(site)),
         site = gsub("_", " ", site)) |>
  # Clean date column 
  mutate(visit_date = as.Date(visit_date, format = "%d/%m/%Y")) |>
  # Clean BP column 
  mutate(sbp = as.numeric(str_extract(blood_pressure, "^[0-9]+")),
         dbp = as.numeric(str_extract(blood_pressure, "(?<=/)[0-9]+"))) |>
  # Clean notes column 
  mutate (notes = tolower(trimws(notes)))

str(clinic_clean)
summary(clinic_clean)

clinic_clean |>
  group_by(site) |>
  summarise (mean_age = mean(age, na.rm = TRUE), n = n())

write_csv(clinic_clean, "data/clinic_clean_ex.csv")
