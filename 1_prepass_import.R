library(tidyverse)
library(readxl)
library(lubridate)
library(labelled)

source("0_importfunctions.R")

# ==============================================================================
# Import data2.xlsx
# ==============================================================================

data2_raw <- read_excel("raw_data/data_revised.xlsx", col_types = "text")

# Store original column names for labels
original_names <- names(data2_raw)

# Create short snake_case variable names
new_names <- c(
  "notes",                # Notes
  "ecmo_yn",              # ECMO initiated 1:Yes 2:No
  "cohort",               # Cohort
  "ems_agency",           # 911 EMS Agency
  "patient_id",           # Patient ID
  "age",                  # Age at arrest
  "sex",                  # M=1, F=2
  "race",                 # Race 1:White, 2:AA, 3:Hispanic, 4: Native American, 5:Hmong, 6:Asian, 7:Other
  "bmi",                  # BMI
  "arrest_date",          # Date of cardiac arrest
  "call_911_dt",          # Date and time original 911 call
  "ohca_time",            # Time of OHCA (if after 911 call)
  "ems_enroute_dt",       # Date and time EMS en route
  "ems_scene_dt",         # Date and time EMS on scene
  "ems_depart_dt",        # Date and time EMS left scene to transport pt to hospital
  "ems_hospital_dt",      # Date and time EMS at hospital
  "init_rhythm",          # Initial Rhythm
  "witnessed",            # Witnessed
  "bystander_cpr",        # Bystander CPR
  "lucas",                # Was LUCAS placed?
  "field_intubation",     # Was pt intubated with ANY airway in the field
  "intubation_type",      # What was used for field intubation
  "n_shocks",             # Number of shocks
  "hems_request",         # Time of HEMS request
  "hems_enroute",         # HEMS Enroute
  "hems_scene",           # HEMS time on scene
  "hems_depart",          # HEMS time leaving scene to transport to hospital
  "hems_scene_time",      # HEMS scene time
  "hems_flight_time",     # HEMS patient loaded flight time
  "ground_transport_time",# Predicted Ground transport time
  "ecmo_time",            # Needle stick to ECMO time (mins)
  "lactate",              # Lactate
  "ph",                   # pH
  "low_flow_time",        # Low flow time
  "ecmo_days",            # Days on VA-ECMO
  "los_days",             # Total_Hospital_LOS (days)
  "alive",                # 0:Dead, 1:Alive
  "discharge_dest",       # 0=died; 1=home; 2: ARU; 3: TCU; 4: LTAC; 5: Other hospital
  "cpc",                  # CPC at discharge
  "htn",                  # HTN
  "dm",                   # DM
  "hyperlipidemia",       # Hyperlipidemia
  "cad",                  # CAD history
  "hf",                   # HF history
  "afib",                 # A-fib
  "ckd",                  # CKD
  "smoking",              # Smoking
  "copd_asthma",          # COPD or asthma
  "stroke",               # Stroke
  "icm"                   # 1-ICM; 2: NICM
)

names(data2_raw) <- new_names

# Set original column names as labels
var_label(data2_raw) <- setNames(as.list(original_names), new_names)

# ==============================================================================
# Apply type parsing
# ==============================================================================

data <- data2_raw %>%
  #Ensure no extraneous rows remain
  filter(!is.na(cohort)) %>%
  mutate(
    # Numeric variables
    age = as.numeric(age),
    sex = as.integer(sex),
    race = as.integer(race),
    bmi = as.numeric(bmi),
    init_rhythm = parse_int_robust(init_rhythm),
    witnessed = parse_binary(witnessed),
    ecmo_yn = parse_binary(ecmo_yn),
    bystander_cpr = parse_binary(bystander_cpr),
    lucas = parse_binary(lucas),
    field_intubation = parse_binary(field_intubation),
    n_shocks = as.numeric(n_shocks),
    hems_scene_time = parse_num_robust(hems_scene_time),
    hems_flight_time = as.numeric(hems_flight_time),
    ground_transport_time = as.numeric(ground_transport_time),
    ecmo_time = parse_num_robust(ecmo_time),
    lactate = parse_num_robust(lactate),
    ph = parse_num_robust(ph),
    low_flow_time = as.numeric(low_flow_time),
    ecmo_days = as.numeric(ecmo_days),
    los_days = as.numeric(los_days),
    alive = as.integer(alive),
    discharge_dest = as.integer(discharge_dest),
    cpc = as.integer(cpc),
    htn = as.integer(htn),
    dm = as.integer(dm),
    hyperlipidemia = as.integer(hyperlipidemia),
    cad = as.integer(cad),
    hf = as.integer(hf),
    afib = as.integer(afib),
    ckd = as.integer(ckd),
    smoking = as.integer(smoking),
    copd_asthma = as.integer(copd_asthma),
    stroke = as.integer(stroke),
    icm = as.integer(icm),

    # Date variable
    arrest_date = parse_mixed_date(arrest_date),

    # Datetime variables
    call_911_dt = parse_mixed_datetime(call_911_dt),
    ems_enroute_dt = parse_mixed_datetime(ems_enroute_dt),
    ems_scene_dt = parse_mixed_datetime(ems_scene_dt),
    ems_depart_dt = parse_mixed_datetime(ems_depart_dt),
    ems_hospital_dt = parse_mixed_datetime(ems_hospital_dt),

    # Time variables
    ohca_time = parse_mixed_time(ohca_time),
    hems_request = parse_mixed_time(hems_request),
    hems_enroute = parse_mixed_time(hems_enroute),
    hems_scene = parse_mixed_time(hems_scene),
    hems_depart = parse_mixed_time(hems_depart)
  )

# Summary
cat("data imported:", nrow(data), "rows x", ncol(data), "columns\n")

# ==============================================================================
# Missingness
# ==============================================================================

# Identify columns that are 100% null
null_cols_100 <- names(data)[sapply(data, function(x) mean(is.na(x)) == 1)]
cat("\nColumns with 100% missing values (will be removed):\n")
print(null_cols_100)

# Remove 100% null columns from data
data <- data %>% select(-all_of(null_cols_100))

# Restore labels for remaining columns
label_lookup <- setNames(original_names, new_names)
remaining_labels <- label_lookup[names(data)]
var_label(data) <- as.list(remaining_labels)

# Identify columns that are 85-99% null
null_cols_85 <- names(data)[sapply(data, function(x) {
  pct <- mean(is.na(x))
  pct >= 0.85 & pct < 1.0
})]
cat("\nColumns with 85-99% missing values:\n")
print(null_cols_85)

# Missingness summary for all variables
missing_summary <- data.frame(
  variable = names(data),
  pct_missing = sapply(data, function(x) round(mean(is.na(x)) * 100, 1))
) %>%
  arrange(desc(pct_missing))
print(missing_summary)

# Check for parsing warnings
cat("\n=== DPLYR PARSING WARNINGS ===\n")
dplyr::last_dplyr_warnings(n = 10)

# ==============================================================================
# Save cleaned data
# ==============================================================================
save(data, file="clean_data/data.RData")
