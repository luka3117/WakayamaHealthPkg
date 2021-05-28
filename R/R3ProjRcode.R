
# devtools::install_github(repo = "luka3117/JcPackage/OsakaUniv2020")

# 使用package
suppressMessages(library(readxl))
suppressMessages(library(dplyr))
suppressMessages(library(data.table))
suppressMessages(library(kableExtra))
suppressMessages(library(curl))
suppressMessages(library(tidyverse))
suppressMessages(library(plotly))
suppressMessages(library(ggrepel))


## ---- load data d -----------------
# ____        _          _                            _
# |  _ \  __ _| |_ __ _  (_)_ __ ___  _ __   ___  _ __| |_
# | | | |/ _` | __/ _` | | | '_ ` _ \| '_ \ / _ \| '__| __|
# | |_| | (_| | || (_| | | | | | | | | |_) | (_) | |  | |_
# |____/ \__,_|\__\__,_| |_|_| |_| |_| .__/ \___/|_|   \__|
#                                    |_|

## ---- load data d -----------------

file="https://raw.githubusercontent.com/luka3117/ClassData/master/Wakayama/DataFormat.csv"
d<-read_csv(url(file))
d<-d[-95,]
d %>% dim()


## ---- load data variable name : JpnEng.csv to var -----------------
## ---- change variable name  to Eng -----------------

varFile<-
  "https://raw.githubusercontent.com/luka3117/ClassData/master/Wakayama/JpnEng.csv"

var<-read_csv(url(varFile))

# var<-read.csv("/Users/jlee/Dropbox/00000健康和歌山県/0 wakayamaPkg/ClassData/Wakayama/JpnEng.csv")
colnames(d)<-var$var_name_Eng
# d %>% DT::datatable()


## 和英対応、出典など

var

## d_common data --------------------------------------------------

d_common<-d[, sapply(d[48,], is.na)]
d_common<-d_common[1:47, ]
d_common<-d %>% select(1:6) %>% .[1:47,] %>% bind_cols(d_common)
d_common %>% dim()
d_common %>% colnames()
d_common

## 男女分ける開始--------------------------------------------------
name <- function(x) {
  !is.na(x)
}
d_mf<-d[, sapply(d[48,], name)]
d_mf %>% dim()
d_m<-d_mf %>% filter(sex=="M")
d_f<-d_mf %>% filter(sex=="F")


## --------------------------------------------------
d_m

## --------------------------------------------------
d_f

## --------------------------------------------------
d_common %>% colnames() %>% enframe()

# ----------------- ----------------- ----------------- -----------------
#      _       _          _                            _                    _
#    _| | __ _| |_ __ _  (_)_ __ ___  _ __   ___  _ __| |_    ___ _ __   __| |
#  / _` |/ _` | __/ _` | | | '_ ` _ \| '_ \ / _ \| '__| __|  / _ \ '_ \ / _` |
# | (_| | (_| | || (_| | | | | | | | | |_) | (_) | |  | |_  |  __/ | | | (_| |
#  \__,_|\__,_|\__\__,_| |_|_| |_| |_| .__/ \___/|_|   \__|  \___|_| |_|\__,_|
#                                    |_|
# ----------------- ----------------- ----------------- -----------------


# ----------------- ----------------- ----------------- -----------------
# ____             _
# |  _ \ __ _ _ __ | | __
# | |_) / _` | '_ \| |/ /
# |  _ < (_| | | | |   <
# |_| \_\__,_|_| |_|_|\_\
# ----------------- ----------------- ----------------- -----------------



## 和歌山県順位 with common data--------------------------------------------------
d_common %>% dplyr::select_if(is.numeric) %>% sapply(rank) %>%
  tbl_df() %>%dplyr::filter(key==30) %>% t() %>% as.data.frame() %>%
  rownames_to_column() %>% tbl_df() %>%
  left_join(var, by=c("rowname"="var_name_Eng")) %>%
  # select(var_name_Jpn, V1, everything())
  select(var_name_Jpn, V1) %>%
  mutate(rank=V1) %>%select(-V1) %>% filter(rank<5 | rank>42)



## 和歌山県順位 with d_f data--------------------------------------------------
d_f %>% dplyr::select_if(is.numeric) %>% sapply(rank) %>%
  tbl_df() %>%dplyr::filter(key==30) %>% t() %>% as.data.frame() %>%
  rownames_to_column() %>% tbl_df() %>%
  left_join(var, by=c("rowname"="var_name_Eng")) %>%
  # select(var_name_Jpn, V1, everything())
  select(var_name_Jpn, V1) %>%
  mutate(rank=V1) %>%select(-V1) %>% filter(rank<5 | rank>42)


## 和歌山県順位 with d_m data--------------------------------------------------
d_m %>% dplyr::select_if(is.numeric) %>% sapply(rank) %>%
  tbl_df() %>%dplyr::filter(key==30) %>% t() %>% as.data.frame() %>%
  rownames_to_column() %>% tbl_df() %>%
  left_join(var, by=c("rowname"="var_name_Eng")) %>%
  # select(var_name_Jpn, V1, everything())
  select(var_name_Jpn, V1) %>%
  mutate(rank=V1) %>%select(-V1) %>% filter(rank<5 | rank>42)


# ----------------- ----------------- ----------------- -----------------
# ____  _                  _               _ _          _   _
# / ___|| |_ __ _ _ __   __| | __ _ _ __ __| (_)______ _| |_(_) ___  _ __
# \___ \| __/ _` | '_ \ / _` |/ _` | '__/ _` | |_  / _` | __| |/ _ \| '_ \
#  ___) | || (_| | | | | (_| | (_| | | | (_| | |/ / (_| | |_| | (_) | | | |
# |____/ \__\__,_|_| |_|\__,_|\__,_|_|  \__,_|_/___\__,_|\__|_|\___/|_| |_|
#
# ----------------- ----------------- ----------------- -----------------

## d_common 標準化--------------------------------------------------
# d_common_standarize
d_common %>% map_df(typeof) %>% t()
d_m %>% map_df(typeof) %>% t()
d_f %>% map_df(typeof) %>% t()


d_common_standarize<-
  d_common %>% dplyr::select_if(is.numeric) %>%
  select(-key, -pref.id) %>% scale() %>% tbl_df()

d_common_standarize<-bind_cols(d_common["pref.id"], d_common_standarize)



# ----------------- ----------------- ----------------- -----------------
# ____                              _
# |  _ \ ___  __ _ _ __ ___  ___ ___(_) ___  _ __
# | |_) / _ \/ _` | '__/ _ \/ __/ __| |/ _ \| '_ \
# |  _ <  __/ (_| | | |  __/\__ \__ \ | (_) | | | |
# |_| \_\___|\__, |_|  \___||___/___/_|\___/|_| |_|
#            |___/
# ----------------- ----------------- ----------------- -----------------
#


# d_f data 変数選択　and regression
# 変数選択のためのデータ分割

# LE_d_m_1,HLE_d_m_1,LE_d_f_1,HLE_d_f_1,LE_d_m_2,HLE_d_m_2,LE_d_f_2,HLE_d_f_2
# の8のデータセット生成

#
# LE_d_m <-  d_m %>% select(-1:-6) %>% select(LE_2015,-HLE_2016, everything())
# HLE_d_m <-  d_m %>% select(-1:-6) %>% select(HLE_2016, -LE_2015,  everything())
# LE_d_f <-  d_f %>% select(-1:-6) %>% select(LE_2015,-HLE_2016, everything())
# HLE_d_f <-  d_f %>% select(-1:-6) %>% select(HLE_2016,-LE_2015,  everything())
#

LE_d_m <-  d_m %>% select(-1:-6) %>% select(LE_2015,  everything())  %>% select(-HLE_2016)
HLE_d_m <-  d_m %>% select(-1:-6) %>% select(HLE_2016, everything())  %>% select(-LE_2015)
LE_d_f <-  d_f %>% select(-1:-6) %>% select(LE_2015,  everything())  %>% select(-HLE_2016)
HLE_d_f <-  d_f %>% select(-1:-6) %>% select(HLE_2016, everything())  %>% select(-LE_2015)


for (i in c("LE_d_m", "HLE_d_m", "LE_d_f", "HLE_d_f")) {
  assign(paste0(i, "_1"), eval(parse(text = i)) %>% select(1, 2:32))
  assign(paste0(i, "_2"), eval(parse(text = i)) %>% select(1, 33:63))
}



LE_d_common_m<-d_common %>% select(-1:-6) %>% bind_cols(d_m["LE_2015"]) %>% select(LE_2015, everything())
HLE_d_common_m<-d_common %>% select(-1:-6) %>% bind_cols(d_m["HLE_2016"]) %>% select(HLE_2016, everything())
LE_d_common_f<-d_common %>% select(-1:-6) %>% bind_cols(d_f["LE_2015"]) %>% select(LE_2015, everything())
HLE_d_common_f<-d_common %>% select(-1:-6) %>% bind_cols(d_f["HLE_2016"]) %>% select(HLE_2016, everything())


for (i in c("LE_d_common_m", "HLE_d_common_m", "LE_d_common_f", "HLE_d_common_f")) {
  assign(paste0(i, "_1"), eval(parse(text = i)) %>% select(1, 2:33))
  assign(paste0(i, "_2"), eval(parse(text = i)) %>% select(1, 34:66))
  assign(paste0(i, "_3"), eval(parse(text = i)) %>% select(1, 67:99))
}




# rm(LE_d_m_1,HLE_d_m_1,LE_d_f_1,HLE_d_f_1,LE_d_m_2,HLE_d_m_2,LE_d_f_2,HLE_d_f_2)




# ----------------- ----------------- ----------------- -----------------
# ____  _____ ____   _____                    _
# |  _ \| ____/ ___| |  ___|__ _ __ ___   __ _| | ___
# | |_) |  _|| |  _  | |_ / _ \ '_ ` _ \ / _` | |/ _ \
# |  _ <| |__| |_| | |  _|  __/ | | | | | (_| | |  __/
# |_| \_\_____\____| |_|  \___|_| |_| |_|\__,_|_|\___|
# ----------------- ----------------- ----------------- -----------------
# reg and variable selection : 1st screeening : female
# selected 40 variables with common data
# selected 20 variables with d_f data
# total : 60変数：女性
# ----------------- ----------------- ----------------- -----------------

# ----------------- ----------------- ----------------- -----------------


t1<-lm(LE_2015~. , data = LE_d_common_f_1) %>% MASS::stepAIC() %>% broom::tidy() %>% filter(p.value<.01) %>% select(term)
t2<-lm(LE_2015~. , data = LE_d_common_f_2) %>% MASS::stepAIC() %>% broom::tidy() %>% filter(p.value<.01) %>% select(term)
t3<-lm(LE_2015~. , data = LE_d_common_f_3) %>% MASS::stepAIC() %>% broom::tidy() %>% filter(p.value<.01) %>% select(term)
t4<-lm(HLE_2016~. , data = HLE_d_common_f_1) %>% MASS::stepAIC() %>% broom::tidy() %>% filter(p.value<.01) %>% select(term)
t5<-lm(HLE_2016~. , data = HLE_d_common_f_2) %>% MASS::stepAIC() %>% broom::tidy() %>% filter(p.value<.01) %>% select(term)
t6<-lm(HLE_2016~. , data = HLE_d_common_f_3) %>% MASS::stepAIC() %>% broom::tidy() %>% filter(p.value<.01) %>% select(term)


bind_rows(t1,t2,t3,t4,t5,t6) %>% unique() %>% .$term %>% c()
# selected 40 variables lists with female data
#
# [2] "Treatment_rate_Hospitalization_Malignant_neoplasm_2017"
# [3] "Treatment_rate_outpatient_heart_dz_2017"
# [4] "Treatment_rate_Outpatient_Cerebrovascular_dz_2017"
# [5] "Num_of_hospitals_2019"
# [6] "Num_of_clinics_2019"
# [7] "pop_oldElderly_pop_Ratio_2020"
# [8] "pop_Working_Age_pop_Ratio_2020"
# [9] "Natural_environment_annual_avg_temperature"
# [10] "Admin_base_balance_ratio"
# [11] "Admin_infrastructure_Edu_cost_ratio_prefectural_finance"
# [12] "Labor_secondary_industry_emp_ratio"
# [13] "Labor_Unemp_rate"
# [14] "Num_of_libraries"
# [15] "Residence_house_ratio"
# [16] "Volunteer_Activity_Participant_Rate"
# [17] "Residence_Num_of_city_parks"
# [18] "Residence_road_pavement_rate"
# [19] "HM_Num_of_public_health_nurses_per_100k_pop"
# [20] "Household_PC_ownership_quantity"
# [21] "Hypertension_Outpatient_2014"
# [22] "Dairy_2014"
# [23] "High_barrier_free_handrails_2018"
# [24] "barrier_free_wheelchairs_pass_Width"
# [25] "Total_salary_2016"
# [26] "Fish_meat_consumption_avg_2014_2016"
# [27] "Bone_density_disorder_2014"
# [28] "Num_of_cardiologists_2020"
# [29] "pop_Rough_Mortality_2020"
# [30] "Residence_sewerage_ratio"
# [31] "Residence_simachi_pavement_rate"
# [32] "Safety_Num_of_traffic_accidents_per_100k_pop"
# [33] "Household_actual_income"
# [34] "Household_consumption_expenditure"
# [35] "Household_Smartphone_ownership_quantity"
# [36] "Household_Tablet_terminal_Ownership_quantity"
# [37] "Diabetes_hospitalization_2014"
# [38] "High_barrier_free_rate_2018"
# [39] "Academic_ability_middle_school_2015"
# [40] "Academic_ability_elementary_school_2015"
# [41] "Alzheimer_dz_2014"




t7<-lm(LE_2015~. , data = LE_d_f_1) %>% MASS::stepAIC() %>% broom::tidy() %>% filter(p.value<.01) %>% select(term)
t8<-lm(LE_2015~. , data = LE_d_f_2) %>% MASS::stepAIC() %>% broom::tidy() %>% filter(p.value<.01) %>% select(term)
t9<-lm(HLE_2016~. , data = HLE_d_f_1) %>% MASS::stepAIC() %>% broom::tidy() %>% filter(p.value<.01) %>% select(term)
t10<-lm(HLE_2016~. , data = HLE_d_f_2) %>% MASS::stepAIC() %>% broom::tidy() %>% filter(p.value<.01) %>% select(term)

bind_rows(t7,t8,t9,t10) %>% unique() %>% .$term %>% c()
# selected 21 variables lists with female data
#
# [2] "Under_75_Adjusted_Mortality_Evil_Neoplasms_2019"
# [3] "mortality_heart_dz_2015"
# [4] "mortality_cerebrovascular_dz_2015"
# [5] "Read_rate_2016"
# [6] "Sports_Activity_rate_walking"
# [7] "Volunteer_town_development"
# [8] "Self_development_languages_other_than_Eng"
# [9] "Cerebrovascular_mortality_2015"
# [10] "Malignant_neoplasm_intestine_mortality_rate_2015"
# [11] "Heart_dz_mortality_2015"
# [12] "Pneumonia_mortality_2015"
# [13] "Eat_within_2_hours_before_sleep_binary_2014"
# [14] "Gain_10kg_Wt_compared_to_20yr_binary_2014"
# [15] "Travel_Traveling_Tourism_Activity_Rate"
# [16] "Volunteer_Activity_Rate_total"
# [17] "Volunteer_for_the_Elderly"
# [18] "Volunteer_for_Persons_with_Disabilities"
# [19] "Self_development_PC_etc"
# [20] "Malignant_neoplasm_lungs_mortality_2015"
# [21] "Use_Insulin_binary_2014"
# [22] "Physical_activity_walking_binary_2014"


#
#
# ~/D/0/0 wakayamaPkg ❯❯❯ figlet REG Male
# ----------------- ----------------- ----------------- -----------------
#  ____  _____ ____   __  __       _
# |  _ \| ____/ ___| |  \/  | __ _| | ___
# | |_) |  _|| |  _  | |\/| |/ _` | |/ _ \
# |  _ <| |__| |_| | | |  | | (_| | |  __/
# |_| \_\_____\____| |_|  |_|\__,_|_|\___|
# ----------------- ----------------- ----------------- -----------------
# reg and variable selection : 1st screeening : male
# selected 37 variables with common data
# selected 23 variables with d_m data
# total : 60変数：男性
# ----------------- ----------------- ----------------- -----------------


t11<-lm(LE_2015~. , data = LE_d_common_m_1) %>% MASS::stepAIC() %>% broom::tidy() %>% filter(p.value<.01) %>% select(term)
t12<-lm(LE_2015~. , data = LE_d_common_m_2) %>% MASS::stepAIC() %>% broom::tidy() %>% filter(p.value<.01) %>% select(term)
t13<-lm(LE_2015~. , data = LE_d_common_m_3) %>% MASS::stepAIC() %>% broom::tidy() %>% filter(p.value<.01) %>% select(term)
t14<-lm(HLE_2016~. , data = HLE_d_common_m_1) %>% MASS::stepAIC() %>% broom::tidy() %>% filter(p.value<.01) %>% select(term)
t15<-lm(HLE_2016~. , data = HLE_d_common_m_2) %>% MASS::stepAIC() %>% broom::tidy() %>% filter(p.value<.01) %>% select(term)
t16<-lm(HLE_2016~. , data = HLE_d_common_m_3) %>% MASS::stepAIC() %>% broom::tidy() %>% filter(p.value<.01) %>% select(term)


bind_rows(t11,t12,t13,t14,t15,t16) %>% unique() %>% .$term %>% c()
# selected 37 variables lists with male data
#
# [2] "Treatment_rate_Hospitalization_Malignant_neoplasm_2017"
# [3] "Medical_treatment_rate_hospitalization_heart_dz_2017"
# [4] "Treatment_rate_Outpatient_Malignant_neoplasm_2017"
# [5] "Num_of_clinics_2019"
# [6] "Book_purchase_price_2019"
# [7] "pop_Rough_Mortality_2020"
# [8] "pop_Double_income_household_ratio_2020"
# [9] "Natural_environment_annual_avg_temperature"
# [10] "Admin_base_Financial_strength_index"
# [11] "Edu_Ptc_of_university_graduate_students_with_a_final_academic_background"
# [12] "Num_of_general_clinics"
# [13] "Travel_Rate"
# [14] "HM_Num_of_public_health_nurses_per_100k_pop"
# [15] "Household_Savings"
# [16] "Household_Car_ownership_quantity"
# [17] "pop_Household_Ratio_of_elderly_single_person_households"
# [18] "Meat_2014"
# [19] "barrier_free_wheelchairs_pass_Width"
# [20] "Total_working_hours_2016"
# [21] "Total_salary_2016"
# [22] "Fish_meat_consumption_avg_2014_2016"
# [23] "Bone_density_disorder_2014"
# [24] "Treatment_rate_Hospitalization_Cerebrovascular_dz_2017"
# [25] "Treatment_rate_outpatient_heart_dz_2017"
# [26] "Num_of_hospitals_2019"
# [27] "Labor_primary_industry_emp_ratio"
# [28] "Residence_sewerage_ratio"
# [29] "Residence_Num_of_city_parks"
# [30] "Residence_simachi_pavement_rate"
# [31] "Household_Tablet_terminal_Ownership_quantity"
# [32] "Hypertension_Outpatient_2014"
# [33] "Diabetes_hospitalization_2014"
# [34] "Usual_barrier_free_rate_2018"
# [35] "High_barrier_free_rate_2018"
# [36] "High_barrier_free_handrails_2018"
# [37] "Academic_ability_middle_school_2015"
# [38] "Alzheimer_dz_2014"





LE_d_m_2<-LE_d_m_2 %>% select(-Malignant_neoplasm_breast_mortality_rate_2015,-Malignant_neoplasm_uterus_mortality_rate_2015)
HLE_d_m_2<-HLE_d_m_2 %>% select(-Malignant_neoplasm_breast_mortality_rate_2015,-Malignant_neoplasm_uterus_mortality_rate_2015)


t17<-lm(LE_2015~. , data = LE_d_m_1) %>% MASS::stepAIC() %>% broom::tidy() %>% filter(p.value<.01) %>% select(term)
t18<-lm(LE_2015~. , data = LE_d_m_2) %>% MASS::stepAIC() %>% broom::tidy() %>% filter(p.value<.01) %>% select(term)
t19<-lm(HLE_2016~. , data = HLE_d_m_1) %>% MASS::stepAIC() %>% broom::tidy() %>% filter(p.value<.01) %>% select(term)
t20<-lm(HLE_2016~. , data = HLE_d_m_2) %>% MASS::stepAIC() %>% broom::tidy() %>% filter(p.value<.01) %>% select(term)

bind_rows(t17,t18,t19,t20) %>% unique() %>% .$term %>% c()

# selected 28 variables lists with male data

#
# [2] "Mortality_Malignant_Neoplasm_2018_Under_75"
# [3] "Under_75_Adjusted_Mortality_Evil_Neoplasms_2019"
# [4] "mortality_heart_dz_2015"
# [5] "Sports_Activity_rate_walking"
# [6] "Tourism_Rate"
# [7] "Volunteer_for_Persons_with_Disabilities"
# [8] "Cerebrovascular_mortality_2015"
# [9] "Malignant_neoplasm_intestine_mortality_rate_2015"
# [10] "Malignant_neoplasm_liver_mortality_rate_2015"
# [11] "Pneumonia_mortality_2015"
# [12] "Use_blood_pressure_binary_2014"
# [13] "Physical_activity_walking_binary_2014"
# [14] "Sweat_exercise_twice_a_week_binary_2014"
# [15] "Smoking_over_100_binary_2014"
# [16] "Drinking_2_or_more_per_day_binary_2014"
# [17] "Skip_breakfast_three_times_a_week_binary_2014"
# [18] "Vegetable_intake_2016"
# [19] "BMI_2016"
# [20] "mortality_cerebrovascular_dz_2015"
# [21] "Read_rate_2016"
# [22] "Volunteer_health_and_medical"
# [23] "Volunteer_for_Children"
# [24] "Hobbies_Total"
# [25] "Self_development_art_culture"
# [26] "Self_development_languages_other_than_Eng"
# [27] "Self_development_PC_etc"
# [28] "Volunteer_environmental_activities"




# ----------------- ----------------- ----------------- -----------------
#  __     __         _       _     _
#  \ \   / /_ _ _ __(_) __ _| |__ | | ___
#   \ \ / / _` | '__| |/ _` | '_ \| |/ _ \
#    \ V / (_| | |  | | (_| | |_) | |  __/
#     \_/ \__,_|_|  |_|\__,_|_.__/|_|\___|
#
#  ____       _           _   _
# / ___|  ___| | ___  ___| |_(_) ___  _ __
# \___ \ / _ \ |/ _ \/ __| __| |/ _ \| '_ \
# ___) |  __/ |  __/ (__| |_| | (_) | | | |
# |____/ \___|_|\___|\___|\__|_|\___/|_| |_|
#
# _____ _   _ ____
# | ____| \ | |  _ \
# |  _| |  \| | | | |
# | |___| |\  | |_| |
# |_____|_| \_|____/
# ----------------- ----------------- ----------------- -----------------

#  女性データ処理
tt<-bind_rows(t1,t2,t3,t4,t5,t6) %>% unique() %>% .$term %>% .[-1]
LE_d_common_f_selected<-LE_d_common_f %>% select(LE_2015, tt)
HLE_d_common_f_selected<-HLE_d_common_f %>% select(HLE_2016, tt)

tt<-bind_rows(t7,t8,t9,t10)%>% unique() %>% .$term %>% .[-1]
LE_d_f_selected<-LE_d_f %>% select(LE_2015, tt)
HLE_d_f_selected<-HLE_d_f %>% select(HLE_2016, tt)

#  男性データ処理

tt<-bind_rows(t11,t12,t13,t14,t15,t16) %>% unique() %>% .$term %>% .[-1]
LE_d_common_m_selected<-LE_d_common_m %>% select(LE_2015, tt)
HLE_d_common_m_selected<-HLE_d_common_m %>% select(HLE_2016, tt)

tt<-bind_rows(t17,t18,t19,t20)%>% unique() %>% .$term %>% .[-1]
LE_d_m_selected<-LE_d_m %>% select(LE_2015, tt)
HLE_d_m_selected<-HLE_d_m %>% select(HLE_2016, tt)


LE_d_f_with_selected <- bind_cols(LE_d_common_f_selected, LE_d_f_selected[,-1])
HLE_d_f_with_selected <- bind_cols(HLE_d_common_f_selected, HLE_d_f_selected[,-1])


LE_d_m_with_selected <- bind_cols(LE_d_common_m_selected, LE_d_m_selected[,-1])
HLE_d_m_with_selected <- bind_cols(HLE_d_common_m_selected, HLE_d_m_selected[,-1])



LE_d_f_with_selected %>% dim()
HLE_d_f_with_selected %>% dim()
LE_d_m_with_selected %>% dim()
HLE_d_m_with_selected %>% dim()

# ----------------- ----------------- ----------------- -----------------
#  ____            _   ____                           _
# |___ \ _ __   __| | / ___|  ___ _ __ ___  ___ _ __ (_)_ __   __ _
#   __) | '_ \ / _` | \___ \ / __| '__/ _ \/ _ \ '_ \| | '_ \ / _` |
#  / __/| | | | (_| |  ___) | (__| | |  __/  __/ | | | | | | | (_| |
# |_____|_| |_|\__,_| |____/ \___|_|  \___|\___|_| |_|_|_| |_|\__, |
#                                                             |___/
# ----------------- ----------------- ----------------- -----------------
# ----------------- ----------------- ----------------- -----------------
# __     ___    ____    ____  _____ _                _ _   _
# \ \   / / \  |  _ \  / ___|| ____| |     __      _(_) |_| |__
#  \ \ / / _ \ | |_) | \___ \|  _| | |     \ \ /\ / / | __| '_ \
#   \ V / ___ \|  _ <   ___) | |___| |___   \ V  V /| | |_| | | |
#   \_/_/   \_\_| \_\ |____/|_____|_____|   \_/\_/ |_|\__|_| |_|
# ----------------- ----------------- ----------------- -----------------
# LE_d_common_f$
# bind_rows(t1,t2,t3,t4,t5,t6) %>% unique() %>% .$term %>% c()


#  _____ _____ __  __    _    _     _____
# |  ___| ____|  \/  |  / \  | |   | ____|
# | |_  |  _| | |\/| | / _ \ | |   |  _|
# |  _| | |___| |  | |/ ___ \| |___| |___
# |_|   |_____|_|  |_/_/   \_\_____|_____|
#

# p value less than .01 is deleted
lm(LE_2015~., data=LE_d_f_with_selected[,c(1, 2:30)]) %>% MASS::stepAIC() %>% broom::tidy() %>% filter(p.value<0.01)
lm(LE_2015~., data=LE_d_f_with_selected[,c(1, 31:62)]) %>% MASS::stepAIC() %>% broom::tidy()%>% filter(p.value<0.01)
lm(HLE_2016~., data=HLE_d_f_with_selected[,c(1, 2:30)]) %>% MASS::stepAIC() %>% broom::tidy() %>% filter(p.value<0.01)
lm(HLE_2016~., data=HLE_d_f_with_selected[,c(1, 31:62)]) %>% MASS::stepAIC() %>% broom::tidy()%>% filter(p.value<0.01)

temp1 <- lm(LE_2015~., data=LE_d_f_with_selected[,c(1, 2:30)]) %>% MASS::stepAIC() %>% broom::tidy() %>% filter(p.value<0.01)%>%select(term)
temp2 <- lm(LE_2015~., data=LE_d_f_with_selected[,c(1, 31:62)]) %>% MASS::stepAIC() %>% broom::tidy()%>% filter(p.value<0.01)%>%select(term)
temp3 <- lm(HLE_2016~., data=HLE_d_f_with_selected[,c(1, 2:30)]) %>% MASS::stepAIC() %>% broom::tidy() %>% filter(p.value<0.01)%>%select(term)
temp4 <- lm(HLE_2016~., data=HLE_d_f_with_selected[,c(1, 31:62)]) %>% MASS::stepAIC() %>% broom::tidy()%>% filter(p.value<0.01)%>%select(term)


tt<-bind_rows(temp1, temp2, temp3, temp4) %>% unique() %>% .[-1, ] %>% .$term %>% as.vector()

LE_d_f_with_selected_2nd_screen <- LE_d_common_f %>% bind_cols(LE_d_f[,-1]) %>% select(LE_2015, tt)
HLE_d_f_with_selected_2nd_screen <- HLE_d_common_f %>% bind_cols(HLE_d_f[,-1]) %>% select(HLE_2016, tt)


#  __  __    _    _     _____
# |  \/  |  / \  | |   | ____|
# | |\/| | / _ \ | |   |  _|
# | |  | |/ ___ \| |___| |___
# |_|  |_/_/   \_\_____|_____|
#


# p value less than .01 is deleted
lm(LE_2015~., data=LE_d_m_with_selected[,c(1, 2:32)]) %>% MASS::stepAIC() %>% broom::tidy() %>% filter(p.value<0.01)
lm(LE_2015~., data=LE_d_m_with_selected[,c(1, 33:65)]) %>% MASS::stepAIC() %>% broom::tidy()%>% filter(p.value<0.01)
lm(HLE_2016~., data=HLE_d_m_with_selected[,c(1, 2:32)]) %>% MASS::stepAIC() %>% broom::tidy() %>% filter(p.value<0.01)
lm(HLE_2016~., data=HLE_d_m_with_selected[,c(1, 33:65)]) %>% MASS::stepAIC() %>% broom::tidy()%>% filter(p.value<0.01)

temp1 <- lm(LE_2015~., data=LE_d_m_with_selected[,c(1, 2:32)]) %>% MASS::stepAIC() %>% broom::tidy() %>% filter(p.value<0.01)%>%select(term)
temp2 <- lm(LE_2015~., data=LE_d_m_with_selected[,c(1, 33:65)]) %>% MASS::stepAIC() %>% broom::tidy()%>% filter(p.value<0.01)%>%select(term)
temp3 <- lm(HLE_2016~., data=HLE_d_m_with_selected[,c(1, 2:32)]) %>% MASS::stepAIC() %>% broom::tidy() %>% filter(p.value<0.01)%>%select(term)
temp4 <- lm(HLE_2016~., data=HLE_d_m_with_selected[,c(1, 33:65)]) %>% MASS::stepAIC() %>% broom::tidy()%>% filter(p.value<0.01)%>%select(term)


tt<-bind_rows(temp1, temp2, temp3, temp4) %>% unique() %>% .[-1, ] %>% .$term %>% as.vector()

LE_d_m_with_selected_2nd_screen <- LE_d_common_m %>% bind_cols(LE_d_m[,-1]) %>% select(LE_2015, tt)
HLE_d_m_with_selected_2nd_screen <-HLE_d_common_m %>% bind_cols(HLE_d_m[,-1]) %>% select(HLE_2016, tt)


LE_d_f_with_selected_2nd_screen %>% colnames()
LE_d_m_with_selected_2nd_screen %>% colnames()

HLE_d_f_with_selected_2nd_screen %>% colnames()
HLE_d_m_with_selected_2nd_screen %>% colnames()


# ----------------- ----------------- ----------------- -----------------
#  ____            _   ____                           _
# |___ \ _ __   __| | / ___|  ___ _ __ ___  ___ _ __ (_)_ __   __ _
#   __) | '_ \ / _` | \___ \ / __| '__/ _ \/ _ \ '_ \| | '_ \ / _` |
#  / __/| | | | (_| |  ___) | (__| | |  __/  __/ | | | | | | | (_| |
# |_____|_| |_|\__,_| |____/ \___|_|  \___|\___|_| |_|_|_| |_|\__, |
#                                                             |___/
# ----------------- ----------------- ----------------- -----------------
# _____ _   _ ____
#| ____| \ | |  _ \
#|  _| |  \| | | | |
#| |___| |\  | |_| |
#|_____|_| \_|____/
#
# ----------------- ----------------- ----------------- -----------------

# ----------------- ----------------- ----------------- -----------------
#  ____          _   ____                           _
# |___ / _ __ __| | / ___|  ___ _ __ ___  ___ _ __ (_)_ __   __ _
#   |_ \| '__/ _` | \___ \ / __| '__/ _ \/ _ \ '_ \| | '_ \ / _` |
#  ___) | | | (_| |  ___) | (__| | |  __/  __/ | | | | | | | (_| |
# |____/|_|  \__,_| |____/ \___|_|  \___|\___|_| |_|_|_| |_|\__, |
#                                                           |___/
# ----------------- ----------------- ----------------- -----------------

# Work with this for obj
# 令和3年5月21日

# 4 obj after 2nd screen
LE_d_f_with_selected_2nd_screen #
LE_d_m_with_selected_2nd_screen

HLE_d_f_with_selected_2nd_screen
HLE_d_m_with_selected_2nd_screen

# 2nd screen data dim info

LE_d_f_with_selected_2nd_screen %>% dim()  # 47 28
HLE_d_f_with_selected_2nd_screen %>% dim()  # 47 28

LE_d_m_with_selected_2nd_screen %>% dim()  # 47 20
HLE_d_m_with_selected_2nd_screen %>% dim()  # 47 20

# 3rd screening start


#  _____ _____ __  __    _    _     _____
# |  ___| ____|  \/  |  / \  | |   | ____|
# | |_  |  _| | |\/| | / _ \ | |   |  _|
# |  _| | |___| |  | |/ ___ \| |___| |___
# |_|   |_____|_|  |_/_/   \_\_____|_____|
#


lm(LE_2015~., data = LE_d_f_with_selected_2nd_screen) %>% broom::tidy() %>% filter(p.value<.1)
lm(HLE_2016~., data = HLE_d_f_with_selected_2nd_screen) %>% broom::tidy() %>% filter(p.value<.1)

temp1 <- lm(LE_2015~., data = LE_d_f_with_selected_2nd_screen) %>% broom::tidy() %>% filter(p.value<.1)
temp2 <- lm(HLE_2016~., data = HLE_d_f_with_selected_2nd_screen) %>% broom::tidy() %>% filter(p.value<.1)

tt<-bind_rows(temp1[-1, ], temp2[-1, ]) %>% .$term %>%unique() %>%  as.vector()

LE_d_f_with_selected_3rd_screen <- LE_d_common_f %>% bind_cols(LE_d_f[,-1]) %>% select(LE_2015, tt)
HLE_d_f_with_selected_3rd_screen <-HLE_d_common_f %>% bind_cols(HLE_d_f[,-1]) %>% select(HLE_2016, tt)




#  __  __    _    _     _____
# |  \/  |  / \  | |   | ____|
# | |\/| | / _ \ | |   |  _|
# | |  | |/ ___ \| |___| |___
# |_|  |_/_/   \_\_____|_____|
#


lm(LE_2015~., data = LE_d_m_with_selected_2nd_screen) %>% broom::tidy() %>% filter(p.value<.1)
lm(HLE_2016~., data = HLE_d_m_with_selected_2nd_screen) %>% broom::tidy() %>% filter(p.value<.1)

temp1 <- lm(LE_2015~., data = LE_d_m_with_selected_2nd_screen) %>% broom::tidy() %>% filter(p.value<.1)
temp2 <- lm(HLE_2016~., data = HLE_d_m_with_selected_2nd_screen) %>% broom::tidy() %>% filter(p.value<.1)

tt<-bind_rows(temp1[-1, ], temp2[-1, ]) %>% .$term %>%unique() %>%  as.vector()

LE_d_m_with_selected_3rd_screen <- LE_d_common_m %>% bind_cols(LE_d_m[,-1]) %>% select(LE_2015, tt)
HLE_d_m_with_selected_3rd_screen <-HLE_d_common_m %>% bind_cols(HLE_d_m[,-1]) %>% select(HLE_2016, tt)


# 3rd screen data dim info

LE_d_f_with_selected_3rd_screen %>% dim()
HLE_d_f_with_selected_3rd_screen %>% dim()
LE_d_m_with_selected_3rd_screen %>% dim()
HLE_d_m_with_selected_3rd_screen %>% dim()


LE_d_f_with_selected_3rd_screen %>% colnames() %>% cat()
HLE_d_f_with_selected_3rd_screen %>% colnames() %>% cat()
LE_d_m_with_selected_3rd_screen %>% colnames() %>% cat()
HLE_d_m_with_selected_3rd_screen %>% colnames() %>% cat()




# ----------------- ----------------- ----------------- -----------------
#  _____         _   ____                           _
# |___ / _ __ __| | / ___|  ___ _ __ ___  ___ _ __ (_)_ __   __ _
#   |_ \| '__/ _` | \___ \ / __| '__/ _ \/ _ \ '_ \| | '_ \ / _` |
#  ___) | | | (_| |  ___) | (__| | |  __/  __/ | | | | | | | (_| |
# |____/|_|  \__,_| |____/ \___|_|  \___|\___|_| |_|_|_| |_|\__, |
#                                                           |___/
# ----------------- ----------------- ----------------- -----------------
#  _____ _   _ ____
# | ____| \ | |  _ \
# |  _| |  \| | | | |
# | |___| |\  | |_| |
# |_____|_| \_|____/
# ----------------- ----------------- ----------------- -----------------




# ----------------- ----------------- ----------------- -----------------
#  _____           _
# |_   _|__     __| | ___
#   | |/ _ \   / _` |/ _ \
#   | | (_) | | (_| | (_) |  _ _
#   |_|\___/   \__,_|\___/  (_|_)
#




## ---- warning=FALSE--------------------------------
jc_shapiro <- function(x) {
  shapiro.test(x) %>% broom::tidy() %>% select(p.value) %>% round(3)
}


d_common_normality_test<-d_common %>% dplyr::select_if(is.numeric) %>%
  select(-key, -pref.id) %>%
  # scale() %>%







## ---- warning=FALSE--------------------------------
jc_shapiro <- function(x) {
  shapiro.test(x) %>% broom::tidy() %>% select(p.value) %>% round(3)
}


d_common_normality_test<-d_common %>% dplyr::select_if(is.numeric) %>%
  select(-key, -pref.id) %>%
  # scale() %>%
  tbl_df() %>% purrr::map_df(jc_shapiro)

rownames(d_common_normality_test)<-d_common %>% dplyr::select_if(is.numeric) %>%
  select(-key, -pref.id) %>% colnames()

DT::datatable(d_common_normality_test)


## --------------------------------------------------
d_common_normality_test %>% filter(p.value<0.05) %>% DT::datatable()










d_common_standarize_long<-

  d_common_standarize %>% dplyr::select_if(is.numeric) %>%
  pivot_longer(
    cols = -pref.id,
    names_to ="var_name" ,
    values_to ="value"
  )


plot_ly(
  x =d_common_standarize_long$value,
  type = "histogram",
  name = "Histogram",
  frame =  ~d_common_standarize_long$var_name
)




## --------------------------------------------------
name1 <- function(temp) {
  temp %>% select(value) %>% .[[1]] %>% density() %>%
    broom::tidy()
}

d_common_standarize_long_density<-
  d_common_standarize_long %>% group_by(var_name) %>% nest() %>% mutate(dens=map(data, name1)) %>% select(var_name, dens) %>% unnest()

d_common_standarize_long_density %>% DT::datatable()

d_common_standarize_long_density %>%
  plot_ly(x=~x, y=~y) %>%
  # add_lines(frame=~var_name)
  add_lines(frame=~var_name,
            text = ~paste(var_name),
            hoverinfo = "text"
  ) %>%
  layout(title="分布")

# add_lines(color=~status) %>%
# storms %>%




## --------------------------------------------------
library(plotly)
temp<-left_join(d_common_standarize_long, d_common,
                by = c("pref.id"="key")) %>% select(pref.J, 2,3)


temp %>% filter(pref.J=="和歌山"|
                  pref.J=="青森"|
                  pref.J=="滋賀"|
                  pref.J=="長野") %>%
  plot_ly(y =  ~ pref.J,
          x =  ~ value,
          color =  ~ pref.J) %>%
  add_bars(frame=~var_name,
           hoverinfo="text",
           text=~paste(var_name))



ls()
