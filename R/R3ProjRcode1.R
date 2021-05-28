
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

load("ScreenEnd.RData")

# only the followings are valid obj
# d , var, pref_id
# d_f_merge_X, d_m_merge_X, y_f , y_m
# LE_d_f_final, HLE_d_f_final, LE_d_m_final, HLE_d_m_final


# ----------------- ----------------- ----------------- -----------------
#  _____ _             _   _     __  __
# |  ___(_)_ __   __ _| | | |   |  \/  |
# | |_  | | '_ \ / _` | | | |   | |\/| |
# |  _| | | | | | (_| | | | |___| |  | |
# |_|   |_|_| |_|\__,_|_| |_____|_|  |_|
# ----------------- ----------------- ----------------- -----------------

# -----------------
# Q1:Do the final regression and interprete : with 4 data
# -----------------

var$var_name_Jpn
lm(LE_2015~. , data = LE_d_f_final) %>%
  broom::tidy() %>%
  left_join(var, by=c("term"="var_name_Eng")) %>%
  select(term, var_name_Jpn, everything()) %>% DT::datatable()

  left_join(var, by=c("rowname"="var_name_Eng")) %>%
 %>% left_join()
lm(LE_2015~. , data = LE_d_f_final) %>% broom::glance()


system("open .")


LE_d_m_final,
HLE_d_f_final,
 HLE_d_m_final





# -----------------
# Q2:logistic regression the final regression and interprete
# -----------------


# try glmm and  interprete

# find correlated variable



















