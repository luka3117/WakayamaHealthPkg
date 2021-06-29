# devtools::install_github(repo = "luka3117/JcPackage/OsakaUniv2020")


## ----sink example  ------------------------------
# ____  _       _      _____
# / ___|(_)_ __ | | __ | ____|_  __
# \___ \| | '_ \| |/ / |  _| \ \/ /
#  ___) | | | | |   <  | |___ >  <
# |____/|_|_| |_|_|\_\ |_____/_/\_\
# -----------------
# ref for Q and A
# https://stackoverflow.com/questions/33994194/changing-the-font-size-of-table-using-print-xtable
# print(xtable(results), only.contents=TRUE, include.rownames=F,        include.colnames=T, floating=F,       hline.after=NULL, size="\\fontsize{9pt}{10pt}\\selectfont",       file = '~/Dropbox/Paper/table.tex')


library(xtable)

## ----tex file sink example ------------------------------
# table tex saving folder
path="./final_report-kenko(和歌山県)/table/"
# table tex file name
file="example.tex"

sink(file = paste0(path, file))
xtable(iris[1:5, ], label = "tablelabel", caption = c(
  "$\\beta_0
  X_1+
  \\beta_0 X_2$
  寿命", "bbb")) %>% print(size="\\tiny")
sink()

## ---- Pre material ------------------------------

# # -----------------
#  ____           __  __       _            _       _
# |  _ \ _ __ ___|  \/  | __ _| |_ ___ _ __(_) __ _| |
# | |_) | '__/ _ \ |\/| |/ _` | __/ _ \ '__| |/ _` | |
# |  __/| | |  __/ |  | | (_| | ||  __/ |  | | (_| | |
# |_|   |_|  \___|_|  |_|\__,_|\__\___|_|  |_|\__,_|_|
# # -----------------
# 使用package
suppressMessages(library(readxl))
suppressMessages(library(dplyr))
suppressMessages(library(data.table))
suppressMessages(library(kableExtra))
suppressMessages(library(curl))
suppressMessages(library(tidyverse))
suppressMessages(library(plotly))
suppressMessages(library(ggrepel))
suppressMessages(library(magrittr))


# load("../ScreenEnd.RData")
load("ScreenEnd.RData")

# -----------------
#  _   _              _  __     ___    ____
# | | | |___  ___  __| | \ \   / / \  |  _ \
# | | | / __|/ _ \/ _` |  \ \ / / _ \ | |_) |
# | |_| \__ \  __/ (_| |   \ V / ___ \|  _ <
#  \___/|___/\___|\__,_|    \_/_/   \_\_| \_\
# -----------------


f_var=LE_d_f_final %>% colnames() %>% tbl_df() %>% left_join(var, by=c("value"="var_name_Eng")) %>%
  select(3) %>% filter(!is.na(var_name_Jpn)) %>% rename(f_var=var_name_Jpn) %>% .[-1, ]

m_var=LE_d_m_final %>% colnames() %>% tbl_df() %>% left_join(var, by=c("value"="var_name_Eng")) %>%
  select(3) %>% filter(!is.na(var_name_Jpn)) %>% rename(m_var=var_name_Jpn) %>% .[-1, ]
m_var %<>% bind_rows(as.data.frame(rep(NA, 7)))
m_var %<>% select(1)



# table tex file name
file="UsedVar.tex"
# path="./final_report-kenko(和歌山県)/table/"

sink(file = paste0(path, file))
f_var %>%
  bind_cols(m_var) %>%
  xtable(label = "UsedVariable",
         caption = c("$\\beta_0 X_1+\\beta_0 X_2$寿命",
                     "bbb")) %>%
  print(size = "\\tiny")
sink()




LE_d_m_final %>% colnames() %>% tbl_df() %>% left_join(var, by=c("value"="var_name_Eng")) %>%
  select(3) %>% filter(!is.na(var_name_Jpn)) %>% rename(female_var=var_name_Jpn) %>% .[-1, ]


LE_d_m_final %>% colnames() %>% tbl_df() %>% left_join(var, by=c("value"="var_name_Eng")) %>%
  select(3)

## ----warning=FALSE------------------------------

# -----------------
# Q1:Do the final regression and interprete : with 4 data
# -----------------
# ----------------- ----------------- ----------------- -----------------
#  _____ _             _   _     __  __
# |  ___(_)_ __   __ _| | | |   |  \/  |
# | |_  | | '_ \ / _` | | | |   | |\/| |
# |  _| | | | | | (_| | | | |___| |  | |
# |_|   |_|_| |_|\__,_|_| |_____|_|  |_|
# ----------------- ----------------- ----------------- -----------------


# file="table_LM_HLE_mf.tex"


options(digits = 5)              # Modify global options


fit_with_X_lm_LE_d_f<-lm(LE_2015~. , data = LE_d_f_final)

lm(LE_2015~. , data = LE_d_f_final) %>%
  broom::tidy() %>%
  left_join(var, by=c("term"="var_name_Eng")) %>%
  select(term, var_name_Jpn, everything(), -id,- address, -std.error, -columm_letter ) %>% DT::datatable() %>% DT::formatRound(columns=c('estimate', 'statistic', "p.value"), digits=3)


# table tex file name
file="table_LM_LE_f.tex"
# path="./final_report-kenko(和歌山県)/table/"

sink(file = paste0(path, file))
fit_with_X_lm_LE_d_f %>% broom::tidy() %>%
  left_join(var, by=c("term"="var_name_Eng")) %>%
  select(term, var_name_Jpn, everything(), -id,- address, -std.error, -columm_letter ) %>%
    xtable(label = "UsualLMLEf",
         caption = c("女性の線形回帰(平均寿命)")) %>%
  print(size = "\\tiny")
sink()



## ----warning=FALSE------------------------------
options(digits = 5)              # Modify global options

fit_with_X_lm_LE_d_m<-lm(LE_2015~. , data = LE_d_m_final)

lm(LE_2015~. , data = LE_d_m_final) %>%
  broom::tidy() %>%
  left_join(var, by=c("term"="var_name_Eng")) %>%
  select(term, var_name_Jpn, everything(), -id,- address, -std.error, -columm_letter ) %>% DT::datatable() %>% DT::formatRound(columns=c('estimate', 'statistic', "p.value"), digits=3)



# table tex file name
file="table_LM_LE_m.tex"
# path="./final_report-kenko(和歌山県)/table/"

sink(file = paste0(path, file))
fit_with_X_lm_LE_d_m %>% broom::tidy() %>%
  left_join(var, by=c("term"="var_name_Eng")) %>%
  select(term, var_name_Jpn, everything(), -id,- address, -std.error, -columm_letter ) %>%
    xtable(label = "UsualLMLEm",
         caption = c("男性の線形回帰(平均寿命)")) %>%
  print(size = "\\tiny")
sink()


## ----warning=FALSE------------------------------

options(digits = 5)              # Modify global options

fit_with_X_lm_HLE_d_f<-lm(HLE_2016~. , data = HLE_d_f_final)


lm(HLE_2016~. , data = HLE_d_f_final) %>%
  broom::tidy() %>%
  left_join(var, by=c("term"="var_name_Eng")) %>%
  select(term, var_name_Jpn, everything(), -id,- address, -std.error, -columm_letter ) %>% DT::datatable() %>% DT::formatRound(columns=c('estimate', 'statistic', "p.value"), digits=3)


# table tex file name
file="table_LM_HLE_f.tex"
# path="./final_report-kenko(和歌山県)/table/"

sink(file = paste0(path, file))
fit_with_X_lm_HLE_d_f %>% broom::tidy() %>%
  left_join(var, by=c("term"="var_name_Eng")) %>%
  select(term, var_name_Jpn, everything(), -id,- address, -std.error, -columm_letter ) %>%
    xtable(label = "UsualHLMLEf",
         caption = c("女性の線形回帰(健康寿命)")) %>%
  print(size = "\\tiny")
sink()

## ----warning=FALSE------------------------------
options(digits = 5)              # Modify global options

fit_with_X_lm_HLE_d_m<-lm(HLE_2016~. , data = HLE_d_m_final)

lm(HLE_2016~. , data = HLE_d_m_final) %>%
  broom::tidy() %>%
  left_join(var, by=c("term"="var_name_Eng")) %>%
  select(term, var_name_Jpn, everything(), -id,- address, -std.error, -columm_letter ) %>% DT::datatable() %>% DT::formatRound(columns=c('estimate', 'statistic', "p.value"), digits=3)


# -----------------# -----------------
# table tex file name
file="table_LM_HLE_m.tex"
# path="./final_report-kenko(和歌山県)/table/"

sink(file = paste0(path, file))
fit_with_X_lm_HLE_d_m %>% broom::tidy() %>%
  left_join(var, by=c("term"="var_name_Eng")) %>%
  select(term, var_name_Jpn, everything(), -id,- address, -std.error, -columm_letter ) %>%
    xtable(label = "UsualHLMLEf",
         caption = c("男性の線形回帰(健康寿命)")) %>%
  print(size = "\\tiny")
sink()
# -----------------# -----------------



# -----------------
# -----------------
# -----------------
# -----------------
# -----------------
# _____ _
# |  ___/ \
# | |_ / _ \
# |  _/ ___ \
# |_|/_/   \_\
# -----------------
# -----------------
# -----------------
# -----------------


## ----warning=FALSE------------------------------
LE_d_f_final.reg<-lm(LE_2015~. , data = LE_d_f_final)
d_f_.FA<-LE_d_f_final.reg$model[,-1] %>% zemi::JcFA()
rownames(d_f_.FA$VAR.rotate)<-colnames(LE_d_f_final.reg$model[,-1] )
d_f_.FA$VAR.rotate %>% data.frame() %>% rownames_to_column() %>%
  left_join(var, by=c("rowname"="var_name_Eng")) %>%
  select(rowname,var_name_Jpn,X1,X2) %>% rename(F1=X1, F2=X2) %>%
  DT::datatable() %>% DT::formatRound(columns=c('F1', 'F2'), digits=3)

# -----------------# -----------------
# table tex file name
file="table_FA_f.tex"
# path="./final_report-kenko(和歌山県)/table/"

sink(file = paste0(path, file))
rownames(d_f_.FA$VAR.rotate)<-colnames(LE_d_f_final.reg$model[,-1] )
d_f_.FA$VAR.rotate %>% data.frame() %>% rownames_to_column() %>%
  left_join(var, by=c("rowname"="var_name_Eng")) %>%
  select(rowname,var_name_Jpn,X1,X2) %>% rename(F1=X1, F2=X2)
    xtable(label = "FAf",
         caption = c("女性のFA")) %>%
  print(size = "\\tiny")
sink()
# -----------------# -----------------


## ----warning=FALSE------------------------------
d_f_.FA$OBS.rotate %>% as.data.frame() %>% tbl_df() %>%
  rename(F1=V1, F2=V2) %>%
  mutate(pref.J=d_common$pref.J)%>% select(pref.J, everything()) %>%
  DT::datatable() %>% DT::formatRound(columns=c('F1', 'F2'), digits=3)


## ----warning=FALSE------------------------------
LE_d_m_final.reg<-lm(LE_2015~. , data = LE_d_m_final)
d_m_.FA<-LE_d_m_final.reg$model[,-1] %>% zemi::JcFA()
rownames(d_m_.FA$VAR.rotate)<-colnames(LE_d_m_final.reg$model[,-1] )
d_m_.FA$VAR.rotate %>% data.frame() %>% rownames_to_column() %>%
  left_join(var, by=c("rowname"="var_name_Eng")) %>%
  select(rowname,var_name_Jpn,X1,X2) %>% rename(F1=X1, F2=X2) %>%
  DT::datatable() %>% DT::formatRound(columns=c('F1', 'F2'), digits=3)


## ----warning=FALSE------------------------------
d_m_.FA$OBS.rotate %>% as.data.frame() %>% tbl_df() %>%
  rename(F1=V1, F2=V2) %>%
  mutate(pref.J=d_common$pref.J)%>% select(pref.J, everything()) %>%
  DT::datatable() %>% DT::formatRound(columns=c('F1', 'F2'), digits=3)



# -----------------# -----------------
# table tex file name
file="table_FA_m.tex"
# path="./final_report-kenko(和歌山県)/table/"

sink(file = paste0(path, file))
rownames(d_m_.FA$VAR.rotate)<-colnames(LE_d_m_final.reg$model[,-1] )
d_m_.FA$VAR.rotate %>% data.frame() %>% rownames_to_column() %>%
  left_join(var, by=c("rowname"="var_name_Eng")) %>%
  select(rowname,var_name_Jpn,X1,X2) %>% rename(F1=X1, F2=X2)
    xtable(label = "FAm",
         caption = c("男性のFA")) %>%
  print(size = "\\tiny")
sink()
# -----------------# -----------------




# -----------------
# -----------------
# -----------------
# -----------------
# -----------------
# _     __  __            _ _   _       _____ _
# | |   |  \/  | __      _(_) |_| |__   |  ___/ \
# | |   | |\/| | \ \ /\ / / | __| '_ \  | |_ / _ \
# | |___| |  | |  \ V  V /| | |_| | | | |  _/ ___ \
# |_____|_|  |_|   \_/\_/ |_|\__|_| |_| |_|/_/   \_\
# -----------------
# -----------------
# -----------------
# -----------------

## ----warning=FALSE------------------------------
fit_with_FA_lm_LE_d_f<-lm(LE_d_f_final$LE_2015~d_f_.FA$OBS.rotate)

lm(LE_d_f_final$LE_2015~d_f_.FA$OBS.rotate) %>% broom::tidy() %>% DT::datatable() %>% DT::formatRound(columns=c('estimate', 'std.error', 'statistic', 'p.value'), digits=3)
lm(LE_d_f_final$LE_2015~d_f_.FA$OBS.rotate) %>% broom::glance()


# -----------------# -----------------
# table tex file name
file="table_LM_LE_FA_f.tex"
# path="./final_report-kenko(和歌山県)/table/"

sink(file = paste0(path, file))
fit_with_FA_lm_LE_d_f %>% broom::tidy() %>%
    xtable(label = "tableLMLEFAf",
         caption = c("女性の回帰withFA(平均寿命)")) %>%
  print(size = "\\tiny")
sink()
# -----------------# -----------------


## ----warning=FALSE------------------------------
fit_with_FA_lm_LE_d_m<-lm(LE_d_m_final$LE_2015~d_m_.FA$OBS.rotate)

lm(LE_d_m_final$LE_2015~d_m_.FA$OBS.rotate) %>% broom::tidy()%>% DT::datatable() %>% DT::formatRound(columns=c('estimate', 'std.error', 'statistic', 'p.value'), digits=3)
lm(LE_d_m_final$LE_2015~d_m_.FA$OBS.rotate) %>% broom::glance()


# -----------------# -----------------
# table tex file name
file="table_LM_LE_FA_m.tex"
# path="./final_report-kenko(和歌山県)/table/"

sink(file = paste0(path, file))
fit_with_FA_lm_LE_d_m %>% broom::tidy() %>%
    xtable(label = "tableLMLEFAm",
         caption = c("男性の回帰withFA(平均寿命)")) %>%
  print(size = "\\tiny")
sink()
# -----------------# -----------------



## ----warning=FALSE------------------------------
fit_with_FA_lm_HLE_d_f<-lm(HLE_d_f_final$HLE_2016~d_f_.FA$OBS.rotate)

lm(HLE_d_f_final$HLE_2016~d_f_.FA$OBS.rotate) %>% broom::tidy()%>% DT::datatable() %>% DT::formatRound(columns=c('estimate', 'std.error', 'statistic', 'p.value'), digits=3)
lm(HLE_d_f_final$HLE_2016~d_f_.FA$OBS.rotate) %>% broom::glance()


# -----------------# -----------------
# table tex file name
file="table_LM_HLE_FA_f.tex"
# path="./final_report-kenko(和歌山県)/table/"

sink(file = paste0(path, file))
fit_with_FA_lm_HLE_d_f %>% broom::tidy() %>%
    xtable(label = "tableLMHLEFAf",
         caption = c("女性の回帰withFA(健康寿命)")) %>%
  print(size = "\\tiny")
sink()
# -----------------# -----------------


## ----warning=FALSE------------------------------
fit_with_FA_lm_HLE_d_m<-lm(HLE_d_m_final$HLE_2016~d_m_.FA$OBS.rotate)

lm(HLE_d_m_final$HLE_2016~d_m_.FA$OBS.rotate) %>% broom::tidy()%>% DT::datatable() %>% DT::formatRound(columns=c('estimate', 'std.error', 'statistic', 'p.value'), digits=3)
lm(HLE_d_m_final$HLE_2016~d_m_.FA$OBS.rotate) %>% broom::glance()


# -----------------# -----------------
# table tex file name
file="table_LM_HLE_FA_m.tex"
# path="./final_report-kenko(和歌山県)/table/"

sink(file = paste0(path, file))
fit_with_FA_lm_HLE_d_m %>% broom::tidy() %>%
    xtable(label = "tableLMHLEFAm",
         caption = c("男性の回帰withFA(健康寿命)")) %>%
  print(size = "\\tiny")
sink()
# -----------------# -----------------



# -----------------
# -----------------
# -----------------
# -----------------
# -----------------
#   ____
#  / ___| __ _ _ __ ___  _ __ ___   __ _
# | |  _ / _` | '_ ` _ \| '_ ` _ \ / _` |
# | |_| | (_| | | | | | | | | | | | (_| |
#  \____|\__,_|_| |_| |_|_| |_| |_|\__,_|
# -----------------
# -----------------
# -----------------
# -----------------
load("ScreenEnd.RData")



## ----warning=FALSE------------------------------
LE_FF_d_f=LE_d_f_final$LE_2015 %>% enframe() %>%
  bind_cols(as.data.frame(d_f_.FA$OBS.rotate))%>%
  rename(LE=value,F1=V1, F2=V2) %>% select(-name)
# mutate(LE=as.factor(LE_binary))

fit_with_FA_gamma_LE_d_f<-glm(LE~., family=Gamma(link="log"), data=LE_FF_d_f)

glm(LE~., family=Gamma(link="log"), data=LE_FF_d_f) %>%
  broom::tidy() %>% DT::datatable() %>% DT::formatRound(columns=c('estimate', 'std.error', 'statistic', 'p.value'), digits=3)


# -----------------# -----------------
# table tex file name
# path="./final_report-kenko(和歌山県)/table/"

file="table_Gamma_LE_FA_f.tex"
# path="./final_report-kenko(和歌山県)/table/"

sink(file = paste0(path, file))
fit_with_FA_gamma_LE_d_f %>% broom::tidy() %>% mutate_if(is.numeric, round, 5) %>%
    xtable(label = "table_Gamma_LE_FA_f",
         caption = c("女性の一般化線形モデルwithFA(平均寿命)"), digits=3) %>%
  print(size = "\\tiny")
sink()
# -----------------# -----------------



## ----warning=FALSE------------------------------
LE_FF_d_m=LE_d_m_final$LE_2015 %>% enframe() %>%
  bind_cols(as.data.frame(d_m_.FA$OBS.rotate))%>%
  rename(LE=value,F1=V1, F2=V2) %>% select(-name)
# mutate(LE=as.factor(LE_binary))

fit_with_FA_gamma_LE_d_m<-glm(LE~., family=Gamma(link="log"), data=LE_FF_d_m)

glm(LE~., family=Gamma(link="log"), data=LE_FF_d_m) %>%
  broom::tidy() %>% DT::datatable() %>% DT::formatRound(columns=c('estimate', 'std.error', 'statistic', 'p.value'), digits=3)


# -----------------# -----------------
# table tex file name
# path="./final_report-kenko(和歌山県)/table/"

file="table_Gamma_LE_FA_m.tex"
# path="./final_report-kenko(和歌山県)/table/"

sink(file = paste0(path, file))
fit_with_FA_gamma_LE_d_m %>% broom::tidy() %>% mutate_if(is.numeric, round, 5) %>%
    xtable(label = "table_Gamma_LE_FA_m",
         caption = c("男性の一般化線形モデルwithFA(平均寿命)"), digits=3) %>%
  print(size = "\\tiny")
sink()
# -----------------# -----------------



## ----warning=FALSE------------------------------
HLE_FF_d_f=HLE_d_f_final$HLE_2016 %>% enframe() %>%
  bind_cols(as.data.frame(d_f_.FA$OBS.rotate))%>%
  rename(HLE=value,F1=V1, F2=V2) %>% select(-name)
# mutate(HLE=as.factor(HLE_binary))

fit_with_FA_gamma_HLE_d_f<-
  glm(HLE~., family=Gamma(link="log"), data=HLE_FF_d_f)

glm(HLE~., family=Gamma(link="log"), data=HLE_FF_d_f) %>%
  broom::tidy() %>% DT::datatable() %>% DT::formatRound(columns=c('estimate', 'std.error', 'statistic', 'p.value'), digits=3)


# -----------------# -----------------
# table tex file name
# path="./final_report-kenko(和歌山県)/table/"

file="table_Gamma_HLE_FA_f.tex"
# path="./final_report-kenko(和歌山県)/table/"

sink(file = paste0(path, file))
fit_with_FA_gamma_HLE_d_f %>% broom::tidy() %>% mutate_if(is.numeric, round, 5) %>%
    xtable(label = "table_Gamma_HLE_FA_f",
         caption = c("女性の一般化線形モデルwithFA(健康寿命)"), digits=3) %>%
  print(size = "\\tiny")
sink()
# -----------------# -----------------



## ----warning=FALSE------------------------------
HLE_FF_d_m=HLE_d_m_final$HLE_2016 %>% enframe() %>%
  bind_cols(as.data.frame(d_m_.FA$OBS.rotate))%>%
  rename(HLE=value,F1=V1, F2=V2) %>% select(-name)
# mutate(HLE=as.factor(HLE_binary))

fit_with_FA_gamma_HLE_d_m<-glm(HLE~., family=Gamma(link="log"), data=HLE_FF_d_m)

glm(HLE~., family=Gamma(link="log"), data=HLE_FF_d_m) %>%
  broom::tidy() %>% DT::datatable() %>% DT::formatRound(columns=c('estimate', 'std.error', 'statistic', 'p.value'), digits=3)


# -----------------# -----------------
# table tex file name
# path="./final_report-kenko(和歌山県)/table/"

file="table_Gamma_HLE_FA_m.tex"
# path="./final_report-kenko(和歌山県)/table/"

sink(file = paste0(path, file))
fit_with_FA_gamma_HLE_d_m %>% broom::tidy() %>% mutate_if(is.numeric, round, 5) %>%
    xtable(label = "table_Gamma_HLE_FA_m",
         caption = c("男性の一般化線形モデルwithFA(健康寿命)"), digits=3) %>%
  print(size = "\\tiny")
sink()
# -----------------# -----------------



## ----warning=FALSE------------------------------
LE_d_f_final$LE_binary <- ifelse(LE_d_f_final$LE_2015>median(LE_d_f_final$LE_2015),1,0)
HLE_d_f_final$HLE_binary <- ifelse(HLE_d_f_final$HLE_2016>median(HLE_d_f_final$HLE_2016),1,0)
LE_d_m_final$LE_binary <- ifelse(LE_d_m_final$LE_2015>median(LE_d_m_final$LE_2015),1,0)
HLE_d_m_final$HLE_binary <- ifelse(HLE_d_m_final$HLE_2016>median(HLE_d_m_final$HLE_2016),1,0)

# LE_d_f_final$LE_binary   # 女性
# HLE_d_f_final$HLE_binary   # 女性
# LE_d_m_final$LE_binary   # 男性
# HLE_d_m_final$HLE_binary   # 男性

## -----------------
## -----------------
## -----------------
## -----------------
## -----------------
## -----------------
#  _             _ _
# | | ___   __ _(_) |_
# | |/ _ \ / _` | | __|
# | | (_) | (_| | | |_
# |_|\___/ \__, |_|\__|
#          |___/
# -----------------
# -----------------
# -----------------
# -----------------
# -----------------

## ----warning=FALSE------------------------------

fit_with_FA_logit_LE_d_f<-glm(LE_d_f_final$LE_binary ~ d_f_.FA$OBS.rotate, family =  "binomial")

glm(LE_d_f_final$LE_binary ~ d_f_.FA$OBS.rotate, family =  "binomial")%>% broom::tidy() %>% DT::datatable() %>% DT::formatRound(columns=c('estimate', 'std.error', 'statistic', 'p.value'), digits=3)


# -----------------# -----------------
# table tex file name
# path="./final_report-kenko(和歌山県)/table/"

file="table_logit_LE_FA_f.tex"
# path="./final_report-kenko(和歌山県)/table/"

sink(file = paste0(path, file))
fit_with_FA_logit_LE_d_f %>% broom::tidy() %>% mutate_if(is.numeric, round, 5) %>%
    xtable(label = "table_Gamma_HLE_FA_m",
         caption = c("女性の一般化線形モデル(logit)withFA(平均寿命)"), digits=3) %>%
  print(size = "\\tiny")
sink()
# -----------------# -----------------


## ----warning=FALSE------------------------------
fit_with_FA_logit_LE_d_m<-glm(LE_d_m_final$LE_binary ~ d_m_.FA$OBS.rotate, family =  "binomial")

glm(LE_d_m_final$LE_binary ~ d_m_.FA$OBS.rotate, family =  "binomial")%>% broom::tidy() %>% DT::datatable() %>% DT::formatRound(columns=c('estimate', 'std.error', 'statistic', 'p.value'), digits=3)


# -----------------# -----------------
# table tex file name
# path="./final_report-kenko(和歌山県)/table/"

file="table_logit_LE_FA_m.tex"
# path="./final_report-kenko(和歌山県)/table/"

sink(file = paste0(path, file))
fit_with_FA_logit_LE_d_m %>% broom::tidy() %>% mutate_if(is.numeric, round, 5) %>%
    xtable(label = "table_Gamma_HLE_FA_m",
         caption = c("男性の一般化線形モデル(logit)withFA(平均寿命)"), digits=3) %>%
  print(size = "\\tiny")
sink()
# -----------------# -----------------

## ----warning=FALSE------------------------------
fit_with_FA_logit_HLE_d_f<-glm(HLE_d_f_final$HLE_binary ~ d_f_.FA$OBS.rotate, family =  "binomial")%>% broom::tidy()

glm(HLE_d_f_final$HLE_binary ~ d_f_.FA$OBS.rotate, family =  "binomial")%>% broom::tidy() %>% DT::datatable() %>% DT::formatRound(columns=c('estimate', 'std.error', 'statistic', 'p.value'), digits=3)


# -----------------# -----------------
# table tex file name
# path="./final_report-kenko(和歌山県)/table/"

file="table_logit_HLE_FA_f.tex"
# path="./final_report-kenko(和歌山県)/table/"

sink(file = paste0(path, file))
fit_with_FA_logit_HLE_d_f %>% broom::tidy() %>% mutate_if(is.numeric, round, 5) %>%
    xtable(label = "table_Gamma_HLE_FA_f",
         caption = c("女性の一般化線形モデル(logit)withFA(健康寿命)"), digits=3) %>%
  print(size = "\\tiny")
sink()
# -----------------# -----------------


## ----warning=FALSE------------------------------

fit_with_FA_logit_HLE_d_m<-glm(HLE_d_m_final$HLE_binary ~ d_m_.FA$OBS.rotate, family =  "binomial")

glm(HLE_d_m_final$HLE_binary ~ d_m_.FA$OBS.rotate, family =  "binomial")%>% broom::tidy() %>% DT::datatable() %>% DT::formatRound(columns=c('estimate', 'std.error', 'statistic', 'p.value'), digits=3)


# -----------------# -----------------
# table tex file name
# path="./final_report-kenko(和歌山県)/table/"

file="table_logit_HLE_FA_m.tex"
# path="./final_report-kenko(和歌山県)/table/"

sink(file = paste0(path, file))
fit_with_FA_logit_HLE_d_m %>% broom::tidy() %>% mutate_if(is.numeric, round, 5) %>%
    xtable(label = "table_Gamma_HLE_FA_m",
         caption = c("男性の一般化線形モデル(logit)withFA(健康寿命)"), digits=3) %>%
  print(size = "\\tiny")
sink()
# -----------------# -----------------

# -----------------
# -----------------
# -----------------
# -----------------
# -----------------
# -----------------
# ____
# | __ )  __ _ _   _  ___  ___
# |  _ \ / _` | | | |/ _ \/ __|
# | |_) | (_| | |_| |  __/\__ \
# |____/ \__,_|\__, |\___||___/
#              |___/
# -----------------
# -----------------
# -----------------
# -----------------
# -----------------
# -----------------

## ---- Bayes ------------------------------
## ---- Bayes : 女性、平均寿命---------------
## ----warning=FALSE------------------------------
suppressMessages(library(rethinking))

Bayes_fit_LE_d_f<-
  rethinking::map(alist(
    LE~dnorm(mu, sigma),
    mu~beta0+beta1*F1+beta2*F2,
    beta0~dnorm(80, 100),
    beta1~dnorm(0, 10),
    beta2~dnorm(0, 10),
    sigma ~ dunif(0, 50)
  ), data=LE_FF_d_f
  )





## ----warning=FALSE------------------------------
precis(Bayes_fit_LE_d_f) %>% DT::datatable() %>% DT::formatRound(columns=c("mean", "sd","5.5%","94.5%"), digits=3)
precis(Bayes_fit_LE_d_f) %>% as.matrix() %>% as.data.frame()



# -----------------# -----------------
# table tex file name
# path="./final_report-kenko(和歌山県)/table/"

file="Bayes_fit_LE_d_f.tex"
# path="./final_report-kenko(和歌山県)/table/"

sink(file = paste0(path, file))
precis(Bayes_fit_LE_d_f) %>% as.matrix() %>% as.data.frame() %>%
  xtable(label = "table_Gamma_HLE_FA_m",
         caption = c("女性のBayes(平均寿命)"), digits=3) %>%
  print(size = "\\tiny")
sink()
# -----------------# -----------------

## ----warning=FALSE------------------------------

post<-extract.samples(Bayes_fit_LE_d_f)


# purrr::map(post, hist)


# rethinking::HPDI(post$beta0)
# rethinking::HPDI(post$beta1)
# rethinking::HPDI(post$beta2)

# rethinking::PI(post$beta0)
# rethinking::PI(post$beta1)
# rethinking::PI(post$beta2)
dens(post)

path="./final_report-kenko(和歌山県)/fig/"
file="Bayes_LE_f_estimates.pdf"
pdf(paste0(path, file), family="Japan1")
dens(post)
dev.off()


## ----warning=FALSE------------------------------
mu_at_Q1<-post$beta0 + post$beta1*mean(LE_FF_d_f$F1)+ post$beta2*quantile(LE_FF_d_f$F2)[1]
mu_at_Q2<-post$beta0 + post$beta1*mean(LE_FF_d_f$F1)+ post$beta2*quantile(LE_FF_d_f$F2)[2]
mu_at_Q3<-post$beta0 + post$beta1*mean(LE_FF_d_f$F1)+ post$beta2*quantile(LE_FF_d_f$F2)[3]
mu_at_Q4<-post$beta0 + post$beta1*mean(LE_FF_d_f$F1)+ post$beta2*quantile(LE_FF_d_f$F2)[4]
mu_at_Q5<-post$beta0 + post$beta1*mean(LE_FF_d_f$F1)+ post$beta2*quantile(LE_FF_d_f$F2)[5]



par(family= "HiraKakuProN-W3")
path="./final_report-kenko(和歌山県)/fig/"
file="Bayes_LE_M_F2.pdf"
pdf(paste0(path, file), family="Japan1")
par(mfrow=c(1,1))
mu_at_Q2 %>% dens(col="red", main = "(女性平均寿命)F2因子の寿命分布への影響", xlim=c(86.8, 87.2))
mu_at_Q3 %>% dens(col="green", main = "F2が Q3である場合", add = T)
mu_at_Q4 %>% dens(col="blue", main = "F2が Q4である場合", add = T)
dev.off()



aa<-tibble::tibble(
  mu_at_Q2=mu_at_Q2,
  mu_at_Q3=mu_at_Q3,
  mu_at_Q4=mu_at_Q4
) %>% pivot_longer(cols = everything()) %>% nest(value)


name <- function(aa) {
  ggplot_build(aa %>% ggplot(aes(value))+stat_ecdf())$data[[1]] %>% select(y, x) %>%
    mutate(ccdf=1-y)
}

aa$ccdf<-purrr::map(aa$data, name)

aa %>% unnest(cols = ccdf) %>% ggplot(aes(x=x, y=ccdf, group=name, color=name))+geom_line()+theme_bw(base_family = "HiraKakuProN-W3")+xlim(86.8, 87.2)
# 1

aa_plot<-aa %>% unnest(cols = ccdf) %>% ggplot(aes(x=x, y=ccdf, group=name, color=name))+geom_line()+xlim(86.8, 87.2)

path="./final_report-kenko(和歌山県)/fig/"
file="Bayes_LE_f_ccdf_F2.pdf"
# ggsave(filename = paste0(path, file),aa,width = 8, height = 8, family="Japan1")
ggsave(filename = paste0(path, file),aa_plot, family="Japan1")



## ----warning=FALSE------------------------------
mu_at_Q1<-post$beta0 + post$beta1*quantile(LE_FF_d_f$F1)[1]+ post$beta2*mean(LE_FF_d_f$F2)
mu_at_Q2<-post$beta0 + post$beta1*quantile(LE_FF_d_f$F1)[2]+ post$beta2*mean(LE_FF_d_f$F2)
mu_at_Q3<-post$beta0 + post$beta1*quantile(LE_FF_d_f$F1)[3]+ post$beta2*mean(LE_FF_d_f$F2)
mu_at_Q4<-post$beta0 + post$beta1*quantile(LE_FF_d_f$F1)[4]+ post$beta2*mean(LE_FF_d_f$F2)
mu_at_Q5<-post$beta0 + post$beta1*quantile(LE_FF_d_f$F1)[5]+ post$beta2*mean(LE_FF_d_f$F2)


par(family= "HiraKakuProN-W3")


par(family= "HiraKakuProN-W3")
path="./final_report-kenko(和歌山県)/fig/"
file="Bayes_LE_M_F1.pdf"
pdf(paste0(path, file), family="Japan1")
par(mfrow=c(1,1))
mu_at_Q2 %>% dens(col="red", main = "(女性平均寿命)F1因子の寿命への影響", xlim=c(86.8, 87.2))
mu_at_Q3 %>% dens(col="green", main = "F1が Q3である場合", add = T)
mu_at_Q4 %>% dens(col="blue", main = "F1が Q4である場合", add = T)
dev.off()

aa<-tibble::tibble(
  mu_at_Q2=mu_at_Q2,
  mu_at_Q3=mu_at_Q3,
  mu_at_Q4=mu_at_Q4
) %>% pivot_longer(cols = everything()) %>% nest(value)


name <- function(aa) {
  ggplot_build(aa %>% ggplot(aes(value))+stat_ecdf())$data[[1]] %>% select(y, x) %>%
    mutate(ccdf=1-y)
}

aa$ccdf<-purrr::map(aa$data, name)

aa %>% unnest(cols = ccdf) %>% ggplot(aes(x=x, y=ccdf, group=name, color=name))+geom_line()+theme_bw(base_family = "HiraKakuProN-W3")+xlim(86.8, 87.2)
# 2




## ---- Bayes : 男性、平均寿命---------------

suppressMessages(library(rethinking))


Bayes_fit_LE_d_m<-
  rethinking::map(alist(
    LE~dnorm(mu, sigma),
    mu~beta0+beta1*F1+beta2*F2,
    beta0~dnorm(80, 100),
    beta1~dnorm(0, 10),
    beta2~dnorm(0, 10),
    sigma ~ dunif(0, 50)
  ), data=LE_FF_d_m
  )





## ----warning=FALSE------------------------------
precis(Bayes_fit_LE_d_m)
precis(Bayes_fit_LE_d_m) %>% as.matrix() %>% as.data.frame()


# -----------------# -----------------
# table tex file name
# path="./final_report-kenko(和歌山県)/table/"

file="Bayes_fit_LE_d_m.tex"
# path="./final_report-kenko(和歌山県)/table/"

sink(file = paste0(path, file))
precis(Bayes_fit_LE_d_m) %>% as.matrix() %>% as.data.frame() %>%
  xtable(label = "table_Gamma_HLE_FA_m",
         caption = c("男性のBayes(平均寿命)"), digits=3) %>%
  print(size = "\\tiny")
sink()
# -----------------# -----------------


## ----warning=FALSE------------------------------

post<-extract.samples(Bayes_fit_LE_d_m)


# purrr::map(post, hist)


# rethinking::HPDI(post$beta0)
# rethinking::HPDI(post$beta1)
# rethinking::HPDI(post$beta2)

# rethinking::PI(post$beta0)
# rethinking::PI(post$beta1)
# rethinking::PI(post$beta2)

dens(post)

path="./final_report-kenko(和歌山県)/fig/"
file="Bayes_LE_m_estimates.pdf"
pdf(paste0(path, file), family="Japan1")
dens(post)
dev.off()


## ----warning=FALSE------------------------------
mu_at_Q1<-post$beta0 + post$beta1*mean(LE_FF_d_m$F1)+ post$beta2*quantile(LE_FF_d_m$F2)[1]
mu_at_Q2<-post$beta0 + post$beta1*mean(LE_FF_d_m$F1)+ post$beta2*quantile(LE_FF_d_m$F2)[2]
mu_at_Q3<-post$beta0 + post$beta1*mean(LE_FF_d_m$F1)+ post$beta2*quantile(LE_FF_d_m$F2)[3]
mu_at_Q4<-post$beta0 + post$beta1*mean(LE_FF_d_m$F1)+ post$beta2*quantile(LE_FF_d_m$F2)[4]
mu_at_Q5<-post$beta0 + post$beta1*mean(LE_FF_d_m$F1)+ post$beta2*quantile(LE_FF_d_m$F2)[5]


par(family= "HiraKakuProN-W3")

mu_at_Q2 %>% dens(col="red", main = "F2因子の寿命分布への影響", xlim=c(80-.0, 80+1))
mu_at_Q3 %>% dens(col="green", main = "F2が Q3である場合", add = T)
mu_at_Q4 %>% dens(col="blue", main = "F2が Q4である場合", add = T)

aa<-tibble::tibble(
  mu_at_Q2=mu_at_Q2,
  mu_at_Q3=mu_at_Q3,
  mu_at_Q4=mu_at_Q4
) %>% pivot_longer(cols = everything()) %>% nest(value)


name <- function(aa) {
  ggplot_build(aa %>% ggplot(aes(value))+stat_ecdf())$data[[1]] %>% select(y, x) %>%
    mutate(ccdf=1-y)
}

aa$ccdf<-purrr::map(aa$data, name)

aa %>% unnest(cols = ccdf) %>% ggplot(aes(x=x, y=ccdf, group=name, color=name))+geom_line()+theme_bw(base_family = "HiraKakuProN-W3")+xlim(80-.0, 80+1)
# 3



## ----warning=FALSE------------------------------
mu_at_Q1<-post$beta0 + post$beta1*quantile(LE_FF_d_m$F1)[1]+ post$beta2*mean(LE_FF_d_m$F2)
mu_at_Q2<-post$beta0 + post$beta1*quantile(LE_FF_d_m$F1)[2]+ post$beta2*mean(LE_FF_d_m$F2)
mu_at_Q3<-post$beta0 + post$beta1*quantile(LE_FF_d_m$F1)[3]+ post$beta2*mean(LE_FF_d_m$F2)
mu_at_Q4<-post$beta0 + post$beta1*quantile(LE_FF_d_m$F1)[4]+ post$beta2*mean(LE_FF_d_m$F2)
mu_at_Q5<-post$beta0 + post$beta1*quantile(LE_FF_d_m$F1)[5]+ post$beta2*mean(LE_FF_d_m$F2)


par(family= "HiraKakuProN-W3")

mu_at_Q2 %>% dens(col="red", main = "F1因子の寿命への影響", xlim=c(80-.0, 80+1))
mu_at_Q3 %>% dens(col="green", main = "F1が Q3である場合", add = T)
mu_at_Q4 %>% dens(col="blue", main = "F1が Q4である場合", add = T)

aa<-tibble::tibble(
  mu_at_Q2=mu_at_Q2,
  mu_at_Q3=mu_at_Q3,
  mu_at_Q4=mu_at_Q4
) %>% pivot_longer(cols = everything()) %>% nest(value)


name <- function(aa) {
  ggplot_build(aa %>% ggplot(aes(value))+stat_ecdf())$data[[1]] %>% select(y, x) %>%
    mutate(ccdf=1-y)
}

aa$ccdf<-purrr::map(aa$data, name)

aa %>% unnest(cols = ccdf) %>% ggplot(aes(x=x, y=ccdf, group=name, color=name))+geom_line()+theme_bw(base_family = "HiraKakuProN-W3")+xlim(80-.0, 80+1)
# 4



## ---- Bayes : 女性、健康寿命---------------
suppressMessages(library(rethinking))


Bayes_fit_HLE_d_f<-
  rethinking::map(alist(
    HLE~dnorm(mu, sigma),
    mu~beta0+beta1*F1+beta2*F2,
    beta0~dnorm(80, 100),
    beta1~dnorm(0, 10),
    beta2~dnorm(0, 10),
    sigma ~ dunif(0, 50)
  ), data=HLE_FF_d_f
  )





## ----warning=FALSE------------------------------
precis(Bayes_fit_HLE_d_f)
precis(Bayes_fit_HLE_d_f) %>% as.matrix() %>% as.data.frame()


# -----------------# -----------------
# table tex file name
# path="./final_report-kenko(和歌山県)/table/"

file="Bayes_fit_HLE_d_f.tex"
# path="./final_report-kenko(和歌山県)/table/"

sink(file = paste0(path, file))
precis(Bayes_fit_HLE_d_f) %>% as.matrix() %>% as.data.frame() %>%
  xtable(label = "table_Gamma_HLE_FA_m",
         caption = c("女性のBayes(健康寿命)"), digits=3) %>%
  print(size = "\\tiny")
sink()
# -----------------# -----------------


## ----warning=FALSE------------------------------

post<-extract.samples(Bayes_fit_HLE_d_f)


# purrr::map(post, hist)


# rethinking::HPDI(post$beta0)
# rethinking::HPDI(post$beta1)
# rethinking::HPDI(post$beta2)

# rethinking::PI(post$beta0)
# rethinking::PI(post$beta1)
# rethinking::PI(post$beta2)
dens(post)

path="./final_report-kenko(和歌山県)/fig/"
file="Bayes_HLE_f_estimates.pdf"
pdf(paste0(path, file), family="Japan1")
dens(post)
dev.off()

## ----warning=FALSE------------------------------
mu_at_Q1<-post$beta0 + post$beta1*mean(HLE_FF_d_f$F1)+ post$beta2*quantile(HLE_FF_d_f$F2)[1]
mu_at_Q2<-post$beta0 + post$beta1*mean(HLE_FF_d_f$F1)+ post$beta2*quantile(HLE_FF_d_f$F2)[2]
mu_at_Q3<-post$beta0 + post$beta1*mean(HLE_FF_d_f$F1)+ post$beta2*quantile(HLE_FF_d_f$F2)[3]
mu_at_Q4<-post$beta0 + post$beta1*mean(HLE_FF_d_f$F1)+ post$beta2*quantile(HLE_FF_d_f$F2)[4]
mu_at_Q5<-post$beta0 + post$beta1*mean(HLE_FF_d_f$F1)+ post$beta2*quantile(HLE_FF_d_f$F2)[5]


par(family= "HiraKakuProN-W3")

mu_at_Q2 %>% dens(col="red", main = "F2因子の寿命分布への影響", xlim=c(75-.3, 75+.3))
mu_at_Q3 %>% dens(col="green", main = "F2が Q3である場合", add = T)
mu_at_Q4 %>% dens(col="blue", main = "F2が Q4である場合", add = T)

aa<-tibble::tibble(
  mu_at_Q2=mu_at_Q2,
  mu_at_Q3=mu_at_Q3,
  mu_at_Q4=mu_at_Q4
) %>% pivot_longer(cols = everything()) %>% nest(value)


name <- function(aa) {
  ggplot_build(aa %>% ggplot(aes(value))+stat_ecdf())$data[[1]] %>% select(y, x) %>%
    mutate(ccdf=1-y)
}

aa$ccdf<-purrr::map(aa$data, name)

aa %>% unnest(cols = ccdf) %>% ggplot(aes(x=x, y=ccdf, group=name, color=name))+geom_line()+theme_bw(base_family = "HiraKakuProN-W3")+xlim(75-.3, 75+.3)
# 5






## ----warning=FALSE------------------------------
mu_at_Q1<-post$beta0 + post$beta1*quantile(HLE_FF_d_f$F1)[1]+ post$beta2*mean(HLE_FF_d_f$F2)
mu_at_Q2<-post$beta0 + post$beta1*quantile(HLE_FF_d_f$F1)[2]+ post$beta2*mean(HLE_FF_d_f$F2)
mu_at_Q3<-post$beta0 + post$beta1*quantile(HLE_FF_d_f$F1)[3]+ post$beta2*mean(HLE_FF_d_f$F2)
mu_at_Q4<-post$beta0 + post$beta1*quantile(HLE_FF_d_f$F1)[4]+ post$beta2*mean(HLE_FF_d_f$F2)
mu_at_Q5<-post$beta0 + post$beta1*quantile(HLE_FF_d_f$F1)[5]+ post$beta2*mean(HLE_FF_d_f$F2)


par(family= "HiraKakuProN-W3")

mu_at_Q2 %>% dens(col="red", main = "F1因子の寿命への影響", xlim=c(75-.3, 75+.3))
mu_at_Q3 %>% dens(col="green", main = "F1が Q3である場合", add = T)
mu_at_Q4 %>% dens(col="blue", main = "F1が Q4である場合", add = T)

aa<-tibble::tibble(
  mu_at_Q2=mu_at_Q2,
  mu_at_Q3=mu_at_Q3,
  mu_at_Q4=mu_at_Q4
) %>% pivot_longer(cols = everything()) %>% nest(value)


name <- function(aa) {
  ggplot_build(aa %>% ggplot(aes(value))+stat_ecdf())$data[[1]] %>% select(y, x) %>%
    mutate(ccdf=1-y)
}

aa$ccdf<-purrr::map(aa$data, name)

aa %>% unnest(cols = ccdf) %>% ggplot(aes(x=x, y=ccdf, group=name, color=name))+geom_line()+theme_bw(base_family = "HiraKakuProN-W3")+xlim(75-.3, 75+.3)
# 6





## ---- Bayes : 男性、健康寿命---------------
suppressMessages(library(rethinking))


Bayes_fit_HLE_d_m<-
  rethinking::map(alist(
    HLE~dnorm(mu, sigma),
    mu~beta0+beta1*F1+beta2*F2,
    beta0~dnorm(80, 100),
    beta1~dnorm(0, 10),
    beta2~dnorm(0, 10),
    sigma ~ dunif(0, 50)
  ), data=HLE_FF_d_m
  )





## ----warning=FALSE------------------------------
precis(Bayes_fit_HLE_d_m)
precis(Bayes_fit_HLE_d_m) %>% as.matrix() %>% as.data.frame()



# -----------------# -----------------
# table tex file name
# path="./final_report-kenko(和歌山県)/table/"

file="Bayes_fit_HLE_d_m.tex"
# path="./final_report-kenko(和歌山県)/table/"

sink(file = paste0(path, file))
precis(Bayes_fit_HLE_d_m) %>% as.matrix() %>% as.data.frame() %>%
  xtable(label = "table_Gamma_HLE_FA_m",
         caption = c("男性のBayes(健康寿命)"), digits=3) %>%
  print(size = "\\tiny")
sink()
# -----------------# -----------------

## ----warning=FALSE------------------------------

post<-extract.samples(Bayes_fit_HLE_d_m)


# purrr::map(post, hist)


# rethinking::HPDI(post$beta0)
# rethinking::HPDI(post$beta1)
# rethinking::HPDI(post$beta2)

# rethinking::PI(post$beta0)
# rethinking::PI(post$beta1)
# rethinking::PI(post$beta2)
dens(post)

path="./final_report-kenko(和歌山県)/fig/"
file="Bayes_HLE_m_estimates.pdf"
pdf(paste0(path, file), family="Japan1")
dens(post)
dev.off()

## ----warning=FALSE------------------------------
mu_at_Q1<-post$beta0 + post$beta1*mean(HLE_FF_d_m$F1)+ post$beta2*quantile(HLE_FF_d_m$F2)[1]
mu_at_Q2<-post$beta0 + post$beta1*mean(HLE_FF_d_m$F1)+ post$beta2*quantile(HLE_FF_d_m$F2)[2]
mu_at_Q3<-post$beta0 + post$beta1*mean(HLE_FF_d_m$F1)+ post$beta2*quantile(HLE_FF_d_m$F2)[3]
mu_at_Q4<-post$beta0 + post$beta1*mean(HLE_FF_d_m$F1)+ post$beta2*quantile(HLE_FF_d_m$F2)[4]
mu_at_Q5<-post$beta0 + post$beta1*mean(HLE_FF_d_m$F1)+ post$beta2*quantile(HLE_FF_d_m$F2)[5]


par(family= "HiraKakuProN-W3")

mu_at_Q2 %>% dens(col="red", main = "F2因子の寿命分布への影響", xlim=c(72-.3, 72+.3))
mu_at_Q3 %>% dens(col="green", main = "F2が Q3である場合", add = T)
mu_at_Q4 %>% dens(col="blue", main = "F2が Q4である場合", add = T)

aa<-tibble::tibble(
  mu_at_Q2=mu_at_Q2,
  mu_at_Q3=mu_at_Q3,
  mu_at_Q4=mu_at_Q4
) %>% pivot_longer(cols = everything()) %>% nest(value)


name <- function(aa) {
  ggplot_build(aa %>% ggplot(aes(value))+stat_ecdf())$data[[1]] %>% select(y, x) %>%
    mutate(ccdf=1-y)
}

aa$ccdf<-purrr::map(aa$data, name)

aa %>% unnest(cols = ccdf) %>% ggplot(aes(x=x, y=ccdf, group=name, color=name))+geom_line()+theme_bw(base_family = "HiraKakuProN-W3")+xlim(72-.3, 72+.3)
# 7





## ----warning=FALSE------------------------------
mu_at_Q1<-post$beta0 + post$beta1*quantile(HLE_FF_d_m$F1)[1]+ post$beta2*mean(HLE_FF_d_m$F2)
mu_at_Q2<-post$beta0 + post$beta1*quantile(HLE_FF_d_m$F1)[2]+ post$beta2*mean(HLE_FF_d_m$F2)
mu_at_Q3<-post$beta0 + post$beta1*quantile(HLE_FF_d_m$F1)[3]+ post$beta2*mean(HLE_FF_d_m$F2)
mu_at_Q4<-post$beta0 + post$beta1*quantile(HLE_FF_d_m$F1)[4]+ post$beta2*mean(HLE_FF_d_m$F2)
mu_at_Q5<-post$beta0 + post$beta1*quantile(HLE_FF_d_m$F1)[5]+ post$beta2*mean(HLE_FF_d_m$F2)


par(family= "HiraKakuProN-W3")

mu_at_Q2 %>% dens(col="red", main = "F1因子の寿命への影響", xlim=c(72-.3, 72+.3))
mu_at_Q3 %>% dens(col="green", main = "F1が Q3である場合", add = T)
mu_at_Q4 %>% dens(col="blue", main = "F1が Q4である場合", add = T)

aa<-tibble::tibble(
  mu_at_Q2=mu_at_Q2,
  mu_at_Q3=mu_at_Q3,
  mu_at_Q4=mu_at_Q4
) %>% pivot_longer(cols = everything()) %>% nest(value)


name <- function(aa) {
  ggplot_build(aa %>% ggplot(aes(value))+stat_ecdf())$data[[1]] %>% select(y, x) %>%
    mutate(ccdf=1-y)
}

aa$ccdf<-purrr::map(aa$data, name)

aa %>% unnest(cols = ccdf) %>% ggplot(aes(x=x, y=ccdf, group=name, color=name))+geom_line()+theme_bw(base_family = "HiraKakuProN-W3")+xlim(72-.3, 72+.3)
# 8



#


# file="table_FA_m.tex"
# file="table_FA_f.tex"
# file="table_LM_with_FA_LE_mf.tex"
# file="table_LM_with_FA_HLE_mf.tex"
# file="table_logit_with_FA_LE_mf.tex"
# file="table_logit_with_FA_HLE_mf.tex"
# file="table_gamma_with_FA_LE_mf.tex"
# file="table_gamma_with_FA_HLE_mf.tex"
# file="table_bayes_with_FA_LE_mf.tex"
# file="table_bayes_with_FA_HLE_mf.tex"
