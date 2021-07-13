# library(tikzDevice)
# tikz('tikz-example.tex', width = 3.25, height = 3.25)
# plot(1, 1, main = 'Hello \\TeX !')
# dev.off()
#
# system("open .")

## ---- Pre material ------------------------------
# |  _ \ _ __ ___|  \/  | __ _| |_ ___ _ __(_) __ _| |
# | |_) | '__/ _ \ |\/| |/ _` | __/ _ \ '__| |/ _` | |
# |  __/| | |  __/ |  | | (_| | ||  __/ |  | | (_| | |
# |_|   |_|  \___|_|  |_|\__,_|\__\___|_|  |_|\__,_|_|
## ---- Pre material ------------------------------
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
suppressMessages(library(xtable))
suppressMessages(library(tikzDevice))


JcXtable <- function(data,
                     #<- table assign
                     label = "aa.tex",
                     #<- file name
                     caption = "tttest",
                     # <- caption
                     size = "\\tiny",
                     place = "top") {
  data %>% xtable(label = label, caption = caption) %>%
    print(size = size, caption.placement = place)
}



# load("../ScreenEnd.RData")
load("ScreenEnd.RData")


# -----------------　Ttest　# -----------------
# _____ _____ _____ ____ _____
# |_   _|_   _| ____/ ___|_   _|
#   | |   | | |  _| \___ \ | |
#   | |   | | | |___ ___) || |
#   |_|   |_| |_____|____/ |_|
# -----------------　Ttest　# -----------------

# ----------------- tttest LE female # -----------------

load("ScreenEnd.RData")

LE_d_f_final %>% dim()
LE_d_f_final$LE_binary <- ifelse(LE_d_f_final$LE_2015>median(LE_d_f_final$LE_2015),1,0)
name <- function(x) {t.test(x~LE_d_f_final$LE_binary) %>% broom::tidy()}

LE_d_f_final %>% select(-LE_binary) %>% purrr::map_df(name) %>%
  select(-method, -alternative, -parameter) -> LE_d_f_Ttest

colnames(LE_d_f_final[-c(19)]) %>% enframe() %>% bind_cols(LE_d_f_Ttest) %>%
  left_join(var, by=c("value"="var_name_Eng")) %>%
  select(-name, -value, -id, -columm_letter, -address) %>%
  select(var_name_Jpn, everything())-> LE_d_f_Ttest



# table tex file name
file="LE_Ttest_d_f.tex"
path="./final_report-kenko(和歌山県)/table/"

sink(file = paste0(path, file))
LE_d_f_Ttest %>%
  JcXtable(label = "LE_Ttest_d_f.tex", caption ="平均寿命が長い県と短い県のt-検定(女性)" , size = "\\tiny" , place = "top")
sink()

system('
cd "./final_report-kenko(和歌山県)/table/";
sed -e "s/㎡/m$^2$/g" "LE_Ttest_d_f.tex" > "temp.tex";
python3 ../../exex.py "temp.tex" "temp1.tex";
mv -f "temp1.tex" "LE_Ttest_d_f.tex"
')


# ----------------- tttest LE male # -----------------
load("ScreenEnd.RData")

LE_d_m_final %>% dim()
LE_d_m_final$LE_binary <- ifelse(LE_d_m_final$LE_2015>median(LE_d_m_final$LE_2015),1,0)
name <- function(x) {t.test(x~LE_d_m_final$LE_binary) %>% broom::tidy()}

LE_d_m_final %>% select(-LE_binary) %>% purrr::map_df(name) %>%
  select(-method, -alternative, -parameter) -> LE_d_m_Ttest

colnames(LE_d_m_final[-c(12)]) %>% enframe() %>% bind_cols(LE_d_m_Ttest) %>%
  left_join(var, by=c("value"="var_name_Eng")) %>%
  select(-name, -value, -id, -columm_letter, -address) %>%
  select(var_name_Jpn, everything())-> LE_d_m_Ttest



# table tex file name
file="LE_Ttest_d_m.tex"
path="./final_report-kenko(和歌山県)/table/"

sink(file = paste0(path, file))
LE_d_m_Ttest %>%
  JcXtable(label = "LE_Ttest_d_m.tex", caption ="平均寿命が長い県と短い県のt-検定(男性)" , size = "\\tiny" , place = "top")
sink()

system('
cd "./final_report-kenko(和歌山県)/table/";
sed -e "s/㎡/m$^2$/g" "LE_Ttest_d_m.tex" > "temp.tex";
python3 ../../exex.py "temp.tex" "temp1.tex";
mv -f "temp1.tex" "LE_Ttest_d_m.tex"
')






options(digits = 5)              # Modify global options


ls(pattern = "logit")


LE_d_f_final %>% summary()


LE_d_f_final %>% dim()
LE_d_m_final %>% dim()



HLE_d_f_final$HLE_binary <- ifelse(HLE_d_f_final$HLE_2016>median(HLE_d_f_final$HLE_2016),1,0)
LE_d_m_final$LE_binary <- ifelse(LE_d_m_final$LE_2015>median(LE_d_m_final$LE_2015),1,0)
HLE_d_m_final$HLE_binary <- ifelse(HLE_d_m_final$HLE_2016>median(HLE_d_m_final$HLE_2016),1,0)


t.test(LE_d_f_final$Treatment_rate_Outpatient_Cerebrovascular_dz_2017~LE_d_f_final$LE_binary) %>% broom::tidy()


# ----------------- used var # -----------------
#  _   _              _  __     ___    ____
# | | | |___  ___  __| | \ \   / / \  |  _ \
# | | | / __|/ _ \/ _` |  \ \ / / _ \ | |_) |
# | |_| \__ \  __/ (_| |   \ V / ___ \|  _ <
#  \___/|___/\___|\__,_|    \_/_/   \_\_| \_\
# ----------------- used var # -----------------

f_var=LE_d_f_final %>% colnames() %>% tbl_df() %>% left_join(var, by=c("value"="var_name_Eng")) %>%
  select(3) %>% filter(!is.na(var_name_Jpn)) %>% rename(f_var=var_name_Jpn) %>% .[-1, ]

m_var=LE_d_m_final %>% colnames() %>% tbl_df() %>% left_join(var, by=c("value"="var_name_Eng")) %>%
  select(3) %>% filter(!is.na(var_name_Jpn)) %>% rename(m_var=var_name_Jpn) %>% .[-1, ]


m_var=
  LE_d_m_final %>% colnames() %>% tbl_df() %>% left_join(var, by=c("value"="var_name_Eng")) %>%  select(1,3)

%>% filter(!is.na(var_name_Jpn)) %>%
  rename(m_var=var_name_Jpn) %>% .[-1, ]

m_var %<>% bind_rows(as.data.frame(rep(NA, 7)))
m_var %<>% select(1)



# table tex file name
file="UsedVar.tex"
# path="./final_report-kenko(和歌山県)/table/"

sink(file = paste0(path, file))
f_var %>%
  bind_cols(m_var) %>%
  xtable(label = "UsedVariable",
         caption = c("変数選択後の変数")) %>%
  print(size = "\\tiny", caption.placement = "top")
sink()

system('
cd "./final_report-kenko(和歌山県)/table/";
sed -e "s/㎡/m$^2$/g" "UsedVar.tex" > "UsedVar1.tex";
rm UsedVar.tex;
mv UsedVar1.tex UsedVar.tex;
python3 ../../exex.py "UsedVar.tex" "temp.tex";
mv "temp.tex" "UsedVar.tex"
')




LE_d_m_final %>% colnames() %>% tbl_df() %>% left_join(var, by=c("value"="var_name_Eng")) %>%
  select(3) %>% filter(!is.na(var_name_Jpn)) %>% rename(female_var=var_name_Jpn) %>% .[-1, ]


LE_d_m_final %>% colnames() %>% tbl_df() %>% left_join(var, by=c("value"="var_name_Eng")) %>%
  select(3)
