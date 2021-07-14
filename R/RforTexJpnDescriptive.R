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
  select(-method, -alternative, -parameter) %>% select(estimate, estimate1, estimate2) -> LE_d_f_Ttest

colnames(LE_d_f_final[-c(19)]) %>% enframe() %>% bind_cols(LE_d_f_Ttest) %>%
  left_join(var, by=c("value"="var_name_Eng")) %>%
  select(-name, -value, -id, -columm_letter, -address) %>%
  select(var_name_Jpn, everything())-> LE_d_f_Ttest



# table tex file name
file="LE_Ttest_d_f.tex"
path="./final_report-kenko(和歌山県)/table/"

sink(file = paste0(path, file))
LE_d_f_Ttest %>%
  JcXtable(label = "LE_Ttest_d_f.tex", caption ="平均寿命が長い県と短い県の平均値比較(女性)" , size = "\\tiny" , place = "top")
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
  select(-method, -alternative, -parameter) %>% select(estimate, estimate1, estimate2) -> LE_d_m_Ttest

colnames(LE_d_m_final[-c(12)]) %>% enframe() %>% bind_cols(LE_d_m_Ttest) %>%
  left_join(var, by=c("value"="var_name_Eng")) %>%
  select(-name, -value, -id, -columm_letter, -address) %>%
  select(var_name_Jpn, everything())-> LE_d_m_Ttest



# table tex file name
file="LE_Ttest_d_m.tex"
path="./final_report-kenko(和歌山県)/table/"

sink(file = paste0(path, file))
LE_d_m_Ttest %>%
  JcXtable(label = "LE_Ttest_d_m.tex", caption ="平均寿命が長い県と短い県の平均値比較(男性)" , size = "\\tiny" , place = "top")
sink()

system('
cd "./final_report-kenko(和歌山県)/table/";
sed -e "s/㎡/m$^2$/g" "LE_Ttest_d_m.tex" > "temp.tex";
python3 ../../exex.py "temp.tex" "temp1.tex";
mv -f "temp1.tex" "LE_Ttest_d_m.tex"
')


# ----------------- tttest HLE female # -----------------

load("ScreenEnd.RData")

HLE_d_f_final %>% dim()
HLE_d_f_final$HLE_binary <- ifelse(HLE_d_f_final$HLE_2016>median(HLE_d_f_final$HLE_2016),1,0)
name <- function(x) {t.test(x~HLE_d_f_final$HLE_binary) %>% broom::tidy()}

HLE_d_f_final %>% select(-HLE_binary) %>% purrr::map_df(name) %>%
  select(-method, -alternative, -parameter) %>% select(estimate, estimate1, estimate2) -> HLE_d_f_Ttest

colnames(HLE_d_f_final[-c(19)]) %>% enframe() %>% bind_cols(HLE_d_f_Ttest) %>%
  left_join(var, by=c("value"="var_name_Eng")) %>%
  select(-name, -value, -id, -columm_letter, -address) %>%
  select(var_name_Jpn, everything())-> HLE_d_f_Ttest



# table tex file name
file="HLE_Ttest_d_f.tex"
path="./final_report-kenko(和歌山県)/table/"

sink(file = paste0(path, file))
HLE_d_f_Ttest %>%
  JcXtable(label = "HLE_Ttest_d_f.tex", caption ="健康寿命が長い県と短い県の平均値比較(女性)" , size = "\\tiny" , place = "top")
sink()

system('
cd "./final_report-kenko(和歌山県)/table/";
sed -e "s/㎡/m$^2$/g" "HLE_Ttest_d_f.tex" > "temp.tex";
python3 ../../exex.py "temp.tex" "temp1.tex";
mv -f "temp1.tex" "HLE_Ttest_d_f.tex"
')


# ----------------- tttest HLE male # -----------------
load("ScreenEnd.RData")

HLE_d_m_final %>% dim()
HLE_d_m_final$HLE_binary <- ifelse(HLE_d_m_final$HLE_2016>median(HLE_d_m_final$HLE_2016),1,0)
name <- function(x) {t.test(x~HLE_d_m_final$HLE_binary) %>% broom::tidy()}

HLE_d_m_final %>% select(-HLE_binary) %>% purrr::map_df(name) %>%
  select(-method, -alternative, -parameter) %>% select(estimate, estimate1, estimate2) -> HLE_d_m_Ttest

colnames(HLE_d_m_final[-c(12)]) %>% enframe() %>% bind_cols(HLE_d_m_Ttest) %>%
  left_join(var, by=c("value"="var_name_Eng")) %>%
  select(-name, -value, -id, -columm_letter, -address) %>%
  select(var_name_Jpn, everything())-> HLE_d_m_Ttest



# table tex file name
file="HLE_Ttest_d_m.tex"
path="./final_report-kenko(和歌山県)/table/"

sink(file = paste0(path, file))
HLE_d_m_Ttest %>%
  JcXtable(label = "HLE_Ttest_d_m.tex", caption ="健康寿命が長い県と短い県の平均値比較(男性)" , size = "\\tiny" , place = "top")
sink()

system('
cd "./final_report-kenko(和歌山県)/table/";
sed -e "s/㎡/m$^2$/g" "HLE_Ttest_d_m.tex" > "temp.tex";
python3 ../../exex.py "temp.tex" "temp1.tex";
mv -f "temp1.tex" "HLE_Ttest_d_m.tex"
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

# ----------------- corr # -----------------
#  ____ ___  ____  ____
# / ___/ _ \|  _ \|  _ \
# | |  | | | | |_) | |_) |
# | |__| |_| |  _ <|  _ <
# \____\___/|_| \_\_| \_\
# ----------------- corr # -----------------


LE_d_common_f_corr = LE_d_common_f %>% cor() %>% broom::tidy() %>% select(1, 2) %>% filter(abs(LE_2015)>0.4) %>% arrange(desc(LE_2015))
LE_d_common_f_corr %<>%  left_join(var, by=c(".rownames"="var_name_Eng")) %>%
  select(4,2)

# table tex file name
file="LE_d_common_f_corr.tex"
path="./final_report-kenko(和歌山県)/table/"

sink(file = paste0(path, file))
LE_d_common_f_corr %>%
  JcXtable(label = "LE_d_common_f_corr.tex", caption ="平均寿命と説明変数の相関(女性,共通変数)" , size = "\\tiny" , place = "top")
sink()

system('
cd "./final_report-kenko(和歌山県)/table/";
sed -e "s/㎡/m$^2$/g" "LE_d_common_f_corr.tex" > "temp.tex";
python3 ../../exex.py "temp.tex" "temp1.tex";
mv -f "temp1.tex" "LE_d_common_f_corr.tex"
')



# -----------------

LE_d_common_m_corr = LE_d_common_m %>% cor() %>% broom::tidy() %>% select(1, 2) %>% filter(abs(LE_2015)>0.4) %>% arrange(desc(LE_2015))
LE_d_common_m_corr %<>%  left_join(var, by=c(".rownames"="var_name_Eng")) %>%
  select(4,2)


# table tex file name
file="LE_d_common_m_corr.tex"
path="./final_report-kenko(和歌山県)/table/"

sink(file = paste0(path, file))
LE_d_common_m_corr %>%
  JcXtable(label = "LE_d_common_m_corr.tex", caption ="平均寿命と説明変数の相関(男性,共通変数)" , size = "\\tiny" , place = "top")
sink()

system('
cd "./final_report-kenko(和歌山県)/table/";
sed -e "s/㎡/m$^2$/g" "LE_d_common_m_corr.tex" > "temp.tex";
python3 ../../exex.py "temp.tex" "temp1.tex";
mv -f "temp1.tex" "LE_d_common_m_corr.tex"
')




# -----------------


HLE_d_common_f_corr = HLE_d_common_f %>% cor() %>% broom::tidy() %>% select(1, 2) %>% filter(abs(HLE_2016)>0.4) %>% arrange(desc(HLE_2016))
HLE_d_common_f_corr %<>%  left_join(var, by=c(".rownames"="var_name_Eng")) %>%
  select(4,2)


# table tex file name
file="HLE_d_common_f_corr.tex"
path="./final_report-kenko(和歌山県)/table/"

sink(file = paste0(path, file))
HLE_d_common_f_corr %>%
  JcXtable(label = "HLE_d_common_f_corr.tex", caption ="健康寿命と説明変数の相関(女性,共通変数)" , size = "\\tiny" , place = "top")
sink()

system('
cd "./final_report-kenko(和歌山県)/table/";
sed -e "s/㎡/m$^2$/g" "HLE_d_common_f_corr.tex" > "temp.tex";
python3 ../../exex.py "temp.tex" "temp1.tex";
mv -f "temp1.tex" "HLE_d_common_f_corr.tex"
')



# -----------------
HLE_d_common_m_corr = HLE_d_common_m %>% cor() %>% broom::tidy() %>% select(1, 2) %>% filter(abs(HLE_2016)>0.4) %>% arrange(desc(HLE_2016))
HLE_d_common_m_corr %<>%  left_join(var, by=c(".rownames"="var_name_Eng")) %>%
  select(4,2)


# table tex file name
file="HLE_d_common_m_corr.tex"
path="./final_report-kenko(和歌山県)/table/"

sink(file = paste0(path, file))
HLE_d_common_m_corr %>%
  JcXtable(label = "HLE_d_common_m_corr.tex", caption ="健康寿命と説明変数の相関(男性,共通変数)" , size = "\\tiny" , place = "top")
sink()

system('
cd "./final_report-kenko(和歌山県)/table/";
sed -e "s/㎡/m$^2$/g" "HLE_d_common_m_corr.tex" > "temp.tex";
python3 ../../exex.py "temp.tex" "temp1.tex";
mv -f "temp1.tex" "HLE_d_common_m_corr.tex"
')

# -----------------
LE_d_f_corr = LE_d_f %>% cor() %>% broom::tidy() %>% select(1, 2) %>% filter(abs(LE_2015)>0.4) %>% arrange(desc(LE_2015))
LE_d_f_corr %<>%  left_join(var, by=c(".rownames"="var_name_Eng")) %>%
  select(4,2)


# table tex file name
file="LE_d_f_corr.tex"
path="./final_report-kenko(和歌山県)/table/"

sink(file = paste0(path, file))
LE_d_f_corr %>%
  JcXtable(label = "LE_d_f_corr.tex", caption ="平均寿命と説明変数の相関(女性,性別変数)" , size = "\\tiny" , place = "top")
sink()

system('
cd "./final_report-kenko(和歌山県)/table/";
sed -e "s/㎡/m$^2$/g" "LE_d_f_corr.tex" > "temp.tex";
python3 ../../exex.py "temp.tex" "temp1.tex";
mv -f "temp1.tex" "LE_d_f_corr.tex"
')


# -----------------
LE_d_m_corr = LE_d_m %>% cor() %>% broom::tidy() %>% select(1, 2) %>% filter(abs(LE_2015)>0.4) %>% arrange(desc(LE_2015))
LE_d_m_corr %<>%  left_join(var, by=c(".rownames"="var_name_Eng")) %>%
  select(4,2)


# table tex file name
file="LE_d_m_corr.tex"
path="./final_report-kenko(和歌山県)/table/"

sink(file = paste0(path, file))
LE_d_m_corr %>%
  JcXtable(label = "LE_d_m_corr.tex", caption ="平均寿命と説明変数の相関(男性,性別変数)" , size = "\\tiny" , place = "top")
sink()

system('
cd "./final_report-kenko(和歌山県)/table/";
sed -e "s/㎡/m$^2$/g" "LE_d_m_corr.tex" > "temp.tex";
python3 ../../exex.py "temp.tex" "temp1.tex";
mv -f "temp1.tex" "LE_d_m_corr.tex"
')


# -----------------
HLE_d_f_corr = HLE_d_f %>% cor() %>% broom::tidy() %>% select(1, 2) %>% filter(abs(HLE_2016)>0.4) %>% arrange(desc(HLE_2016))
HLE_d_f_corr %<>%  left_join(var, by=c(".rownames"="var_name_Eng")) %>%
  select(4,2)


# table tex file name
file="HLE_d_f_corr.tex"
path="./final_report-kenko(和歌山県)/table/"

sink(file = paste0(path, file))
HLE_d_f_corr %>%
  JcXtable(label = "HLE_d_f_corr.tex", caption ="健康寿命と説明変数の相関(女性,性別変数)" , size = "\\tiny" , place = "top")
sink()

system('
cd "./final_report-kenko(和歌山県)/table/";
sed -e "s/㎡/m$^2$/g" "HLE_d_f_corr.tex" > "temp.tex";
python3 ../../exex.py "temp.tex" "temp1.tex";
mv -f "temp1.tex" "HLE_d_f_corr.tex"
')


# -----------------
HLE_d_m_corr = HLE_d_f %>% cor() %>% broom::tidy() %>% select(1, 2) %>% filter(abs(HLE_2016)>0.4) %>% arrange(desc(HLE_2016))
HLE_d_m_corr %<>%  left_join(var, by=c(".rownames"="var_name_Eng")) %>%
  select(4,2)


# table tex file name
file="HLE_d_m_corr.tex"
path="./final_report-kenko(和歌山県)/table/"

sink(file = paste0(path, file))
HLE_d_m_corr %>%
  JcXtable(label = "HLE_d_m_corr.tex", caption ="健康寿命と説明変数の相関(男性,性別変数)" , size = "\\tiny" , place = "top")
sink()

system('
cd "./final_report-kenko(和歌山県)/table/";
sed -e "s/㎡/m$^2$/g" "HLE_d_m_corr.tex" > "temp.tex";
python3 ../../exex.py "temp.tex" "temp1.tex";
mv -f "temp1.tex" "HLE_d_m_corr.tex"
')










# ----------------- used var # -----------------
#  _   _              _  __     ___    ____
# | | | |___  ___  __| | \ \   / / \  |  _ \
# | | | / __|/ _ \/ _` |  \ \ / / _ \ | |_) |
# | |_| \__ \  __/ (_| |   \ V / ___ \|  _ <
#  \___/|___/\___|\__,_|    \_/_/   \_\_| \_\
# ----------------- used var # -----------------



# -----------------




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
