
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


aa=d_f %>% select(-key, -pref.id,  -pref.E, -pref.A, -pref.J, -sex, -pop) %>%
  select(LE_2015,everything()) %>% select(-HLE_2016)


bbb=glm(LE_2015~., data=aa[,1:30])

cc=step(bbb, direction = "backward")

cc %>% broom::tidy()

#
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
