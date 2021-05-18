
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


# summary(d_mf)



## --------------------------------------------------
d_m %>% DT::datatable()


## --------------------------------------------------
d_f %>% DT::datatable()


## --------------------------------------------------
d_common %>% colnames() %>% enframe() %>% DT::datatable()


## --------------------------------------------------
d_m %>% colnames() %>% enframe() %>% DT::datatable()



## --------------------------------------------------
d_f %>% colnames() %>% enframe() %>% DT::datatable()


## ---- warning=FALSE--------------------------------
suppressMessages(library(plotly))

t<-d_m %>% select(pref.J,HLE_2016,LE_2015) %>%
  mutate(pref.J=forcats::fct_reorder(pref.J, LE_2015))
t$pref.J

t %>%
  plot_ly() %>%
  add_segments(
    x=~HLE_2016,y=~pref.J ,
    xend=~LE_2015,yend=~pref.J ,
    # x = ~c, y = ~model,
    # xend = ~h, yend = ~model,
    color = I("gray"), showlegend = FALSE
  ) %>%
  add_markers(
    # x = ~c, y = ~model,
    x=~HLE_2016,y=~pref.J ,
    color = I("blue"),
    name = "健康寿命"
  ) %>%
  add_markers(
    # x = ~h, y = ~model,
    x=~LE_2015,y=~pref.J ,
    color = I("red"),
    name  = "平均寿命"
  ) %>%
  layout(
    xaxis = list(
      range=c(60,83),
      title="男性の平均寿命と健康寿命の差"
    )
  )



## ---- warning=FALSE--------------------------------

suppressMessages(library(plotly))

t<-d_f %>% select(pref.J,HLE_2016,LE_2015) %>%
  mutate(pref.J=forcats::fct_reorder(pref.J, LE_2015))
t$pref.J

t %>%
  # plot_ly(width = 600, height = 1000) %>%
  plot_ly() %>%
  add_segments(
    x=~HLE_2016,y=~pref.J ,
    xend=~LE_2015,yend=~pref.J ,
    # x = ~c, y = ~model,
    # xend = ~h, yend = ~model,
    color = I("gray"), showlegend = FALSE
  ) %>%
  add_markers(
    # x = ~c, y = ~model,
    x=~HLE_2016,y=~pref.J ,
    color = I("blue"),
    name = "健康寿命"
  ) %>%
  add_markers(
    # x = ~h, y = ~model,
    x=~LE_2015,y=~pref.J ,
    color = I("red"),
    name  = "平均寿命"
  ) %>%
  layout(
    xaxis = list(
      range=c(60,88),
      title="女性の平均寿命と健康寿命の差"
    )
  )



## --------------------------------------------------
d_m %>% select(LE_2015) %>% bind_cols(Wakayama::pref["pref.J"]) %>% dplyr::arrange(LE_2015) %>% mutate(rank=row_number()) %>%DT::datatable()


## --------------------------------------------------
jc.dotplot <- function(x) {
  # x is HLE or LE in c()　vector
  names(x)<-Wakayama::pref$pref.J
  x<-x[order(x)]

}

colfunc <- colorRampPalette(c("gray90","black"))

par(family= "HiraKakuProN-W3")

dotchart(
  main = "平均寿命(2015年, 男性)",
  jc.dotplot(d_m$LE_2015),
  cex = 0.7,
  lcolor = "gray90",
  pch = 19,
  col = colfunc(47),
  pt.cex = 1.5
)

abline(v = 79, lty = 2)



## --------------------------------------------------
d_m %>% select(HLE_2016) %>% bind_cols(Wakayama::pref["pref.J"]) %>% dplyr::arrange(HLE_2016) %>% mutate(rank=row_number()) %>%DT::datatable()


## --------------------------------------------------
jc.dotplot <- function(x) {
  # x is HLE or LE in c()　vector
  names(x)<-Wakayama::pref$pref.J
  x<-x[order(x)]

}

colfunc <- colorRampPalette(c("gray90","black"))

par(family= "HiraKakuProN-W3")

dotchart(
  main = "健康寿命(2016年, 男性)",
  jc.dotplot(d_m$HLE_2016),
  cex = 0.7,
  lcolor = "gray90",
  pch = 19,
  col = colfunc(47),
  pt.cex = 1.5
)

abline(v = 79, lty = 2)



## --------------------------------------------------
d_f %>% select(LE_2015) %>% bind_cols(Wakayama::pref["pref.J"]) %>% dplyr::arrange(LE_2015) %>% mutate(rank=row_number()) %>%DT::datatable()


## --------------------------------------------------
jc.dotplot <- function(x) {
  # x is HLE or LE in c()　vector
  names(x)<-Wakayama::pref$pref.J
  x<-x[order(x)]

}

colfunc <- colorRampPalette(c("gray90","black"))

par(family= "HiraKakuProN-W3")

dotchart(
  main = "平均寿命(2015年, 女性)",
  jc.dotplot(d_f$LE_2015),
  cex = 0.7,
  lcolor = "gray90",
  pch = 19,
  col = colfunc(47),
  pt.cex = 1.5
)

abline(v = 79, lty = 2)



## --------------------------------------------------
d_f %>% select(HLE_2016) %>% bind_cols(Wakayama::pref["pref.J"]) %>% dplyr::arrange(HLE_2016) %>% mutate(rank=row_number()) %>%DT::datatable()


## --------------------------------------------------
jc.dotplot <- function(x) {
  # x is HLE or LE in c()　vector
  names(x)<-Wakayama::pref$pref.J
  x<-x[order(x)]

}

colfunc <- colorRampPalette(c("gray90","black"))

par(family= "HiraKakuProN-W3")

dotchart(
  main = "平均寿命(2015年, 女性)",
  jc.dotplot(d_f$HLE_2016),
  cex = 0.7,
  lcolor = "gray90",
  pch = 19,
  col = colfunc(47),
  pt.cex = 1.5
)

abline(v = 79, lty = 2)



## --------------------------------------------------
d_common %>% dplyr::select_if(is.numeric) %>% sapply(rank) %>%
  tbl_df() %>%dplyr::filter(key==30) %>% t() %>% as.data.frame() %>% DT::datatable()


## ---- warning=FALSE--------------------------------

# purrr::map(iris[,-5], ~hist(.x))

suppressMessages(library(plotly))


d_common_standarize<-
  d_common %>% dplyr::select_if(is.numeric) %>%
  select(-key, -pref.id) %>% scale() %>% tbl_df()

d_common_standarize<-bind_cols(d_common["pref.id"], d_common_standarize)


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





