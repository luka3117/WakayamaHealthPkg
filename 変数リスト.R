---
title: "来店解析 : no_profile_20200330.csv"
subtitle: "来店確率解析 "
author: "滋賀大学 佐藤、李"
date: "最終更新: 2021/05/28"
# date: "最終更新: 2019/07/16"

output:
  # word_document:
  html_document:
  # html_notebook:
   number_section: true
   toc: true
  # fig_width: 18
  # fig_height: 12
---

```{r}
# 使用package
suppressMessages(library(readxl))
suppressMessages(library(dplyr))
suppressMessages(library(data.table))
suppressMessages(library(kableExtra))
suppressMessages(library(curl))
suppressMessages(library(tidyverse))
suppressMessages(library(plotly))
library(DT)
DT::datatable(var)
```


