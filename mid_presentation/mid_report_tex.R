system("open .")


Wakayama::d %>% dim()

Wakayama::JpnEng %>% tbl_df() %>% xtable::xtable()



Wakayama::d_m %>% select(LE_2015) %>% bind_cols(Wakayama::pref["pref.J"]) %>% dplyr::arrange(LE_2015) %>% mutate(rank=row_number()) %>%
  xtable::xtable()


Wakayama::d_f %>% select(LE_2015) %>% bind_cols(Wakayama::pref["pref.J"]) %>% dplyr::arrange(LE_2015) %>% mutate(rank=row_number()) %>%
  xtable::xtable()


Wakayama::d_m %>% select(HLE_2016) %>%
  bind_cols(Wakayama::pref["pref.J"]) %>%
  dplyr::arrange(HLE_2016) %>% mutate(rank=row_number()) %>%
  xtable::xtable()

Wakayama::d_f %>% select(HLE_2016) %>%
  bind_cols(Wakayama::pref["pref.J"]) %>%
  dplyr::arrange(HLE_2016) %>% mutate(rank=row_number()) %>%
  xtable::xtable()






jc.dotplot <- function(x) {
  # x is HLE or LE in c()　vector
  names(x)<-Wakayama::pref$pref.J
  x<-x[order(x)]

}

colfunc <- colorRampPalette(c("gray90","black"))

par(family= "HiraKakuProN-W3")


pdf("./fig/wakayama_LE_M.pdf", family="Japan1")

dotchart(
  main = paste("平均寿命(2015年, 男性), 全国平均",
               mean(Wakayama::d_m$LE_2015) %>% round(2),
               "歳"),
  jc.dotplot(Wakayama::d_m$LE_2015),
  cex = 0.7,
  lcolor = "gray90",
  pch = 19,
  col = colfunc(47),
  pt.cex = 1.5
)

abline(v = mean(Wakayama::d_m$LE_2015), lty = 2)

dev.off()



pdf("./fig/wakayama_LE_F.pdf", family="Japan1")

dotchart(
  main = paste("平均寿命(2015年, 女性), 全国平均",
               mean(Wakayama::d_f$LE_2015) %>% round(2),
               "歳"),
  jc.dotplot(Wakayama::d_f$LE_2015),
  cex = 0.7,
  lcolor = "gray90",
  pch = 19,
  col = colfunc(47),
  pt.cex = 1.5
)

abline(v = mean(Wakayama::d_f$LE_2015), lty = 2)

dev.off()



pdf("./fig/wakayama_HLE_M.pdf", family="Japan1")

dotchart(
  main = paste("健康寿命(2016年, 男性), 全国平均",
               mean(Wakayama::d_m$HLE_2016) %>% round(2),
               "歳"),
  jc.dotplot(Wakayama::d_m$HLE_2016),
  cex = 0.7,
  lcolor = "gray90",
  pch = 19,
  col = colfunc(47),
  pt.cex = 1.5
)

abline(v = mean(Wakayama::d_m$HLE_2016), lty = 2)

dev.off()



pdf("./fig/wakayama_HLE_F.pdf", family="Japan1")

dotchart(
  main = paste("健康寿命(2016年, 女性), 全国平均",
               mean(Wakayama::d_f$HLE_2016) %>% round(2),
               "歳"),
  jc.dotplot(Wakayama::d_f$HLE_2016),
  cex = 0.7,
  lcolor = "gray90",
  pch = 19,
  col = colfunc(47),
  pt.cex = 1.5
)

abline(v = mean(Wakayama::d_f$HLE_2016), lty = 2)

dev.off()



# -----------------
# to report

p<-Wakayama::d_common %>% dplyr::select_if(is.numeric) %>% sapply(rank) %>%
  tbl_df() %>%dplyr::filter(key==30) %>% t() %>% as.data.frame() %>%
  tibble::rownames_to_column() %>% tbl_df() %>%
  mutate(rowname=forcats::fct_reorder(rowname, V1))




p1<-p[1:33,] %>%
  # theme_bw(base_family = "HiraKakuProN-W3")+
  ggplot(aes(x=rowname, y=V1))+geom_point()+
  theme(axis.text.x=element_text(angle=90,hjust=1))+
  labs(x="variable name ", y="rank")




p2<-p[34:66,] %>%
  # theme_bw(base_family = "HiraKakuProN-W3")+
  ggplot(aes(x=rowname, y=V1))+geom_point()+
  theme(axis.text.x=element_text(angle=90,hjust=1))+
  labs(x="variable name ", y="rank")


p3<-p[67:100,] %>%
  # theme_bw(base_family = "HiraKakuProN-W3")+
  ggplot(aes(x=rowname, y=V1))+geom_point()+
  theme(axis.text.x=element_text(angle=90,hjust=1))+
  labs(x="variable name ", y="rank")



p1 %>% ggsave(filename = "on working/report-kenko(和歌山県)/fig/WakayamRank1.pdf", width = 8,
              height = 8)

p2 %>% ggsave(filename = "on working/report-kenko(和歌山県)/fig/WakayamRank2.pdf", width = 8,
              height = 8)

p3 %>% ggsave(filename = "on working/report-kenko(和歌山県)/fig/WakayamRank3.pdf", width = 8,
              height = 8)

system("open .")
p2

p3





Wakayama::d_common %>% dplyr::select_if(is.numeric) %>% sapply(rank) %>%
  tbl_df() %>%dplyr::filter(key==30) %>% pivot_longer(everything()) %>% arrange(value)%>% xtable::xtable()

# -----------------



# 使用package
suppressMessages(library(readxl))
suppressMessages(library(dplyr))
suppressMessages(library(data.table))
suppressMessages(library(kableExtra))
suppressMessages(library(curl))
suppressMessages(library(tidyverse))
suppressMessages(library(plotly))


plot_ly(
  x =Wakayama::d_common_standarize_long$value,
  type = "histogram",
  name = "Histogram",
  frame =  ~Wakayama::d_common_standarize_long$var_name
)




p4 <-
  Wakayama::d_common_standarize_long %>%
  ggplot(aes(x = value, color = var_name)) +
  geom_density() +
  # theme_bw(base_family = "HiraKakuProN-W3") +
  theme_bw() +
  theme(legend.position = 'none') +
  stat_function(fun = dnorm,
                args = list(mean = 0, sd = 1),
                size=3,
                color="black"
            )


p4 %>% ggsave(filename = "on working/report-kenko(和歌山県)/fig/DistCommondVariable.pdf", width = 8,
            height = 8)



?stat_function



JpnEng<-read.csv("data/DataFormat-32021年3月11日受領/JpnEng.csv") %>% tbl_df()


Wakayama::d_common_normality_test %>% tibble::rownames_to_column()%>%
  filter(p.value<0.05) %>% left_join(JpnEng, c("rowname"="var_name_Eng"))


library(plotly)



temp<-left_join(
  Wakayama::d_common_standarize_long,
  Wakayama::d_common,
                by = c("pref.id"="key")) %>% select(pref.J, 2,3)


temp1<-temp %>% filter(pref.J=="和歌山"|
                  pref.J=="青森"|
                  pref.J=="滋賀"|
                  pref.J=="長野")

ggplot(temp1, aes(x=pref.J, y=value, color=pref.J))+
  theme_bw(base_family = "HiraKakuProN-W3")+
  facet_wrap(~var_name)+geom_point()

temp2<-temp1 %>% mutate(var_group=
                   round(as.numeric(as.factor(var_name))/10, 0)

                 )

temp3<-temp2 %>% group_by(var_group) %>% nest()



# 以下operato 修正 2021年3月24日
# i=1 to 11



pp1<-temp3$data[1] %>% as.data.frame() %>% ggplot(aes(x=pref.J, y=value, color=pref.J))+theme_bw(base_family = "HiraKakuProN-W3")+facet_wrap(~var_name)+geom_point(size=5)
pp2<-temp3$data[2] %>% as.data.frame() %>% ggplot(aes(x=pref.J, y=value, color=pref.J))+theme_bw(base_family = "HiraKakuProN-W3")+facet_wrap(~var_name)+geom_point(size=5)
pp3<-temp3$data[3] %>% as.data.frame() %>% ggplot(aes(x=pref.J, y=value, color=pref.J))+theme_bw(base_family = "HiraKakuProN-W3")+facet_wrap(~var_name)+geom_point(size=5)
pp4<-temp3$data[4] %>% as.data.frame() %>% ggplot(aes(x=pref.J, y=value, color=pref.J))+theme_bw(base_family = "HiraKakuProN-W3")+facet_wrap(~var_name)+geom_point(size=5)
pp5<-temp3$data[5] %>% as.data.frame() %>% ggplot(aes(x=pref.J, y=value, color=pref.J))+theme_bw(base_family = "HiraKakuProN-W3")+facet_wrap(~var_name)+geom_point(size=5)
pp6<-temp3$data[6] %>% as.data.frame() %>% ggplot(aes(x=pref.J, y=value, color=pref.J))+theme_bw(base_family = "HiraKakuProN-W3")+facet_wrap(~var_name)+geom_point(size=5)
pp7<-temp3$data[7] %>% as.data.frame() %>% ggplot(aes(x=pref.J, y=value, color=pref.J))+theme_bw(base_family = "HiraKakuProN-W3")+facet_wrap(~var_name)+geom_point(size=5)
pp8<-temp3$data[8] %>% as.data.frame() %>% ggplot(aes(x=pref.J, y=value, color=pref.J))+theme_bw(base_family = "HiraKakuProN-W3")+facet_wrap(~var_name)+geom_point(size=5)
pp9<-temp3$data[9] %>% as.data.frame() %>% ggplot(aes(x=pref.J, y=value, color=pref.J))+theme_bw(base_family = "HiraKakuProN-W3")+facet_wrap(~var_name)+geom_point(size=5)
pp10<-temp3$data[10] %>% as.data.frame() %>% ggplot(aes(x=pref.J, y=value, color=pref.J))+theme_bw(base_family = "HiraKakuProN-W3")+facet_wrap(~var_name)+geom_point(size=5)
pp11<-temp3$data[11] %>% as.data.frame() %>% ggplot(aes(x=pref.J, y=value, color=pref.J))+theme_bw(base_family = "HiraKakuProN-W3")+facet_wrap(~var_name)+geom_point(size=5)



pp1<-temp3$data[1] %>% as.data.frame() %>% ggplot(aes(x=pref.J, y=value, color=pref.J))+facet_wrap(~var_name)+geom_point(size=5)
pp2<-temp3$data[2] %>% as.data.frame() %>% ggplot(aes(x=pref.J, y=value, color=pref.J))+facet_wrap(~var_name)+geom_point(size=5)
pp3<-temp3$data[3] %>% as.data.frame() %>% ggplot(aes(x=pref.J, y=value, color=pref.J))+facet_wrap(~var_name)+geom_point(size=5)
pp4<-temp3$data[4] %>% as.data.frame() %>% ggplot(aes(x=pref.J, y=value, color=pref.J))+facet_wrap(~var_name)+geom_point(size=5)
pp5<-temp3$data[5] %>% as.data.frame() %>% ggplot(aes(x=pref.J, y=value, color=pref.J))+facet_wrap(~var_name)+geom_point(size=5)
pp6<-temp3$data[6] %>% as.data.frame() %>% ggplot(aes(x=pref.J, y=value, color=pref.J))+facet_wrap(~var_name)+geom_point(size=5)
pp7<-temp3$data[7] %>% as.data.frame() %>% ggplot(aes(x=pref.J, y=value, color=pref.J))+facet_wrap(~var_name)+geom_point(size=5)
pp8<-temp3$data[8] %>% as.data.frame() %>% ggplot(aes(x=pref.J, y=value, color=pref.J))+facet_wrap(~var_name)+geom_point(size=5)
pp9<-temp3$data[9] %>% as.data.frame() %>% ggplot(aes(x=pref.J, y=value, color=pref.J))+facet_wrap(~var_name)+geom_point(size=5)
pp10<-temp3$data[10] %>% as.data.frame() %>% ggplot(aes(x=pref.J, y=value, color=pref.J))+facet_wrap(~var_name)+geom_point(size=5)
pp11<-temp3$data[11] %>% as.data.frame() %>% ggplot(aes(x=pref.J, y=value, color=pref.J))+facet_wrap(~var_name)+geom_point(size=5)



pp1	 %>% ggsave(filename = "on working/report-kenko(和歌山県)/fig/WakayamAomoriComp1.pdf", width = 8, height = 8, family="Japan1")
pp2	 %>% ggsave(filename = "on working/report-kenko(和歌山県)/fig/WakayamAomoriComp2.pdf", width = 8, height = 8, family="Japan1")
pp3	 %>% ggsave(filename = "on working/report-kenko(和歌山県)/fig/WakayamAomoriComp3.pdf", width = 8, height = 8, family="Japan1")
pp4	 %>% ggsave(filename = "on working/report-kenko(和歌山県)/fig/WakayamAomoriComp4.pdf", width = 8, height = 8, family="Japan1")
pp5	 %>% ggsave(filename = "on working/report-kenko(和歌山県)/fig/WakayamAomoriComp5.pdf", width = 8, height = 8, family="Japan1")
pp6	 %>% ggsave(filename = "on working/report-kenko(和歌山県)/fig/WakayamAomoriComp6.pdf", width = 8, height = 8, family="Japan1")
pp7	 %>% ggsave(filename = "on working/report-kenko(和歌山県)/fig/WakayamAomoriComp7.pdf", width = 8, height = 8, family="Japan1")
pp8	 %>% ggsave(filename = "on working/report-kenko(和歌山県)/fig/WakayamAomoriComp8.pdf", width = 8, height = 8, family="Japan1")
pp9	 %>% ggsave(filename = "on working/report-kenko(和歌山県)/fig/WakayamAomoriComp9.pdf", width = 8, height = 8, family="Japan1")
pp10 %>% ggsave(filename = "on working/report-kenko(和歌山県)/fig/WakayamAomoriComp10.pdf", width = 8, height = 8, family="Japan1")
pp11 %>% ggsave(filename = "on working/report-kenko(和歌山県)/fig/WakayamAomoriComp11.pdf", width = 8, height = 8, family="Japan1")

system("open .")
temp3$data[1] %>% as.data.frame() %>% ggplot(aes(x=pref.J, y=value, color=pref.J))+
  theme_bw(base_family = "HiraKakuProN-W3")+
  facet_wrap(~var_name)+geom_point(size=5)


temp3$data[2] %>% as.data.frame() %>% ggplot(aes(x=pref.J, y=value, color=pref.J))+
  theme_bw(base_family = "HiraKakuProN-W3")+
  facet_wrap(~var_name)+geom_point(size=10)


temp3$data[11] %>% as.data.frame() %>% ggplot(aes(x=pref.J, y=value, color=pref.J))+
  theme_bw(base_family = "HiraKakuProN-W3")+
  facet_wrap(~var_name)+geom_point(size=10)





#
#   temp1$var_name %>% as.factor() %>% as.numeric() %>% range()
# %>%
#   plot_ly(y =  ~ pref.J,
#           x =  ~ value,
#           color =  ~ pref.J) %>%
#   add_bars(frame=~var_name,
#            hoverinfo="text",
#            text=~paste(var_name))




