
suppressMessages(library(tidyverse))

data <- tibble(count = rpois(n = 1000, lambda = 20))
data

data %>%
  ggplot(
    aes(
      x = count
    )
  ) +
  stat_ecdf()


data_ecdf <-
  ggplot_build(
    data %>%
      ggplot(
        aes(
          x = count
        )
      ) +
      stat_ecdf(pad = FALSE)
  )$data[[1]]





ggplot_build(data_ecdf %>% tbl_df() %>% ggplot()+
               geom_point(aes(x, y))
)$data[[1]]




aa=ggplot_build(iris %>% ggplot(aes(Sepal.Width))+
               stat_ecdf()
)

aa$data[[1]] %>% tbl_df() %>% mutate(y1=1-y) %>% ggplot()+
  geom_line(aes(x, y1))



aa$data



















