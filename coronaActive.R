# remotes::install_github('ramikrispin/coronavirus')
# library(coronavirus)
# update_datasets(silence=TRUE)
pacman::p_load(char=c('tidyverse','janitor', 'ggrepel', 'gghighlight'))

# Due to changes in JHU data storage, I'm now directly downloading from JHU repo
# rather than use the coronavirus package.
deaths <- read_csv('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv') %>%
  gather(date, deaths, -(1:4)) %>%
  clean_names() %>%
  mutate(date = as.Date(date, format = '%m/%d/%y')) %>%
  group_by(country_region, date) %>%
  summarize(deaths = sum(deaths, na.rm=T)) %>%
  ungroup() %>%
  arrange(date)

confirmed <- read_csv('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv') %>%
  gather(date, cases, -(1:4)) %>%
  clean_names() %>%
  mutate(date = as.Date(date, format = '%m/%d/%y')) %>%
  group_by(country_region, date) %>%
  summarize(cases = sum(cases, na.rm=T)) %>%
  ungroup() %>%
  arrange(date)

recovered <- read_csv('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv') %>%
  gather(date, recovered, -(1:4)) %>%
  clean_names() %>%
  mutate(date = as.Date(date, format = '%m/%d/%y')) %>%
  group_by(country_region, date) %>%
  summarize(recovered = sum(recovered, na.rm=T)) %>%
  ungroup() %>%
  arrange(date)


dat <- confirmed %>% left_join(deaths) %>% left_join(recovered)

dat_after_1000 <- dat %>% filter(cases >= 1000)
dat_after_1000 <- dat_after_1000 %>%
  mutate(active = cases - deaths - recovered) %>%
  group_by(country_region) %>%
  mutate(days = as.numeric(date - min(date))) %>%
  ungroup()

dat_top10 <- dat_after_1000 %>%
  filter(!is.na(active)) %>%
  filter(date == max(date)) %>%
  top_n(10, active)

staticplot <- ggplot(dat_after_1000 %>% filter(country_region!='China') %>%
         filter(country_region %in% dat_top10$country_region), aes(days, active)) +
  geom_line(aes(color = country_region), show.legend=F)+
  geom_label_repel(data = dat_after_1000 %>%
                     filter(country_region %in% dat_top10$country_region) %>%
                     filter(date == unique(dat_top10$date)),
                   aes(label = country_region, color= country_region), show.legend = F)+
  # gghighlight(country_region %in% dat_top10$country_region,
  #             label_key=country_region)+
  # geom_abline(intercept = log10(1000), slope = log10(2)/c(2,4,6),
              # color = 'grey', linetype=2)+
  # scale_y_log10('Active infections')+
  labs(x = 'Days since 1000th confirmed case')+
  labs(caption = glue::glue('Source: JHU, updated {max(deaths$date)}'))+
  theme_classic()


x_range <- ggplot_build(staticplot)$layout$panel_scales_x[[1]]$range$range
y_range <- ggplot_build(staticplot)$layout$panel_scales_y[[1]]$range$range
y_target <- y_range[1] + diff(y_range)*0.95
x_target <- (y_target - y_range[1])/(log10(2)/c(2,4,6))
x_0 <- x_range[1] + diff(x_range)*0.05

print(
  staticplot + annotate('text', y = 10^y_target, x = x_0, label = 'Doubling in ...', color='grey')+
    annotate('text', y = 10^y_target, x = x_target, label = paste(c(2,4,6), 'days'), color='grey')
)
# interactive_plot <- plotly::ggplotly(staticplot)
