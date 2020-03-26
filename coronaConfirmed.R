# remotes::install_github('ramikrispin/coronavirus')
# library(coronavirus)
# update_datasets(silence=TRUE)
pacman::p_load(char=c('tidyverse','janitor', 'ggrepel', 'gghighlight'))

# Due to changes in JHU data storage, I'm now directly downloading from JHU repo
# rather than use the coronavirus package.
confirmed <- read_csv('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv') %>%
  gather(date, cases, -(1:4)) %>%
  clean_names() %>%
  mutate(date = as.Date(date, format = '%m/%d/%y')) %>%
  group_by(country_region, date) %>%
  summarize(cases = sum(cases, na.rm=T)) %>%
  ungroup() %>%
  arrange(date)

cases_after_1000 <- confirmed %>%
  filter(cases >= 1000) %>%
  arrange(date) %>%
  group_by(country_region) %>%
  mutate(days = as.numeric(date - min(date))) %>%
  ungroup()

top_ten <- cases_after_1000 %>%
  filter(date==max(date)) %>%
  top_n(10, cases)


staticplot <- ggplot(cases_after_1000, aes(x = days, y = cases)) +
  geom_line(aes(color = country_region), show.legend = F)+
  geom_point(data = cases_after_1000 %>%
               group_by(country_region) %>%
               filter(days == max(days)) %>%
               ungroup(),
             aes(color = country_region, size=cases),
             show.legend=F)+
  gghighlight(country_region %in% top_ten$country_region)+
  geom_abline(intercept = log10(1000), slope = log10(2)/c(2,4,6),
              color = 'grey', linetype=2)+
  scale_y_log10('Cumulative confirmed cases', labels = scales::label_comma())+
  labs(x = 'Days since 1000th case')+
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
