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

deaths_after_10 <- deaths %>%
  filter(deaths >= 100) %>%
  arrange(date) %>%
  group_by(country_region) %>%
  mutate(days = as.numeric(date - min(date))) %>%
  ungroup()

top_ten <- deaths_after_10 %>%
  filter(date==max(date)) %>%
  top_n(10, deaths)


staticplot <- ggplot(deaths_after_10, aes(x = days, y = deaths)) +
  geom_line(aes(color = country_region), show.legend = F)+
  geom_point(data = deaths_after_10 %>%
               group_by(country_region) %>%
               filter(days == max(days)) %>%
               ungroup(),
             aes(color = country_region, size=deaths),
              show.legend=F)+
  gghighlight(country_region %in% top_ten$country_region)+
  geom_abline(intercept = log10(100), slope = log10(2)/c(2,4,6),
              color = 'grey', linetype=2)+
  scale_y_log10('Cumulative deaths')+
  labs(x = 'Days since 100th death')+
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
