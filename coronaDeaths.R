# remotes::install_github('ramikrispin/coronavirus')
# library(coronavirus)
# update_datasets(silence=TRUE)
pacman::p_load(char=c('tidyverse','janitor', 'ggrepel'))

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


top_ten <- deaths %>%
  filter(date==max(date)) %>%
  top_n(11, deaths)

deaths_top10 <-
  deaths %>%
  filter(country_region %in% top_ten$country_region) %>%
  group_by(country_region) %>%
  filter(deaths >= 10) %>%
  mutate(days = as.numeric(date - min(date))) %>%
  ungroup()

(staticplot <- ggplot(deaths_top10, aes(x = days, y = deaths)) +
  geom_label_repel(data = deaths_top10 %>%
                     filter(country_region %in% top_ten$country_region) %>%
                     group_by(country_region) %>%
                     filter(days == max(days)) %>%
                     ungroup(),
                   aes(color = country_region, label = country_region),
                  vjust=1, hjust = 1, show.legend=F)+
  geom_line(aes(color = country_region), show.legend = F)+
  geom_point(data = deaths_top10 %>%
               filter(country_region %in% top_ten$country_region) %>%
               group_by(country_region) %>%
               filter(days == max(days)) %>%
               ungroup(),
             aes(color = country_region, size=deaths),
              show.legend=F)+
    geom_abline(intercept=1, slope = log10(1.33), linetype=2)+
    annotate('text',label = '33% growth', x = 21, y = 10^(1+log10(1.33)*21),
             angle = 63, vjust=0)+
  scale_y_log10('Cumulative deaths')+
  labs(x = 'Days since 10th death')+
  labs(caption = glue::glue('Source: JHU, updated {max(deaths$date)}'))+
  theme_classic()
)

# interactive_plot <- plotly::ggplotly(staticplot)
