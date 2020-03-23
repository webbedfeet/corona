# remotes::install_github('ramikrispin/coronavirus')
library(coronavirus)
update_datasets(silence=TRUE)
pacman::p_load(char=c('tidyverse','janitor', 'ggrepel'))

deaths <- coronavirus %>%
  clean_names() %>%
  filter(type=='death') %>%
  group_by(country_region, date) %>%
  summarise(cases = sum(cases)) %>%
  ungroup() %>%
  group_by(country_region) %>%
  arrange(date) %>%
  mutate(cum_cases = cumsum(cases)) %>%
  ungroup() %>%
  as_tibble()

top_ten <- deaths %>%
  filter(date==max(date)) %>%
  top_n(10, cum_cases)

deaths_top10 <-
  deaths %>%
  filter(country_region %in% top_ten$country_region) %>%
  group_by(country_region) %>%
  filter(cum_cases >= 10) %>%
  mutate(days = as.numeric(date - min(date))) %>%
  ungroup()

(staticplot <- ggplot(deaths_top10, aes(x = days, y = cum_cases)) +
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
             aes(color = country_region, size=cum_cases),
              show.legend=F)+
    geom_abline(intercept=1, slope = log10(1.33), linetype=2)+
    annotate('text',label = '33% growth', x = 21, y = 10^(1+log10(1.33)*21),
             angle = 60, vjust=0)+
  scale_y_log10('Cumulative deaths')+
  labs(x = 'Days since 10th death')+
  labs(caption = glue::glue('Data last updated: {max(deaths$date)}'))+
  theme_classic()
)

interactive_plot <- plotly::ggplotly(staticplot)
