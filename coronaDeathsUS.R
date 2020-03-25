## Using data from JHU
pacman::p_load(char=c('tidyverse','janitor', 'coronavirus','ggrepel',
                      'gghighlight','fs', 'git2r'))
# update_datasets(silence=T)

## On 2020-03-23, JHU changed their data updates and format, so the coronavirus
## package is not being updated properly. Now grabbing data directly from
## JHU by pulling the repository locally and processing the daily reports, since
## the cumulative time series doesn't provide state-level data anymore.

data_repo <- '~/GitHub/COVID-19/'
git2r::pull(data_repo)

raw_data_dir <- '~/GitHub/COVID-19/csse_covid_19_data/csse_covid_19_daily_reports/'
fnames <- dir_ls(raw_data_dir, glob = '*.csv')

states <- state.name; names(states) <- state.abb
raw_data <- map(fnames, function(x){ read_csv(x) %>%
    clean_names() %>%
    select(province_state, country_region, deaths)})
names(raw_data) <- fnames %>% basename()
raw_data <- bind_rows(raw_data, .id = 'date') %>%
  mutate(date = as.Date(date, format = '%m-%d-%y')) %>%
  filter(country_region=='US')
raw_data <- raw_data %>%
  mutate(province_state = ifelse(province_state=='Chicago', 'Chicago, IL', province_state)) %>%
  separate(province_state, c('loc','state'), sep = ', ', fill = 'left') %>%
  mutate(state = str_remove_all(state, '\\.')) %>%
  filter(state %in% c(state.abb, state.name, 'District of Columbia','DC')) %>%
  mutate(state = ifelse(state %in% state.abb, states[state], state)) %>%
  mutate(state = ifelse(state =='DC', 'District of Columbia',state)) %>%
  select(date, state, country_region, deaths) %>%
  group_by(state, date) %>%
  summarize(deaths = sum(deaths, na.rm=T)) %>%
  ungroup() %>%
  arrange(state,date)


deaths_us <- raw_data %>%
  filter(deaths >= 5) %>%
  group_by(state) %>%
  mutate(days = as.numeric(date - min(date))) %>%
  ungroup()

top_10 <- deaths_us %>%
	group_by(state) %>%
	filter(days == max(days)) %>%
	ungroup() %>%
	top_n(10, deaths)

us_plot <- ggplot(deaths_us,
	   aes(x = days, y = deaths, color = state))+
	geom_line(show.legend=F)+
	geom_point(data = deaths_us %>% group_by(state) %>% filter(days==max(days)) %>% ungroup(),
	           aes(size = deaths), show.legend=F)+
  gghighlight(state %in% top_10$state)+
	scale_y_log10('Cumulative number of deaths')+
	theme_classic() +
	labs(x = 'Days since fifth death',
		 caption = glue::glue('Source: JHU, updated {max(deaths_us$date)}'))+
  geom_abline(intercept = log10(5), slope = log10(2)/c(2,3,4), linetype=2, color = 'grey')

y_range <- ggplot_build(us_plot)$layout$panel_scales_y[[1]]$range$range
y_target <- y_range[1] + diff(y_range)*0.95
x_target <- (y_target - y_range[1])/(log10(2)/c(2,3,4))

print(
  us_plot +
    annotate('text', y = 10^y_target, x = 1, label = 'Doubling in ...', color='grey')+
    annotate('text', y = 10^y_target, x = x_target, label = paste(2:4, 'days'), color='grey')
)
