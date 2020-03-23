## Using data from JHU
pacman::p_load(char=c('tidyverse','janitor', 'coronavirus','ggrepel'))
update_datasets(silence=T)

raw_data <- coronavirus %>%
	clean_names() %>%
	filter(type=='death') %>%
   	filter(country_region == 'US')

deaths_us <- raw_data %>%
	group_by(province_state) %>%
	arrange(date) %>%
	mutate(cum_death = cumsum(cases)) %>%
	ungroup() %>%
	filter(cum_death >= 5) %>%
	group_by(province_state) %>%
	arrange(date) %>%
	mutate(days = as.numeric(date - min(date)))

top_10 <- deaths_us %>%
	group_by(province_state) %>%
	filter(days == max(days)) %>%
	ungroup() %>%
	top_n(10, cum_death)

ggplot(deaths_us, 
	   aes(x = days, y = cum_death, color = province_state))+
	geom_label_repel(data = top_10, aes(x = days, y=cum_death, label = province_state, color = province_state),
					 hjust = 1, show.legend=F)+ 
	geom_line(show.legend=F)+
	geom_point(data = top_5, aes(size = cum_death), show.legend=F)+
	scale_y_log10('Cumulative number of deaths')+
	theme_classic() + 
	labs(x = 'Days since fifth death',
		 caption = glue::glue('Source: JHU, updated {max(deaths_us$date)}'))

# 
# raw_data <- read_csv('https://covidtracking.com/api/states/daily.csv') %>%
#   mutate(date = as.Date(as.character(date), format = '%Y%m%d'))
# 
# deaths_us <- raw_data %>% filter(!is.na(death)) %>%
#   filter(death >= 5) %>%
#   group_by(state) %>%
#   arrange(date) %>%
#   mutate(days = as.numeric(date-min(date))) %>%
#   ungroup()
# 
# top_5 <- deaths_us %>%
#   group_by(state) %>%
#   filter(days == max(days)) %>%
#   ungroup() %>%
#   top_n(5, death)
# 
# ggplot(deaths_us %>% filter(state != 'MA'), aes(x = days, y = death, color = state))+
#   geom_label_repel(data = top_5, aes(x = days, y = death, label=state, color=state),
#                    hjust = 1, show.legend = F)+
#   geom_line(show.legend=F) +
#   geom_point(data=top_5, aes(size = death), show.legend = F)+
#   scale_y_log10('Cumulative number of deaths')+
#   theme_classic()+
#   labs(x = 'Days since 2020-03-11 or fifth death',
#        caption = "Source: covidtracking.com")
