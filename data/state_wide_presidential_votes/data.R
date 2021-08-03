library(dplyr)
library(ggplot2)
past_votes <- readRDS('./pres_vote_historical.RDS') %>% #GSOC/data/hierarchical_gp
	filter(state != 'DC')

past_votes %>% head()
table(past_votes$state)
state_groups <- list(c("ME","NH","VT","MA","RI","CT"),
                      c("NY","NJ","PA","MD","DE"),
                      c("OH","MI","IL","WI","MN"),
                      c("WA","OR","CA","HI"),
                      c("AZ","CO","NM","NV"),
                      c("IA","NE","KS","ND","SD"),
                      c("KY","TN","MO","WV","IN"),
                      c("VA","OK","FL","TX","NC"),
                      c("AL","MS","LA","GA","SC","AR"),
                      c("MT","ID","WY","UT","AK"))
region_names <- c("New England", "Mid-Atlantic", "Midwest", "West Coast",
                  "Southwest","Plains", "Border South", "Outer South", "Deep South",
                  "Mountain West")
state_region_map <- mapply(FUN = function(states, region)
  data.frame(state = states,
             region = rep(region,length(states)),
             stringsAsFactors = F),state_groups,region_names,
  SIMPLIFY = F)

state_region_map <- bind_rows(state_region_map) %>%
  arrange(state) %>% mutate(
    region_ind = as.integer(as.factor(region))
  )

year_map <- data.frame(year = sort(unique(past_votes$year)),
                       year_ind = 1:11)
past_votes <- past_votes %>%
  arrange(state, year) %>%
  left_join(state_region_map, by = 'state') %>%
  left_join(year_map, by = 'year') %>%
  mutate(
    state_ind = as.integer(as.factor(state)),
    two_party_turnout = dem + rep,
    y = rep / two_party_turnout
  )

head(past_votes[,c('year','state','state_ind','region','region_ind','y')])

tail(past_votes[,c('year','state','state_ind','region','region_ind','y')])

past_votes %>%
	ggplot(aes(x = year, y = y, colour = state)) +
	geom_line() + facet_wrap(~ region) +
	theme_bw() + theme(legend.position = 'None') +
	ylab('Republican share of two-party vote') + xlab('Year')

stan_state_region_map <- unique(past_votes[,c('state_ind','region_ind')]) %>%
	arrange(state_ind)

stan_state_region_map %>% head()

data <- with(past_votes,
				list(
					N = dim(past_votes)[1],
					state_region_ind = stan_state_region_map$region_ind,
					N_states = length(unique(past_votes$state)),
					N_regions = length(unique(past_votes$region)),
					N_years_obs = length(unique(past_votes$year)),
					state_ind = state_ind,
					region_ind = region_ind,
					y = y,
					year_ind = year_ind,
					N_years = 14))


