### WiDS presentation materials

## load libraries
library(tidyverse)
library(plotly)
library(sf)
library(scales)

## read in data
covid_data <- read_csv("COVID-19_Case_Surveillance_Public_Use_Data_with_Geography.csv")
covid_data <- covid_data %>% 
  mutate_at(c(1,2,4,6), as.numeric)

## create two dataframes to reduce size
# df 1 - subset data to 2 states: tx & ca
state_df <- covid_data %>% filter(res_state == "CA" | res_state == "TX")
state_df$date <- as.yearmon(paste(state_df$year, state_df$month), "%Y %m") #repeat fixing date for both instead of large df

# df 2 - create pivot table of cases, deaths, and hospitalizations for all u.s. counties by month
covid_pa <- covid_data %>%
  group_by(year, month, state_fips_code, res_state, county_fips_code) %>% 
  summarize(cases = n())

covid_pb <- covid_data %>%
  filter(death_yn == "Yes") %>%
  group_by(year, month, state_fips_code, res_state, county_fips_code) %>% 
  summarize(deaths = n())

covid_pc <- covid_data %>%
  filter(hosp_yn == "Yes") %>%
  group_by(year, month, state_fips_code, res_state, county_fips_code) %>% 
  summarize(hospitalizations = n())

cpivot <-merge(covid_pa,covid_pb, by = c("year", "month", "state_fips_code", "res_state", "county_fips_code"), all = T)
covpivot <- merge(cpivot, covid_pc, by = c("year", "month", "state_fips_code", "res_state", "county_fips_code"), all = T)
covpivot$date <- as.yearmon(paste(covpivot$year, covpivot$month), "%Y %m") #repeat fixing date for both instead of large df

## Save dfs as csv and rm large covid_data variable / clean up environment
## remaining variables will be covpivot, state_curr, and state_df
write.csv(state_df, 'covid_state.csv')
write.csv(covpivot, 'covid_pivot.csv')
rm(covid_data, covid_pa, covid_pb, covid_pc, cpivot)

## visualizing

# Map of cases
data(state.fips)
stabbr <- read.csv("stabbr.csv")
states <- covpivot %>% left_join(state.fips, by=c('state_fips_code'='fips'))
states <- states %>% left_join(stabbr, by=c('res_state'='Abb'))
states$month <- sprintf("%02d", states$month) 
states$dator <- paste0(states$year, "-", states$month)

cp <- states %>%
  group_by(dator, date, res_state, State) %>% 
  summarize(cases_total= sum(cases)) %>% 
  mutate(hover=paste(State, " (", res_state, ") \n ", cases_total, "cases"))


fontStyle = list(
  size = 15,
  color = "black"
)

label = list(
  bgcolor = "#EEEEEE",
  bordercolor = "transparent",
  font = fontStyle
)

casegraph <- plot_geo(cp,
                     locationmode = "USA-states") %>% 
  add_trace(locations= ~res_state,
            z = ~cases_total,
            zmin = 0,
            zmax = 10000,
            frame = ~dator,
            color= ~cases_total,
            colors = "Purples",
            text = ~hover,
            hoverinfo = "text") %>% 
  layout(geo = list(scope = "usa"),
         title = "COVID-19 Monthly Cases\n2020-present") %>% 
  style(hoverlabel = label) %>% 
  config(diplayModeBar = FALSE)

casegraph
htmlwidgets::saveWidget(as_widget(casegraph), "casegraph.html") 

#line chart of cases in texas and california

scp <- state_df %>%
  group_by(date, state_fips_code, res_state) %>% 
  summarize(cases = n())

caseline <- ggplot(scp, aes(date, cases)) +
  geom_line(aes(color = res_state),size=2) +
  scale_color_manual(values = c("steelblue", "darkred")) +
  geom_dl(aes(label = res_state), method = list(dl.trans(x = x - 0.5),
                                           "top.bumpup", cex = 1.5,fontface='bold')) +
  scale_y_continuous(labels = label_number(suffix = " M", scale = 1e-6)) +
  labs(title = "COVID-19 Cases in California and Texas\n2020-present")

caseline + theme(legend.position="none", plot.title = element_text(size=22), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank())

# deaths
#### Removed chart because death data in this dataset are not accurate/reliable ####

