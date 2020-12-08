library(shinythemes)
library(dplyr)
library(tidyverse)
library(tidyr)
library(ggplot2)
library(leaflet)
require(maps)
library(ggmap)
library(maps)
library(mapproj)
library(viridis)
library(ggthemes)
library(shiny)
library(plotly)
library(DT)
library(highcharter)
library(hrbrthemes)
library(lubridate)


all_state <- read.csv("us_state_level_clean_2020-12-02.csv", stringsAsFactors = FALSE)
all_state$Date <- parse_date(all_state$Date, "%m/%d/%y")
state_name <- unique(all_state$Province_State)
all_state$month <- month(all_state$Date)
all_state$month <- as.factor(all_state$month)

all_state_geo <- all_state
all_state_geo$state <- all_state_geo$Province_State
# The maps package has geographic information on all U.S states
us.states <- map_data("state")
us.states <- as_data_frame(us.states)
us.states <- dplyr::rename(us.states, state = region)
us.states$subregion = NULL
us.states$state <- str_to_title(us.states$state)

# Add State Abbreviations and Centers
statenames <- as_data_frame(
  cbind(state=state.name, state.abb = state.abb, 
        state.center.x = state.center$x, 
        state.center.y = state.center$y))
statenames <- statenames %>% mutate_each_(funs(as.numeric), 
                                          vars=c("state.center.x","state.center.y"))
us.states <- left_join(us.states, statenames)
all_state_sp <- left_join(all_state_geo, us.states, by='state')

make_map <- function(date = "2020-12-02"){
  all_state_sp %>%
    filter(Date == date)%>%
    ggplot(aes(x = long, y = lat, group=group)) + 
    geom_polygon(aes(fill= per_100k_new_confirmed_7), color="white")+
    scale_fill_gradient2_tableau(palette = "Orange-Blue Diverging", trans = "reverse")+
    geom_text(data=statenames, inherit.aes = FALSE,   
              aes(label=state.abb,   
                  x=state.center.x, y=state.center.y),  
              colour="white")+
    labs(fill = "New Confirmed Cases Per 100k People")+
    theme_map() + 
    theme(legend.position = "bottom",
          legend.title = element_text(size=8, face="bold"),
          legend.key = element_rect(colour = "transparent", fill = "white"),
          legend.justification = c(1,0)
    )+
    coord_map(projection = "mercator")
}

make_table_edition_old <- function(date="2020-12-02"){
  headers <- c("Date", "State", "Population", "Accumalated Confirmed", "Accumulated Deaths", "New Confirmed", "New Deaths")
  
  data_for_table<- all_state %>%
    select(Date, StateName, population, Confirmed, Deaths, new_confirmed, new_deaths) %>%
    filter(Date == date) %>%
    arrange(desc(new_confirmed))
  
  datatable(data_for_table, 
            colnames = headers, 
            rownames = FALSE,
            filter = list(position = "top"),
            options = list(language = list(sSearch = "Filter:")))
} 

make_table <- function(date="2020-12-02"){
  headers <- c("State", "New Confirmed", "New Deaths")
  
  data_for_table<- all_state %>%
    filter(Date == date) %>%
    select(StateName, new_confirmed, new_deaths) %>%
    arrange(desc(new_confirmed))
  
  datatable(data_for_table, 
            colnames = headers, 
            rownames = FALSE,
            filter = list(position = "top"),
            options = list(language = list(sSearch = "Filter:")))
}


mobility_new_case <- function(state='New York', date="2020-12-02"){
  mobility_cols <- c('retail_recreation',
                     'grocery_pharmacy',
                     'parks',
                     'transit',
                     'work',
                     'residential')
  
  state_cases <- all_state %>%
    filter(Province_State == state, Date <= date) %>%
    gather(key='index', value='index_value', mobility_cols, factor_key=TRUE) 
  
  ggplot(state_cases, aes(x=Date)) +
    geom_bar(aes(y=new_confirmed), stat="identity", size=.2, color="Goldenrod1", alpha=.2)+
    geom_line(aes(y=index_value*300, color=index))+
    scale_x_date(breaks = "1 months", date_labels = "%b")+
    scale_y_continuous(
      name = "New Confirmed Cases",
      sec.axis = sec_axis(~./300, name="Mobility Indices"))+
    ggtitle(paste0("The change of new confirmed cases and mobility indices in ", state)) +
    theme_minimal()+
    theme(plot.title = element_text(
      size = rel(1.5), hjust = 0.5, lineheight = .9,
      family = "Times", face = "bold.italic", colour = "black"), 
      legend.position="top",
      legend.direction = "horizontal",
      legend.title = element_blank(),
      legend.text = element_text(size = 10),
      legend.justification = 0.5,
      legend.key.height=unit(1,"line"),
      legend.key.width=unit(3,"line"))
}


policy_new_cases <- function(state='New York', date="2020-12-02"){
  policy_cols <- c('stringency','containment')
  
  policy_state_cases <- all_state %>%
    filter(Province_State == state, Date <= date) %>%
    gather(key='policy', value='policy_value', policy_cols, factor_key=TRUE) 
  
  ggplot(policy_state_cases, aes(x=Date)) +
    geom_bar(aes(y=new_confirmed), stat="identity", size=.2, color="Goldenrod1", alpha=.2)+
    geom_line(aes(y=policy_value*100, color=policy))+
    scale_x_date(breaks = "1 months", date_labels = "%b")+
    scale_y_continuous(
      name = "New Confirmed Cases",
      sec.axis = sec_axis(~./100, name="Mobility Indices"))+
    ggtitle(paste0("The change of new confirmed cases and policy indices in ", state)) +
    theme_minimal()+
    theme(plot.title = element_text(
      size = rel(1.5), hjust = 0.5, lineheight = .9,
      family = "Times", face = "bold.italic", colour = "black"), 
      legend.position="top",
      legend.direction = "horizontal",
      legend.title = element_blank(),
      legend.text = element_text(size = 10),
      legend.justification = 0.5,
      legend.key.height=unit(1,"line"),
      legend.key.width=unit(3,"line"))
}

box_values <- function(state = 'New York', date= "2020-12-01"){
  certain_state <- all_state %>%
    filter(Province_State == state, Date == date)
  
  confirmed_cases <- certain_state$Confirmed
  confirmed_deaths <- certain_state$Deaths
  new_confirmed <- certain_state$new_confirmed
  new_deaths <- certain_state$new_deaths
  
  box_values_list <- list(confirmed_cases, confirmed_deaths, new_confirmed, new_deaths)
  return(box_values_list)
  
}


new_lines <- function(date ="2020-12-02", indicators = c("new_cases", "new_deaths", "new_cases_7", "new_deaths_7", "new_cases_14", "new_deaths_14")){
  cols <- indicators
  nation <- all_state %>%
    group_by(as.factor(Date)) %>%
    summarise(
              new_cases = sum(new_confirmed),
              new_deaths = sum(new_deaths),
              new_cases_7 = sum(new_confirmed_7),
              new_deaths_7 = sum(new_deaths_7),
              new_cases_14 = sum(new_confirmed_14),
              new_deaths_14 = sum(new_deaths_14))%>%
    rename(Date = "as.factor(Date)")
  
  nation$Date <- as.Date(nation$Date)
  
  nation %>%
    filter(Date <= date) %>%
    gather(key="indicator", value="number", cols, factor_key = TRUE)%>%
    ggplot(aes(x=Date, y=number, group=indicator, color=indicator, shape=indicator)) +
    geom_line() +
    scale_color_viridis(discrete = TRUE, option='plasma', direction=-1, alpha = 1) +
    labs(col = NULL)+
    theme_bw() +
    ylab("New Cases/Deaths")+
    scale_x_date(breaks="1 month", date_labels = "%B")+
    theme(legend.position = "bottom")
}

cum_lines <- function(date ="2020-12-02"){
  #cols <- c("total_cases", "total_deaths")
  nation <- all_state %>%
    group_by(as.factor(Date)) %>%
    summarise( total_cases = sum(Confirmed),
               total_deaths = sum(Deaths))%>%
    rename(Date = "as.factor(Date)")
  
  nation$Date <- as.Date(nation$Date)
  
  
  nation %>%
    filter(Date <= date) %>%
    ggplot(aes(x=Date)) +
    geom_line(aes(y=total_cases, color='cases'), size=1) +
    geom_line(aes(y=total_deaths*10, color='deaths'), size=1)+
    scale_color_manual(name = NULL, 
                       values = c('cases' = 'red', 'deaths' = 'black'),
                       labels = c("Cases", "Deaths"))+
    scale_y_continuous(
      name = "Total Confirmed Cases",
      sec.axis = sec_axis(~./10, name="Total Deaths"))+
    theme_bw() +
    ylab("Cummulative Cases/Deaths")+
    scale_x_date(breaks="1 month", date_labels = "%B")+
    theme(legend.position = "bottom")
}


make_box <- function(state="New York", indicator = "New Confirmed Cases"){
  state_data <- all_state %>%
    filter(Province_State == state)
  
  base_1 <- max(state_data$new_confirmed)
  base_2 <- max(state_data$new_deaths)
  
  if(indicator == "New Confirmed Cases"){
    
    p<- all_state %>%
      filter(Province_State == state) %>%
      group_by(month) %>%
      ggplot(aes(Date))+
      geom_boxplot(aes(y=new_confirmed, group=month, fill=month),alpha=0.4)+
      geom_line(aes(y=per_100k_new_confirmed_7*300), color="#69b3a2", size=1, alpha=0.9, linetype= 2)+
      geom_text(aes(x = as.Date("2020-09-01"), y = 3/4*base_1, label = " -- Weekly Avarage (Per 100K People)"), color="#69b3a2")+
      scale_x_date(date_labels = "%b", breaks = "1 months")+
      scale_fill_viridis(discrete=TRUE)+
      ggtitle(state)+
      scale_y_continuous(
        name = indicator,
        sec.axis = sec_axis(~./300, name=NULL))+
      theme_bw()+
      theme(legend.position = "none",
            plot.title = element_text(
              hjust =0.5, lineheight = .9,
              face = "bold", colour = "black"))
  } 
  else if(indicator == "New Deaths"){
    
    p<- all_state %>%
      filter(Province_State == state) %>%
      group_by(month) %>%
      ggplot(aes(Date))+
      geom_boxplot(aes(y=new_deaths, group=month, fill=month))+
      geom_line(aes(y=per_100k_new_deaths_7*300), color="#A0A0A0", size=1, alpha=0.9, linetype= 2)+
      geom_text(aes(x = as.Date("2020-09-01"), y = 3/4*base_2, label = " -- Weekly Avarage (Per 100K People)"), color="#A0A0A0")+
      scale_x_date(date_labels = "%b", breaks = "1 months")+
      scale_fill_viridis(discrete=TRUE, option="inferno")+
      ggtitle(state)+
      scale_y_continuous(
        name = indicator,
        sec.axis = sec_axis(~./300, name=NULL))+
      theme_bw()+
      theme(legend.position = "none",
            plot.title = element_text(
              hjust =0.5, lineheight = .9,
              face = "bold", colour = "black"))
  }
  
  return(p)
}
  
  
