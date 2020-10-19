# ============================================================================================================
# Name:   sample_data.R
# Author: Gethryn Ghavalas gethfromthefuture@gmail.com
# Date:   2020-10-17
# Desc:   Generates a list of teams, their tasks and avg time in secs per task; 
#         then simulates data using random poisson process for each task between
#         given start and end dates.
# ============================================================================================================


# -[TODO]-----------------------------------------------------------------------------------------------------
# 1: better way to simulate task data so it is seasonal, has trend, possibly
#    simulate ARIMA data or something like that?
# 2: use purrr to generate data functionally  √
# 3: weekend data should be NA not 0 if not working; add na.rm's everywhere.

# -[GET CONFIG.YAML]------------------------------------------------------------------------------------------
require(yaml); .config <- yaml::read_yaml("config.yaml")

# -[PACKAGES]-------------------------------------------------------------------------------------------------
.config.packages.new = .config$packages[! .config$packages %in% installed.packages()]
if (length(.config.packages.new) != 0) install.packages(.config.packages.new)
sapply(.config$packages, require, character = TRUE)

# -[CONSTANTS]------------------------------------------------------------------------------------------------

# %||% substitutes a default if config.yaml reference returns NULL

# number of teams to simulate
NUM_TEAMS = .config$constants$NUM_TEAMS %||% 3

# the start and end date of days to simulate, with a vector of dates
START_DATE = as.Date(.config$constants$START_DATE %||% "2018-01-01")
END_DATE = as.Date(.config$constants$END_DATE %||% "2020-09-30")
DAYS = seq(from=START_DATE, to=END_DATE, by = "day")

# remove volume from Sat and Sun if FALSE, work all days when TRUE
WORK_WEEKENDS = .config$constants$WORK_WEEKENDS %||% FALSE
WEEKEND_DAYS = .config$constants$WEEKEND_DAYS %||% c("Saturday","Sunday")

# when simulating task counts per day, use a uniform prediction in this range 
MIN_NUM_TASKS_PER_DAY = .config$constants$MIN_NUM_TASKS_PER_DAY %||% 0
MAX_NUM_TASKS_PER_DAY = .config$constants$MAX_NUM_TASKS_PER_DAY %||% 100

# the number of tasks to simulate in each team (min, max)
MIN_TASKS_PER_TEAM = .config$constants$MIN_TASKS_PER_TEAM %||% 10
MAX_TASKS_PER_TEAM = .config$constants$MAX_TASKS_PER_TEAM %||% 25

# running the forecast takes a while, set this to TRUE if you want to run them automatically
DO_FC = .config$constants$DO_FC %||% FALSE
FC_HORIZON = .config$constants$FC_HORIZON %||% "3 months"


# -[FUNCTIONS]------------------------------------------------------------------------------------------------

# generate a list of team names, with the number of tasks that team has
generate_teams <- function(num_teams=NUM_TEAMS, 
                           min_tasks=MIN_TASKS_PER_TEAM, max_tasks=MAX_TASKS_PER_TEAM) {
  tasks_per_team = round(runif(num_teams, min_tasks,max_tasks))
  adjectives <- .config$team_names$adjectives %||% c("Big")
  mascots <- .config$team_names$mascots %||% c("Dragons")
  team_names <- crossing(adjectives, mascots) %>% 
    mutate(name = toupper(paste(adjectives, mascots, "Team"))) %>% 
    select(name)
  out <- tibble(team=sample(team_names$name, num_teams, replace=FALSE),
                num_tasks=tasks_per_team)
  out
}

# generate a list of task names, base volumes (for poisson process) and task lengths (secs)
generate_tasks <- function(num_tasks, 
                                    min_tasks = MIN_NUM_TASKS_PER_DAY, 
                                    max_tasks = MAX_NUM_TASKS_PER_DAY) {
  verbs <- .config$task_names$verbs %||% c("Cleaning")
  adjectives <- .config$task_names$adjectives %||% c("Complex")
  nouns <-.config$task_names$nouns %||%  c("Documents")
  task_names <- crossing(verbs, adjectives, nouns) %>% 
    mutate(name = paste(verbs, adjectives, nouns)) %>% 
    select(name)
  out <- tibble(task=sample(task_names$name, num_tasks, replace=FALSE),
                base_vol=round(runif(num_tasks, min_tasks, max_tasks)),
                std_time=round(runif(num_tasks)*30)*15) 
  out
}

# generate the data between start and end date for a single task using poisson process
# removing weekends if we = TRUE
generate_task_data <- function(base_value, days=DAYS, we=WORK_WEEKENDS) {
  n = length(DAYS)
  data = rpois(n, base_value)
  out = tibble(date=DAYS, volume=data)
  if (!we) { out$volume[weekdays(DAYS) %in% WEEKEND_DAYS] <- 0 }
  out
}

# generates the entire data set: teams, their tasks, and task data as a tsibble
generate_all <- function(num_teams=NUM_TEAMS, min_tasks=MIN_TASKS_PER_TEAM,
                         max_tasks=MAX_TASKS_PER_TEAM) {
  
  # make the teams
  tm <- generate_teams(num_teams, min_tasks, max_tasks)
  
  # add tasks to each team
  df <- map2_df(tm$team, tm$num_tasks,
                ~ tibble(team=.x,
                         generate_tasks(.y, min_tasks, max_tasks))) %>%
    mutate(team=as.factor(team), task=as.factor(task))
  
  # create the list to use in pmap
  ddf <- list(team=df$team, task=df$task, std_time=df$std_time, base_vol=df$base_vol)
  
  # output the simulated data for each team/task combo for all DAYS
  pmap_df(ddf, 
          function(team, task, std_time, base_vol, we=WORK_WEEKENDS, days=DAYS)
            tibble(team=team,
                   task=task,
                   std_time=std_time, 
                   generate_task_data(base_vol, days, we)) %>%
            mutate(workload = std_time * volume / 3600) %>%
            select(team, task, date, std_time, volume, workload) %>%
            as_tsibble(key=c(team,task), index=date))
  
}

# -[CODE]-----------------------------------------------------------------------------------------------------

# generate all data (-> dat) and then convert to weekly (-> dat_weekly) and 
# to monthly (-> dat_monthly).
dat <- generate_all() 

dat_weekly <- dat %>%
  group_by_key() %>%
  index_by(week_num = ~ week(.)) %>% # weekly aggregates
  summarise(volume = sum(volume, na.rm = TRUE),
            workload = sum(workload),
            week_start = min(date)) 

dat_monthly <- dat %>%
  group_by_key() %>%
  index_by(year_month = ~ yearmonth(.)) %>% # monthly aggregates
  summarise(volume = sum(volume, na.rm = TRUE),
            workload = sum(workload)) 

# example: hierarchical monthly forecast by team.
hdat <- dat_monthly %>% 
  aggregate_key(team, workload = sum(workload))

hdat %>%
  model(ets=ETS(workload), ARIMA(workload)) %>% 
  #reconcile(bu = bottom_up(ets)) %>%
  forecast(h=FC_HORIZON) %>%
  autoplot(hdat)

# set DO_FC = TRUE to run the forecasts (may take some time)
if (DO_FC) {
  
  # example: produce ARIMA, ETS, SNAIVE, TSLM and THETA models to fit the data
  #          one per task for all teams; returns a mable.
  fit <- dat %>% 
    model(
      ARIMA(workload),
      ETS(workload),
      SNAIVE(workload),
      TSLM(workload),
      THETA(workload)
    )
  
  tidy(fit)
  
  # produce forecasts from each of the models in fit  (each task x 5 models)
  fc <- fit %>% forecast(h=FC_HORIZON)
  
  # graph all of the output
  fc %>% autoplot(dat) +
    #theme_minimal() +
    labs(
      x="Month",
      y= "Workload (hrs)",
      title= "Team A Forecast",
      subtitle = paste(FC_HORIZON,"horizon based on data from",
                       yearmonth(START_DATE), "to", yearmonth(END_DATE))
    )
  
  
  # example: hierarchical forecast by team.
  hdat <- dat_monthly %>% 
    aggregate_key(team, workload = sum(workload))
  
  hdat %>%
    model(ets=ETS(workload), ARIMA(workload)) %>% 
    #reconcile(bu = bottom_up(ets)) %>%
    forecast(h=FC_HORIZON) %>%
    autoplot(hdat)
  
}

# NOTES ========================================================================
# nest() allows you to nest tibbles into other tibbles following group_by()
# then use map(newcol, ~lm(A ~ B + C, data = .))  # e.g. for a linear model on the 
# nested content

# # Create the model summary for each dataset
# model_summary <- bound_models %>% 
#   group_by(imp_model) %>%
#   nest() %>%
#   mutate(mod = map(data, ~lm(sea_temp_c ~ air_temp_c + humidity + year, data = .)),
#          res = map(mod, residuals),
#          pred = map(mod, predict),
#          tidy = map(mod, brom::tidy))

### 
