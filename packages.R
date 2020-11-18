## library() calls go here
library(conflicted) # explicitly control overlapping function names
# library(dotenv) # not sure, something with the env params used in drake, stops
# stargazer from working. Not using stargazer anymore, so could enable.
library(drake) # better, make-like project management
library(tidyverse) # 
library(here) # relative project paths
library(janitor) # cleaning names, freq tables
library(curl)
library(jsonlite)
library(future)
library(furrr)

conflicted::conflict_prefer("filter", "dplyr")
conflicted::conflict_prefer("gather", "tidyr")
conflicted::conflict_prefer("lag", "dplyr")
conflicted::conflict_prefer("map", "purrr")
