if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, here, vroom, xtable, fixest, modelsummary, ivmodel)

WAU <- function(name="wonjun") {
  if (name=="wonjun"){
    if (Sys.info()['sysname'] == "Darwin") {  # mac
      dir_root <<- "/Users/wonjun/Dropbox/Emory/ECON771_health2/assignments/assignment2"
      dir_data <<- "/Users/wonjun/Dropbox/research_data/health"
    }
    if (Sys.info()['sysname'] == "Linux") {
      dir_root <<- "~/Dropbox/Emory/ECON771_health2/assignments/assignment2"
      dir_data <<- "~/Dropbox/research_data/health"
    }
    print(paste0("Hi ",name,". You are on ",Sys.info()['sysname']))
  }
  
  if (name=="haha"){
    print('haha')
  }
}