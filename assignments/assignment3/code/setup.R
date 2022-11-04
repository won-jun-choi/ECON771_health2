if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, janitor, reshape2, here, haven, xtable,
               rdrobust, rddensity)

WAU <- function(name="wonjun") {
  if (name=="wonjun"){
    if (Sys.info()['sysname'] == "Darwin") {  # mac
      dir_root <<- "/Users/wonjun/Dropbox/Emory/ECON771_health2/assignments/assignment3"
      dir_data <<- "/Users/wonjun/Dropbox/Emory/ECON771_health2/assignments/assignment3/input"
    }
    if (Sys.info()['sysname'] == "Linux") {
      dir_root <<- "~/Dropbox/Emory/ECON771_health2/assignments/assignment3"
      dir_data <<- "~/Dropbox/Emory/ECON771_health2/assignments/assignment3/input"
    }
    print(paste0("Hi ",name,". You are using ",Sys.info()['sysname']))
  }
  
  if (name=="haha"){
    print('haha')
  }
}