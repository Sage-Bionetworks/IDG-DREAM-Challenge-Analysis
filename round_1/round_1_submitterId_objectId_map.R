library(tidyverse)
library(synapser)
synLogin()
##from challengeutils query 'select createdOn, name, objectId, submitterId, team, userId from evaluation_9614078 where status == "SCORED"' > round_1_submitterIds.csv

objectId <- read_csv('round_1_submitterIds.csv')

synStore(File("round_1_submitterIds.csv", parentId = "syn17083205"))

##get names function
get_user_or_team_names <- function(id){
  name <- try(synGetTeam(id)$name, silent = T) ##try to get the team name from id 
  if(class(name)=='try-error'){ ##if it is not a team, will return error, so then try to get user profile
    try({
      prof <- synGetUserProfile(id = id) ##get first and last name
      fn <- prof$firstName
      ln <- prof$lastName
      if(is.null(fn) | is.null(ln)){
        un <- prof$userName
        return(un) 
      }else if(fn == "" | ln == ""){ ##if empty, get username instead
        un <- prof$userName
        return(un)
      }else{
        return(paste(fn, ln))
      }
    })
  }else{
    return(name)
  }
}

#examples:: 
get_user_or_team_names("3342573") #Robert Allaway
get_user_or_team_names("3379336") #PlosCB Baseline
