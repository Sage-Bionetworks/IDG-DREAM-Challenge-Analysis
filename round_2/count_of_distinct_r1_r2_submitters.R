library(synapser)
library(tidyverse)


get_all_participants <- function(id){
  
  test <- try(synGetUserProfile(id)$name, silent = T) ##check if user, if is, return input id
  
  if(class(test)=='try-error'){ ##if it is not a user, will return error, so then try to get team members
    
    try({
      mem <- synGetTeamMembers(id) %>% as.list()
      sapply(mem, function(x){
        foo <- x$json() %>% jsonlite::fromJSON(.) 
        foo$member$ownerId
      })
    })
  }else{
    return(id)
  }
}


r1 <- synGet('syn17054264')$path %>% read_csv
r2 <- synGet('syn18520916')$path %>% read_csv


res <- sapply(unique(c(r1$submitterId, r2$submitterId)), get_all_participants)
res_flat <- flatten_chr(res)
length(res_flat)
