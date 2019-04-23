library(synapser)
library(tidyverse)
evaluation_id <- 9614192L
upload_dir_id <- "syn18487587" #put the synapse id of the directory you want to upload the files to

synLogin()

upload_file_to_synapse <- function(
  path, synapse_id, annotation_list = NULL, activity_obj = NULL){

  entity <- synapser::File(
    path = path,
    parent = synapse_id,
    annotations = annotation_list)
  entity <- synapser::synStore(entity, activity = activity_obj)
  return(entity)
}

get_path <- function(x){
  path <- x[['filePath']]
  id <- x[['id']]
  c(path,id)
}


get_details <- function(x){
  id <- x[['id']]
  userId <- x[['userId']]
  teamId <- x[['teamId']]
  if(is.null(teamId)){
    teamId <- NA
  }
  versionNumber <- x[['versionNumber']]

  foo <- c("submissionId"=id,"userId"=userId,"teamId"=teamId,"versionNumber"=versionNumber)
}

syn_ids <- synapser::synGetSubmissions(evaluation_id, status = "ACCEPTED") %>%
  synapser::as.list() %>%
  purrr::map_chr("id") %>%
  as.integer()

###submission 9684841 was incorrectly marked as invalid, but was scored...add it here
syn_ids <- c(syn_ids, 9684841L)

submissions <- purrr::map(syn_ids, synGetSubmission) %>%
  set_names(syn_ids)

details <- purrr::map(submissions, get_details)
paths <- purrr::map(submissions, get_path)

corrected.paths<- lapply(paths, function(x){
  foo <- read_csv(x[1])
  my.file <- tempfile(pattern = paste0(x[2],"_"), fileext = ".csv")
  write.csv(foo,file=my.file, row.names = F)
  return(my.file)
}) %>% set_names(syn_ids)

list <- list('path'=corrected.paths,'annotation_list'=details)

entities <- pmap(list, upload_file_to_synapse, synapse_id = upload_dir_id)

