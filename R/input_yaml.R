#' @title Input values into yaml file
#' @description
#'Inputs values into yaml file by locating the label and key within the yaml file. Preserves comments (#) if present. NOTE: this does not use a yaml parser so if there are yaml formatting errors this function will not pick them up.
#' @param file filepath; to .yaml which you wish to edit
#' @param label string; which corresponds to section where the key is located
#' @param key string; name of key in which to input the value
#' @param value string; to be input into the key/value pair. Note boolean values must be input as 'true'/'false' as per the yaml format
#' @param out_file filepath; to write the output yaml file (optional); defaults to overwriting file if not specified
#' @export
#' @author
#'Tadhg Moore
#' @examples
#'input_yaml(file = 'gotm.yaml', label = 'airp', key = 'file', value = 'met.dat', out_file = 'samp.yaml')
#'input_yaml(file = 'gotm.yaml', label = 'airp', key = 'file', value = 'met.dat', out_file = 'samp.yaml')
#'input_yaml(file = 'gotm.yaml', label = 'meteo', key = 'calc_evaporation', value = 'true', out_file = 'samp.yaml')

input_yaml <- function(file = 'gotm.yaml', label, key, value, out_file = NULL){
  yml <- readLines(file)
  
  # Prevent from finding labels/keys in comments
  yml_no_comments <- unname(sapply(yml, function(x) strsplit(x, "#")[[1]][1]))
  
  if(is.null(out_file)){
    out_file = file
  }

  #Find index of label
  if(is.null(label)){
    ind_label = 0
  }else{
    label_id <- paste0(label,':')
    ind_label <- grep(label_id, yml_no_comments)
    
    if(length(ind_label) == 0){
      stop(label, ' not found in ', file)
    }
  }

  #Find index of key to replace
  key_id <- paste0(' ',key, ':')
  ind_key = grep(key_id, yml_no_comments)
  if(length(ind_key) == 0){
    stop(key, ' not found in ', label, ' in ', file)
  }
  ind_key = ind_key[ind_key > ind_label]
  ind_map <- ind_key[which.min(ind_key - ind_label)]
  if(length(ind_map) == 0){
    stop(key, ' not found in ', label, ' in ', file)
  }

  #Split to extract comment
  spl1 <- strsplit(yml[ind_map], c('#'))[[1]]
  if(length(spl1) == 2){
    comment <- spl1[2]
  }

  #Split to extract current value and identify pattern to sub in for
  spl_tmp <- strsplit(spl1[1], ": ")[[1]]
  if(length(spl_tmp) == 1){
    spl2 <- ""
  }else{
    spl2 <- spl_tmp[2]
  }

  # if(!is.na(comment)){
    # sub = paste0(' ', value,' #', comment)
  # }else{
    sub = paste0(value,' ')
  # }

  #Sub in new value
  # Addition of \Q and \E is to avoid errors in case spl2 contains
  # characters that could be interpreted as regular expressions
  # see ?base::regex
  yml[ind_map] <- gsub(pattern = paste0("\\Q", spl1[1], "\\E"),
                       replacement = paste0(spl_tmp[1], ": ", sub),
                       x = yml[ind_map])
  
  #Write to file
  writeLines(yml, out_file)
  old_val <- gsub(" ", "", spl2, fixed = TRUE) #remove white space for printing

  message('Replaced ', label, ' ', key, ' ', old_val, ' with ', value)
}
