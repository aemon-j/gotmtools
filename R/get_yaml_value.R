#' @title Extract values from yaml file
#' @description
#'Inputs values into yaml file by locating the label and key within the yaml file. Preserves comments (#) if present. NOTE: this does not use a yaml parser so if there are yaml formatting errors this function will not pick them up.
#' @param file filepath; to .yaml which you wish to edit
#' @param label string; which corresponds to section where the key is located
#' @param key string; name of key in which to input the value
#' @export
#' @author
#'Tadhg Moore
#' @examples
#' yaml_file <- system.file('extdata/gotm.yaml', package = 'GOTMr')
#' get_yaml_value(file = yaml_file, label = 'airp', key = 'file')
#' @importFrom yaml read_yaml
#' @export
#'
get_yaml_value <- function(file = 'gotm.yaml', label, key){

  yml <- yaml::read_yaml(file)
  val <- yml[[label]][[key]]

  if(is.null(val)) {
    stop(paste0(label, "/", key), ' not found in ', file)
  }
  return(val)
}
