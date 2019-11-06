#' Edit nml file
#'
#' Input values and file names into the GOTM namelist files
#'
#' @param val value; value to input into .nml file
#' @param nml .nml file; for the parameters to be input into
#' @param par parameter for which the value will be replaced
#' @param print logical; prints changes to .nml field to the console. Defaults to TRUE.
#' @return data
#' @importFrom XML xmlParse
#' @importFrom XML saveXML
#' @export
input_nml <- function(val,nml, par, print =T){
  fil <- list.files()[grep(nml, list.files())]
  file <- glmtools::read_nml(fil)
  init = glmtools::get_nml_value(file, par)
  file <- glmtools::set_nml(glm_nml = file, arg_name = par,arg_val = val)
  glmtools::write_nml(file,fil)
  if(print == T){
    message('In file "',fil,'" and parameter ',par,' - Replaced ',init,' with ',val)
  }
}
