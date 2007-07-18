askForString <- function(prompt = "?", default = ""){
  if(runningWindows() && .Platform$GUI == "Rgui")
    return(winDialogString(prompt, default))
  if(missing(default)) prompt <- paste(prompt, ":", sep = "")
  else                 prompt <- paste(prompt, " [", default, "]:", sep = "")

  input <- readline(prompt = prompt)
  if(input == "") default
  else input
}

askForPassword <- function(prompt = "Password"){
  askForString(prompt)
}

