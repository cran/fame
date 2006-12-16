askForString <- function(prompt = "?: ", default = ""){
  if(runningWindows() && .Platform$GUI == "Rgui")
    winDialogString(prompt, default)
  else readline(prompt = prompt)
}

askForPassword <- function(prompt = "Password: "){
  askForString(prompt)
}
