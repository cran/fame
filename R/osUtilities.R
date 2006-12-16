user <- function() as.vector(Sys.info()["user"])

groups <- function(user. = user()){
  unlist(strsplit(system(paste("groups", user.), intern = T),
                  split = " "))[-(1:2)]
}

pid <- function(){
  ## return the PID of the current R process
  Sys.getpid()
}

pgid <- function(){
  ## return the PGID of the current R.bin process
  cmd <- paste("ps -p", pid(), "-o pgid | grep -v PGID")
  as.integer(system(cmd, intern = T))
}

ppid <- function(){
  ## return the PPID of the current R process
  cmd <- paste("ps -p", pid(), "-o ppid | grep -v PPID")
  as.integer(system(cmd, intern = T))
}

killProcess <- function(pid){
  if(.Platform$OS.type == "windows")
    system(paste("kill --f", pid))
  else
    system(paste("kill -9", pid, ">& /dev/null"))
}

pwd <- function(){
  if(Sys.info()["sysname"] == "Linux")
    system("pwd", intern = T)
  else
    getwd()
}

runningWindows <- function() .Platform$OS.type == "windows"
runningLinux   <- function() Sys.info()["sysname"] == "Linux"

