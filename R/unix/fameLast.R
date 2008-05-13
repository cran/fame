.Last.fame <- function(){
  if(exists("fameRunning") && fameRunning()) fameStop()
  if(validServerIsRunning()) endServerSession()
}
