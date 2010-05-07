## Unfortunately, FAME does not provide any way at all to discover
## how many attributes an object has, or what their names are. You have to
## know what you're looking for.  
getFameUpdated <- function(fname, db){
  attString <- getFameAttribute("updated", fname, db)
  strptime(gsub("\\..*", "", attString), format = "%d-%b-%y")
}

getFameCreated <- function(fname, db){
  attString <- getFameAttribute("created", fname, db)
  strptime(gsub("\\..*", "", attString), format = "%d-%b-%y")
}

