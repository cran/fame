getFameUpdated <- function(fname, db){
  attString <- getFameAttribute("updated", fname, db)
  strptime(gsub("\\..*", "", attString), format = "%d-%b-%y")
}

getFameCreated <- function(fname, db){
  attString <- getFameAttribute("created", fname, db)
  strptime(gsub("\\..*", "", attString), format = "%d-%b-%y")
}

