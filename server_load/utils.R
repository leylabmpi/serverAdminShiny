format_time = function(x){
  x = as.POSIXct(as.POSIXlt(x,tz=Sys.timezone()))
  return(x)
}

as.Num = function(x){
  x = as.numeric(as.character(x))
  return(x)
}

#' where are the log files
which_file = function(log_file){
  vol_dir = '/Volumes/abt3_projects/databases/server/'
  vm_dir = '/ebio/abt3_projects/databases/server/'
  F = file.path(vol_dir, log_file)
  if(! file.exists(F)){
    F = file.path(vm_dir, log_file)
  }
  return(F)
}