source('utils.R')

#' Reading in qstat log
qstat_log = function(file, input){
  x = fread(file, sep='\t', header=FALSE, fill=TRUE) 
  x = as.data.frame(x)
  colnames(x) = c('Time', 'X', 'JB_job_number', 'JB_name', 
                  'uname', 'JB_department', 'state', 
                  'io_usage', 'cpu_usage', 'mem_usage')
  x$Time = strptime(x$Time, "%Y-%m-%d %H:%M:%S")
  
  # filter df
  min_time = max(x$Time) - input$num_hours2 * 60 * 60
  x = x[x$Time >= min_time,]
  x$Time = as.character(x$Time)
  
  # summarizing
  x = x %>%
    group_by(Time, uname) %>%
    summarize(n_jobs = n(),
              io_usage = sum(as.Num(io_usage), na.rm=TRUE),
              cpu_usage = sum(as.Num(cpu_usage), na.rm=TRUE),
              mem_usage = sum(as.Num(mem_usage), na.rm=TRUE)) %>%
    ungroup()
  return(x)
}

