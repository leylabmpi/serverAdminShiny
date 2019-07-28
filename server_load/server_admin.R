# server admin

df_log_format = function(df, input){
  df = df[df$Time == max(df$Time),]
  to_rm = c('collectl', 'disk-usage', 'server-load', 'small_projects')
  df = df %>%
    mutate(file = gsub('/ebio/abt3_projects/', '', df$file)) %>%
    filter(!grepl('^\\.', file),
           ! file %in% to_rm) %>%
    distinct(Time, file, .keep_all=TRUE) 
  return(df)
}

df_log_now_perc_plot = function(df, input){
  df = df_log_format(df, input)
  
  # plotting
  p = df %>%
    dplyr::select(file, pcent) %>%
    rename('Perc_Used' = pcent) %>%
    mutate(file = reorder(file, Perc_Used)) %>%
    ggplot(aes(file, Perc_Used, fill=Perc_Used)) + 
    geom_bar(stat='identity') +
    scale_y_continuous(limits=c(0, 100)) +
    scale_fill_continuous(low='black', high='red') +
    coord_flip() +
    labs(x='/ebio/abt3_projects/', y='% size used') +
    theme_bw() +
    theme(
      legend.position = 'none'
    )
  return(p)
}

df_log_now_iperc_plot = function(df, input){
  df = df_log_format(df, input)
  
  # plotting
  p = df %>%
    dplyr::select(file, ipcent) %>%
    rename('Perc_Used' = ipcent) %>%
    mutate(file = reorder(file, Perc_Used)) %>%
    ggplot(aes(file, Perc_Used, fill=Perc_Used)) + 
    geom_bar(stat='identity') +
    scale_y_continuous(limits=c(0, 100)) +
    scale_fill_continuous(low='black', high='red') +
    coord_flip() +
    labs(x='/ebio/abt3_projects/', y='% inodes used') +
    theme_bw() +
    theme(
      legend.position = 'none'
    )
  return(p)
}

df_log_now_size_plot = function(df, input){
  df = df_log_format(df, input)
  
  Mt = c('Avail. (Tb)', 'Used (Tb)')
  RN = data.frame(old_name = c('avail', 'used'),
                  Metric = factor(Mt, levels=Mt))
  
  # plotting
  p = df %>%
    dplyr::select(file, avail, used, pcent) %>%
    gather(old_name, Value, -file, -pcent) %>%
    inner_join(RN, 'old_name') %>%
    mutate(file = reorder(file, pcent)) %>%
    ggplot(aes(file, Value, fill=Metric)) + 
    geom_bar(stat='identity') +
    scale_fill_manual(values=c('blue', 'red')) +
    coord_flip() +
    labs(y='Available [blue] & Used [red]  (Tb)') +
    theme_bw() +
    theme(
      legend.position = 'none',
      axis.title.y = element_blank(),
      axis.text.y = element_blank()
    )
  return(p)
}

df_log_now_inodes_plot = function(df, input){
  df = df_log_format(df, input)
  
  Mt = c('Available', 'Used')
  RN = data.frame(old_name = c('iavail', 'iused'),
                  Metric = factor(Mt, levels=Mt))
  
  # plotting
  p = df %>%
    dplyr::select(file, iavail, iused, ipcent) %>%
    gather(old_name, Value, -file, -ipcent) %>%
    inner_join(RN, 'old_name') %>%
    mutate(file = reorder(file, ipcent)) %>%
    ggplot(aes(file, Value, fill=Metric)) + 
    geom_bar(stat='identity') +
    scale_fill_manual(values=c('blue', 'red')) +
    coord_flip() +
    labs(y='Available [blue] & Used [red]  (mil. inodes)') +
    theme_bw() +
    theme(
      legend.position = 'none',
      axis.title.y = element_blank(),
      axis.text.y = element_blank()
    )
  return(p)
}


#' calling df 
sh_df = function(){
  # call df and parse
  O='--output=source,fstype,itotal,iused,iavail,ipcent,size,used,avail,pcent,file,target'
  P = '/ebio/abt3_projects/*'
  ret = system2('df', args=c(O, P), stdout=TRUE)
  ret = lapply(as.list(ret[2:length(ret)]), function(x) strsplit(x, ' +')[[1]])
  ret = as.data.frame(do.call(rbind, ret))
  colnames(ret) = c('Filesystem','Type','Inodes','IUsed','IFree',
                    'IUsePerc','1K_blocks','Used','Avail','UsePerc',
                    'File','Mounted_on')
  return(ret)
}

#' getting processes via `ps -aux`
sh_ps = function(){
  args = c('--no-headers') #, '-auxo "uname,ppid,pid,etime,%cpu,%mem"')
  ret = system2('ps', args, stdout=TRUE)
  print(ret)
}
sh_ps()




