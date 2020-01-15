#' Reading in disk-usage log
disk_usage_log = function(file, input){
  x = fread(file, sep='\t', header=FALSE, fill=TRUE) %>%
    as.data.frame(x) %>%
    mutate(V4 = V4 / 1000)
  colnames(x) = c('Category', 'Time', 'Directory', 'Terabytes', 'Percent')
  x$Time = as.POSIXct(strptime(x$Time, "%Y-%m-%d %H:%M:%S"))
  return(x)
}

du_now_plot = function(df, keep_cat, dir_filter=NULL){
  if (!is.null(dir_filter)){
    df = df %>%
      filter(grepl(dir_filter, Directory),
             Category == keep_cat) 
  }
  if(nrow(df) < 1){
    return(NULL)
  }
  levs = c('Million_files', 'Terabytes', 'Percent')
  p = df %>%
    filter(max(Time) - Time < 5) %>%
    mutate(Percent = Percent %>% as.Num,
           Directory = Directory %>% as.character,
           Directory = reorder(Directory, Percent)) %>%
    gather(Unit, Usage, -Category, -Time, -Directory) %>%
    mutate(Unit = factor(Unit, levels=levs)) %>%
    ggplot(aes(Directory, Usage)) +
    geom_bar(stat='identity') +
    coord_flip() +
    facet_grid(. ~ Unit, scales='free_x') +
    theme_bw() +
    theme(
      axis.title.y = element_blank()
    )
  return(p)
}

inodes_log = function(file, input){
  x = fread(file, sep='\t', header=FALSE, fill=TRUE) %>%
    as.data.frame(x) %>%
    mutate(V4 = V4 / 1000)
  colnames(x) = c('Category', 'Time', 'Directory', 'Million_files', 'Percent')
  x$Time = as.POSIXct(strptime(x$Time, "%Y-%m-%d %H:%M:%S"))
  return(x)
}

