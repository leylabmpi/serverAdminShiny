node_FS_log = function(file, input){
  x = read.delim(file, sep=' ', header=FALSE) 
  colnames(x) = c('node', 'file_system', 'status')
  x$node = gsub(':$', '', x$node)
  return(x)
}


node_FS_plot = function(df){
  p = ggplot(df, aes(node, file_system, fill=status)) +
    geom_tile() +
    theme_minimal() +
    labs(y = 'File System') +
    theme(
      axis.text.x = element_text(angle=55, hjust=1)
    )
  return(p)
}