downsample_using_mean <- function(df, col_names) {
  per_sec_df <- NULL
  if(nrow(df) <= 0) {
    return(NULL)
  }
  
  max_time <- floor(tail(df$Time, 1))
  if(!is.numeric(max_time) || length(max_time) == 0) {
    return(NULL)
  }
  
  print(max_time)
  
  for(cur_time in c(0 : max_time)) {   #Use minTime instead of 0
    one_sec_data = df[df$Time >= cur_time & df$Time < cur_time+1,]
    if(nrow(one_sec_data) > 0) {
      mean_values = colMeans(one_sec_data, na.rm = TRUE)
      data = data.frame("Time" = cur_time)
      for (column in col_names) {
        if(is.nan(mean_values[column])) {
          ################
          # val = NA/''
          ################
          val = ''
        } else {
          val = mean_values[column]
        }
        d = data.frame(column = val, row.names = NULL)
        colnames(d) = c(column)
        data = cbind(data, d)
      }
      per_sec_df = rbind(per_sec_df, data)
    }
    else {
      data = data.frame("Time" = cur_time)
      for (column in col_names) {
        ################
        # val = NA/''
        ################
        val = ''
        d = data.frame(column = val, row.names = NULL)
        colnames(d) = c(column)
        data = cbind(data, d)
      }
      per_sec_df = rbind(per_sec_df, data) 
    }
    # data = NULL
  }
  return(per_sec_df)
}