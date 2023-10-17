fncWeights <- function(working_max = working_max, intensity_pct = intensity_pct){ 
  
  out = (working_max*intensity_pct)*(c(1,.85,.65))
  
  return(glue::glue('warmup 1 = {ceiling(out[3])} 
                     warmup 2 = {ceiling(out[2])} 
                    
                     working sets = {round(out[1],0)}'))}


fncRunTimes <- function(times = times){
  
  require(tidyverse)
  
  data = tibble(
    run_times = times
    )
  
  run_data = data |> 
    mutate(run_times = as.POSIXct(run_times, format = '%M:%S')
           ,minutes = lubridate::minute(run_times)
           ,seconds = lubridate::second(run_times)
           ,tot_seconds = (minutes*60)+seconds
           ,dec_minutes = tot_seconds/60 )
  
  avg = mean(run_data$dec_minutes)
  sd = sd(run_data$dec_minutes)
  
  hi95  = avg + (1.96*sd)
  lo95 = avg - (1.96*sd)
  
  avg_out = glue::glue('{floor(avg)}:{round(((avg%%1)*60),1)}')
  hi95_out = glue::glue('{floor(hi95)}:{round(((hi95%%1)*60),1)}')
  lo95_out = glue::glue('{floor(lo95)}:{round(((lo95%%1)*60),1)}')
  
  output = glue::glue('Avg = {avg_out}
                       p95 Range = {lo95_out} - {hi95_out}')
  
  return(print(output))
  
}
  
