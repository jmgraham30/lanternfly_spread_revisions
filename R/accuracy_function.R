compute_accuracy <- function(infest_status,mod_prop){
  accuracy_val <- ifelse(infest_status == 1,100 * mod_prop,100 * (1 - mod_prop))
}