rtvs <- function(){
  f <- file.info("rtvs.xlsx")
  f <- as.Date(f$mtime)
  t <- Sys.Date()
  if(lubridate::isoweek(t) %in% c(lubridate::isoweek(f), lubridate::isoweek(f) +1)){
    stop("RTVS menu is probably old.")
  }
  menu <- readxl::read_excel("rtvs.xlsx")
  colnames(menu) <- paste0("col_", 1:ncol(menu))
  menu <- bind_rows(menu[1,], filter(menu, !is.na(col_1))) %>%
    distinct() %>% 
    select(all_of(paste0("col_", seq(1,ncol(menu),by=2))))
  
  t <- format(lubridate::today(), "%A")
  days_of_the_week <- c("Monday", "Tuesday", "Wednesday", "Thursday","Friday", "Saturday","Sunday")
  t <- which(t == days_of_the_week)
  
  menu <- menu[2:nrow(menu),] %>% pull(t+1) %>% na.omit()

  return(c("RTVS",menu))
}
