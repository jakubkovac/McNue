fix_rtvs_rows <- function(x){
  x <- x[!is.na(x)]
  x <- paste0(x, collapse = " ")
  x <- x %>% str_split("[1-9]") %>% unlist()
  return(x[str_detect(x,"[a-zA-Z]{4,}")])
}

rtvs <- function(){
  reference_day <- lubridate::ymd("2019-09-23")
  #im not going to bother with doing the correct file name loading right now
  #lets assume it's alway
  days_of_the_week <- c("Monday", "Tuesday", "Wednesday", "Thursday","Friday", "Saturday","Sunday")
  
  today_full <- Sys.Date()
  today <- format(today_full, "%A")
  
  sheets <- readxl::excel_sheets("rtvs.xls")
  sheets <- sheets %>% str_split("\\.") 
  sheets <- map(sheets, ~str_remove_all(.x,"-")) 
  sheets <- map(sheets, ~if(length(.x) == 4) rev(.x[c(1,3,4,2,3,4)]) else rev(.x[c(1,2,5,3,4,5)]))
  
  int1 <- lubridate::interval( ymd(paste0(sheets[[1]][4:6], collapse = "-")), ymd(paste0(sheets[[1]][1:3], collapse = "-")))
  int2 <- lubridate::interval( ymd(paste0(sheets[[2]][4:6], collapse = "-")), ymd(paste0(sheets[[2]][1:3], collapse = "-")))
  which_sheet <- which(today_full %within% c(int1, int2))
  
  raw <- readxl::read_excel("rtvs.xls", col_names = FALSE, sheet = which_sheet)
  raw <- raw[-(1:4), seq(2,10,by =2)]
  tyzdenne_jedlo <- map(raw, fix_rtvs_rows)
  
  
  jedlo <- str_trim(tyzdenne_jedlo[[which(today == days_of_the_week)]])
  return(c("RTVS",jedlo))
}
