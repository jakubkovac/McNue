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
  sheets <- map(sheets, ~.x[nchar(.x)>0])
  
  rtvs_sheets <- function(x){
    y <- case_when(length(x) == 4 ~ rev(c(x[c(1,3)],year(today_full), x[c(2,3)],year(today_full))),
                  #length(x) == 4 ~ rev(c(x[c(1,2)],year(today_full), x[c(3,4)],year(today_full))),
                   length(x) == 5 ~ rev(x[c(1,2,5,3,4,5)]),
                   length(x) == 3 ~ rev(c(x[c(1,3)],year(today_full), x[c(2,3)],year(today_full))))
    return(y)
  }
  
  
  sheets <- map(sheets, rtvs_sheets)
  
  time_ints <- sheets %>% 
    map(function(x) lubridate::interval(ymd(paste0(x[4:6], collapse = "-")),
                                        ymd(paste0(x[1:3], collapse = "-"))))
  
  which_sheet <- time_ints %>%
    map(function(x) today_full %within% x) %>% 
    unlist()
  
  which_sheet <- which(which_sheet)
  
  # int1 <- lubridate::interval( ymd(paste0(sheets[[1]][4:6], collapse = "-")), ymd(paste0(sheets[[1]][1:3], collapse = "-")))
  # int2 <- NULL
  # if(length(sheets) > 1){
  #   int2 <- lubridate::interval( ymd(paste0(sheets[[2]][4:6], collapse = "-")), ymd(paste0(sheets[[2]][1:3], collapse = "-")))
  #   which_sheet <- which(today_full %within% c(int1, int2))
  # }else{
  #   which_sheet<- which(today_full %within% int1)
  # }
  
  raw <- readxl::read_excel("rtvs.xls", col_names = FALSE, sheet = which_sheet)
  raw <- raw[-(1:4), seq(2,10,by =2)]
  tyzdenne_jedlo <- map(raw, fix_rtvs_rows)
  
  
  jedlo <- str_trim(tyzdenne_jedlo[[which(today == days_of_the_week)]])
  return(c("RTVS",jedlo))
}
