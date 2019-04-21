bluebear <- function(){
  url_bluebear <- "http://blue-bear.sk/denne-menu-blue-bear/"
  Sys.Date()
  strftime(Sys.Date(), '%A')
  download.file(url_bluebear, destfile = "scrapedpage.html", quiet=TRUE)
  raw <- read_html("scrapedpage.html")
  jedlo <- raw %>%
    html_nodes(".menux > div:nth-child(1)") %>%
    html_children() %>%
    html_text()
  jedlo <- str_trim(jedlo)
  jedlo <- jedlo[str_length(jedlo) >0]
  tyzden <- jedlo[length(jedlo) -1]
  day_index <- c(1,7,13,20,26)
  days_of_the_week <- c("Monday", "Tuesday", "Wednesday", "Thursday","Friday", "Saturday","Sunday")
  today <- format(Sys.Date(), "%A")
  today_i <- day_index[which(days_of_the_week %in% today)]
  jedlo <- jedlo[(today_i+1):(today_i + 6)]
  if(today_i == 15){
    jedlo <- jedlo[-5]
  } else jedlo <- jedlo[-6]
  jedlo <- jedlo[-2]
  jedlo[2:4] <- 
    jedlo[2:4] %>% 
    str_extract("g(.*)obsahuje") %>% 
    str_sub(start = 2, end = -9) %>%
    str_trim()
  jedlo[1] <- 
    jedlo[1] %>% 
    str_extract("ml(.*)obsahuje") %>%
    str_sub(start = 3, end = -9) %>%
    str_trim()
  jedlo <- str_replace_all(jedlo, "([\n\t])", "")
  jedlo[is.na(jedlo)] <- ""
  return(c("BlueBear",jedlo,paste("TYZDENNE",tyzden)))
}
