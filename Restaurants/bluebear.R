bluebear <- function(){
  url_bluebear <- "http://blue-bear.sk/denne-menu-blue-bear/"
  Sys.Date()
  strftime(Sys.Date(), '%A')
  download.file(url_bluebear, destfile = "scrapedpage.html", quiet=TRUE)
  raw <- read_html("scrapedpage.html")
  jedlo3 <- raw %>%
    html_nodes(".menux > div:nth-child(1)") %>%
    html_children() %>%
    html_text()
  day_index <- c(1,7,13,19,25)
  day_menu <- jedlo3[day_index] %>% dmy
  today_i <- day_index[which(Sys.Date() == day_menu)]
  jedlo3 <- jedlo3[(today_i+1):(today_i + 5)]
  jedlo3 <- jedlo3[-2]
  jedlo3[2:4] <- 
    jedlo3[2:4] %>% 
    str_extract("g(.*)obsahuje") %>% 
    str_sub(start = 2, end = -9) %>%
    str_trim()
  jedlo3[1] <- 
    jedlo3[1] %>% 
    str_extract("ml(.*)obsahuje") %>%
    str_sub(start = 3, end = -9) %>%
    str_trim()
  jedlo3 <- str_replace_all(jedlo3, "([\n\t])", "")
  return(c("BlueBear",jedlo3,""))
}
