milton <- function(){
  milton_url <- "http://www.restauraciamilton.sk/"
  download.file(milton_url, destfile = "scrapedpage.html", quiet=TRUE)
  raw <- read_html("scrapedpage.html")
  day_diff <- as.numeric(ymd(Sys.Date()) - ymd("2019-12-09")) #thursday
  daily_menu_number <- 7793 + day_diff #- day_diff %/% 7
  jedlo <-
    raw %>%
    html_nodes(paste0("#dailymenu-", daily_menu_number ," > div:nth-child(1)")) %>%
    html_text()
  jedlo <- jedlo %>% str_split("\\\n|\\/") %>% unlist() %>% str_squish()
  jedlo <- jedlo[jedlo != ""]
  jedlo <- jedlo[!str_detect(jedlo, "€|A:")]
  jedlo <- jedlo[1:(which(str_detect(jedlo, "Týždenné"))-1)]
  return(c("Milton", jedlo[1:5]))
}
