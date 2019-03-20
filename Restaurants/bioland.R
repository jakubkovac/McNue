bioland <- function(){
  url_bioland <- "https://www.bioland.sk/restauracia-bratislava/"
  download.file(url_bioland, destfile = "scrapedpage.html", quiet=TRUE)
  raw <- read_html("scrapedpage.html")
  jedlo8 <- raw %>% 
    html_nodes(".td_content_body") %>% 
    html_children() %>%
    html_text()
  Sys.Date()
  strftime(Sys.Date(), '%A')
  jedlo8 <- jedlo8[13:56]
  jedlo8 <- jedlo8[str_length(jedlo8) >0]
  day_index <- seq(1,25,by = 5)
  day_menu <- jedlo8[day_index] %>% str_sub(start = -6)
  today <- paste0(format(Sys.Date(), "%d  %m") %>% str_replace("  ","."),".")
  today_i <- day_index[which(today == day_menu)]
  jedlo8 <- jedlo8[(today_i+1):(today_i + 4)]
  return(c("Bioland",jedlo8,""))
}