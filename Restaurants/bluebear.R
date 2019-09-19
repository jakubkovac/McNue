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
  source("slovak_language_destroyer.R",encoding="utf-8")
  tyzden <- jedlo[which(str_detect(slovak_language_destroyer(jedlo),"Tyzdenna ponuka|Business menu")) + 1]
  tyzden <- tyzden[1]
  #day_index <- c(1,7,13,20,26)
  day_index <- which(str_detect(jedlo,"Pondelok|Utorok|Streda|Štvrtok|Piatok"))
  days_of_the_week <- c("Monday", "Tuesday", "Wednesday", "Thursday","Friday", "Saturday","Sunday")
  today <- format(Sys.Date(), "%A")
  today_i <- day_index[which(days_of_the_week %in% today)]
  message(today_i)
  jedlo <- jedlo[(today_i+1):(today_i + 5)]

  jedlo <- jedlo[-2]
  jedlo[2:4] <- 
    jedlo[2:4] %>% 
    str_replace_all("obsahuje","") %>%
    str_sub(start = 12) %>%
    str_trim()
  jedlo[1] <- 
    jedlo[1] %>% 
    str_extract("ml(.*)obsahuje") %>%
    str_sub(start = 3, end = -9) %>%
    str_trim()
  jedlo <- str_remove_all(jedlo, "([\n\t])") %>% str_remove_all("obsahuje") %>% str_remove_all("EUR")
  tyzden <- str_remove_all(tyzden,"obsahuje") %>% str_remove_all("EUR")
  jedlo[is.na(jedlo)] <- ""
  jedlo[5] <- paste("TYZDENNE",tyzden)
  return(c("BlueBear",jedlo))
}
