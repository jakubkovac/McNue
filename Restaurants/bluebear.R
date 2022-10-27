bluebear <- function(){
  url_bluebear <- "http://blue-bear.sk/denne-menu-blue-bear/"
  download.file(url_bluebear, destfile = "scrapedpage.html", quiet=TRUE)
  raw <- read_html("scrapedpage.html")
  jedlo <- raw %>%
    html_nodes(".menux > div:nth-child(1)") %>%
    html_children() %>%
    html_text()
  jedlo <- str_trim(jedlo)
  jedlo <- jedlo[str_length(jedlo) >0]
  
  tyzden <- jedlo[which(str_detect(slovak_language_destroyer(jedlo),"Tyzdenna ponuka|Business menu")) + 3]
  tyzden <- tyzden[1]
  #day_index <- c(1,7,13,20,26)
  day_index <- which(str_detect(jedlo,"Pondelok|Utorok|Streda|Štvrtok|Piatok"))
  days_of_the_week <- c("Monday", "Tuesday", "Wednesday", "Thursday","Friday", "Saturday","Sunday")
  today <- format(Sys.Date(), "%A")
  today_i <- day_index[which(days_of_the_week %in% today)]
  
  jedlo <- jedlo[(today_i+2):(today_i+9)]
  jedlo <- jedlo[!str_detect(jedlo, "Menu|Retro")]
  jedlo <- 
    str_squish(jedlo) %>%
    str_remove_all("Menu [A-Z]|Retro XXL|obsahuje|Polievka")
  return(c("BlueBear",jedlo))
}
