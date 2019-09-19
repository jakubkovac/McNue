bioland <- function(){
  url_bioland <- "https://www.bioland.sk/restauracia-bratislava/"
  download.file(url_bioland, destfile = "scrapedpage.html", quiet=TRUE)
  raw <- read_html("scrapedpage.html")
  jedlo <-
    raw %>% 
    html_nodes(".td_content_body") %>% 
    html_children() %>%
    html_text()
  
  jedlo <- str_trim(jedlo)
  jedlo <- jedlo[str_length(jedlo) >0]
  jedlo[6] <- "Pondelok 09.09"
  day_index <- which(str_detect(jedlo,"Pondelok|Utorok|Streda|Štvrtok|Piatok"))
  days_of_the_week <- c("Monday", "Tuesday", "Wednesday", "Thursday","Friday", "Saturday","Sunday")
  today <- format(Sys.Date(), "%A")
  today_i <- day_index[which(days_of_the_week %in% today)]
  
  jedlo <- jedlo[(today_i+1):(today_i + 4)]
  return(c("Bioland",jedlo,""))
}
