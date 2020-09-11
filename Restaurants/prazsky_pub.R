prazsky_pub <- function(){
  url_pub <- "https://prazskypub.sk/denne-menu/"
  download.file(url_pub, destfile = "scrapedpage.html", quiet=TRUE)
  raw <- read_html("scrapedpage.html")
  jedlo <- 
    raw %>% 
    html_nodes("div.row:nth-child(2)") %>% 
    html_children() %>%
    html_children() %>%
    html_children() %>%
    html_text()
  
  jedlo <-
    jedlo %>% 
    str_split("\\n") %>%
    lapply(function(x) x[[1]]) %>% 
    unlist() %>% str_remove_all("\\r|\\t")
  day_index <- which(str_detect(jedlo,"Pondelok|Utorok|Streda|Štvrtok|Piatok"))
  days_of_the_week <- c("Monday", "Tuesday", "Wednesday", "Thursday","Friday", "Saturday","Sunday")
  today <- format(Sys.Date(), "%A")
  today_i <- day_index[which(days_of_the_week %in% today)]
  jedlo <- jedlo[c(today_i+2,
                   (today_i + 4):(today_i + 6))]
  return(c("Prazsky pub",jedlo))
}
