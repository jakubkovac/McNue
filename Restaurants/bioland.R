bioland <- function(){
  url_bioland <- "https://www.bioland.sk/restauracia-bratislava/"
  download.file(url_bioland, destfile = "scrapedpage.html", quiet=TRUE)
  raw <- read_html("scrapedpage.html")
  jedlo <- raw %>% 
    html_nodes(".td_content_body") %>% 
    html_children() %>%
    html_children() %>%
    html_text()
  
  jedlo <- jedlo[14:48]
  jedlo <- jedlo[str_length(jedlo) >0]
  day_index <- seq(1,20,by = 4)
  
  days_of_the_week <- c("Monday", "Tuesday", "Wednesday", "Thursday","Friday", "Saturday","Sunday")
  today <- format(Sys.Date(), "%A")
  today_i <- day_index[which(days_of_the_week %in% today)]

 
  
  jedlo <- jedlo[(today_i):(today_i + 3)]
  return(c("Bioland",jedlo,""))
}