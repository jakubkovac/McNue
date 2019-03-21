realstar <- function(){
  url <- "https://www.realstar.sk/denne-menu"
  download.file(url, destfile = "scrapedpage.html", quiet=TRUE)
  raw <- read_html("scrapedpage.html")
  day_index <- c(3,6,8,10,12)
  days_of_the_week <- c("Monday", "Tuesday", "Wednesday", "Thursday","Friday", "Saturday","Sunday")
  today <- format(Sys.Date(), "%A")
  today_i <- day_index[which(days_of_the_week %in% today)]
  
  jedlo <- raw %>%
    html_nodes(paste0("table.denne_menu:nth-child(",today_i,")")) %>%
    html_children() %>%
    html_children() %>%
    html_text() %>%
    str_trim()
  jedlo <- jedlo[c(1,3:5)]
  jedlo <- jedlo %>% str_replace_all("\r","") %>% str_replace_all("\n","")
  jedlo[2:4] <- jedlo[2:4] %>% str_sub(15)
  jedlo <- jedlo %>% str_replace_all("príloha","")
  jedlo[1] <- jedlo[1] %>% str_sub(11)
  return(c("U Majky",jedlo,""))
}
