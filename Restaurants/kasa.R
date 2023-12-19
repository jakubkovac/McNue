kasa <- function(sme = TRUE){
  if(!sme){
    url_kasa <- "https://www.kasabar.sk/?page_id=415"
  #download.file(url_kasa, destfile = "scrapedpage.html", quiet=TRUE)
  raw <- read_html(url_kasa)
  # jedlo <- raw %>% 
  #   html_nodes(".dnesne_menu .jedlo_polozka .left") %>% 
  #   html_text()
  # jedlo <- str_trim(jedlo)
  # jedlo <- str_replace_all(jedlo, "([\n\t])", "")
  jedlo <-
    raw %>%
    html_nodes("#denne-menu > article:nth-child(13)") %>% html_children() %>% html_text()
  #day_index <- seq(1,25,by = 5)
  day_index <- c(1,4,9,14,19)
  days_of_the_week <- c("Monday", "Tuesday", "Wednesday", "Thursday","Friday", "Saturday","Sunday")
  today <- format(Sys.Date(), "%A")
  today_i <- day_index[which(days_of_the_week %in% today)]
  jedlo <- jedlo[c((today_i+1),(today_i + 3),(today_i + 4))]
  jedlo[1] <- jedlo[1] %>% str_sub(10)
  jedlo[2] <- jedlo[2] %>% str_sub(47)
  jedlo[3] <- jedlo[3] %>% str_sub(14)
  }else{
    url_kasa <- "https://restauracie.sme.sk/restauracia/kasa-3_9938-stare-mesto_2949/denne-menu"
    #download.file(url_kasa, destfile = "scrapedpage.html", quiet=TRUE)
    raw <- read_html(url_kasa)
    jedlo <- raw %>%
      html_nodes(".dnesne_menu .jedlo_polozka .left") %>%
      html_text() %>%
      str_trim()
  }
  
  return(c("Kasa 3",jedlo[1:3]))
}
