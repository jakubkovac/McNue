alzbetka <- function(sme = FALSE){
  if(sme){
  url <- "https://restauracie.sme.sk/restauracia/mestsky-pivovar-alzbetka_9975-stare-mesto_2949/denne-menu"
  download.file(url, destfile = "scrapedpage.html", quiet=TRUE)
  raw <- read_html("scrapedpage.html")
  jedlo <- raw %>%
    html_nodes(".dnesne_menu .jedlo_polozka .left") %>% html_text()
  jedlo <- jedlo %>% 
    str_trim() %>%
    str_remove_all("FIT") %>%
    str_remove_all("P1") %>%
    str_remove_all("P2") %>%
    str_remove_all("M1") %>%
    str_remove_all("M2") %>%
    str_trim()
  }else{
    url <- "http://www.pivovaralzbetka.sk/denne-menu/"
    download.file(url, destfile = "scrapedpage.html", quiet=TRUE)
    raw <- read_html("scrapedpage.html")
    jedlo <-
      raw %>% html_nodes(".et_pb_section") %>% 
      html_children %>% html_children %>% 
      html_children %>% html_children %>% 
      html_children %>% html_text()
    days_of_the_week <- c("Monday", "Tuesday", "Wednesday", "Thursday","Friday", "Saturday","Sunday")
    dni <- str_to_upper(c("pondelok", "utorok", "streda", "stvrtok", "piatok", "sobota", "nedela"))
    today <- format(Sys.Date(), "%A")
    source("slovak_language_destroyer.R",encoding="utf-8")

    jedlo <- slovak_language_destroyer(jedlo)
    
    jedlo <- jedlo[which(str_detect(jedlo,dni[which(today == days_of_the_week)])) +1]
    jedlo <- unlist(str_split(jedlo,"\\\n"))
    jedlo <- jedlo %>% 
      str_trim() %>%
      str_remove_all("FIT") %>%
      str_remove_all("P1") %>%
      str_remove_all("P2") %>%
      str_remove_all("M1") %>%
      str_remove_all("M2") %>%
      str_trim()
  }
  return(c("Alzbetka", jedlo, "", ""))
}
