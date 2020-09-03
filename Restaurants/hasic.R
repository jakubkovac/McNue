hasic <- function(){
  #url <- "https://www.zomato.com/sk/bratislava/piv%C3%A1re%C5%88-u-hasi%C4%8Da-star%C3%A9-mesto-bratislava-i/denn%C3%A9-menu"
  #download.file(url, destfile = "scrapedpage.html", quiet=TRUE)
  #we need to download the html once a week
  #how to do this automatically
  raw <- read_html("hasic.html")
  jedlo <- 
    raw %>% 
    html_nodes(".sc-giPFmd") %>% 
    html_children() %>%
    html_children() %>%
    html_text() %>%
    str_trim()
  days_of_the_week <- c("Monday", "Tuesday", "Wednesday", "Thursday","Friday", "Saturday","Sunday")
  dni <- c("Pondelok", "Utorok", "Streda", "Stvrtok", "Piatok", "Sobota", "Nedela")
  today <- format(Sys.Date(), "%A")
  source("slovak_language_destroyer.R",encoding="utf-8")
  source("benson_string_destroyer.R")
  jedlo <- slovak_language_destroyer(jedlo)
  match_day <- paste0(dni[which(today == days_of_the_week)],
                      ", ",
                      unlist(str_split(format(Sys.Date(), "%D"),"/"))[2])
  dnes <- which(str_detect(jedlo,match_day))
  jedlo <- jedlo[(dnes+1):(dnes+4)]
  jedlo <- benson_string_destroyer(jedlo)
  jedlo <- c(str_sub(jedlo[1],3),str_sub(jedlo[2:4],5))
  c("U Hasica",jedlo,"")
}
