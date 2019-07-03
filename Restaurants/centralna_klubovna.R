centralna_klubovna <- function(){
  url <- "https://www.nasaklubovna.sk/sk/menu/centralna/tyzdenne-menu/"
  download.file(url, destfile = "scrapedpage.html", quiet=TRUE)
  raw <- read_html("scrapedpage.html")
  jedlo <- 
    raw %>% 
    html_nodes(".daily-menu-wrapper > table:nth-child(1)") %>% 
    html_children() %>%
    html_text()
  days_of_the_week <- c("Monday", "Tuesday", "Wednesday", "Thursday","Friday", "Saturday","Sunday")
  dni <- c("Pondelok", "Utorok", "Streda", "Stvrtok", "Piatok", "Sobota", "Nedela")
  today <- format(Sys.Date(), "%A")
  source("slovak_language_destroyer.R",encoding="utf-8")
  source("benson_string_destroyer.R")
  jedlo <- slovak_language_destroyer(jedlo)
  match_day <- paste0(dni[which(today == days_of_the_week)])#,
                     # " ",
                     # unlist(str_split(format(Sys.Date(), "%D"),"/"))[2])
  dnes <- which(str_detect(jedlo,match_day))
  jedlo <- jedlo[(dnes+1):(dnes+4)]
  jedlo <- 
    jedlo %>%
    str_remove_all("Menu [a-zA-Z] s polievkou \\|") %>%
    str_remove_all("Polievka \\|") %>%
    str_remove_all("obsahuje") %>%
    str_trim()
  return(c("Centr. klub.",jedlo,""))
}
