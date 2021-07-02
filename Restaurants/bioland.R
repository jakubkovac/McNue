bioland <- function(){
  url_bioland <- "https://www.bioland.sk/restauracia-bratislava/"
  download.file(url_bioland, destfile = "scrapedpage.html", quiet=TRUE)
  raw <- read_html("scrapedpage.html")
  jedlo <-
    raw %>% html_elements("[class='module_cont']") %>% html_text()
  jedlo <- jedlo[which(str_detect(jedlo, "Polievka"))[[2]]]
  jedlo <- str_split(jedlo, "Pondelok|Utorok|Streda|Štvrtok|Piatok") %>% unlist()
  jedlo <- jedlo[-1]
  jedlo <- str_replace_all(jedlo, "Polievka", " Polievka") %>% str_trim()
  jedlo <- str_trim(jedlo) %>% .[. != ""]
  today <- format(Sys.Date(), "%d.%m.")

  day_index <- which(str_detect(jedlo,today))
  jedlo <- jedlo[day_index]
  jedlo <- unlist(str_split(jedlo,"Hl. jedlo|Dezert"))
  jedlo <- str_remove(jedlo, "Polievka")
  return(c("Bioland",jedlo))
}
