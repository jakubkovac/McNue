hasic <- function(){
  url <- "https://www.zomato.com/sk/bratislava/piv%C3%A1re%C5%88-u-hasi%C4%8Da-star%C3%A9-mesto-bratislava-i/denn%C3%A9-menu"
  download.file(url, destfile = "scrapedpage.html", quiet=TRUE)
  raw <- read_html("scrapedpage.html")
  jedlo <-
    raw %>% 
    html_nodes(".tmi-groups") %>% 
    html_text()
}
> 