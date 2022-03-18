spilka <- function(){
  url <- "https://www.spilkarestaurant.sk/"
  download.file(url, destfile = "scrapedpage.html", quiet=TRUE)
  raw <- read_html("scrapedpage.html")
  pdf_link <- html_nodes(raw, ".et_pb_button_4") %>% html_attr("href")
  pdf_link <- str_replace(pdf_link,"https:", "http:")
  
  if(file.exists("spilka_menu.pdf")){
    f <- file.info("spilka_menu.pdf")
    f <- as.Date(f$mtime)
    t <- Sys.Date()
    if(lubridate::isoweek(t) != lubridate::isoweek(f)){
      message("PDF menu is not from this week. Downloading spilka menu.")
      download.file(pdf_link, destfile = "spilka_menu.pdf", quiet=TRUE, mode = "wb")
    }
  }else{
    download.file(pdf_link, destfile = "spilka_menu.pdf", quiet=TRUE, mode = "wb")
  }
  
  bitmap <- pdftools::pdf_render_page("spilka_menu.pdf", page = 1)
  png::writePNG(bitmap, "spilka_menu.png")
  
  #OCR
  #tesseract::tesseract_download("slk")
  im <- magick::image_ocr(magick::image_read("spilka_menu.png"),
                          language = "slk")
  unlink("spilka_menu.png")
  jedlo <- str_split(im, "PONDELOK|UTOROK|STREDA|ŠTVRTOK|PIATOK") %>% unlist()
  jedlo <- jedlo[-1]
  jedlo <- str_split(jedlo, "\n")
  
  days_of_the_week <- c("Monday", "Tuesday", "Wednesday", "Thursday","Friday", "Saturday","Sunday")
  #dni <- str_to_upper(c("PONDELOK", "UTOROK", "STREDA", "ŠTVRTOK", "PIATOK", "SOBOTA", "NEDELA"))
  today <- format(Sys.Date(), "%A")
  den <- which(today == days_of_the_week)
  jedlo <- jedlo[[den]]
  jedlo <- jedlo[nchar(jedlo) > 6]
  jedlo <- jedlo[!str_detect(jedlo,"Dobrú")]
  jedlo <- jedlo[1:3]
  return(c("Spilka", jedlo))
}
