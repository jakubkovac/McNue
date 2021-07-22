rtvs <- function(){
  unlink("tmp", recursive = FALSE)
  extract_Textbox_From_Excel <- function(excel_File_Name)
  {
    library(stringr)
    library(xml2)
    zip_file <- str_replace(excel_File_Name, ".xlsx", ".zip")
    dir.create("tmp")
    file.copy(excel_File_Name, paste0("tmp/", zip_file))
    
    unzip(zipfile = paste0("tmp/", zip_file), exdir = "tmp")
    
    drawing_files <- list.files("tmp/xl/drawings", pattern = "\\.xml", full.names = T)
    
    xml_Text <- read_xml(drawing_files)
    text <- xml_text(xml_Text, trim = TRUE)
    text <- str_split(text, "[0-9]") %>% unlist()
    text <- text[nchar(text)>1]
    text <- text[!str_detect(text, "obilniny|zeler|vajcia|mlieko")]
    return(text)
  }
  menu <- extract_Textbox_From_Excel("rtvs.xlsx")
  #unlink("tmp", recursive = FALSE)
  t <- format(lubridate::today(), "%A")
  days_of_the_week <- c("Monday", "Tuesday", "Wednesday", "Thursday","Friday", "Saturday","Sunday")
  t <- which(t == days_of_the_week)
  menu <- menu[seq(t, length(menu), 5)]
  menu <- rev(menu)
  return(c("RTVS",menu))
}
