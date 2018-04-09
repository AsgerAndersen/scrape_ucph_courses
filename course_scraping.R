library(tidyverse)
library(rvest)

deltagere <- Vectorize(function(t, b) {
  if(t=='Forelæsning') {
    'Alle'
  }
  else {
    b
    #str_extract(b, 'Hold .')
  }
})

scrape <- function(url) {
  webpage <- read_html(url) #Get raw html
  title <- unlist(str_split(html_text(html_node(webpage, '.header-5-0-5')), '[-;]')) #Extract course name, institute and semester
  timetables <- html_table(html_nodes(webpage, '.spreadsheet'), header=T) #Extract the html tables as dataframes
  
  #Collect all the weekly dataframes to one dataframe. Use the dates column to keep track of the observations.
  distinct( #Remove duplicates
    bind_rows( #Collect all the weekly dataframes to one dataframe
      map(timetables, ~unnest( #One observation for each date
          mutate(., Dato=str_split(Dato, ';'))) #Split the dates 
        )
      )
    ) %>% #Make new columns
    mutate(Deltagere = deltagere(Type,Beskrivelse), #Who should be participating in the class as a column
           Institut = title[1],
           Semester = title[2],
           Kursusnavn = title[3]) %>% 
    select(Institut, Semester, Kursusnavn, Type, Deltagere, Dato, Start, Slut, Lokale) #Drop unnecessary columns
}

url <- 'https://skema.ku.dk/tt/tt.asp?SDB=KU1718&language=DK&folder=Reporting&style=textspreadsheet&type=module&idtype=id&id=71500&weeks=1-27&days=1-5&periods=5-52&width=0&height=0&template=SWSCUST2+module+textspreadsheetA'
test <- scrape(url)
