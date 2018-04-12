library(magrittr)
library(tidyverse)
library(rvest)

setwd('~/my_files/arbejde/sodas/scrape_ucph_courses')

#---------------------------------------------------------------------------------------
#Scrape list of courses

sems <- c('B5-5F17', 'B5-5F18', 'E17', 'F18')
timetable_sem <- Vectorize(function(sem) {
  if (is.element(sem, sems[1:2])) {'S'}
  else if (sem==sems[3]) {'E'}
  else {'F'}
})

course_list <- read_csv('~/Downloads/course_list.csv', col_names = c('Modul', 'Institut', 'Id'))
course_list %<>%
  mutate(Institut = as.integer(str_sub(Institut,1,4)),
         Semester = str_remove(str_sub(Modul, 6),';.*')) %>%
  filter(is.element(Semester, sems)) %>% #Tjek for de andre institutter, at jeg også kun filterere irrelevante rækker fra her
  mutate(Timetable_sem = timetable_sem(Semester))

#---------------------------------------------------------------------------------------
#Function for scraping the timetable of a single course


scrape_course <- function(url) {

  webpage <- read_html(url) #Get raw html
  
  #Extract course name, institute and semester
  title <- webpage %>% 
    html_node('.header-5-0-5') %>% 
    html_text() %>% 
    str_split(';')
  
  title[[1]][1] %<>% str_split('-', n=2)
  title %<>% unlist()
  
  #Extract the html tables as dataframes
  timetables <- webpage %>% 
    html_nodes('.spreadsheet') %>%
    html_table(header=T)
  
  #Returns all the weekly dataframes collected as one dataframe
  map(timetables, format_timetable) %>% 
    bind_rows() %>%
    distinct() %>%
    mutate(Institut = title[1],
           Semester = title[2],
           Kursusnavn = title[3]) %>% 
    select(Institut, Semester, Kursusnavn, Type, Deltagere, Dato, Start, Slut, Lokale)
}

#Add error handling
scrape_course_err <- function(url) {
  tryCatch(scrape_course(url), error = function(e) {e})
}

#Helper functions
format_timetable <- function(tt) {
  tt %>% 
    mutate_all(to_character) %>%
    mutate(Deltagere = deltagere(Type, Beskrivelse)) %>% 
    mutate(Dato=str_split(Dato, ';')) %>% 
    unnest()
}

to_character <- Vectorize(function(x) {
  if (is.logical(x)) {
    str_sub(as.character(x), 1, 1)
  }
  else x
})

regex <- '((H|h)old|(E|e)x. (C|c)lass) {0,1}\\d+( {0,1}\\+ {0,1}\\d+)*'
deltagere <- Vectorize(function(t, b) {
  if(t=='Forelæsning') {
    'Alle'
  }
  else {
    d <- str_extract(b, regex) 
    if (is.na(d)) {b}
    else {d}
  }
})

#---------------------------------------------------------------------------------------
#Scrape timetables of all relevant courses

#Url generation
start <- 'https://skema.ku.dk/tt/tt.asp?SDB=KU1718&language=DK&folder=Reporting&style=textspreadsheet&type=module&idtype=id' 
end <- '&width=0&height=0&template=SWSCUST2+module+textspreadsheetA'

weeks <- list('E'='&weeks=1-27', 
              'F'='&weeks=27-52', 
              'S'=c('&weeks=1-27', '&weeks=27-52'))

periods <- list('E'='&days=1-5&periods=1-68', 
                'F'='&days=1-5&periods=5-52', 
                'S'=c('&days=1-5&periods=1-68', '&days=1-5&periods=5-52'))

url <- Vectorize(function(sem, id) {
  these_weeks <- weeks[[sem]]
  these_periods <- periods[[sem]]
  this_id <- str_c('&id=',id)
  map2(these_weeks, these_periods,
       function(x,y) {str_c(start, this_id, x, y, end)})
})

#Function for scraping all courses in a department

get_timetable <- Vectorize(function(scraped) {
  tt <- bind_rows(scraped[map_lgl(scraped, is.data.frame)])
  if (nrow(tt)==0) {
    tt <- NA
  }
  tt
})

get_errors <- Vectorize(function(scraped) {
  es <- scraped[map_lgl(scraped, ~!is.data.frame(.))]
  if (length(es)==0) {
    es <- NA
  }
  es
})

scrape_department <- function(department, n=0) {
  
  courses <- course_list %>% 
    filter(Institut==department) %>% 
    select(-Institut, -Semester) %>% 
    mutate(Url=url(Timetable_sem, Id))
  
  if (n>0) {courses %<>% slice(1:n)}
  
  courses %<>% 
    mutate(Scraped = map(Url, ~map(unlist(.), scrape_course_err)),
           Timetable = get_timetable(Scraped),
           Errors = get_errors(Scraped))
  
  timetable <- courses %>%
    filter(!is.na(Timetable)) %>%
    .$Timetable %>%
    bind_rows()
  
  failures <- courses %>%
    filter(is.na(Timetable))
  
  list('Timetable'=timetable, 'Failures'=failures, 'Courses'=courses)
}

economy_timetable <- scrape_department(2200)
write_csv(economy_timetable[[1]], 'economy_timetable.csv')
write_csv(select(economy_timetable[[2]], Modul), 'errors.csv')
