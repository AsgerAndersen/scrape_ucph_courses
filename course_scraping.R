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
  filter(is.element(Semester, sems)) %>% 
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
    select(Institut, Semester, Kursusnavn, Type, Dato, Start, Slut, Lokale, Beskrivelse)
}

#Add error handling
scrape_course_err <- function(url) {
  tryCatch(scrape_course(url), error = function(e) {e})
}

#Helper functions
format_timetable <- function(tt) {
  tt %>% 
    mutate_all(to_character) %>%
    mutate(Dato=str_split(Dato, ';')) %>% 
    unnest()
}

to_character <- Vectorize(function(x) {
  if (is.logical(x)) {
    str_sub(as.character(x), 1, 1)
  }
  else x
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

scrape_departments <- function(course_list, departments, simplify_participants) {
  
}

economy_timetable <- scrape_department(2200)
politics_timetable <- scrape_department(2300)
antro_timetable <- scrape_department(2400)
socio_timetable <- scrape_department(2500)

#------------------------------------------------------------------------------------------------------------------
#Make one single timetable for all departments and format it

timetable <- bind_rows(list(economy_timetable[[1]],
               politics_timetable[[1]],
               antro_timetable[[1]],
               socio_timetable[[1]]))

format_yearly_timetable <- function(tt) {
  tt %>% 
    filter(is.na(Beskrivelse) | 
             Beskrivelse != 'Please do not take notice of this booking - it is a system related matter') %>% 
    mutate(Lokale = ifelse(is.na(Lokale), Beskrivelse, Lokale),
           Type = type(Type, Beskrivelse),
           Team = deltagere(Type, Beskrivelse),
           Team_num = ifelse(Team=='Alle','-1',Team) %>% 
             str_extract_all(str_c('-{0,1}',team_number)) %>%
             map(replace_romans) %>%
             map_chr(function(ns) str_c(ns, collapse = ',')))
}

type <- Vectorize(function(t, b) {
  if (!is.na(t)) {
    t
  }
  else {
    if (is.na(b)) {
      'Undervisning'
    }
    else if (str_detect(b, '(F|f)orelæsning|(L|l)ecture|F |L ')) {
      'Forelæsning'
    }
    else {
      'Undervisning'
    }
  }
})

team <- '(hold|ex. class|class|klynge|cluster|team)'
team_number <- '(\\d+|I+)' 
concat <- '(\\+|og|and|&|,)'
space <- '( {0,1})'
repeat_regex <- function(reg, star) {
  if (star) {s <- '*'}
  else {s <- '+'}
  str_c('(',reg,')',s)
}

regex_deltagere <- repeat_regex(str_c(team,
                                      space,
                                      team_number,
                                      repeat_regex(str_c(space,
                                                         concat,
                                                         space,
                                                         team_number), T),
                                      space), F)

deltagere <- Vectorize(function(t, b, simplify=T) {
  if(t=='Forelæsning') {
    'Alle'
  }
  else {
    d <- str_extract(b, regex(regex_deltagere, ignore_case = T)) 
    if (is.na(d)) {
      if (simplify) {
        'Alle'
      }
      else {
        b
      }
    }
    else {d}
  }
})

replace_romans <- Vectorize(function(n) {
  if (n=='I') {
    '1'
  }
  else if (n=='II') {
    '2'
  }
  else if (n=='III') {
    '3'
  }
  else {
    n
  }
})

timetable %<>% format_yearly_timetable()
write_csv(timetable, 'timetable_all_departments_1718.csv')

#1) Make function from yearly course list to formatted dataframe from all departments
#2) Get course lists from all years
#3) Scrape all years
