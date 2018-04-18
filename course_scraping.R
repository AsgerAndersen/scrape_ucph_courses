library(magrittr)
library(tidyverse)
library(rvest)

setwd('~/my_files/arbejde/sodas/scrape_ucph_courses')

#---------------------------------------------------------------------------------------
#Function for loading and formatting the course lists. 
#The loaded csv files have been made with js files taken from the timetable webpage.

read_courselist <- function(year) {
  
  starty <- str_sub(year, end=2)
  endy <- str_sub(year, start=3)
  sems <- c(str_c('B5-5F',starty), 
            str_c('B5-5F',endy), 
            str_c('E',starty), 
            str_c('F',endy))
  
  timetable_sem <- Vectorize(function(sem) {
    if (is.element(sem, sems[1:2])) {'S'}
    else if (sem==sems[3]) {'E'}
    else {'F'}
  })
  
  read_csv(str_c('course_list',year,'.csv'),
           col_names = c('module', 'institute', 'id')) %>%
    mutate(institute_num = as.integer(str_sub(institute,1,4)),
           semester = str_remove(str_sub(module, 6),';.*')) %>%
    filter(is.element(semester, sems)) %>% 
    mutate(timetable_sem = timetable_sem(semester),
           start_year = starty,
           end_year = endy)
  
}


years <- c('1516','1617','1718')
course_lists <- map(years, read_courselist)
names(course_lists) <- years

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
    mutate(institute_num = title[1],
           semester = title[2],
           course_name = title[3]) %>% 
    select(institute_num, semester, course_name, 
           class_type = Type, date = Dato, 
           start_time = Start, end_time = Slut, 
           place = Lokale, description = Beskrivelse)
}

 
#x <- scrape_course('https://skema.ku.dk/tt/tt.asp?SDB=KU1617&language=DK&folder=Reporting&style=textspreadsheet&type=module&idtype=id&id=61367&weeks=1-27&days=1-5&periods=5-52&width=0&height=0&template=SWSCUST2+module+textspreadsheetA')
#rm(x)

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
url_start <- 'https://skema.ku.dk/tt/tt.asp?SDB=KU'
url_mid <- '&language=DK&folder=Reporting&style=textspreadsheet&type=module&idtype=id' 
url_end <- '&width=0&height=0&template=SWSCUST2+module+textspreadsheetA'

weeks <- list('E'='&weeks=1-27', 
              'F'='&weeks=27-52', 
              'S'=c('&weeks=1-27', '&weeks=27-52'))

periods <- list('E'='&days=1-5&periods=1-68', 
                'F'='&days=1-5&periods=5-52', 
                'S'=c('&days=1-5&periods=1-68', '&days=1-5&periods=5-52'))

url <- Vectorize(function(starty, endy, sem, id) {
  these_weeks <- weeks[[sem]]
  these_periods <- periods[[sem]]
  this_id <- str_c('&id=',id)
  this_year <- str_c(starty, endy)
  map2(these_weeks, these_periods,
       function(x,y) {str_c(url_start, this_year, url_mid, this_id, x, y, url_end)})
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

scrape_department <- function(course_list, department, n=0) {
  
  courses <- course_list %>% 
    filter(institute_num==department) %>% 
    select(-institute_num, -semester) %>% 
    mutate(Url=url(start_year, end_year, timetable_sem, id))
  
  if (n>0) {courses %<>% slice(1:n)}
  
  courses %<>% 
    mutate(scraped = map(Url, ~map(unlist(.), scrape_course_err)),
           timetable = get_timetable(scraped),
           errors = get_errors(scraped))
  
  timetable <- courses %>%
    filter(!is.na(timetable)) %>%
    .$timetable %>%
    bind_rows()
  
  failures <- courses %>%
    filter(is.na(timetable))
  
  list('timetable'=timetable, 'failures'=failures, 'courses'=courses)
}

#------------------------------------------------------------------------------------------------------------------
#Make one single timetable for all departments and format it

format_yearly_timetable <- function(tt, studyyear) {
  tt %>% 
    filter(is.na(description) | 
             description != 'Please do not take notice of this booking - it is a system related matter') %>% 
    mutate(place = ifelse(is.na(place), description, place),
           class_type = find_class_type(class_type, description),
           participants = find_participants(class_type, description),
           class_num = ifelse(str_detect(participants, regex('All|tilvalg|samf', ignore_case = T)),'-1',participants) %>% 
             str_extract_all(str_c('-{0,1}',team_number)) %>%
             map(replace_romans) %>%
             map_chr(function(ns) str_c(ns, collapse = ',')),
           class_num = ifelse(str_detect(participants, regex('tilvalg|samf', ignore_case = T)),'-2', class_num),
           studyyear=studyyear
          )
}

find_class_type <- Vectorize(function(t, b) {
  if ((!is.na(t)) && (t != "")) {
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
team_number <- '(\\d+|I+|samf|tilvalg)' 
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

find_participants <- Vectorize(function(t, b) {
  if(t=='Forelæsning') {
    'All'
  }
  else {
    d <- str_extract(b, regex(regex_deltagere, ignore_case = T)) 
    if (is.na(d)) {
      'All'
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

scrape_year <- function(course_list, departments, n_rows=0) {
  
  scraped_departs <- map(departments, function(d) {scrape_department(course_list, d, n_rows)})
  
  timetable <- bind_rows(map(scraped_departs, function(d) {d[['timetable']]}))
  failures <- bind_rows(map(scraped_departs, function(d) {d[['failures']]}))
  courses <- bind_rows(map(scraped_departs, function(d) {d[['courses']]}))
  
  timetable %<>% 
    format_yearly_timetable(str_c(course_list$start_year[1], '/', course_list$end_year[2]))
  
  list('timetable'=timetable, 'failures'=failures, 'courses'=courses)
  
}

departments <- c(2200, 2300, 2400, 2500)
year1718 <- scrape_year(course_lists[['1718']], departments)
year1617 <- scrape_year(course_lists[['1617']], departments)
year1516 <- scrape_year(course_lists[['1516']], departments)

collect_years <- function(years) {
  timetable <- bind_rows(map(years, function(d) {d[['timetable']]}))
  courses <- bind_rows(map(years, get_courses))
  list('timetable'=timetable, 'courses'=courses)
}

get_courses <- function(year) {
  year[['courses']] %>% 
    select(module, id, institute, start_year, end_year) %>%
    left_join(year[['failures']] %>% 
                select(id) %>%  
                mutate(error = 1)) %>%
    mutate(studyyear = str_c(start_year,'/',end_year),
           institute_num = str_sub(institute, end=4),
           error = ifelse(is.na(error), 0, 1)) %>%
    select(-id, -start_year, -end_year, -institute)
}

all_years <- collect_years(list(year1516, year1617, year1718))
timetable <- all_years[['timetable']]
courses <- all_years[['courses']]
rm(all_years, year1516, year1617, year1718)

timetable %<>% 
  mutate(class_num = str_split(class_num,',')) %>% 
  unnest() %>%
  mutate(class_num = as.integer(class_num))

#-----------------------------------------------------------------------------------------------
#Scrape course ids

scrape_course_ids <- Vectorize(function(url) {
  read_html(url) %>% 
    html_nodes('#main-area li') %>% 
    html_text()
})

simplify_names <- function(df) {
  df %>% 
    mutate(course_name_tomatch = 
             str_remove_all(course_name, 
             regex('\\(.*\\)|cancel{1,2}ed|udbydes|.*:|in(k|c)l.*|ects', ignore_case=T)))
}

course_ids <- data_frame(institute_num = departments,
                         studyboard_num = c('0009','0033','0010','0012'),
                         year = rep(list(list(c('2015-2016', '15/16'), 
                                              c('2016-2017', '16/17'), 
                                              c('2017-2018', '17/18'))), 4)) %>% 
  unnest() %>% 
  mutate(year_url = map_chr(year, function(y) y[1]),
         studyyear = map_chr(year, function(y) y[2]),
         url = str_c('https://kurser.ku.dk/archive/', year_url, '/STUDYBOARD_', studyboard_num),
         course_id = scrape_course_ids(url)) %>% 
  select(-studyboard_num, -year_url, -year) %>% 
  unnest() %>% 
  mutate(course_id = str_split(course_id, '-', 2),
         course_name = map_chr(course_id, function(c) {c[2]}),
         course_id = map_chr(course_id, function(c) {c[1]})) %>%
  simplify_names()

timetable_names <- timetable %>% 
  distinct(institute_num, studyyear, course_name, semester) %>% 
  simplify_names()

match_courses <- function(name, inst, year) {
  
  course_ids %>% 
    filter(institute_num == inst, 
           studyyear == year) %>%
    mutate(str_dist = as.double(adist(course_name_tomatch, name, ignore.case = T))) %>% 
    arrange(str_dist) %>% 
    select(course_name, course_name_tomatch, course_id, str_dist)
  
}

course_map <- timetable_names %>% 
  mutate(matches = pmap(list(course_name_tomatch, 
                             institute_num, 
                             studyyear), match_courses),
         matched_name = map_chr(matches, function(l) l[['course_name_tomatch']][1]),
         matched_name_full = map_chr(matches, function(l) l[['course_name']][1]),
         matched_id = map_chr(matches, function(l) l[['course_id']][1]),
         matched_dist = map_dbl(matches, function(l) l[['str_dist']][1]))

ggplot(course_map, aes(matched_dist)) + geom_bar()
ggplot(filter(course_map, matched_dist < 16), aes(matched_dist)) + geom_bar()
accept_threshold <- 5

timetable <- course_map %>% 
  filter(matched_dist <= accept_threshold) %>% 
  select(institute_num, studyyear, semester, 
         course_name, matched_name = matched_name_full, 
         matched_id) %>% 
  right_join(timetable)

filter(timetable, is.na(matched_id)) %>% distinct(course_name) %>% .$course_name

#----------------------------------------------------------------------------------------------
#Save data
write_csv(timetable, 'timetable.csv')
write_csv(courses, 'scraped_courses.csv')
write_csv(select(course_map, -matches), 'course_map.csv')
