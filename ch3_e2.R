#install and load library(nycflights13)
#devtools::install_github("hadley/nycflights13")
#library(nycflights13)
#remove previous flight data frame
rm(f)
#load local data frame
f <- tbl_df(flights)

#hide month and day column
f %>% 
  select (-month, -day)

#hide range columns
f %>% 
  select(-(dep_time:arr_delay))

#hide columns which mathces the criteria
f %>% 
  select(-contains("time"))

#list of column to display
cols <- c("carrier", "flight")
f %>% 
  select(one_of(cols))
  
#rename column
f %>% 
  rename(tail = tailnum)

#between operator
f %>% 
  filter(between(dep_time, 600, 605) & !is.na(arr_time))

#slice
f %>% 
  group_by(month, day) %>% 
  slice(1:2)

#sample- random
f %>% 
  group_by(month, day) %>% 
  sample_n(3)

#top_n
f %>% 
  group_by(month, day) %>% 
  top_n(2, dep_delay) %>% 
  arrange(month, day, dep_delay)

#get unique/distinct rows
f %>% 
  select(origin, dest) %>% 
  distinct

#get unique/distinct count
f %>% 
  select(dest) %>% 
  distinct %>% 
  count
  
#mutate
f %>% mutate(speed = distance/air_time * 60)

#tansmutate, only show new column
f %>% transmute(speed = distance/air_time * 60)

mtcars %>% head()

mtcars %>% add_rownames("model") %>% head

#count
f %>% 
  group_by(month, day) %>% 
  summarise(cnt = n()) %>% 
  arrange(desc(cnt))

f %>% 
  count(month,day, sort=TRUE)

#sum
f %>% 
  group_by(month, day) %>% 
  summarise(t = sum(distance))

f %>% 
  count(month,day, wt=distance, sort=TRUE)

#group size, convert into vector
f %>% 
  group_by(month) %>% 
  group_size()

#no of group
f %>% 
  group_by(month) %>% 
  n_groups()

f %>% 
  group_by(month, day) %>% 
  summarise(cnt = n()) %>% 
  arrange(desc(cnt)) %>% print(n = 40)

f %>% 
  group_by(month, day) %>% 
  summarise(cnt = n()) %>% 
  ungroup() %>% 
  arrange(desc(cnt))

#use data_frame instead.

data_frame(a = 1:6, b = a*2, c = 'string-', 'd+e' = 1) %>% glimpse()

#joins

(a <- data_frame(color = c("green","yellow","red"), num = 1:3))
(b <- data_frame(color = c("green","yellow","pink"), num=c(1, 4, 5), size = c("S","M","L")))

inner_join(a, b)

full_join(a, b)

left_join(a, b)

semi_join(a,b)

anti_join(a, b)

#specify the colname in join
inner_join(a, b, by= c("color" = "color"))

View(f)