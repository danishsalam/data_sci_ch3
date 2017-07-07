#install and load hflights  data frame 
#devtools::install_github("hadley/hflights")
#library(hflights)

#load local data frame in f dataframe variable
f <- tbl_df(hflights)

#print first 10 rows of f
print(f, n=10)

#filter: limit the result set by specified row criteria
#& (AND) operator
f %>% 
  filter(DayOfWeek==7 & DayofMonth==2)

#| (OR) operator
f %>% 
  filter(DayOfWeek==7 | DayofMonth==2)

#| and & operator
f %>% 
  filter((DayOfWeek==7 | DayofMonth==2) & UniqueCarrier=="AA")

#in operator
f %>% 
  filter(UniqueCarrier %in% c("AS"))

#not In operator and count
f %>% 
  filter(!UniqueCarrier %in% c("AS")) %>% 
  count(UniqueCarrier, na.rm=TRUE)

#Filtering out NA values 
f %>% 
  filter(is.na(CancellationCode)) %>% 
  count(CancellationCode)

#select
f %>% 
  select(Year, Month, DayofMonth)

#select with string functionss
f %>% 
  select(ends_with("delay"), contains("month", ignore.case=TRUE) )

#range select 
f %>% 
  select(Dest:Cancelled, one_of(c("Year", "Month")))

# select everything
f %>% 
  select(everything())

# select ignore column
f %>% 
  select(-(Dest:Cancelled))

# select ignore and add column with index
f %>% 
  select((2:4), "DepTime")

# select vector
f %>% 
  select(c("Month", "DayofMonth", "DayOfWeek"))

cname <- c("Month", "DayofMonth", "DayOfWeek")
f %>% 
  select(one_of(cname))

#arrange + chaining
f %>% 
  filter(UniqueCarrier=="AS") %>% 
  select("Month", "DayofMonth", DayOfWeek, "Year") %>% 
  arrange(desc(Month))
  
#mutate- add variable
f %>% 
  select(ends_with("delay")) %>% 
  mutate(diffdelay = ArrDelay - DepDelay)


#mutate- add and store new diffdelay variable
f <- f %>%  mutate(diffdelay = ArrDelay - DepDelay)

#Summrize
f %>% 
  group_by(Dest) %>% 
  summarise(ayg_delay = mean(ArrDelay, na.rm=TRUE))

#Summrize_at
f %>% 
  group_by(Dest) %>% 
  summarise_at(c("ArrDelay", "DepDelay"), mean, na.rm=TRUE)


# for each carrier, calculate the minimum and maximum arrival and departure delays
f %>% 
  group_by(UniqueCarrier) %>% 
  summarise_each(funs(min(., na.rm=TRUE), max(.,na.rm=TRUE)), ends_with("Delay"))

# count the flight on each day.
f %>% 
  group_by(Month, DayofMonth) %>% 
  summarize(fcount = n()) %>% 
  arrange(Month, DayofMonth, desc(fcount))


# count the flight on each day using Tally function.
f %>% 
  group_by(Month, DayofMonth) %>% 
  tally(sort=TRUE)

# count the flight on each day using Tally function.
f %>% 
  group_by(Dest) %>% 
  summarise(fcount = n(), pcount = n_distinct(TailNum))

# for each destination, show the number of cancelled and not cancelled flights
f %>% 
  filter(DayofMonth == 1) %>% 
  group_by(Dest) %>% 
  select(Cancelled) %>% 
  table() %>% 
  head()
  
##window function - min-rank
f %>% 
  group_by(UniqueCarrier) %>% 
  select(Month, DayofMonth, DayOfWeek, DepDelay) %>% 
  filter(min_rank(desc(DepDelay))<=2) %>% 
  arrange(UniqueCarrier, desc(DepDelay))

#rewrite rank function
f %>% 
  group_by(UniqueCarrier) %>% 
  select(Month, DayofMonth, DayOfWeek, DepDelay) %>% 
  top_n(2) %>% 
  arrange(UniqueCarrier, desc(DepDelay))

#for each month, calculate the number of flights and difference from the last month.
f %>% 
  group_by(Month) %>% 
  summarise(fcount = n()) %>% 
  mutate(change = fcount - lag(fcount))

f %>% 
  group_by(Month) %>% 
  tally() %>% 
  mutate(change = n - lag(n))
  


  
  
  




  
  
  

  
  
  
