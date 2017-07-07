org_df <- read.csv("titanic_original.csv")
df = tbl_df(org_df)

#Embarked  	C = Cherbourg, Q = Queenstown, S = Southampton
#pclass    	1 = 1st, 2 = 2nd, 3 = 3rd

#task-1: Port of embarkation
#update missing Embarked value
df <- df %>% 
  mutate(embarked = gsub("^$", "S", embarked))

#task-2: Age
df <- df %>% 
  mutate(age = if_else(is.na(age), mean(df$age, na.rm=TRUE), age))

#task-3: LifeBoat
df <- df %>% 
  mutate(boat = gsub("^$", "NA", boat))

#task-4: Cabin
df <- df %>% 
  mutate(as_cabin_number = if_else(grepl("^$", cabin), 0, 1))

unique(df$cabin)