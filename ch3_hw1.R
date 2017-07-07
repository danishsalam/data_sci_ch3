
org_df = read.csv("refine_original.csv")
df <- tbl_df(org_df)

#task-1
df$company <-gsub("\\w+ps$", "philips", df$company, ignore.case = TRUE)
df$company <-gsub("^ak\\s*\\w+", "akzo", df$company, ignore.case = TRUE)
df$company <-gsub("^van\\s\\w+", "van houten", df$company, ignore.case = TRUE)
df$company <-gsub("^un\\w+", "unilever", df$company, ignore.case = TRUE)
df

#task-2
df <- df %>%
  separate(Product.code...number, c("ProductCode", "ProductNumber"), "-")

#task-3
df <- df %>% 
  mutate(category = case_when(
    .$ProductCode=="p" ~ "Smartphone",
    .$ProductCode=="v" ~ "TV",
    .$ProductCode=="x" ~ "Laptop",
    .$ProductCode=="q" ~ "Tablet"
  ))

#task-4
df <-df %>% 
  mutate(full_address = paste(address, city, country, sep = ', '))

#task-5
df %>% 
  mutate(company_philips = if_else(company=="philips", 1, 0)) %>% 
  mutate(company_akzo = if_else(company=="akzo", 1, 0)) %>% 
  mutate(company_van_houten = if_else(company=="van houten", 1, 0)) %>% 
  mutate(company_unilever = if_else(company=="unilever", 1, 0)) %>% 
  mutate_at(vars(starts_with("company_")),funs(as.logical)) %>%
  select(company, company_akzo, company_philips, company_van_houten, company_unilever)
  

df <- data.frame(x = c("a", "a b", "a b c", NA))
df %>% select(x) %>% filter(x=="a")

df <- data.frame(x = c("x: 123", "y: error: 7"))
df %>% separate(x, c("key", "value"), ": ", extra = "merge")

  
  