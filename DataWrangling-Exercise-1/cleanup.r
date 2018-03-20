library("dplyr")
library("stringr")
library("readr")

refine_original <- read.csv("~/Documents/Rstudio/DataScienceR/refine_original.csv", stringsAsFactors = FALSE)
data <- tbl_df(refine_original)


data$company <- str_replace(data$company, "0", "o")
data$company <- str_replace(data$company, "^f", "ph")
data$company <- str_replace(data$company, "ll", "l")
data$company <- str_replace(data$company, "hl", "hil")
data$company <- str_replace(data$company, "lp", "lip")
data$company <- str_replace(data$company, "lv", "lev")
data$company <- str_replace(data$company, "k z", "kz")
data$company <- str_to_lower(data$company)

data <- data %>% separate(Product.code...number, into=c("product_code", "product_number"), sep="-", remove = FALSE )

tlate = data.frame( product_code = c("p", "v", "x", "q"), product_category = c("Smartphone", "TV", "Laptop", "Tablet") )

data <- data %>% inner_join(tlate)

data <- data %>%
  mutate(company_philips = data$company == "philips") %>% 
  mutate(company_akzo = data$company == "akzo") %>% 
  mutate(company_vanhouten = data$company == "van houten") %>% 
  mutate(company_unilever = data$company == "unilever")

data <- data %>%
  mutate(product_smartphone = data$product_code == "p") %>% 
  mutate(product_tv = data$product_code == "v") %>% 
  mutate(product_laptop = data$product_code == "x") %>% 
  mutate(product_tablet = data$product_code == "q")

write_csv(data, "refine_clean.csv")
