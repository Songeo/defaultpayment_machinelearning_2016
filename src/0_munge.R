
# librerías
sapply(c("tidyverse", "forcats", "stringr", "lubridate"), 
       require, character.only = T)
theme_set(theme_light())

polls <- read_csv(file = "data/presidential_polls.csv")
polls %>% print(width =Inf) 

credit %>% print(width =Inf) 


# 1. Credit
credit <- read.csv(file = "data/UCI_Credit_Card.csv") %>% 
  tbl_df()

credit %>% summary()
credit %>% dim()
sum(credit$default.payment.next.month)

credit <- credit %>% 
  mutate(SEX = as.numeric(SEX == 1), # HOMBRE
         EDUCATION = ifelse(EDUCATION %in% c(6,0), 5, EDUCATION), 
         MARRIAGE = as.numeric(MARRIAGE == 1), # CASADO
         AGE_GRP = as.numeric(cut(AGE, 
                                  breaks = c(seq(20, 70, by = 5), 80), 
                                  include.lowest = T)),
         AGE_CENT = scale(AGE, center = T, scale = F)
         )
credit %>% print(width =Inf) 


# maximum delay
tab.max <- credit %>% 
  dplyr::select(ID, PAY_0:PAY_6) %>% 
  gather(var.lab, var.val, -1) %>% 
  group_by(ID) %>% 
  summarise( maxval = max(var.val), 
             maximum.delay = ifelse(maxval == -2, -1, maxval) ) %>% 
  dplyr::select(-maxval)
saveRDS(tab.max, 'tablas/tab.max.RDS')

tab.cents <- credit %>% 
  dplyr::select(ID, BILL_AMT1:PAY_AMT6) %>% 
  gather(var.lab, var.val, -1) %>% 
  group_by(var.lab) %>% 
  mutate(prom = mean(var.val), 
         var.est = (var.val - prom) /sd(var.val)) %>% 
  ungroup %>% 
  mutate(var.lab = paste0(var.lab, "_cent")) %>% 
  dplyr::select(-prom, -var.val) %>% 
  spread(var.lab, var.est)
saveRDS(tab.cents, 'tablas/tab.cents.RDS')  
apply(is.na(tab.cents), 2, sum)


# Leer tablas

tab.cents <- readRDS("tablas/tab.cents.RDS")
tab.porc <- readRDS("tablas/tabla_porc.rds")
tab.max <- readRDS("tablas/tab.max.RDS")

union.credit <- credit %>% 
  left_join(tab.cents, by = "ID") %>% 
  left_join(tab.porc, by = "ID") %>% 
  left_join(tab.max, by = "ID")


union.credit <- union.credit %>% 
  mutate(EDUCATION = ifelse(EDUCATION %in% c(6,0), 5, EDUCATION)) #%>%
  # group_by(EDUCATION) %>%  tally

apply(is.na(union.credit), 2, sum)
write.csv(union.credit, "tablas/union.credit.csv", row.names = F)
