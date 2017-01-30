source('00_config.R')

union.credit <- read.csv("tablas/union.credit.csv") %>% 
  tbl_df() %>% 
  mutate(y = default.payment.next.month)
union.credit %>% head
dim(union.credit)
names(union.credit)

# KMEANS

x.kmeans <- union.credit %>% 
  dplyr::select(SEX:AGE) %>% 
  mutate(EDUCATION = factor(EDUCATION))
kmeans.2 <- kmeans(x.kmeans, centers = 4)

x.kmeans$gpo <- kmeans.2$cluster

x.kmeans %>% 
  mutate(age.gpo = cut_interval(AGE, n = 4)) %>% 
  dplyr::select(-AGE) %>% 
  gather(var.lab, var.val, SEX:MARRIAGE, age.gpo) %>% 
  group_by(gpo, var.lab, var.val) %>% 
  tally %>% 
  group_by(gpo, var.lab) %>% 
  mutate(prop =100*n/sum(n)) %>% 
  dplyr::select(-n) %>% 
  spread(gpo, prop) %>% data.frame()
# JERARQUICO

x.kmeans <- union.credit %>% 
  dplyr::select(SEX:AGE) %>% 
  mutate(EDUCATION = factor(EDUCATION))
kmeans.2 <- kmeans(x.kmeans, centers = 4)

x.kmeans$gpo <- kmeans.2$cluster

x.kmeans %>% 
  mutate(age.gpo = cut_interval(AGE, n = 4)) %>% 
  dplyr::select(-AGE) %>% 
  gather(var.lab, var.val, SEX:MARRIAGE, age.gpo) %>% 
  group_by(gpo, var.lab, var.val) %>% 
  tally %>% 
  group_by(gpo, var.lab) %>% 
  mutate(prop =100*n/sum(n)) %>% 
  dplyr::select(-n) %>% 
  spread(gpo, prop) %>% data.frame()