source('00_config.R')

union.credit <- read.csv("tablas/union.credit.csv") %>% 
  tbl_df() %>% 
  mutate(y = default.payment.next.month)
union.credit %>% head
dim(union.credit)
names(union.credit)



# Probabilidades
tab.probs.test <- read.csv("tablas/prob_SVM_rbf_conSelected_RAM_01.csv") %>% 
  dplyr::rename( ID = X) 
dim(tab.probs.test)

tab.probs.train <- read.csv("tablas/X_train_s_IDs.csv") %>% 
  dplyr::rename( ID = X) 
dim(tab.probs.train)

tab.probs.test %>% 
  rbind(tab)



# KMEANS

x.kmeans <- union.credit %>% 
  dplyr::select(SEX:AGE) %>% 
  mutate(EDUCATION = factor(EDUCATION))

kmns.3 <- kmeans(x.kmeans, centers = 3)
kmns.4 <- kmeans(x.kmeans, centers = 4)
kmns.5 <- kmeans(x.kmeans, centers = 5)

union.credit %>% 
  dplyr::select(ID) %>% 
  mutate(
    gpo3 = kmns.3$cluster,
    gpo4 = kmns.4$cluster,
    gpo5 = kmns.5$cluster
  ) %>% 
  write.csv("tablas/grupos_id.csv", row.names = F)

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



union.credit$gpo <- kmns.2$cluster
union.credit %>% 
  dplyr::select(ID, gpo) %>% 
  write.csv("tablas/tabla_grupos.csv", row.names = F)



# JERARQUICO

x.jer <- union.credit %>% 
  filter(y == 1) %>% 
  dplyr::select(SEX:AGE)

hcl <- hclust(d = dist(as.matrix(x.jer))^2, method = "ward.D")
plot(hcl)
rect.hclust(hcl, k = 3, border = 'red')
rect.hclust(hcl, k = 4, border = 'blue')
rect.hclust(hcl, k = 5, border = 'green')
rect.hclust(hcl, k = 6, border = 'green')


gposhc.3 <- cutree(hcl, k = 3)
gposhc.4 <- cutree(hcl, k = 4)
gposhc.6 <- cutree(hcl, k = 6)

prop.table(table(gposhc.3) )

x.jer %>% 
  mutate(age.gpo = cut_interval(AGE, n = 4), 
         gposhc.4 = gposhc.3) %>% 
  dplyr::select(-AGE) %>% 
  gather(var.lab, var.val, SEX:MARRIAGE, age.gpo) %>% 
  group_by(gposhc.4, var.lab, var.val) %>% 
  tally %>% 
  group_by(gposhc.4, var.lab) %>% 
  mutate(prop =100*n/sum(n)) %>% 
  dplyr::select(-n) %>% 
  spread(gposhc.4, prop) %>% data.frame()


tab.probs <- read.csv("tablas/prob_svm.csv") 

tab.gposprobs <- union.credit %>% 
  filter(y == 1) %>% 
  dplyr::select(ID:AGE, y) %>% 
  mutate(gpos = gposhc.3) %>% 
  left_join(tab.probs, by = "ID")


ggprobs <- ggplot(tab.gposprobs, aes( x= prb_uno, fill = factor(gpos))) + 
  geom_density(alpha = .5)+
  guides(fill = guide_legend(title = "Grupos")) + 
  scale_fill_manual(values = c("#A52A2A","#4682B4","#708090","#C0C0C0","#5F9EA0")) +
  facet_wrap(~gpos) + 
  xlab("Probabilidad de No Pago")

ggsave(plot = ggprobs, filename = "graphs/5_densidad_gpos.png", width = 8, height = 3)

