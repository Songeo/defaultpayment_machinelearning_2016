
source('00_config.R')

union.credit <- read.csv("tablas/union.credit.csv") %>% 
  tbl_df() %>% 
  mutate(y = default.payment.next.month, 
         LIMIT_BAL_cent = scale(LIMIT_BAL)) 
  
union.credit %>% head %>%  print(width = Inf)
dim(union.credit)
names(union.credit)

ConfusionMatrix <- function(vec.obs, vec.aju){
  mat <- table(vec.obs, vec.aju)
  print(mat)
  accuracy <- sum(diag(mat))/sum(mat)
  precision <- diag(mat)["1"]/sum(mat[,"1"])
  recall <- diag(mat)["1"]/sum(mat["1",])
  return( c("accuracy" = accuracy, 
            "precision" = precision, 
            "recall" = recall))
}





# Entrenamiento y muestra
set.seed(20161201)
n <- nrow(union.credit)
samp.train <- sample(1:nrow(union.credit), size = round(n*.75)) 
samp.test <- setdiff(1:n, samp.train)
length(samp.train) + length(samp.test) # 30000
  

# ≈≈≈ #
# 1. variables centradas
# ≈≈≈ #
x.train <- union.credit[samp.train, ] %>% 
  dplyr::select(LIMIT_BAL, SEX:AGE, BILL_AMT1_cent:PAY_AMT6_cent)
y.train <- union.credit[samp.train, ] %>% 
  dplyr::select(y)

# rf.1 <- readRDS("cache/rf.1.rds")
rf.1 <- randomForest(x = x.train, 
                        y = factor(y.train$y),
                        importance=TRUE)
# saveRDS(rf.1, "cache/rf.1.rds")
varImpPlot(rf.1, type = 1)
ConfusionMatrix(vec.obs = y.train$y, vec.aju = predict(rf.1))


# error de prueba
x.test <- union.credit[samp.test, ] %>% 
  dplyr::select(  one_of(names(x.train)) )
y.test <- union.credit[samp.test, ] %>% 
  dplyr::select(y)
ConfusionMatrix(vec.obs = y.test$y, vec.aju = predict(rf.1, x.test))




# ≈≈≈ #
# 2. variables excluyendo demograficas
# ≈≈≈ #
x.train <- union.credit[samp.train, ] %>% 
  dplyr::select(LIMIT_BAL, 
                PAY_0:PAY_6,
                BILL_AMT1_cent:BILL_AMT6_cent,
                PAY_AMT1_cent:PAY_AMT6_cent,
                AMT1_PBILLIM:AMT6_PBILLIM,
                AMT1_PPAYBIL:AMT6_PPAYBIL,
                maximum.delay)
y.train <- union.credit[samp.train, ] %>% 
  dplyr::select(y)

# rf.2 <- readRDS("cache/rf.2.rds")
rf.2 <- randomForest(x = x.train, 
                     y = factor(y.train$y),
                     importance=TRUE)
# saveRDS(rf.2, "cache/rf.2.rds")
ConfusionMatrix(vec.obs = y.train$y, vec.aju = predict(rf.2))
varImpPlot(rf.2)
  

# error de prueba
x.test <- union.credit[samp.test, ] %>% 
  dplyr::select(  one_of(names(x.train)) )
y.test <- union.credit[samp.test, ] %>% 
  dplyr::select(y)
ConfusionMatrix(vec.obs = y.test$y, vec.aju = predict(rf.2,newdata =  x.test))

# ≈≈≈ #
# 3. variables completas
# ≈≈≈ #
x.train <- union.credit[samp.train, ] %>% 
  dplyr::select(LIMIT_BAL, 
                SEX:AGE, 
                PAY_0:PAY_6,
                BILL_AMT1_cent:PAY_AMT6_cent,
                AMT1_PBILLIM:AMT6_PBILLIM,
                AMT1_PPAYBIL:AMT6_PPAYBIL,
                maximum.delay)
y.train <- union.credit[samp.train, ] %>% 
  dplyr::select(y)

rf.3 <- randomForest(x = x.train, 
                     y = factor(y.train$y),
                     importance=TRUE)
# saveRDS(rf.3, "cacherf.3.rds")
ConfusionMatrix(vec.obs = y.train$y, vec.aju = predict(rf.3))


# error de prueba
x.test <- union.credit[samp.test, ] %>% 
  dplyr::select(  one_of(names(x.train)) )
y.test <- union.credit[samp.test, ] %>% 
  dplyr::select(y)
ConfusionMatrix(vec.obs = y.test$y, vec.aju = predict(rf.3, x.test))


# IMPORTANCE GRAPHS
varImpPlot(rf.3)
varImpPlot(rf.2, type = 2, cex = .8, main = "")

png("graphs/0_imp_bosque1.png", width = 250, height = 400)
varImpPlot(rf.1, type = 2, cex = .8, main = "")
dev.off()

png("graphs/0_imp_bosque2.png", width = 300, height = 500)
varImpPlot(rf.2, type = 2, cex = .8, main = "")
dev.off()


# IMPORTANCE TABLES
importance(rf.3) %>% 
  as.data.frame() %>% 
  mutate(vars = rownames(.)) %>% 
  arrange(MeanDecreaseGini)

tab.imp.2 <- importance(rf.2) %>% 
  as.data.frame() %>% 
  mutate(vars = rownames(.), 
         tipo = "bosque aleatorio 2") %>% 
  arrange(desc(MeanDecreaseGini))

tab.imp.1 <- importance(rf.1) %>% 
  as.data.frame() %>% 
  mutate(vars = rownames(.), 
         tipo = "bosque aleatorio 1") %>% 
  arrange(desc(MeanDecreaseGini))



gg.1 <- ggplot(tab.imp.1, aes( x = MeanDecreaseGini, 
                       y = fct_reorder(vars, MeanDecreaseGini))) + 
    geom_point(size = 2)+ 
    facet_grid(tipo~., scales = 'free', space = 'free') + 
  ylab(NULL) + 
  xlab(NULL)

gg.2 <- ggplot(tab.imp.2, aes( x = MeanDecreaseGini, 
                       y = fct_reorder(vars, MeanDecreaseGini))) + 
    geom_point(size = 2)+ 
    facet_grid(tipo~., scales = 'free', space = 'free') + 
  ylab(NULL) + 
  xlab("Importancia de Gini")


gg <- gridExtra::grid.arrange(
  gg.1, gg.2, widths = c(7), heights = c(6, 10)
)
ggsave(plot = gg, filename = "graphs/gg_imps.png", width = 5, height = 9)


library(xtable)
tab.imp.2 %>% 
  dplyr::select(vars, MeanDecreaseGini) %>% 
  xtable() %>% 
  print(include.rownames = T)

png("")
varImpPlot(rf.2, type = 2, cex = .8, main = "")
dev.off()
importance(rf.1) %>% 
  as.data.frame() %>% 
  mutate(vars = rownames(.)) %>% 
  arrange(MeanDecreaseGini)


# ≈≈≈
# Prueba de selección
# ≈≈≈

vars.seleccion <- tab.imp.2[1:10,"vars"]

# ≈≈≈ #
# 3. variables completas
# ≈≈≈ #
x.train <- union.credit[samp.train, ] %>% 
  dplyr::select(one_of(vars.seleccion) )
y.train <- union.credit[samp.train, ] %>% 
  dplyr::select(y)

rf.pr <- randomForest(x = x.train, 
                     y = factor(y.train$y),
                     importance=TRUE)
# saveRDS(rf.pr, "cacherf.3.rds")
ConfusionMatrix(vec.obs = y.train$y, vec.aju = predict(rf.pr))


# error de prueba
x.test <- union.credit[samp.test, ] %>% 
  dplyr::select(  one_of(names(x.train)) )
y.test <- union.credit[samp.test, ] %>% 
  dplyr::select(y)
ConfusionMatrix(vec.obs = y.test$y, vec.aju = predict(rf.pr, x.test))




# 
union.credit %>% 
  dplyr::select(LIMIT_BAL_cent, 
                PAY_0:PAY_6,
                BILL_AMT1_cent:PAY_AMT6_cent,
                AMT1_PBILLIM:AMT6_PBILLIM,
                AMT1_PPAYBIL:AMT6_PPAYBIL,
                maximum.delay, default.payment.next.month) %>% 
  write.csv("tablas/variables_seleccion.csv", row.names = F)


union.credit %>% 
  dplyr::select(ID, SEX:AGE) %>% 
  write.csv("tablas/variables_demos.csv", row.names = F)




library(glmnet)
x.train <- union.credit[samp.train, ] %>% 
  dplyr::select(LIMIT_BAL_cent, 
                PAY_0:PAY_6,
                AMT1_PBILLIM:AMT6_PBILLIM,
                AMT1_PPAYBIL:AMT6_PPAYBIL,
                maximum.delay)
y.train <- union.credit[samp.train, ] %>% 
  dplyr::select(y)


cv.ridge <- cv.glmnet(x = as.matrix(x.train), y = (y.train$y), 
                      family= "binomial", 
                     alpha = .99)
plot(cv.ridge)
pred.ridge <- predict(cv.ridge, as.matrix(x.train), s = exp(-5))
ConfusionMatrix(y.train$y, pred.ridge)
coef(cv.ridge, s = cv.ridge$lambda)
cv.ridge$lambda.min