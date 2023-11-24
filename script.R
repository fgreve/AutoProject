library(dplyr)
library(stringr)
library(ggplot2)

folder <- "C:/Users/fgrev/autored/"
setwd(folder)

df = read.csv(file="dalbagli.csv", header=TRUE, sep=";", encoding = "UTF-8")

df <- tbl_df(df)
df

df$price <- as.numeric(as.character(df$price))
df$price_mlls <- df$price / (10^6)

class(df$price)
mean(df$price, na.rm=TRUE)

#elimina las observaciones con price = NA
df <- na.omit(df, cols="price")

summarise(df, avg = round(mean(price_mlls, na.rm = TRUE),1), median = median(price_mlls, na.rm = TRUE), sd = round(sd(price_mlls, na.rm = TRUE), 1), min = min(price_mlls, na.rm = TRUE), max = max(price_mlls, na.rm = TRUE), n = n())

df <- df %>% mutate(dumi_aceptada = if_else(estado == "Aceptada", 1, 0) )
df <- df %>% mutate(dumi_rechazada = if_else(estado == "Rechazada", 1, 0) )
df <- df %>% mutate(dumi_abandonada = if_else(estado == "Abandonada", 1, 0) )
df <- df %>% mutate(dumi_negociacion = if_else(estado == "En Negociación", 1, 0) )
df <- df %>% mutate(dumi_otroestado = if_else(estado != "Rechazada" & estado != "Aceptada" & estado != "Abandonada" & estado != "En Negociación", 1, 0) )

df <- df %>% mutate(antiguedad = 2018 - year)

#marca
marca <- df %>% group_by(MARCA) %>% summarise(avg_price_mlls_m = round(mean(price_mlls),1), median_price_mlls_m = round(median(price_mlls,1),1), 
                                               sd_price_mlls_m = round(sd(price_mlls), 1), min_price_mlls_m = round(min(price_mlls), 1), 
                                               max_price_mlls_m = round(max(price_mlls), 1), 
                                               trans_aceptada_m = sum(dumi_aceptada), trans_rechazada_m = sum(dumi_rechazada), 
                                               trans_abandonada_m = sum(dumi_abandonada), trans_negociacion_m = sum(dumi_negociacion),  
                                               marca_n = n())

#marca_modelo
marca_modelo <- df %>% group_by(MARCA, MODELO) %>% summarise(avg_price_mlls_mm = round(mean(price_mlls),1), median_price_mlls_mm = round(median(price_mlls,1),1), 
                                               sd_price_mlls_mm = round(sd(price_mlls), 1), min_price_mlls_mm = round(min(price_mlls), 1), 
                                               max_price_mlls_mm = round(max(price_mlls), 1), 
                                               trans_aceptada_mm = sum(dumi_aceptada), trans_rechazada_mm = sum(dumi_rechazada), 
                                               trans_abandonada_mm = sum(dumi_abandonada), trans_negociacion_mm = sum(dumi_negociacion),                                                 
                                               marca_modelo_n = n())


df2 <- df %>% left_join(marca_modelo, by = c("MARCA", "MODELO")) 
df2 <- df2 %>% mutate(v_porcentaje_mm = (trans_aceptada_mm + trans_negociacion_mm) / marca_modelo_n) %>% filter(marca_modelo_n >= 50)

hist(df2$v_porcentaje_mm)


hist(df2$v_porcentaje_mm)


df2 <- df2 %>% mutate(venta_dumi = if_else(trans_aceptada_mm + trans_negociacion_mm > 0, 1, 0) )


df2 <- df2 %>% mutate(gap_mm = price_mlls / median_price_mlls_mm)

df2$venta_dumi <- as.factor(df2$venta_dumi)
is.factor(df2$venta_dumi)

df2 <- df2 %>% mutate(km_miles = km / 1000)
df2 <- df2 %>% mutate(km_10mil = km_miles / 10)




#MODELO == "SAIL"
datos <- df2 %>% filter(MODELO == "SAIL" & km < 500000 & price_mlls > 0.5 & venta_dumi == 1) 
#ggplot(data = datos, aes(x = km, y = price_mlls)) + geom_point()
y <- datos$price_mlls
x <- datos$km_10mil
x2 <- x^2
x3 <- x^3

plot(x,y, pch=19, xlab = "Kilometraje (10 mil)", ylab = "Precio (millones)")

fit1 <- lm(y ~ x)
print(fit1)
summary(fit1) 
abline(fit1, col = "red")

fit2 <- lm(y ~ x+x2)
print(fit2)
summary(fit2) 
xv <- seq(min(x), max(x), 0.1)
yv <- predict(fit2, list(x=xv, x2=xv^2))
lines(xv,yv, col = "green")

hist(datos$price_mlls)

mean(datos$price_mlls)
median(datos$price_mlls)

quantile(datos$price_mlls)

quantile(datos$price_mlls, 0.25)

quantile(datos$price_mlls, 0.1)


min(datos$antiguedad)

max(datos$antiguedad)

var(datos$antiguedad)^0.5


fit3 <- lm(y ~ x+x2+x3)
print(fit3)
summary(fit3) 

xv <- seq(min(x), max(x), 0.1)
yv <- predict(fit3, list(x=xv, x2=xv^2, x3=xv^3))
lines(xv,yv, col = "black")



fit <- lm(price_mlls ~ km_miles, data=datos)  
print(fit)
summary(fit) 


datos <- df2 %>% filter(MODELO == "RAV4" & km < 500000 & price_mlls > 0.5 & venta_dumi == 1) 
ggplot(data = datos, aes(x = km, y = price_mlls)) + geom_point()
fit <- lm(price_mlls ~ km_miles, data=datos)  
print(fit)
summary(fit) 


glm.fit <- glm(venta_dumi ~ gap_mm + antiguedad, data = df2, family = binomial)


median(df2$marca_modelo_n)


hist(df2$marca_modelo_n)


#marca_modelo_version
marca_modelo_version <- df %>% group_by(MARCA, MODELO, version) %>% summarise(avg_price_mlls_mmv = round(mean(price_mlls),1), median_price_mlls_mmv = round(median(price_mlls,1),1), 
                                                             sd_price_mlls_mmv = round(sd(price_mlls), 1), min_price_mlls_mmv = round(min(price_mlls), 1), 
                                                             max_price_mlls_mmv = round(max(price_mlls), 1), 
                                                             trans_aceptada_mmv = sum(dumi_aceptada), trans_rechazada_mmv = sum(dumi_rechazada), 
                                                             trans_abandonada_mmv = sum(dumi_abandonada), trans_negociacion_mmv = sum(dumi_negociacion),  
                                                             marca_modelo_version_n = n())

#marca_modelo_version_year
marca_modelo_version_year <- df %>% group_by(MARCA, MODELO, version, year) %>% summarise(avg_price_mlls_mmvy = round(mean(price_mlls),1), median_price_mlls_mmvy = round(median(price_mlls,1),1), 
                                                                              sd_price_mlls_mmvy = round(sd(price_mlls), 1), min_price_mlls_mmvy = round(min(price_mlls), 1), 
                                                                              max_price_mlls_mmvy = round(max(price_mlls), 1), 
                                                                              trans_aceptada_mmvy = sum(dumi_aceptada), trans_rechazada_mmvy = sum(dumi_rechazada), 
                                                                              trans_abandonada_mmvy = sum(dumi_abandonada), trans_negociacion_mmvy = sum(dumi_negociacion),  
                                                                              marca_modelo_version_year_n = n())


df1 <- df %>% left_join(select(marca, MARCA, median_price_mlls_m, marca_n), by = "MARCA") %>%
        mutate(gap_m = price_mlls / median_price_mlls_m)



df2 <- df %>% left_join(select(marca_modelo, MARCA, MODELO, median, marca_modelo_n), by = c("MARCA", "MODELO") ) %>%
  mutate(gap_marca_modelo = price_mlls / median) %>% select(-median)

df3 <- df %>% left_join(select(marca_modelo_version, MARCA, MODELO, version, median, marca_modelo_version_n), by = c("MARCA", "MODELO", "version") ) %>%
  mutate(gap_marca_modelo_version = price_mlls / median) %>% select(-median)

df4 <- df %>% left_join(select(marca_modelo_version_year, MARCA, MODELO, year, version, median, marca_modelo_version_year_n), by = c("MARCA", "MODELO", "version", "year") ) %>%
  mutate(gap_marca_modelo_version_year = price_mlls / median) %>% select(-median)


#histograma sacando las muestras que tienen 1 solo auto gap=1
#marca
datos_hist <- df %>% filter(marca_n > 1) 
hist(datos_hist$gap_marca, main="", ylab="", xlab="", xlim=c(0.5, 1.5), breaks=500)

#marca_modelo
datos_hist <- df %>% filter(marca_modelo_n > 1) 
hist(datos_hist$gap_marca_modelo, main="", ylab="", xlab="", xlim=c(0.5, 1.5), breaks=500)


#marca_modelo_version
datos_hist <- df %>% filter(marca_modelo_version_n > 1) 
hist(datos_hist$gap_marca_modelo_version, main="", ylab="", xlab="", xlim=c(0.5, 1.5), breaks=500)

#marca_modelo_version_year
datos_hist <- df %>% filter(marca_modelo_version_year_n > 1) 
hist(datos_hist$gap_marca_modelo_version_year, main="", ylab="", xlab="", xlim=c(0.5, 1.5), breaks=500)


trans_abandonada_mmvy

datos_hist <- df %>% filter(year_n > 1)
ggplot2.histogram(data=datos_hist, xName='gap_marca_modelo_version_year',
                    groupName='aceptada', legendPosition="top", xlim=c(0.5, 1.5), breaks=500)


datos_hist <- df %>% filter(year_n >= 50) %>% filter(aceptada == 1 | abandonada == 1)
ggplot2.histogram(data=datos_hist, xName='gap_marca_modelo_version_year',
                  groupName='aceptada', legendPosition="top", xlim=c(0.5, 1.5), breaks=500)


datos <- df %>% filter(year_n >= 50) 
fit <- lm(gap_marca_modelo_version ~ antiguedad, data=datos)  
print(fit)
summary(fit) 

datos <- df %>% filter(year_n >= 50) %>% filter(aceptada == 1 | abandonada == 1)
fit <- lm(gap_marca_modelo_version ~ antiguedad, data=datos)  
print(fit)
summary(fit) 

datos <- df %>% filter(year_n >= 70) 
fit <- lm(gap_marca_modelo_version ~ antiguedad, data=datos)  
print(fit)
summary(fit) 


lines(predict(fit))
plot(datos)+abline(lm(gap_marca_modelo_version ~ antiguedad, data=datos))

abline(lm(datos$gap_marca_modelo_version ~ datos$antiguedad))

install.packages("devtools")
library(devtools)
install_github("easyGgplot2", "kassambara")

library("easyGgplot2", "kassambara")

sum(df$sex == 'M')

df_Tata <- df2 %>% filter(MARCA == "Tata")

nrow(df2)

class(marca$MARCA)

e <- ggplot(df2 %>% filter(price_mlls <= 45) , aes(x = reorder(MARCA, -price_mlls), price_mlls))
e + geom_boxplot() + coord_flip()

e <- ggplot(marca, aes(x = reorder(MARCA, -median), y = median)) 
e + geom_bar(stat = "identity") + coord_flip()


class(df2$MARCA)

e <- ggplot(df2 , aes(MARCA))
e + geom_bar(position = position_stack(reverse = TRUE)) + coord_flip()


ggplot(corr.m, aes(x = reorder(miRNA, -value), y = value, fill = variable)) + 
  geom_bar(stat = "identity")

geom_bar(aes(fill = drv), position = position_stack(reverse = TRUE)) +
  coord_flip() +
  theme(legend.position = "top")

mean(filter(df, MARCA == "Audi")$price, na.rm=TRUE)
median(filter(df, MARCA == "Audi")$price, na.rm=TRUE)

hist(df$price)

df$ln_km <- log(df$km)


df2 <- filter(df, km < 300000)

e <- ggplot(df2, aes(price, km))
e + geom_point()


#marca modelo version year
df$mmvy <- with(df, interaction(MARCA,  MODELO, version, year))
df$mmv <- with(df, interaction(MARCA,  MODELO, version))


df$price_n = toString(gsub("\\$", "", df$price))
df$price_n = toString(gsub("[.]", "", df$price))

df$price_n = str_trim(df$price_n)

head(df$price_n)

df$price_n = as.numeric(df$price)
sum(df$price_n)

head(df$price_n)

df$price <- as.numeric(df$price)

View(df$price_n)

sum(df$price_n)

summarise(Manzanas, avg = round(mean(PERSONAS),1), median = median(PERSONAS), sd = round(sd(PERSONAS), 1), min = min(PERSONAS), max = max(PERSONAS), n = n())


