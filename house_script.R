## recherche automatique des colonnes quantitatives et catégorielles
nb_col = c(1:ncol(train))

quant_list = c()
cat_list = c()

for (i in nb_col) {
  if(is.numeric(train[,i])==TRUE){
    quant_list = c(quant_list,i)
  }else{
    cat_list = c(cat_list,i)
  }
}

quant_list
cat_list

## matrice de corélation
# Libraries
library(ellipse)
library(RColorBrewer)

# Use of the mtcars data proposed by R
data <- cor(train[,quant_list])

# Build a Pannel of 100 colors with Rcolor Brewer
my_colors <- brewer.pal(5, "YlOrRd")
my_colors <- colorRampPalette(my_colors)(100)

# Order the correlation matrix
ord <- order(data[1, ])
data_ord <- data[ord, ord]
plotcorr(data_ord , col=my_colors[data_ord*50+50] , mar=c(1,1,1,1)  )

##
quant_list1 = quant_list[1:(length(quant_list)/3)]
quant_list2 = quant_list[(length(quant_list)/3+1):(2*length(quant_list)/3)]
quant_list3 = quant_list[(2*length(quant_list)/3+1):(length(quant_list))]
par(mfrow=c(4,4))
for (i in quant_list1){
  hist(train[,i], main = colnames(train)[i], col = sample(colours(), 1))
}
par(mfrow=c(4,4))
for (i in quant_list2){
  hist(train[,i], main = colnames(train)[i], col = sample(colours(), 1))
}
par(mfrow=c(4,4))
for (i in quant_list3){
  hist(train[,i], main = colnames(train)[i], col = sample(colours(), 1))
}

par(mfrow=c(1,1))
hist(train[,1], col = rgb(1,0,0,0.5))
hist(train[,3], col = rgb(0,0,1,0.5), add = T)

## challenge
## voir ce qui cause une chute se salePrice

# Régression linéaire multiple 
reg.multiple <- lm(SalePrice~Id + MSSubClass + LotFrontage + LotArea + OverallQual + OverallCond
                   + YearBuilt + YearRemodAdd + MasVnrArea + BsmtFinSF1 + BsmtFinSF2 + BsmtUnfSF
                   + TotalBsmtSF + X1stFlrSF + X2ndFlrSF + LowQualFinSF + GrLivArea + BsmtFullBath
                   + BsmtHalfBath + FullBath + HalfBath + BedroomAbvGr + KitchenAbvGr + TotRmsAbvGrd
                   + Fireplaces + GarageYrBlt + GarageCars + GarageArea + WoodDeckSF + OpenPorchSF
                   + EnclosedPorch + X3SsnPorch + ScreenPorch + PoolArea + MiscVal + MoSold + YrSold
                   , data = train)


# Evaluer la multicolinéarité avec le Variance Inflation Factor 
library(car) 
vif(reg.multiple)

# calcul
reg.multipleOptimale <- step(reg.multiple, direction = "backward")

par(mfrow=c(2,2))

library(visreg)
visreg(reg.multipleOptimale)

library(coefplot)
coefplot(reg.multiple, intercept=F)

summary(reg.multiple)

#glm
logistique.res <- glm(SalePrice~Id + MSSubClass + LotFrontage + LotArea + OverallQual + OverallCond
                      + YearBuilt + YearRemodAdd + MasVnrArea + BsmtFinSF1 + BsmtFinSF2 + BsmtUnfSF
                      + TotalBsmtSF + X1stFlrSF + X2ndFlrSF + LowQualFinSF + GrLivArea + BsmtFullBath
                      + BsmtHalfBath + FullBath + HalfBath + BedroomAbvGr + KitchenAbvGr + TotRmsAbvGrd
                      + Fireplaces + GarageYrBlt + GarageCars + GarageArea + WoodDeckSF + OpenPorchSF
                      + EnclosedPorch + X3SsnPorch + ScreenPorch + PoolArea + MiscVal + MoSold + YrSold
                      , data = train)

visreg::visreg(logistique.res)
