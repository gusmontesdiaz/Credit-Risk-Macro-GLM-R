install.packages("DescTools")
library("DescTools")
#Cargamos la base de datos
library(readxl)
BaseDatosProyecto2 <- read_excel("BaseDatosProyecto3.xlsx")
View(BaseDatosProyecto2)
#Creamos los modelos

#Total banca múltiple 

m1 <- glm(TDC_Total ~ Inflación + Desempleo + TIIE + `PIB_LOG/100` +
            `Deuda_P_LOG/100` + `Ind. Act. Industrial/100` + `Remesas ln/100` +
            `población económicamente activa ln/100` + `IGAE ln/100` + `VaR IGAE` +
            `Imp ln/100` + `Exp (billones de dólares) ln/100` + `tipo de cambio vs dólar ln/100` +
            `Consumo Privado ln/100` + `Cetes (%)` + IGREMSE + `Remuneraciones empresas constructoras` +
            `IMFBCF ln/100` + `INPC  ln/100` + `UDIS  ln/100` + `Tasa objetivo ln/100` +
            `índice nacional de precios al consumidor` + `índice de confianza al consumidor ln/100`,
            data = BaseDatosProyecto2,
            family = quasibinomial(link = "logit"))
summary(m1)
#Variables irrelevantes:
#Inflación, Desempleo, TIIE, `PIB_LOG/100`, `Ind. Act. Industrial/100`,`IGAE ln/100`,
#`Exp (billones de dólares) ln/100` ,`Cetes (%)` ,IGREMSE, `Remuneraciones empresas constructoras` ,
#`IMFBCF ln/100`, `INPC  ln/100`,`UDIS  ln/100`,`Tasa objetivo ln/100` e `índice nacional de precios al consumidor` 

#Modelo ajustado
m11 <- glm(TDC_Total ~ `Deuda_P_LOG/100`  + `Remesas ln/100` +
            `población económicamente activa ln/100`  + `VaR IGAE` +
            `Imp ln/100` + `tipo de cambio vs dólar ln/100` +
            `Consumo Privado ln/100` +
              `índice de confianza al consumidor ln/100`,
          data = BaseDatosProyecto2,
          family = quasibinomial(link = "logit"))
summary(m11)
#BETAS DEL MODELO
#(Intercept)                                -1.458e+00
#`Deuda_P_LOG/100`                           7.702e+02 
#`Remesas ln/100`                            2.409e+01
#`población económicamente activa ln/100`   -3.212e+02 
#`VaR IGAE`                                 -2.357e-02 
#`Imp ln/100`                               -5.376e+02 
#`tipo de cambio vs dólar ln/100`           -1.767e+02 
#`Consumo Privado ln/100`                    9.587e+01 
#`índice de confianza al consumidor ln/100`  5.387e+01

# Ajustar el modelo nulo (solo intercepto)
modelo_nulo <- glm(TDC_Total ~ 1, data = BaseDatosProyecto2, family = quasibinomial(link = "logit"))

# Comparar el modelo nulo con el modelo completo usando ANOVA
anova(modelo_nulo, m11, test = "Chisq")

#Obtuvimos un Pr(>Chi) de 2.2e-16 *** lo cual nos indica que el modelo completo explica mejor la probabilidad
m13 <- lm(TDC_Total ~ `Deuda_P_LOG/100`  + `Remesas ln/100` +
             `población económicamente activa ln/100`  + `VaR IGAE` +
             `Imp ln/100` + `tipo de cambio vs dólar ln/100` +
             `Consumo Privado ln/100` +
             `índice de confianza al consumidor ln/100`,
           data = BaseDatosProyecto2)
summary(m13)
#Tenemos una R2 de .8543 lo cual es muy bueno y nos indica que las variables 
#explican aprox. 85% de la probabilidad de incumplimiento.


#BBVA 

m2 <- glm(TDC_BBVA ~ Inflación + Desempleo + TIIE + `PIB_LOG/100` +
            `Deuda_P_LOG/100` + `Ind. Act. Industrial/100` + `Remesas ln/100` +
            `población económicamente activa ln/100` + `IGAE ln/100` + `VaR IGAE` +
            `Imp ln/100` + `Exp (billones de dólares) ln/100` + `tipo de cambio vs dólar ln/100` +
            `Consumo Privado ln/100` + `Cetes (%)` + IGREMSE + `Remuneraciones empresas constructoras` +
            `IMFBCF ln/100` + `INPC  ln/100` + `UDIS  ln/100` + `Tasa objetivo ln/100` +
            `índice nacional de precios al consumidor` + `índice de confianza al consumidor ln/100`,
          data = BaseDatosProyecto2,
          family = quasibinomial(link = "logit"))
summary(m2)
#Variables irrelevantes:
#Inflación, Desempleo, `PIB_LOG/100`, `Ind. Act. Industrial/100`,consumo privado
#`Exp (billones de dólares) ln/100` ,`Cetes (%)` ,IGREMSE, `Remuneraciones empresas constructoras` ,
#`IMFBCF ln/100`, `INPC  ln/100`,`UDIS  ln/100`,`Tasa objetivo ln/100`, `índice de confianza al consumidor ln/100` e `índice nacional de precios al consumidor`

#Modelo ajustado
m22 <- glm(TDC_BBVA ~ TIIE + `Deuda_P_LOG/100` + `Remesas ln/100` +
            `población económicamente activa ln/100` + `IGAE ln/100` + `VaR IGAE` +
            `Imp ln/100` + `tipo de cambio vs dólar ln/100`,
          data = BaseDatosProyecto2,
          family = quasibinomial(link = "logit"))
summary(m22)
#BETAS DEL MODELO
#(Intercept)                              -2.438e+00  
#TIIE                                     -8.220e-01  
#`Deuda_P_LOG/100`                         6.681e+02  
#`Remesas ln/100`                          2.318e+01 
#`población económicamente activa ln/100` -1.194e+02 
#`IGAE ln/100`                             4.639e+01  
#`VaR IGAE`                               -1.356e-02  
#`Imp ln/100`                             -4.699e+02  
#`tipo de cambio vs dólar ln/100`         -2.203e+02  

# Ajustar el modelo nulo (solo intercepto)
modelo_nulo <- glm(TDC_BBVA ~ 1, data = BaseDatosProyecto2, family = quasibinomial(link = "logit"))

# Comparar el modelo nulo con el modelo completo usando ANOVA
anova(modelo_nulo, m22, test = "Chisq")

#Obtuvimos un Pr(>Chi) de 2.2e-16 *** lo cual nos indica que el modelo completo explica mejor la probabilidad

m23 <- lm(TDC_BBVA ~ TIIE + `Deuda_P_LOG/100` + `Remesas ln/100` +
             `población económicamente activa ln/100` + `IGAE ln/100` + `VaR IGAE` +
             `Imp ln/100` + `tipo de cambio vs dólar ln/100`,
           data = BaseDatosProyecto2)
summary(m23)
#Tenemos una R2 de .7905 lo cual es muy bueno y nos indica que las variables 
#explican aprox. 75% de la probabilidad de incumplimiento.


#Banorte

m3 <- glm(TDC_Banorte ~ Inflación + Desempleo + TIIE + `PIB_LOG/100` +
            `Deuda_P_LOG/100` + `Ind. Act. Industrial/100` + `Remesas ln/100` +
            `población económicamente activa ln/100` + `IGAE ln/100` + `VaR IGAE` +
            `Imp ln/100` + `Exp (billones de dólares) ln/100` + `tipo de cambio vs dólar ln/100` +
            `Consumo Privado ln/100` + `Cetes (%)` + IGREMSE + `Remuneraciones empresas constructoras` +
            `IMFBCF ln/100` + `INPC  ln/100` + `UDIS  ln/100` + `Tasa objetivo ln/100` +
            `índice nacional de precios al consumidor` + `índice de confianza al consumidor ln/100`,
          data = BaseDatosProyecto2,
          family = quasibinomial(link = "logit"))
summary(m3)
#Variables irrelevantes:
#Inflación, Desempleo, TIIE, `PIB_LOG/100`, `Ind. Act. Industrial/100`,consumo privado
#`Exp (billones de dólares) ln/100` ,`Cetes (%)` ,IGREMSE, `Remuneraciones empresas constructoras` ,
#`IMFBCF ln/100`, `INPC  ln/100`,`UDIS  ln/100`,`Tasa objetivo ln/100` e `índice nacional de precios al consumidor`

#Modelo ajustado
m32 <- glm(TDC_Banorte ~ `Deuda_P_LOG/100` + `Remesas ln/100` +
            `población económicamente activa ln/100` + `IGAE ln/100` + `VaR IGAE` +
            `Imp ln/100` +  `tipo de cambio vs dólar ln/100` +
            `índice de confianza al consumidor ln/100`,
          data = BaseDatosProyecto2,
          family = quasibinomial(link = "logit"))
summary(m32)
#BETAS DEL MODELO
#(Intercept)                                  -3.44585   
#`Deuda_P_LOG/100`                           807.70101   
#`Remesas ln/100`                             30.11324 
#`población económicamente activa ln/100`   -143.98573 
#`IGAE ln/100`                                18.48053 
#`VaR IGAE`                                   -0.01493
#`Imp ln/100`                               -618.62156
#`tipo de cambio vs dólar ln/100`           -171.54161
#`índice de confianza al consumidor ln/100`   46.42445

# Ajustar el modelo nulo (solo intercepto)
modelo_nulo <- glm(TDC_Banorte ~ 1, data = BaseDatosProyecto2, family = quasibinomial(link = "logit"))

# Comparar el modelo nulo con el modelo completo usando ANOVA
anova(modelo_nulo, m32, test = "Chisq")

#Obtuvimos un Pr(>Chi) de 2.2e-16 *** lo cual nos indica que el modelo completo explica mejor la probabilidad
m33 <- lm(TDC_Banorte ~ `Deuda_P_LOG/100` + `Remesas ln/100` +
             `población económicamente activa ln/100` + `IGAE ln/100` + `VaR IGAE` +
             `Imp ln/100` +  `tipo de cambio vs dólar ln/100` +
             `índice de confianza al consumidor ln/100`,
           data = BaseDatosProyecto2)
summary(m33)
#Tenemos una R2 de .8773 lo cual es muy bueno y nos indica que las variables 
#explican aprox. 87% de la probabilidad de incumplimiento.


#Santander

m4 <- glm(TDC_Santander ~ Inflación + Desempleo + TIIE + `PIB_LOG/100` +
            `Deuda_P_LOG/100` + `Ind. Act. Industrial/100` + `Remesas ln/100` +
            `población económicamente activa ln/100` + `IGAE ln/100` + `VaR IGAE` +
            `Imp ln/100` + `Exp (billones de dólares) ln/100` + `tipo de cambio vs dólar ln/100` +
            `Consumo Privado ln/100` + `Cetes (%)` + IGREMSE + `Remuneraciones empresas constructoras` +
            `IMFBCF ln/100` + `INPC  ln/100` + `UDIS  ln/100` + `Tasa objetivo ln/100` +
            `índice nacional de precios al consumidor` + `índice de confianza al consumidor ln/100`,
          data = BaseDatosProyecto2,
          family = quasibinomial(link = "logit"))
summary(m4)
#Variables irrelevantes:
#Inflación, Desempleo, TIIE, `PIB_LOG/100`, `Ind. Act. Industrial/100`,
#`IGAE ln/100` ,`Cetes (%)` ,IGREMSE, `Remuneraciones empresas constructoras` ,
#`IMFBCF ln/100`, `INPC  ln/100`,`UDIS  ln/100`,`Tasa objetivo ln/100` e `índice nacional de precios al consumidor`

#Modelo ajustado
m42 <- glm(TDC_Santander ~ `Deuda_P_LOG/100` + `Remesas ln/100` +
             `población económicamente activa ln/100`  + `VaR IGAE` +
             `Imp ln/100` + `Exp (billones de dólares) ln/100` +  `tipo de cambio vs dólar ln/100` +
             `Consumo Privado ln/100` + `índice de confianza al consumidor ln/100`,
           data = BaseDatosProyecto2,
           family = quasibinomial(link = "logit"))
summary(m42)
#BETAS DEL MODELO
#(Intercept)                                -8.457e+00  
#`Deuda_P_LOG/100`                           8.047e+02  
#`Remesas ln/100`                            5.211e+01 
#`población económicamente activa ln/100`   -5.751e+02  
#`VaR IGAE`                                 -3.655e-02 
#`Imp ln/100`                               -4.659e+02  
#`Exp (billones de dólares) ln/100`         -4.443e+01 
#`tipo de cambio vs dólar ln/100`           -1.468e+02 
#`Consumo Privado ln/100`                    3.976e+02 
#`índice de confianza al consumidor ln/100`  4.706e+01

# Ajustar el modelo nulo (solo intercepto)
modelo_nulo <- glm(TDC_Santander ~ 1, data = BaseDatosProyecto2, family = quasibinomial(link = "logit"))

# Comparar el modelo nulo con el modelo completo usando ANOVA
anova(modelo_nulo, m42, test = "Chisq")

#Obtuvimos un Pr(>Chi) de 2.2e-16 *** lo cual nos indica que el modelo completo explica mejor la probabilidad
m43 <- lm(TDC_Santander ~ `Deuda_P_LOG/100` + `Remesas ln/100` +
             `población económicamente activa ln/100`  + `VaR IGAE` +
             `Imp ln/100` + `Exp (billones de dólares) ln/100` +  `tipo de cambio vs dólar ln/100` +
             `Consumo Privado ln/100` + `índice de confianza al consumidor ln/100`,
           data = BaseDatosProyecto2)
summary(m43)
#Tenemos una R2 de .57 lo cual nos indica que las variables 
#explican aprox. 57% de la probabilidad de incumplimiento.


#Banamex

m5 <- glm(TDC_Citi ~ Inflación + Desempleo + TIIE + `PIB_LOG/100` +
            `Deuda_P_LOG/100` + `Ind. Act. Industrial/100` + `Remesas ln/100` +
            `población económicamente activa ln/100` + `IGAE ln/100` + `VaR IGAE` +
            `Imp ln/100` + `Exp (billones de dólares) ln/100` + `tipo de cambio vs dólar ln/100` +
            `Consumo Privado ln/100` + `Cetes (%)` + IGREMSE + `Remuneraciones empresas constructoras` +
            `IMFBCF ln/100` + `INPC  ln/100` + `UDIS  ln/100` + `Tasa objetivo ln/100` +
            `índice nacional de precios al consumidor` + `índice de confianza al consumidor ln/100`,
          data = BaseDatosProyecto2,
          family = quasibinomial(link = "logit"))
summary(m5)
#Variables irrelevantes:
#Inflación, Desempleo, TIIE, `PIB_LOG/100`, `Ind. Act. Industrial/100`,
#`IGAE ln/100` ,`Cetes (%)` ,IGREMSE, `Remuneraciones empresas constructoras` ,
#`IMFBCF ln/100`, `INPC  ln/100`,`UDIS  ln/100`,`Tasa objetivo ln/100` e `índice nacional de precios al consumidor`

#Modelo ajustado
m52 <- glm(TDC_Citi ~ `Deuda_P_LOG/100` + `Remesas ln/100` +
             `población económicamente activa ln/100`  + `VaR IGAE` +
             `Imp ln/100` + `Exp (billones de dólares) ln/100` +  `tipo de cambio vs dólar ln/100` +
             `Consumo Privado ln/100` + `índice de confianza al consumidor ln/100`,
           data = BaseDatosProyecto2,
           family = quasibinomial(link = "logit"))
summary(m52)
#BETAS DEL MODELO 
#(Intercept)                                -8.889e-01 
#`Deuda_P_LOG/100`                           8.575e+02  
#`Remesas ln/100`                            3.438e+01 
#`población económicamente activa ln/100`   -3.385e+02 
#`VaR IGAE`                                 -1.842e-02 
#`Imp ln/100`                               -6.448e+02 
#`Exp (billones de dólares) ln/100`         -3.700e+01 
#`tipo de cambio vs dólar ln/100`           -1.800e+02 
#`Consumo Privado ln/100`                    1.075e+02 
#`índice de confianza al consumidor ln/100`  8.964e+01 

# Ajustar el modelo nulo (solo intercepto)
modelo_nulo <- glm(TDC_Citi ~ 1, data = BaseDatosProyecto2, family = quasibinomial(link = "logit"))

# Comparar el modelo nulo con el modelo completo usando ANOVA
anova(modelo_nulo, m52, test = "Chisq")

#Obtuvimos un Pr(>Chi) de 2.2e-16 *** lo cual nos indica que el modelo completo explica mejor la probabilidad
m53 <- lm(TDC_Citi ~ `Deuda_P_LOG/100` + `Remesas ln/100` +
             `población económicamente activa ln/100`  + `VaR IGAE` +
             `Imp ln/100` + `Exp (billones de dólares) ln/100` +  `tipo de cambio vs dólar ln/100` +
             `Consumo Privado ln/100` + `índice de confianza al consumidor ln/100`,
           data = BaseDatosProyecto2)
summary(m53)
#Tenemos una R2 de .8966 lo cual es muy bueno y nos indica que las variables 
#explican aprox. 89% de la probabilidad de incumplimiento.


#HSBC

m6 <- glm(TDC_HSBC ~ Inflación + Desempleo + TIIE + `PIB_LOG/100` +
            `Deuda_P_LOG/100` + `Ind. Act. Industrial/100` + `Remesas ln/100` +
            `población económicamente activa ln/100` + `IGAE ln/100` + `VaR IGAE` +
            `Imp ln/100` + `Exp (billones de dólares) ln/100` + `tipo de cambio vs dólar ln/100` +
            `Consumo Privado ln/100` + `Cetes (%)` + IGREMSE + `Remuneraciones empresas constructoras` +
            `IMFBCF ln/100` + `INPC  ln/100` + `UDIS  ln/100` + `Tasa objetivo ln/100` +
            `índice nacional de precios al consumidor` + `índice de confianza al consumidor ln/100`,
          data = BaseDatosProyecto2,
          family = quasibinomial(link = "logit"))
summary(m6)
#Variables irrelevantes:
#Inflación, Desempleo, TIIE, `PIB_LOG/100`, `Ind. Act. Industrial/100`,
#`IGAE ln/100` ,`Cetes (%)` ,IGREMSE, `Remuneraciones empresas constructoras` ,
#`IMFBCF ln/100`, `INPC  ln/100`,`UDIS  ln/100`,`Tasa objetivo ln/100`,`índice de confianza al consumidor ln/100` e `índice nacional de precios al consumidor`

#Modelo ajustado
m62 <- glm(TDC_HSBC ~ `Deuda_P_LOG/100` + `Remesas ln/100` +
             `población económicamente activa ln/100`  + `VaR IGAE` +
             `Imp ln/100` + `Exp (billones de dólares) ln/100` +  `tipo de cambio vs dólar ln/100` +
             `Consumo Privado ln/100`,
           data = BaseDatosProyecto2,
           family = quasibinomial(link = "logit"))
summary(m62)
#BETAS DEL MODELO
#(Intercept)                              -4.909e+00
#`Deuda_P_LOG/100`                         1.078e+03 
#`Remesas ln/100`                          7.441e+01 
#`población económicamente activa ln/100` -6.107e+02 
#`VaR IGAE`                               -4.233e-02
#`Imp ln/100`                             -8.112e+02 
#`Exp (billones de dólares) ln/100`       -2.590e+01
#`tipo de cambio vs dólar ln/100`         -1.997e+02
#`Consumo Privado ln/100`                  4.407e+02

# Ajustar el modelo nulo (solo intercepto)
modelo_nulo <- glm(TDC_HSBC ~ 1, data = BaseDatosProyecto2, family = quasibinomial(link = "logit"))

# Comparar el modelo nulo con el modelo completo usando ANOVA
anova(modelo_nulo, m62, test = "Chisq")

#Obtuvimos un Pr(>Chi) de 2.2e-16 *** lo cual nos indica que el modelo completo explica mejor la probabilidad
m63 <- lm(TDC_HSBC ~ `Deuda_P_LOG/100` + `Remesas ln/100` +
             `población económicamente activa ln/100`  + `VaR IGAE` +
             `Imp ln/100` + `Exp (billones de dólares) ln/100` +  `tipo de cambio vs dólar ln/100` +
             `Consumo Privado ln/100`,
           data = BaseDatosProyecto2)
summary(m63)
#Tenemos una R2 de .84 lo cual es muy bueno y nos indica que las variables 
#explican aprox. 84% de la probabilidad de incumplimiento.


#Scotia

m7 <- glm(TDC_Scotia ~ Inflación + Desempleo + TIIE + `PIB_LOG/100` +
            `Deuda_P_LOG/100` + `Ind. Act. Industrial/100` + `Remesas ln/100` +
            `población económicamente activa ln/100` + `IGAE ln/100` + `VaR IGAE` +
            `Imp ln/100` + `Exp (billones de dólares) ln/100` + `tipo de cambio vs dólar ln/100` +
            `Consumo Privado ln/100` + `Cetes (%)` + IGREMSE + `Remuneraciones empresas constructoras` +
            `IMFBCF ln/100` + `INPC  ln/100` + `UDIS  ln/100` + `Tasa objetivo ln/100` +
            `índice nacional de precios al consumidor` + `índice de confianza al consumidor ln/100`,
          data = BaseDatosProyecto2,
          family = quasibinomial(link = "logit"))
summary(m7)
#Variables irrelevantes:
#Inflación, Desempleo, TIIE, `Ind. Act. Industrial/100`,`Remesas ln/100`,`población económicamente activa ln/100`,
#Cetes (%)`,VaR IGAE` ,IGREMSE, `Remuneraciones empresas constructoras` ,`Exp (billones de dólares) ln/100` ,
#`IMFBCF ln/100`, `INPC  ln/100`,`UDIS  ln/100`,`Tasa objetivo ln/100`,`Consumo Privado ln/100` e `índice nacional de precios al consumidor`

#Modelo ajustado
m72 <- glm(TDC_Scotia ~ `PIB_LOG/100` + `Deuda_P_LOG/100` + `IGAE ln/100` +
             `Imp ln/100`  +  `tipo de cambio vs dólar ln/100` +
             `índice de confianza al consumidor ln/100`,
           data = BaseDatosProyecto2,
           family = quasibinomial(link = "logit"))
summary(m72)
#BETAS DEL MODELO
#(Intercept)                                  12.135 
#`PIB_LOG/100`                              -395.629 
#`Deuda_P_LOG/100`                           837.419
#`IGAE ln/100`                               -42.449
#`Imp ln/100`                               -555.231
#`tipo de cambio vs dólar ln/100`            -34.683
#`índice de confianza al consumidor ln/100`  136.470 

# Ajustar el modelo nulo (solo intercepto)
modelo_nulo <- glm(TDC_Scotia ~ 1, data = BaseDatosProyecto2, family = quasibinomial(link = "logit"))

# Comparar el modelo nulo con el modelo completo usando ANOVA
anova(modelo_nulo, m72, test = "Chisq")

#Obtuvimos un Pr(>Chi) de 2.2e-16 *** lo cual nos indica que el modelo completo explica mejor la probabilidad
m73 <- lm(TDC_Scotia ~ `PIB_LOG/100` + `Deuda_P_LOG/100` + `IGAE ln/100` +
             `Imp ln/100`  +  `tipo de cambio vs dólar ln/100` +
             `índice de confianza al consumidor ln/100`,
           data = BaseDatosProyecto2)
summary(m73)
#Tenemos una R2 de .7662 lo cual es bueno y nos indica que las variables 
#explican aprox. 76% de la probabilidad de incumplimiento.


#Inbursa

m8 <- glm(TDC_Inbursa ~ Inflación + Desempleo + TIIE + `PIB_LOG/100` +
            `Deuda_P_LOG/100` + `Ind. Act. Industrial/100` + `Remesas ln/100` +
            `población económicamente activa ln/100` + `IGAE ln/100` + `VaR IGAE` +
            `Imp ln/100` + `Exp (billones de dólares) ln/100` + `tipo de cambio vs dólar ln/100` +
            `Consumo Privado ln/100` + `Cetes (%)` + IGREMSE + `Remuneraciones empresas constructoras` +
            `IMFBCF ln/100` + `INPC  ln/100` + `UDIS  ln/100` + `Tasa objetivo ln/100` +
            `índice nacional de precios al consumidor` + `índice de confianza al consumidor ln/100`,
          data = BaseDatosProyecto2,
          family = quasibinomial(link = "logit"))
summary(m8)
#Variables irrelevantes:
#Inflación, Desempleo, TIIE, `PIB_LOG/100`, `Ind. Act. Industrial/100`,
#`IGAE ln/100` ,`VaR IGAE`,`Cetes (%)` ,`Exp (billones de dólares) ln/100`, IGREMSE, `Remuneraciones empresas constructoras` ,
#`IMFBCF ln/100`, `INPC  ln/100`,`UDIS  ln/100`,`Tasa objetivo ln/100`,`índice de confianza al consumidor ln/100` e `índice nacional de precios al consumidor`

#Modelo ajustado
m82 <- glm(TDC_Inbursa ~ `Deuda_P_LOG/100` + `Remesas ln/100` +
             `población económicamente activa ln/100`  + `VaR IGAE` +
             `Imp ln/100`  +  `tipo de cambio vs dólar ln/100`,
           data = BaseDatosProyecto2,
           family = quasibinomial(link = "logit"))
summary(m82)
#BETAS DEL MODELO
#(Intercept)                              -1.696e+01 
#`Deuda_P_LOG/100`                         1.377e+03
#`Remesas ln/100`                          9.057e+01
#`población económicamente activa ln/100`  1.835e+02 
#`VaR IGAE`                               -1.520e-03
#`Imp ln/100`                             -1.116e+03
#`tipo de cambio vs dólar ln/100`         -2.430e+02


# Ajustar el modelo nulo (solo intercepto)
modelo_nulo <- glm(TDC_Inbursa ~ 1, data = BaseDatosProyecto2, family = quasibinomial(link = "logit"))

# Comparar el modelo nulo con el modelo completo usando ANOVA
anova(modelo_nulo, m82, test = "Chisq")

#Obtuvimos un Pr(>Chi) de 2.2e-16 *** lo cual nos indica que el modelo completo explica mejor la probabilidad
m83 <- lm(TDC_Inbursa ~ `Deuda_P_LOG/100` + `Remesas ln/100` +
             `población económicamente activa ln/100`  + `VaR IGAE` +
             `Imp ln/100`  +  `tipo de cambio vs dólar ln/100`,
           data = BaseDatosProyecto2)
summary(m83)
#Tenemos una R2 de .57 lo cual nos indica que las variables 
#explican aprox. 57% de la probabilidad de incumplimiento.