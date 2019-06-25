






glmFit <- train(sales ~ .,
              trainData,
              method = "glm")

glmFit <- train(sales ~ . 
                -inc0_10
                - inc10_14
                - inc14_20
                - inc20_30
                - inc30_50
                - inc50_100
                - sch12Plus
                - percentHardGoods
                - aircond
                - dishw
                - secHome
                - washers
                - car1
                - inc100Plus
                - sch12
                - tvs
                - owners
                - nocars
                - medianInc
                - medianRent
                - medianHome
                - black
                - dryers,
                rawData,
                method = "glm")

glmFit
summary(glmFit)





cor(rawData %>% select(-comType))[,'inc0_10']

gbmImp <- varImp(glmFit, scale = FALSE)


trees <- train(sales ~ .,
               rawData,
              method = "cforest")

varImp(trees[[11]]) %$% 
  tibble(Names = rownames(.), Overall = .[["Overall"]]) %>%
  filter(substr(Names, 1, 7) != "comType") %>% 
  arrange(Overall %>% desc()) %>% 
  slice(1:10) %>% 
  pull(Names) -> vars

b <- paste(c(vars), collapse ="+")
test <- lm(paste("sales ~ ",b,sep = ""), data = rawData)
test <- lm(sales ~ . - comType, data= rawData)
summary(test)



featurePlot(rawData[vars], rawData[['sales']], 
            plot = 'scatter')








