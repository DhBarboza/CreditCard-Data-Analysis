### Fraud detection in purchases made by credit card ###

# 1º STEP = Collection, Obtaining or Importing Data:
# In your RStudio Environment:
# - Import DataSet
# - From text (readr)
# - Select file
# - set in tab separator 
# - import code:
    library(readr)
    data_creditcard <- read_delim("C:Users/Douglas Henrique/Desktop/Github/CreditCard-Data-Analysis/data/creditcard_alterado.txt", 
                                      "\t", escape_double = FALSE, trim_ws = TRUE)

# 2º STEP = Data Analysis and Exploration:
# Dimension of the data:
    dim(data_creditcard)
    
    # First six lines:
    head(data_creditcard)
    
    # Last six lines:
    tail(data_creditcard)
    
    # names columns:
    names(data_creditcard)
    
    # Frequency of some data:
    table(data_creditcard$Class)
    
    summary(data_creditcard$Class)
    summary(data_creditcard$Valor)
    
    # Staandart deviation Calculatation:
    sd(data_creditcard$Valor)

# 3º STEP = Data Handling:
# standardize variables:
    data_creditcard$Valor <- c(scale(data_creditcard$Valor))
    
    data_creditcard$Valor

# 4º STEP = Data Discrimination:
#     - 4/5 = Training data
#     - 1/5 = Test Model
    library(caTools)
    data_sample = sample.split(data_creditcard$Class, SplitRatio = 0.80)
    
    # Separator data:
    data_training = subset(data_creditcard, data_sample == TRUE)
    data_test = subset(data_creditcard, data_sample == FALSE)
    
    data_sample
    
    dim(data_training)
    dim(data_test)
    
    # Frequency of the classification variable
    table(data_training$Class)
    table(data_test$Class)

# 5º STEP = Configurate Model
    # Logistic Regression Model: for binary response
    model_losgistic <- glm(Class ~., data_training, family = binomial)
    
    summary(model_losgistic)

# 6º STEP = Present the Data:
    library(magrittr)
    probability <- model_losgistic %>% predict(data_test, type = "response")
    
    probability
    
    # "1" = fraud
    # "0" = Correct 
    forecast.rule <- ifelse(probability > 0.5, "1", "0")
    
    forecast.rule
    
    mean(forecast.rule == data_test$Class)
    
    table(forecast.rule, data_test$Class)
    
# Config:
    library(gdata)
    gdata::keep(model_losgistic, sure = T)
    save.image(".RData")
    
    
    
    
    
    