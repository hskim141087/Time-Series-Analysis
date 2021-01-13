install.packages("pageviews")
install.packages("simts")
install.packages("stringr")
install.packages("fpp2")

Xxt <- article_pageviews(article = "Manchester_United", end = "2019120100")$views 
#View(article_pageviews(article = "Manchester_United", end = "2019120100")) 
#View(Xxt)
mol <- ts(Xxt)
plot(ts(Xxt))
check(mol,simple = TRUE)
corr_analysis(Xxt)
select(ARMA(7,7),Xxt)
#model_copper_ar1 = estimate(AR(1), Xt)
#check(model_copper_ar1)
model1.1 <- estimate(AR(6),Xxt)
check(model1.1)
model2.1 <- estimate(ARMA(3,1),Xxt)
check(model2.1)
model3.1 <- estimate(ARMA(7,6),Xxt)
check(model3.1)
client_function_manu <- function(date = Sys.Date()) { 
  # Load required libraries
  library(pageviews)
  library(simts)
  library(stringr)
  # Format date
  date <- paste0(str_remove_all(date, "-"), "00")
  # Retrieve time series
  Xt <- gts(article_pageviews(article = "Manchester_United", end = date)$views)
  forecast <- predict(estimate(ARMA(7,6), Xt), n.ahead=7)
  # Create output list
  output <- list("point_forecast" = as.vector(forecast$pred), "forecast_pred.int" =cbind(as.vector(forecast$CI0.95[,1]), as.vector(forecast$CI0.95[,2])))
  # Return forecast object 
  return(output)} 
client_function_manu(20191206)



Xt <- article_pageviews(article = "Liverpool", end = "2019120100")$views
mol <- ts(Xt)
plot(ts(Xt))
check(mol,simple = TRUE)
corr_analysis(Xt)
#select(ARMA(5,5),Xt)
#evaluate(list(AR(1),ARMA(2,1),ARMA(3,3)), Xt, criterion = "MAPE", start = 0.5) #model_copper_ar1 = estimate(AR(1), Xt)
#check(model_copper_ar1)
model1 <- estimate(AR(1),Xt)
check(model1)
model2 <- estimate(ARMA(2,1),Xt)
check(model2)
model3 <- estimate(ARMA(3,3),Xt)
check(model3)
client_function_liverpool <- function(date = Sys.Date()) { # Load required libraries
  library(pageviews)
  library(simts)
  library(stringr)
  # Format date
  date <- paste0(str_remove_all(date, "-"), "00")
  # Retrieve time series
  Xt <- gts(article_pageviews(article = "Liverpool", end = date)$views)
  forecast <- predict(estimate(ARMA(2,1), Xt), n.ahead=7)
  # Create output list
  output <- list("point_forecast" = as.vector(forecast$pred), "forecast_pred.int" =cbind(as.vector(forecast$CI0.95[,1]), as.vector(forecast$CI0.95[,2])))
  # Return forecast object
  return(output) 
  }
client_function_liverpool(20191206)


Xt <- article_pageviews(article = "Real_Madrid", end = "2019120100")$views mol <- ts(Xt)
View(Xt)
plot(ts(Xt))
check(mol,simple = TRUE)
corr_analysis(Xt)
select(ARMA(5,5),Xt)
#evaluate(list(AR(1),ARMA(2,1),ARMA(3,3)), Xt, criterion = "MAPE", start = 0.5) #model_copper_ar1 = estimate(AR(1), Xt)
#check(model_copper_ar1)
model1 <- estimate(ARMA(4,4),Xt)
check(model1)
model2 <- estimate(ARMA(3,3),Xt)
check(model2)
model3 <- estimate(ARMA(3,4),Xt)
check(model3)
client_function_rm <- function(date = Sys.Date()) { # Load required libraries
  library(pageviews)
  library(simts)
  library(stringr)
  # Format date
  date <- paste0(str_remove_all(date, "-"), "00")
  # Retrieve time series
  Xt <- gts(article_pageviews(article = "Real_Madrid", end = date)$views)
  forecast <- predict(estimate(ARMA(4,4), Xt), n.ahead=7)
  # Create output list
  output <- list("point_forecast" = as.vector(forecast$pred), "forecast_pred.int" =cbind(as.vector(forecast$CI0.95[,1]), as.vector(forecast$CI0.95[,2])))
  # Return forecast object
  return(output)
}
 client_function_rm(20191206)
