install.packages("pageviews")
install.packages("simts")
install.packages("stringr")
install.packages("fpp2")
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

Xxt <- article_pageviews(article = "Manchester_United", end = "2019120100")$views 
View(article_pageviews(article = "Manchester_United", end = "2019120100")) 
View(Xxt)
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
  library(fpp2)
  # Format date
  date <- paste0(str_remove_all(date, "-"), "00")
  # Retrieve time series
  Xt <- gts(article_pageviews(article = "Manchester_United", end = date)$views)
  # Use Simple Exponential Smoothing to forecast
  forecast <- predict(estimate(ARMA(7,6), Xt), n.ahead=7)
  # Create output list
  output <- list("point_forecast" = as.vector(forecast$pred), "forecast_pred.int" =cbind(as.vector(forecast$CI0.95[,1]), as.vector(forecast$CI0.95[,2])))
  # Return forecast object
  return(output)}
client_function_manu(20191201)
client_function_manu(20190301)
client_function_manu(20190404)
client_function_manu(20191209)

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
  return(output) }
client_function_liverpool(yyyymmdd)
