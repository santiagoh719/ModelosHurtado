random_walk_predict <- function(df,prediction_steps = 23){
  return(
    rep(df$NDVI[df$Fechas == max(df$Fechas)],prediction_steps)
  )
}
mean_predict <- function(df,prediction_steps = 23){
  return(
    rep(mean(df$NDVI, na.rm = T),prediction_steps)
  )
}
AR_predict <- function(df,prediction_steps = 23){
  AR_mod <- df %>% tsibble::as_tsibble(index = Fechas) %>%
    model(ARIMA(NDVI ~ pdq(,0,0) + PDQ(0,0,0))) %>%
    forecast(h = prediction_steps)
  return(AR_mod$.mean)
}
ARIMA_predict <- function(df,prediction_steps = 23){
  AR_mod <- df%>% as_tsibble(index = Fechas) %>%
    model(ARIMA(NDVI ~ pdq() + PDQ(0,0,0))) %>%
    forecast(h = prediction_steps)
  return(AR_mod$.mean)
}
ETS_predict <- function(df,prediction_steps = 23){
  AR_mod <- df %>%
    mutate(t = row_number()) %>%
    as_tsibble(index = t) %>% model(ETS(NDVI,ic = 'aic')) %>%
    forecast(h = prediction_steps)
  return(AR_mod$.mean)
}
# Multivariado Estadistico
VAR_predict <- function(df,prediction_steps = 23){
  df <- as.data.frame(df[,colnames(df) != "Fechas"])
  aux <- VARselect(df, lag.max = 23)
  var_model <- vars::VAR(df, p = aux$selection[1], type = "const")
  return( predict(var_model, n.ahead = prediction_steps)$fcst$NDVI[,1])
}
# Univariado ML
NNETAR_predict <- function(df,prediction_steps = 23){
  AR_mod <- df %>%
    mutate(t = row_number()) %>%
    as_tsibble(index = t) %>%
    model(NNETAR(NDVI ~ AR(p = 13, P = 1, period = 23),
                 n_networks = 10)) %>%
    forecast(h = prediction_steps)
  return(AR_mod$.mean)
}
