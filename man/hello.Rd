\name{Modelos}
\alias{random_walk_predict}
\alias{mean_predict}
\alias{AR_predict}
\alias{ARIMA_predict}
\alias{ETS_predict}
\alias{VAR_predict}
\alias{NNETAR_predict}
\title{Muchos Filtros}
\usage{
random_walk_predict(df,prediction_steps = 23)
mean_predict(df,prediction_steps = 23)
AR_predict(df,prediction_steps = 23)
ARIMA_predict(df,prediction_steps = 23)
ETS_predict(df,prediction_steps = 23)
VAR_predict(df,prediction_steps = 23)
NNETAR_predict(df,prediction_steps = 23)

}
\arguments{
  \item{df}{dataframe, columna NDVI se usa a predecir}
  \item{prediction_steps}{Integer, cantidad de tiempos futuros a predecir}

}
\description{
Ajusta el modelo al df, y predice los siguientes pasos futuros
}
