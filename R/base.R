#' @import dplyr
#' @importFrom lubridate floor_date ceiling_date
#' @importFrom stringi stri_trans_general
#' @importFrom stringr str_squish str_to_upper str_to_lower str_to_title str_to_sentence
#' @export
NULL

#' Barra de progresso impressa no console
#'
#' @param x Valor numérico da posição na barra de progresso, de 1 a 100. 
#' @param msg Mensagem de saída
#' @param bar Caracter símbolo da barra de progresso
#' @param time Inclui a hora atual se 'TRUE'
#' 
#' @return Barra de progresso.
#' 
#' @export
pp_progress <- function(x, msg = "", bar = "-", time = F){
  base::message()
  base::message(paste0(paste0(c("|", rep(bar, x), rep(" ", 100-x),"| ", x, "%", ifelse(time, paste0(" [",format(Sys.time(), "%X"),"]"), ""))), sep = ""))
  base::message(msg)
}

#' Último dia de determinada referência mensal ou anual
#'
#' @param x Data de referência no formato 'yyyy-mm-dd'
#' @param by Unidade da função 'round_date' do pacote lubridate, 
#' podendo ser 'day', 'week', 'month' ou 'year'
#' @param week_start Início da semana. Variável a ser utilizada
#' caso o parâmetro 'by' seja igual a 'month'
#'
#' @return Último dia de determinada referência mensal ou anual
#' 
#' @export
pp_last_day <- function(x, by = "month", week_start = 1){
  ceiling_date(
    x = as.Date(x), 
    unit = by,
    week_start = week_start)-1
}

#' Intervalo entre datas
#'
#' @param start Data de início do intervalo, no formato 'yyyy-mm-dd'
#' @param end Data fim do intervalo, no formato 'yyyy-mm-dd'
#' @param by Unidade da função 'round_date' do pacote lubridate, 
#' podendo ser 'day', 'week', 'month' ou 'year'
#' @param position Caso preenchido, retorna o primeiro 'first' ou
#' o último 'last' dia do intervalo para os parâmetros de 'by' 
#' 'month' ou 'year'
#'
#' @return Intervalo entre datas
#' 
#' @export
pp_interval <- function(start, end = Sys.Date(), by = "day", position = "first"){
  switch(position,
         first = unique(
           floor_date(
             x = unique(seq(from = as.Date(start), to = as.Date(end), by = 1)), 
             unit = by)),
         last = unique(
           ceiling_date(
             x = unique(seq(from = as.Date(start), to = as.Date(end), by = 1)), 
             unit = by)-1),
         stop(""))
}

#' Importa e lista objetos de determinado arquivo '.rda'
#'
#' @param x Arquivo de extensão '.rda' para importação dos dados.
#' Aceita caminho completo.
#'
#' @return Arquivos '.rda' em formato de lista
#' 
#' @export
pp_list_rda <- function(x) {
  e <- new.env()
  load(file = x, envir = e)
  as.list(e)
}

#' Calculo percentual
#'
#' @param df Objeto do tipo 'data.frame', o que requer colunas para os 
#' parâmetros 'num' e 'den'. Pode ser dispensado caso os parâmetros 
#' 'num' e 'den' sejam vetores.
#' @param num Numerador da fração, podendo ser uma ou mais colunas de classe 
#' numérica de determinado 'data.frame' ou vetor de classe numérica, caso o 
#' parâmetro 'df' não seja informado. 
#' @param den Denominador da fração, podendo uma coluna de classe 
#' numérica de determinado 'data.frame' ou vetor de classe numérica, caso
#' o parâmetro 'df' não seja informado.
#' @param nsmall Número de casas decimais após a vírgula, correspondente ao
#' parâmetro 'nsmall' da função 'base::format'
#' @param format Opção verdadeira 'TRUE' ou falsa 'FALSE', caso necessária
#' a formatação do resultado final da fração
#' 
#' @return Vetor 'vector' ou coluna 'column' do tipo numérica ou textual
#' 
#' @export
pp_percent <- function(df, num=NULL, den=NULL, nsmall=1, format=FALSE, diff=FALSE){
  if(!is.null(num) & !is.null(den)){
    if(missing(df)){
      df <- (num/den)*100
      if(diff) df <- df-100
      df <- ifelse(is.infinite(df) | is.na(df), 0, df)
      df <- round(df, digits = nsmall)
      if(format) df <- paste0(format(x = df, digits = 0, nsmall = nsmall, big.mark = ".", decimal.mark = ",", scientific = FALSE), "%")
      } else {
        columns <- paste0(num,"_PER")
        for(i in seq_along(columns)){
          df[,columns[i]] <- (df[,num[i]]/df[,den])*100
          df <- df %>% 
            mutate_at(.vars = columns[i], .funs = function(x) ifelse(is.infinite(x) | is.na(x), 0, x))
          if(diff) df[,columns[i]] <- df[,columns[i]]-100
          df[,columns[i]] <- round(df[,columns[i]], digits = nsmall)
          if(format){
            df <- df %>% 
              mutate_at(.vars = columns[i], .funs = function(x) paste0(format(x = x, digits = 0, nsmall = nsmall, big.mark = ".", decimal.mark = ",", scientific = FALSE), "%"))
          }
          }
        }
    } else {
      stop("Necessário um numerador e um denominador")
      }
  return(df)
}

#' Soma total de determinada coluna de um 'data.frame'
#'
#' @param df Objeto do tipo 'data.frame'.
#' @param fixed Colunas do 'data.frame' fixas, a serem desconsideradas
#' em determinado cálculo somatório do total.
#' 
#' @return Soma total por colunas de um 'data.frame'
#' 
#' @export
pp_total_fixed <- function(df, fixed){
  for(i in seq_along(fixed)){
    df[, c(names(fixed[i]))] <- as.character(df[, c(names(fixed[i]))])
    df[, c(names(fixed[i]))] <- as.character(fixed[i][[1]])}
  df <- df %>% 
    group_by_at(.vars = c(names(fixed))) %>% 
    summarise_all(.funs = sum, na.rm=TRUE)
  return(df)
}

#' Formatação padrão de uma 'string' para tratamento de dados
#'
#' @param x Fragmento de texto.
#' @param enc Codificação de texto padrão.
#' @param sep Separador a ser utilizado em substituição ao espaço ' '.
#' @param str_to Formatação final compatível com as funções
#' do pacote 'stringr': 
#' - upper (str_to_upper) para tornar todas as letras maiúsculas
#' - lower (str_to_lower) para tornar todas as letras minúsculas
#' - sentence (str_to_sentence) para a primeira letra maiúscula e as 
#' demais minúsculas
#' - title (str_to_title) tornar maiúscia a primeira letra de cada 
#' palavra que compôem o fragmento de texto 
#' 
#' @return Fragmento de texto tratado.
#' 
#' @export

pp_format_str <- function(x, enc = "Latin-ASCII", sep = "_", str_to = "upper"){
  x <- stri_trans_general(str = x, id = enc)
  x <- gsub(pattern = "[^[:alnum:] ]", replacement = " ", x = x)
  x <- str_squish(x)
  x <- gsub(" ", sep, x)
  x <- switch(str_to,
         upper = str_to_upper(x),
         lower = str_to_lower(x),
         sentence = str_to_sentence(x),
         title = str_to_title(x),
         stop(""))
  return(x)
}

#' Conversão de mês numérico do TG para sigla do mês
#'
#' @param x Mês ou meses em formato numérico (1, 2, 3, 01, 02, 03).
#' 
#' @return Mês ou meses no formato texto (Jan, Fev, Mar, Jan, Fev, Mar)
#' 
#' @export
pp_format_month <- function(x){
  case_when(
    x == "1" | x == "01" ~ "Jan",
    x == "2" | x == "02" ~ "Fev",
    x == "3" | x == "03" ~ "Mar",
    x == "4" | x == "04" ~ "Abr",
    x == "5" | x == "05" ~ "Mai",
    x == "6" | x == "06" ~ "Jun",
    x == "7" | x == "07" ~ "Jul",
    x == "8" | x == "08" ~ "Ago",
    x == "9" | x == "09" ~ "Set",
    x == "10" ~ "Out",
    x == "11" ~ "Nov",
    x == "12" ~ "Dez",
    TRUE ~ as.character(x))
}
