#' @import dplyr
#' @importFrom lubridate floor_date ceiling_date
#' @importFrom stringi stri_trans_general
#' @importFrom stringr str_squish str_to_upper str_to_lower str_to_title str_to_sentence
#' @export
NULL

#' Barra de progresso impressa no console
#'
#' @param x Valor num<c3><a9>rico da posi<c3><a7><c3><a3>o na barra de progresso, de 1 a 100. 
#' @param msg Mensagem de sa<c3><ad>da
#' @param bar bar Caracter s<c3><ad>mbolo da barra de progresso
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

#' Barra de progresso impressa no console
#'
#' @param etapa Valor num<c3><a9>rico da etapa
#' @param total Valor num<c3><a9>rico do total
#' @param label Texto a ser exibido na mensagem de progresso
#' 
#' @return Barra de progresso.
#' 
#' @export
pp_progress2 <- function(etapa, total, label = "Progresso") {
  percent <- round((etapa / total) * 100)
  barra_len <- 20
  preenchido <- round((etapa / total) * barra_len)
  vazio <- barra_len - preenchido
  
  barra <- paste0(replicate(preenchido, "="), collapse = "")
  espaco <- paste0(replicate(vazio, "-"), collapse = "")
  
  timestamp <- format(Sys.time(), "%H:%M:%S")
  cat(sprintf("[%s] [%s%s] %d%% - %s\n", 
              timestamp, barra, espaco, percent, label))
}

#' <c3><9a>ltimo dia de determinada refer<c3><aa>ncia mensal ou anual
#'
#' @param x Data de refer<c3><aa>ncia no formato 'yyyy-mm-dd'
#' @param by Unidade da fun<c3><a7><c3><a3>o 'round_date' do pacote lubridate, 
#' podendo ser 'day', 'week', 'month' ou 'year'
#' @param week_start In<c3><ad>cio da semana. Vari<c3><a1>vel a ser utilizada
#' caso o par<c3><a2>metro 'by' seja igual a 'month'
#'
#' @return <c3><9a>ltimo dia de determinada refer<c3><aa>ncia mensal ou anual
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
#' @param start Data de in<c3><ad>cio do intervalo, no formato 'yyyy-mm-dd'
#' @param end Data fim do intervalo, no formato 'yyyy-mm-dd'
#' @param by Unidade da fun<c3><a7><c3><a3>o 'round_date' do pacote lubridate,
#' podendo ser 'day', 'week', 'month' ou 'year'
#' @param position Caso preenchido, retorna o primeiro 'first' ou
#' o <c3><ba>ltimo 'last' dia do intervalo para os par<c3><a2>metros de 'by'
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
#' @param x Arquivo de extens<c3><a3>o '.rda' para importa<c3><a7><c3><a3>o dos dados.
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
#' par<c3><a2>metros 'num' e 'den'. Pode ser dispensado caso os par<c3><a2>metros 
#' 'num' e 'den' sejam vetores.
#' @param num Numerador da fra<c3><a7><c3><a3>o, podendo ser uma ou mais colunas de classe 
#' num<c3><a9>rica de determinado 'data.frame' ou vetor de classe num<c3><a9>rica, caso o 
#' par<c3><a2>metro 'df' n<c3><a3>o seja informado. 
#' @param den Denominador da fra<c3><a7><c3><a3>o, podendo uma coluna de classe 
#' num<c3><a9>rica de determinado 'data.frame' ou vetor de classe num<c3><a9>rica, caso
#' o par<c3><a2>metro 'df' n<c3><a3>o seja informado.
#' @param nsmall N<c3><ba>mero de casas decimais ap<c3><b3>s a v<c3><ad>rgula, correspondente ao
#' par<c3><a2>metro 'nsmall' da fun<c3><a7><c3><a3>o 'base::format'
#' @param format Op<c3><a7><c3><a3>o verdadeira 'TRUE' ou falsa 'FALSE', caso necess<c3><a1>ria
#' a formata<c3><a7><c3><a3>o do resultado final da fra<c3><a7><c3><a3>o
#' @param diff Se 'T' subtrai por 100
#' 
#' @return Vetor 'vector' ou coluna 'column' do tipo num<c3><a9>rica ou textual
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
      stop("Necess<c3><a1>rio um numerador e um denominador")
      }
  return(df)
}

#' Soma total de determinada coluna de um 'data.frame'
#'
#' @param df Objeto do tipo 'data.frame'.
#' @param fixed Colunas do 'data.frame' fixas, a serem desconsideradas
#' em determinado c<c3><a1>lculo somat<c3><b3>rio do total.
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

#' Formata<c3><a7><c3><a3>o padr<c3><a3>o de uma 'string' para tratamento de dados
#'
#' @param x Fragmento de texto.
#' @param enc Codifica<c3><a7><c3><a3>o de texto padr<c3><a3>o.
#' @param sep Separador a ser utilizado em substitui<c3><a7><c3><a3>o ao espa<c3><a7>o ' '.
#' @param str_to Formata<c3><a7><c3><a3>o final compat<c3><ad>vel com as fun<c3><a7><c3><b5>es
#' do pacote 'stringr': 
#' - upper (str_to_upper) para tornar todas as letras mai<c3><ba>sculas
#' - lower (str_to_lower) para tornar todas as letras min<c3><ba>sculas
#' - sentence (str_to_sentence) para a primeira letra mai<c3><ba>scula e as 
#' demais min<c3><ba>sculas
#' - title (str_to_title) tornar mai<c3><ba>scia a primeira letra de cada 
#' palavra que comp<c3><b4>em o fragmento de texto
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

#' Convers<c3><a3>o de m<c3><aa>s num<c3><a9>rico do TG para sigla do m<c3><aa>s
#'
#' @param x M<c3><aa>s ou meses em formato num<c3><a9>rico (1, 2, 3, 01, 02, 03).
#' 
#' @return M<c3><aa>s ou meses no formato texto (Jan, Fev, Mar, Jan, Fev, Mar)
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
