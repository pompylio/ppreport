#' @import dplyr
#' @importFrom readxl read_xlsx
#' @importFrom tidyr pivot_longer pivot_wider replace_na
#' @export
NULL

utils::globalVariables(
  c("CREDITO_DISPONIVEL", "CO_PLANO_INTERNO", "DISPONIVEL", "INDISPONIVEL","EMPENHADO", ".", "where"))

#' Tratamento da base de dados
#'
#' @param path Caminho do arquivo base no formato 'xlsx' 
#' @param database Base de dados correspondente ao arquivo 'xlsx', podendo ser:
#' - tg_nota_credito
#' - tg_nota_dotacao
#' - tg_nota_empenho
#' - tg_orcamento
#' - tg_receita_propria
#' - sp_pessoal
#'
#' @return Dado tratado
#' 
#' @export
pp_database <- function(path, database){
  tb_data <- tb_data[tb_data$file==database,]
  tb_title <- suppressMessages(
    read_xlsx(
      path = path, 
      n_max = max(tb_data$line_title), 
      col_types = "text",
      col_names = FALSE, 
      progress = FALSE))
  tb_title_new <- as.character()
  for(i in 1:nrow(tb_title)){
    x <- as.character(tb_title[i,])
    x <- x[x%in%tb_data$colnames]
    tb_title_new <- append(tb_title_new, x)
  }
  tb_title_missing <- tb_data[!tb_data$colnames%in%tb_title_new,]
  tb_title_valid <- tb_data[tb_data$colnames%in%tb_title_new,]
  df <- suppressMessages(
    read_xlsx(
      path = path, 
      col_names = tb_title_valid$colnames_new, 
      col_types = tb_title_valid$type,
      skip = max(tb_data$skip), 
    progress = FALSE))
  if(!ncol(tb_title)==nrow(tb_data)){
    for(i in 1:nrow(tb_title_missing)){
      df[,tb_title_missing$colnames_new[i]] <- ifelse(tb_title_missing$type[i]=="numeric",0,"")
    }
    df <- df %>% 
      select_at(.vars = tb_data$colnames_new)
  }
  df <- df %>% 
    mutate(across(where(is.numeric), ~replace_na(., 0))) %>% 
    mutate(across(where(is.character), ~replace_na(., ""))) %>% 
    mutate(across(where(is.character), ~ifelse(. %in% c("'-7", "'-8","'-9", "SEM INFORMACAO", "NAO SE APLICA", "CODIGO INVALIDO"),"",.)))
  if(any(tb_data$colnames_new=="EMISSAO"))
    df$EMISSAO <- as.Date(df$EMISSAO,"%d/%m/%Y")
  if(any(tb_data$colnames_new=="MES"))
    df$MES <- ifelse(nchar(df$MES)==1,paste0("0",df$MES),df$MES)
  if(any(tb_data$colnames_new=="CO_UGR")){
    df$RE_UGR <- ""
    if(!any(tb_data$colnames_new=="UGR"))
      df$UGR <- ""
    for(i in 1:nrow(tb_ug)){
      df[df$CO_UGR == tb_ug$co_ug[i], ]$UGR <- tb_ug$ug[i]
      df[df$CO_UGR == tb_ug$co_ug[i], ]$RE_UGR <- tb_ug$re_ug[i]
    }
  }
  if(any(tb_data$colnames_new=="CO_UGE")){
    df$RE_UGE <- ""
    if(!any(tb_data$colnames_new=="UGE"))
      df$UGE <- ""
    for(i in 1:nrow(tb_ug)){
      df[df$CO_UGE == tb_ug$co_ug[i],]$UGE <- tb_ug$ug[i]
      df[df$CO_UGE == tb_ug$co_ug[i],]$RE_UGE <- tb_ug$re_ug[i]
    }
  }
  if(any(tb_data$colnames_new=="UG_EMITENTE")){
    df$RE_EMITENTE <- ""
    for(i in 1:nrow(tb_ug)){
      df[df$UG_EMITENTE == tb_ug$co_ug[i],]$EMITENTE <- tb_ug$ug[i]
      df[df$UG_EMITENTE == tb_ug$co_ug[i],]$RE_EMITENTE <- tb_ug$re_ug[i]
    }
  }
  if(any(tb_data$colnames_new=="CO_FAVORECIDO")){
    df$RE_FAVORECIDO <- ""
    for(i in 1:nrow(tb_ug)){
      df[df$CO_FAVORECIDO == tb_ug$co_ug[i],]$FAVORECIDO <- tb_ug$ug[i]
      df[df$CO_FAVORECIDO == tb_ug$co_ug[i],]$RE_FAVORECIDO <- tb_ug$re_ug[i]
    }
  }
  if(any(tb_data$colnames_new=="CO_GRUPO") &
     any(tb_data$colnames_new=="GRUPO")){
    for(i in 1:nrow(tb_gnd)){
      df[df$CO_GRUPO == tb_gnd$co_grupo[i],]$GRUPO <- tb_gnd$grupo[i]
    }
  }
  if(any(tb_data$colnames_new=="CO_PLANO_INTERNO") & 
     any(tb_data$colnames_new=="CO_UO") &
     any(tb_data$colnames_new=="CO_ACAO")){
    df$AREA <- ifelse(is.na(df$CO_PLANO_INTERNO), "",
               ifelse(df$CO_UO == "26428" & substr(df$CO_PLANO_INTERNO, 2, 5) %in% c("-8", "-9", ""), "",
               ifelse(df$CO_UO == "26428" & substr(df$CO_PLANO_INTERNO, 2, 5) %in% c("IFB", "GERA"), "GERAL",
               ifelse(df$CO_UO == "26428" & substr(df$CO_PLANO_INTERNO, 2, 5) %in% c("GADM", "REIT", "ADMI"), "ADMINISTRACAO",
               ifelse(df$CO_UO == "26428" & substr(df$CO_PLANO_INTERNO, 2, 8) %in% c("ENSINO", "GEPEP19", "GEPEP22"), "ENSINO",
               ifelse(df$CO_UO == "26428" & substr(df$CO_PLANO_INTERNO, 2, 8) %in% c("GEPEP23") & df$CO_ACAO == "2994", "ASSISTENCIA",
               ifelse(df$CO_UO == "26428" & substr(df$CO_PLANO_INTERNO, 2, 8) %in% c("EXTENSA","GEPEP21"), "EXTENSAO",
               ifelse(df$CO_UO == "26428" & substr(df$CO_PLANO_INTERNO, 2, 8) %in% c("PESQUIS","INOVACA", "GEPEP20"), "PESQUISA E INOVACAO",
               ifelse(df$CO_UO == "26428" & substr(df$CO_PLANO_INTERNO, 2, 5) %in% c("CAPA", "GPES"), "CAPACITACAO",
               ifelse(df$CO_UO == "26428" & substr(df$CO_PLANO_INTERNO, 2, 5) %in% c("PPES"), "FOLHA", ""))))))))))
  }
  if(database=="tg_nota_credito"){
    df$CO_UGR <- ifelse(df$GESTAO_EMITENTE == "26428" & !df$UG_EMITENTE == "158143" &  df$CO_FAVORECIDO == "158143" & df$CO_UGR == "'-8", df$UG_EMITENTE,
                 ifelse(df$GESTAO_EMITENTE == "26428" &  df$UG_EMITENTE == "158143" & !df$CO_FAVORECIDO == "158143" & df$CO_UGR == "'-8", df$CO_FAVORECIDO, df$CO_UGR))
    df$CATEGORIA <- ifelse(!df$GESTAO_EMITENTE == "26428", "EXTERNO",
                    ifelse( df$GESTAO_EMITENTE == "26428" & !df$CO_FAVORECIDO %in% unique(tb_ug$co_ug), "EXTERNO",
                    ifelse( df$GESTAO_EMITENTE == "26428" &  df$UG_EMITENTE == "158143" & df$CO_FAVORECIDO == df$CO_UGR, "DISTRIBUICAO",
                    ifelse( df$GESTAO_EMITENTE == "26428" &  df$CO_FAVORECIDO == "158143" & df$UG_EMITENTE == df$CO_UGR, "DISTRIBUICAO",
                    ifelse( df$GESTAO_EMITENTE == "26428" & !df$UG_EMITENTE == "158143" & !df$CO_FAVORECIDO == "151843", "MOVIMENTACAO",
                    ifelse( df$GESTAO_EMITENTE == "26428" &  df$CO_UGR == "158143", "MOVIMENTACAO", "OUTRA"))))))
    df$TIPO <- ifelse(!df$GESTAO_EMITENTE == "26428" & df$CO_EVENTO %in% c("300300","300301","300303","300306","300316"), "ENTRADA",
               ifelse(!df$GESTAO_EMITENTE == "26428" & df$CO_EVENTO %in% c("300302","300318"), "SAIDA",
               ifelse(!df$CO_FAVORECIDO %in% unique(tb_ug$co_ug) & df$CO_EVENTO %in% c("300300","300301","300303","300304","300306","300307","300316","300317"), "SAIDA",
               ifelse(!df$CO_FAVORECIDO %in% unique(tb_ug$co_ug) & df$CO_EVENTO %in% c("300302","300318"), "ENTRADA",
               ifelse( df$UG_EMITENTE == "158143" & df$CO_EVENTO %in% c("300063"), "SAIDA",
               ifelse( df$UG_EMITENTE == "158143" & df$CO_EVENTO %in% c("300083","300089"), "ENTRADA",
               ifelse( df$CO_FAVORECIDO == "158143" & df$CO_EVENTO %in% c("300063", "300084"), "ENTRADA",
               ifelse( df$CO_FAVORECIDO == "158143" & df$CO_EVENTO %in% c("300083","305063"), "SAIDA", "OUTRA"))))))))
    df$VALOR_RIFB = ifelse(df$TIPO == "SAIDA", df$VALOR*-1, 
                    ifelse(df$TIPO == "OUTRA", 0, df$VALOR))
    df$VALOR_CAMPI = ifelse(df$TIPO == "ENTRADA", df$VALOR*-1,
                     ifelse(df$TIPO == "OUTRA", 0,df$VALOR))
  }
  df <- df %>% relocate(where(is.numeric), .after = where(is.character))
  return(df)
}

#' Soma e agrupamento de variáveis
#'
#' @param df 'data.frame'
#' @param vars_group Variáveis do 'data.frame' a serem agrupadas
#' @param vars_sum Variáveis do 'data.frame' a serem somadas
#'
#' @return 'data.frame' agrupado
#' 
#' @export
pp_group_sum <- function(df, vars_group, vars_sum){
  col_names <- c(vars_group, vars_sum)
  df <- df %>% 
    group_by_at(.vars = vars_group) %>% 
    summarise_at(.vars = vars_sum, .funs = sum, na.rm=TRUE)
  for(i in 1:length(col_names)){
    if(!any(colnames(df)==col_names[i])) df[,col_names[i]] <- 0
  }
  df <- df %>% 
    mutate(across(where(is.numeric), ~replace_na(., 0))) %>% 
    mutate(across(where(is.character), ~replace_na(., "-")))
  vars_df <- sapply(df, class)
  vars_df <- names(vars_df[vars_df=="numeric"])
  df <- df %>% 
    filter_at(all_of(vars_df), any_vars(. != 0))
  return(df)
}

#' Extração de dados a partir de referenciais padronizados do orçamento
#'
#' @param df 'data.frame' da base de dados 'tg_orcamento'
#' @param area Área gestora do orçamento, conforme coluna AREA criada pela
#' função 'pp_database' para a base de dados 'tg_orcamento'
#' @param co_uge Código da Unidade Gestora Executora (UGE)
#' @param co_ugr Código da Unidade Gestora Responsável (UGR)
#' @param co_ug_by Filtrar UGE e/ou UGR
#' @param rap Desconsidera restos a pagar (RAP) se 'FALSE'
#'
#' @return 'data.frame' do orçamento
#' 
#' @export
#' 
pp_orcamento <- function(df, area=NULL, co_uge=NULL, co_ugr=NULL, co_ug_by="ou",  rap=T){
  if(!is.null(co_uge) & !is.null(co_ugr)){
    if(co_ug_by=="e")
      df <- df[df$CO_UGE %in% co_uge & df$CO_UGE %in% co_ugr,]
    if(co_ug_by=="ou")
      df <- df[df$CO_UGE %in% co_uge | df$CO_UGE %in% co_ugr,]
  }
  if(!is.null(co_uge) & is.null(co_ugr))
    df <- df[df$CO_UGE %in% co_uge,]
  if(is.null(co_uge) & !is.null(co_ugr))
    df <- df[df$CO_UGR %in% co_ugr,]
  if(!missing(area)){
    df <- df[df$AREA %in% area,] 
  }
  df <- df %>% 
    mutate(across(where(is.numeric), ~replace_na(., 0)))
  if(!rap){
    if(any(colnames(df)=="RAP_INSCRITO")){
      df <- df[df$RAP_INSCRITO==0,]
    }
    if(any(colnames(df)=="RAP_PAGO")){
      df <- df[df$RAP_PAGO==0,]
    }
  }
  return(df)
}

#' Gera dados de execução orçamentária e financeira
#'
#' @param df 'data.frame' da base de dados 'tg_orcamento'
#' @param vars_group Variáveis do 'data.frame' a serem agrupadas
#' @param available Considera detalhamento do 'CREDITO_DISPONIVEL' se 'TRUE'
#' @param unselect Variáveis do 'data.frame' a serem desconsideradas
#'
#' @return 'data.frame' com a execução orçamentária e financeira
#' 
#' @export
#' 
pp_execucao <- function(df, vars_group, available=F, unselect=NULL){
  vars_sum <- c("CREDITO_INDISPONIVEL", "CREDITO_DISPONIVEL", "EMPENHADO", "LIQUIDADO", "PAGO")
  if(!is.null(unselect)){
    unselect <- gsub(pattern = "CREDITO_", replacement = "", x = unselect, fixed = TRUE)
  }
  if(available){
    vars_num <- c("DOTACAO","INDISPONIVEL","DISPONIVEL", "DISPONIVEL_CND", "DISPONIVEL_SND", 
                  "EMPENHADO", "LIQUIDADO", "PAGO")
    df <- left_join(
      df %>% 
        group_by_at(.vars = c(vars_group, "CO_PLANO_INTERNO")) %>%
        summarise(VALOR = sum(CREDITO_DISPONIVEL, na.rm=TRUE)) %>% 
        mutate(TIPO = ifelse(
          substr(CO_PLANO_INTERNO, 2, 10) %in% 
            c("", "REITORIA", "IFB", "GERAL", "ENSINO", "EXTENSAO","INOVACAO", "PESQUISA"), 
          "DISPONIVEL_SND", "DISPONIVEL_CND")) %>% 
        group_by_at(.vars = c(vars_group, "TIPO")) %>% 
        summarise_at(.vars = "VALOR",.funs = sum, na.rm=TRUE) %>% 
        pivot_wider(names_from = "TIPO", values_from = "VALOR"),
      df %>% 
        group_by_at(.vars = vars_group) %>% 
        summarise_at(.vars = c(vars_sum), .funs = sum, na.rm=TRUE),
      by = vars_group)
  } else {
    vars_num <- c("DOTACAO","INDISPONIVEL","DISPONIVEL", "EMPENHADO", "LIQUIDADO", "PAGO")
    df <- df %>% 
      group_by_at(.vars = vars_group) %>%
      summarise_at(.vars = vars_sum, .funs = sum, na.rm=TRUE)
  }
  df <- df %>% 
    mutate(across(where(is.numeric), ~replace_na(., 0))) %>% 
    mutate(across(where(is.character), ~replace_na(., "-"))) %>%  
    rename_with(~ gsub("CREDITO_", "", .x, fixed = TRUE))
  for(i in 1:length(vars_num)){
    if(!any(colnames(df)==vars_num[i])) df[,vars_num[i]] <- 0
  }
  vars_num <- vars_num[!vars_num %in% unselect]
  df <- df %>% 
    mutate(DOTACAO = DISPONIVEL+INDISPONIVEL+EMPENHADO) %>% 
    select_at(.vars = c(vars_group, vars_num)) %>% 
    filter_at(all_of(vars_num), any_vars(. != 0))
  return(df)
}

#' Gera dados mensais de execução orçamentária e financeira
#'
#' @param df 'data.frame' da base de dados 'tg_orcamento'
#' @param vars_group Variáveis do 'data.frame' a serem agrupadas
#' @param available Considera detalhamento do 'CREDITO_DISPONIVEL' se 'TRUE'
#' @param unselect Variáveis do 'data.frame' a serem desconsideradas
#' @param cumsum_num Considera soma acumulada por colunas numéricas se 'TRUE'
#' @param percent Acrescenta variáveis a partir da função 'pp_percent' se 'TRUE.
#' Requer detalhamento em '...'
#' @param ... Argumentos da função 'pp_percent'
#'
#' @return 'data.frame' com o mês a mês da execução orçamentária e financeira
#' 
#' @export
#' 
pp_execucao_mes <- function(df, vars_group, available=F, unselect=NULL, cumsum_num=F, percent=F, ...){
  df <- pp_execucao(
    df=df, 
    vars_group = c(vars_group, "MES"), 
    available=available, 
    unselect=unselect)
  if(cumsum_num){
    db <- df[0,]
    for(n in 1:length(vars_group)){
      vars_cumsum <- names(table(df[,vars_group[n]]))
      vars_df <- sapply(df, class)
      vars_df <- names(vars_df[vars_df=="numeric"])
      for(i in 1:length(vars_cumsum)){
        db1 <- df[df[,vars_group[n]]==vars_cumsum[i],]
        for(w in 1:length(vars_df)){
          db1[,vars_df[w]] <- cumsum(db1[,vars_df[w]])
        }
        db <- bind_rows(db, db1)
      }
    }
    df <- db
  }
  if(percent){
    df <- pp_percent(df = df, ...) 
  }
  return(df)
}

#' Acrescenta linhas de subtotais e total em 'data.frame'
#'
#' @param df 'data.frame' da base de dados
#' @param vars_group Variáveis do 'data.frame' a serem agrupadas
#' @param total_name Nome da linha que conterá o somatório
#' @param total_all Considera soma total por colunas numéricas se 'TRUE'
#' @param position Posição das linhas subtotais no 'data.frame', podendo ser
#' 'up' para acima das variáveis agrupadas ou 'down' para baixo dessas variáveis
#' @param vars_suppress Variáveis a serem suprimidas
#'
#' @return 'data.frame' com o mês a mês da execução orçamentária e financeira
#' 
#' @export
#' 
pp_subtotal <- function(df, vars_group, total_name = NULL, total_all = T, position = "down", vars_suppress = NULL){
  vars_df <- sapply(df, class)
  vars_df <- names(vars_df[vars_df=="character"])
  no_group <- vars_df[!vars_df==vars_group]
  db_subtotal <- df
  if(is.null(total_name)) total_name <- ""
  name_sub <- case_when(position=="up" ~ paste0("00TOTAL", total_name), 
                        position=="down" ~ paste0("ZZTOTAL", total_name))
  db <- bind_rows(
    df,
    df %>% 
      mutate_at(.vars = no_group, .funs = function(x){x = name_sub}) %>% 
      group_by_at(.vars = vars_df) %>% 
      summarise(across(where(is.numeric), ~sum(., na.rm = TRUE))))
  tt <- which(db[,no_group[1]]==name_sub)
  for(n in seq_along(no_group)){
    for(i in tt){
      db[i, no_group[n]] <- paste(db[i, no_group[n]], db[i, vars_group])
    }
  }
  db <- db %>% 
    arrange_at(.vars = vars_df)
  if(total_all){
    for(i in seq_along(vars_df)){
      db_subtotal[, vars_df[i]] <- "Total"
    }
    db <- bind_rows(
      db, 
      db_subtotal %>% 
        group_by_at(.vars = vars_df) %>% 
        summarise(across(where(is.numeric), ~sum(., na.rm = TRUE))))
  }
  db <- db %>% 
    mutate_at(.vars = vars_df, ~gsub(pattern = "^00TOTAL*|^ZZTOTAL*", replacement = "", x = .))
  if(!is.null(vars_suppress)){
    db <- db[,colnames(db)[!colnames(db)==vars_suppress]]
  }
  return(db)
}

#' Função completa que aplica as funções de coleta e tratamento dos dados
#'
#' @param df 'data.frame' da base de dados
#' @param vars_group Variáveis do 'data.frame' a serem agrupadas por meio da
#' função 'pp_execucao'
#' @param available Considera detalhamento do 'CREDITO_DISPONIVEL' se 'TRUE' a
#' ser considerada na função 'pp_execucao'
#' @param unselect Variáveis do 'data.frame' a serem desconsideradas na
#' função 'pp_execucao'
#' @param total_vars_group Variáveis do 'data.frame' a serem agrupadas por meio
#' da função 'pp_subtotal'
#' @param total_name Nome da linha que conterá o somatório
#' @param total_all Considera soma total por colunas numéricas se 'TRUE'
#' @param position Posição das linhas subtotais no 'data.frame', podendo ser
#' 'up' para acima das variáveis agrupadas ou 'down' para baixo dessas variáveis
#' @param vars_suppress Variáveis a serem suprimidas
#' @param per_num Numerador da fração, podendo ser uma ou mais colunas de classe 
#' numérica de determinado 'data.frame' ou vetor de classe numérica, caso o 
#' parâmetro 'df' não seja informado. 
#' @param per_den Denominador da fração, podendo uma coluna de classe 
#' numérica de determinado 'data.frame' ou vetor de classe numérica, caso
#' o parâmetro 'df' não seja informado.
#' @param per_nsmall Número de casas decimais após a vírgula, correspondente ao
#' parâmetro 'nsmall' da função 'base::format'
#' @param per_format Opção verdadeira 'TRUE' ou falsa 'FALSE', caso necessária
#' a formatação do resultado final da fração
#' @param per_diff Calcula a partir da diferenca
#' @param arrange Ordenar por variáveis indicadas em 'arrange'
#'
#' @return 'data.frame' com o mês a mês da execução orçamentária e financeira
#' 
#' @export
#' 
pp_complete <- function(df, vars_group, available = FALSE, unselect=NULL, 
                        total_vars_group, total_name = NULL, total_all = TRUE, position = "down", vars_suppress = NULL,
                        per_num, per_den, per_nsmall = 1, per_format = F, per_diff = F,arrange){
  df <- pp_execucao(
    df = df,
    vars_group = vars_group,
    available = available,
    unselect = unselect)
  df <- pp_subtotal(
    df = df, 
    vars_group = total_vars_group, 
    total_name = total_name, 
    total_all = total_all,
    position = position,
    vars_suppress = vars_suppress)
  if(!missing(per_num) & !missing(per_den)){
    df <- pp_percent(
      df = df, 
      num = per_num, 
      den = per_den, 
      nsmall = per_nsmall,
      format = per_format, 
      diff = per_diff)
    }
  df <- df %>% 
    mutate(across(where(is.numeric), ~replace_na(., 0)))
  if(!missing(arrange)){
    df <- df %>% 
      arrange_at(.vars = arrange)
  }
  return(df)
}