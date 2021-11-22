#' @import flextable
#' @export
NULL


#' Tema padrão para a tabela do tipo 'flextable' a ser aplicado nos relatórios
#' 'rmarkdown'
#'
#' @param x Objeto 'flextable'
#' @param bg_title Cor do fundo para o título
#' @param border_color Cor da borda
#' @param bg_row Cor do fundo das linhas da tabela
#' @param align_col Permite definir o alinhamento para cada coluna
#'
#' @return Tema padrão para a tabela
#' 
#' @export
pp_flextheme <- function(x, bg_title = "#336699", border_color = "#d9d9d9", bg_row, align_col) {
  x <- border_remove(x)
  std_border <- fp_border_default(width = 0.9, color = border_color)
  x <- border_outer(x, part="all", border = std_border)
  x <- border_inner_h(x, border = std_border, part="all")
  x <- border_inner_v(x, border = std_border, part="all")
  x <- italic(x = x, italic = TRUE, part = "footer")
  if(!missing(align_col)){
    align_col <- case_when(align_col == "l" ~ "left",
                           align_col == "r" ~ "right",
                           align_col == "c" ~ "center",
                           align_col == "j" ~ "justify")
    for(j in seq_along(align_col)){
      x <- align(x, j = j, align = align_col[j], part = "all")
    }
  }
  x <- colformat_date(x = x, fmt_date = "%d/%m/%Y",)
  x <- colformat_int(x = x, big.mark=".", na_str = "")
  x <- colformat_double(x = x, big.mark=".", decimal.mark = ",", digits = 2, na_str = "")
  x <- bold(x = x, bold = TRUE, part = "header")
  x <- bg(x = x, bg = bg_title, part = "header")
  x <- align(x, align = "center", part = "header")
  x <- color(x, color = "#ffffff", part = "header")
  if(!missing(bg_row)){
    if(length(bg_row$i)>1){
      for(i in 1:length(bg_row$i)){
        x <- bg(x = x, i = bg_row$i, j = bg_row$j,bg = bg_row$bg)
      }
    } else{
      x <- bg(x = x, i = bg_row$i, j = bg_row$j, bg = bg_row$bg)
    }
    
  }
  return(x)
}

#' Tabela padrão do tipo 'flextable' a ser aplicado nos relatórios em 'rmarkdown'
#'
#' @param x Data.frame para conversão em tabela do tipo 'flextable'
#' @param caption Título da tabela
#' @param title Novos títulos para as colunas da tabela. Se ausente 'NULL', retorna
#' nomes das colunas do 'data.frame' corrigido pela função 'pp_format_str'
#' @param title_group Cria um título mesclado para agrupamento de colunas especificados
#' no argumento
#' @param merge Colunas e/ou linhas a serem mescladas
#' @param group Agrupamento pela função 'as_grouped_data'
#' @param height Altura da linha em pixels
#' @param width Tamanho da coluna em pixels
#' @param font_size Tamanho da fonte
#' @param ... Outros argumentos para a função 'pp_theme'
#'
#' @return Tabela flextable 'html' ou 'latex'
#' 
#' @export
pp_flextable <- function(x, caption, title, title_group, merge, group, height, width, font_size=10, ...){
  if(missing(group)){
    ft <- flextable(x)
  } else {
    ft <- as_grouped_data(x, groups = c(group))
    ft <- as_flextable(ft)
  }
  if(missing(title)){
    title <- pp_format_str(x = ft$col_keys, sep = " ", str_to = "sentence")
    names(title) <- ft$col_keys
  } else {
    names(title) <- ft$col_keys
  }
  if(!missing(title_group)){
    ft <- add_header_row(x = ft, values = names(title_group), colwidths = as.vector(title_group))
  }
  if(!missing(merge)){
    if(!is.null(merge$i)){
      ft <- merge_h_range(x = ft, i = merge$i, j1 = merge$ij1, j2 = merge$ij2)
    }
    if(!is.null(merge$j)){
      ft <- merge_v(x = ft, j = merge$j)
      ft <- valign(x = ft, j = merge$j, valign = "top", part = "body")
    }
  }
  if(missing(width)){
    ft <- autofit(ft)
  } else {
    for(i in 1:length(width)){
      ft <- flextable::width(x = ft, j = i, width = width[i])
    }
  }
  if(!missing(height)){
    ft <- height_all(ft, height = height)
    ft <- hrule(ft, rule = "exact")
  }
  if(!missing(caption)){
    ft <- set_caption(x = ft, caption = caption)
  }
  ft <- set_header_labels(x = ft, values = as.list(title))
  ft <- fontsize(ft, size = font_size, part = "all")
  ft <- pp_flextheme(ft, ...)
  ft
}