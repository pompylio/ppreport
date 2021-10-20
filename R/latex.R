#' @importFrom knitr asis_output
#' @export
NULL

#' Layout padrão para título de relatórios em 'rmarkdown' do tipo 'latex'
#'
#' @param title Título do relatório 
#' @param subtitle Subtítulo do relatório
#' @param bg_color Cor do fundo do título
#' @param font_color Cor da fonte a ser aplicada ao título 'title'  e subtítulo 'subtitle'
#' @param logo Caminho para o arquivo de logotipo
#'
#' @return Layout padrão do título
#' 
#' @export

pp_title_latex <- function(title = "", subtitle = "", bg_color = "2f9e41", font_color = "ffffff", logo){
  bg_color <- gsub(pattern = "#", replacement = "", x = bg_color)
  font_color <- gsub(pattern = "#", replacement = "", x = font_color)
  asis_output(
    as.character(
      paste0("\\definecolor{bgcolor}{HTML}{", bg_color, "}
             \\begin{tcolorbox}[arc=0mm,colback=bgcolor,boxsep=3mm,colframe=bgcolor,colframe=white,sidebyside,righthand width=5cm]
              \\color{", font_color, "}{\\vspace{0.2cm}{\\LARGE{\\textbf{", title, "}}}}
              
              \\color{gray!20}{\\rule{\\linewidth}{0.1mm}}
              
              \\color{", font_color, "}{\\textbf{", subtitle,"}}
              
              \\tcblower
              \\includegraphics[height=6cm]{", logo,"}
              \\end{tcolorbox}
             \\color{black}")))
}

#' Layout padrão da tabela de conteúdo para relatórios em 'rmarkdown' 
#' do tipo 'latex'
#'
#' @param bg_color Cor do fundo da tabela de conteúdo
#' @param border_color Cor da borda da tabela de conteúdo
#' @param border_width Tamanho da linha da borda da tabela de conteúdo
#'
#' @return Layout padrão da tabela de conteúdo
#' 
#' @export
#' 
pp_content_latex <- function(bg_color = "ffffff", border_color = "ffffff", border_width = 0.1){
  bg_color <- gsub(pattern = "#", replacement = "", bg_color)
  border_color <- gsub(pattern = "#", replacement = "", border_color)
  asis_output(as.character(
    paste0("\\definecolor{bgcolor}{HTML}{", bg_color, "}  
           \\definecolor{bdcolor}{HTML}{", border_color,"}  
           \\color{bdcolor}{\\rule{\\width}{", border_width, "mm}}  
           \\begin{tcolorbox}[arc=0mm,colback=bgcolor,colframe=white] 
              
              \\tableofcontents
              
              \\end{tcolorbox}
           \\color{bdcolor}{\\rule{\\width}{", border_width, "mm}}")))
}

#' Layout padrão da tabela de informações básicas sobre o relatório em
#' 'rmarkdown' do tipo 'latex'
#'
#' @param x Vetor de itens para a tabela
#' @param border_color Cor da borda da tabela de conteúdo
#' @param border_width Tamanho da linha da borda da tabela de conteúdo
#'
#' @return Layout padrão da tabela de informações básicas
#' 
#' @export
#' 
pp_info_latex <- function(x, border_color = "ffffff", border_width = 0.1){
  txt <- character()
  for(i in 1:length(x)){
    txt <- append(txt, paste0("\\textcolor{black}{\\text{\\textbf{",names(x[i]),"} ",x[i][[1]],"}}"))}
  txt <- paste(txt, collapse = "\\linebreak")
  border_color <- gsub(pattern = "#", replacement = "", border_color)
  asis_output(
    as.character(
      c(paste0("\\definecolor{colline}{HTML}{", border_color,"}"),
        "\\vfill",
        "\\color{colline}{\\textbf{Parâmetros}}\\linebreak",
        paste0("\\color{colline}{\\rule{\\linewidth}{", border_width,"mm}}\\linebreak"),
        txt,
        "\\linebreak",
        paste0("\\color{colline}{\\rule{\\linewidth}{", border_width,"mm}}"),
        "\\color{black}")))
}

#' Layout padrão da assinatura do relatório emitido em 'rmarkdown' do tipo 'latex'
#'
#' @param x Primeiro item da assinatura, podendo ser o nome
#' @param y Segundo item da assinatura, podendo ser o cargo ou setor
#' @param z Terceiro item da assinatura, podendo ser o local e data
#'
#' @return Layout padrão da assinatura
#' 
#' @export
#' 
pp_assign_latex <- function(x, y, z){
  asis_output(as.character(
    paste0(
      "\\begin{center}
      \\mbox{}
      \\vfill
      \\textcolor{black}{", x, "}
    
      \\textcolor{black}{", y, "}
    
      \\textcolor{black}{", z,"}
    
      \\end{center}")))
}
