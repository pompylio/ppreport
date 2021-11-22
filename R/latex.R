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

pp_title_latex <- function(title = "", subtitle = "", bg_color = "#2f9e41", font_color = "#ffffff", logo){
  asis_output(
    as.character(
      paste0(
      "\\definecolor{bgcolor}{HTML}{", gsub(pattern = "#", replacement = "", x = bg_color), "} 
       \\definecolor{ftcolor}{HTML}{", gsub(pattern = "#", replacement = "", x = font_color),"}
       \\begin{tcolorbox}[arc=0mm,colback=bgcolor,boxsep=3mm,colframe=bgcolor,colframe=white,sidebyside,righthand width=5cm]
        \\color{ftcolor}{\\vspace{0.2cm}{\\LARGE{\\textbf{", title, "}}}}
        
        \\color{gray!20}{\\rule{\\linewidth}{0.2mm}}
        
        \\color{ftcolor}{\\textbf{", subtitle,"}}
        
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
#' @param height Altura da tabela em cm. Se ausente, altura automática.
#'
#' @return Layout padrão da tabela de conteúdo
#' 
#' @export
#' 
pp_content_latex <- function(bg_color = "#ffffff", border_color = "#ffffff", border_width = 0.1, height = 3){
  if(missing(height)) 
    tbox <- paste0("\\begin{tcolorbox}[arc=0mm,colback=colback,colframe=white]")
  else
    tbox <- paste0("\\begin{tcolorbox}[arc=0mm,colback=colback,colframe=white,height=", height,"cm]")
  asis_output(
    as.character(
      paste0(
      "\\definecolor{colback}{HTML}{", gsub(pattern = "#", replacement = "", bg_color), "}
       \\definecolor{colline}{HTML}{", gsub(pattern = "#", replacement = "", border_color), "}
       \\color{colline}{\\rule{\\linewidth}{", border_width, "mm}}
       ", tbox, "
          
          \\tableofcontents
      
          \\end{tcolorbox}
       \\color{colline}{\\rule{\\linewidth}{", border_width, "mm}}
       \\color{black}")))
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
pp_info_latex <- function(x, title_color = "#ffffff", border_color = "#ffffff", border_width = 0.1){
  txt <- character()
  for(i in 1:length(x)){
    txt <- append(txt, paste0("\\textcolor{black}{\\text{\\textbf{",names(x[i]),"} ",x[i][[1]],"}}"))}
  txt <- paste(txt, collapse = "\\linebreak")
  asis_output(
    as.character(
      c(paste0("\\definecolor{colline}{HTML}{",gsub(pattern = "#", replacement = "", border_color),"}"),
        paste0("\\definecolor{titlecolor}{HTML}{",gsub(pattern = "#", replacement = "", title_color),"}"),
        "\\vfill",
        "\\color{titlecolor}{\\textbf{Parâmetros}}\\linebreak",
        paste0("\\color{colline}{\\rule{\\linewidth}{",border_width,"mm}}\\linebreak"),
        txt,
        "\\linebreak",
        paste0("\\color{colline}{\\rule{\\linewidth}{",border_width,"mm}}"),
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
