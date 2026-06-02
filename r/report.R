# =============================================================================
# RENDERIZAÇÃO DE RELATÓRIOS PDF
# =============================================================================
#
# Descrição:
#   Funções que chamam rmarkdown::render() para os dois templates do projeto.
#   Separa a lógica de renderização do main.R, mantendo-o limpo.
#
# Funções exportadas:
#   renderizar_relatorio_variavel(res_variavel, run_name, report_dir, template_path)
#   renderizar_relatorio_consolidado(resultados_por_variavel, consolidado,
#                                    run_name, report_dir, template_path)
#   renderizar_todos(resultados_por_variavel, consolidado, run_name, dirs)
# =============================================================================


# -----------------------------------------------------------------------------
#' Renderiza o relatório PDF de uma única variável
#'
#' @param res_variavel  Elemento de `resultados_por_variavel` (lista com
#'                      $cfg, $graficos, $tabela_resumo).
#' @param run_name      character. Nome do run.
#' @param report_dir    Diretório de saída para o PDF.
#' @param template_path Caminho para `relatorio_variavel.Rmd`.
#'
#' @return Caminho do PDF gerado (invisível).
# -----------------------------------------------------------------------------
renderizar_relatorio_variavel <- function(res_variavel,
                                          run_name,
                                          report_dir,
                                          template_path = here("relatorio_variavel.Rmd")) {
  
  var_id   <- res_variavel$cfg$id
  pdf_path <- path(report_dir, paste0("relatorio_", var_id, ".pdf"))
  
  message(sprintf("[Relatório] Renderizando PDF — %s...", res_variavel$cfg$label))
  
  rmarkdown::render(
    input       = template_path,
    output_file = pdf_path,
    params      = list(
      graficos  = res_variavel$graficos,
      cfg       = res_variavel$cfg,
      tabela    = res_variavel$tabela_resumo,
      run_name  = run_name
    ),
    envir  = new.env(parent = globalenv()),
    quiet  = TRUE
  )
  
  message(sprintf("[Relatório] Salvo: %s", pdf_path))
  invisible(pdf_path)
}


# -----------------------------------------------------------------------------
#' Renderiza o relatório PDF consolidado (todas as variáveis)
#'
#' @param resultados_por_variavel  Lista completa do main.R.
#' @param consolidado              Saída de consolidar_resultados().
#' @param run_name                 character. Nome do run.
#' @param report_dir               Diretório de saída para o PDF.
#' @param template_path            Caminho para `relatorio_consolidado.Rmd`.
#'
#' @return Caminho do PDF gerado (invisível).
# -----------------------------------------------------------------------------
renderizar_relatorio_consolidado <- function(resultados_por_variavel,
                                             consolidado,
                                             run_name,
                                             report_dir,
                                             template_path = here("relatorio_consolidado.Rmd")) {
  
  pdf_path <- path(report_dir, paste0("relatorio_consolidado_", run_name, ".pdf"))
  
  message("[Relatório] Renderizando PDF consolidado...")
  
  rmarkdown::render(
    input       = template_path,
    output_file = pdf_path,
    params      = list(
      resultados_por_variavel = resultados_por_variavel,
      consolidado             = consolidado,
      run_name                = run_name
    ),
    envir  = new.env(parent = globalenv()),
    quiet  = TRUE
  )
  
  message(sprintf("[Relatório] Salvo: %s", pdf_path))
  invisible(pdf_path)
}


# -----------------------------------------------------------------------------
#' Renderiza todos os relatórios (um por variável + consolidado)
#'
#' Função de conveniência chamada no final do main.R.
#'
#' @param resultados_por_variavel  Lista completa do main.R.
#' @param consolidado              Saída de consolidar_resultados().
#' @param run_name                 character. Nome do run.
#' @param dirs                     Lista de diretórios retornada por setup_dirs().
# -----------------------------------------------------------------------------
renderizar_todos <- function(resultados_por_variavel,
                             consolidado,
                             run_name,
                             dirs) {
  
  # Um PDF por variável
  walk(resultados_por_variavel, function(res) {
    renderizar_relatorio_variavel(
      res_variavel  = res,
      run_name      = run_name,
      report_dir    = dirs$report_dir
    )
  })
  
  # PDF consolidado
  renderizar_relatorio_consolidado(
    resultados_por_variavel = resultados_por_variavel,
    consolidado             = consolidado,
    run_name                = run_name,
    report_dir              = dirs$report_dir
  )
  
  message(sprintf(
    "\n[Relatório] %d PDFs gerados em: %s",
    length(resultados_por_variavel) + 1,
    dirs$report_dir
  ))
}