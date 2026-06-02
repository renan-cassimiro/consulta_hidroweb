# =============================================================================
# VISUALIZAÇÕES — ANÁLISE DE TENDÊNCIAS HIDROLÓGICAS
# =============================================================================
#
# Autor:   Renan Cassimiro Brito
# Data:    2026-05-13
#
# Descrição:
#   Produz todos os gráficos do relatório hidrológico como objetos ggplot,
#   sem efeitos colaterais de I/O e sem dependência de variáveis globais.
#   Camadas geográficas externas (rios, AMACRO) são recebidas como argumentos.
#
# Funções exportadas:
#   gerar_graficos(tabela_resumo, dados_empilhados, inventario,
#                  area_estudo, cfg, rios = NULL, amacro = NULL)
#
#   salvar_graficos(graficos, image_dir, var_id)
# =============================================================================

# Paleta e ordem de tendências — constantes de estilo compartilhadas
CORES_TENDENCIA <- c(
  "Negativa"          = "#d73027",
  "Positiva"          = "#4575b4",
  "Não significativa" = "#969696"
)
ORDEM_TENDENCIA <- c("Negativa", "Não significativa", "Positiva")


# -----------------------------------------------------------------------------
#' Gera todos os gráficos do relatório para uma variável
#'
#' @param tabela_resumo    tibble com colunas: station_code, slope_sen,
#'                         p_valor_mk, tau_mk, tendencia (e demais métricas).
#' @param dados_empilhados tibble longo com séries mensais de todas as estações;
#'                         colunas: date, <value_col>, trend, station_code.
#' @param inventario       sf com geometria das estações (station_code, geometry).
#' @param area_estudo      sf com o polígono da área de estudo.
#' @param cfg              Lista de configuração da variável (de VARIABLE_CONFIGS).
#'                         Usado para adaptar rótulos de eixo e subtítulos.
#' @param rios             sf com hidrografia. Se NULL, mapa é gerado sem rios.
#' @param amacro           sf com polígono AMACRO. Se NULL, omitido no G5.
#'
#' @return Lista com objetos ggplot:
#'   $tendencias_estacoes           — barplot horizontal de slope por estação
#'   $tendencias_classificadas      — facets: série + tendência STL
#'   $mapa_tendencias_classificadas — mapa por categoria de tendência
#'   $contagens_tendencia           — barplot de frequência/proporção
#'   $mapa_tendencia_hidrologica    — mapa com tamanho = |τ| e camadas geo
# -----------------------------------------------------------------------------
gerar_graficos <- function(tabela_resumo,
                           dados_empilhados,
                           inventario,
                           area_estudo,
                           cfg,
                           rios   = NULL,
                           amacro = NULL) {
  
  value_col   <- cfg$value_col
  unit_label  <- cfg$unit_label
  var_label   <- cfg$label
  
  # Garante ordem dos níveis do fator em toda a função
  tabela_resumo <- tabela_resumo |>
    mutate(tendencia = factor(tendencia, levels = ORDEM_TENDENCIA))
  
  # ---------------------------------------------------------------------------
  # G1. Barplot horizontal: slope de Sen por estação
  # ---------------------------------------------------------------------------
  tabela_g1 <- tabela_resumo |>
    mutate(label = paste0(
      round(slope_sen, 2),
      case_when(
        tendencia == "Positiva" ~ " ▲",
        tendencia == "Negativa" ~ " ▼",
        .default                = " ○"
      )
    ))
  
  tendencias_estacoes <- ggplot(
    tabela_g1,
    aes(x = reorder(station_code, slope_sen), y = slope_sen, fill = tendencia)
  ) +
    geom_bar(stat = "identity") +
    geom_text(
      aes(label = label),
      hjust = ifelse(tabela_g1$slope_sen < 0, 1.1, -0.1),
      size  = 3
    ) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    coord_flip() +
    scale_fill_manual(values = c(
      "Positiva"          = "steelblue",
      "Negativa"          = "darkred",
      "Não significativa" = "gray70"
    )) +
    labs(
      title    = paste("Tendência de", var_label, "por estação"),
      subtitle = paste0(
        "Valores em ", unit_label, "/ano | ▲ positivo ▼ negativo ○ não significativo"
      ),
      x    = "Estação",
      y    = paste0("Tendência (", unit_label, "/ano)"),
      fill = "Tendência"
    ) +
    theme_minimal()
  
  # ---------------------------------------------------------------------------
  # G2. Facets: série temporal original + tendência STL por estação
  # ---------------------------------------------------------------------------
  dados_com_classif <- dados_empilhados |>
    left_join(
      tabela_resumo |> select(station_code, slope_sen, p_valor_mk, tendencia),
      by = "station_code"
    ) |>
    mutate(
      titulo_facet = paste0(
        station_code, "\n",
        round(slope_sen, 2), " ", unit_label, "/ano | p = ", round(p_valor_mk, 4),
        case_when(
          tendencia == "Negativa" ~ " ▼",
          tendencia == "Positiva" ~ " ▲",
          .default                = " ○"
        )
      )
    )
  
  tendencias_classificadas <- ggplot(
    dados_com_classif,
    aes(x = date, y = .data[[value_col]])
  ) +
    geom_line(alpha = 0.3, linewidth = 0.3, color = "gray50") +
    geom_line(aes(y = trend, color = tendencia), linewidth = 1.2) +
    facet_wrap(~titulo_facet, scales = "free_y", ncol = 7) +
    scale_color_manual(values = CORES_TENDENCIA, name = "Tendência") +
    labs(
      title    = paste("Tendência de", var_label, "por estação — Bacia Amazônica"),
      subtitle = "Série original (cinza) | Colorido = tendência STL | ▼ Negativa ▲ Positiva ○ Não significativa",
      x        = "Data",
      y        = paste0(var_label, " (", unit_label, ")")
    ) +
    theme_minimal() +
    theme(
      axis.text.x     = element_text(angle = 45, hjust = 1, size = 6),
      axis.text.y     = element_text(size = 7),
      strip.text      = element_text(size = 8, face = "bold"),
      legend.position = "bottom",
      plot.title      = element_text(size = 14, face = "bold"),
      plot.subtitle   = element_text(size = 10)
    )
  
  # ---------------------------------------------------------------------------
  # G3. Mapa: categoria de tendência por estação
  # ---------------------------------------------------------------------------
  dados_map <- dados_com_classif |>
    distinct(station_code, tendencia) |>
    inner_join(
      inventario |> select(station_code, geometry),
      by = "station_code"
    ) |>
    st_as_sf()
  
  mapa_base <- ggplot() +
    geom_sf(data = area_estudo, color = "black", linewidth = 1, fill = NA)
  
  if (!is.null(rios)) {
    mapa_base <- mapa_base +
      geom_sf(data = rios, color = "blue", linewidth = 0.5)
  }
  
  mapa_tendencias_classificadas <- mapa_base +
    geom_sf(data = dados_map, aes(color = tendencia), size = 5) +
    scale_color_manual(values = CORES_TENDENCIA, name = "Tendência") +
    theme_minimal() +
    theme(
      axis.text.x     = element_text(angle = 45, hjust = 1, size = 7),
      axis.text.y     = element_text(size = 7),
      legend.position = "bottom",
      plot.title      = element_text(size = 14, face = "bold")
    ) +
    labs(title = paste("Mapa de tendências —", var_label))
  
  # ---------------------------------------------------------------------------
  # G4. Barplot: frequência e proporção de estações por categoria de tendência
  # ---------------------------------------------------------------------------
  contagem_tend <- tabela_resumo |>
    count(tendencia) |>
    mutate(
      prop      = n / sum(n),
      label_bar = paste0(n, "\n(", round(prop * 100, 1), "%)")
    )
  
  contagens_tendencia <- ggplot(
    contagem_tend,
    aes(x = tendencia, y = n, fill = tendencia)
  ) +
    geom_col(width = 0.6, show.legend = FALSE) +
    geom_text(aes(label = label_bar), vjust = -0.3, size = 3.5, lineheight = 0.9) +
    scale_fill_manual(values = CORES_TENDENCIA) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.18))) +
    labs(
      title = paste("Distribuição de tendências —", var_label),
      x     = NULL,
      y     = "Nº de estações"
    ) +
    theme_bw(base_size = 14) +
    theme(panel.grid.major.x = element_blank())
  
  # ---------------------------------------------------------------------------
  # G5. Mapa temático: tamanho = |τ|, cor = tendência, camadas geo opcionais
  # ---------------------------------------------------------------------------
  estacoes_map <- inventario |>
    left_join(tabela_resumo, by = "station_code") |>
    mutate(abs_tau = abs(tau_mk))
  
  tema_mapa <- theme_void(base_size = 14) +
    theme(
      plot.title       = element_text(face = "bold", size = 16),
      plot.subtitle    = element_text(size = 12, color = "gray40"),
      legend.position  = "right",
      legend.key.width = unit(1, "cm")
    )
  
  mapa_hidro <- ggplot()
  
  if (!is.null(amacro)) {
    mapa_hidro <- mapa_hidro +
      geom_sf(data = amacro, fill = "pink", linewidth = 0.8)
  }
  
  if (!is.null(rios)) {
    mapa_hidro <- mapa_hidro +
      geom_sf(data = rios, color = "blue", linewidth = 0.5)
  }
  
  mapa_tendencia_hidrologica <- mapa_hidro +
    geom_sf(data = area_estudo, color = "black", linewidth = 1, fill = NA) +
    geom_sf(data = estacoes_map, aes(color = tendencia, size = abs_tau)) +
    scale_color_manual(values = CORES_TENDENCIA, name = "Tendência") +
    scale_size_continuous(
      name   = "|τ Mann-Kendall|",
      range  = c(1.5, 6),
      breaks = c(0.1, 0.2, 0.3, 0.4)
    ) +
    guides(
      color = guide_legend(override.aes = list(size = 4)),
      size  = guide_legend()
    ) +
    labs(
      title    = paste("Direção e força da tendência —", var_label),
      subtitle = "Cor = categoria | Tamanho = |τ|"
    ) +
    tema_mapa
  
  # ---------------------------------------------------------------------------
  # Retorno
  # ---------------------------------------------------------------------------
  list(
    tendencias_estacoes           = tendencias_estacoes,
    tendencias_classificadas      = tendencias_classificadas,
    mapa_tendencias_classificadas = mapa_tendencias_classificadas,
    contagens_tendencia           = contagens_tendencia,
    mapa_tendencia_hidrologica    = mapa_tendencia_hidrologica
  )
}


# -----------------------------------------------------------------------------
#' Exporta todos os gráficos de uma variável para PNG
#'
#' @param graficos   Lista retornada por gerar_graficos().
#' @param image_dir  Diretório de saída para as imagens.
#' @param var_id     character. Id da variável (ex: "discharge"). Usado como sufixo.
# -----------------------------------------------------------------------------
salvar_graficos <- function(graficos, image_dir, var_id) {
  
  specs <- list(
    list(nome = "tendencias_estacoes",           w = 10, h = 8,  dpi = 300),
    list(nome = "tendencias_classificadas",       w = 20, h = 12, dpi = 150),
    list(nome = "mapa_tendencias_classificadas",  w = 16, h = 12, dpi = 300),
    list(nome = "contagens_tendencia",            w = 12, h = 5,  dpi = 150),
    list(nome = "mapa_tendencia_hidrologica",     w = 8,  h = 16, dpi = 150)
  )
  
  walk(specs, function(s) {
    img_path <- path(image_dir, paste0(s$nome, "_", var_id, ".png"))
    ggsave(img_path, graficos[[s$nome]], width = s$w, height = s$h, dpi = s$dpi)
    message(sprintf("  Salvo: %s", img_path))
  })
}