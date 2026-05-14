# Análise Hidrológica de Tendências — Bacia Amazônica

**Autor:** Renan Cassimiro Brito  
**Data:** 2026-05-13  
**Versão:** 1.0

---

## Descrição

Pipeline de análise hidrológica para detecção de tendências em séries de vazão de estações fluviométricas da ANA (Agência Nacional de Águas), com foco na Bacia Amazônica. O projeto obtém dados via API HidroWeb, aplica testes estatísticos clássicos de tendência e gera relatório PDF com visualizações temáticas.

---

## Estrutura do projeto

```
.
├── main.R                          # Script principal
├── relatorio.Rmd                   # Template do relatório PDF
├── functions/
│   └── functions.R                 # Funções de análise e visualização
├── resources/                      # Dados de entrada (não versionados)
│   ├── hybas_lake_sa_lev03_v1c_bacia_amazonica.gpkg
│   ├── dourada_geoft_estacao_hidrometeorologica_filtradas.gpkg
│   ├── ne_10m_rivers_lake_centerlines_amazonia.gpkg
│   └── AMACRO/
│       └── AMACRO.shp
└── output/
    └── refactor_test/
        ├── data/                   # Objetos RDS intermediários
        ├── images/                 # Gráficos PNG exportados
        ├── report/                 # Relatório PDF final
        └── resumo_tendencias_mensal.csv
```

---

## Dependências

### Pacotes CRAN

```r
install.packages(c(
  "fs", "here", "Kendall", "lubridate", "openxlsx",
  "patchwork", "scales", "sf", "tidyverse", "trend", "zyp", "zoo"
))
```

### Pacote GitHub

```r
# install.packages("devtools")
devtools::install_github("hydroversebr/hydrobr", build_vignettes = FALSE)
```

---

## Como executar

1. Clone o repositório e abra o projeto no RStudio (`.Rproj`)
2. Coloque os arquivos de entrada em `resources/`
3. Instale as dependências acima
4. Execute `main.R` do início ao fim

O relatório PDF será gerado em `output/refactor_test/report/`.

Para usar todas as estações da bacia (sem filtro prévio), defina:

```r
estacoes_filtradas <- NULL
```

---

## Fluxo de análise

```
Área de estudo (.gpkg)
        ↓
  Inventário HidroWeb (hydrobr::inventory)
        ↓
  Download das séries (hydrobr::stationsData)
        ↓
  Seleção por qualidade (hydrobr::selectStations)
        ↓
  Análise por estação (analisar_estacao)
  ├── Agregação mensal
  ├── Decomposição STL
  ├── Mann-Kendall (componente de tendência)
  ├── Yue-Pilon (série original, corrige autocorrelação)
  ├── Slope de Sen
  ├── Pettitt (ponto de mudança)
  └── Anomalias por z-score (climatologia mensal)
        ↓
  Tabela resumo + gráficos (gerar_graficos)
        ↓
  Relatório PDF (RMarkdown)
```

---

## Outputs principais

| Arquivo | Descrição |
|---|---|
| `resumo_tendencias_mensal.csv` | Tabela com métricas por estação |
| `report/relatorio.pdf` | Relatório completo com todos os gráficos |
| `images/tendencias_estacoes.png` | Barplot de slope de Sen por estação |
| `images/mapa_tendencias_classificadas.png` | Mapa por categoria de tendência |
| `images/mapas_tematicos.png` | Mapas com \|τ\|, slope e ponto de mudança |
| `images/contagens_tendencia.png` | Frequência/proporção de tendências |
| `data/*.rds` | Objetos intermediários para reprocessamento parcial |

---

## Testes estatísticos

| Teste | Pacote | Aplicado em | Finalidade |
|---|---|---|---|
| Mann-Kendall | `Kendall` | Componente de tendência STL | Detectar tendência monotônica |
| Yue-Pilon | `zyp` | Série original | Corrigir autocorrelação serial |
| Slope de Sen | `trend` | Série original | Estimar magnitude da tendência |
| Pettitt | `trend` | Série original | Detectar ponto de mudança |

Classificação da tendência: significativa quando `p < 0.05` (Mann-Kendall), direção pelo sinal do slope de Sen.

---

## TODOs

- [ ] Melhorar mapa inicial de apresentação (incluir camada de rios e estados)
- [ ] Aplicar filtro de estações antes do download (atualmente feito após)
- [ ] Cruzar estações com camada de rios/bacias para enriquecer atributos
- [ ] Aplicar análises temporais adicionais disponíveis no `{hydrobr}`
- [ ] Eliminar dependência de variáveis globais (`RESOURCES_DIR`, `area_estudo`) dentro de `gerar_graficos()`
- [ ] Interface Shiny para seleção interativa de estações e bacias hidrográficas