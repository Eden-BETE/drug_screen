# ══════════════════════════════════════════════════════════════════════════════
# tab_classes.R — Onglet "Visualisation des classes de lignées" (Ania)
# Trois visualisations à partir de data/classification_uploadedData.csv :
#   1. Bar plot   — Répartition des lignées par sous-type
#   2. ACP        — Projection 2D des profils probabilistes (avec ellipses 80 %)
#   3. Dendrogramme — Clustering hiérarchique Ward D2
# ══════════════════════════════════════════════════════════════════════════════

library(ggplot2)
library(dplyr)
library(tidyr)
library(plotly)

# ── Chargement & préparation (mémoïsé au niveau du module) ───────────────────
.classif_path <- "data/classification_uploadedData.csv"
.classif_df   <- read.csv(.classif_path, stringsAsFactors = FALSE, row.names = 1)
.prob_cols    <- c("MES", "PN", "CL.A", "NL", "CL.C", "PN.L", "CL.B")

# Palette cohérente avec data_prep.R (CLUSTER_COLORS)
.palette_classes <- CLUSTER_COLORS

# ══════════════════════════════════════════════════════════════════════════════
# UI
# ══════════════════════════════════════════════════════════════════════════════
tab_classes_ui <- function() {
  tabItem(tabName = "tab_classes",

    fluidRow(
      box(width = 12, status = "warning", solidHeader = TRUE,
          title = tags$span(icon("microscope"),
                            " Visualisation des classes de lignées cellulaires"),
          div(style = "font-size:14px; color:#444;",
              tags$strong("70 lignées cellulaires, 7 sous-types moléculaires."),
              " Le cancer du sein n'est pas une seule maladie : derrière un même
              diagnostic se cachent des profils aux comportements très différents.
              À partir des scores d'influence des facteurs de transcription, ",
              tags$strong("cRegMap"),
              " attribue à chacune des 70 lignées une probabilité d'appartenance
              aux 7 sous-types identifiés (MES, PN, CL-A, CL-B, CL-C, NL, PN-L).
              Les visualisations suivantes répondent à trois questions : ",
              tags$strong("quelle est la taille de chaque groupe ?"), " ",
              tags$strong("Les sous-types se séparent-ils nettement ?"), " ",
              tags$strong("Les lignées se regroupent-elles spontanément comme prévu ?")
          )
      )
    ),

    fluidRow(
      box(width = 12, status = "warning", solidHeader = TRUE,
          title = "1. Répartition des lignées par sous-type",
          plotlyOutput("classes_barplot", height = "420px"),
          div(class = "text-muted",
              style = "margin-top:8px;",
              "Effectif et pourcentage de chaque sous-type. ",
              tags$strong("CL-B, CL-C, MES et PN"), " sont les classes
              majoritaires ; ", tags$strong("CL-A, NL et PN-L"),
              " comportent peu de lignées et seront à utiliser avec prudence
              dans les analyses de réponse aux traitements.")
      )
    ),

    fluidRow(
      box(width = 12, status = "warning", solidHeader = TRUE,
          title = "2. ACP — séparation des sous-types dans l'espace probabiliste",
          plotlyOutput("classes_pca", height = "560px"),
          div(class = "text-muted",
              style = "margin-top:8px;",
              "Chaque point est une lignée projetée sur les deux premières
              composantes principales calculées à partir des 7 probabilités
              d'appartenance. Les ellipses indiquent la zone de confiance à 80 %
              de chaque sous-type.")
      )
    ),

    fluidRow(
      box(width = 12, status = "warning", solidHeader = TRUE,
          title = "3. Dendrogramme — clustering hiérarchique des lignées",
          plotOutput("classes_dendro", height = "520px"),
          div(class = "text-muted",
              style = "margin-top:8px;",
              "Clustering ascendant Ward D2 (distance euclidienne) sur les
              profils de probabilités. Permet de comparer la classification
              attribuée aux regroupements naturels des lignées.")
      )
    )
  )
}

# ══════════════════════════════════════════════════════════════════════════════
# SERVER
# ══════════════════════════════════════════════════════════════════════════════
tab_classes_server <- function(input, output, session) {

  classif_data <- reactive({
    .classif_df %>%
      mutate(cluster = factor(cluster, levels = names(.palette_classes)))
  })

  prob_matrix <- reactive({
    m <- as.matrix(.classif_df[, .prob_cols])
    rownames(m) <- rownames(.classif_df)
    m
  })

  # ── 1. Bar plot ────────────────────────────────────────────────────────────
  output$classes_barplot <- renderPlotly({
    df_count <- classif_data() %>%
      count(cluster) %>%
      mutate(cluster   = as.character(cluster),
             pct       = n / sum(n) * 100,
             etiquette = sprintf("%d (%.1f%%)", n, pct)) %>%
      arrange(desc(n))

    couleurs <- unname(.palette_classes[df_count$cluster])

    plot_ly(
      data    = df_count,
      x       = ~factor(cluster, levels = cluster),
      y       = ~n,
      type    = "bar",
      text    = ~etiquette,
      textposition = "outside",
      textfont = list(size = 13, color = "#222"),
      marker  = list(color = couleurs,
                     line  = list(color = "white", width = 1)),
      hovertemplate = paste0("<b>%{x}</b><br>",
                             "Lignées : %{y}<br>",
                             "Proportion : %{text}<extra></extra>")
    ) %>%
      layout(
        title  = list(text = "Répartition des lignées cellulaires par sous-type",
                      font = list(size = 16)),
        xaxis  = list(title = "Sous-type"),
        yaxis  = list(title = "Nombre de lignées",
                      range = c(0, max(df_count$n) * 1.20)),
        margin = list(t = 60, b = 50)
      ) %>%
      config(displayModeBar = FALSE)
  })

  # ── 2. ACP avec ellipses 80 % ──────────────────────────────────────────────
  output$classes_pca <- renderPlotly({
    m <- prob_matrix()
    pca_res  <- prcomp(m, center = TRUE, scale. = FALSE)
    var_expl <- summary(pca_res)$importance[2, ] * 100

    cd <- classif_data()
    df_pca <- data.frame(
      PC1     = pca_res$x[, 1],
      PC2     = pca_res$x[, 2],
      cluster = as.character(cd[rownames(m), "cluster"]),
      lignee  = rownames(m),
      stringsAsFactors = FALSE
    )

    ellipse_xy <- function(x, y, level = 0.80, n = 80) {
      if (length(x) < 3) return(NULL)
      mu  <- c(mean(x), mean(y))
      S   <- stats::cov(cbind(x, y))
      eig <- eigen(S, symmetric = TRUE)
      if (any(eig$values <= 0)) return(NULL)
      scale_factor <- sqrt(stats::qchisq(level, df = 2))
      theta <- seq(0, 2 * pi, length.out = n)
      circle <- cbind(cos(theta), sin(theta))
      pts <- circle %*% diag(sqrt(eig$values) * scale_factor) %*% t(eig$vectors)
      data.frame(x = pts[, 1] + mu[1], y = pts[, 2] + mu[2])
    }

    fig <- plot_ly()

    for (sub in unique(df_pca$cluster)) {
      sub_df <- df_pca[df_pca$cluster == sub, , drop = FALSE]
      ell    <- ellipse_xy(sub_df$PC1, sub_df$PC2, level = 0.80)
      if (is.null(ell)) next
      coul <- unname(.palette_classes[[sub]])
      fig <- fig %>%
        add_polygons(
          x          = ell$x, y = ell$y,
          line       = list(color = coul, width = 2),
          fillcolor  = paste0(coul, "22"),
          name       = paste(sub, "(zone)"),
          legendgroup = sub,
          showlegend = FALSE,
          hoverinfo  = "skip"
        )
    }

    for (sub in unique(df_pca$cluster)) {
      sub_df <- df_pca[df_pca$cluster == sub, , drop = FALSE]
      fig <- fig %>%
        add_trace(
          data        = sub_df,
          x           = ~PC1, y = ~PC2,
          type        = "scatter", mode = "markers",
          name        = sub,
          legendgroup = sub,
          text        = ~lignee,
          marker      = list(size  = 11,
                             color = unname(.palette_classes[[sub]]),
                             line  = list(color = "white", width = 1),
                             opacity = 0.9),
          hovertemplate = paste0("<b>%{text}</b><br>",
                                 "Sous-type : ", sub, "<br>",
                                 "PC1 : %{x:.3f}<br>",
                                 "PC2 : %{y:.3f}<extra></extra>")
        )
    }

    fig %>%
      layout(
        title = list(text = "ACP des profils de probabilités — séparation des sous-types",
                     font = list(size = 16)),
        xaxis = list(title = sprintf("PC1 (%.1f %%)", var_expl[1]),
                     zeroline = TRUE, zerolinecolor = "#bbb",
                     zerolinewidth = 1),
        yaxis = list(title = sprintf("PC2 (%.1f %%)", var_expl[2]),
                     zeroline = TRUE, zerolinecolor = "#bbb",
                     zerolinewidth = 1),
        legend = list(title = list(text = "Sous-type")),
        margin = list(t = 60, b = 50)
      ) %>%
      config(displayModeBar = FALSE)
  })

  # ── 3. Dendrogramme ────────────────────────────────────────────────────────
  output$classes_dendro <- renderPlot({
    m <- prob_matrix()
    dist_mat <- dist(m, method = "euclidean")
    hc <- hclust(dist_mat, method = "ward.D2")

    par(mar = c(8, 4, 4, 2))
    plot(hc,
         main = "Dendrogramme des lignées (Ward D2, distance euclidienne)",
         xlab = "", sub = "", cex = 0.7,
         col.main = "black", font.main = 2)

    k <- length(unique(classif_data()$cluster))
    rect.hclust(hc, k = k,
                border = unname(.palette_classes[seq_len(k)]))
  })
}
