# Visualisierung der MCA Ergebnisse der ESS Analyse

# Pakete laden ------------------------------------------------------------

library(readr)
library(dplyr)
library(ggplot2)
library(FactoMineR)
# Typografisch optimierte ggplot2-Themes:
library(hrbrthemes)

# Eigene Funktionen -------------------------------------------------------

my_plot_prep <- function(plot, title = "", subtitle = "", caption = "") {
    plot +
        xlab("Achse 1") +
        ylab("Achse 2") +
        labs(
            title = title,
            subtitle = subtitle,
            caption = caption
        ) +
        theme_ipsum()
}


# Daten laden -------------------------------------------------------------

mca_media <- read_rds("repo/mca-media.rds")

# Wolke der Individuen ----------------------------------------------------

# Zur Visualisierung der Individuen wird zunächst auf die Plot-Funktionen das Pakets FactoMineR zurückgegriffen:

p_mca_media_ind <-
    mca_media |>
    plot(
        choix = "ind",
        invisible = c("var", "quali.sup"),
        label = "none"
    )
p_mca_media_ind

# Da diese auf ggplot2 basieren, kann mit den resultierenden Plot-Objekten wie gehabt gearbeitet werden.
# In diesem Fall wird die zuvor selbst erstellte Funktion genutzt:

p_mca_media_ind_final <-
    p_mca_media_ind |>
    my_plot_prep(
        title = "Media use and trust",
        subtitle = "Cloud of individuals",
        caption = "Data: ESS Round 10"
    )
p_mca_media_ind_final

# Wolke der Kategorien ----------------------------------------------------

p_mca_media_cat <-
    mca_media |>
    plot(
        choix = "ind",
        invisible = c("ind", "quali.sup"),
        col.var = "black",

    )
p_mca_media_cat

# Da diese auf ggplot2 basieren, kann mit den resultierenden Plot-Objekten wie gehabt gearbeitet werden. Die selbst
# erstellte Funktion ermöglicht es den selben Code einfach in mehreren Zusammenhängen zu verwenden:

p_mca_media_cat_final <-
    p_mca_media_cat |>
    my_plot_prep(
        title = "Media use and trust",
        subtitle = "Cloud of categories",
        caption = "Data: ESS Round 10"
    )
p_mca_media_cat_final

# Passive Variablen -------------------------------------------------------

p_mca_media_cat_sup <-
    mca_media |>
    plot(
        choix = "ind",
        invisible = c("ind", "var"),
        col.var = "black",

    )
p_mca_media_cat_sup

p_mca_media_cat_sup_final <-
    p_mca_media_cat_sup |>
    my_plot_prep(
        title = "Media use and trust",
        subtitle = "Supplementary variable: country",
        caption = "Data: ESS Round 10"
    )
p_mca_media_cat_sup_final


# Passive Variable: Wolken der Individuen ---------------------------------
# Im Folgenden geht es darum, eine komplexe Visualisierung Schritt für Schritt zu konstruieren. Angefangen bei einem
# Ergebnisobjekt über spezielle Berechnungen bis hin zum finalen Plot.

# Subsetting der Koordination aller Individuen für die ersten beiden Achsen und Ergänzung der Spalte country:
#
df_mca_coord <-
    mca_media$ind$coord |>
    as_tibble() |>
    select(
        dim_1 = 1,
        dim_2 = 2
    ) |>
    mutate(
        country = mca_media$call$X$country
    ) |>
    filter(
        country != "Spain"
    )
df_mca_coord

# Berechnung der Koordinaten-Mittelwerte. Dabei handelt es sich um die geometrischen Mittelpunkte der später
# kalkulierten Ellipsen:

df_mca_coord_mean <-
    df_mca_coord |>
    group_by(country) |>
    summarise(
        dim_1 = mean(dim_1),
        dim_2 = mean(dim_2),
        n = n()
    )
df_mca_coord_mean

# Konzentration landesspezifischer Konzentrationsellipsen:

p_mca_media_ind_sup_final <-
    ggplot(
        data = df_mca_coord,
        mapping = aes(
            x = dim_1,
            y = dim_2,
            colour = country
        )
    ) +
    geom_hline(
        yintercept = 0,
        colour = "#cccccc"
    ) +
    geom_vline(
        xintercept = 0,
        colour = "#cccccc"
    ) +
    geom_point(
        alpha = 0.3,
        show.legend = FALSE
    ) +
    stat_ellipse(
        type = "norm",
        level = 0.8647,
        show.legend = FALSE
    ) +
    geom_point(
        data = df_mca_coord_mean,
        mapping = aes(
            size = n
        ),
        shape = 18,
        show.legend = FALSE
    ) +
    scale_size(
        range = c(6, 12)
    ) +
    facet_wrap(~country) +
    xlab("Achse 1") +
    ylab("Achse 2") +
    labs(
        title = "Media use and trust",
        subtitle = "Concentration ellipses: country",
        caption = "Data: ESS Round 10"
    ) +
    theme_ipsum(
        grid = FALSE,
    ) +
    theme(
        panel.border = element_rect(
            colour = "black", fill = NA
        ),
        strip.text = element_text(
            face = "bold"
        )
    )
p_mca_media_ind_sup_final

# Grafiken speichern ------------------------------------------------------

ggsave(
    "output/Media use and trust. Cloud of individuals.png",
    plot = p_mca_media_ind_final,
    width = 20,
    height = 20,
    units = "cm",
    dpi = 300
)

ggsave(
    "output/Media use and trust. Cloud of categories.png",
    plot = p_mca_media_cat_final,
    width = 20,
    height = 20,
    units = "cm",
    dpi = 300
)

ggsave(
    "output/Media use and trust. Concentration ellipses.png",
    plot = p_mca_media_ind_sup_final,
    width = 20,
    height = 20,
    units = "cm",
    dpi = 300
)

# Plots speichern ---------------------------------------------------------

write_rds(p_mca_media_ind_sup_final, "repo/p-mca-media-ind-sup-final.rds")

