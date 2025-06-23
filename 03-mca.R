# Multiple Korrespondenzanalyse der Mediendaten des ESS
# Hierbei handelt es sich um ein exemplatisches Analyseprojekt, bei dem es vor allem um die Realisierung in R mit den
# in den einzelnen Lernschritten beschriebenen Techniken geht. Inahltlich wird dabei wenig spannendes herausgearbeitet.
# Stattdessen darf das Projekt als funktionierender Ausgangspunkte für weitere, interessante Forschungsfragen
# betrachtet werden.

# Pakete laden ------------------------------------------------------------

library(readr)
library(tidyr)
library(dplyr)
library(forcats)
library(ggplot2)
# Zur Berechnung geometrischer Datenanalysen:
library(FactoMineR)

# Daten laden -------------------------------------------------------------

df_media <- read_rds("repo/df-media.rds")

# Daten zusammenstellen ---------------------------------------------------

df_mca <-
    df_media |>
    select(
        country,
        news_pol_time:people_help
    ) |>
    mutate(
        country = fct_recode(
            country,
            "Austria" = "AT",
            "Germany" = "DE",
            "Spain" = "ES",
            "Poland" = "PL",
            "Sweden" = "SE"
        ),
        internet_use = fct_recode(
            internet_use,
            "Occasionally to not at all" = "Only occasionally",
            "Occasionally to not at all" = "A few times a week",
            "Occasionally to not at all" = "Never"
        ),
        people_trust = fct_recode(
            people_trust,
            "Great mistrust" = "0",
            "Great mistrust" = "1",
            "Great mistrust" = "2",
            "Great mistrust" = "3",
            "Neither trust nor mistrust" = "4",
            "Neither trust nor mistrust" = "5",
            "Neither trust nor mistrust" = "6",
            "Great trust" = "7",
            "Great trust" = "8",
            "Great trust" = "9",
            "Great trust" = "10"
        ),
        people_fair = fct_recode(
            people_fair,
            "Very unfair" = "0",
            "Very unfair" = "1",
            "Very unfair" = "2",
            "Very unfair" = "3",
            "Neither fair nor unfair" = "4",
            "Neither fair nor unfair" = "5",
            "Neither fair nor unfair" = "6",
            "Very fair" = "7",
            "Very fair" = "8",
            "Very fair" = "9",
            "Very fair" = "10"
        ),
        people_help = fct_recode(
            people_help,
            "Great selfishness" = "0",
            "Great selfishness" = "1",
            "Great selfishness" = "2",
            "Great selfishness" = "3",
            "Neither great helpfulness nor great selfishness" = "4",
            "Neither great helpfulness nor great selfishness" = "5",
            "Neither great helpfulness nor great selfishness" = "6",
            "Great helpfulness" = "7",
            "Great helpfulness" = "8",
            "Great helpfulness" = "9",
            "Great helpfulness" = "10"
        )
    ) |>
    select(
        -news_pol_time,
        -internet_time
    ) |>
    drop_na()
df_mca

# Geometrische Modellierung -----------------------------------------------

# Weiterführende Quellen (Webseiten, Beispiele, Literatur) zur Berechnung finden sich bei der Dokumentation der
# verwendeten Funktion, die aus dem FactoMineR Paket stammt: ?MCA

mca_media <-
    MCA(
        df_mca,
        quali.sup = 1,
        graph = FALSE
    )

# Scree plot anfertigen ---------------------------------------------------
# Dies ist vor allem als kleine, übersichtliche Übung enthalten, die das Zusammenspiel von Ergebnisobjekten und
# Tidyverse-Funktionen illustriert. Ausgehend vom Zugriff auf einzelne MCA-Ergebniswerte bis hin zu deren
# Visualisierung wird die entsprechende "Anleitung" formuliert.

df_eigenwerte <-
    mca_media$eig |>
    as_tibble() |>
    mutate(
        dim = 1:n(),
    )
df_eigenwerte

p_eigenwerte <-
    ggplot(
        data = df_eigenwerte,
        mapping = aes(
            x = dim,
            y = eigenvalue
        )
    ) +
    geom_col(
        alpha = 0.7
    ) +
    geom_path(
        colour = "red",
        size = 2
    ) +
    theme_minimal()
p_eigenwerte

# Ergebnisse speichern ----------------------------------------------------

write_rds(mca_media, "repo/mca-media.rds")

write_rds(p_eigenwerte, "repo/p-eigenwerte.rds")
