# Die ESS Daten werden beschreibend Analysiert
# Das Codebook findet sich im data-Ordner. Dort können weiterführende Informationen zu den einzelnen Variablen eingeholt
# werden. Beispielsweise spezielle und allgemeine Kodierungen sowie inhaltliche Erläuterungen.

# Pakete laden ------------------------------------------------------------

library(readr)
library(dplyr)
library(ggplot2)

# Daten laden -------------------------------------------------------------

df_media <- read_rds("repo/df-media.rds")

# Daten analysieren -------------------------------------------------------

# Wie viele Fälle (Zeilen) und Variablen (Spalten) gibt es?

df_media |> nrow()
#> 17363

df_media |> ncol()
#> 19

# Gibt es fehlende Werte?

df_media |>
    is.na() |>
    any()

# Wie viele Fälle mit fehlenden Werten gibt es?

alle_faelle <-
    df_media |> nrow()

vollstaendige_faelle <-
    df_media |>
    na.omit() |>
    nrow()

fehlende_faelle <-
    alle_faelle - vollstaendige_faelle
fehlende_faelle
#> 3008

fehlende_faelle * 100 / alle_faelle
#> 17,32 %

# Die ersten Zeilen anschauen:

df_media |> head()

# Eine allgemeine Zusammenfassung der einzelnen Spalten:

df_media |> summary()

# Angaben zu den Ländern:

df_media |>
    count(country)

# Angaben zur Internetnutzung inkl. Prozent:

df_media |>
    count(internet_use) |>
    mutate(
        freq = n / sum(n)
    )

# … visuell:

ggplot(
    data = df_media,
    mapping = aes(
        x = internet_use
    )
) +
    geom_bar()

# … mit Angaben zu den realtiven Werten:

df_media_internet_use <-
    df_media |>
    count(internet_use) |>
    mutate(
        freq = round(n / sum(n), digits = 2) |> paste("%")
    )
df_media_internet_use

# Vorsicht, da wir die Daten anpassen und dort bereits summieren, müssen wir eine andere geom-Funktion nutzen und
# deshalb zusätzlich bei den Mappings auch die y-Achse explizit definieren!

ggplot(
    data = df_media_internet_use,
    mapping = aes(
        x = internet_use,
        y = n,
    )
) +
    geom_col() +
    geom_text(
        mapping = aes(
            label = freq
        ),
        nudge_y = 300
    )

# Durchschnittliche Zeit im Internet und für pol. News:

df_media |>
    summarise(
        internet_time = mean(internet_time, na.rm = TRUE),
        news_pol_time = mean(news_pol_time, na.rm = TRUE),
        n = n()
    )

# … nach Ländern:

df_media |>
    group_by(country) |>
    summarise(
        internet_time = mean(internet_time, na.rm = TRUE),
        news_pol_time = mean(news_pol_time, na.rm = TRUE),
        n = n()
    )

# Angaben zur Hilfsbereitschaft der Menschen

df_media |>
    count(people_help)

# … visuell als Balkendiagramm:

ggplot(
    data = df_media,
    mapping = aes(
        x = people_help
    )
) +
    geom_bar() +
    coord_flip()

# … nach Ländern:

ggplot(
    data = df_media,
    mapping = aes(
        x = people_help
    )
) +
    geom_bar() +
    coord_flip() +
    facet_wrap(~country)

# Die Visualsierung wird zwar problemlos dargestellt, weist alkerdings noch ein paar Probleme auf. Zum einen werden die
# fehlenden Werte prominent als NA ausgewiesen, was verbessert werden könnte. Lernschritt 17 liefert hierfür die
# notwendigen Hinweise.
# Zudem ist die Reihenfolge der Werte offenkundig nicht korrekt. auf 0 und 1 folgt 10. Eine Überprüfung des Datentyps
# liefert hierfür die Erklärung, bei people_help handelt es sich aktuell um einen Character, R interpretiert die Zahlen
# daher als Text, was eine numerische Sortierung unmöglich macht. Eine Lösung wäre es, den Datentyp anzupassen
# (Lernschritt 6). Alternativ kann auch die Umwandlung in einen Faktor (Lernschritt 16) erprobt werden.
