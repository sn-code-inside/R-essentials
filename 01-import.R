# Import von ESS-Daten der 10. Erhebungswelle
# © Daten: https://www.europeansocialsurvey.org
# Das Codebook findet sich im data-Ordner. Dort können weiterführende Informationen zu den einzelnen Variablen eingeholt
# werden. Beispielsweise spezielle und allgemeine Kodierungen sowie inhaltliche Erläuterungen.

# Pakete laden ------------------------------------------------------------

library(readr)
library(dplyr)
library(forcats)

# Daten laden -------------------------------------------------------------

df_media <-
    read_csv(
        file = "data/ESS10SC-subset.csv",
        col_types = cols(
            name = col_character(),
            essround = col_double(),
            edition = col_double(),
            # Die Spalte wird als spezielle Datums-Spalte festgelegt:
            proddate = col_date(format = "%d.%m.%Y"),
            idno = col_double(),
            # Die Spalte wird als Faktor festgelegt:
            cntry = col_factor(),
            dweight = col_double(),
            pspwght = col_double(),
            pweight = col_double(),
            anweight = col_double(),
            prob = col_double(),
            stratum = col_double(),
            psu = col_double(),
            nwspol = col_double(),
            # Die Spalte wird als Faktor festgelegt:
            netusoft = col_factor(),
            netustm = col_double(),
            ppltrst = col_character(),
            pplfair = col_character(),
            pplhlp = col_character()
        ),
        # Fehlende Werte werden global definiert:
        na = c("", "NA", "6666", "7777", "9999", "77", "99")
    )

# Daten aufbereiten -------------------------------------------------------

df_media <-
    df_media |>
    rename(
        prod_date = proddate,
        id_no = idno,
        country = cntry,
        news_pol_time = nwspol,
        internet_use = netusoft,
        internet_time = netustm,
        people_trust = ppltrst,
        people_fair = pplfair,
        people_help = pplhlp
    ) |>
    mutate(
        internet_use = fct_recode(
            internet_use,
            "Never" = "1",
            "Only occasionally" = "2",
            "A few times a week" = "3",
            "Most days" = "4",
            "Every day" = "5",
            NULL = "7",
            "Don't know" = "8",
            NULL = "9"
        )
    )
df_media

# Daten speichern ---------------------------------------------------------

write_rds(df_media, "repo/df-media.rds", compress = "gz")
