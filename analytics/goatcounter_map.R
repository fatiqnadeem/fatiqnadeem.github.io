# goatcounter_map.R
# Downloads the GoatCounter visit export for fatiqnadeem.com and draws a
# world map of visits by country. Requires the API key in the environment
# variable GOATCOUNTER_KEY (Settings > API in the GoatCounter dashboard).
#
# Usage:  Rscript analytics/goatcounter_map.R
# Output: analytics/visits_by_country.csv, analytics/visits_map.png

suppressPackageStartupMessages({
  library(httr2)
  library(readr)
  library(dplyr)
  library(ggplot2)
  library(rnaturalearth)
  library(sf)
})

site_code <- "fatiqnadeem"
base_url  <- sprintf("https://%s.goatcounter.com/api/v0", site_code)
api_key   <- Sys.getenv("GOATCOUNTER_KEY")
if (!nzchar(api_key)) stop("Set GOATCOUNTER_KEY in your environment first.")

auth <- function(req) {
  req |>
    req_headers(Authorization = paste("Bearer", api_key),
                `Content-Type` = "application/json")
}

# 1. Request an export and poll until it is finished ------------------------
export_id <- request(paste0(base_url, "/export")) |>
  auth() |>
  req_body_json(list(start_from_hit_id = 0)) |>
  req_perform() |>
  resp_body_json() |>
  (\(x) x$id)()

repeat {
  status <- request(sprintf("%s/export/%s", base_url, export_id)) |>
    auth() |> req_perform() |> resp_body_json()
  if (isTRUE(status$finished_at != "")) break
  Sys.sleep(2)
}

# 2. Download the gzipped CSV -----------------------------------------------
out_dir <- "analytics"   # run from the repository root
if (!dir.exists(out_dir)) stop("Run this script from the repository root.")
gz_path <- tempfile(fileext = ".csv.gz")
request(sprintf("%s/export/%s/download", base_url, export_id)) |>
  auth() |> req_perform(path = gz_path)
hits <- read_csv(gz_path, show_col_types = FALSE)

# The export carries a "Location" column holding ISO-3166 country code,
# optionally followed by a region code (e.g. "US-CA"). Keep the country part.
by_country <- hits |>
  mutate(iso_a2 = toupper(substr(Location, 1, 2))) |>
  filter(nzchar(iso_a2)) |>
  count(iso_a2, name = "visits") |>
  arrange(desc(visits))

write_csv(by_country, file.path(out_dir, "visits_by_country.csv"))
print(by_country, n = 30)

# 3. Map --------------------------------------------------------------------
world <- ne_countries(scale = "medium", returnclass = "sf") |>
  select(iso_a2, name) |>
  left_join(by_country, by = "iso_a2")

p <- ggplot(world) +
  geom_sf(aes(fill = visits), colour = "grey70", linewidth = 0.1) +
  scale_fill_viridis_c(name = "Visits", trans = "log10", na.value = "grey95") +
  coord_sf(crs = "+proj=robin") +
  labs(title = "fatiqnadeem.com visitors by country",
       caption = sprintf("Source: GoatCounter export, %s", Sys.Date())) +
  theme_void() +
  theme(legend.position = "bottom")

ggsave(file.path(out_dir, "visits_map.png"), p, width = 10, height = 6, dpi = 200)
cat("Saved", file.path(out_dir, "visits_map.png"), "\n")
