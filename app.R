# ============================================================
# NETEJA DE L'ENTORN I ALLIBERAMENT DE MEMÒRIA
# ============================================================
rm(list = ls())
gc()


suppressPackageStartupMessages({
  library(shiny)
  library(shinydashboard)
  library(shinydashboardPlus)
  library(leaflet)
  library(leaflet.extras)
  library(dplyr)
  library(tidyr)
  library(plotly)
  library(scales)
  library(sf)
  library(rlang)
  library(shinyWidgets)
  library(htmlwidgets)
  library(htmltools)
})

# ============================================================
# FUNCIONS AUXILIARS
# ============================================================

netejar_coord = function(x) {
  x = gsub(",", ".", trimws(as.character(x)))
  x = gsub("\\.(?=.*\\.)", "", x, perl = TRUE)
  suppressWarnings(as.numeric(x))
}

dist_haversine_km = function(lon1, lat1, lon2, lat2) {
  R    = 6371
  dlat = (lat2 - lat1) * pi / 180
  dlon = (lon2 - lon1) * pi / 180
  a    = sin(dlat / 2)^2 + cos(lat1 * pi / 180) * cos(lat2 * pi / 180) * sin(dlon / 2)^2
  R * 2 * atan2(sqrt(a), sqrt(1 - a))
}

`%||%` = function(a, b) if (!is.null(a) && length(a) > 0 && !is.na(a[1])) a else b

cat_labels = c(
  "EINF1C" = "Escola Bressol",
  "EINF2C" = "Educació Infantil",
  "EPRI"   = "Educació Primària",
  "EE"     = "Educació Especial"
)

cat_label = function(x) ifelse(x %in% names(cat_labels), cat_labels[x], x)

outliers_df = function(df, grup_col, val_col) {
  df %>%
    filter(!is.na(.data[[val_col]]), !is.na(.data[[grup_col]])) %>%
    group_by(.data[[grup_col]]) %>%
    mutate(
      q1  = quantile(.data[[val_col]], 0.25),
      q3  = quantile(.data[[val_col]], 0.75),
      iqr = q3 - q1,
      es_outlier = .data[[val_col]] < (q1 - 1.5*iqr) | .data[[val_col]] > (q3 + 1.5*iqr)
    ) %>%
    ungroup() %>%
    filter(es_outlier)
}

icqa_score_fn = function(val, breaks, scores) {
  vapply(val, function(v) {
    if (is.na(v)) return(NA_real_)
    idx = findInterval(v, breaks, rightmost.closed = TRUE)
    scores[min(idx + 1, length(scores))]
  }, numeric(1))
}

calcular_idw = function(cont) {
  conc_est = stations_day %>% filter(CONTAMINANT == cont) %>%
    group_by(CODI.EOI) %>% summarise(conc = mean(concentracio, na.rm = TRUE), .groups = "drop")
  dist_mat_full %>% left_join(conc_est, by = "CODI.EOI") %>% filter(!is.na(conc)) %>%
    group_by(idx_centre) %>%
    summarise(conc_est = { w = 1 / dist^PODER_IDW; sum(w * conc) / sum(w) }, .groups = "drop") %>%
    mutate(contaminant = cont)
}

assign_provincia = function(comarca) {
  if (is.na(comarca) || comarca == "") return("Barcelona")
  if (!is.na(comarca_prov_map[comarca])) return(comarca_prov_map[[comarca]])
  for (nm in names(comarca_prov_map)) {
    if (grepl(nm, comarca, ignore.case=TRUE) || grepl(comarca, nm, ignore.case=TRUE))
      return(comarca_prov_map[[nm]])
  }
  return("Barcelona")
}

comarca_prov_map = c(
  "Barcelonès"="Barcelona","Baix Llobregat"="Barcelona","Vallès Occidental"="Barcelona",
  "Maresme"="Barcelona","Vallès Oriental"="Barcelona","Bages"="Barcelona",
  "Berguedà"="Barcelona","Anoia"="Barcelona","Garraf"="Barcelona",
  "Alt Penedès"="Barcelona","Osona"="Barcelona","Moianès"="Barcelona",
  "Baix Penedès"="Barcelona",
  "Gironès"="Girona","Selva"="Girona","Alt Empordà"="Girona",
  "Baix Empordà"="Girona","Garrotxa"="Girona","Ripollès"="Girona",
  "Pla de l'Estany"="Girona","Cerdanya"="Girona","La Selva"="Girona",
  "Segrià"="Lleida","Noguera"="Lleida","Pla d'Urgell"="Lleida",
  "Urgell"="Lleida","Segarra"="Lleida","Alt Urgell"="Lleida",
  "Solsonès"="Lleida","Pallars Jussà"="Lleida","Pallars Sobirà"="Lleida",
  "Alta Ribagorça"="Lleida","Val d'Aran"="Lleida","Garrigues"="Lleida",
  "Les Garrigues"="Lleida","Pla de l'Urgell"="Lleida",
  "Tarragonès"="Tarragona","Baix Camp"="Tarragona","Alt Camp"="Tarragona",
  "Conca de Barberà"="Tarragona","Priorat"="Tarragona","Ribera d'Ebre"="Tarragona",
  "Baix Ebre"="Tarragona","Montsià"="Tarragona","Terra Alta"="Tarragona",
  "Baix Penedès"="Tarragona"
)

# ============================================================
# CÀRREGA DE LES DADES
# ============================================================
message("Carregant dades...")

quali_aire_raw = read.csv("data/Qualitat_de_l’aire_als_punts_de_mesurament_automàtics_de_la_Xarxa_de_Vigilància_i_Previsió_de_la_Contaminació_Atmosfèrica_20260525.csv", stringsAsFactors = FALSE)
equip_raw = read.csv("data/Equipaments_de_Catalunya_20260525.csv", stringsAsFactors = FALSE)

# ============================================================
# PREPROCESSAMENT DE LES DADES
# ============================================================
stations_long = quali_aire_raw %>%
  rename_with(toupper) %>%
  mutate(
    LATITUD  = netejar_coord(LATITUD),
    LONGITUD = netejar_coord(LONGITUD),
    DATA     = as.Date(DATA, tryFormats = c("%d/%m/%Y", "%Y-%m-%d"))
  ) %>%
  filter(
    between(LATITUD,  40.3, 43.0),
    between(LONGITUD,  0.1,  4.0),
    CONTAMINANT %in% c("NO2", "PM10", "PM2.5", "O3")
  ) %>%
  mutate(across(matches("^[HX]\\d{2}"), ~ {
    v = as.character(.x)
    v[v %in% c("", "-", "ND", "N/D", "NA", "NULL", " ")] = NA
    suppressWarnings(as.numeric(v))
  })) %>%
  pivot_longer(cols = matches("^[HX]\\d{2}"),
               names_to = "hora_col", values_to = "concentracio") %>%
  filter(!is.na(concentracio)) %>%
  mutate(hora = as.integer(gsub("\\D", "", hora_col)))

data_max     = max(stations_long$DATA, na.rm = TRUE)
stations_day = stations_long %>% filter(DATA == data_max)

area_urbana_estacio = quali_aire_raw %>%
  rename_with(toupper) %>%
  mutate(LATITUD = netejar_coord(LATITUD), LONGITUD = netejar_coord(LONGITUD)) %>%
  filter(between(LATITUD, 40.3, 43.0), between(LONGITUD, 0.1, 4.0),
         CONTAMINANT %in% c("NO2", "PM10", "PM2.5", "O3")) %>%
  {
    cols_sel = intersect(c("CODI.EOI","NOM.ESTACIO","LATITUD","LONGITUD","TIPUS.ESTACIO","AREA.URBANA"), names(.))
    select(., all_of(cols_sel))
  } %>% distinct()

te_area_urbana = "AREA.URBANA" %in% names(area_urbana_estacio)

area_urbana_estacio = area_urbana_estacio %>%
  mutate(entorn_estacio = if (te_area_urbana) case_when(
    grepl("urb", AREA.URBANA, ignore.case = TRUE) & !grepl("sub", AREA.URBANA, ignore.case = TRUE) ~ "Urbà",
    grepl("sub", AREA.URBANA, ignore.case = TRUE) ~ "Suburbà",
    grepl("rural|fons|camp", AREA.URBANA, ignore.case = TRUE) ~ "Rural",
    grepl("industr", AREA.URBANA, ignore.case = TRUE) ~ "Suburbà",
    TRUE ~ "Rural"
  ) else NA_character_)

stations_geo = stations_day %>%
  select(CODI.EOI, NOM.ESTACIO, LATITUD, LONGITUD, TIPUS.ESTACIO) %>%
  distinct() %>%
  left_join(area_urbana_estacio %>% select(CODI.EOI, entorn_estacio) %>% distinct(), by = "CODI.EOI")

dates_disponibles  = sort(unique(stations_long$DATA))
dates_setmana      = if (length(dates_disponibles) >= 7) tail(dates_disponibles, 7) else dates_disponibles
data_inici_setmana = min(dates_setmana)
data_fi_setmana    = max(dates_setmana)
stations_setmana   = stations_long %>% filter(DATA %in% dates_setmana)

perfil_setmana_raw = stations_setmana %>%
  group_by(CONTAMINANT, DATA, hora, CODI.EOI, NOM.ESTACIO, LATITUD, LONGITUD) %>%
  mutate(
    timestamp = as.POSIXct(paste(format(as.Date(DATA), "%Y-%m-%d"),
                                 sprintf("%02d:00:00", as.integer(hora))),
                           format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
  ) %>%
  filter(!is.na(timestamp))

CATS_EDUC = c("EINF1C", "EINF2C", "EPRI", "EE")

centres_raw_all = equip_raw %>%
  rename_with(toupper) %>%
  filter(grepl("educaci", CATEGORIA, ignore.case = TRUE),
         grepl("EINF1C|EINF2C|EPRI|EE", CATEGORIA))

centres_raw_all$LATITUD[centres_raw_all$IDEQUIPAMENT==29647132]='41,37064'
centres_raw_all$GEOREFERÈNCIA[centres_raw_all$IDEQUIPAMENT==29647132] = "POINT (2.119867 41.37064)"

centres_cats = centres_raw_all %>%
  select(IDEQUIPAMENT, CATEGORIA) %>%
  distinct() %>%
  group_by(IDEQUIPAMENT) %>%
  summarise(
    te_EINF1C  = any(grepl("EINF1C", CATEGORIA)),
    te_EINF2C  = any(grepl("EINF2C", CATEGORIA)),
    te_EPRI    = any(grepl("EPRI",   CATEGORIA)),
    te_EE      = any(grepl("EE",     CATEGORIA)),
    categories = paste(na.omit(c(
      if (any(grepl("EINF1C", CATEGORIA))) "Escola Bressol",
      if (any(grepl("EINF2C", CATEGORIA))) "Ed. Infantil",
      if (any(grepl("EPRI",   CATEGORIA))) "Ed. Primària",
      if (any(grepl("EE",     CATEGORIA))) "Ed. Especial"
    )), collapse = ", "),
    .groups = "drop"
  ) %>%
  mutate(IDEQUIPAMENT = as.character(IDEQUIPAMENT))

centres = centres_raw_all %>%
  mutate(
    lat = netejar_coord(LATITUD), lon = netejar_coord(LONGITUD),
    titularitat = case_when(
      grepl("P\\.blic|Públic|Public", PROPIETATS, ignore.case = TRUE) ~ "Pública",
      TRUE ~ "Privada/Concertada")
  ) %>%
  select(id_centre = IDEQUIPAMENT, nom = NOM, municipi = POBLACIO,
         comarca = COMARCA, titularitat, lon, lat) %>%
  filter(!is.na(lat), !is.na(lon), between(lat, 40.3, 43.0), between(lon, 0.1, 4.0)) %>%
  distinct(id_centre, .keep_all = TRUE) %>%
  mutate(
    id_centre = as.character(id_centre),
    provincia = sapply(comarca, assign_provincia)
  ) %>%
  left_join(centres_cats, by = c("id_centre" = "IDEQUIPAMENT")) %>%
  mutate(
    te_EINF1C  = ifelse(is.na(te_EINF1C), FALSE, te_EINF1C),
    te_EINF2C  = ifelse(is.na(te_EINF2C), FALSE, te_EINF2C),
    te_EPRI    = ifelse(is.na(te_EPRI),   FALSE, te_EPRI),
    te_EE      = ifelse(is.na(te_EE),     FALSE, te_EE),
    categories = ifelse(is.na(categories) | categories == "", "Altres", categories)
  )

message(sprintf("  Centres: %d  |  Estacions: %d", nrow(centres), nrow(stations_geo)))

# ============================================================
# CÀLCUL DE DISTÀNCIES CENTRE–ESTACIÓ I ESTIMACIÓ PER IDW
# ============================================================
RADI_KM    = 10
PODER_IDW  = 2
CONTAM_VEC = c("NO2", "PM2.5", "PM10", "O3")

centres_sf  = st_as_sf(centres,      coords = c("lon",      "lat"),     crs = 4326)
stations_sf = st_as_sf(stations_geo, coords = c("LONGITUD", "LATITUD"), crs = 4326)
dist_m      = st_distance(centres_sf, stations_sf)

centres$dist_min_km         = apply(dist_m, 1, min) / 1000
centres$te_cobertura        = centres$dist_min_km <= RADI_KM
centres$idx_estacio_propera = apply(dist_m, 1, which.min)

dist_mat_full = expand.grid(
  idx_centre = seq_len(nrow(centres)), idx_estacio = seq_len(nrow(stations_geo)),
  stringsAsFactors = FALSE) %>%
  mutate(
    dist     = dist_haversine_km(centres$lon[idx_centre], centres$lat[idx_centre],
                                 stations_geo$LONGITUD[idx_estacio], stations_geo$LATITUD[idx_estacio]),
    CODI.EOI = stations_geo$CODI.EOI[idx_estacio]
  ) %>% filter(dist <= RADI_KM, dist > 0)

conc_idw = bind_rows(lapply(CONTAM_VEC, calcular_idw)) %>%
  pivot_wider(names_from = contaminant, values_from = conc_est, names_prefix = "conc_") %>%
  rename_with(~ gsub("conc_", "", .x), starts_with("conc_"))

centres_data = centres %>%
  mutate(idx_centre = seq_len(n())) %>%
  left_join(conc_idw, by = "idx_centre") %>%
  select(-idx_centre)

# ============================================================
# CLASSIFICACIÓ DE L'ENTORN DEL CENTRE (URBÀ, SUBURBÀ, RURAL)
# ============================================================
entorn_vec = stations_geo$entorn_estacio
centres_data$entorn = entorn_vec[centres$idx_estacio_propera]
centres_data$entorn = ifelse(is.na(centres_data$entorn), "Rural", centres_data$entorn)

# ============================================================
# CÀLCUL DE L'ÍNDEX ICQA
# ============================================================
centres_data <- centres_data %>%
  mutate(
    sc_NO2  = icqa_score_fn(NO2,     c(0, 40, 90, 120, 230, 340), 1:6),
    sc_PM10 = icqa_score_fn(PM10,    c(0, 20, 40,  50, 100, 150), 1:6),
    sc_PM25 = icqa_score_fn(`PM2.5`, c(0, 10, 20,  25,  50,  75), 1:6),
    sc_O3   = icqa_score_fn(O3,      c(0, 50,100, 130, 240, 380), 1:6),
    
    ICQA_score = pmax(sc_NO2, sc_PM10, sc_PM25, sc_O3, na.rm = TRUE),
    
    ICQA = case_when(
      is.na(ICQA_score) ~ "Sense dades",
      ICQA_score == 1   ~ "Bona",
      ICQA_score == 2   ~ "Raonablement bona",
      ICQA_score == 3   ~ "Regular",
      ICQA_score == 4   ~ "Desfavorable",
      ICQA_score == 5   ~ "Molt desfavorable",
      ICQA_score == 6   ~ "Extremadament desfavorable",
      TRUE              ~ "Sense dades"
    )
  ) %>%
  select(-sc_NO2, -sc_PM10, -sc_PM25, -sc_O3, -ICQA_score)

# ============================================================
# CONCENTRACIONS ACTUALS PER ESTACIÓ
# ============================================================
conc_actual_est = stations_day %>%
  group_by(CODI.EOI, CONTAMINANT) %>%
  summarise(conc = mean(concentracio, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = CONTAMINANT, values_from = conc)
for (col in c("NO2","PM10","PM2.5","O3")) if (!col %in% names(conc_actual_est)) conc_actual_est[[col]] = NA_real_
stations_geo = stations_geo %>% left_join(conc_actual_est, by = "CODI.EOI")

# ============================================================
# ASSIGNACIÓ DE COMARCA A LES ESTACIONS
# ============================================================
centre_comarca_idx = dist_mat_full %>%
  left_join(centres_data %>% mutate(idx_centre = seq_len(n())) %>% select(idx_centre, comarca),
            by = "idx_centre") %>%
  group_by(CODI.EOI) %>% slice_min(dist, n = 1, with_ties = FALSE) %>% ungroup() %>%
  select(CODI.EOI, comarca)
perfil_setmana_raw = perfil_setmana_raw %>% left_join(centre_comarca_idx, by = "CODI.EOI")

# ============================================================
# ESTADÍSTIQUES PER COMARCA
# ============================================================
comarca_stats = centres_data %>%
  filter(!is.na(comarca)) %>%
  group_by(comarca, provincia) %>%
  summarise(
    n_centres     = n(),
    n_cob         = sum(te_cobertura, na.rm = TRUE),
    pct_cobertura = round(n_cob / n_centres * 100, 1),
    NO2_mitj      = round(mean(NO2,    na.rm = TRUE), 1),
    PM25_mitj     = round(mean(`PM2.5`,na.rm = TRUE), 1),
    PM10_mitj     = round(mean(PM10,   na.rm = TRUE), 1),
    O3_mitj       = round(mean(O3,     na.rm = TRUE), 1),
    n_desfav      = sum(ICQA %in% c("Desfavorable","Molt desfavorable","Extremadament desfavorable"), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    score_NO2  = ifelse(is.na(NO2_mitj),  0, NO2_mitj  / 40),
    score_PM25 = ifelse(is.na(PM25_mitj), 0, PM25_mitj / 25),
    score_PM10 = ifelse(is.na(PM10_mitj), 0, PM10_mitj / 20),
    score_O3   = ifelse(is.na(O3_mitj),   0, O3_mitj   / 100),
    score_global = 0.35*score_NO2 + 0.25*score_PM25 + 0.25*score_PM10 + 0.15*score_O3
  )

comarca_bbox = centres_data %>%
  filter(!is.na(comarca), !is.na(lat), !is.na(lon)) %>%
  group_by(comarca) %>%
  summarise(lat_min = min(lat), lat_max = max(lat),
            lon_min = min(lon), lon_max = max(lon), .groups = "drop")

# ============================================================
# PALETES DE COLORS
# ============================================================
icqa_pal = c(
  "Bona"                       = "#2795F5",
  "Raonablement bona"          = "#52b788",
  "Regular"                    = "#f4d03f",
  "Desfavorable"               = "#FF0000",
  "Molt desfavorable"          = "#A32E00",
  "Extremadament desfavorable" = "#8e44ad",
  "Sense dades"                = "#aaaaaa"
)
icqa_nivells = names(icqa_pal)

pal_icqa_leaf = colorFactor(palette = unname(icqa_pal), levels = icqa_nivells, na.color = "#aaaaaa")
color_tit     = c("Pública" = "#2980b9", "Privada/Concertada" = "#e74c3c")
color_entorn  = c("Urbà" = "#8e44ad", "Suburbà" = "#e67e22", "Rural" = "#27ae60")
color_cat     = c(
  "Escola Bressol"    = "#3498db",
  "Educació Infantil" = "#9b59b6",
  "Educació Primària" = "#27ae60",
  "Educació Especial" = "#e67e22"
)

# Icona per a les estacions (triangle)
triangle_icon = makeIcon(
  iconUrl = paste0(
    "data:image/svg+xml;charset=UTF-8,",
    "%3Csvg%20xmlns%3D%27http%3A%2F%2Fwww.w3.org%2F2000%2Fsvg%27%20width%3D%2720%27%20height%3D%2720%27%3E",
    "%3Cpolygon%20points%3D%2710%2C2%2018%2C18%202%2C18%27%20fill%3D%27%23000000%27%20stroke%3D%27%23333333%27%20stroke-width%3D%271.5%27%2F%3E",
    "%3C%2Fsvg%3E"
  ),
  iconWidth = 15,
  iconHeight = 15,
  iconAnchorX = 10,
  iconAnchorY = 18
)

# Mapeig de nivell ICQA a lletra
icqa_lletra = c(
  "Bona"                       = "B",
  "Raonablement bona"          = "RB",
  "Regular"                    = "R",
  "Desfavorable"               = "D",
  "Molt desfavorable"          = "MD",
  "Extremadament desfavorable" = "ED",
  "Sense dades"                = "?"
)

# Funció que retorna l'URL de la icona SVG (codificada correctament)
icona_icqa_url = function(color_hex, lletra) {
  # Assegurem valors per defecte
  if (is.na(lletra)) lletra <- "?"
  if (is.na(color_hex)) color_hex <- "#aaaaaa"
  
  # Codifiquem el color
  svg = sprintf(
    '<svg xmlns="http://www.w3.org/2000/svg" width="22" height="22">
       <circle cx="11" cy="11" r="10" fill="%s" stroke="white" stroke-width="0.5"/>
       <text x="11" y="15" text-anchor="middle"
             font-size="10" font-family="sans-serif"
             font-weight="bold" fill="white">%s</text>
     </svg>',
    color_hex, lletra
  )
  # Eliminem salts de línia i espais innecessaris
  svg <- gsub("\\s+", " ", svg)
  paste0("data:image/svg+xml;charset=UTF-8,", URLencode(svg, reserved = TRUE))
}

message("Preprocessat finalitzat")

# ============================================================
# CSS PERSONALITZAT (amb contrast millorat)
# ============================================================
custom_css = "
@import url('https://fonts.googleapis.com/css2?family=Syne:wght@400;600;700;800&family=DM+Serif+Display:ital@0;1&family=DM+Sans:wght@300;400;500&display=swap');

body, .content-wrapper, .main-sidebar { font-family: 'DM Sans', sans-serif !important; }

.skin-blue .main-header .logo {
  background: #06111f !important;
  font-family: 'Syne', sans-serif !important;
  font-weight: 800; font-size: 12px; letter-spacing: 1px; text-transform: uppercase;
}
.skin-blue .main-header .navbar { background: #06111f !important; }
.skin-blue .main-sidebar { background: #06111f; }
.skin-blue .sidebar-menu > li.active > a {
  background: rgba(255,77,77,0.18) !important;
  border-left: 3px solid #ff4d4d !important;
  color: #fff !important;
}
.skin-blue .sidebar-menu > li > a:hover { background: rgba(255,255,255,0.06) !important; }
.sidebar-menu > li > a {
  color: #a8bdd1 !important;  /* contrast 4.6:1 sobre #06111f */
  font-size: 12px !important;
  font-family: 'Syne', sans-serif !important; letter-spacing: 0.5px;
}

.sidebar-menu > li:nth-child(2) > a::before { content: '01 '; color: #ff4d4d; font-weight: 800; }
.sidebar-menu > li:nth-child(3) > a::before { content: '02 '; color: #ff4d4d; font-weight: 800; }
.sidebar-menu > li:nth-child(4) > a::before { content: '03 '; color: #ff4d4d; font-weight: 800; }
.sidebar-menu > li:nth-child(5) > a::before { content: '04 '; color: #ff4d4d; font-weight: 800; }
.sidebar-menu > li:nth-child(6) > a::before { content: '  '; }

.box { border-radius: 10px; box-shadow: 0 2px 16px rgba(0,0,0,0.07); border: none; }
.box.box-primary > .box-header { background: #06111f; color: #fff; border-radius: 10px 10px 0 0; }
.box.box-info > .box-header    { background: #0e2238; color: #fff; border-radius: 10px 10px 0 0; }
.box.box-warning > .box-header { background: #2c1a00; color: #fff; border-radius: 10px 10px 0 0; }
.box.box-danger > .box-header  { background: #2c0000; color: #fff; border-radius: 10px 10px 0 0; }
.box.box-success > .box-header { background: #0a2e1a; color: #fff; border-radius: 10px 10px 0 0; }
.box-title { font-family: 'Syne', sans-serif !important; font-weight: 700; font-size: 11px; letter-spacing: 1px; text-transform: uppercase; }

.content-wrapper { background: #f3f5f8; }
.nav-tabs > li > a { font-family: 'Syne', sans-serif !important; font-size: 11px; font-weight: 700; text-transform: uppercase; letter-spacing: 0.8px; }
.shiny-input-container label { font-family: 'Syne', sans-serif !important; font-size: 10px !important; font-weight: 700 !important; text-transform: uppercase !important; letter-spacing: 0.8px !important; color: #333333 !important; }  /* contrast 12.6:1 */
.selectize-input { font-size: 12px; border-radius: 6px !important; }
.footer-note { font-size: 10px; color: #3a4a5a; padding: 8px 15px 12px; border-top: 1px solid rgba(255,255,255,0.05); margin-top: 8px; }  /* contrast 5.1:1 */

/* ── CERCADOR (bottom-center) ── */
#cerca_wrapper {
  position: absolute;
  bottom: 40px;
  left: 50%;
  transform: translateX(-50%);
  z-index: 1002;
  background: rgba(255,255,255,0.97);
  border-radius: 12px;
  box-shadow: 0 6px 28px rgba(0,0,0,0.22);
  padding: 12px 14px;
  width: 360px;
  backdrop-filter: blur(10px);
}
#cerca_input {
  width: 100%; border: 1.5px solid #e0e7ef; border-radius: 8px;
  padding: 9px 12px 9px 36px; font-size: 13px; outline: none;
  box-sizing: border-box; font-family: 'DM Sans', sans-serif;
  background: white url('data:image/svg+xml,<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"14\" height=\"14\" viewBox=\"0 0 24 24\" fill=\"none\" stroke=\"%23aaa\" stroke-width=\"2.5\"><circle cx=\"11\" cy=\"11\" r=\"8\"/><line x1=\"21\" y1=\"21\" x2=\"16.65\" y2=\"16.65\"/></svg>') no-repeat 12px center;
}
#cerca_input:focus { border-color: #ff4d4d; box-shadow: 0 0 0 3px rgba(255,77,77,0.1); }
.cerca-item { padding: 8px 10px; cursor: pointer; font-size: 12px; border-bottom: 1px solid #f0f4f8; line-height: 1.5; border-radius: 5px; }
.cerca-item:hover { background: #fff5f5; }
#cerca_llista { max-height: 220px; overflow-y: auto; margin-top: 8px; }
.cerca-tag { display: inline-block; font-size: 9px; font-weight: 700; padding: 1px 5px; border-radius: 3px; margin-right: 4px; font-family: 'Syne', sans-serif; text-transform: uppercase; letter-spacing: 0.5px; }
.tag-comarca  { background: #e8f4fd; color: #0d5f9b; }
.tag-municipi { background: #e8fdf0; color: #0a5a30; }
.tag-centre   { background: #fdf0f0; color: #8b0000; }

/* ── TARGETES KPI ── */
.kpi-row { display: flex; gap: 10px; margin-bottom: 12px; flex-wrap: wrap; }
.kpi-card {
  flex: 1; min-width: 120px; background: white; border-radius: 10px;
  padding: 14px 16px; box-shadow: 0 2px 12px rgba(0,0,0,0.07);
  border-top: 4px solid #ff4d4d;
}
.kpi-card.green  { border-top-color: #2dc653; }
.kpi-card.orange { border-top-color: #e67e22; }
.kpi-card.blue   { border-top-color: #2980b9; }
.kpi-card.purple { border-top-color: #8e44ad; }
.kpi-val { font-family: 'Syne', sans-serif; font-size: 26px; font-weight: 800; color: #06111f; line-height: 1; }
.kpi-lbl { font-size: 11px; color: #555555; margin-top: 4px; line-height: 1.4; }  /* contrast 7.0:1 */

/* ── INTERPRETACIÓ ── */
.interpret-box {
  background: linear-gradient(135deg, #fffbf0, #fff);
  border: 1px solid #f0d9a0; border-left: 4px solid #e67e22;
  border-radius: 8px; padding: 12px 16px; font-size: 12.5px;
  line-height: 1.8; color: #444; margin: 10px 0;
}
.interpret-box .ib-icon { font-size: 16px; margin-right: 6px; vertical-align: middle; }
.interpret-box strong { color: #222; }
.interpret-box .hl   { color: #c0392b; font-weight: 700; }
.interpret-box .hl-g { color: #1a7a48; font-weight: 700; }

/* ── BREADCRUMB ── */
.hier-bc {
  font-family: 'Syne', sans-serif; font-size: 11px; color: #aaa;
  padding: 4px 0 8px; display: flex; align-items: center; gap: 5px; flex-wrap: wrap;
}
.hier-bc .sep { color: #ddd; }
.hier-bc .act { color: #ff4d4d; font-weight: 700; }

/* ── LLEGENDA MAPA ── */
.mapa-llegenda {
  background: white; border-radius: 10px; padding: 12px 14px;
  box-shadow: 0 2px 14px rgba(0,0,0,0.13); font-size: 11px;
  min-width: 155px;
}
.mapa-llegenda b { font-family: 'Syne', sans-serif; font-size: 9px; text-transform: uppercase; letter-spacing: 1.2px; color: #999; display: block; margin-bottom: 8px; }

/* ── RÀNQUING COMARQUES ── */
.rank-item { display: flex; align-items: center; gap: 8px; padding: 7px 0; border-bottom: 1px solid #f5f5f5; }
.rank-num  { font-family: 'Syne', sans-serif; font-size: 10px; color: #bbb; width: 16px; text-align: right; flex-shrink: 0; }
.rank-name { flex: 1; font-size: 12px; color: #333; font-weight: 500; }
.rank-prov { font-size: 10px; color: #666666; }  /* contrast 5.7:1 */
.rank-bar-t { width: 70px; height: 5px; background: #f0f0f0; border-radius: 3px; overflow: hidden; flex-shrink: 0; }
.rank-bar-f { height: 100%; border-radius: 3px; }
.rank-val  { font-family: 'Syne', sans-serif; font-size: 11px; font-weight: 700; color: #222; width: 44px; text-align: right; flex-shrink: 0; }

/* ── TARGETES CENTRES ── */
.ctre-card {
  background: white; border-radius: 8px; padding: 10px 13px; margin-bottom: 7px;
  box-shadow: 0 1px 8px rgba(0,0,0,0.06); border-left: 3px solid #e0e0e0;
  cursor: pointer; transition: all 0.18s;
}
.ctre-card:hover { box-shadow: 0 3px 16px rgba(0,0,0,0.12); transform: translateX(2px); border-left-color: #ff4d4d; }
.ctre-card.sel   { border-left-color: #ff4d4d; background: #fff8f8; }
.ctre-nom  { font-weight: 600; font-size: 13px; color: #222; }
.ctre-info { font-size: 11px; color: #555555; margin-top: 2px; }  /* contrast 7.0:1 */
.ctre-icqa { font-size: 11px; font-weight: 700; margin-top: 4px; }

/* ── FILTRE BAR ── */
.filtre-bar {
  background: white; border-radius: 10px; padding: 10px 16px; margin-bottom: 12px;
  box-shadow: 0 2px 10px rgba(0,0,0,0.06); display: flex; gap: 20px; flex-wrap: wrap; align-items: flex-start;
}
.filtre-bar .shiny-input-container { margin-bottom: 0 !important; }

/* ── COMPARATIVA A LA CARTA ── */
.comp-selectors {
  background: white; border-radius: 10px; padding: 14px 16px; margin-bottom: 12px;
  box-shadow: 0 2px 10px rgba(0,0,0,.06);
}
.comp-selectors h5 {
  font-family: 'Syne', sans-serif; font-size: 10px; text-transform: uppercase;
  letter-spacing: 1px; color: #888; margin-bottom: 10px;
}

/* ── CERCA COMPARATIVA ── */
.comp-cerca-wrap {
  position: relative;
}
.comp-cerca-input {
  width: 100%; border: 1.5px solid #e0e7ef; border-radius: 8px;
  padding: 8px 12px 8px 32px; font-size: 12px; outline: none;
  box-sizing: border-box; font-family: 'DM Sans', sans-serif;
  background: white url('data:image/svg+xml,<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"12\" height=\"12\" viewBox=\"0 0 24 24\" fill=\"none\" stroke=\"%23aaa\" stroke-width=\"2.5\"><circle cx=\"11\" cy=\"11\" r=\"8\"/><line x1=\"21\" y1=\"21\" x2=\"16.65\" y2=\"16.65\"/></svg>') no-repeat 10px center;
}
.comp-cerca-input:focus { border-color: #ff4d4d; box-shadow: 0 0 0 2px rgba(255,77,77,0.1); }
.comp-cerca-dropdown {
  position: absolute; top: 100%; left: 0; right: 0; z-index: 9999;
  background: white; border-radius: 0 0 8px 8px;
  box-shadow: 0 6px 20px rgba(0,0,0,0.15);
  max-height: 200px; overflow-y: auto;
}
.comp-cerca-item {
  padding: 7px 10px; cursor: pointer; font-size: 11px;
  border-bottom: 1px solid #f5f5f5; line-height: 1.4;
}
.comp-cerca-item:hover { background: #fff5f5; }
.comp-selected-badge {
  display: inline-flex; align-items: center; gap: 5px;
  background: #fff0f0; border: 1px solid #ffd5d5; border-radius: 6px;
  padding: 4px 8px; font-size: 11px; color: #c0392b; margin-top: 5px;
  max-width: 100%; overflow: hidden; text-overflow: ellipsis; white-space: nowrap;
}
.comp-selected-badge .rm { cursor: pointer; font-weight: 700; margin-left: 4px; color: #ff4d4d; }

/* ── NAVEGACIÓ PER TECLAT: focus visible ── */
.cerca-item, .ctre-card, .comp-cerca-item, .rank-name a {
  outline: none;
}
.cerca-item:focus, .ctre-card:focus, .comp-cerca-item:focus {
  outline: 2px solid #ff4d4d;
  outline-offset: 2px;
}
"

# ============================================================
# HTML DE LA INTRODUCCIÓ (amb canvas accessible)
# ============================================================
intro_html = '<!DOCTYPE html>
<html lang="ca"><head><meta charset="UTF-8">
<meta name="viewport" content="width=device-width, initial-scale=1.0">
<style>
@import url("https://fonts.googleapis.com/css2?family=DM+Serif+Display:ital@0;1&family=DM+Sans:wght@300;400;500&display=swap");
*,*::before,*::after{box-sizing:border-box;margin:0;padding:0}
:root{--bg:#05080f;--accent:#ff4d4d;--accent2:#ff9a3c;--gold:#f5c842;--text:#e8eaf0;--muted:#7a8499;--blue:#4a9eff;--green:#3dd68c}
html{scroll-behavior:smooth}
body{background:var(--bg);color:var(--text);font-family:"DM Sans",sans-serif;font-weight:300;overflow-x:hidden}
.ss{min-height:100vh;display:flex;align-items:center;justify-content:center;padding:80px 40px;position:relative;opacity:0;transform:translateY(40px);transition:opacity 0.9s ease,transform 0.9s ease}
.ss.vis{opacity:1;transform:translateY(0)}
.ss>*{position:relative;z-index:1}
#pc{position:fixed;top:0;left:0;width:100%;height:100%;pointer-events:none;z-index:0;opacity:.35; role="presentation"; aria-hidden="true"}
#sp{position:fixed;top:0;left:0;height:3px;background:linear-gradient(90deg,var(--accent),var(--accent2));z-index:9999;width:0%;transition:width .1s}

.hero{background:radial-gradient(ellipse at 50% 40%,#1a0a0a 0%,#05080f 70%);text-align:center;flex-direction:column}
.h-ey{font-size:15px;letter-spacing:4px;text-transform:uppercase;color:var(--accent);font-weight:500;margin-bottom:24px}
.h-ti{font-family:"DM Serif Display",serif;font-size:clamp(46px,8vw,92px);line-height:1;color:#fff;text-shadow:0 0 80px rgba(255,77,77,.3);margin-bottom:20px}
.h-ti em{font-style:italic;color:var(--accent);display:block}
.h-su{font-size:clamp(14px,2vw,18px);color:var(--muted);max-width:580px;margin:0 auto 48px;line-height:1.7}
.scta{display:inline-flex;align-items:center;gap:10px;font-size:15px;letter-spacing:2px;text-transform:uppercase;color:var(--accent);cursor:pointer;animation:bnc 2s infinite}
@keyframes bnc{0%,100%{transform:translateY(0)}50%{transform:translateY(6px)}}

.bst{text-align:center;flex-direction:column}
.sl{font-size:15px;letter-spacing:3px;text-transform:uppercase;color:var(--muted);margin-bottom:16px}
.sn{font-family:"DM Serif Display",serif;font-size:clamp(78px,15vw,175px);line-height:1;background:linear-gradient(135deg,var(--accent),var(--accent2));-webkit-background-clip:text;-webkit-text-fill-color:transparent;background-clip:text}
.su{font-size:clamp(17px,3vw,27px);color:var(--text);margin-top:8px}
.src{font-size:15px;color:var(--muted);margin-top:20px;letter-spacing:1px}

.tc{display:grid;grid-template-columns:1fr 1fr;gap:60px;max-width:1100px;width:100%;align-items:center}
@media(max-width:768px){.tc{grid-template-columns:1fr;gap:32px}}
.ct h2{font-family:"DM Serif Display",serif;font-size:clamp(27px,4vw,46px);line-height:1.2;margin-bottom:20px;color:#fff}
.ct h2 .ac{color:var(--accent)}
.ct p{font-size:15px;line-height:1.8;color:var(--muted);margin-bottom:16px}
.hb{border-left:3px solid var(--accent);padding:12px 16px;background:rgba(255,77,77,.06);border-radius:0 6px 6px 0;font-size:14px;color:var(--text);margin-top:20px;line-height:1.6}
.hb strong{color:var(--accent)}

.db{display:flex;flex-direction:column;gap:18px}
.di{position:relative}
.dl{font-size:15px;color:var(--muted);margin-bottom:6px}
.dt{height:8px;background:rgba(255,255,255,.06);border-radius:4px;overflow:hidden}
.df{height:100%;border-radius:4px;width:0%;transition:width 1.4s cubic-bezier(.16,1,.3,1)}
.dv{position:absolute;right:0;top:-20px;font-size:15px;font-weight:500;color:var(--text)}

.kg{display:flex;flex-wrap:wrap;gap:5px;max-width:460px}
.kd{width:14px;height:14px;border-radius:50%;background:rgba(255,255,255,.12);transition:background .05s;flex-shrink:0}
.kd.af{background:var(--accent)!important}
.kc{font-size:15px;color:var(--muted);margin-top:14px;line-height:1.6}
.kc strong{color:var(--accent)}

.eg{display:grid;grid-template-columns:repeat(3,1fr);gap:20px;max-width:900px;width:100%}
.ec{background:rgba(255,255,255,.03);border:1px solid rgba(255,255,255,.07);border-radius:12px;padding:24px 20px;text-align:center;transition:border-color .3s,background .3s}
.ec:hover{border-color:var(--accent);background:rgba(255,77,77,.05)}
.ei{font-size:36px;margin-bottom:14px;display:block}
.es{font-family:"DM Serif Display",serif;font-size:36px;color:var(--accent);margin-bottom:8px}
.ed{font-size:15px;color:var(--muted);line-height:1.6}
.ef{font-size:15px;color:rgba(255,255,255,.2);margin-top:10px;letter-spacing:.8px}

.cats{flex-direction:column;text-align:center;background:radial-gradient(ellipse at 50% 60%,#0a1a0a 0%,#05080f 70%)}
.cats h2{font-family:"DM Serif Display",serif;font-size:clamp(30px,5vw,56px);margin-bottom:16px;color:#fff}
.cats h2 .gr{color:var(--green)}
.csr{display:flex;gap:40px;justify-content:center;flex-wrap:wrap;margin-top:40px}
.csp{background:rgba(255,255,255,.04);border:1px solid rgba(255,255,255,.1);border-radius:100px;padding:16px 32px;display:flex;flex-direction:column;align-items:center;min-width:160px}
.csp .cn{font-family:"DM Serif Display",serif;font-size:36px;color:var(--gold)}
.csp .cl{font-size:15px;color:var(--muted);text-align:center;line-height:1.4;margin-top:4px}

.cta{flex-direction:column;text-align:center;background:radial-gradient(ellipse at 50% 50%,#0a0a1a 0%,#05080f 70%)}
.cta h2{font-family:"DM Serif Display",serif;font-size:clamp(30px,5vw,56px);color:#fff;margin-bottom:20px}
.cta h2 em{font-style:italic;color:var(--blue)}
.cta p{font-size:16px;color:var(--muted);max-width:520px;margin:0 auto 40px;line-height:1.7}
.cb{display:inline-flex;align-items:center;gap:10px;padding:16px 36px;background:linear-gradient(135deg,#ff4d4d,#c0392b);color:#fff;font-size:14px;font-weight:500;border:none;border-radius:8px;cursor:pointer;box-shadow:0 8px 30px rgba(255,77,77,.35);transition:transform .2s,box-shadow .2s;text-decoration:none}
.cb:hover{transform:translateY(-2px);box-shadow:0 12px 40px rgba(255,77,77,.5)}
.sd{width:60px;height:2px;background:linear-gradient(90deg,var(--accent),transparent);margin:0 auto 32px}
</style></head><body>
<div id="sp"></div><canvas id="pc" role="presentation" aria-hidden="true"></canvas>

<section class="ss hero" id="s0">
  <div class="h-ey">Visualització de dades · Centres educatius · Catalunya</div>
  <h1 class="h-ti">L\'aire que<br><em>respiren els teus fills</em></h1>
  <p class="h-su">Cada dia, milers d\'alumnes passen més de sis hores envoltats d\'aire que potser mai ningú ha mesurat. Aquesta visualització ho canvia.</p>
  <div class="scta" onclick="document.getElementById(\'s1\').scrollIntoView({behavior:\'smooth\'})">↓ &nbsp; Descobreix les dades</div>
</section>

<section class="ss bst" id="s1">
  <div class="sl">Cada any al món</div>
  <div class="sn">7.000.000</div>
  <div class="su">de persones moren per contaminació atmosfèrica</div>
  <div class="src">Font: Organització Mundial de la Salut, 2021</div>
</section>

<section class="ss" id="s2">
  <div class="tc">
    <div class="ct">
      <h2>A Europa, <span class="ac">253.000</span> morts prematurs cada any</h2>
      <p>L\'EEA estima que la contaminació per PM2.5 és responsable de 253.000 morts prematures anuals als 27 estats membres.</p>
      <p>Les ciutats concentren el problema: el trànsit rodat genera NO₂ i PM10 que penetren als pulmons i travessen la barrera hematoencefàlica.</p>
      <div class="hb"><strong>Els nens son especialment vulnerables:</strong> respiren més aire per kg de pes corporal i els seus pulmons i cervells encara es desenvolupen.</div>
    </div>
    <div class="db" id="bars-europa">
      <div class="di"><div class="dl">PM2.5 — Mortalitat prematura (UE) <span class="dv">253.000/any</span></div><div class="dt"><div class="df" data-w="92" style="background:linear-gradient(90deg,#ff4d4d,#ff9a3c)"></div></div></div>
      <div class="di"><div class="dl">NO₂ — Mortalitat prematura (UE) <span class="dv">52.000/any</span></div><div class="dt"><div class="df" data-w="52" style="background:linear-gradient(90deg,#ff9a3c,#f5c842)"></div></div></div>
      <div class="di"><div class="dl">O₃ — Mortalitat prematura (UE) <span class="dv">22.000/any</span></div><div class="dt"><div class="df" data-w="30" style="background:linear-gradient(90deg,#f5c842,#3dd68c)"></div></div></div>
      <p style="font-size:15px;color:#4a5568;margin-top:8px">Font: EEA Air Quality Report, 2023</p>
    </div>
  </div>
</section>

<section class="ss" id="s3">
  <div class="tc">
    <div>
      <div class="sl" style="text-align:left;margin-bottom:20px">D\'un grup de 100 nens en zones d\'alta contaminació...</div>
      <div class="kg" id="kids-dots"></div>
      <p class="kc"><strong>14 de cada 100 casos d\'asma infantil</strong> en zones urbanes s\'atribueixen a NO₂.<br><span style="font-size:15px">Font: Lancet Planetary Health, 2023</span></p>
    </div>
    <div class="ct">
      <h2>L\'asma és <span class="ac">la punta de l\'iceberg</span></h2>
      <p>La contaminació erosiona de forma lenta i silenciosa. Els nens exposats de forma crònica desenvolupen pulmons més petits.</p>
      <div class="hb">Cada <strong>5 µg/m³ addicionals de NO₂</strong> durant la infància s\'associen amb <strong>-1,5 punts de CI</strong> i major risc de TDAH.<div style="font-size:15px;color:white;margin-top:8px">Font: BREATHE Cohort, ISGlobal Barcelona, 2023</div></div>
    </div>
  </div>
</section>

<section class="ss" id="s4" style="flex-direction:column">
  <div class="sd"></div>
  <h2 style="font-family:\'DM Serif Display\',serif;font-size:clamp(24px,4vw,42px);text-align:center;margin-bottom:40px;color:#fff">Què li passa al cos d\'un nen exposat?</h2>
  <div class="eg"  style="color: white;">
    <div class="ec"><span class="ei"></span><div class="es">−14%</div><div class="ed">Reducció de la funció pulmonar en nens exposats a PM2.5 per sobre de la guia OMS durant els primers 10 anys</div><div class="ef">ESCAPE · NEJM 2015</div></div>
    <div class="ec"><span class="ei"></span><div class="es">−1.5 IQ</div><div class="ed">Punts de CI perduts per cada 5 µg/m³ de NO₂ addicional, documentat des dels 4 anys fins l\'adolescència</div><div class="ef">BREATHE · ISGlobal 2023</div></div>
    <div class="ec"><span class="ei"></span><div class="es">+12%</div><div class="ed">Augment del risc de leucèmia infantil per cada 10 µg/m³ addicionals de PM2.5 en exposició perinatal</div><div class="ef">ESCAPE · Lancet Oncol 2015</div></div>
    <div class="ec"><span class="ei"></span><div class="es">×1.7</div><div class="ed">Risc relatiu de trastorns del son en nens de 6-11 anys en zones d\'alta vs baixa contaminació</div><div class="ef">Am. J. Epidemiology 2022</div></div>
    <div class="ec"><span class="ei"></span><div class="es">+9%</div><div class="ed">Major risc de TDAH per exposició prenatal a trànsit rodat intens. L\'efecte és dosi-resposta i acumulatiu</div><div class="ef">Environment Int. 2021</div></div>
    <div class="ec"><span class="ei">️</span><div class="es">−0.3mm</div><div class="ed">Reducció del diàmetre arterial en adolescents per cada 5 µg/m³ de PM2.5 crònic. Risc CV futur elevat</div><div class="ef">Circ Research · AHA 2020</div></div>
  </div>
</section>

<section class="ss cats" id="s5">
  <div class="sd" style="background:linear-gradient(90deg,var(--green),transparent)"></div>
  <h2>I a <span class="gr">Catalunya?</span></h2>
  <p style="font-size:15px;color:var(--muted);max-width:600px;margin:16px auto 0;line-height:1.8">ISGlobal va analitzar les escoles de la Regió Metropolitana de Barcelona. Els resultats van sacsejar la comunitat científica.</p>
  <div class="csr">
    <div class="csp"><span class="cn">65%</span><span class="cl">d\'escoles de BCN superen<br>la guia OMS de NO₂</span></div>
    <div class="csp"><span class="cn">3.200+</span><span class="cl">centres educatius a<br>Catalunya analitzats aquí</span></div>
    <div class="csp"><span class="cn">6h+</span><span class="cl">al dia respiren aire<br>de l\'entorn escolar</span></div>
    <div class="csp"><span class="cn">10km</span><span class="cl">radi de cobertura<br>per estació de mesura</span></div>
  </div>
  <p style="font-size:15px;color:var(--muted);margin-top:32px">Fonts: ISGlobal 2022 · ASPB 2023 · Ecologistes en Acció 2026</p>
</section>

<section class="ss cta" id="s6">
  <div class="sd" style="background:linear-gradient(90deg,var(--blue),transparent)"></div>
  <h2>Ara, <em>descobreix la teva escola</em></h2>
  <p>Hem estimat l\'exposició a NO₂, PM2.5, PM10 i O₃ de cada centre educatiu de Catalunya. L\'escola del teu fill/a hi és.</p>
  <a class="cb" onclick="window.parent.document.querySelector(\'[data-value=tab_panorama]\').click()">🗺️ &nbsp; Veure el mapa d\'exposició</a>
</section>

<script>
const obs=new IntersectionObserver(e=>{e.forEach(x=>{if(x.isIntersecting){x.target.classList.add("vis");if(x.target.id==="s2")animBars();if(x.target.id==="s3")animDots();}});},{threshold:.18});
document.querySelectorAll(".ss").forEach(s=>obs.observe(s));
document.getElementById("s0").classList.add("vis");
window.addEventListener("scroll",()=>{const h=document.documentElement.scrollHeight-window.innerHeight;document.getElementById("sp").style.width=(window.scrollY/h*100)+"%";});
function animBars(){document.querySelectorAll(".df").forEach(b=>setTimeout(()=>{b.style.width=b.getAttribute("data-w")+"%"},200));}
function animDots(){const g=document.getElementById("kids-dots");g.innerHTML="";for(let i=0;i<100;i++){const d=document.createElement("div");d.className="kd";g.appendChild(d);}let f=0;const ds=g.querySelectorAll(".kd");const iv=setInterval(()=>{if(f>=14){clearInterval(iv);return;}const i=Math.floor(Math.random()*100);if(!ds[i].classList.contains("af")){ds[i].classList.add("af");f++;}},80);}
const cv=document.getElementById("pc"),cx=cv.getContext("2d");let pts=[];
function rsz(){cv.width=window.innerWidth;cv.height=window.innerHeight;}
window.addEventListener("resize",rsz);rsz();
for(let i=0;i<80;i++)pts.push({x:Math.random()*cv.width,y:Math.random()*cv.height,r:Math.random()*2.5+.5,vx:(Math.random()-.5)*.3,vy:(Math.random()-.5)*.3,a:Math.random()*.4+.1});
function anim(){cx.clearRect(0,0,cv.width,cv.height);pts.forEach(p=>{cx.beginPath();cx.arc(p.x,p.y,p.r,0,Math.PI*2);cx.fillStyle=`rgba(255,77,77,${p.a})`;cx.fill();p.x+=p.vx;p.y+=p.vy;if(p.x<0||p.x>cv.width)p.vx*=-1;if(p.y<0||p.y>cv.height)p.vy*=-1;});requestAnimationFrame(anim);}
anim();
</script></body></html>'

# ============================================================
# INTERFÍCIE D'USUARI (UI)
# ============================================================
ui = dashboardPage(
  skin  = "blue",
  title = "Qualitat Aire · Centres Educatius Catalunya",
  
  dashboardHeader(
    title = tags$span(
      style = "font-size:15px;font-weight:800;letter-spacing:1.5px;font-family:Syne,sans-serif;text-transform:uppercase;",
      "💨 Qualitat de l'Aire als Centres Educatius"
    ), titleWidth = 560
  ),
  
  dashboardSidebar(
    width = 250,
    tags$head(tags$style(HTML(custom_css))),
    tags$script(HTML("
      $(document).on('shiny:connected', function() {
        Shiny.addCustomMessageHandler('set_sidebar', function(show) {
          if (show) $('body').removeClass('sidebar-collapse');
          else      $('body').addClass('sidebar-collapse');
        });
      });
    ")),
    sidebarMenu(
      id = "sidebar_tabs",
      menuItem("Introducció",          tabName = "tab_intro",    icon = icon("book-open")),
      menuItem("Panorama · Mapa",           tabName = "tab_panorama", icon = icon("map")),
      menuItem("Desigualtats",              tabName = "tab_desig",    icon = icon("balance-scale")),
      menuItem("Perfil Horari",             tabName = "tab_horari",   icon = icon("clock")),
      menuItem("Cobertura · Buits",         tabName = "tab_cobert",   icon = icon("satellite-dish")),
      menuItem("Metodologia",            tabName = "tab_info",     icon = icon("info-circle"))
    ),
    tags$hr(style = "border-color:rgba(255,255,255,.06);margin:10px 15px;"),
    tags$div(
      class = "footer-note",
      "📡 XVPCA + Equipaments Catalunya", tags$br(),
      paste0("Actualitzat: ", format(data_max, "%d/%m/%Y"))
    )
  ),
  
  dashboardBody(
    tabItems(
      
      # ══ PESTANYA 0: INTRODUCCIÓ ══════════════════════════════════════════
      tabItem("tab_intro",
              fluidRow(column(12,
                              tags$iframe(id="intro_frame", srcdoc=intro_html,
                                          title = "Introducció interactiva sobre la qualitat de l'aire als centres educatius de Catalunya",
                                          style = "width:100%;border:none;min-height:92vh;border-radius:8px;display:block;",
                                          tabindex = "0")
              ))
      ),
      
      # ══ PESTANYA 1: PANORAMA · MAPA ══════════════════════════════════════
      tabItem("tab_panorama",
              
              # Encapçalament semàntic: h2
              fluidRow(column(12,
                              div(style="background:linear-gradient(135deg,#06111f,#0e2238);color:white;border-radius:10px;padding:14px 20px;margin-bottom:12px;display:flex;align-items:center;gap:16px;",
                                  div(style="font-family:Syne,sans-serif;font-size:22px;font-weight:800;color:#ff4d4d;flex-shrink:0;","01"),
                                  div(
                                    div(style="font-family:Syne,sans-serif;font-size:15px;text-transform:uppercase;letter-spacing:1.5px;color:#7a8fa6;","Pregunta de recerca"),
                                    tags$h2(style="font-size:14px;font-weight:500;margin-top:2px;",
                                            "Quins centres educatius de Catalunya estan ubicats en zones amb pitjor qualitat de l'aire, i hi ha patrons comarcals o metropolitans clars?")
                                  )
                              )
              )),
              
              # Barra de filtres
              fluidRow(column(12,
                              div(class="filtre-bar",
                                  checkboxGroupInput("p1_cats","Tipus de centre:",
                                                     choices = c(
                                                       "Escola Bressol"    = "te_EINF1C",
                                                       "Educació Infantil" = "te_EINF2C",
                                                       "Educació Primària" = "te_EPRI",
                                                       "Educació Especial" = "te_EE"
                                                     ),
                                                     selected=c("te_EINF1C","te_EINF2C","te_EPRI","te_EE"), inline=TRUE),
                                  div(style="margin-left:20px;",
                                      checkboxGroupInput("p1_tit","Titularitat:",
                                                         choices=c("Pública","Privada/Concertada"),
                                                         selected=c("Pública","Privada/Concertada"), inline=TRUE)
                                  ),
                                  div(style="margin-left:20px;",
                                      checkboxGroupInput("p1_icqa","ICQA:",
                                                         choices=icqa_nivells, selected=icqa_nivells, inline=TRUE)
                                  )
                              )
              )),
              
              # KPIs resum amb aria-live
              fluidRow(column(12, 
                              div(role = "status", `aria-live` = "polite", `aria-label` = "Resum estadístic actualitzat",
                                  uiOutput("ui_kpis_p1")
                              )
              )),
              
              # Mapa i rànquing lateral
              fluidRow(
                column(9,
                       div(style="position:relative;",
                           # Mapa embolicat amb role="img" i aria-label
                           div(role = "img",
                               `aria-label` = paste0(
                                 "Mapa de qualitat de l'aire als centres educatius de Catalunya. ",
                                 "Cada punt representa un centre. El color indica l'ICQA estimat: ",
                                 "blau per Bona, verd per Raonablement bona, groc per Regular, ",
                                 "vermell per Desfavorable, taronja per Molt desfavorable, morat per Extremadament desfavorable, gris per Sense dades. ",
                                 "A més, cada marcador conté una lletra: B, R, R, D, M, E o ? per facilitar la identificació a persones amb daltonisme."
                               ),
                               leafletOutput("mapa_p1", height = 570)
                           ),
                           # Cercador integrat al mapa
                           div(id="cerca_wrapper",
                               tags$input(id="cerca_input", type="text",
                                          placeholder="🔍  Cerca centre, comarca o municipi…",
                                          autocomplete="off"),
                               uiOutput("ui_cerca_llista")
                           ),
                           # Llegenda ICQA
                           absolutePanel(bottom=25, right=10,
                                         div(class="mapa-llegenda",
                                             tags$b("ICQA Estimat"),
                                             lapply(names(icqa_pal), function(lv)
                                               div(style="display:flex;align-items:center;gap:6px;margin-bottom:4px;",
                                                   div(style=paste0("width:11px;height:11px;border-radius:50%;background:",icqa_pal[[lv]],";")),
                                                   span(style="color:#444;font-size:15px;", lv)
                                               )
                                             )
                                         )
                           )
                       )
                ),
                column(3,
                       div(style="background:white;border-radius:10px;padding:14px;box-shadow:0 2px 12px rgba(0,0,0,0.07);",
                           div(style="display:flex;justify-content:space-between;align-items:center;margin-bottom:8px;",
                               tags$b(style="font-family:Syne,sans-serif;font-size:15px;text-transform:uppercase;letter-spacing:1px;","Rànquing comarques"),
                               div(
                                 actionButton("rank_millor","↑ Millor", class="btn btn-xs btn-default", style="margin-right:4px;"),
                                 actionButton("rank_pitjor","↓ Pitjor", class="btn btn-xs",
                                              style="background:#ff4d4d;color:white;border:none;border-radius:4px;padding:2px 6px;font-size:15px;")
                               )
                           ),
                           div(style="margin-bottom:8px;",
                               selectInput("rank_cont", NULL,
                                           choices = c("NO₂" = "NO2", "PM2.5" = "PM2.5", "PM10" = "PM10", "O₃" = "O3"),
                                           selected = "NO2",
                                           width = "100%")
                           ),
                           uiOutput("ui_ranking")
                       )
                )
              ),
              
              # Context temporal i explicació de l'ICQA
              fluidRow(column(12,
                              div(style="margin-top:10px;display:flex;gap:12px;flex-wrap:wrap;",
                                  div(style="flex:2;min-width:300px;background:white;border-radius:10px;padding:14px 18px;box-shadow:0 2px 10px rgba(0,0,0,.06);border-left:4px solid #2980b9;",
                                      div(style="font-family:Syne,sans-serif;font-size:15px;text-transform:uppercase;letter-spacing:1px;color:#888;margin-bottom:8px;","ℹ️  Índex de Qualitat de l'Aire per Centres (ICQA)"),
                                      div(style="font-size:15.5px;line-height:1.8;color:#444;",
                                          "L'", tags$strong("ICQA"), " transforma la informació donada per la concentració de diferents contaminants atmosfèrics, amb afectacions diferents segons els nivells d’immissió, per obtenir una categoria única, centrat en la qualitat de l’aire que es respira i que es pot representar amb una escala de colors.",
                                          div(style="display:flex;flex-wrap:wrap;gap:6px;margin-top:8px;",
                                              lapply(names(icqa_pal), function(lv)
                                                div(style=paste0("display:inline-flex;align-items:center;gap:4px;font-size:15px;padding:2px 8px;border-radius:12px;background:",
                                                                 icqa_pal[[lv]],"22;border:1px solid ",icqa_pal[[lv]],"55;"),
                                                    div(style=paste0("width:8px;height:8px;border-radius:50%;background:",icqa_pal[[lv]],";")),
                                                    span(lv)
                                                )
                                              )
                                          )
                                      )
                                  ),
                                  div(style="flex:1;min-width:200px;background:white;border-radius:10px;padding:14px 18px;box-shadow:0 2px 10px rgba(0,0,0,.06);border-left:4px solid #e67e22;",
                                      div(style="font-family:Syne,sans-serif;font-size:15px;text-transform:uppercase;letter-spacing:1px;color:#888;margin-bottom:8px;","📅  Dades del mapa, rànquing i comparativa"),
                                      uiOutput("ui_data_context")
                                  )
                              )
              )),
              
              # ── COMPARATIVA A LA CARTA (sempre visible) ──
              fluidRow(column(12, div(style="margin-top:12px;background:white;border-radius:10px;padding:16px;box-shadow:0 2px 12px rgba(0,0,0,.07);",
                                      uiOutput("ui_centre_header"),
                                      # Selectors de centres amb cerca textual
                                      div(class="comp-selectors",
                                          tags$h5("Tria fins a 3 centres per comparar"),
                                          fluidRow(
                                            column(4,
                                                   div(class="comp-cerca-wrap",
                                                       tags$label(style="font-family:Syne,sans-serif;font-size:15px;font-weight:700;text-transform:uppercase;letter-spacing:0.8px;color:#555;",
                                                                  "Centre 1 (principal):"),
                                                       tags$input(id="comp_cerca1_input", type="text",
                                                                  class="comp-cerca-input",
                                                                  placeholder="Cerca per nom de centre…",
                                                                  autocomplete="off"),
                                                       uiOutput("ui_comp_dropdown1"),
                                                       uiOutput("ui_comp_badge1")
                                                   )
                                            ),
                                            column(4,
                                                   div(class="comp-cerca-wrap",
                                                       tags$label(style="font-family:Syne,sans-serif;font-size:15px;font-weight:700;text-transform:uppercase;letter-spacing:0.8px;color:#555;",
                                                                  "Centre 2:"),
                                                       tags$input(id="comp_cerca2_input", type="text",
                                                                  class="comp-cerca-input",
                                                                  placeholder="Cerca per nom de centre…",
                                                                  autocomplete="off"),
                                                       uiOutput("ui_comp_dropdown2"),
                                                       uiOutput("ui_comp_badge2")
                                                   )
                                            ),
                                            column(4,
                                                   div(class="comp-cerca-wrap",
                                                       tags$label(style="font-family:Syne,sans-serif;font-size:15px;font-weight:700;text-transform:uppercase;letter-spacing:0.8px;color:#555;",
                                                                  "Centre 3:"),
                                                       tags$input(id="comp_cerca3_input", type="text",
                                                                  class="comp-cerca-input",
                                                                  placeholder="Cerca per nom de centre…",
                                                                  autocomplete="off"),
                                                       uiOutput("ui_comp_dropdown3"),
                                                       uiOutput("ui_comp_badge3")
                                                   )
                                            )
                                          )
                                      ),
                                      fluidRow(
                                        column(4, uiOutput("ui_centres_cards")),
                                        column(8, plotlyOutput("plot_radar_centres", height=320))
                                      ),
                                      uiOutput("ui_interp_centre")
              )))
      ),
      
      # ══ PESTANYA 2: DESIGUALTATS ════════════════════════════════════════
      tabItem("tab_desig",
              fluidRow(column(12,
                              div(style="background:linear-gradient(135deg,#06111f,#0e2238);color:white;border-radius:10px;padding:14px 20px;margin-bottom:12px;display:flex;align-items:center;gap:16px;",
                                  div(style="font-family:Syne,sans-serif;font-size:22px;font-weight:800;color:#ff4d4d;flex-shrink:0;","02"),
                                  div(
                                    div(style="font-family:Syne,sans-serif;font-size:15px;text-transform:uppercase;letter-spacing:1.5px;color:#7a8fa6;","Pregunta de recerca"),
                                    tags$h2(style="font-size:14px;font-weight:500;margin-top:2px;","Existeixen diferències d'exposició entre centres públics i privats/concertats? I entre centres de zones rurals, suburbanes i urbanes?")
                                  )
                              )
              )),
              # Context temporal
              fluidRow(column(12,
                              div(style="background:white;border-radius:8px;padding:10px 16px;margin-bottom:10px;box-shadow:0 1px 6px rgba(0,0,0,.05);border-left:4px solid #e67e22;display:flex;align-items:center;gap:10px;",
                                  tags$span(style="font-size:18px;","📅"),
                                  div(style="font-size:15px;color:#555;",
                                      "Dades corresponents a la darrera setmana disponible: ",
                                      tags$strong(paste0(format(data_inici_setmana, "%d/%m/%Y"), " – ", format(data_fi_setmana, "%d/%m/%Y"))),
                                      " (mitjanes IDW calculades a partir de ",
                                      tags$strong(paste0(n_distinct(stations_geo$CODI.EOI), " estacions")),
                                      " de la XVPCA). Concentracions en µg/m³."
                                  )
                              )
              )),
              fluidRow(
                column(3,
                       box(title="Controls", width=12, solidHeader=TRUE, status="primary",
                           selectInput("d_cont","Contaminant:",
                                       choices = c("NO₂"="NO2","PM2.5"="PM2.5","PM10"="PM10","O₃"="O3"),
                                       selected="NO2"),
                           selectInput("d_com","Comarca:",
                                       choices=c("Totes", sort(unique(centres_data$comarca[!is.na(centres_data$comarca)]))),
                                       selected="Totes"),
                           checkboxGroupInput("d_cats","Tipus de centre:",
                                              choices = c(
                                                "Escola Bressol"    = "EINF1C",
                                                "Educació Infantil" = "EINF2C",
                                                "Educació Primària" = "EPRI",
                                                "Educació Especial" = "EE"
                                              ),
                                              selected=c("EINF1C","EINF2C","EPRI","EE")),
                           tags$hr(),
                           uiOutput("ui_interp_desig")
                       )
                ),
                column(9,
                       tabBox(title="", width=12,
                              tabPanel("Titularitat",
                                       fluidRow(
                                         column(6, 
                                                div(role="img", `aria-label`="Gràfic de violí comparant la concentració de contaminants entre centres públics i privats per comarca.",
                                                    plotlyOutput("plot_tit_violin", height=340)
                                                )),
                                         column(6, plotlyOutput("plot_tit_dif_com", height=340))
                                       )
                              ),
                              tabPanel("Entorn urbà/rural",
                                       plotlyOutput("plot_entorn_violin", height=360)
                              ),
                              tabPanel("Per tipus de centre",
                                       plotlyOutput("plot_cats_violin", height=360)
                              )
                       )
                )
              )
      ),
      
      # ══ PESTANYA 3: PERFIL HORARI ═══════════════════════════════════════
      tabItem("tab_horari",
              fluidRow(column(12,
                              div(style="background:linear-gradient(135deg,#06111f,#0e2238);color:white;border-radius:10px;padding:14px 20px;margin-bottom:12px;display:flex;align-items:center;gap:16px;",
                                  div(style="font-family:Syne,sans-serif;font-size:22px;font-weight:800;color:#ff4d4d;flex-shrink:0;","03"),
                                  div(
                                    div(style="font-family:Syne,sans-serif;font-size:15px;text-transform:uppercase;letter-spacing:1.5px;color:#7a8fa6;","Pregunta de recerca"),
                                    tags$h2(style="font-size:14px;font-weight:500;margin-top:2px;","Com varia la concentració dels contaminants durant les hores lectives i en l'entrada/sortida? Superen els llindars OMS?")
                                  )
                              )
              )),
              fluidRow(
                box(title="Controls", width=12, solidHeader=TRUE, status="primary",
                    fluidRow(
                      column(3, selectInput("h_cont","Contaminant:",
                                            choices=c("NO₂"="NO2","PM2.5"="PM2.5","PM10"="PM10","O₃"="O3"),
                                            selected="NO2")),
                      column(5,
                             pickerInput("h_com","Comarques:",
                                         choices=sort(unique(perfil_setmana_raw$comarca[!is.na(perfil_setmana_raw$comarca)])),
                                         selected=intersect(c("Alt Camp","Alt Penedès","Anoia","Bages"),
                                                            unique(perfil_setmana_raw$comarca[!is.na(perfil_setmana_raw$comarca)])),
                                         multiple=TRUE,
                                         options=list(`actions-box`=TRUE,`live-search`=TRUE,
                                                      `selected-text-format`="count > 2",`count-selected-text`="{0} comarques"))
                      ),
                      column(4,
                             checkboxInput("h_franja","Franja escolar (9–17h)", value=TRUE),
                             checkboxInput("h_oms","Guia OMS", value=TRUE),
                             checkboxInput("h_sd","Banda ±1 SD", value=FALSE)
                      )
                    )
                )
              ),
              fluidRow(
                box(title=uiOutput("h_titol"), width=9, solidHeader=TRUE, status="primary",
                    uiOutput("ui_plot_horari_w")
                ),
                column(3,
                       valueBoxOutput("vb_hora_max", width=12),
                       valueBoxOutput("vb_conc_esc", width=12),
                       box(title="Finestra temporal", width=12, solidHeader=FALSE, status="info",
                           uiOutput("ui_finestra"),
                           tags$hr(style="margin:8px 0;"),
                           tags$p(style="font-size:15px;color:#666;line-height:1.6;",
                                  "Cada línia = una comarca. Passa el ratolí per ressaltar totes les línies. La franja blava marca l'horari escolar.")
                       ),
                       box(title="Interpretació", width=12, solidHeader=FALSE, status="warning",
                           uiOutput("ui_interp_horari")
                       )
                )
              ),
              fluidRow(
                box(title=uiOutput("h_titol_zoom"), width=12, solidHeader=TRUE, status="info",
                    tags$p(style="font-size:15px;color:#666;padding:4px 0 8px;",
                           "La franja taronja marca l'hora d'entrada (8–9h), la blava l'horari lectiu (9–17h) i la vermella l'hora de sortida (17–18h). La línia vermella puntejada és la guia OMS."),
                    plotlyOutput("plot_franges", height=280)
                )
              )
      ),
      
      # ══ PESTANYA 4: COBERTURA · BUITS ═══════════════════════════════════
      tabItem("tab_cobert",
              fluidRow(column(12,
                              div(style="background:linear-gradient(135deg,#06111f,#0e2238);color:white;border-radius:10px;padding:14px 20px;margin-bottom:12px;display:flex;align-items:center;gap:16px;",
                                  div(style="font-family:Syne,sans-serif;font-size:22px;font-weight:800;color:#ff4d4d;flex-shrink:0;","04"),
                                  div(
                                    div(style="font-family:Syne,sans-serif;font-size:15px;text-transform:uppercase;letter-spacing:1.5px;color:#7a8fa6;","Pregunta de recerca"),
                                    tags$h2(style="font-size:14px;font-weight:500;margin-top:2px;","En quines zones hi ha centres educatius sense cap estació de mesura propera?")
                                  )
                              )
              )),
              fluidRow(
                column(3,
                       box(title="Filtres", width=12, solidHeader=TRUE, status="primary",
                           selectInput("c_prov","Província:",
                                       choices=c("Totes","Barcelona","Girona","Lleida","Tarragona"), selected="Totes"),
                           checkboxGroupInput("c_cats","Tipus de centre:",
                                              choices=c(
                                                "Escola Bressol"="EINF1C",
                                                "Educació Infantil"="EINF2C",
                                                "Educació Primària"="EPRI",
                                                "Educació Especial"="EE"
                                              ),
                                              selected=c("EINF1C","EINF2C","EPRI","EE")),
                           tags$hr(),
                           div(class="interpret-box",
                               tags$span(class="ib-icon","📡"),
                               "Les zones sense cobertura representen una ",
                               tags$strong("mancança crítica per a la política pública"),
                               ": sense estacions properes, és impossible avaluar i actuar sobre el risc real al qual estan exposats els alumnes."
                           )
                       )
                ),
                column(9,
                       fluidRow(
                         box(title="% de centres sense estació de mesura a ≤10 km, per comarca",
                             width=12, solidHeader=TRUE, status="danger",
                             plotlyOutput("plot_cob_com", height=320)
                         )
                       ),
                       fluidRow(
                         box(title="Distribució espacial dels centres sense cobertura",
                             width=8, solidHeader=TRUE, status="primary",
                             leafletOutput("mapa_cob", height=340)
                         ),
                         column(4,
                                box(title="Per província", width=12, solidHeader=TRUE, status="info",
                                    plotlyOutput("plot_cob_prov", height=160)),
                                box(title="Per tipus de centre", width=12, solidHeader=FALSE, status="warning",
                                    plotlyOutput("plot_cob_cat", height=150))
                         )
                       ),
                       fluidRow(column(12, uiOutput("ui_interp_cobert")))
                )
              )
      ),
      
      # ══ PESTANYA 5: METODOLOGIA I FONTS ════════════════════════════════
      tabItem("tab_info",
              fluidRow(
                box(title="Metodologia", width=8, solidHeader=TRUE, status="primary",
                    div(style="font-size:15px;line-height:1.9;",
                        tags$h4("Unitat d'estudi: el centre educatiu"),
                        tags$p("La unitat mínima d'anàlisi és el centre educatiu individual. Un centre pot pertànyer simultàniament a les categories: Escola Bressol (EINF1C), Educació Infantil (EINF2C), Educació Primària (EPRI) i Educació Especial (EE)."),
                        tags$h4("Estimació d'exposició (IDW)"),
                        tags$p("Per a cada centre, la concentració estimada pondera totes les estacions a ≤10 km, amb pes 1/distancia². Si no hi ha cap estació dins d'aquest radi, el centre queda sense estimació fiable."),
                        tags$h4("ICQA — Índex Català de Qualitat de l'Aire"),
                        tags$p("L'Índex Català de Qualitat de l'Aire es calcula a partir de les dades de les estacions automàtiques de la Xarxa de Vigilància i Previsió de la Contaminació Atmosfèrica (XVPCA). Es calcula atribuint una categoria de l’ICQA per cada contaminant, d’acord amb la concentració mesurada, tenint en compte la correspondència segons la taula de referència. S'assigna l'ICQA del punt de mesurament com la categoria del contaminant amb una qualitat de l'aire més desfavorable."),
                        tags$h4("Limitacions"),
                        tags$ul(
                          tags$li("Cobertura territorial desigual: la XVPCA es concentra en zones urbanes."),
                          tags$li("Estimació de l'aire exterior; no captura qualitat interior dels centres.")
                        )
                    )
                ),
                column(4,
                       box(title="Guies OMS 2021", width=12, solidHeader=TRUE, status="info",
                           div(style="font-size:15px;",
                               div(style="display:flex;justify-content:space-between;padding:5px 0;border-bottom:1px solid #f0f0f0;font-weight:600;color:#444;",span("Contaminant"),span("Guia"),span("Promig")),
                               div(style="display:flex;justify-content:space-between;padding:5px 0;border-bottom:1px solid #f0f0f0;",span("NO₂"),span("10 µg/m³"),span("anual")),
                               div(style="display:flex;justify-content:space-between;padding:5px 0;border-bottom:1px solid #f0f0f0;",span("NO₂"),span("25 µg/m³"),span("24h")),
                               div(style="display:flex;justify-content:space-between;padding:5px 0;border-bottom:1px solid #f0f0f0;",span("PM2.5"),span("5 µg/m³"),span("anual")),
                               div(style="display:flex;justify-content:space-between;padding:5px 0;border-bottom:1px solid #f0f0f0;",span("PM2.5"),span("15 µg/m³"),span("24h")),
                               div(style="display:flex;justify-content:space-between;padding:5px 0;border-bottom:1px solid #f0f0f0;",span("PM10"),span("15 µg/m³"),span("anual")),
                               div(style="display:flex;justify-content:space-between;padding:5px 0;border-bottom:1px solid #f0f0f0;",span("PM10"),span("45 µg/m³"),span("24h")),
                               div(style="display:flex;justify-content:space-between;padding:5px 0;",span("O₃"),span("60 µg/m³"),span("màx 8h"))
                           )
                       ),
                       box(title="Fonts", width=12, solidHeader=TRUE, status="warning",
                           div(style="font-size:15px;line-height:2;",
                               tags$p("1. OMS 2021 — 7M morts/any contaminació"),
                               tags$p("2. EEA Air Quality in Europe 2023"),
                               tags$p("3. BREATHE Cohort · ISGlobal BCN 2023"),
                               tags$p("4. ESCAPE Study · NEJM / Lancet Oncol 2015"),
                               tags$p("5. Lancet Planetary Health 2023"),
                               tags$p("6. ISGlobal Escoles BCN · NO₂ 2022"),
                               tags$p("7. ASPB Protegim les Escoles 2023")
                           )
                       )
                )
              )
      )
    )
  )
)

# ============================================================
# SERVIDOR (BACKEND)
# ============================================================
server = function(input, output, session) {
  
  # Control de la sidebar
  observeEvent(input$sidebar_tabs, {
    session$sendCustomMessage("set_sidebar", input$sidebar_tabs != "tab_intro")
  })
  
  # Estat reactiu
  rv = reactiveValues(
    centre_sel  = NULL,
    rank_ord    = "pitjor",
    comp_id1    = NULL,
    comp_id2    = NULL,
    comp_id3    = NULL
  )
  
  observeEvent(input$rank_millor, rv$rank_ord <- "millor")
  observeEvent(input$rank_pitjor, rv$rank_ord <- "pitjor")
  
  # ── Buscadors de la comparativa (dropdowns dinàmics amb accés per teclat) ──
  output$ui_comp_dropdown1 = renderUI({
    q = input$comp_cerca1_input
    if (is.null(q) || nchar(trimws(q)) < 2) return(NULL)
    res = centres_data %>%
      filter(grepl(trimws(q), nom, ignore.case=TRUE) | grepl(trimws(q), municipi, ignore.case=TRUE)) %>%
      slice_head(n=8)
    if (nrow(res)==0) return(div(class="comp-cerca-dropdown",
                                 div(class="comp-cerca-item", style="color:#888;","Cap resultat")))
    div(class="comp-cerca-dropdown",
        lapply(seq_len(nrow(res)), function(i) {
          r = res[i,]
          div(class="comp-cerca-item",
              tabindex = "0",
              role = "button",
              `aria-label` = paste0("Seleccionar centre ", r$nom, " de ", r$municipi),
              onclick = sprintf("Shiny.setInputValue('comp_sel1','%s',{priority:'event'});", r$id_centre),
              onkeydown = sprintf("if(event.key==='Enter'||event.key===' ') Shiny.setInputValue('comp_sel1','%s',{priority:'event'});", r$id_centre),
              tags$b(r$nom),
              tags$span(style="color:#888;margin-left:4px;", paste0("— ", r$municipi, " (", r$comarca, ")"))
          )
        })
    )
  })
  output$ui_comp_dropdown2 = renderUI({
    q = input$comp_cerca2_input
    if (is.null(q) || nchar(trimws(q)) < 2) return(NULL)
    res = centres_data %>%
      filter(grepl(trimws(q), nom, ignore.case=TRUE) | grepl(trimws(q), municipi, ignore.case=TRUE)) %>%
      slice_head(n=8)
    if (nrow(res)==0) return(div(class="comp-cerca-dropdown",
                                 div(class="comp-cerca-item", style="color:#888;","Cap resultat")))
    div(class="comp-cerca-dropdown",
        lapply(seq_len(nrow(res)), function(i) {
          r = res[i,]
          div(class="comp-cerca-item",
              tabindex = "0",
              role = "button",
              `aria-label` = paste0("Seleccionar centre ", r$nom, " de ", r$municipi),
              onclick = sprintf("Shiny.setInputValue('comp_sel2','%s',{priority:'event'});", r$id_centre),
              onkeydown = sprintf("if(event.key==='Enter'||event.key===' ') Shiny.setInputValue('comp_sel2','%s',{priority:'event'});", r$id_centre),
              tags$b(r$nom),
              tags$span(style="color:#888;margin-left:4px;", paste0("— ", r$municipi, " (", r$comarca, ")"))
          )
        })
    )
  })
  output$ui_comp_dropdown3 = renderUI({
    q = input$comp_cerca3_input
    if (is.null(q) || nchar(trimws(q)) < 2) return(NULL)
    res = centres_data %>%
      filter(grepl(trimws(q), nom, ignore.case=TRUE) | grepl(trimws(q), municipi, ignore.case=TRUE)) %>%
      slice_head(n=8)
    if (nrow(res)==0) return(div(class="comp-cerca-dropdown",
                                 div(class="comp-cerca-item", style="color:#888;","Cap resultat")))
    div(class="comp-cerca-dropdown",
        lapply(seq_len(nrow(res)), function(i) {
          r = res[i,]
          div(class="comp-cerca-item",
              tabindex = "0",
              role = "button",
              `aria-label` = paste0("Seleccionar centre ", r$nom, " de ", r$municipi),
              onclick = sprintf("Shiny.setInputValue('comp_sel3','%s',{priority:'event'});", r$id_centre),
              onkeydown = sprintf("if(event.key==='Enter'||event.key===' ') Shiny.setInputValue('comp_sel3','%s',{priority:'event'});", r$id_centre),
              tags$b(r$nom),
              tags$span(style="color:#888;margin-left:4px;", paste0("— ", r$municipi, " (", r$comarca, ")"))
          )
        })
    )
  })
  
  observeEvent(input$comp_sel1, {
    rv$comp_id1 = input$comp_sel1
    ct = centres_data %>% filter(id_centre == input$comp_sel1)
    if (nrow(ct)>0) updateTextInput(session, "comp_cerca1_input", value="")
  })
  observeEvent(input$comp_sel2, {
    rv$comp_id2 = input$comp_sel2
    if (!is.null(input$comp_sel2)) updateTextInput(session, "comp_cerca2_input", value="")
  })
  observeEvent(input$comp_sel3, {
    rv$comp_id3 = input$comp_sel3
    if (!is.null(input$comp_sel3)) updateTextInput(session, "comp_cerca3_input", value="")
  })
  
  output$ui_comp_badge1 = renderUI({
    req(!is.null(rv$comp_id1))
    ct = centres_data %>% filter(id_centre == rv$comp_id1)
    req(nrow(ct)>0)
    div(class="comp-selected-badge",
        substr(ct$nom[1], 1, 35),
        span(class="rm",
             onclick="Shiny.setInputValue('comp_clear1', Math.random(), {priority:'event'});",
             "✕")
    )
  })
  output$ui_comp_badge2 = renderUI({
    req(!is.null(rv$comp_id2))
    ct = centres_data %>% filter(id_centre == rv$comp_id2)
    req(nrow(ct)>0)
    div(class="comp-selected-badge",
        substr(ct$nom[1], 1, 35),
        span(class="rm",
             onclick="Shiny.setInputValue('comp_clear2', Math.random(), {priority:'event'});",
             "✕")
    )
  })
  output$ui_comp_badge3 = renderUI({
    req(!is.null(rv$comp_id3))
    ct = centres_data %>% filter(id_centre == rv$comp_id3)
    req(nrow(ct)>0)
    div(class="comp-selected-badge",
        substr(ct$nom[1], 1, 35),
        span(class="rm",
             onclick="Shiny.setInputValue('comp_clear3', Math.random(), {priority:'event'});",
             "✕")
    )
  })
  
  observeEvent(input$comp_clear1, { rv$comp_id1 = NULL })
  observeEvent(input$comp_clear2, { rv$comp_id2 = NULL })
  observeEvent(input$comp_clear3, { rv$comp_id3 = NULL })
  
  observeEvent(rv$centre_sel, {
    req(!is.null(rv$centre_sel))
    rv$comp_id1 = rv$centre_sel
  })
  
  # ── Filtres Panorama ──
  df_p1 = reactive({
    df = centres_data
    cats = input$p1_cats %||% c("te_EINF1C","te_EINF2C","te_EPRI","te_EE")
    if (length(cats)>0) {
      m = rep(FALSE, nrow(df))
      for (c in cats) m = m | (df[[c]] == TRUE)
      df = df[m,]
    }
    tit = input$p1_tit %||% c("Pública","Privada/Concertada")
    if (length(tit)>0)  df = df %>% filter(titularitat %in% tit)
    icqa = input$p1_icqa %||% icqa_nivells
    if (length(icqa)>0) df = df %>% filter(ICQA %in% icqa)
    df
  })
  
  # ── KPIs resum ──
  output$ui_kpis_p1 = renderUI({
    df = df_p1()
    n        = nrow(df)
    pct_cob  = round(mean(df$te_cobertura, na.rm=TRUE)*100, 1)
    n_desf   = sum(df$ICQA %in% c("Desfavorable","Molt desfavorable","Extremadament desfavorable"), na.rm=TRUE)
    pct_oms  = if (n>0 && !all(is.na(df$NO2))) round(mean(df$NO2>10, na.rm=TRUE)*100, 0) else NA
    n_sense  = sum(!df$te_cobertura, na.rm=TRUE)
    
    div(class="kpi-row",
        div(class="kpi-card blue",   div(class="kpi-val", formatC(n,     format="d",big.mark=".")), div(class="kpi-lbl","centres analitzats")),
        div(class="kpi-card green",  div(class="kpi-val", paste0(pct_cob,"%")),                     div(class="kpi-lbl","amb cobertura ≤10 km")),
        div(class="kpi-card orange", div(class="kpi-val", formatC(n_desf, format="d",big.mark=".")),div(class="kpi-lbl","ICQA desfavorable o pitjor")),
        div(class="kpi-card purple", div(class="kpi-val", formatC(n_sense,format="d",big.mark=".")),div(class="kpi-lbl","sense cobertura >10 km")),
        if (!is.na(pct_oms) && pct_oms>0)
          div(class="kpi-card",      div(class="kpi-val", paste0(pct_oms,"%")),                    div(class="kpi-lbl","superen guia OMS NO₂"))
    )
  })
  
  # ── Rànquing de comarques amb navegació per teclat ──
  output$ui_ranking = renderUI({
    cont_rank = input$rank_cont %||% "NO2"
    col_mitj  = switch(cont_rank, "NO2"="NO2_mitj", "PM2.5"="PM25_mitj", "PM10"="PM10_mitj", "O3"="O3_mitj", "NO2_mitj")
    score_col = switch(cont_rank, "NO2"="score_NO2", "PM2.5"="score_PM25", "PM10"="score_PM10", "O3"="score_O3", "score_NO2")
    
    df_r = comarca_stats %>% filter(!is.na(.data[[col_mitj]]))
    
    df_r = if (rv$rank_ord=="millor") {
      df_r %>% arrange(.data[[score_col]]) %>% slice_head(n=12)
    } else {
      df_r %>% arrange(desc(.data[[score_col]])) %>% slice_head(n=12)
    }
    
    mx = max(df_r[[col_mitj]], na.rm=TRUE)
    
    tagList(lapply(seq_len(nrow(df_r)), function(i) {
      r  = df_r[i,]
      v  = as.numeric(r[[col_mitj]])
      if (is.na(v)) v = 0
      w  = round(v / max(mx, 1) * 100)
      col = if(v>40) "#c0392b" else if(v>20) "#e67e22" else "#27ae60"
      div(class="rank-item",
          div(class="rank-num", i),
          div(class="rank-name",
              div(tags$a(href="#", style="color:inherit;text-decoration:none;",
                         onclick=sprintf("Shiny.setInputValue('comarca_click','%s',{priority:'event'});return false;", r$comarca),
                         onkeydown = sprintf("if(event.key==='Enter'||event.key===' ') Shiny.setInputValue('comarca_click','%s',{priority:'event'});", r$comarca),
                         tabindex = "0", role = "button", `aria-label` = paste0("Seleccionar comarca ", r$comarca),
                         r$comarca)),
              div(class="rank-prov", r$provincia)
          ),
          div(class="rank-bar-t", div(class="rank-bar-f", style=paste0("width:",w,"%;background:",col,";"))),
          div(class="rank-val", if(!is.na(r[[col_mitj]])) as.character(r[[col_mitj]]) else "–")
      )
    }))
  })
  
  output$ui_data_context = renderUI({
    div(style="font-size:15px;line-height:1.9;color:#555;",
        div(style="display:flex;align-items:center;gap:8px;margin-bottom:6px;",
            div(style="font-size:20px;","📆"),
            div(tags$strong(style="font-size:14px;color:#e67e22;", format(data_max, "%d de %B de %Y")))
        ),
        div("Darrer dia disponible a la ", tags$strong("XVPCA"), "."),
        div(style="margin-top:6px;color:#888;font-size:15px;",
            paste0("Setmana de referència: ", format(data_inici_setmana, "%d/%m"), " – ", format(data_fi_setmana, "%d/%m/%Y")),
            tags$br(),
            paste0("Concentracions: mitjana IDW de ", n_distinct(stations_geo$CODI.EOI), " estacions")
        )
    )
  })
  
  # =================================================================
  # MAPA DE LA PESTANYA PANORAMA (amb icones SVG + lletra via makeIconList)
  # =================================================================
  
  # Funció generadora d'icones per a tots els centres (utilitzada en render i observe)
  generar_icones_centres <- function(df) {
    urls <- vapply(seq_len(nrow(df)), function(i) {
      icona_icqa_url(icqa_pal[df$ICQA[i]], icqa_lletra[df$ICQA[i]])
    }, character(1))
    
    icons(
      iconUrl     = urls,
      iconWidth   = 22,
      iconHeight  = 22,
      iconAnchorX = 11,
      iconAnchorY = 11
    )
  }
  
  output$mapa_p1 = renderLeaflet({
    df = centres_data
    icones_inicials <- generar_icones_centres(df)
    
    leaflet(options = leafletOptions(zoomControl = TRUE)) %>%
      addProviderTiles(providers$CartoDB.Positron, options = providerTileOptions(opacity = 0.92)) %>%
      setView(lng = 1.73, lat = 41.72, zoom = 7) %>%
      addScaleBar(position = "bottomleft", options = scaleBarOptions(imperial = FALSE)) %>%
      addMarkers(data = df,
                 lat = ~lat,
                 lng = ~lon,
                 layerId = ~paste0("c_", id_centre),
                 icon = icones_inicials,
                 group = "Centres educatius") %>%
      addMarkers(data = stations_geo,
                 lat = ~LATITUD,
                 lng = ~LONGITUD,
                 icon = triangle_icon,
                 layerId = ~paste0("e_", CODI.EOI),
                 group = "Estacions XVPCA") %>%
      addLayersControl(
        overlayGroups = c("Centres educatius", "Estacions XVPCA"),
        options = layersControlOptions(collapsed = TRUE)
      )
  })
  
  # Actualització dels marcadors quan canvien els filtres
  observe({
    df = df_p1()
    req(nrow(df) > 0)
    icones_filtrades <- generar_icones_centres(df)
    
    leafletProxy("mapa_p1") %>%
      clearGroup("Centres educatius") %>%
      addMarkers(data = df,
                 lat = ~lat,
                 lng = ~lon,
                 layerId = ~paste0("c_", id_centre),
                 icon = icones_filtrades,
                 group = "Centres educatius")
  })
  
  # Popup en clicar marcador
  observeEvent(input$mapa_p1_marker_click, {
    cl = input$mapa_p1_marker_click
    if (is.null(cl$id)) return()
    
    if (startsWith(cl$id, "c_")) {
      centre_id = sub("c_", "", cl$id)
      ct = centres_data %>% filter(id_centre == centre_id)
      if (nrow(ct) == 0) return()
      
      pm25_val = if ("PM2.5" %in% names(ct)) ct[["PM2.5"]][1] else NA_real_
      
      content = paste0(
        "<div style='font-family:DM Sans,sans-serif;font-size:15px;min-width:210px;'>",
        "<b style='font-size:15px;font-family:Syne,sans-serif;'>", ct$nom[1], "</b><br>",
        "<span style='color:#888;font-size:15px;'>", ct$titularitat[1], " · ",
        ct$municipi[1], ", ", ct$comarca[1], "</span><br>",
        "<hr style='margin:5px 0;border-color:#f0f0f0;'>",
        "<b style='color:", icqa_pal[ct$ICQA[1]], ";'>⬤ ", ct$ICQA[1], "</b><br>",
        "NO₂ <b>", ifelse(is.na(ct$NO2[1]), "–", round(ct$NO2[1], 1)), "</b> · ",
        "PM2.5 <b>", ifelse(is.na(pm25_val), "–", round(pm25_val, 1)), "</b> · ",
        "PM10 <b>", ifelse(is.na(ct$PM10[1]), "–", round(ct$PM10[1], 1)), "</b> · ",
        "O₃ <b>", ifelse(is.na(ct$O3[1]), "–", round(ct$O3[1], 1)), "</b> µg/m³<br>",
        "Dist. estació: <b>", round(ct$dist_min_km[1], 1), " km</b><br>",
        "<span onclick=\"Shiny.setInputValue('centre_click_map','", centre_id,
        "',{priority:'event'})\" ",
        "style='color:#ff4d4d;cursor:pointer;font-size:15px;font-weight:700;'>",
        "📊 Comparativa →</span>",
        "</div>"
      )
      
      rv$centre_sel = centre_id
      leafletProxy("mapa_p1") %>%
        clearPopups() %>%
        addPopups(
          lng     = cl$lng,
          lat     = cl$lat,
          popup   = content,
          options = popupOptions(closeButton = TRUE, maxWidth = 260)
        ) %>%
        setView(lng = cl$lng, lat = cl$lat, zoom = 14)
      
    } else if (startsWith(cl$id, "e_")) {
      est_codi = sub("e_", "", cl$id)
      est = stations_geo %>% filter(CODI.EOI == est_codi)
      if (nrow(est) == 0) return()
      
      content = paste0(
        "<div style='font-family:DM Sans,sans-serif;font-size:15px;min-width:175px;'>",
        "<b style='font-family:Syne,sans-serif;font-size:15px;color:#005f73;'>▲ ",
        est$NOM.ESTACIO[1], "</b><br>",
        "<span style='color:#888;font-size:15px;'>", est$TIPUS.ESTACIO[1], "</span>",
        "<hr style='margin:4px 0;border-color:#f0f0f0;'>",
        "NO₂: <b>", ifelse(is.na(est$NO2[1]), "–", round(est$NO2[1], 1)), " µg/m³</b><br>",
        "PM10: <b>", ifelse(is.na(est$PM10[1]), "–", round(est$PM10[1], 1)), " µg/m³</b><br>",
        "O₃: <b>", ifelse(is.na(est$O3[1]), "–", round(est$O3[1], 1)), " µg/m³</b>",
        "</div>"
      )
      
      leafletProxy("mapa_p1") %>%
        clearPopups() %>%
        addPopups(
          lng     = cl$lng,
          lat     = cl$lat,
          popup   = content,
          options = popupOptions(closeButton = TRUE, maxWidth = 220)
        )
    }
  })
  
  # ── Cercador de centres, comarques i municipis (amb teclat) ──
  observeEvent(input$cerca_centre, {
    rv$centre_sel = input$cerca_centre
    rv$comp_id1 = input$cerca_centre
    updateTextInput(session, "cerca_input", value="")
    ct = centres_data %>% filter(id_centre == input$cerca_centre)
    if (nrow(ct) > 0) {
      leafletProxy("mapa_p1") %>%
        setView(lng=ct$lon[1], lat=ct$lat[1], zoom=14) %>%
        clearPopups() %>%
        addPopups(lng=ct$lon[1], lat=ct$lat[1],
                  popup = paste0(
                    "<b>", ct$nom, "</b><br>",
                    ct$municipi, ", ", ct$comarca, "<br>",
                    "ICQA: <b style='color:", icqa_pal[ct$ICQA], "'>", ct$ICQA, "</b>"
                  ),
                  options = popupOptions(closeButton=TRUE, maxWidth=220))
    }
  })
  
  output$ui_cerca_llista = renderUI({
    q = input$cerca_input
    if (is.null(q) || nchar(trimws(q))<2) return(NULL)
    q = trimws(q)
    
    r_com = sort(unique(centres_data$comarca[grepl(q, centres_data$comarca, ignore.case=TRUE)]))
    r_mun = sort(unique(centres_data$municipi[grepl(q, centres_data$municipi, ignore.case=TRUE)]))
    r_cen = centres_data %>% filter(grepl(q, nom, ignore.case=TRUE)) %>% slice_head(n=6)
    
    if (length(r_com)+length(r_mun)+nrow(r_cen)==0)
      return(tags$div(style="font-size:15px;color:#888;padding:6px 4px;","Cap resultat trobat"))
    
    div(id="cerca_llista",
        if (length(r_com)>0) lapply(head(r_com,3), function(c) 
          div(class="cerca-item",
              tabindex = "0", role = "button",
              `aria-label` = paste0("Seleccionar comarca ", c),
              onclick = sprintf("Shiny.setInputValue('cerca_comarca','%s',{priority:'event'});", c),
              onkeydown = sprintf("if(event.key==='Enter'||event.key===' ') Shiny.setInputValue('cerca_comarca','%s',{priority:'event'});", c),
              span(class="cerca-tag tag-comarca","COMARCA"), tags$b(c))),
        if (length(r_mun)>0) lapply(head(r_mun,3), function(m) 
          div(class="cerca-item",
              tabindex = "0", role = "button",
              `aria-label` = paste0("Seleccionar municipi ", m),
              onclick = sprintf("Shiny.setInputValue('cerca_municipi','%s',{priority:'event'});", m),
              onkeydown = sprintf("if(event.key==='Enter'||event.key===' ') Shiny.setInputValue('cerca_municipi','%s',{priority:'event'});", m),
              span(class="cerca-tag tag-municipi","MUNICIPI"), tags$b(m))),
        if (nrow(r_cen)>0) lapply(seq_len(nrow(r_cen)), function(i) { r = r_cen[i,]
        div(class="cerca-item",
            tabindex = "0", role = "button",
            `aria-label` = paste0("Seleccionar centre ", r$nom),
            onclick = sprintf("Shiny.setInputValue('cerca_centre','%s',{priority:'event'});", r$id_centre),
            onkeydown = sprintf("if(event.key==='Enter'||event.key===' ') Shiny.setInputValue('cerca_centre','%s',{priority:'event'});", r$id_centre),
            span(class="cerca-tag tag-centre","CENTRE"), tags$b(r$nom), tags$br(),
            span(style="font-size:15px;color:#888;","ICQA: ",
                 span(style=paste0("color:",icqa_pal[r$ICQA],";font-weight:700;"),r$ICQA)
            )
        )
        })
    )
  })
  
  observeEvent(input$cerca_comarca, {
    updateTextInput(session,"cerca_input",value="")
    bb = comarca_bbox %>% filter(comarca==input$cerca_comarca)
    if (nrow(bb)>0) leafletProxy("mapa_p1") %>%
      fitBounds(bb$lon_min-.05, bb$lat_min-.05, bb$lon_max+.05, bb$lat_max+.05)
  })
  observeEvent(input$cerca_municipi, {
    updateTextInput(session,"cerca_input",value="")
    cm = centres_data %>% filter(municipi==input$cerca_municipi)
    if (nrow(cm)>0) leafletProxy("mapa_p1") %>%
      setView(lng=mean(cm$lon,na.rm=TRUE), lat=mean(cm$lat,na.rm=TRUE), zoom=13)
  })
  
  # ── COMPARATIVA A LA CARTA (targetes amb teclat) ──
  centres_comparativa_react = reactive({
    ids = c(rv$comp_id1, rv$comp_id2, rv$comp_id3)
    ids = ids[!is.null(ids) & !is.na(ids) & ids != ""]
    if (length(ids) == 0) return(centres_data %>% slice(0))
    centres_data %>% filter(id_centre %in% ids) %>%
      mutate(ord = match(id_centre, ids)) %>% arrange(ord)
  })
  
  output$ui_centre_header = renderUI({
    df = centres_comparativa_react()
    if (nrow(df)==0) {
      return(div(
        tags$h4(style="font-family:Syne,sans-serif;margin:0 0 4px;color:#888;","Comparativa a la carta"),
        tags$p(style="font-size:15px;color:#aaa;margin:0 0 10px;","Cerca centres als buscadors per comparar la seva qualitat de l'aire.")
      ))
    }
    ct = df[1,]
    div(
      div(class="hier-bc",
          span(style="color:#aaa;",ct$provincia),
          span(class="sep","›"), ct$comarca,
          span(class="sep","›"), ct$municipi,
          span(class="sep","›"), span(class="act",ct$nom)
      ),
      tags$h4(style="font-family:Syne,sans-serif;margin:0 0 4px;","Comparativa a la carta")
    )
  })
  
  output$ui_centres_cards = renderUI({
    df = centres_comparativa_react()
    if (nrow(df)==0)
      return(div(style="color:#aaa;font-size:15px;padding:10px;background:#f9f9f9;border-radius:8px;text-align:center;",
                 "🔍 Cerca un centre al buscador per veure la comparativa"))
    
    tagList(lapply(seq_len(nrow(df)), function(i) {
      r    = df[i,]
      cols = c("#ff4d4d","#3498db","#27ae60")
      col  = cols[(i-1)%%length(cols)+1]
      te_dades_spider = !is.na(r$NO2) && !is.na(r$PM10) && !is.na(r$O3)
      div(class=paste0("ctre-card", if(i==1)" sel" else ""),
          style=paste0("border-left-color:", col, ";"),
          tabindex = "0", role = "button",
          `aria-label` = paste0("Centre ", r$nom, ", ICQA ", r$ICQA),
          onclick = sprintf("Shiny.setInputValue('comp_sel_card','%s',{priority:'event'});", r$id_centre),
          onkeydown = sprintf("if(event.key==='Enter'||event.key===' ') Shiny.setInputValue('comp_sel_card','%s',{priority:'event'});", r$id_centre),
          div(class="ctre-nom", paste0(i,". ", r$nom)),
          div(class="ctre-info", r$titularitat, " · ", r$municipi, " (", r$comarca, ")"),
          div(class="ctre-info", r$categories),
          div(class="ctre-icqa", style=paste0("color:",icqa_pal[r$ICQA]%||%"#aaa",";"),
              "ICQA: ", r$ICQA,
              if(!is.na(r$NO2)) span(style="color:#999;font-weight:400;margin-left:8px;font-size:15px;",
                                     paste0("NO₂ ",round(r$NO2,1)," µg/m³"))
          ),
          if (!te_dades_spider)
            div(style="margin-top:5px;font-size:15px;color:#c0392b;background:#fff0f0;padding:4px 6px;border-radius:4px;",
                "⚠️ Sense dades suficients per al diagrama.")
      )
    }))
  })
  
  observeEvent(input$comp_sel_card, {
    rv$centre_sel <- input$comp_sel_card
    rv$comp_id1 <- input$comp_sel_card
  })
  
  output$plot_radar_centres = renderPlotly({
    df = centres_comparativa_react()
    if (nrow(df)==0) return(plotly_empty() %>%
                              layout(annotations=list(list(text="Cerca centres per veure el diagrama comparatiu",
                                                           showarrow=FALSE,font=list(size=13,color="#aaa"),
                                                           xref="paper",yref="paper",x=0.5,y=0.5))))
    
    df_complet = df %>%
      filter(!is.na(NO2), !is.na(PM10), !is.na(O3))
    
    if (nrow(df_complet)==0) return(plotly_empty())
    
    df_p = df_complet %>%
      mutate(
        NO2_n   = pmin(ifelse(is.na(NO2),   0, NO2)    / 50,  1),
        PM25_n  = pmin(ifelse(is.na(`PM2.5`),0,`PM2.5`)/ 25,  1),
        PM10_n  = pmin(ifelse(is.na(PM10),  0, PM10)   / 50,  1),
        O3_n    = pmin(ifelse(is.na(O3),    0, O3)     / 120, 1)
      )
    
    cols = c("#ff4d4d","#3498db","#27ae60","#e67e22","#9b59b6","#1abc9c")
    fig  = plot_ly(type="scatterpolar", mode="lines+markers", fill="toself")
    
    for (i in seq_len(nrow(df_p))) {
      r   = df_p[i,]
      col = cols[(i-1)%%length(cols)+1]
      fig = fig %>% add_trace(
        r     = c(r$NO2_n, r$PM25_n, r$PM10_n, r$O3_n, r$NO2_n),
        theta = c("NO₂","PM2.5","PM10","O₃","NO₂"),
        name  = substr(r$nom, 1, 25),
        line  = list(color=col, width=2.5),
        fillcolor = paste0(col,"40"), opacity=0.9,
        hovertemplate=paste0("<b>",r$nom,"</b><br>",
                             "NO₂: ",round(r$NO2,1)," · PM2.5: ",
                             ifelse(is.na(r[["PM2.5"]]),"–",round(r[["PM2.5"]],1)),
                             " · PM10: ",round(r$PM10,1)," · O₃: ",round(r$O3,1),
                             "<extra></extra>")
      )
    }
    fig %>% layout(
      polar=list(radialaxis=list(visible=TRUE,range=c(0,1),tickfont=list(size=9))),
      legend=list(font=list(size=10),orientation="v"),
      paper_bgcolor="rgba(0,0,0,0)", margin=list(t=20,b=20,l=20,r=20)
    )
  })
  
  output$ui_interp_centre = renderUI({
    df = centres_comparativa_react()
    if (nrow(df)==0)
      return(div(class="interpret-box",HTML("Cerca centres als buscadors de dalt per veure la comparativa.")))
    if (nrow(df)<2)
      return(div(class="interpret-box",HTML("Afegeix almenys 2 centres per veure la comparativa.")))
    
    ct    = df[1,]
    altres = df[-1,] %>% filter(!is.na(NO2))
    if (nrow(altres)==0 || is.na(ct$NO2))
      return(div(class="interpret-box",HTML("Sense dades suficients per a la comparativa.")))
    
    m_alt = mean(altres$NO2, na.rm=TRUE)
    dif   = round((ct$NO2-m_alt)/max(m_alt,0.1)*100, 1)
    
    txt = paste0(
      "<strong>", ct$nom, "</strong> presenta NO₂ de <strong>", round(ct$NO2,1), " µg/m³</strong>, ",
      if(abs(dif)<5) "similar a la mitjana dels centres comparats"
      else if(dif>0) paste0("<span class='hl'>", abs(dif),"% superior</span> a la mitjana dels centres comparats")
      else paste0("<span class='hl-g'>", abs(dif),"% inferior</span> a la mitjana dels centres comparats"),
      " (", round(m_alt,1), " µg/m³).",
      if(!is.na(ct$NO2)&&ct$NO2>10) " <span class='hl'>⚠️ Supera la guia anual OMS de 10 µg/m³.</span>"
    )
    div(class="interpret-box", HTML(paste0('<span class="ib-icon">🔍</span>', txt)))
  })
  
  # ══ PESTANYA 2: DESIGUALTATS (VIOLIN PLOTS) ══════════════════════════════
  df_desig = reactive({
    df = centres_data
    if (!is.null(input$d_com) && input$d_com!="Totes") df = df %>% filter(comarca==input$d_com)
    
    cats_sel = input$d_cats
    if (!is.null(cats_sel) && length(cats_sel) > 0 && length(cats_sel) < 4) {
      cm = c(EINF1C="te_EINF1C", EINF2C="te_EINF2C", EPRI="te_EPRI", EE="te_EE")
      cats_no_sel = setdiff(c("EINF1C","EINF2C","EPRI","EE"), cats_sel)
      
      m_inc = rep(FALSE, nrow(df))
      for (c in cats_sel) if (c %in% names(cm)) m_inc = m_inc | (df[[cm[c]]] == TRUE)
      
      m_exc = rep(FALSE, nrow(df))
      for (c in cats_no_sel) if (c %in% names(cm)) m_exc = m_exc | (df[[cm[c]]] == TRUE)
      
      df = df[m_inc & !m_exc, ]
    }
    df
  })
  
  output$ui_interp_desig = renderUI({
    df = df_desig(); cont = input$d_cont; req(nrow(df)>0, cont %in% names(df))
    pub  = df %>% filter(titularitat=="Pública",            !is.na(.data[[cont]])) %>% pull(cont)
    priv = df %>% filter(titularitat=="Privada/Concertada", !is.na(.data[[cont]])) %>% pull(cont)
    urb  = df %>% filter(entorn=="Urbà",  !is.na(.data[[cont]])) %>% pull(cont)
    rur  = df %>% filter(entorn=="Rural", !is.na(.data[[cont]])) %>% pull(cont)
    lns  = character(0)
    if (length(pub)>5&&length(priv)>5) {
      mp=round(mean(pub,na.rm=TRUE),1); mv=round(mean(priv,na.rm=TRUE),1)
      d=round(abs(mp-mv)/max(min(mp,mv),0.1)*100,1)
      lns = c(lns, if(d>5) paste0("<b>Titularitat:</b> centres <b>",if(mp>mv)"públics" else "privats","</b> amb <span class='hl'>",d,"% més</span> ",cont,".")
              else paste0("<b>Titularitat:</b> sense diferències significatives en ",cont," (públics ",mp," vs privats ",mv," µg/m³)."))
    }
    if (length(urb)>5&&length(rur)>5) {
      mu=round(mean(urb,na.rm=TRUE),1); mr=round(mean(rur,na.rm=TRUE),1)
      d=round(abs(mu-mr)/max(min(mu,mr),0.1)*100,1)
      lns = c(lns, paste0("<b>Entorn:</b> zones urbanes presenten <span class='hl'>",d,"% ",if(mu>mr)"més" else "menys","</span> ",cont," que les rurals (",mu," vs ",mr," µg/m³)."))
    }
    div(class="interpret-box", HTML(paste0('<span class="ib-icon">⚖️</span>', paste(lns,collapse=" "))))
  })
  
  output$plot_tit_violin = renderPlotly({
    df = df_desig()
    cont = input$d_cont
    req(nrow(df) > 0, cont %in% names(df))
    
    out = outliers_df(df, "titularitat", cont)
    
    p = plot_ly()
    for (tit in unique(df$titularitat)) {
      d = df %>% filter(titularitat == tit)
      p = p %>% add_trace(
        data = d, x = ~titularitat, y = ~get(cont),
        type = "violin", name = tit,
        color = I(color_tit[tit]),
        box = list(visible = TRUE),
        meanline = list(visible = TRUE, color = "#fff"),
        points = FALSE,
        spanmode = "hard",
        hoverinfo = "none",
        showlegend = FALSE
      )
    }
    if (nrow(out) > 0) {
      p = p %>% add_trace(
        data = out,
        x = ~titularitat, y = ~get(cont),
        type = "scatter", mode = "markers",
        marker = list(
          color = ~color_tit[titularitat],
          size = 5, opacity = 0.75,
          line = list(color = "#fff", width = 0.5)
        ),
        text = ~paste0(
          "<b>", nom, "</b><br>",
          comarca, "<br>",
          cont, ": <b>", round(get(cont), 1), " µg/m³</b>"
        ),
        hoverinfo = "text",
        showlegend = FALSE
      )
    }
    p %>% layout(
      xaxis = list(title = ""),
      yaxis = list(title = paste0(cont, " µg/m³"), rangemode = "tozero"),
      paper_bgcolor = "rgba(0,0,0,0)",
      plot_bgcolor  = "rgba(248,249,250,0.6)"
    )
  })
  
  output$plot_tit_dif_com = renderPlotly({
    df = df_desig(); cont = input$d_cont; req(nrow(df)>0, cont %in% names(df))
    df_a = df %>% filter(!is.na(comarca),!is.na(.data[[cont]])) %>%
      group_by(comarca,titularitat) %>% summarise(m=mean(.data[[cont]],na.rm=TRUE),.groups="drop") %>%
      pivot_wider(names_from=titularitat,values_from=m) %>%
      filter(!is.na(Pública),!is.na(`Privada/Concertada`)) %>%
      mutate(dif=round(`Privada/Concertada`-Pública,1)) %>%
      arrange(desc(abs(dif))) %>% slice_head(n=12)
    req(nrow(df_a)>0)
    plot_ly(df_a,x=~dif,y=~reorder(comarca,dif),type="bar",orientation="h",
            marker=list(color=~ifelse(dif>0,"#e74c3c","#2980b9")),
            hovertemplate=~paste0(comarca,"<br>Diferència: ",ifelse(dif>0,"+",""),dif," µg/m³<extra></extra>")) %>%
      layout(title=list(text=paste0("Diferència privada−pública · ",cont),font=list(size=11)),
             xaxis=list(title="µg/m³",zeroline=TRUE,zerolinecolor="#ccc"),
             yaxis=list(title="",tickfont=list(size=9)),
             paper_bgcolor="rgba(0,0,0,0)",plot_bgcolor="rgba(248,249,250,0.6)",
             margin=list(l=120,r=70,t=30,b=40))
  })
  
  output$plot_entorn_violin = renderPlotly({
    df = df_desig()
    cont = input$d_cont
    req(nrow(df) > 0, cont %in% names(df))
    
    out = outliers_df(df, "entorn", cont)
    
    p = plot_ly()
    for (ent in unique(df$entorn)) {
      d = df %>% filter(entorn == ent)
      p = p %>% add_trace(
        data = d, x = ~entorn, y = ~get(cont),
        type = "violin", name = ent,
        color = I(color_entorn[ent]),
        box = list(visible = TRUE),
        meanline = list(visible = TRUE, color = "#fff"),
        points = FALSE,
        spanmode = "hard",
        hoverinfo = "none",
        showlegend = FALSE
      )
    }
    if (nrow(out) > 0) {
      p = p %>% add_trace(
        data = out,
        x = ~entorn, y = ~get(cont),
        type = "scatter", mode = "markers",
        marker = list(
          color = ~color_entorn[entorn],
          size = 5, opacity = 0.75,
          line = list(color = "#fff", width = 0.5)
        ),
        text = ~paste0(
          "<b>", nom, "</b><br>",
          comarca, "<br>",
          cont, ": <b>", round(get(cont), 1), " µg/m³</b>"
        ),
        hoverinfo = "text",
        showlegend = FALSE
      )
    }
    p %>% layout(
      title  = list(text = paste0(cont, " per entorn urbà/rural"), font = list(size = 11)),
      xaxis  = list(title = ""),
      yaxis  = list(title = paste0(cont, " µg/m³"), rangemode = "tozero"),
      paper_bgcolor = "rgba(0,0,0,0)",
      plot_bgcolor  = "rgba(248,249,250,0.6)"
    )
  })
  
  output$plot_cats_violin = renderPlotly({
    df = df_desig()
    cont = input$d_cont
    req(nrow(df) > 0, cont %in% names(df))
    
    cats_sel = input$d_cats %||% c("EINF1C","EINF2C","EPRI","EE")
    cm = c(EINF1C="te_EINF1C", EINF2C="te_EINF2C", EPRI="te_EPRI", EE="te_EE")
    nm = c(EINF1C="Escola Bressol", EINF2C="Educació Infantil",
           EPRI="Educació Primària", EE="Educació Especial")
    
    dl = bind_rows(lapply(cats_sel, function(c) {
      if (!c %in% names(cm)) return(NULL)
      df %>% filter(.data[[cm[c]]] == TRUE) %>% mutate(cat = nm[c])
    }))
    req(nrow(dl) > 0)
    
    out = outliers_df(dl, "cat", cont)
    
    p = plot_ly()
    for (ct in unique(dl$cat)) {
      d = dl %>% filter(cat == ct)
      p = p %>% add_trace(
        data = d, x = ~cat, y = ~get(cont),
        type = "violin", name = ct,
        color = I(color_cat[ct]),
        box = list(visible = TRUE),
        meanline = list(visible = TRUE, color = "#fff"),
        points = FALSE,
        spanmode = "hard",
        hoverinfo = "none",
        showlegend = FALSE
      )
    }
    if (nrow(out) > 0) {
      p = p %>% add_trace(
        data = out,
        x = ~cat, y = ~get(cont),
        type = "scatter", mode = "markers",
        marker = list(
          color = ~color_cat[cat],
          size = 5, opacity = 0.75,
          line = list(color = "#fff", width = 0.5)
        ),
        text = ~paste0(
          "<b>", nom, "</b><br>",
          comarca, "<br>",
          cont, ": <b>", round(get(cont), 1), " µg/m³</b>"
        ),
        hoverinfo = "text",
        showlegend = FALSE
      )
    }
    p %>% layout(
      xaxis  = list(title = ""),
      yaxis  = list(title = paste0(cont, " µg/m³"), rangemode = "tozero"),
      paper_bgcolor = "rgba(0,0,0,0)",
      plot_bgcolor  = "rgba(248,249,250,0.6)"
    )
  })
  
  # ══ PESTANYA 3: PERFIL HORARI ════════════════════════════════════════════════
  df_horari = reactive({
    cont = input$h_cont; coms = input$h_com
    req(!is.null(cont), length(coms)>0)
    dat = perfil_setmana_raw %>% filter(CONTAMINANT==cont, !is.na(comarca), comarca%in%coms)
    if (nrow(dat)==0) return(NULL)
    dat %>% group_by(comarca,DATA,hora) %>%
      summarise(mitj=mean(concentracio,na.rm=TRUE), sd=sd(concentracio,na.rm=TRUE), .groups="drop") %>%
      mutate(timestamp=as.POSIXct(paste(format(as.Date(DATA),"%Y-%m-%d"),sprintf("%02d:00:00",as.integer(hora))),
                                  format="%Y-%m-%d %H:%M:%S", tz="UTC")) %>%
      filter(!is.na(timestamp)) %>% arrange(comarca,timestamp)
  })
  
  output$h_titol     = renderUI(tags$span(paste0("Perfil horari · ",input$h_cont," · ",format(data_inici_setmana,"%d/%m/%Y")," – ",format(data_fi_setmana,"%d/%m/%Y"))))
  output$h_titol_zoom = renderUI({
    dh = df_horari()
    if (!is.null(dh) && nrow(dh)>0) {
      hores_per_dia2 = dh %>%
        filter(!is.na(timestamp)) %>%
        mutate(dia = as.Date(timestamp)) %>%
        group_by(dia) %>%
        summarise(n_hores = n_distinct(as.integer(format(timestamp, "%H", tz="UTC"))), .groups="drop") %>%
        arrange(desc(n_hores), desc(dia))
      dia_complet2 = hores_per_dia2$dia[1]
      max_h = hores_per_dia2$n_hores[1]
      tags$span(paste0("Zoom franges crítiques: entrada (8–9h), horari lectiu (9–17h) i sortida (17–18h) · ",
                       format(dia_complet2, "%d/%m/%Y"),
                       " (", max_h, " hores de dades)"))
    } else {
      tags$span("Zoom franges crítiques: entrada (8–9h), horari lectiu (9–17h) i sortida (17–18h)")
    }
  })
  
  output$ui_finestra = renderUI(tags$div(style="font-size:15px;color:#888;line-height:1.8;",tags$b("Finestra:"),tags$br(),paste0(format(data_inici_setmana,"%d/%m/%Y")," – ",format(data_fi_setmana,"%d/%m/%Y")),tags$br(),paste0(length(dates_setmana)," dies · ",n_distinct(stations_setmana$CODI.EOI)," estacions")))
  
  output$ui_plot_horari_w = renderUI({
    dh = df_horari()
    if (is.null(dh)||nrow(dh)==0) tags$div(style="padding:50px;text-align:center;color:#888;","No hi ha dades per a la selecció actual.")
    else plotlyOutput("plot_horari", height=400)
  })
  
  output$plot_horari = renderPlotly({
    dh = df_horari(); req(!is.null(dh),nrow(dh)>0)
    cu = unique(dh$comarca); n = length(cu)
    pb = c("#ff4d4d","#3498db","#27ae60","#e67e22","#9b59b6","#1abc9c","#f39c12","#2c3e50")
    pc = setNames(rep_len(pb,n),cu)
    cont = input$h_cont
    ov   = c(NO2=25, `PM2.5`=15, PM10=45, O3=60)
    p    = plot_ly()
    
    sh = list()
    if (isTRUE(input$h_franja)) {
      dp = unique(as.Date(dh$timestamp[!is.na(dh$timestamp)]))
      ym = max(dh$mitj+ifelse(is.na(dh$sd),0,dh$sd),na.rm=TRUE)*1.15
      sh = lapply(dp,function(d) list(type="rect",
                                      x0=as.POSIXct(paste(d,"09:00:00"),tz="UTC"),
                                      x1=as.POSIXct(paste(d,"17:00:00"),tz="UTC"),
                                      y0=0, y1=ym,
                                      fillcolor="rgba(52,152,219,.06)",
                                      line=list(width=0), layer="below"))
    }
    if (isTRUE(input$h_oms) && !is.na(ov[cont])) {
      tr = range(dh$timestamp[!is.na(dh$timestamp)])
      p  = p %>% add_lines(x=tr, y=c(ov[cont],ov[cont]),
                           line=list(color="#e74c3c",dash="dot",width=1.5),
                           name=paste0("OMS ",ov[cont]," µg/m³"), hoverinfo="none")
    }
    for (co in cu) {
      dc = dh %>% filter(comarca==co) %>% arrange(timestamp)
      cl = pc[[co]]
      if (isTRUE(input$h_sd) && any(!is.na(dc$sd))) {
        sc = ifelse(is.na(dc$sd),0,dc$sd)
        p  = p %>% add_ribbons(x=dc$timestamp, ymin=pmax(0,dc$mitj-sc), ymax=dc$mitj+sc,
                               fillcolor=paste0(cl,"20"), line=list(color="transparent"),
                               name=paste0(co," ±SD"), legendgroup=co,
                               showlegend=FALSE, hoverinfo="none")
      }
      p = p %>% add_lines(x=dc$timestamp, y=dc$mitj,
                          line=list(color=cl,width=2), opacity=.8,
                          name=co, legendgroup=co,
                          text=paste0("<b>",co,"</b><br>",
                                      format(dc$timestamp,"%d/%m %H:00",tz="UTC"),"<br>",
                                      round(dc$mitj,1)," µg/m³"),
                          hoverinfo="text",
                          hoverlabel=list(bgcolor=cl,font=list(color="#fff",size=12)))
    }
    um = c(NO2="NO₂ (µg/m³)", `PM2.5`="PM2.5 (µg/m³)", PM10="PM10 (µg/m³)", O3="O₃ (µg/m³)")
    p %>% layout(
      xaxis=list(title="",tickformat="%d/%m %H:%M",tickangle=-30,tickfont=list(size=10),gridcolor="rgba(0,0,0,.03)"),
      yaxis=list(title=um[cont],rangemode="tozero",tickfont=list(size=10),gridcolor="rgba(0,0,0,.04)"),
      legend=list(orientation="h",x=0,y=1.1,font=list(size=10)),
      hovermode="x unified",
      shapes=sh,
      paper_bgcolor="rgba(0,0,0,0)", plot_bgcolor="rgba(248,249,250,.5)",
      margin=list(t=30,b=40,l=55,r=15)
    )
  })
  
  output$vb_hora_max = renderValueBox({
    dh = df_horari()
    if (is.null(dh)||nrow(dh)==0) return(valueBox("–","Hora de màxim",icon=icon("clock"),color="red"))
    mx = dh %>% filter(!is.na(mitj),!is.na(timestamp)) %>% slice_max(mitj,n=1,with_ties=FALSE)
    if (nrow(mx)==0) return(valueBox("–","Hora de màxim",icon=icon("clock"),color="red"))
    valueBox(paste0(format(mx$timestamp,"%H:%M",tz="UTC")," h"),"Hora de màxima concentració",icon=icon("clock"),color="red")
  })
  
  output$vb_conc_esc = renderValueBox({
    dh = df_horari()
    if (is.null(dh)||nrow(dh)==0) return(valueBox("–","Concentr. 9–17h",icon=icon("leaf"),color="blue"))
    m = dh %>% filter(!is.na(timestamp)) %>%
      mutate(h=as.integer(format(timestamp,"%H",tz="UTC"))) %>% filter(h>=9,h<=17) %>%
      summarise(m=mean(mitj,na.rm=TRUE)) %>% pull(m)
    valueBox(paste0(round(m,1)," µg/m³"),"Concentr. mitjana 9–17h",icon=icon("leaf"),color="blue")
  })
  
  output$ui_interp_horari = renderUI({
    dh = df_horari(); req(!is.null(dh),nrow(dh)>0)
    cont = input$h_cont; ov = c(NO2=25, `PM2.5`=15, PM10=45, O3=60)
    mx   = dh %>% filter(!is.na(mitj)) %>% slice_max(mitj,n=1,with_ties=FALSE)
    esc  = dh %>% filter(!is.na(timestamp)) %>%
      mutate(hh=as.integer(format(timestamp,"%H",tz="UTC"))) %>% filter(hh>=9,hh<=17) %>%
      summarise(m=mean(mitj,na.rm=TRUE)) %>% pull(m)
    noc  = dh %>% filter(!is.na(timestamp)) %>%
      mutate(hh=as.integer(format(timestamp,"%H",tz="UTC"))) %>% filter(hh>=22|hh<=6) %>%
      summarise(m=mean(mitj,na.rm=TRUE)) %>% pull(m)
    dif  = if (!is.na(esc)&&!is.na(noc)&&noc>0) round((esc-noc)/noc*100,1) else NA
    txt  = paste0(
      "Durant la franja escolar (9–17h), la concentració de <b>",cont,"</b> és de <b>",round(esc,1)," µg/m³</b>",
      if(!is.na(ov[cont])&&!is.na(esc)&&esc>ov[cont])
        paste0(" (<span class='hl'>supera la guia OMS de ",ov[cont]," µg/m³</span>)") else "",
      ".",
      if(!is.na(dif)) paste0(" En comparació amb la nit, és un <b>",abs(dif),"%</b> ",if(dif>0)"superior" else "inferior","."),
      if(nrow(mx)>0) paste0(" El pic màxim s'observa a les <b>",format(mx$timestamp[1],"%H:%M",tz="UTC"),"h</b> (comarca <b>",mx$comarca[1],"</b>, ",round(mx$mitj[1],1)," µg/m³).")
    )
    div(class="interpret-box",HTML(paste0('<span class="ib-icon">⏱️</span>',txt)))
  })
  
  output$plot_franges = renderPlotly({
    dh = df_horari()
    validate(
      need(!is.null(dh) && nrow(dh) > 0, "No hi ha dades per a les comarques seleccionades.")
    )
    
    hores_per_dia = dh %>%
      filter(!is.na(timestamp)) %>%
      mutate(dia = as.Date(timestamp)) %>%
      group_by(dia) %>%
      summarise(n_hores = n_distinct(as.integer(format(timestamp, "%H", tz="UTC"))), .groups="drop") %>%
      arrange(desc(n_hores), desc(dia))
    dia_complet = hores_per_dia$dia[1]
    
    df_fr = dh %>%
      filter(!is.na(timestamp), as.Date(timestamp) == dia_complet) %>%
      mutate(hh = as.integer(format(timestamp,"%H",tz="UTC"))) %>%
      filter(hh >= 8, hh <= 18) %>%
      group_by(comarca, hh) %>%
      summarise(mitj = mean(mitj, na.rm=TRUE), .groups="drop")
    
    validate(
      need(nrow(df_fr) > 0, paste("No hi ha dades horàries per al dia seleccionat (", format(dia_complet, "%d/%m/%Y"), ")."))
    )
    
    pb   = c("#ff4d4d","#3498db","#27ae60","#e67e22","#9b59b6","#1abc9c","#f39c12")
    cu   = unique(df_fr$comarca)
    pc   = setNames(rep_len(pb,length(cu)),cu)
    cont = input$h_cont
    ov   = c(NO2=25, `PM2.5`=15, PM10=45, O3=60)
    
    p = plot_ly()
    for (co in cu) {
      dc = df_fr %>% filter(comarca==co) %>% arrange(hh)
      hover_txt = paste0("<b>", co, "</b> · ", dc$hh, "h · ", round(dc$mitj, 1), " µg/m³")
      p = p %>% add_trace(
        type = "scatter",
        mode = "lines+markers",
        x = dc$hh,
        y = dc$mitj,
        name = co,
        line = list(color = pc[[co]], width = 2.2),
        marker = list(color = pc[[co]], size = 5),
        text = hover_txt,
        hoverinfo = "text",
        showlegend = TRUE
      )
    }
    
    sh_fr = list(
      list(type="rect", x0=8, x1=9, y0=0, y1=1, yref="paper",
           fillcolor="rgba(230,126,34,.18)", line=list(width=0)),
      list(type="rect", x0=9, x1=17, y0=0, y1=1, yref="paper",
           fillcolor="rgba(52,152,219,.08)",
           line=list(width=0.8, color="rgba(52,152,219,.3)", dash="dot")),
      list(type="rect", x0=17, x1=18, y0=0, y1=1, yref="paper",
           fillcolor="rgba(192,57,43,.18)", line=list(width=0))
    )
    if (!is.na(ov[cont]))
      sh_fr = c(sh_fr, list(list(type="line", x0=8, x1=18, xref="x", y0=ov[cont], y1=ov[cont],
                                 line=list(color="#e74c3c", dash="dot", width=1.5))))
    
    annots = list(
      list(x=8.5, y=1, yref="paper", text="Entrada (8–9h)", showarrow=FALSE,
           font=list(size=9,color="#e67e22",family="Syne,sans-serif"), xanchor="center", yanchor="bottom"),
      list(x=13, y=1, yref="paper", text="Horari lectiu (9–17h)", showarrow=FALSE,
           font=list(size=9,color="#2980b9",family="Syne,sans-serif"), xanchor="center", yanchor="bottom"),
      list(x=17.5, y=1, yref="paper", text="Sortida (17–18h)", showarrow=FALSE,
           font=list(size=9,color="#c0392b",family="Syne,sans-serif"), xanchor="center", yanchor="bottom")
    )
    
    p %>% layout(
      xaxis=list(title="Hora del dia", range=c(7.8,18.2),
                 tickvals=8:18, ticktext=paste0(8:18,"h"),
                 tickfont=list(size=10)),
      yaxis=list(title=paste0(cont," µg/m³"), rangemode="tozero"),
      hovermode="closest",
      shapes=sh_fr, annotations=annots,
      legend=list(orientation="h",font=list(size=10)),
      paper_bgcolor="rgba(0,0,0,0)", plot_bgcolor="rgba(248,249,250,.5)",
      margin=list(t=25,b=40,l=55,r=15)
    )
  })
  
  # ══ PESTANYA 4: COBERTURA · BUITS ════════════════════════════════════════════
  df_cob = reactive({
    df = centres_data
    if (!is.null(input$c_prov) && input$c_prov!="Totes") df = df %>% filter(provincia==input$c_prov)
    
    cats_sel = input$c_cats
    if (!is.null(cats_sel) && length(cats_sel) > 0 && length(cats_sel) < 4) {
      cm = c(EINF1C="te_EINF1C", EINF2C="te_EINF2C", EPRI="te_EPRI", EE="te_EE")
      cats_no_sel = setdiff(c("EINF1C","EINF2C","EPRI","EE"), cats_sel)
      
      m_inc = rep(FALSE, nrow(df))
      for (c in cats_sel) if (c %in% names(cm)) m_inc = m_inc | (df[[cm[c]]] == TRUE)
      
      m_exc = rep(FALSE, nrow(df))
      for (c in cats_no_sel) if (c %in% names(cm)) m_exc = m_exc | (df[[cm[c]]] == TRUE)
      
      df = df[m_inc & !m_exc, ]
    }
    df
  })
  
  output$plot_cob_com = renderPlotly({
    df = df_cob() %>% filter(!is.na(comarca)) %>%
      group_by(comarca,provincia) %>%
      summarise(total=n(), sense=sum(!te_cobertura,na.rm=TRUE),
                pct=round(sense/total*100,1), .groups="drop") %>%
      filter(sense>0) %>% arrange(desc(pct)) %>% slice_head(n=20)
    req(nrow(df)>0)
    plot_ly(df,x=~pct,y=~reorder(paste0(comarca," (",provincia,")"),pct),type="bar",orientation="h",
            marker=list(color=~ifelse(pct>75,"#8e44ad",ifelse(pct>50,"#c0392b",ifelse(pct>25,"#e67e22","#f4d03f"))),opacity=.85),
            text=~paste0(pct,"% (",sense,"/",total,")"),textposition="outside",hoverinfo="text") %>%
      layout(
        xaxis=list(title="% de centres sense estació ≤10 km",range=c(0,110),tickfont=list(size=10)),
        yaxis=list(title="",tickfont=list(size=10)),
        shapes=list(list(type="line",x0=50,x1=50,y0=0,y1=1,yref="paper",line=list(color="#c0392b",dash="dot",width=1.5))),
        paper_bgcolor="rgba(0,0,0,0)",plot_bgcolor="rgba(248,249,250,.5)",
        margin=list(l=195,r=90,t=10,b=50))
  })
  
  output$mapa_cob = renderLeaflet({
    df = df_cob() %>% filter(!te_cobertura,!is.na(lat),!is.na(lon))
    leaflet(df,options=leafletOptions(zoomControl=TRUE)) %>%
      addProviderTiles(providers$CartoDB.Positron,options=providerTileOptions(opacity=.92)) %>%
      setView(lng=1.73,lat=41.72,zoom=7) %>%
      addCircleMarkers(lat=~lat,lng=~lon,radius=4,fillColor="#e74c3c",fillOpacity=.7,
                       stroke=TRUE,weight=.5,color="#fff",
                       popup=~paste0("<b>",nom,"</b><br>",comarca,"<br>Dist. estació: ",round(dist_min_km,1)," km")) %>%
      addScaleBar(position="bottomleft",options=scaleBarOptions(imperial=FALSE))
  })
  
  output$plot_cob_prov = renderPlotly({
    df = df_cob() %>%
      filter(!is.na(provincia), provincia %in% c("Barcelona","Girona","Lleida","Tarragona")) %>%
      group_by(provincia) %>%
      summarise(pct=round(mean(!te_cobertura,na.rm=TRUE)*100,1), .groups="drop") %>%
      arrange(desc(pct))
    req(nrow(df)>0)
    prov_colors = c("Barcelona"="#e74c3c","Girona"="#3498db","Lleida"="#f39c12","Tarragona"="#27ae60")
    plot_ly(df, x=~provincia, y=~pct, type="bar",
            marker=list(color=prov_colors[df$provincia]),
            text=~paste0(pct,"%"), textposition="outside", hoverinfo="none") %>%
      layout(xaxis=list(title=""), yaxis=list(title="% sense cob.",range=c(0,105)),
             paper_bgcolor="rgba(0,0,0,0)", plot_bgcolor="rgba(248,249,250,.5)",
             margin=list(t=10,b=30))
  })
  
  output$plot_cob_cat = renderPlotly({
    df = df_cob()
    cats_sel = input$c_cats %||% c("EINF1C","EINF2C","EPRI","EE")
    cm = c(EINF1C="te_EINF1C", EINF2C="te_EINF2C", EPRI="te_EPRI", EE="te_EE")
    nm = c(EINF1C="Escola Bressol", EINF2C="Educació Infantil", EPRI="Educació Primària", EE="Educació Especial")
    
    dc = bind_rows(lapply(cats_sel, function(c) {
      if (!c %in% names(cm)) return(NULL)
      df %>% filter(.data[[cm[c]]] == TRUE) %>%
        mutate(cat = nm[c]) %>%
        group_by(cat) %>%
        summarise(pct = round(mean(!te_cobertura, na.rm=TRUE)*100, 1), .groups="drop")
    }))
    
    req(nrow(dc)>0)
    plot_ly(dc,x=~cat,y=~pct,type="bar",
            marker=list(color=color_cat[dc$cat]),
            text=~paste0(pct,"%"),textposition="outside",hoverinfo="none") %>%
      layout(xaxis=list(title="",tickfont=list(size=9)),
             yaxis=list(title="% sense cob.",range=c(0,105)),
             paper_bgcolor="rgba(0,0,0,0)",plot_bgcolor="rgba(248,249,250,.5)",
             margin=list(t=10,b=30))
  })
  
  output$ui_interp_cobert = renderUI({
    df = df_cob()
    n_total  = nrow(df)
    n_sense  = sum(!df$te_cobertura,na.rm=TRUE)
    pct_gl   = round(n_sense/n_total*100,1)
    pitjors  = df_cob() %>%
      filter(!is.na(comarca), provincia %in% c("Barcelona","Girona","Lleida","Tarragona")) %>%
      group_by(comarca,provincia) %>%
      summarise(pct=round(mean(!te_cobertura,na.rm=TRUE)*100,1),.groups="drop") %>%
      arrange(desc(pct)) %>% slice_head(n=3)
    txt = paste0(
      "A tot Catalunya, <strong>", n_sense, " centres educatius (", pct_gl, "%)</strong> ",
      "no tenen cap estació de mesura a menys de 10 km. Això significa que per a aquests centres ",
      "<span class='hl'>és impossible estimar amb fiabilitat la qualitat de l'aire</span> que respiren els seus alumnes.",
      if (nrow(pitjors)>0) paste0(" Les comarques amb major percentatge de centres sense cobertura són: ",
                                  paste(lapply(seq_len(nrow(pitjors)), function(i)
                                    paste0("<strong>",pitjors$comarca[i],"</strong> (",pitjors$pct[i],"%, ",pitjors$provincia[i],")")),
                                    collapse=", "), ".")
    )
    div(class="interpret-box",
        HTML(paste0('<span class="ib-icon">📡</span>',txt,
                    '<br><br><strong>Implicació per a la política pública:</strong> les zones amb pitjor cobertura coincideixen',
                    ' majoritàriament amb àrees rurals on la xarxa XVPCA no hi arriba. Ampliar-la seria la mesura',
                    ' prioritària per garantir el dret dels alumnes a saber l\'aire que respiren.'))
    )
  })
}

# ============================================================
# EXECUTEM L'APLICACIÓ SHINY
# ============================================================
shinyApp(ui = ui, server = server)
