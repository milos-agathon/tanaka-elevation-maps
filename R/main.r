# 1. Packages
#------------
install.packages("pacman")
pacman::p_load(
    geodata, sf, elevatr, terra, 
    tidyterra, colorspace, tidyverse,
    metR, ggspatial, rayshader
)

# 2. Boundary
#------------
country_sf <- geodata::gadm(
    country = "ITA",
    level = 1, path = tempdir()
) |>
    sf::st_as_sf()

tail(country_sf)

region_sf <- subset(country_sf, NAME_1 == "Toscana")
plot(
    sf::st_geometry(country_sf),
    col = "grey70"
)
plot(
    sf::st_geometry(region_sf),
    col = "maroon", add = TRUE
)

# 3. Download & prepare DEM
#--------------------------

dem_rast <- elevatr::get_elev_raster(
    region_sf,
    z = 9, clip = "locations"
)

# print(dem_rast)

# Pseudo-Mercator projection
proj <- "EPSG:3857"

# Convert raster to data.frame for ggplot
dem_rast_proj <- dem_rast |>
    terra::rast() |>
    terra::project(proj)

dem_df <- as.data.frame(dem_rast_proj, xy = TRUE)

# head(dem_df)
colnames(dem_df) <- c("x", "y", "elevation")
head(dem_df)

# 4. Compute breaks & limits
# --------------------------
limits <- range(dem_df$elevation, na.rm = TRUE)
breaks <- seq(
    floor(limits[1] / 50) * 50,
    ceiling(limits[2] / 50) * 50,
    by = 200
)

# 5. Build your hypsometric palette
# ---------------------------------
pal_vec <- tidyterra::hypso.colors2(
    n = 12,
    palette = "dem_poster",
    alpha = 1,
    rev = FALSE
)

# pie(rep(1, length(pal_vec)), col = pal_vec)
# pal <- pal_vec[c(1:6)]
# pie(rep(1, length(pal)), col = pal)

light_col <- colorspace::lighten(pal[2], amount = 0.15)
dark_col <- colorspace::darken(pal[5], amount = 0.25)

# 6. Define a custom theme
# ------------------------
theme_for_the_win <- function() {
    theme_minimal(base_family = "Helvetica") +
        theme(
            axis.line = element_blank(),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            plot.background = element_rect(
                fill = "white", color = NA
            ),
            plot.title = element_text(
                size = 16, color = "grey10",
                hjust = .5, margin = margin(b = 5),
                vjust = -5
            ),
            plot.caption = element_text(
                size = 8, face = "italic", hjust = 1,
                margin = margin(t = 5), vjust = 15
            ),
            plot.margin = unit(
                c(
                    t = .1, r = .1,
                    l = .1, b = .1
                ), "lines"
            ),
            legend.position = "right"
        )
}

# 7. Build the 2D Tanaka‐style map
# --------------------------------
gg_tanaka_hypso <- ggplot(
    data = dem_df, aes(x = x, y = y, z = elevation)
) +
    geom_contour_fill(
        breaks = breaks
    ) +
    scale_fill_gradientn(
        name = "Elevation",
        colors = pal,
        breaks = breaks,
        labels = round(breaks, 0),
        limits = limits,
        guide = guide_colourbar(
            title.position = "top", 
            title.hjust = .5,
            ticks = FALSE, 
            barheight = unit(5, "cm"),
            frame.colour = NA
        )
    ) +
    metR::geom_contour_tanaka(
        breaks = breaks,
        sun.angle = 45,
        light = light_col,
        dark = dark_col,
        range = c(0.01, 0.3),
        smooth = 0.8
    ) +
    ggspatial::annotation_scale(
        location = "bl",
        width_hint = 0.25,
        text_cex = 0.7
    ) +
    ggspatial::annotation_north_arrow(
        location = "bl",
        which_north = "true",
        pad_x = unit(0.1, "in"),
        pad_y = unit(0.6, "in"),
        style = north_arrow_fancy_orienteering()
    ) +
    coord_sf(crs = proj) +
    labs(
        title = "Tuscany: Digital Elevation Model",
        caption = "Data: Amazon Web Services Tiles"
    ) +
    theme_for_the_win()

ggsave(
    "tuscany-tanaka-2d.png", gg_tanaka_hypso,
    width = 7, height = 7, bg = "white"
)

# 8. 3D extrusion & high‐quality render
# -------------------------------------
rayshader::plot_gg(
    ggobj = gg_tanaka_hypso,
    width = 7,
    height = 7,
    scale = 150,
    shadow = TRUE,
    shadow_intensity = 1,
    windowsize = c(700, 700),
    zoom = 0.55,
    phi = 60,
    theta = 0,
    background = "white",
    multicore = TRUE
)

# 9. Prepare HDR environment map
# ------------------------------
u <- "https://dl.polyhaven.org/file/ph-assets/HDRIs/hdr/4k/venice_sunrise_4k.hdr"
hdri_file <- basename(u)

download.file(
    url = u,
    destfile = hdri_file,
    mode = "wb"
)
# 10. Final high‐quality PNG
# --------------------------
rayshader::render_highquality(
    filename = "tuscany-tanaka-3d-test.png",
    preview = TRUE,
    light = FALSE,
    environment_light = hdri_file,
    intensity = 3,
    rotate_env = 90,
    parallel = TRUE,
    width = 1800,
    height = 1800,
    interactive = FALSE
)

