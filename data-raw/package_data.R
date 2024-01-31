## code to prepare hem_fields dataset goes here
library(sf)
library(data.table)

hem_fields <- sf::st_read(
  dsn = "./data-raw/HEM/HEM_fields.gdb",
  layer = "HEM_fields_update_elev_threshold_slope_limit"
) |>
  names() |>
  data.table::data.table(names = _)

albers <-
  '
PROJCS[
  "NAD_1983_BC_Environment_Albers",
  GEOGCS[
    "GCS_North_American_1983",
    DATUM[
      "D_North_American_1983",
      SPHEROID[
        "GRS_1980",
        6378137.0,
        298.257222101
      ]
    ],
    PRIMEM[
      "Greenwich",
      0.0
    ],
    UNIT[
      "Degree",
      0.0174532925199433
    ]
  ],
  PROJECTION["Albers"],
  PARAMETER["False_Easting",1000000.0],
  PARAMETER["False_Northing",0.0],
  PARAMETER["Central_Meridian",-126.0],
  PARAMETER["Standard_Parallel_1",50.0],
  PARAMETER["Standard_Parallel_2",58.5],
  PARAMETER["Latitude_Of_Origin",45.0],
  UNIT["Meter",1.0]
]
'

usethis::use_data(hem_fields, albers, overwrite = TRUE)
