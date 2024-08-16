#' @import dplyr

# Configuration needed by ui and server

# start and end year - amend when new data added to database

start_year <- 2000
end_year <- 2023
start_year_dft <- 2000
end_year_dft <- 2023

icon_road <- leaflet::makeAwesomeIcon(
  icon = "road",
  iconColor = "#FFFFFF",
  markerColor = "darkblue",
  library = "fa"
)

# named list of modes
modes <-
  list(
    "all_mv",
    "pedal_cycles",
    "two_wheeled_mv",
    "cars_and_taxis",
    "buses_and_coaches",
    "lgvs",
    "hgvs_all"
  )

names(modes) <-
  c("All motorised",
    "Bicycle",
    "Motorcycle",
    "Cars/taxis",
    "Bus/coach",
    "LGV",
    "HGV")

# named list of DfT regions

regions <- as.list(10001:10011)
names(regions) <- c(
  "South West",
  "East Midlands",
  "Scotland",
  "Wales",
  "North West",
  "London",
  "East of England",
  "Yorkshire and The Humber",
  "South East",
  "West Midlands",
  "North East"
)

regions_dft <- as.list(1:11)
names(regions_dft) <- c(
  "South West",
  "East Midlands",
  "Scotland",
  "Wales",
  "North West",
  "London",
  "East of England",
  "Yorkshire and The Humber",
  "South East",
  "West Midlands",
  "North East"
)

# regions bounding boxes

lng1 <- c(-6.318284,
          -2.0199759,
          -7.5004947,
          -5.265306,
          -3.587313,
          -0.5081202,
          -0.7006804,
          -2.55798854,
          -1.955052,
          -3.135146,
          -2.5994797)
lat1 <- c(49.914590,
          51.9819169,
          54.6901911,
          51.391254,
          52.973318,
          51.2923594,
          51.4518961,
          53.30541703,
          50.594969,
          51.833302,
          54.4707861)
lng2 <- c(-1.532656,
          0.3396256,
          -0.8356425,
          -2.660596,
          -1.925699,
          0.2872065,
          1.7540324,
          0.02866016,
          1.439483,
          -1.183902,
          -0.7991049)
lat2 <- c(52.046743,
          53.614324,
          60.7852752,
          53.409645,
          55.049492,
          51.6878102,
          52.9408214,
          54.55272066,
          52.196044,
          53.213192,
          55.8067195)

regions_bbox <- data.frame(region = c(10001:10011), lng1, lat1, lng2, lat2)

# named list of local authority areas
las <-
  as.list(c(1:214))
names(las) <-
  c(
    "Isles of Scilly",
    "Nottinghamshire",
    "Glasgow City",
    "North Lanarkshire",
    "Somerset",
    "Newport",
    "Bridgend",
    "Swansea",
    "Isle of Anglesey",
    "Gwynedd",
    "Conwy",
    "Denbighshire",
    "Monmouthshire",
    "Powys",
    "Carmarthenshire",
    "Pembrokeshire",
    "Neath Port Talbot",
    "The Vale of Glamorgan",
    "Cardiff",
    "Flintshire",
    "Merthyr Tydfil",
    "Rhondda, Cynon, Taff",
    "Ceredigion",
    "Blaenau Gwent",
    "Torfaen",
    "Wrexham",
    "West Lothian",
    "Renfrewshire",
    "City of Edinburgh",
    "Falkirk",
    "Perth & Kinross",
    "Fife",
    "Scottish Borders",
    "East Lothian",
    "Highland",
    "Midlothian",
    "South Lanarkshire",
    "Dumfries & Galloway",
    "East Ayrshire",
    "South Ayrshire",
    "North Ayrshire",
    "Stirling",
    "West Dunbartonshire",
    "Argyll & Bute",
    "Aberdeenshire",
    "Moray",
    "Dundee City",
    "Inverclyde",
    "East Dunbartonshire",
    "Clackmannanshire",
    "Aberdeen City",
    "Angus",
    "Comhairle nan Eilean Siar",
    "Orkney Islands",
    "Shetland Islands",
    "Stockport",
    "Barnet",
    "Central Bedfordshire",
    "Northamptonshire",
    "Leicestershire",
    "Derbyshire",
    "Rotherham",
    "Leeds",
    "Medway",
    "Hampshire",
    "Hillingdon",
    "West Berkshire",
    "Wiltshire",
    "Worcestershire",
    "Gloucestershire",
    "Devon",
    "Warwickshire",
    "East Cheshire",
    "Warrington",
    "Wigan",
    "Lancashire",
    "Cumbria",
    "Hertfordshire",
    "Doncaster",
    "Kent",
    "West Sussex",
    "Portsmouth",
    "Buckinghamshire",
    "Wirral",
    "Manchester",
    "Bury",
    "Calderdale",
    "Wakefield",
    "East Riding of Yorkshire",
    "Salford",
    "Trafford",
    "Durham",
    "Tower Hamlets",
    "Sunderland",
    "Rochdale",
    "Islington",
    "Cambridgeshire",
    "Rutland",
    "Lincolnshire",
    "North Yorkshire",
    "Gateshead",
    "Northumberland",
    "Southwark",
    "Lewisham",
    "Greenwich",
    "Bexley",
    "Lambeth",
    "Wandsworth",
    "Westminster",
    "Kensington and Chelsea",
    "Hounslow",
    "Slough",
    "Wokingham",
    "Reading",
    "Bath and North East Somerset",
    "Shropshire",
    "Staffordshire",
    "Brent",
    "Derby",
    "Luton",
    "Enfield",
    "Hackney",
    "Essex",
    "Redbridge",
    "Waltham Forest",
    "Suffolk",
    "Southend-on-Sea",
    "Thurrock",
    "Peterborough",
    "North Lincolnshire",
    "North East Lincolnshire",
    "Hartlepool",
    "East Sussex",
    "Croydon",
    "Surrey",
    "Brighton and Hove",
    "Southampton",
    "Dorset",
    "Cornwall excluding Isles of Scilly",
    "Stoke-on-Trent",
    "Birmingham",
    "Oxfordshire",
    "South Gloucestershire",
    "Bristol, City of",
    "Camden",
    "Hammersmith and Fulham",
    "Ealing",
    "West Cheshire",
    "Wolverhampton",
    "Walsall",
    "Solihull",
    "Coventry",
    "Leicester",
    "Norfolk",
    "Herefordshire, County of",
    "Halton",
    "Knowsley",
    "St. Helens",
    "Sheffield",
    "Bolton",
    "Liverpool",
    "Oldham",
    "Stockton-on-Tees",
    "Darlington",
    "Haringey",
    "South Tyneside",
    "Newham",
    "Barking and Dagenham",
    "Kingston upon Hull, City of",
    "Middlesbrough",
    "Redcar and Cleveland",
    "Newcastle upon Tyne",
    "North Tyneside",
    "City of London",
    "Richmond upon Thames",
    "Bromley",
    "Sutton",
    "Kingston upon Thames",
    "Windsor and Maidenhead",
    "Bracknell Forest",
    "Swindon",
    "Bournemouth",
    "North Somerset",
    "Torbay",
    "Harrow",
    "Bedford",
    "Telford and Wrekin",
    "Nottingham",
    "Dudley",
    "Sandwell",
    "Milton Keynes",
    "Sefton",
    "Blackpool",
    "Barnsley",
    "Bradford",
    "Tameside",
    "Kirklees",
    "Blackburn with Darwen",
    "Isle of Wight",
    "Poole",
    "Havering",
    "York",
    "Plymouth",
    "Merton",
    "Caerphilly",
    "East Renfrewshire",
    "Cheshire",
    "Bedfordshire",
    "Bournemouth, Christchurch and Poole",
    "Dorset",
    "North Northamptonshire",
    "West Northamptonshire",
    "Cumberland",
    "Westmorland and Furness"
  )


# data for all sessions

con <- DBI::dbConnect(
  drv = RPostgres::Postgres(),
  dbname = Sys.getenv('SFTZ_PG_DB'),
  host = Sys.getenv('SFTZ_PG_HOST'),
  port = Sys.getenv('SFTZ_PG_PORT'),
  user = Sys.getenv('GBTRAFFIC_PG_USER'),
  password = Sys.getenv('GBTRAFFIC_PG_PASSWORD'),
  # ensure don't get integer64 returned as R doesn't support (generally)
  bigint = "integer"
)

## Load common data for all sessions ----

# get countpoint data from database

countpoints <-
  sf::st_transform(sf::st_read(con,
                               layer = RPostgres::Id(schema = "traffic_gb", table = "countpoints")), 4326)


# get data for aadf edges
aadf_edges <-
  sf::st_transform(sf::st_read(con, layer = RPostgres::Id(schema = "traffic_gb", table = "mv_aadf_roads")), 4326)


# Ensure la_traffic is ordered by year ascending - very important for calculating annual % change
la_traffic <-
  DBI::dbGetQuery(con,
                  paste0("SELECT * FROM traffic_gb.mv_la_traffic order by year asc"))


DBI::dbDisconnect(con)
