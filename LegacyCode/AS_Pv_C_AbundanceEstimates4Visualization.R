# Coastal Pv Surveys: Create spatial dataset of abundance estimates, share to DB and create shapefile for AGOL
# S. Hardy, 15MAR2019

# Create functions -----------------------------------------------
# Function to install packages needed
install_pkg <- function(x)
{
  if (!require(x,character.only = TRUE))
  {
    install.packages(x,dep=TRUE)
    if(!require(x,character.only = TRUE)) stop("Package not found")
  }
}

process_abund <- function(df) {
  df <- data.frame(t(sapply(df,c)))
  df <- data.frame(value = unlist(data.frame(t(sapply(df$ABU,c)))))
  df$polyid <- sapply(strsplit(row.names(df), "\\."), "[", 2)
  df$variable <- sapply(strsplit(row.names(df), "\\."), "[", 1)
  df <- reshape2::dcast(df, polyid~variable)
  df$polyid <- substr(df$polyid, 1, 4)
  df <- df[, c(1, 5, 4, 2, 3)]
  df <- df[order(df$polyid, df$year), ]
  return(df)
}

process_trend <- function(df) {
  df <- data.frame(t(sapply(df,c)))
  df <- data.frame(value = unlist(data.frame(t(sapply(df$Lin_Trend,c)))))
  df$polyid <- sapply(strsplit(row.names(df), "\\."), "[", 2)
  df$variable <- sapply(strsplit(row.names(df), "\\."), "[", 1)
  df <- reshape2::dcast(df, polyid~variable)
  df$polyid <- substr(df$polyid, 1, 4)
  df <- df[, c(1, 5, 4, 2, 3)]
  df <- df[order(df$polyid, df$year), ]
  return(df)
}

# Install libraries ----------------------------------------------
library("reshape2")
library("RPostgreSQL")
library("sf")

# Run code -------------------------------------------------------
# Set working drive
wd <- "//AKC0SS-N086/NMML_Users/Stacie.Hardy/Work/Projects/AS_HarborSeal_Coastal/Data/AbundanceEstimates/FromJay_201902/"
setwd(wd)

# Load data -----------------------------------------------------------------------------
load("ABUbySITE.RData")

# Process abundance data --------------------------------------------------------------------
abund_glac <- process_abund(Site_Abu_Trend_glac)
abund_stock01 <- process_abund(Site_Abu_Trend_stock1)
abund_stock03 <- process_abund(Site_Abu_Trend_stock3)
abund_stock04 <- process_abund(Site_Abu_Trend_stock4)
abund_stock05 <- process_abund(Site_Abu_Trend_stock5)
abund_stock06 <- process_abund(Site_Abu_Trend_stock6)
abund_stock07 <- process_abund(Site_Abu_Trend_stock7)
abund_stock08 <- process_abund(Site_Abu_Trend_stock8)
abund_stock09 <- process_abund(Site_Abu_Trend_stock9)
abund_stock10 <- process_abund(Site_Abu_Trend_stock10)
abund_stock11 <- process_abund(Site_Abu_Trend_stock11)
abund_stock12 <- process_abund(Site_Abu_Trend_stock12)

abund <- rbind(abund_glac, abund_stock01)
abund <- rbind(abund, abund_stock03)
abund <- rbind(abund, abund_stock04)
abund <- rbind(abund, abund_stock05)
abund <- rbind(abund, abund_stock06)
abund <- rbind(abund, abund_stock07)
abund <- rbind(abund, abund_stock08)
abund <- rbind(abund, abund_stock09)
abund <- rbind(abund, abund_stock10)
abund <- rbind(abund, abund_stock11)
abund <- rbind(abund, abund_stock12)
colnames(abund) <- c("polyid", "year", "abund_est", "abund_b95", "abund_t95")

rm(abund_glac, abund_stock01, abund_stock03, abund_stock04, abund_stock05, abund_stock06, abund_stock07, abund_stock08, abund_stock09, 
   abund_stock10, abund_stock11, abund_stock12)

# Process trend data --------------------------------------------------------------------
trend_glac <- process_trend(Site_Abu_Trend_glac)
trend_stock01 <- process_trend(Site_Abu_Trend_stock1)
trend_stock03 <- process_trend(Site_Abu_Trend_stock3)
trend_stock04 <- process_trend(Site_Abu_Trend_stock4)
trend_stock05 <- process_trend(Site_Abu_Trend_stock5)
trend_stock06 <- process_trend(Site_Abu_Trend_stock6)
trend_stock07 <- process_trend(Site_Abu_Trend_stock7)
trend_stock08 <- process_trend(Site_Abu_Trend_stock8)
trend_stock09 <- process_trend(Site_Abu_Trend_stock9)
trend_stock10 <- process_trend(Site_Abu_Trend_stock10)
trend_stock11 <- process_trend(Site_Abu_Trend_stock11)
trend_stock12 <- process_trend(Site_Abu_Trend_stock12)

trend <- rbind(trend_glac, trend_stock01)
trend <- rbind(trend, trend_stock03)
trend <- rbind(trend, trend_stock04)
trend <- rbind(trend, trend_stock05)
trend <- rbind(trend, trend_stock06)
trend <- rbind(trend, trend_stock07)
trend <- rbind(trend, trend_stock08)
trend <- rbind(trend, trend_stock09)
trend <- rbind(trend, trend_stock10)
trend <- rbind(trend, trend_stock11)
trend <- rbind(trend, trend_stock12)
colnames(trend) <- c("polyid", "year", "trend_est", "trend_b95", "trend_t95")

rm(trend_glac, trend_stock01, trend_stock03, trend_stock04, trend_stock05, trend_stock06, trend_stock07, trend_stock08, trend_stock09, 
   trend_stock10, trend_stock11, trend_stock12)

rm(Site_Abu_Trend_glac, Site_Abu_Trend_stock1, Site_Abu_Trend_stock3, Site_Abu_Trend_stock4, Site_Abu_Trend_stock5, Site_Abu_Trend_stock6, 
   Site_Abu_Trend_stock7, Site_Abu_Trend_stock8, Site_Abu_Trend_stock9, Site_Abu_Trend_stock10, Site_Abu_Trend_stock11, Site_Abu_Trend_stock12)

data <- merge(abund, trend, by = c("polyid", "year"), all = TRUE)
data$year <- as.character(data$year)
rm(abund, trend)

# Connect to DB ------------------------------------------------------------------------
con <- RPostgreSQL::dbConnect(PostgreSQL(), 
                              dbname = Sys.getenv("pep_db"), 
                              host = Sys.getenv("pep_ip"), 
                              user = Sys.getenv("pep_admin"), 
                              rstudioapi::askForPassword(paste("Enter your DB password for user account: ", Sys.getenv("pep_admin"), sep = "")))



# Import abundance estimates to DB -----------------------------------------------------
RPostgreSQL::dbSendQuery(con, "DROP VIEW surv_pv_cst.geo_abundance")
RPostgreSQL::dbWriteTable(con, c("surv_pv_cst", "res_abundance_201902"), data, append = FALSE, row.names = FALSE, overwrite = TRUE)
RPostgreSQL::dbSendQuery(con, "CREATE VIEW surv_pv_cst.geo_abundance AS 
                                SELECT row_number() OVER (ORDER BY polyid, year) id, 
                                p.polyid, p.stockid, p.stockname, e.year, e.abund_est, e.abund_b95, e.abund_t95,
                                e.trend_est, e.trend_b95, e.trend_t95, num_surveys, CASE WHEN num_surveys > 0 THEN 1 ELSE 0 END as surveyed, p.geom
                                FROM surv_pv_cst.geo_polys p
                                INNER JOIN surv_pv_cst.res_abundance_201902 e
                                USING (polyid)
                                LEFT JOIN (SELECT polyid, DATE_PART('year', survey_dt)::text AS year, count(polyid) AS num_surveys
                                FROM surv_pv_cst.summ_count_by_polyid_4analysis
                                GROUP BY polyid, DATE_PART('year', survey_dt)) a
                                USING (polyid, year)")

# Export data to shapefile ------------------------------------------------------------- 
abund.sf <- sf::st_read(con, query = "SELECT polyid, stockname, year, abund_est, abund_b95, abund_t95, trend_est, trend_b95, trend_t95, surveyed, geom FROM surv_pv_cst.geo_abundance where year::integer > 2002")
sf::st_write(abund.sf, paste(wd, "abundance.shp", sep = ""), delete_layer = TRUE)

current.sf <- sf::st_read(con, query = "SELECT polyid, stockname, year, abund_est, abund_b95, abund_t95, trend_est, trend_b95, trend_t95, surveyed, geom FROM surv_pv_cst.geo_abundance where year = \'2018\'")
sf::st_write(current.sf, paste(wd, "abundance_2018.shp", sep = ""), delete_layer = TRUE)

# Disconnect from DB -------------------------------------------------------------------
RPostgreSQL::dbDisconnect(con)
