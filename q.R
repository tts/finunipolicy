# 1. Harvest university codes from Virta
# 2. By university, query their publications via Virta REST API
# 3. Transform XML to CSV
# 4. Clean DOI's
# 5. With DOI, query Altmetric API
# 6. Prepare data for a Shiny web app

library(rvest)
library(httr)
library(dplyr)

####################
#
# Harvest code list
#
####################
url <- "https://confluence.csc.fi/display/VIR/Koodistot"

codepage <- read_html(url)

code <- codepage %>% 
  html_nodes(xpath = "//table[../preceding-sibling::h3/@id='Koodistot-Organisaatiokoodisto']//td[1]") %>% 
  html_text()
uni <- codepage %>% 
  html_nodes(xpath = "//table[../preceding-sibling::h3/@id='Koodistot-Organisaatiokoodisto']//td[2]") %>% 
  html_text()

code_uni <- data.frame(cbind(code, uni), stringsAsFactors = FALSE)

# Only universities
code_uni <- code_uni %>% 
  filter(grepl("yliopisto$|Åbo|handelshögskolan", uni)) 

###########################
#
# Query Virta REST API
#
###########################
set_config( config( ssl_verifypeer = 0L ) )

url <- "https://virta-jtp.csc.fi/api/julkaisut/haku/xml?&"

query_uni_data <- function(code, uni){
  org <- paste0("organisaatioTunnus=", code)
  query <- paste0(url, org)
  con <- GET(query)
  writeBin(content(con, "raw"), paste0("data/", gsub("\\s|\\.", "", uni), ".zip"))
}

for (i in 1:nrow(code_uni)) {
  query_uni_data(code_uni$code[i], code_uni$uni[i])
}

############################################
#
# Transform XML to CSV with XSLT and Saxon
# 
# ./runxslt.sh
############################################

#######################################
#
# Read data in, select only year 2015,
# and clean DOIs
# 
#######################################
filenames <- list.files("data", pattern="[A-Z].*.csv", full.names=TRUE)

# http://stackoverflow.com/a/5186790
read_csv_filename <- function(filename){
  ret <- read.csv2(filename, header=FALSE, stringsAsFactors = FALSE)
  ret$Uni <- filename 
  ret
}

data <- plyr::ldply(filenames, read_csv_filename)

names(data) <- c("DOI", "year", "Title", "Journal", "Authors", "OA", "Org", "SubjectCount", "Subjects", "University")

data <- data %>% 
  filter(year == "2015") %>% 
  filter(!DOI %in% c("eisaatavilla", "Eitiedossa")) %>% 
  filter(DOI != "")

# test <- plyr::llply(paste0('doi:',"10.1093/acprof:oso/9780199560677.003.0006"), altmetrics, .progress = 'text')
# Fails, although ought to be a correct DOI of a book. Anyway, ":" needs to be removed

data$DOI <- gsub("https?://(dx\\.)?doi.org/", "", data$DOI)
data$DOI <- gsub("http://doi.ieeecomputersociety.org/", "", data$DOI)
data$DOI <- gsub("http://|https://", "", data$DOI)
data$DOI <- gsub("[aA]vailable.*", "", data$DOI)
data$DOI <- gsub("[sS]aatavana.*", "", data$DOI)
data$DOI <- gsub("[vV]ainkäytt.*", "", data$DOI)
data$DOI <- gsub("[pP]ubli.*", "", data$DOI)
data$DOI <- gsub("[aA]rticle.*", "", data$DOI)
data$DOI <- gsub("[fF]irst.*", "", data$DOI)
data$DOI <- gsub("[aA]alto.*", "", data$DOI)
data$DOI <- gsub("[tT]hisart.*", "", data$DOI)
data$DOI <- gsub("DOI|DOI:|doi:", "", data$DOI)
data$DOI <- gsub("doi.acm.org","", data$DOI)
data$DOI <- gsub("(.*)(DOI:.*)", "\\1", data$DOI)
data$DOI <- gsub("%", "/", data$DOI)
data$DOI <- gsub(" ", "", data$DOI)
data$DOI <- gsub("^/", "", data$DOI)
data$DOI <- gsub("\\s+$", "", data$DOI)
data$DOI <- gsub("^\\s+", "", data$DOI)
data$DOI <- gsub(":", "", data$DOI)

# Checked these manually
data$DOI[data$DOI =="doi.org.libproxy."] <- "10.1111/cgf.12722"
data$DOI[data$DOI == "dx.doi.org.libproxy."] <- "10.1111/psyp.12521"

#######################
#
# Query Altmetric API
# 
#######################
library(rAltmetric)

# Repetitive code but wanted to keep track by university
# for possible problems etc

data_aalto <- data %>% 
  filter(University == "data/Aaltoyliopisto.csv")
data_hki <- data %>% 
  filter(University == "data/Helsinginyliopisto.csv")
data_turku <- data %>% 
  filter(University == "data/Turunyliopisto.csv")
data_tre <- data %>% 
  filter(University == "data/Tampereenyliopisto.csv")
data_tut <- data %>% 
  filter(University == "data/Tampereenteknyliopisto.csv")
data_jla <- data %>% 
  filter(University == "data/Jyvaskylanyliopisto.csv")
data_ita <- data %>% 
  filter(University == "data/ItaSuomenyliopisto.csv")
data_lappi <- data %>% 
  filter(University == "data/Lapinyliopisto.csv")
data_lut <- data %>% 
  filter(University == "data/Lappeenrannanteknyliopisto.csv")
data_aa <- data %>% 
  filter(University == "data/AboAkademi.csv")
data_taide <- data %>% 
  filter(University == "data/Taideyliopisto.csv")
data_oulu <- data %>% 
  filter(University == "data/Oulunyliopisto.csv")
data_vaasa <- data %>% 
  filter(University == "data/Vaasanyliopisto.csv")
data_hanken <- data %>% 
  filter(University == "data/Svenskahandelshogskolan.csv")

raw_metrics_hki <- plyr::llply(paste0('doi:',data_hki$DOI), altmetrics, .progress = 'text')
metric_data_hki <- plyr::ldply(raw_metrics_hki, altmetric_data)
write.csv(metric_data_hki, file = paste0("data/metric_data_hki_", Sys.Date(), ".csv"), row.names = FALSE)
rm(raw_metrics_hki, metric_data_hki)
gc()

raw_metrics_tre <- plyr::llply(paste0('doi:',data_tre$DOI), altmetrics, .progress = 'text')
metric_data_tre <- plyr::ldply(raw_metrics_tre, altmetric_data)
write.csv(metric_data_tre, file = paste0("data/metric_data_tre_", Sys.Date(), ".csv"), row.names = FALSE)
rm(raw_metrics_tre, metric_data_tre)
gc()

raw_metrics_tut <- plyr::llply(paste0('doi:',data_tut$DOI), altmetrics, .progress = 'text')
metric_data_tut <- plyr::ldply(raw_metrics_tut, altmetric_data)
write.csv(metric_data_tut, file = paste0("data/metric_data_tut_", Sys.Date(), ".csv"), row.names = FALSE)
rm(raw_metrics_tut, metric_data_tut)
gc()

raw_metrics_jla <- plyr::llply(paste0('doi:',data_jla$DOI), altmetrics, .progress = 'text')
metric_data_jla <- plyr::ldply(raw_metrics_jla, altmetric_data)
write.csv(metric_data_jla, file = paste0("data/metric_data_jla_", Sys.Date(), ".csv"), row.names = FALSE)
rm(raw_metrics_jla, metric_data_jla)
gc()

raw_metrics_ita <- plyr::llply(paste0('doi:',data_ita$DOI), altmetrics, .progress = 'text')
metric_data_ita <- plyr::ldply(raw_metrics_ita, altmetric_data)
write.csv(metric_data_ita, file = paste0("data/metric_data_ita_", Sys.Date(), ".csv"), row.names = FALSE)
rm(raw_metrics_ita, metric_data_ita)
gc()

raw_metrics_lappi <- plyr::llply(paste0('doi:',data_lappi$DOI), altmetrics, .progress = 'text')
metric_data_lappi <- plyr::ldply(raw_metrics_lappi, altmetric_data)
write.csv(metric_data_lappi, file = paste0("data/metric_data_lappi_", Sys.Date(), ".csv"), row.names = FALSE)
rm(raw_metrics_lappi, metric_data_lappi)
gc()

raw_metrics_lut <- plyr::llply(paste0('doi:',data_lut$DOI), altmetrics, .progress = 'text')
metric_data_lut <- plyr::ldply(raw_metrics_lut, altmetric_data)
write.csv(metric_data_lut, file = paste0("data/metric_data_lut_", Sys.Date(), ".csv"), row.names = FALSE)
rm(raw_metrics_lut, metric_data_lut)
gc()

raw_metrics_aa <- plyr::llply(paste0('doi:',data_aa$DOI), altmetrics, .progress = 'text')
metric_data_aa <- plyr::ldply(raw_metrics_aa, altmetric_data)
write.csv(metric_data_aa, file = paste0("data/metric_data_aa_", Sys.Date(), ".csv"), row.names = FALSE)
rm(raw_metrics_aa, metric_data_aa)
gc()

raw_metrics_taide <- plyr::llply(paste0('doi:',data_taide$DOI), altmetrics, .progress = 'text')
metric_data_taide <- plyr::ldply(raw_metrics_taide, altmetric_data)
write.csv(metric_data_taide, file = paste0("data/metric_data_taide_", Sys.Date(), ".csv"), row.names = FALSE)
rm(raw_metrics_taide, metric_data_taide)
gc()

raw_metrics_oulu <- plyr::llply(paste0('doi:',data_oulu$DOI), altmetrics, .progress = 'text')
metric_data_oulu <- plyr::ldply(raw_metrics_oulu, altmetric_data)
write.csv(metric_data_oulu, file = paste0("data/metric_data_oulu_", Sys.Date(), ".csv"), row.names = FALSE)
rm(raw_metrics_oulu, metric_data_oulu)
gc()

raw_metrics_vaasa <- plyr::llply(paste0('doi:',data_vaasa$DOI), altmetrics, .progress = 'text')
metric_data_vaasa <- plyr::ldply(raw_metrics_vaasa, altmetric_data)
write.csv(metric_data_vaasa, file = paste0("data/metric_data_vaasa_", Sys.Date(), ".csv"), row.names = FALSE)
rm(raw_metrics_vaasa, metric_data_vaasa)
gc()

raw_metrics_hanken <- plyr::llply(paste0('doi:',data_hanken$DOI), altmetrics, .progress = 'text')
metric_data_hanken <- plyr::ldply(raw_metrics_hanken, altmetric_data)
write.csv(metric_data_hanken, file = paste0("data/metric_data_hanken_", Sys.Date(), ".csv"), row.names = FALSE)
rm(raw_metrics_hanken, metric_data_hanken)
gc()

raw_metrics_aalto <- plyr::llply(paste0('doi:',data_aalto$DOI), altmetrics, .progress = 'text')
metric_data_aalto <- plyr::ldply(raw_metrics_aalto, altmetric_data)
write.csv(metric_data_aalto, file = paste0("data/metric_data_aalto_", Sys.Date(), ".csv"), row.names = FALSE)
rm(raw_metrics_aalto, metric_data_aalto)
gc()


#########################
#
# Combine data and prepare
# for Shiny 
# 
########################

filenames <- list.files("data", pattern="metric_data_.*.csv", full.names=TRUE)

read_csv_filename <- function(filename){
  if (filename != "data/metric_data_taide_2016-12-23.csv") { # empty so left out
    ret <- read.csv(filename, stringsAsFactors = FALSE)
    ret$Uni <- filename 
    ret
  }
}

metrics_data <- plyr::ldply(filenames, read_csv_filename)

data_subset <- metrics_data[ , c("doi",
                                 "Uni",
                                 "details_url",
                                 "title",
                                 "score",
                                 "mendeley",
                                 "citeulike",
                                 "readers_count",
                                 "cited_by_gplus_count",
                                 "cited_by_fbwalls_count",
                                 "cited_by_posts_count",
                                 "cited_by_tweeters_count",
                                 "cited_by_accounts_count",
                                 "cited_by_feeds_count",
                                 "cited_by_videos_count",
                                # "cited_by_delicious_count", # all NA
                                 "cited_by_rdts_count",
                                # "cited_by_forum_count", # all NA
                                #"cited_by_qs_count",     # all NA
                                 "cited_by_rh_count",
                                 "cited_by_msm_count",
                                 "cited_by_wikipedia_count.1",
                                 "cited_by_weibo_count",
                                 "cited_by_policies_count")] 

# Join with original data 
data_joined_with_metrics <- inner_join(data, data_subset, by = c("DOI"="doi"))

data <- data_joined_with_metrics %>% 
  mutate(University = gsub("data/|\\.csv", "", University)) %>% 
  mutate(University = gsub("Aaltoyliopisto", "Aalto University", University)) %>%
  mutate(University = gsub("AboAkademi", "Åbo Akademi", University)) %>%
  mutate(University = gsub("Helsinginyliopisto", "University of Helsinki", University)) %>%
  mutate(University = gsub("ItaSuomenyliopisto", "University of Eastern Finland", University)) %>%
  mutate(University = gsub("Jyvaskylanyliopisto", "University of Jyväskylä", University)) %>%
  mutate(University = gsub("Lapinyliopisto", "University of Lapland", University)) %>%
  mutate(University = gsub("Lappeenrannanteknyliopisto", "Lappeenranta University of Technology", University)) %>% 
  mutate(University = gsub("Oulunyliopisto", "University of Oulu", University)) %>% 
  mutate(University = gsub("Svenskahandelshogskolan", "Hanken School of Economics", University)) %>% 
  mutate(University = gsub("Tampereenteknyliopisto", "Tampere University of Technology", University)) %>% 
  mutate(University = gsub("Tampereenyliopisto", "University of Tampere", University)) %>% 
  mutate(University = gsub("Turunyliopisto", "University of Turku", University)) %>% 
  mutate(University = gsub("Vaasanyliopisto", "University of Vaasa", University)) %>% 
  mutate(Title = gsub("'","", Title)) %>% 
  mutate(href = details_url) %>% 
  mutate(Link = paste0('<a href="', href, '" target="_blank"', '">', substr(href, 47, nchar(href)), '/a>')) %>% 
  mutate(Score = as.integer(ceiling(score))) %>%
  mutate(Mendeley = mendeley) %>% 
  mutate(CiteULike = citeulike) %>% 
  mutate(Readers = readers_count) %>% 
  mutate(GPlus = cited_by_gplus_count) %>% 
  mutate(Facebook = cited_by_fbwalls_count) %>% 
  mutate(Posts = cited_by_posts_count) %>% 
  mutate(Twitter = cited_by_tweeters_count) %>% 
  mutate(Accounts = cited_by_accounts_count) %>% 
  mutate(Blogs = cited_by_feeds_count) %>% 
  mutate(YouTube = cited_by_videos_count) %>% 
  mutate(Reddit = cited_by_rdts_count) %>% 
  mutate(ResHighlight = cited_by_rh_count) %>% 
  mutate(News = cited_by_msm_count) %>% 
  mutate(Wikipedia = cited_by_wikipedia_count.1) %>% 
  mutate(Weibo = cited_by_weibo_count) %>% 
  mutate(Policy = cited_by_policies_count) %>% 
  mutate(OpenAccess = ifelse(data$OA == 0, "Not OA journal",  # for textual output in data table
                             ifelse(data$OA == 1, "OA journal",
                                    ifelse(data$OA == 2, "OA (repository)", "OA status not known")))) %>% 
  mutate(OA = ifelse(OA == 1, 12, # for scaling the circle size in plot
                     ifelse(OA == 2, 8,
                            ifelse(OA == 0, 2, 1)))) %>% 
  select(DOI, University, Link, href, Title, Journal, Authors, OA, OpenAccess, SubjectCount,  
         Score, Mendeley, Policy, Readers, GPlus, Facebook,
         Posts, Twitter, Accounts, Blogs, YouTube, Reddit, ResearchForums,
         NewsOutlets, Wikipedia, Weibo, CiteULike)

# Removing duplicates in DOI+University combinations
data <- data[!duplicated(data[1:2]),]

# Take all the rest except DOI, links first
data <- data[ ,c(3:4,2,5:ncol(data))]

write.csv2(metrics, "data/data_2015.csv", row.names = FALSE, fileEncoding = "UTF-8")

universities <- unique(data$University)
write.csv2(universities, "data/universities.csv", row.names = FALSE, fileEncoding = "UTF-8")
