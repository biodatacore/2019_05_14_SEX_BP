
FHS_event <- readRDS("data/FHS_event.rds") %>% dplyr::select(id = PID, chd, nfchd, chddeath, chd_age, nfchd_age, chddeath_age) %>% mutate(cohort = "FHS")
ARIC_event <- readRDS("data/ARIC_event.rds") %>% dplyr::select(id = ID_C, chd, nfchd, chddeath, chd_age, nfchd_age, chddeath_age) %>% mutate(cohort = "ARIC")
CARDIA_event <- readRDS("data/CARDIA_event.rds") %>% dplyr::select(id = id, chd, nfchd, chddeath, chd_age, nfchd_age, chddeath_age)%>% mutate(cohort = "CARDIA")
MESA_event<- readRDS("data/MESA_event.rds") %>% dplyr::select(id = PID, chd, nfchd, chddeath, chd_age, nfchd_age, chddeath_age)%>% mutate(cohort = "MESA")
CHS_event<- readRDS("data/CHS_event.rds") %>% dplyr::select(id = PID, chd, nfchd, chddeath, chd_age, nfchd_age, chddeath_age)%>% mutate(cohort = "CHS")
JHS_event <- readRDS("data/JHS_event.rds") %>% dplyr::select(id = PID, chd, nfchd, chddeath, chd_age, nfchd_age, chddeath_age)%>% mutate(cohort = "JHS")

comb_event <- rbind(FHS_event, ARIC_event, CARDIA_event, MESA_event,CHS_event, JHS_event)

saveRDS(comb_event, "data/comb_event.rds")
comb_event$cohort %>% as.factor() %>% summary()


