#' @title Logger
#' @description Extract, transform and graph Solinst water table data
#' @param site Site Name
#' @param wtd_path Path to .xle Solinst water table data folder
#' @param aws_path Path to automatic weather station folder
#' @param references Path to .csv of water table depth metadata
#' @return Plots and csv data saved to SolR folder within the water table data folder
#' @import ecoflux
#' @import ggpubr
#' @import lubridate
#' @import sf
#' @import BrailleR
#' @importFrom crayon blue
#' @import dplyr
#' @importFrom magrittr "%>%"
#' @import readr
#' @import tidyr
#' @import ggplot2
#' @import XML
#' @export logger

logger <- function(site, wtd_path, aws_path, references){

  suppressPackageStartupMessages(suppressWarnings(require(ecoflux, quietly = T)))
  suppressPackageStartupMessages(suppressWarnings(require(ggpubr, quietly = T)))
  suppressPackageStartupMessages(suppressWarnings(require(lubridate, quietly = T)))
  suppressPackageStartupMessages(suppressWarnings(require(sf, quietly = T)))
  suppressPackageStartupMessages(suppressWarnings(require(BrailleR, quietly = T)))
  suppressPackageStartupMessages(suppressWarnings(require(crayon, quietly = T)))
  suppressPackageStartupMessages(suppressWarnings(require(readr, quietly = T)))
  suppressPackageStartupMessages(suppressWarnings(require(dplyr, quietly = T)))
  suppressPackageStartupMessages(suppressWarnings(require(magrittr, quietly = T)))
  suppressPackageStartupMessages(suppressWarnings(require(tidyr, quietly = T)))
  suppressPackageStartupMessages(suppressWarnings(require(ggplot2, quietly = T)))
  suppressPackageStartupMessages(suppressWarnings(require(XML, quietly = T)))

  suppressMessages(GoSighted())

  cat(blue("\n Extracting Water Table Data \n"))

  sv <- paste0(wtd_path, "/SolR")

  if(!dir.exists(sv)){
    dir.create(sv)
  }

  csv_sv <- paste0(wtd_path, "/SolR/Data")
  if(!dir.exists(csv_sv)){
    dir.create(csv_sv)
  }

#wtd_data----
ls <- list.files(wtd_path,
                 pattern = "xle", full.names = T)


wtd <- data.frame()
for(i in ls){
  BrailleR::FindReplace(i, 'ISO_8859_1', 'ISO-8859-1')
  a <- read_xle(i)
  wtd <- rbind(wtd, a)
}

#aws_data----

cat(blue("\n Extracting AWS Data \n"))
awsls <- list.files(aws_path,
                    full.names = T, pattern = ".csv")

aws <- data.frame()
for(i in awsls){
  b <- suppressMessages(read.csv(i, nrows = 2))
  nm1 <- names(b)
  nm1 <- gsub(".*X.*", "", nm1)
  nm2 <- as.character(b[1,])
  nm <- paste(nm1, nm2)
  nm <- gsub(" ", "", nm)
  b <- suppressMessages(read_csv(i, skip = 2, col_names = F))
  names(b) <- nm
  aws <- bind_rows(aws, b)
}




aws$Kpa <- aws$Bar * 0.1

aws$mh2o <- aws$Kpa * 0.101972

#wt----
cat(blue("Calculating Water Table Levels"))
ref <- suppressMessages(read_csv(references)) %>%
  dplyr::rename(sample = `Dipwell ID`) %>%
  mutate(ref = Level - `Ground Level`)

names(wtd) <- c("site", "sample", "coordinates", "date", "time",
                "level", "temp", "level_off")
wtd <- wtd %>%
  unite("datetime", date:time, sep = " ", remove = F)

wtd$date <- ymd(wtd$date)
wtd$time <- hms(wtd$time)

wtd$datetime <- ymd_hms(wtd$datetime)

aws <- aws %>%
  unite("datetime", Date:Time, sep = " ", remove = F)

aws$datetime <- dmy_hms(aws$datetime)

#combine----
wtf <- data.frame()
for(i in unique(wtd$sample)){

  wt1 <- wtd %>%
    filter(datetime <= max(aws$datetime)) %>%
    filter(datetime >= min(aws$datetime))

  wt2 <- wt1 %>%
    group_by(datetime) %>%
    summarise(level = mean(level))

  aws1 <- aws %>%
    filter(datetime >= min(wt1$datetime) & datetime <= max(wt1$datetime)) %>%
    arrange(datetime)

  aws1$datetime <- round_date(aws1$datetime, "hour")

  aws2 <- aws1 %>%
    group_by(datetime) %>%
    summarise(mh2o_mean = mean(mh2o),
              rainfall = sum(Rain))

  wt2<- suppressMessages(left_join(wt2, aws2, by = "datetime"))

  wt2$sample <- i
  wtf <- rbind(wtf, wt2)
}

wtf <- left_join(wtf, ref, by = "sample") %>%
  mutate(level.adj = level - mh2o_mean,
         ref = 1.92 - ref,
         level.adj1 = level.adj - ref,
         level = level.adj1) %>%
  select(-c(level.adj, ref, level.adj1, `Ground Level`, Level, mh2o_mean))


write_csv(wtf, paste0(csv_sv,"/", site, "_WTD.csv"))

#graph----


wtf <- wtf %>%
  separate(datetime, c("date", "time"), sep = " ")

wtf$date <- ymd(wtf$date)

cat(blue("\n Creating Graphs \n"))
plt_sv <- paste0(sv,"/Plots/")

all_line <- ggplot(wtf)+
  geom_line(aes(date, level))+
  scale_x_date(date_breaks = "month")+
  xlab(NULL)+
  ylab("M from Surface")+
  rotate_x_text(45)+
  ggtitle("A")+
  labs(caption = "A: Combined line plot of all dipwells.")

suppressMessages(ggsave(paste0(plt_sv, "/Combined/Combined_line_plot.png"), all_line, width = 4800 , height = 1500,
       units = "px", dpi = 200))

t1_box <- ggplot(wtf)+
  geom_boxplot(aes(sample, level))+
  xlab(NULL)+
  ylab(NULL)+
  rotate_x_text(45)+
  ggtitle("B")+
  labs(caption = "B: Boxplots for each sample point.")

suppressWarnings(ggsave(paste0(plt_sv, "/Combined/Box_plots.png"), t1_box, width = 4800 , height = 1500,
       units = "px", dpi = 200))


t1_line <- ggplot(wtf)+
  geom_line(aes(date, level))+
  facet_wrap(facets =  "sample", ncol = 3)+
  scale_x_date(date_breaks = "month")+
  scale_y_continuous(breaks =seq(from = round(min(wtf$level, na.rm = T),2),
                                 to = max(wtf$level, na.rm = T), by =0.2))+
  xlab(NULL)+
  ylab("M from Surface")+
  rotate_x_text(45)+
  ggtitle("C")+
  labs(caption = "C: Line plots for each sample point.")

suppressWarnings(ggsave(paste0(plt_sv, "/Combined/Facated_Line_plots.png"),
                        t1_line, width = 4800 , height = 3500,
       units = "px", dpi = 400))


top_plots <- suppressWarnings(ggarrange(all_line, t1_box,  nrow = 1, ncol = 2))

all_plots <- suppressWarnings(ggarrange(top_plots, t1_line, ncol = 1, nrow = 2,
                                        heights = c(1,1.5)))

suppressMessages(ggsave(paste0(plt_sv, "Overview.png"), all_plots, width = 5400 , height = 4200,
       units = "px", dpi = 400))

cat(red(paste0("\n Analysis complete files saved to: ", wtd_path, "/SolR")))

}




