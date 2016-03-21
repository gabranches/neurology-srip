
setwd("/Users/gabranches/GoogleDrive/work/R/srip")

outcomes_vector <- c(
	"Defect Free Care",
	"rtPA Arrive by 2 hour and Treat by 3 hour",
	"rtPA Arrive by 3.5 hour and Treat by 4.5 hour",
	"Early Antithrombotics (2 days)",
    "VTE Prophylaxis",
    "Antithrombotics at discharge",
    "Anticoagulation for AF at discharge",
    "Statins at Discharge",
    "Smoking Cessation Counseling",
    "Door to CT Time Within 25 Minutes (All Strokes)",
    "Door To Needle Time Within 60 Minutes"
)


load_data <- function(filename) {
	df <- read.csv(paste("resources/", filename, sep=""),
               header = TRUE,
               quote="\"",
#               stringsAsFactors= TRUE,
               strip.white = TRUE);
    return(df)
}


filter_site_id <- function(site_id, data) {
	df <- data[data$site_os_id == site_id, ];
	return(df)
}


filter_outcome <- function(outcome_id, data) {
	df <- data[data$outcome == outcome_id, ];
	return(df)
}

create_bar_plot <- function(title, data) {
    

    if (nrow(data[data$race == "Black", ]) > 0) {
        black <- c(data[data$race == "Black", "Percent_State"], 
                   data[data$race == "Black", "Percent_Region"], 
                   data[data$race == "Black", "Percent_Hospital"]);
    } else {
        black <- c(0,0,0)
    }
    
    if (nrow(data[data$race == "White", ]) > 0) {
        white <- c(data[data$race == "White", "Percent_State"], 
                   data[data$race == "White", "Percent_Region"], 
                   data[data$race == "White", "Percent_Hospital"]);
    } else {
        white <- c(0,0,0)
    }
    
    if (nrow(data[data$race == "FL_Hispanic", ]) > 0) {
        flhp <- c(data[data$race == "FL_Hispanic", "Percent_State"], 
                  data[data$race == "FL_Hispanic", "Percent_Region"],  
                  data[data$race == "FL_Hispanic", "Percent_Hospital"]);
    } else {
        flhp <- c(0,0,0)
    }
    
    if (nrow(data[data$race == "PR_Hispanic", ]) > 0) {
        prhp <- c(data[data$race == "PR_Hispanic", "Percent_State"], 
                  data[data$race == "PR_Hispanic", "Percent_Region"],  
                  data[data$race == "PR_Hispanic", "Percent_Hospital"]);
    } else {
        prhp <- c(0,0,0)
    }
    
    
    black <- black * 100;
    white <- white * 100;
    flhp <- flhp * 100;
    prhp <- prhp * 100;

    bplt <- barplot(
        cbind(black, white, flhp, prhp),
        beside=TRUE,
        horiz=TRUE,
        xlim=c(0,100),
        legend.text=c("Percent State", "Percent Region", "Percent Hospital"),
        names.arg=c("Black", "White", "FL Hispanic", "PR Hispanic"),
        main=title,
        col=c("red", "blue", "orange"),
        xpd=TRUE
    )

    grid(ny=NA, nx=NULL)


}


full_data <- load_data('data.csv');
# site_id_vector <- unique(full_data$site_os_id);

site_id_vector = c(13710);


for (site_id in site_id_vector) {
    
    filename <- paste("reports/", site_id, ".pdf", sep="");
    pdf(file = filename);
    
    df1 <- filter_site_id(site_id, full_data);
    
    for (outcome in 1:11){
        df2 <- filter_outcome(outcome, df1);
        create_bar_plot(outcomes_vector[outcome], df2);
    }
    
    dev.off();
    
}
