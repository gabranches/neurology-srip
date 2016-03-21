setwd("/Users/gabranches/GoogleDrive/work/R/srip")

library(ggplot2)
library(reshape2)


load_data <- function(filename) {
	df <- read.csv(paste("resources/", filename, sep=""),
               header = TRUE,
               quote="\"",
            #   stringsAsFactors= TRUE,
               strip.white = TRUE);
    return(df)
}


filter_site_id <- function(site_id, data) {
	df <- data[data$Hospital_ID == site_id, ];
	return(df)
}


filter_outcome <- function(outcome_id, data) {
	df <- data[data$outcome == outcome_id, ];
	return(df)
}


create_plot <- function(outcome, df) {
	df <- df_restructure(df);
    g <- ggplot(data=df, aes(x=race, y=value, fill=variable)) + geom_bar(stat="identity", position=position_dodge(), ) + coord_flip();
    g <- g + theme(legend.position="bottom", legend.title=element_blank(), plot.margin=unit(c(2,2,2,2), "cm")) + ggtitle(outcome) + ylab("") + xlab("") + scale_y_continuous(breaks=seq(0, 1, 0.1));
    g <- g + geom_text(aes(label=value), position=position_dodge(width=0.9), hjust=1.5);
    print(g);

}

df_restructure <- function(df) {
    df <- melt(df, id=c("race", "Hospital_ID", "outcome", "Region"));
    return(df);
}

full_data <- load_data('data.csv');
site_id_vector <- unique(full_data$Hospital_ID);


for (site_id in site_id_vector) {
    
    print(paste("Creating pdf for site ", site_id));
    
    filename <- paste("reports/", site_id, ".pdf", sep="");
    pdf(file = filename);
    
    df1 <- filter_site_id(site_id, full_data);
    
    outcomes_vector <- unique(df1$outcome);
    
    for (outcome in outcomes_vector){
        df2 <- filter_outcome(outcome, df1);
        create_plot(outcome, df2);
    }
    
    dev.off();
    
}
