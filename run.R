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
    
    g <- ggplot(data=df, aes(x=race, y=value, fill=variable)) +
        geom_bar(size=.3, colour="black", stat="identity", position=position_dodge(), ) + coord_flip() +
        theme(legend.position="bottom", legend.title=element_blank(), plot.margin=unit(c(2,2,2,2), "cm")) +
        ggtitle(outcome) + ylab("") + xlab("") +
        scale_y_continuous(limits=c(0,1), breaks=seq(0, 1, 0.1)) +
        geom_text(aes(label=value), position=position_dodge(width=0.9), hjust=1.5);
        
    print(g);
}


generate_rank_graph <- function(site_id) {
    
    g <- ggplot(data=rank_data, aes(reorder(x=factor(Hospital_ID), DFC_PERCENT), y=DFC_PERCENT)) +
        geom_bar(stat="identity", aes(fill = Hospital_ID == site_id )) + coord_flip() +
        theme(plot.margin=unit(c(1,1,1,1), "cm"), legend.position="none", axis.text.y = element_blank(), axis.ticks.y=element_blank()) +
        scale_y_continuous(limits=c(0,1), breaks=seq(0, 1, 0.1)) +
        scale_fill_manual(values = c("grey", "red")) + 
        geom_text(aes(label=DFC_PERCENT), size=2, position=position_dodge(width=0.9), hjust=-.3) +
        ggtitle("Defect Free Care Performance") + ylab("") + xlab("");
    
    print(g);
}


df_restructure <- function(df) {
    df <- melt(df, id=c("race", "Hospital_ID", "outcome", "Region"));
    return(df);
}


generate_charts <- function() {
    
    site_id_vector <- unique(graph_data$Hospital_ID);

    for (site_id in site_id_vector) {
        
        filename <- paste("reports/", site_id, ".pdf", sep="");
        pdf(file = filename);
        print(paste("Creating pdf for site ", site_id));
        generate_rank_graph(site_id);
        
        df1 <- filter_site_id(site_id, graph_data);
        outcomes_vector <- unique(df1$outcome);
        
        for (outcome in outcomes_vector){
            df2 <- filter_outcome(outcome, df1);
            create_plot(outcome, df2);
        }
        
        dev.off();
    }
}



# Run

graph_data <- load_data('data.csv');
rank_data <- load_data('rankings.csv');

generate_charts();        

