setwd("/Users/gabranches/GoogleDrive/work/R/srip")

require(ggplot2)
require(reshape2)

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                    ncol = cols, nrow = ceiling(numPlots/cols))
  }

 if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


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


create_ethnicities_chart <- function(outcome, df) {
	df <- df_restructure(df, "race");
    
    g <- ggplot(data=df, aes(x=race, y=value, fill=variable)) +
        geom_bar(size=.3, colour="black", stat="identity", position=position_dodge(), ) + coord_flip() +
        theme(legend.position="bottom", legend.title=element_blank(), plot.margin=unit(c(2,2,2,2), "cm")) +
        ggtitle(outcome) + ylab("") + xlab("") +
        scale_y_continuous(labels=percent, limits=c(0,1), breaks=seq(0, 1, 0.1)) +
        geom_text(aes(label=value*100), position=position_dodge(width=0.9), hjust=1.5);
        
    print(g);
}

create_gender_chart <- function(outcome, df) {
	df <- df_restructure(df, "sex");
    
    g <- ggplot(data=df, aes(x=sex, y=value, fill=variable)) +
        geom_bar(size=.3, colour="black", stat="identity", position=position_dodge(), ) + coord_flip() +
        theme(legend.position="bottom", legend.title=element_blank(), plot.margin=unit(c(2,2,2,2), "cm")) +
        ggtitle(outcome) + ylab("") + xlab("") +
        scale_y_continuous(labels=percent, limits=c(0,1), breaks=seq(0, 1, 0.1)) +
        geom_text(aes(label=value*100), position=position_dodge(width=0.9), hjust=1.5);
        
    print(g);
}


create_rank_graph <- function(site_id) {
    
    g <- ggplot(data=rank_data, aes(reorder(x=factor(Hospital_ID), DFC_PERCENT), y=DFC_PERCENT)) +
        geom_bar(stat="identity", aes(fill = Hospital_ID == site_id )) + 
        theme(plot.margin=unit(c(.5,.5,-3,0), "cm"), legend.position="none", axis.text.x = element_blank(), axis.ticks.x=element_blank()) +
        scale_y_continuous(labels=percent, limits=c(0,1), breaks=seq(0, 1, 0.1)) +
        scale_fill_manual(values = c("lightblue", "red")) + 
        geom_text(aes(label=DFC_PERCENT*100), size=2, position=position_dodge(width=0.9), hjust=-.3, angle=90) +
        ggtitle("Defect Free Care Performance") + ylab("") + xlab("");

    summary_text(g);

}

summary_text <- function(p) {
    text <- paste("The following is text that'll appear in a plot window.\n",
         "       As you can see, it's in the plot window\n",
         "       One might imagine useful informaiton here")
    g <- ggplot() + 
        annotate("text", x = 4, y = 10, size=6, label = text) + 
        theme_bw() +
        theme(axis.line=element_blank(),axis.text.x=element_blank(),
          axis.text.y=element_blank(),axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),legend.position="none",
          panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),plot.background=element_blank());
        
    multiplot(p, g);
}


df_restructure <- function(df, type) {
    df <- melt(df, id=c(type, "Hospital_ID", "outcome", "Region"));
    return(df);
}


create_charts <- function() {
    
    site_id_vector <- unique(ethnicity_data$Hospital_ID);

    for (site_id in site_id_vector) {
        
        filename <- paste("reports/", site_id, ".pdf", sep="");
        pdf(file = filename);
        
        create_rank_graph(site_id);
        
        df1_eth <- filter_site_id(site_id, ethnicity_data);
        df1_gen <- filter_site_id(site_id, gender_data);
        
        outcomes_vector_eth <- unique(df1_eth$outcome);
        outcomes_vector_gen <- unique(df1_gen$outcome);
        
        for (outcome in outcomes_vector_eth){
            df2_eth <- filter_outcome(outcome, df1_eth);
            create_ethnicities_chart(outcome, df2_eth);
        }
        
        for (outcome in outcomes_vector_gen){
            df2_gen <- filter_outcome(outcome, df1_gen);
            create_gender_chart(outcome, df2_gen);
        }
        
        print(paste("Created pdf for site ", site_id));
        dev.off();
    }
}


# Run
print("Running...");
ethnicity_data <- load_data('ethnicity.csv');
rank_data <- load_data('dfc_ranks.csv');
gender_data <- load_data('gender.csv');

# create_charts();   

create_rank_graph(10140);
print("Done.");



