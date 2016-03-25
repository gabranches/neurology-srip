setwd("/Users/gabranches/GoogleDrive/work/R/srip")

require(png)
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
        theme_grey() +
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
        theme_grey() +
        theme(legend.position="bottom", legend.title=element_blank(), plot.margin=unit(c(2,2,2,2), "cm")) +
        ggtitle(outcome) + ylab("") + xlab("") +
        scale_y_continuous(labels=percent, limits=c(0,1), breaks=seq(0, 1, 0.1)) +
        geom_text(aes(label=value*100), position=position_dodge(width=0.9), hjust=1.5);
        
    print(g);
}


create_rank_graph <- function(site_id) {
    
    g <- ggplot(data=dfc_rank_data, aes(reorder(x=factor(Hospital_ID), DFC_PERCENT), y=DFC_PERCENT)) +
        geom_bar(stat="identity", aes(fill = Hospital_ID == site_id )) + 
        theme_grey() +
        theme(plot.margin=unit(c(.5,.5,-2,0), "cm"), legend.position="none", axis.text.x = element_blank(), axis.ticks.x=element_blank()) +
        scale_y_continuous(labels=percent, limits=c(0,1), breaks=seq(0, 1, 0.1)) +
        scale_fill_manual(values = c("lightblue", "red")) + 
        geom_text(aes(label=DFC_PERCENT*100), size=2, position=position_dodge(width=0.9), hjust=-.3, angle=90) +
        ggtitle("Defect Free Care Performance") + ylab("") + xlab("");

    summary_text(g, site_id);

}

summary_text <- function(p, site_id) {
    
    df_stats <-  dfc_stats_data[dfc_stats_data$id == site_id, ];
    strokes_site <- sum(df_stats$Black) + 
        sum(df_stats$FL_Hispanic) + 
        sum(df_stats$PR_Hispanic) + 
        sum(df_stats$White);
    
    text <- paste("Hospital Demographics:\n",
                    "Hospital ID", site_id, "Total Ischemic Strokes:\n",
                    strokes_site, "of", total_ischemic_strokes, "Total Ischemic Strokes in the FL-PR Stroke Registry\n\n",
                    "Hospital ID", site_id, "Strokes by Race-Ethnicity:\n",
                    "Non-Hispanic Black:", df_stats$Black, "(", round(df_stats$Black/strokes_site*100, digits=0), "%)\n",
                    "Non-Hispanic White:", df_stats$White, "(", round(df_stats$White/strokes_site*100, digits=0), "%)\n",
                    "Hispanic:", df_stats$FL_Hispanic+df_stats$PR_Hispanic, "(", round(df_stats$FL_Hispanic+df_stats$PR_Hispanic/strokes_site*100, digits=0), "%)");
    
    g <- ggplot() + 
        annotate("text", x = 4, y = 8, size=5, label = text) + 
        theme_bw() +
        theme(axis.line=element_blank(),axis.text.x=element_blank(),
          axis.text.y=element_blank(),axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),legend.position="none",
          panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),plot.background=element_blank());
         
        
    multiplot(p, g);
}

front_page <- function(site_id) {
    
    
    text <- paste("FL-PR CReSD Disparities Dashboard: \n", "2014\n",
                    "Hospital ID:", site_id);
    
    g <- ggplot() + 
        annotate("text", x = 4, y = 8, size=5, label = text) + 
        theme_bw() +
        theme(axis.line=element_blank(),axis.text.x=element_blank(),
          axis.text.y=element_blank(),axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),legend.position="none",
          panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),plot.background=element_blank());
          
    print(g);
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
        
        front_page(site_id);
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
dfc_rank_data <- load_data('dfc_ranks.csv');
dfc_stats_data <- load_data('dfc_stats.csv');
gender_data <- load_data('gender.csv');

logo_img <- readPNG("resources/logo.png");

total_ischemic_strokes <- sum(dfc_stats_data$Black) + 
    sum(dfc_stats_data$FL_Hispanic) + 
    sum(dfc_stats_data$PR_Hispanic) + 
    sum(dfc_stats_data$White);
    
create_charts();   

print("Done.");



