# Dependencies
require(png)
require(ggplot2)
require(reshape2)
require(scales)

# Creates multiple plots in one chart are
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

# Load source data from a file
load_data <- function(filename) {
	df <- read.csv(paste("data/", filename, sep=""),
               header = TRUE,
               quote="\"",
               strip.white = TRUE);
  return(df)
}

# Filters data by site id
filter_site_id <- function(site_id, data) {
	df <- data[data$Hospital_ID == site_id, ];
	return(df)
}

# Filters data by outcome
filter_outcome <- function(outcome_id, data) {
	df <- data[data$outcome == outcome_id, ];
	return(df)
}

# Creates the ethnicities chart for one outcome
create_ethnicities_chart <- function(outcome, df) {

  # Rearranges the data fram by race
  df1 <- df_restructure(df, "race");

  # Generate text for y label fractions
  ylab_text <- "\n";
  for(i in 1:nrow(df)) {
      row <- df[i,]
      ylab_text <- paste(ylab_text, row$race, " " , row$num_N, "/", row$deno_N, "    ", sep="")
  }
   
  # Create ethnicity plot 
  g <- ggplot(data=df1, aes(x=race, y=value, fill=variable)) +
      geom_bar(size=.3, colour="black", stat="identity", position=position_dodge(), ) + coord_flip() +
      theme_grey() +
      theme(legend.position="bottom", legend.title=element_blank(), plot.margin=unit(c(2,2,2,2), "cm")) +
      ggtitle(outcome) + ylab(ylab_text) + xlab("") +
      scale_y_continuous(labels=percent, limits=c(0,1), breaks=seq(0, 1, 0.1)) +
      geom_text(aes(label=paste(label=value*100, "%", sep="")), position=position_dodge(width=0.9), hjust=1.2);
      
  print(g);
}


# Creates the gender chart for one outcome
create_gender_chart <- function(outcome, df) {

  # Rearranges the data fram by sex
  df1 <- df_restructure(df, "sex");

  # Generate text for y label fractions
  ylab_text <- "\n";
  for(i in 1:nrow(df)) {
      row <- df[i,]
      ylab_text <- paste(ylab_text, row$sex, " " , row$num_N, "/", row$deno_N, "    ", sep="")
  }
   
  # Create ethnicity plot 
  g <- ggplot(data=df1, aes(x=sex, y=value, fill=variable)) +
      geom_bar(size=.3, colour="black", stat="identity", position=position_dodge(), ) + coord_flip() +
      theme_grey() +
      theme(legend.position="bottom", legend.title=element_blank(), plot.margin=unit(c(2,2,2,2), "cm")) +
      ggtitle(outcome) + ylab(ylab_text) + xlab("") +
      scale_y_continuous(labels=percent, limits=c(0,1), breaks=seq(0, 1, 0.1)) +
      geom_text(aes(label=paste(label=value*100, "%", sep="")), position=position_dodge(width=0.9), hjust=1.2);
      
  print(g);
}

# Creates the rank graph with all hospitals
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

# Creates the summary text for the summary page
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


# Creates the front page
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

# Rearrange the data structure by "type"
df_restructure <- function(df, type) {
    df <- melt(df, id=c(type, "Hospital_ID", "outcome", "num_N", "deno_N"));
    return(df);
}

# Main function to run the chart generation loop
create_charts <- function() {
    
    # Vector with all site ids
    site_id_vector <- unique(ethnicity_data$Hospital_ID);

    for (site_id in site_id_vector) {
        
        filename <- paste("reports/", site_id, ".pdf", sep="");
        pdf(file = filename);
        
        front_page(site_id);
        create_rank_graph(site_id);
        
        # Ethnicity data frame for site id
        df1_eth <- filter_site_id(site_id, ethnicity_data);
        outcomes_vector_eth <- unique(df1_eth$outcome);

        # Gender data fram for site id
        df1_gen <- filter_site_id(site_id, gender_data);
        outcomes_vector_gen <- unique(df1_gen$outcome);
        
        
        # Ethnicity charts
        for (outcome in outcomes_vector_eth){
            df2_eth <- filter_outcome(outcome, df1_eth);
            create_ethnicities_chart(outcome, df2_eth);
        }

        # Gender charts
        for (outcome in outcomes_vector_gen){
            df2_gen <- filter_outcome(outcome, df1_gen);
            create_gender_chart(outcome, df2_gen);
        }
        
        dev.off();
        print(paste("Created pdf for site ", site_id));
    }
}


# Run
print("Running...");
ethnicity_data <- load_data('ethnicity.csv');
dfc_rank_data <- load_data('dfc_ranks.csv');
dfc_stats_data <- load_data('dfc_stats.csv');
gender_data <- load_data('gender.csv');
logo_img <- readPNG("data/logo.png");

total_ischemic_strokes <- sum(dfc_stats_data$Black) + 
    sum(dfc_stats_data$FL_Hispanic) + 
    sum(dfc_stats_data$PR_Hispanic) + 
    sum(dfc_stats_data$White);
    
create_charts();   

print("Done.");



