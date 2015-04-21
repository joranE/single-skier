library(dplyr)
library(ggplot2)
library(scales)
library(RColorBrewer)
library(readr)

tech_colors <- brewer.pal(n = 3,name = "Set1")
tech_colors <- setNames(tech_colors,c("C","F","FC"))
tech_labels <- c("Classic","Freestyle","Pursuit")

plot_dst <- function(nm,type = c("points","rank","mpb"),maj_int = TRUE){
	type <- match.arg(type)
	yval <- switch(type,
                 "points" = "fispoints",
                 "rank" = "rank",
                 "mpb" = "mpb")
	ylab <- switch(type,
                 "points" = "FIS Points",
                 "rank" = "Finishing Place",
                 "mpb" = "Standardized % Behind Median")
  if (type == "mpb"){
    plot_data <- filter(DATA,name == nm & type == "Distance" & !is.na(mpb))
  }else{
    plot_data <- filter(DATA,name == nm & type == "Distance")
  }
  if (maj_int){
    plot_data <- filter(plot_data,cat1 %in% c("WC","WSC","OWG","TDS"))
  }
	if (NROW(plot_data) < 5){
		return(NULL)
	}
	if (type == "rank"){
		pts_line <- geom_hline(yintercept = 30,color = "black")
	}else{
		pts_line <- NULL
	}
	plot_data$date <- as.Date(plot_data$date)
	ggplot(plot_data,aes_string(x = "date",y = yval)) + 
		pts_line + 
		geom_point(aes(color = tech)) + 
		scale_color_manual(values = tech_colors,labels = tech_labels) +
		scale_x_date(breaks = date_breaks(width = "1 year")) + 
		labs(x = NULL,y = ylab,color = "Technique") + 
		theme(legend.position = "bottom",
					legend.direction = "horizontal",
					axis.text.x = element_text(hjust=0,vjust=1,angle=310,
																		 size=9,color = "black"))
}

plot_spr <- function(nm,type = c("points","rank"),maj_int = TRUE){
  type <- match.arg(type)
  yval <- switch(type,
                 "points" = "fispoints",
                 "rank" = "rank")
  ylab <- switch(type,
                 "points" = "FIS Points",
                 "rank" = "Finishing Place")
	plot_data <- filter(DATA,name == nm & type == "Sprint")
	if (maj_int){
	  plot_data <- filter(plot_data,cat1 %in% c("WC","WSC","OWG","TDS"))
	}
	if (NROW(plot_data) < 5){
		return(NULL)
	}
	plot_data$date <- as.Date(plot_data$date)
	ggplot(plot_data,aes_string(x = "date",y = yval)) + 
		geom_hline(yintercept = 30,color = "black") +
		geom_point(aes(color = tech)) + 
		scale_color_manual(values = tech_colors,labels = tech_labels) +
		scale_x_date(breaks = date_breaks(width = "1 year")) +
		labs(x = NULL,y = ylab,color = "Technique") + 
		theme(legend.position = "bottom",
					legend.direction = "horizontal",
					axis.text.x = element_text(hjust=0,vjust=1,angle=310,
																		 size=9, color = "black"))
}

plot_spr_bar <- function(nms,
										 byTech = FALSE,
                     maj_int = TRUE){
	commapos <- function(x, ...) {
		format(abs(x), big.mark = ",", trim = TRUE,
					 scientific = FALSE, ...)
	}
	spr_data <- filter(DATA,name == nms & type == "Sprint")
	if (maj_int){
	  plot_data <- filter(spr_data,cat1 %in% c("WC","WSC","OWG","TDS"))
	}
	if (NROW(spr_data) < 5){
		return(NULL)
	}
	spr_data$lev <- cut(spr_data$rank,
								 breaks = c(-Inf,6,12,30,Inf),
								 labels = c('Final','Semi','Quarter','Qual'))
	if (byTech){
		spr_data_sum <- spr_data %>%
			group_by(name,season,tech,lev) %>%
			summarise(tot = length(lev))
		spr_data_sum$tech <- c(F = "Freestyle",C = "Classic")[spr_data_sum$tech]
		facet <- facet_wrap(~tech,ncol = 2)
	}else{
		spr_data_sum <- spr_data %>%
			group_by(name,season,lev) %>%
			summarise(tot = length(lev))
		facet <- NULL
	}
	
	spr_data_sum$season <- factor(spr_data_sum$season,
													levels = sort(unique(spr_data_sum$season)))
	spr_data_sum$lev <- factor(spr_data_sum$lev,
											 levels = c('Qual','Final','Semi','Quarter'),
											 labels = paste(c(1,4:2),c('Qual','Final','Semi','Quarter')))
	spr_data_sum$below <- FALSE
	spr_data_sum$below[spr_data_sum$lev == '1 Qual'] <- TRUE
	lower <- filter(spr_data_sum,below)
	upper <- filter(spr_data_sum,!below)
	
	upper$lev <- factor(upper$lev,rev(levels(upper$lev)))
	
	if (nrow(lower) > 0){
		rng_sum <- lower %>%
			group_by(season) %>%
			summarise(tot = sum(tot))
		rng_lower <- range(-rng_sum$tot)
	}else{
		rng_lower <- c(0,0)
	}
	
	if (nrow(upper) > 0){
		rng_sum <- upper %>%
			group_by(season) %>%
			summarise(tot = sum(tot))
		rng_upper <- range(rng_sum$tot)
	}else{
		rng_upper <- c(0,0)
	}
	
	p <- ggplot() + 
		facet +
		geom_bar(data = upper,
						 aes(x = season,y = tot,fill = lev,order = lev),
						 width = 0.5,
						 position = "stack",
						 stat = "identity") + 
		geom_bar(data = lower,
						 aes(x = season,y = -tot,fill = lev),
						 width = 0.5,
						 position = "stack",
						 stat = "identity") +
		geom_hline(data = spr_data_sum,
							 aes(yintercept = 0),
							 colour = "black") + 
		labs(x = NULL,y = "Number of Races",fill = "Max\nround\nreached") + 
		scale_y_continuous(breaks = seq(from = min(rng_lower),to = max(rng_upper),by = 1),
											 labels = commapos) + 
		scale_fill_manual(values = brewer.pal(6,"Blues")[3:6]) +
		theme(axis.text.x = element_text(hjust=0,vjust=1,angle=310,
																		 size=9,color = "black"),
					legend.position = "bottom",
					legend.direction = "horizontal") + 
		guides(fill = guide_legend(reverse = TRUE))
	p
}

ath_summary <- function(nm){
  ath <- filter(DATA,name == nm)
  
  ath_wjc <- filter(ath,cat1 == 'WJC') %>%
    arrange(desc(season),desc(date)) %>%
    select(season,date,type,length,tech,rank,rankqual,fispoints) %>%
    rename(Season = season,
           Date = date,
           Type = type,
           Length = length,
           Tech = tech,
           Posn = rank,
           QualPosn = rankqual,
           FISPoints = fispoints)
  if (nrow(ath_wjc) == 0){
    ath_wjc <- setNames(data.frame(as.list(rep(NA,8))),names(ath_wjc))
  }
  ath_u23 <- filter(ath,cat1 == 'U23') %>%
    arrange(desc(season),desc(date)) %>%
    select(season,date,type,length,tech,rank,rankqual,fispoints) %>%
    rename(Season = season,
           Date = date,
           Type = type,
           Length = length,
           Tech = tech,
           Posn = rank,
           QualPosn = rankqual,
           FISPoints = fispoints)
  if (nrow(ath_u23) == 0){
    ath_u23 <- setNames(data.frame(as.list(rep(NA,8))),names(ath_u23))
  }
  
  ath_maj <- filter(ath,cat1 %in% c('WC','OWG','WSC','TDS')) %>%
    group_by(cat1,type) %>%
    summarise(Races = n_distinct(raceid),
              Wins = sum(rank == 1,na.rm = TRUE),
              Podiums = sum(rank <= 3,na.rm = TRUE),
              Top30 = sum(rank <= 30,na.rm = TRUE)) %>%
    rename(Event = cat1,Type = type)
  ath_maj$Event <- factor(c('WC' = 'World Cup',
                            'OWG' = 'Olympics',
                            'WSC' = 'World Champs',
                            'TDS' = 'Tour de Ski')[ath_maj$Event],
                          levels = c('World Cup','Tour de Ski','World Champs','Olympics','Total'))
  ath_maj <- arrange(ath_maj,Event)
  if (nrow(ath_maj) == 0){
    ath_maj <- setNames(data.frame(as.list(rep(NA,6))),names(ath_maj))
  }
  
  ath_maj_tot <- filter(ath,cat1 %in% c('WC','OWG','WSC','TDS')) %>%
    group_by(type) %>%
    summarise(Event = "Total",
              Races = n_distinct(raceid),
              Wins = sum(rank == 1,na.rm = TRUE),
              Podiums = sum(rank <= 3,na.rm = TRUE),
              Top30 = sum(rank <= 30,na.rm = TRUE)) %>%
    rename(Type = type) %>%
    select(Event,Type,Races,Wins,Podiums,Top30)
  ath_maj_tot$Event <- factor(ath_maj_tot$Event,
                              levels = c('World Cup','Tour de Ski','World Champs','Olympics','Total'))
  if (nrow(ath_maj_tot) == 0){
    ath_maj_tot <- NULL
  }
  return(list(ath_wjc = ath_wjc,
              ath_u23 = ath_u23,
              ath_maj = rbind(ath_maj,ath_maj_tot)))
}
