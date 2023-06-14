library(tidyverse)
library(ggwordcloud)
library(knitr)
library(kableExtra)
library(tidytext)
library(ggridges)

tidy_topics_all <- list.files(path = "data/Topic_Model/Clean", pattern = "tidy_topics_\\d", full.names = T, include.dirs=T, all.files =T) %>% 
	as_tibble() %>% 
	mutate(posts = map(value, read_csv)) %>% 
	mutate(level = str_extract(value, "\\d")) %>% 
	select(posts, level)

tidy_topic_docs_all <- list.files(path = "data/Topic_Model/Clean/", pattern = "tidy_topics_docs", full.names = T, include.dirs=T, all.files =T) %>% 
	as_tibble() %>% 
	mutate(posts = map(value, read_csv)) %>% 
	mutate(level = str_extract(value, "\\d")) %>% 
	select(posts, level)

topic_from_word <- function(my_word, my_level){
	tidy_topics_all %>%
		filter(level == my_level) %>% 
		unnest(posts) %>% 
		filter(word  == my_word) %>% 
		pull(topic) %>% 
		return()
}

all_subs <- tidy_topic_docs_all %>%
	filter(level == max(level)) %>% 
	unnest(posts) %>% 
	pull(Sub) %>% 
	unique()

params <- tibble(my_word = c("incontinence","surgery", "gleason", "diagnosed", "pain", "prostate","sex", "radiation", "biopsy", "prostatectomy"),
								 my_level = c(1,1,1,1,1,1,1,1,1,1) %>% as.character()) %>% 
	mutate(my_topics =
				 	map2(.x = my_word, .y = my_level, .f = ~topic_from_word(.x, .y))
	) %>% 
	unnest(my_topics)

my_id = 6

my_word <- params$my_word[my_id]
my_level <- params$my_level[my_id]
my_topic <- params$my_topics[my_id]
{
	my_level <- 1
	
	tidy_topics_this <- tidy_topics_all %>% 
		filter(level == my_level) %>% 
		unnest(posts)
	
	tidy_topic_this_desc <- tidy_topics_this %>% 
		group_by(topic) %>% 
		arrange(-p) %>% 
		filter(!str_detect(word, "[0-9]")) %>% 
		mutate(rank = 1:n()) %>% 
		filter(rank <= 3) %>% 
		summarise(topic_words = paste(word, collapse = ", "))
	
	my_full_topic <- tidy_topics_all %>%
		filter(level == my_level) %>% 
		unnest(posts) %>% 
		filter(topic == my_topic) %>% 
		select(topic, p, word) %>% 
		select(word)
	
	data <- read_csv("data/Scrape/clean_posts.csv")
	
	
	n_bin = 100
	res <- data %>% 
		select(Post_ID, Content) %>% 
		unnest_tokens(word, Content) %>% 
		group_by(Post_ID) %>% 
		mutate(word_position = (1:n()-0.5)/n()) %>% 
		ungroup() %>% 
		mutate(template_position = list((1:n_bin-0.5)/n_bin)) %>% 
		unnest(template_position) %>% 
		group_by(Post_ID, template_position) %>% 
		filter(abs(template_position - word_position) == min(abs(template_position - word_position))) %>% 
		arrange(Post_ID, template_position) %>% 
		inner_join(tidy_topics_this %>% select(topic, word)) %>% 
		group_by(topic, template_position) %>% 
		left_join(tidy_topic_this_desc, by = "topic") #%>% 
	#	filter(str_detect(topic_words, "(biopsy)|(gleason)|(diagnosed)|(incontinence)|(sex)|(pain)"))
	
	topic_positions <- res %>% 
		group_by(topic) %>% 
		summarise(mean_pos = median(template_position)) %>% 
		arrange(mean_pos) %>% 
		mutate(order = 1:n())
	
	p <- res %>% 
		left_join(topic_positions, by = "topic") %>% 
		drop_na() %>% 
		arrange(order) %>% 
		ggplot(aes(x = template_position, y = reorder(topic_words, -order), height = ..count.. , color = reorder(topic_words, -order), fill = reorder(topic_words, -order), alpha = 0.2, from = 0, to = 1)) +
		geom_density_ridges(stat = "density") + 
		theme_ridges() + 
		labs(x = "Location in text", y = "Topics") + 
		scale_x_continuous(breaks = c(0,0.5, 1), expand = c(0,0), limits = c(0,1)) + 
		theme(legend.position = "none") +   
		coord_cartesian(clip = "off")
	p
	
	res %>% 
		left_join(topic_positions, by = "topic") %>% 
		drop_na() %>% 
		arrange(order) %>% 
		group_by(template_position, topic_words) %>% 
		ggplot() + 
		geom_density(aes(x = template_position, color = topic_words))
	
	afinn <- get_sentiments("afinn")
	
	tmp_res <- res %>% 
		#left_join(classification %>% select(topic = topic2, Classification = descript), by = "topic") %>% 
		left_join(topic_positions, by = "topic") %>% 
		drop_na() %>% 
		arrange(order) %>% 
		#group_by(Classification, template_position) %>% 
	  group_by(topic, template_position) %>% 
		dplyr::summarise(count = n()) %>% 
	  #dplyr::group_by(Classification) %>% 
	  dplyr::group_by(topic) %>% 
	  dplyr::mutate(count = count/sum(count)) %>% 
		group_by(template_position) %>% 
		dplyr::filter(count == max(count)) %>% 
		arrange(template_position) %>%
		ungroup() %>% 
		dplyr::mutate(order = 1:n()) %>% 
	  #group_by(Classification) %>% 
	  group_by(topic) %>% 
	  dplyr::mutate(order = min(order))
	
	tmp_res %>% 
		group_by(template_position) %>% 
		nest() %>% 
		mutate(test_pos = list(c(template_position-1/n_bin/2, template_position, template_position + 1/n_bin/2))) %>% 
		unnest(test_pos) %>% 
		unnest(data) %>% 
		ungroup() %>% 
		ggplot() + 
	  #geom_line(aes(x = test_pos, y = reorder(Classification,order), color = Classification)) + 
	  geom_line(aes(x = test_pos, y = reorder(topic,order), color = topic))
	  #scale_fill_manual("Classification",values = c("Experience" = "#69B578","General"="#1B2F33","Symptoms" = "#D62246", "Treatment" = "#7D82B8", "Diagnosis" = "#FFA400"))+
		
	max_at <- tmp_res %>% 
		mutate(xmin = template_position-1/n_bin/2) %>% 
		mutate(xmax = template_position+1/n_bin/2) %>% 
	  #group_by(Classification) %>% 
	  group_by(topic) %>% 
	  ungroup() %>% 
		mutate(order2 = order)
	p1 <- max_at %>% 
		ggplot() + 
		#geom_linerange(aes(xmin = xmin, xmax = xmax, y = reorder(Classification, -order), color = Classification)) + 
	  geom_linerange(aes(xmin = xmin, xmax = xmax, y = reorder(topic, -order), color = topic)) + 
	  
		theme_minimal() + 
		scale_x_continuous(limits = c(0,1), breaks = c(0,0.5,1),labels = c("Beginning", "Middle", "End")) + 
		labs(x = "Position through document", y = "Topic",
				 title = "Topic peaks in text",
				 subtitle = "Showing topic with highest density p(position | topic)"
		) + 
		theme(
			legend.position = "none",
			legend.title = element_blank(),
			panel.grid.major.y = element_blank(),
			panel.grid.major.x = element_line(color = "gray60", size = 1/5),
			panel.grid.minor.x = element_line(color = "gray80", size = 1/10),
			plot.margin = unit(c(30, 30, 30, 30), "pt"),
			axis.text.y = element_text(hjust=0)
		) + 
		#scale_fill_manual("Classification",values = c("Experience" = "#69B578","General"="#1B2F33","Symptoms" = "#D62246", "Treatment" = "#7D82B8", "Diagnosis" = "#FFA400"))+
		guides(size = "none") 
	p1
	mean_path <- res %>% 
		left_join(afinn, by = "word") %>% 
	  #left_join(tmp_res %>% select(template_position, Classification), by = "template_position") %>% 
	  left_join(tmp_res %>% select(template_position, topic), by = "template_position") %>% 
	  drop_na() %>% 
	  #group_by(template_position, Classification) %>% 
	  group_by(template_position, topic.x) %>% 
	  summarise(value = mean(value, na.rm =T)) %>% 
		ungroup() %>% 
	  transmute(template_position, topic = topic.x, value)
	
	named_things <- mean_path %>% 
		left_join(mean_path %>% mutate(template_position = template_position + 1/n_bin),
							by = "template_position") %>% 
		drop_na() %>%
	  #group_by(Classification.x, Classification.y) %>% 
	  group_by(topic.x, topic.y) %>% 
	  summarise(first = template_position[1]) %>% 
		arrange(first) %>% 
	  #filter(Classification.x == Classification.y) %>% 
	  filter(topic.x == topic.y) %>% 
	  #select(template_position = first,Classification = Classification.x) %>% 
	  select(template_position = first,topic = topic.x) %>% 
	  left_join(mean_path)
	
	p2 <- res %>% 
		left_join(afinn, by = "word") %>% 
	  #left_join(tmp_res %>% select(template_position, Classification), by = "template_position") %>% 
	  left_join(tmp_res %>% select(template_position, topic), by = "template_position") %>% 
	  drop_na() %>% 
	  #group_by(Post_ID,word_position, Classification) %>% 
	  group_by(Post_ID,word_position, topic_words) %>% 
	  summarise(value = mean(value, na.rm =T)) %>% 
		ggplot() + 
		#geom_line(aes(x = word_position, y = zoo::rollmean(value, k = 20, fill = NA),group = Post_ID),alpha = 0.05) +
	  #geom_line(data = mean_path, aes(x = template_position, y = zoo::rollmean(value, k = 6, fill = NA),group = 1, color = Classification),alpha = 1) +
	  geom_line(data = mean_path, aes(x = template_position, y = zoo::rollmean(value, k = 6, fill = NA),group = 1, color = topic),alpha = 1) +
	  theme_minimal() + 
		scale_x_continuous(limits = c(0,1), breaks = c(0,0.5,1),labels = c("Beginning", "Middle", "End")) + 
		labs(x = "Position through document", y = "Sentiment",
				 title = "Narrative arc",
		) + 
		theme(
			legend.position = "none",
			legend.title = element_blank(),
			panel.grid.major.y = element_blank(),
			panel.grid.major.x = element_line(color = "gray60", size = 1/5),
			panel.grid.minor.x = element_line(color = "gray80", size = 1/10),
			plot.margin = unit(c(30, 30, 30, 30), "pt"),
			axis.text.y = element_text(hjust=0)
		)	+
		coord_cartesian(ylim = c(-1,1))  + 
		#geom_point(data = mean_path %>% mutate(y = zoo::rollmean(value, k = 6, fill = NA)) %>% inner_join(named_things, by = "template_position"),
		#					 aes(x = template_position, y = y)) +
	  #ggrepel::geom_text_repel(data = named_things, mapping = aes(x = template_position, y = value, label = Classification, color = Classification)) + 
	  ggrepel::geom_text_repel(data = named_things, mapping = aes(x = template_position, y = value, label = topic, color = topic)) + 
	  #scale_color_manual("Classification",values = c("Experience" = "#69B578","General"="#1B2F33","Symptoms" = "#D62246", "Treatment" = "#7D82B8", "Diagnosis" = "#FFA400"))+
		guides(size = "none") +
		geom_hline(yintercept = 0, alpha = 0.2)
	p2
	
	
	tmp_res <- res %>% 
		left_join(topic_positions, by = "topic") %>% 
		drop_na() %>% 
		arrange(order) %>% 
		group_by(topic, topic_words, template_position) %>% 
		dplyr::summarise(count = n()) %>% 
		dplyr::group_by(topic) %>% 
		dplyr::mutate(count = count/sum(count)) %>% 
		group_by(template_position) %>% 
		dplyr::filter(count == max(count)) %>% 
		arrange(template_position) %>%
		ungroup() %>% 
		dplyr::mutate(order = 1:n()) %>% 
		group_by(topic_words) %>% 
		dplyr::mutate(order = min(order))
	
	tmp_res %>% 
		group_by(template_position) %>% 
		nest() %>% 
		mutate(test_pos = list(c(template_position-1/n_bin/2, template_position, template_position + 1/n_bin/2))) %>% 
		unnest(test_pos) %>% 
		unnest(data) %>% 
		ungroup() %>% 
		ggplot() + 
		geom_line(aes(x = test_pos, y = reorder(topic_words,order), color = topic_words))
	
	max_at <- tmp_res %>% 
		mutate(xmin = template_position-1/n_bin/2) %>% 
		mutate(xmax = template_position+1/n_bin/2) %>% 
		group_by(topic_words) %>% 
		ungroup() %>% 
		mutate(order2 = order)
	p1 <- max_at %>% 
		ggplot() + 
		geom_linerange(aes(xmin = xmin, xmax = xmax, y = reorder(topic_words, -order), color = topic_words)) + 
		theme_minimal() + 
		scale_x_continuous(limits = c(0,1), breaks = c(0,0.5,1),labels = c("Beginning", "Middle", "End")) + 
		labs(x = "Position through document", y = "Topic",
				 title = "Topic peaks in text",
				 subtitle = "Showing topic with highest density p(position | topic)"
		) + 
		theme(
			legend.position = "none",
			legend.title = element_blank(),
			panel.grid.major.y = element_blank(),
			panel.grid.major.x = element_line(color = "gray60", size = 1/5),
			panel.grid.minor.x = element_line(color = "gray80", size = 1/10),
			plot.margin = unit(c(30, 30, 30, 30), "pt"),
			axis.text.y = element_text(hjust=0)
		)
	
	mean_path <- res %>% 
		left_join(afinn, by = "word") %>% 
		left_join(tmp_res %>% select(template_position, top_topic = topic_words, topic2 = topic), by = "template_position") %>% 
		left_join(classification %>% select(topic2, Classification = descript), by = "topic2") %>% 
		drop_na() %>% 
		group_by(template_position, top_topic, Classification) %>% 
		summarise(value = mean(value, na.rm =T))
		
	named_things <- mean_path %>% 
		left_join(mean_path %>% mutate(template_position = template_position + 1/n_bin),
							by = "template_position") %>% 
		drop_na() %>%
		group_by(top_topic.x, top_topic.y) %>% 
		summarise(first = template_position[1]) %>% 
		arrange(first) %>% 
		filter(top_topic.x == top_topic.y) %>% 
		select(top_topic = top_topic.x, template_position = first) %>% 
		left_join(mean_path)
	
	p2 <- res %>% 
		left_join(afinn, by = "word") %>% 
		left_join(tmp_res %>% select(template_position, top_topic = topic_words), by = "template_position") %>% 
		left_join(classification %>% select(topic = topic2, Classification = descript), by = "topic") %>% 
		drop_na() %>% 
		group_by(Post_ID,word_position, top_topic, topic_words,Classification) %>% 
		summarise(value = mean(value, na.rm =T)) %>% 
		ggplot() + 
		#geom_line(aes(x = word_position, y = zoo::rollmean(value, k = 20, fill = NA),group = Post_ID),alpha = 0.05) +
		geom_line(data = mean_path, aes(x = template_position, y = zoo::rollmean(value, k = 6, fill = NA),group = 1, color = Classification),alpha = 1) +
		theme_minimal() + 
		scale_x_continuous(limits = c(0,1), breaks = c(0,0.5,1),labels = c("Beginning", "Middle", "End")) + 
		labs(x = "Position through document", y = "Sentiment",
				 title = "Narrative arc",
		) + 
		theme(
			legend.position = "bottom",
			legend.title = element_blank(),
			panel.grid.major.y = element_blank(),
			panel.grid.major.x = element_line(color = "gray60", size = 1/5),
			panel.grid.minor.x = element_line(color = "gray80", size = 1/10),
			plot.margin = unit(c(30, 30, 30, 30), "pt"),
			axis.text.y = element_text(hjust=0)
		)	+
		coord_cartesian(ylim = c(-1,1))  + 
		#geom_point(data = mean_path %>% mutate(y = zoo::rollmean(value, k = 6, fill = NA)) %>% inner_join(named_things, by = "template_position"),
		#					 aes(x = template_position, y = y)) +
		ggrepel::geom_text_repel(data = named_things, mapping = aes(x = template_position, y = value, label = top_topic, color = Classification))
	p2
	ggsave(p, 
				 filename = paste("Figures/Plots/topic_location_density_", my_level,".pdf", sep = ""),
				 height = nrow(topic_positions)/4,width= 6, device = "pdf", limitsize = F)
	
	p <- res %>% 
		left_join(topic_positions, by = "topic") %>% 
		left_join(classification %>% select(topic = topic2, Classification = descript), by = "topic") %>% 
		drop_na() %>% 
		arrange(order) %>% 
		ggplot(aes(x = template_position, y = reorder(topic_words, -order), from = 0, to = 1, fill = Classification, alpha = 0.2)) +
		geom_density_ridges() + 
		theme_ridges() + 
		labs(x = "Location in text", y = "Topics") + 
		#scale_x_continuous(breaks = c(0,0.5, 1), expand = c(0,0), limits = c(0,1)) + 
		coord_cartesian(clip = "off") + 
		theme_minimal() + 
		scale_x_continuous(limits = c(0,1), breaks = c(0,0.5,1),labels = c("Beginning", "Middle", "End")) + 
		labs(x = "Position through document", y = "Topics",
				 title = "Topic position through documents",
				 subtitle = "Topics that occur earlier are closer to the top.\n"
		) + 
		theme(
			legend.position = "bottom",
			panel.grid.major.y = element_blank(),
			panel.grid.major.x = element_line(color = "gray60", size = 1/5),
			panel.grid.minor.x = element_line(color = "gray80", size = 1/10),
			plot.margin = unit(c(30, 30, 30, 30), "pt"),
			axis.text.y = element_text(hjust=0)
		) + 
		scale_fill_manual("Classification",values = c("Experience" = "#69B578","General"="#1B2F33","Symptoms" = "#D62246", "Treatment" = "#7D82B8", "Diagnosis" = "#FFA400"))+
		guides(alpha = "none", fill = guide_legend(override.aes = list(alpha = 0.5)))
	p
	
	p_grid <- cowplot::plot_grid(p,p2,nrow=2, align = "v",rel_heights = c(2,1))
	ggsave(p_grid, width = 8, height = 10, filename = "Figures/combined.pdf")
		
	library(extrafont)
	
	p <- res %>% 
		left_join(topic_positions, by = "topic") %>% 
		drop_na() %>% 
		arrange(order) %>% 
		ggplot(aes(x = template_position, y = reorder(topic_words, -order))) + #, fill = factor(stat(quantile)), from = 0, to = 1)) +
		stat_density_ridges(
			geom = "density_ridges_gradient", calc_ecdf = TRUE,
			quantiles = 2, quantile_lines = TRUE
		) +
		#scale_fill_viridis_d(name = "Quartiles") + 
		theme_ridges() + 
		labs(x = "Location in text", y = "Topics") + 
		#scale_x_continuous(breaks = c(0,0.5, 1), expand = c(0,0), limits = c(0,1)) + 
		theme(legend.position = "bottom") + 
		coord_cartesian(clip = "off") + theme_minimal() + 
		scale_x_continuous(limits = c(0,1), breaks = c(0,0.5,1),labels = c("Beginning", "Middle", "End")) + 
		labs(x = "Position through document", y = "Topics",
				 title = "Topic position through documents",
				 subtitle = "Interquartile range of topic position. Topics that occur earlier are closer to the top.\n"
		) + 
		theme(
			legend.position = "bottom",
			legend.title = element_blank(),
			panel.grid.major.y = element_blank(),
			panel.grid.major.x = element_line(color = "gray60", size = 1/5),
			panel.grid.minor.x = element_line(color = "gray80", size = 1/10),
			plot.margin = unit(c(30, 30, 30, 30), "pt"),
			axis.text.y = element_text(hjust=0)
		) 
	p
	
	ggsave(p, 
				 filename = paste("Figures/Plots/topic_location_", my_level,".pdf", sep = ""),
				 height = nrow(topic_positions)/2,width= 8, device = "pdf", limitsize = F)
	
}

mode <- function(codes){
	which.max(tabulate(codes))
}

library(plyr)
library(dplyr)
full_data <- res %>% 
	left_join(topic_positions, by = "topic") %>% 
	drop_na() %>% 
	arrange(order) %>% 
	group_by(topic, template_position) %>% 
	dplyr::mutate(count = n()) %>% 
	group_by(topic) %>% 
	dplyr::mutate(maxcount = count == max(count)) %>% 
	dplyr::mutate(mode = ifelse(maxcount, template_position, 0)) %>% 
	group_by(topic, topic_words, order) %>% 
	dplyr::summarise(`Lower quartile`  = quantile(template_position, probs = 0.25),
						 Median = quantile(template_position, probs = 0.5),
						 `Upper quartile` = quantile(template_position, probs = 0.75),
						 #Mean = mean(template_position), 
						 Mode = max(mode)) %>% 
	pivot_longer(`Lower quartile`:Mode) %>% #`Upper quartile`) %>% 
	dplyr::mutate(group2 = ifelse(name == "Mode", NA,paste(topic, "group")))
	
	
p <- full_data %>% 
	filter(name != "Mode") %>% 
	ggplot(aes(x = value, y = reorder(topic_words, -order))) + 
	geom_line(data = full_data %>% 
							filter(name != "Mode") %>% drop_na(), aes(color = "line", group = group2), show.legend = F) +
	geom_point(data = full_data %>% 
						 	filter(name != "Mode"),aes(color = name)) +
	scale_color_manual(values=c("Lower quartile" = "#20a39e", "Median" = "#264653","Upper quartile" = "#ef5b5b",
															#"Mode" = "#3a0ca3", 
															"line" = "grey75"), limits = c("Lower quartile", "Median", "Upper quartile"
																														 #,"Mode"
																														 ))+
	scale_shape_manual(values=c("Lower quartile" = "#20a39e", "Median" = "#264653",
														#	"Mode" = "#3a0ca3",
															"Upper quartile" = "#ef5b5b", "line" = "grey75"), limits = c("Lower quartile", "Median", "Upper quartile"
																																													 #,"Mode"
																																													 )) +
	#scale_color_identity(guide = "none") +
	theme_minimal() + 
	scale_x_continuous(limits = c(0,1), breaks = c(0,0.5,1),labels = c("Beginning", "Middle", "End")) + 
	labs(x = "Position through document", y = "Topics",
			 	title = "Topic position through documents",
			 	subtitle = "Interquartile range of topic position. Topics that occur earlier are closer to the top.\n"
			 ) + 
	theme(
		legend.position = "bottom",
		legend.title = element_blank(),
		panel.grid.major.y = element_blank(),
		panel.grid.major.x = element_line(color = "gray60", size = 1/5),
		panel.grid.minor.x = element_line(color = "gray80", size = 1/10),
		plot.margin = unit(c(30, 30, 30, 30), "pt"),
		axis.text.y = element_text(hjust=0)
	) 
	#geom_segment(data = tibble(), aes(x = 0.9, xend = 0.9, y = 0, yend = nrow(topic_positions)+1), arrow = arrow(length = unit(0.02, "npc")),lineend = "butt", linejoin = "mitre", color = "gray50",size = 1/4) + 
	#annotate("text", x = 0.8, y = nrow(topic_positions), label = "Earlier topics", size = 5, color = "gray50")


p <- max_at %>% 
	mutate(value = 1) %>% 
	ggplot(aes(x = value, y = reorder(topic_words, order2))) + 
	#geom_line(data = full_data %>% 
	#						filter(name != "Mode") %>% drop_na(), aes(color = "line", group = group2), show.legend = F) +
	geom_point(data = full_data %>% left_join(max_at %>% select(topic_words, order2), by = "topic_words") %>%
						 	mutate(order2 = ifelse(is.na(order2), order*10000, order2)) %>% 
						 	filter(name != "Mode"),aes(color = name, y = reorder(topic_words, -order2))) +
	geom_linerange(mapping = aes(xmin = xmin, xmax = xmax, y = reorder(topic_words,-order2), color = topic_words))  + 
	scale_color_manual(values=c("Lower quartile" = "#20a39e", "Median" = "#264653","Upper quartile" = "#ef5b5b",
															#"Mode" = "#3a0ca3", 
															"line" = "grey75"), limits = c("Lower quartile", "Median", "Upper quartile"
																														 #,"Mode"
															))+
	scale_shape_manual(values=c("Lower quartile" = "#20a39e", "Median" = "#264653",
															#	"Mode" = "#3a0ca3",
															"Upper quartile" = "#ef5b5b", "line" = "grey75"), limits = c("Lower quartile", "Median", "Upper quartile"
																																													 #,"Mode"
															)) +
	#scale_color_identity(guide = "none") +
	theme_minimal() + 
	scale_x_continuous(limits = c(0,1), breaks = c(0,0.5,1),labels = c("Beginning", "Middle", "End")) + 
	labs(x = "Position through document", y = "Topics",
			 title = "Topic position through documents",
			 subtitle = "Interquartile range of topic position. Topics that occur earlier are closer to the top.\n"
	) + 
	theme(
		legend.position = "bottom",
		legend.title = element_blank(),
		panel.grid.major.y = element_blank(),
		panel.grid.major.x = element_line(color = "gray60", size = 1/5),
		panel.grid.minor.x = element_line(color = "gray80", size = 1/10),
		plot.margin = unit(c(30, 30, 30, 30), "pt"),
		axis.text.y = element_text(hjust=0)
	) 
p
ggsave(p, 
			 filename = paste("Figures/Plots/topic_location_density_lolipop_", my_level,".pdf", sep = ""),
			 height = nrow(topic_positions)/3,width= 8, device = "pdf", limitsize = F)


data %>% 
	select(Post_ID, Content) %>% 
	unnest_tokens(word, Content) %>% 
	group_by(Post_ID) %>% 
	mutate(word_position = (1:n())/n()) %>% 
	inner_join(my_full_topic, by = "word") %>% 
	summarise(mean_pos = mean(word_position)) %>% 
	ggplot() + 
	geom_histogram(aes(x = mean_pos)) + 
	labs(title = my_word)

data %>% 
	select(Post_ID, Content) %>% 
	unnest_tokens(word, Content) %>% 
	group_by(Post_ID) %>% 
	mutate(word_position = (1:n())/n()) %>% 
	inner_join(my_full_topic, by = "word") %>% 
	summarise(mean_pos = mean(word_position)) %>% 
	ungroup() %>% 
	summarise(mean_mean_pos = mean(mean_pos))


data %>% 
	select(Post_ID, Content) %>% 
	unnest_tokens(word, Content) %>% 
	group_by(Post_ID) %>% 
	dplyr::mutate(word_position = (1:n())/n()) %>% 
	inner_join(tidy_topics_this, by = "word") %>% 
	group_by(Post_ID, topic) %>% 
	summarise(mean_pos = mean(word_position)) %>% 
	group_by(topic) %>% 
	summarise(mean_mean_pos = mean(mean_pos)) %>% 
	left_join(tidy_topic_this_desc, by = "topic") %>% 
	arrange(mean_mean_pos) %>% 
	ggplot() + 
	geom_point(aes(x = mean_mean_pos, y = 0)) + 
	ggrepel::geom_label_repel(aes(x = mean_mean_pos, y = 0, label = topic_words)) +
	ggsave(filename = "Figures/progression.pdf", device = "pdf", width = 45, height = 10)
