library(tidyverse)
library(tsne)
library(ggrepel)
library(superheat)

max_level <- read_lines("data/Topic_Model/max_level.csv") %>% as.integer()

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

# Remove columns for cleaner data
tta_small <- tidy_topics_all %>% 
	unnest(posts) %>% 
	select(word, topic, level) %>% 
	group_by(level) %>% 
	nest(.key = "posts") %>% 
	mutate(level = level %>% as.numeric()) %>% 
	ungroup()

# This gives the hierarchical structure
for(my_level in tta_small$level){
	if(my_level == 0){
		df <- tta_small %>% 
			filter(level == 0) %>% 
			select(-level) %>% 
			unnest(posts)
	}else{
		df <- df %>% 
			left_join(
				tta_small %>% 
					filter(level == my_level) %>% 
					select(-level) %>% 
					unnest(posts), 
				by = "word",
				suffix = c(my_level-1, my_level) %>% as.character
			)
	}
}

# Group-to-group interaction rates between topics
tidy_topic_interactions <- list.files(path = "data/Topic_Model", pattern = "p_td_tw_level_",
																			full.names = T, include.dirs=T, all.files =T) %>% 
	as_tibble() %>%
	select(path = value) %>% 
	mutate(
		mat = map(path, 
							function(path){
								read_csv(path) %>% 
									mutate(td = 1:n()) %>% 
							    select(td, everything()) %>% 
									gather("tw", "p", -td) %>% 
									mutate(tw = as.numeric(tw) + 1)
							}
		)
	) %>% 
	mutate(level = str_extract(path, "\\d")) %>% 
	filter(level <= max_level) %>%
	select(level, mat)

# Need to normalise by topic density
full_interactions <- tidy_topic_interactions %>% 
	inner_join(tidy_topic_interactions, by = "level") %>% 
	unnest(mat.x) %>%
	unnest(mat.y, key = "1") %>% 
	filter(td == td1) %>% 
	group_by(level, tw, tw1) %>% 
	summarise(p = -sum(p*p1)) %>% 
	arrange(level, tw, tw1) %>% 
	group_by(level) %>% 
	nest() %>% 
	ungroup() %>% 
	mutate(level = as.numeric(level)) %>% 
	filter(level != max(level)) 

library(umap)
umap.defaults$n_epochs <- 2000

t <- full_interactions %>% 
	mutate(mat = 
				 	map(data, function(data = .x){
				 		data %>% 
				 			pivot_wider(names_from = tw1, values_from = p) %>% 
				 			select(-tw) %>% 
				 			as.matrix() %>% 
				 			umap::umap() %>% .$layout %>% 
				 			as_tibble()
				 	}
				 	)
	)

t2 <- t %>% 
	inner_join(topic_tops %>% group_by(level) %>% nest() %>% ungroup() %>%  transmute(level, top = data) %>%  mutate(level = as.numeric(level)) %>% filter(level != max(level)), by = "level") %>% 
	mutate(
		mat = map2(mat, top,
							 function(mat = .x, top = .y){
							 	mat %>% mutate(topic = 1:n()) %>% 
							 		inner_join(top, by = "topic")
							 })
	)

p <- t2 %>% 
	filter(level == 0) %>% 
	ungroup() %>% 
	unnest(mat) %>% 
	ggplot(aes(V1, V2, label = word, size = 10*p^(1/0.7))) + 
	geom_text_repel(show.legend = F) + 
	theme_void()
p

classification <- tidy_topics_all %>% filter(level == 1) %>% unnest(posts) %>% 
	select(topic2 = topic, word2 = word,p) %>% 
	group_by(topic2) %>% 
	filter(!str_detect(word2, "[0-9]")) %>% 
	filter(p == max(p)) %>% 
	bind_cols(tibble(descript= c("General","Treatment","Experience","General","Treatment","Experience","Symptoms","Symptoms","Symptoms","Symptoms","General","Treatment","Diagnosis","Diagnosis","Diagnosis","Treatment","General","Treatment","Treatment","Diagnosis"
	))) %>% 
	{. ->> classification_0} %>% 
	select(topic2, descript)

p <- t2 %>% 
	filter(level == 0) %>% 
	ungroup() %>% 
	unnest(mat) %>% 
	select(V1,V2,topic, word, p) %>% 
	left_join(tidy_topics_all %>% filter(level == 0) %>% unnest(posts) %>% 
							select(topic, p2 = p, word2 = word), by = "topic") %>% 
	left_join(tidy_topics_all %>% filter(level == 1) %>% unnest(posts) %>%
							select(topic2 = topic, word2 = word), by = "word2") %>%
	left_join(classification, by = "topic2") %>% 
	filter(!str_detect(word2, "[0-9]")) %>% 
	mutate(topic2 = as.factor(topic2)) %>% 
	group_by(topic) %>% 
	mutate(p2 = p2/max(p2)*p) %>% 
	#mutate(alpha = ifelse(word ==word2, 1,1)) %>% 
	mutate(id = 1:n()) %>% 
	filter(id <= 3) %>% 
	ggplot(aes(V1, V2, label = word2, size = 10*p2^(1/0.7),color = descript)) + 
	geom_text_repel(show.legend = T,max.overlaps = 100,min.segment.length = 100,force_pull = 2) +   theme_void() + 
	theme(legend.position = "right") +
	scale_color_manual("Classification",values = c("Experience" = "#69B578","General"="#1B2F33","Symptoms" = "#D62246", "Treatment" = "#7D82B8", "Diagnosis" = "#FFA400"))+
	guides(size = "none") 
ggsave(p, filename = "Figures/Plots/umap_10.pdf",width = 9, height = 7)

t2 %>% 
	filter(level == 1) %>% 
	ungroup() %>% 
	unnest(mat) %>% 
	ggplot(aes(V1, V2, label = word, size = 10*p^(1/0.7))) + 
	geom_text_repel(show.legend = F) + 
	theme_void()

ggsave(p, filename = "Figures/Plots/mds_part.pdf", device = "pdf",
			 width = 10, height = 8)

p <- t2 %>% 
	filter(level == 0) %>% 
	ungroup() %>% 
	unnest(mat) %>% 
	select(level, V1, V2, topic, word, p) %>% 
	left_join(tidy_topics_all %>% 
							filter(level == 0) %>% 
							unnest(posts),
						by = "topic"
	) %>% 
	mutate(p = p.x*p.y) %>% 
	top_n(500,p) %>% 
	ggplot(aes(V1, V2, label = word.y, size = p^(1/0.7), color = topic %>% as.factor)) + 
	geom_text_repel(show.legend = F, max.overlaps = 300,segment.alpha = 0.2) + 
	theme_void()

ggsave(p, filename = "Figures/Plots/mds_full.pdf", device = "pdf",
			 width = 10, height = 8)


ordered_topics <- topic_tops %>% 
	filter(level == 1) %>% 
	arrange(topic) %>% 
	pull(word)

interaction_matrix <- full_interactions %>% 
	filter(level == 1) %>% 
	select(data) %>% 
	unnest(data) %>% 
	mutate(p = -p) %>% 
	pivot_wider(names_from = tw1, values_from = p) %>% 
	select(-tw) %>% 
	as.matrix()

rownames(interaction_matrix) <- ordered_topics
colnames(interaction_matrix) <- ordered_topics


p_edge <- interaction_matrix %>% rowSums()

p_double_edge <- p_edge%*%t(p_edge)

normalised_matrix <- interaction_matrix/p_double_edge 

normalised_matrix %>% 
	superheat(
		#heat.lim = c(0,45),
		pretty.order.rows = T,
		pretty.order.cols = T,
		row.dendrogram = T,
		clustering.method = "hierarchical",
		#left.label.text.col = my_colouring$colour,
		left.label.col = "white",
		left.label.text.alignment = "left",
		#bottom.label.text.col = my_colouring$colour,
		bottom.label.col = "white",
		bottom.label.text.angle = 90,
		bottom.label.text.alignment = "right",
		grid.hline.col = "white",
		grid.hline.size = 0.5,
		grid.vline.col = "white",
		grid.vline.size = 0.5,
		#yt.plot.type = "bar",
		#	yt = ordered_data$p,
		#yt.axis.name = "Overall Density",
		heat.pal = c("blue", "white", "red"),
		#left.label.text.size = 4,
		#bottom.label.text.size = 3.4,
		#yt.plot.size = 0.1
	)

topic_cors <- tidy_topic_docs_all %>% 
	filter(level ==1) %>% 
	unnest(posts) %>%
	left_join(topic_tops %>% ungroup() %>% filter(level == 1) %>% select(topic, word), by = "topic") %>% 
	select(topic, doc_ID, p, word) %>% 
	pivot_wider(doc_ID, names_from = topic, values_from = p) %>% 
	select(-doc_ID) %>% 
	cor() 
rownames(topic_cors) <- ordered_topics
colnames(topic_cors) <- ordered_topics
topic_cors %>% 
	superheat(
		#heat.lim = c(0,45),
		pretty.order.rows = T,
		pretty.order.cols = T,
		row.dendrogram = T,
		clustering.method = "hierarchical",
		#left.label.text.col = my_colouring$colour,
		left.label.col = "white",
		left.label.text.alignment = "left",
		#bottom.label.text.col = my_colouring$colour,
		bottom.label.col = "white",
		bottom.label.text.angle = 90,
		bottom.label.text.alignment = "right",
		grid.hline.col = "white",
		grid.hline.size = 0.5,
		grid.vline.col = "white",
		grid.vline.size = 0.5,
		#yt.plot.type = "bar",
		#	yt = ordered_data$p,
		#yt.axis.name = "Overall Density",
		heat.pal = c("blue", "white", "red"),
		#left.label.text.size = 4,
		#bottom.label.text.size = 3.4,
		#yt.plot.size = 0.1
	)


normalised_matrix %>% as_tibble(rownames = "Group") %>% 
	pivot_longer(-1, names_to = "Group2", values_to = "value") %>% 
	filter(Group != Group2) %>% 
	arrange(-value)
	
my_heatmap_path <- "Figures/Plots/Topic_Model/plot_heatmap.pdf"

#my_heatmap
#pdf(file=my_heatmap_path,width = 14, height = 11, onefile = F)
cors %>% 
	dev.off()

