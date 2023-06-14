library(tidyverse)
library(ggwordcloud)
library(knitr)
library(kableExtra)
library(tidytext)

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
	group_by(Sub) %>% 
	summarise() %>% 
	drop_na() %>% 
	pull(Sub)

params <- tibble(my_word = c("incontinence","surgery", "gleason", "diagnosed", "pain", "prostate","sex", "radiation", "biopsy", "prostatectomy"),
								 my_level = c(1,1,1,1,1,1,1,1,1,1) %>% as.character()) %>% 
	mutate(my_topics =
				 	map2(.x = my_word, .y = my_level, .f = ~topic_from_word(.x, .y))
	) %>% 
	unnest(my_topics)

plot_list <- list()
for(i in 1:max(tidy_topics_all %>% filter(level ==0) %>% unnest(posts) %>% pull(topic))){
	
	try({
		p <- tidy_topics_all %>%
			filter(level == 0) %>% 
			unnest(posts) %>% 
			filter(topic == i) %>% 
			select(topic, p, word) %>% 
			filter(!str_detect(word, "[0-9]")) %>% 
			top_n(150,p) %>% 
			mutate(p = p/max(p)) %>%
			ungroup() %>% 
			{. ->> tmp_save} %>% 
			ggplot(
				aes(
					label = word, size = p,
					color = ifelse(p == max(p), 1,0))
			) +
			geom_text_wordcloud_area(rm_outside = T,grid_size = 20) +
			scale_size_area(max_size = 20) +
			theme_minimal()
		
		max_word <- tmp_save %>% arrange(-p) %>% mutate(id = 1:n()) %>% filter(id <= 3) %>% pull(word) %>% paste(collapse = ", ")
		ggsave(p, 
					 filename = paste("Figures/Plots/Wordclouds/wc_all/level_0/",max_word,".pdf", sep = ""),
					 width = 8, 
					 height = 8
		)
	})
}

for(i in 1:nrow(params)){
	
	my_word <- params$my_word[i]
	my_level <- params$my_level[i]
	my_topic <- params$my_topics[i]
	
	p <- tidy_topics_all %>%
		filter(level == my_level) %>% 
		unnest(posts) %>% 
		filter(topic == my_topic) %>% 
		select(topic, p, word) %>% 
		mutate(p2 = ifelse(word == my_word, Inf, p),
					 is_my_word = ifelse(word == my_word, 1, 0)) %>% 
		top_n(150,p2) %>% 
		mutate(p = p/max(p)) %>% 
		ungroup() %>% 
		ggplot(
			aes(
				label = word, size = p,
				color = is_my_word
			)
		) +
		geom_text_wordcloud_area(rm_outside = T,grid_size = 20) +
		scale_size_area(max_size = 20) +
		theme_minimal()
	
	fname <- paste("Figures/Plots/Wordclouds/", my_word, "_", my_level, ".pdf", sep = "")
	ggsave(p, filename = fname, device = "pdf",
				 height = 6, width = 8)
}

topic_top_words <- tidy_topics_all %>% 
	unnest(posts) %>% 
	group_by(topic, level) %>% 
	filter(!str_detect(word, "[0-9]")) %>% 
	filter(p == max(p)) %>% 
	select(topic, level, word)

topic_dens <- tidy_topic_docs_all %>% 
	unnest(posts) %>% 
	group_by(topic, level) %>% 
	summarise(p = mean(p))

topic_tops <- full_join(topic_top_words, topic_dens, by = c("topic", "level")) %>% 
	ungroup()

plot_dens <- 
	tidy_topic_docs_all %>%
	inner_join(params, by = c("level" = "my_level")) %>% 
	unnest(posts) %>% 
	filter(topic == my_topics) %>% 
	select(my_word, topic, Date, p) %>%
	group_by(my_word) %>% 
	summarise(p = mean(p))

p <- 
	tidy_topic_docs_all %>%
	full_join(params, by = c("level" = "my_level")) %>% 
	unnest(posts) %>% 
	filter(topic == my_topics) %>% 
	select(my_word, topic, Date, p) %>%
	group_by(my_word) %>% 
	summarise(p = mean(p)) %>% 
	arrange(-p) %>% 
	mutate(Figure = paste("\\ref{fig:", my_word,"}", sep="")) %>% 
	select("Topic identifier" = my_word, Figure, "Usage density" = p) %>% 
	kable("latex", booktabs = T, align = "l", digits = 3, escape = F)

p <- tidy_topic_docs_all %>%
	full_join(params, by = c("level" = "my_level")) %>% 
	unnest(posts) %>% 
	filter(topic == my_topics) %>% 
	select(my_word, topic, Date, p) %>%
	group_by(my_word) %>% 
	summarise(p = mean(p)) %>% 
	arrange(-p) %>% 
	ggplot(aes(reorder(my_word,p), p)) + 
	geom_histogram(stat = "identity") + 
	coord_flip() + 
	theme_minimal() + 
	labs(x = "Topic identifier", 
			 y = "Topic use proportion")

tidy_topic_docs_all %>% 
	unnest(posts) %>% 
	left_join(topic_top_words, by = c("level","topic")) %>% 
	filter(level == 1) %>% 
	select(word, topic, Date, p) %>%
	group_by(word) %>% 
	summarise(p = mean(p)) %>% 
	arrange(-p) %>% 
	ggplot(aes(reorder(word,p), p)) + 
	geom_histogram(stat = "identity") + 
	coord_flip() + 
	theme_minimal()

p <- read_csv("data/Scrape/clean_posts.csv") %>% 
	select(Content) %>% 
	unnest_tokens(word, Content) %>% 
	group_by(word) %>% 
	summarise(count = n()) %>% 
	top_n(100, count) %>% 
	ggplot(
		aes(
			label = word, size = count
		)
	) +
	geom_text_wordcloud_area(rm_outside = T,grid_size = 20) +
	scale_size_area(max_size = 20) +
	theme_minimal()

ggsave(p, filename = "Figures/Plots/Wordclouds/global.pdf", device = "pdf",
			 height = 6, width = 8)

trans_posts <- read_csv("data/Scrape/clean_posts.csv") %>% 
	unnest_tokens(word, Content) %>% 
	filter(word == "woman") %>% 
	group_by(Post_ID) %>% 
	summarise() %>% 
	pull(Post_ID)

read_csv("data/Scrape/clean_posts.csv") %>% 
	filter(Post_ID %in% trans_posts) %>% 
	pull(Content)

get_wc_and_recurse <- function(this_level, this_topic, path){
	
	try({
		p <- tidy_topics_all %>%
			filter(level == this_level) %>% 
			unnest(posts) %>% 
			filter(topic == this_topic) %>% 
			select(topic, p, word) %>% 
			filter(!str_detect(word, "[0-9]")) %>% 
			top_n(150,p) %>% 
			mutate(p = p/max(p)) %>%
			ungroup() %>% 
			{. ->> tmp_save} %>% 
			ggplot(
				aes(
					label = word, size = p,
					color = ifelse(p == max(p), 1,0))
			) +
			geom_text_wordcloud_area(rm_outside = T,grid_size = 20) +
			scale_size_area(max_size = 20) +
			theme_minimal()
		
		if(nrow(tmp_save) == 0){
			return
		}
		
		max_word <- tmp_save %>% arrange(-p) %>% mutate(id = 1:n()) %>% filter(id <= 3) %>% pull(word) %>% paste(collapse = ", ")
		
		path <- paste(path,max_word, sep = "/")
		dir.create(path)
		
		ggsave(p, 
					 filename = paste(path, "/",max_word, ".pdf",sep = ""),
					 width = 8, 
					 height = 8
		)
		
	})
	
	if(level == 0){
		return
	}
	try({
		sub_topics <- tidy_topics_all %>% 
			filter(level == (this_level-1)) %>% 
			unnest(posts) %>% 
			right_join(tmp_save %>% select(word), by = "word") %>% 
			pull(topic) %>% 
			unique()
		
		
		for(sub_topic in sub_topics){
			get_wc_and_recurse(this_level = this_level-1, 
												 this_topic = sub_topic,
												 path = path)
		}
	})
}

get_wc_and_recurse(max_level, 
									 1, 
									 "Figures/Plots/Wordclouds/topic_structure")


p_td_d <- list.files("data/Topic_Model", pattern = "p_td_d_level_", full.names = T, include.dirs = T, all.files = T) %>% 
	as_tibble() %>% 
	mutate(p_td_d = map(value, ~read_csv(.x, col_names = T) %>% mutate(td_id = 1:n()) %>% select(td_id, everything()))) %>% 
	mutate(td_level = str_extract(value, "\\d")) %>% 
	select(-value)

tidy_doc_topics_all <- p_td_d %>% unnest(p_td_d) %>% 
	select(td_level, everything()) %>% 
	pivot_longer(-(1:2), names_to = "doc_id", values_to = "is_topic") %>% 
	transmute(td_level, td_id = td_id + 1, doc_ID = as.numeric(doc_id) + 1, is_topic) %>% 
	filter(is_topic==1) %>% 
	select(-is_topic)

tidy_topic_docs_more <- tidy_topic_docs_all %>% 
	unnest(posts) %>% 
	select(level, everything()) %>% 
	left_join(tidy_doc_topics_all, by = c("doc_ID"))



generate_table <- function(input_words, n_words){
	topics_of_interest <- tidy_topics_all %>%
		unnest(posts) %>% 
		filter(level == 1, word %in% input_words) %>% 
		select(level, topic, word)
	
	sex_docs <- tidy_topic_docs_more %>% 
		group_by(td_id, td_level, topic, level) %>% 
		summarise(p = mean(p), count = n()) %>% 
		right_join(topics_of_interest, by = c("level", "topic")) %>% 
		group_by(td_level, topic, word) %>% 
		filter(p == max(p))
	
	sex_docs %>% 
		left_join(tidy_topic_docs_more %>% group_by(doc, td_level, td_id) %>% summarise(), by = c("td_level", "td_id")) %>% 
		left_join(read_csv("data/Scrape/Subs/ProstateCancer.csv"), by = c("doc" = "Post ID")) %>% 
		select(word, Title, td_id, td_level,Content) %>% 
		DT::datatable()
	
	tidy_topic_docs_more %>% 
		right_join(topics_of_interest, by = c("level", "topic")) %>% 
		group_by(doc, doc_ID, word, p, topic) %>% 
		summarise() %>% 
		group_by(doc, doc_ID) %>% 
		summarise(p = sum(p)) %>%
		ungroup() %>% 
		arrange(-p) %>% 
		left_join(read_csv("data/Scrape/Subs/ProstateCancer.csv"), by = c("doc" = "Post ID")) %>% 
		select(Title, Content, p) %>% 
		filter(!is.na(Title)) %>% 
		top_n(n_words, p)
}


generate_table(c("sex", "ed"),5)
generate_table(c("gleason"), 5) %>% DT::datatable()

types <- c("Diagnosis", "Diagnosis", "Experience", "Experience", "Symptoms", "Symptoms", "Treatment", "Treatment")

stories <- c("My dad's pathology report says `L0' (no spread to lymph node diagnosis) but then says number of lymph nodes examined: 1. Does this make any sense? From what I am reading, the estimated number of lymph nodes necessary for optimal staging accuracy ranges between 20 and 28. Why would the doctor just look at 1? It seems like this provides no accurate diagnosis information.",
"My father has been battling aggressive prostate cancer for 6 months now. All cores came back gleason 8-9. Bone scan and CT scan came back negative. He started hormone therapy and had his prostate removed and pelvic lymph nodes came back positive for cancer. Followup bone scan and CT showed no spread and his PSA is way down now. The thing that bothers me is he has seen something like 5 urologists and several oncologists and not one of them has recommended a PET scan. He even asked about it and they told him PET scans don't reveal what they are looking for. I'm just puzzled by this response. He lives in a big city in the US.",
"Hi all, my dad is 64 and has had a radical prostatectomy (I think that’s what it’s called?) and he’s going through a divorce. Really tough stuff. Are there any books or support groups you recommend? He’s struggling with self esteem and self image issues. He’s got an appointment to speak with a mental health professional soon, but I’m wondering how can I best support him? I was thinking of ordering him any good books geared toward survivors? Hugs for all of you fighting the battle too.",
"So, my father (60) is going under prostatectomy in two weeks, and even if i am feeling hopeful that he will recover from the cancer and from the procedure and go back to living his life, i would like to know what can i do to ensure that he is going to recover properly. Do you have any idea what i should prepare for? Also any idea on average recovery time for this kind of procedure? Thank you.",
"Right. So I have had deep vein thrombosis and pulmonary embolism, so I have to wear over-the-knee compression stockings at all times, even in bed. And now I’m’gonna have my prostate removed, so I’ll have ED and dry orgasms as well and won’t be able to satisfy my wife. So I might as well just go ahead and have the sex change while I’m at it, too, right? Sigh.....",
"As I go to urinate a ball that’s small moves up to my urethra it’s hard it doesn’t hurt once I’m done peeing it disappears the second I urinate it comes back up and stops could this be a kidney stone stuck ? Could that be my reason for the weak urine stream with dribbles , constant urination , painful urination ? My urine culture was clean and this ball comes and goes only when I urinate it’s under my shaft midway A tumor would be there always and not move , Im 26 male .",
"Today, 10 Nov 2020, I finished my course of radiation treatment. Still got a few years of hormone suppression to go but happy and relieved.",
"My dad has had prostate cancer since I’ve been in 3rd grade. I’m 21 now and he’s had 2 radiation treatments and his prostate removed as well as a cyber knife treatment. He recently just told me it came back (he’s 68 now) They caught it very early and i’m very scared it’s going to be fatal. They want to do lupron, and I read reviews that weren’t the best. Should I be concerned? What advice do you guys have")
p <- c(0.54, 0.53, 0.62, 0.45, 0.8, 0.77, 0.6, 0.6)
tibble(Topic = types, Story = stories, `Topic density` = p) %>% 
	mutate_all(linebreak) %>% 
	kbl(booktabs = T,format ="latex",escape = FALSE) %>% 
	column_spec(1, width = "8cm") %>% 
	kable_styling()




res <- classification_0 %>% 
	group_by(descript) %>% 
	summarise(words = list(word2)) %>% 
	mutate(tab = map(words, ~generate_table(.x,10))) %>% 
	unnest(tab) 

read_csv("data/Scrape/clean_posts.csv") %>% 
	ungroup() %>% 
	unnest_tokens(word, Content) %>% 
	group_by(Post_ID) %>% 
	summarise(count = n()) %>% 
	summarise(mean = mean(count))
