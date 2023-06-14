visualise_topics: analyse_topic_locations:
	R CMD BATCH R/visualise_topics
analyse_topic_locations: analyse_topic_interactions
	R CMD BATCH R/analyse_topic_locations.R
analyse_topic_interactions: preprocess
	R CMD BATCH R/analyse_topic_interactions.R
analyse_topics: preprocess
	R CMD BATCH R/analyse_topics.R
model: preprocess
	python Python/model_with_hSBM.py
preprocess: acquire
	R CMD BATCH R/preprocess.R
acquire:
	python Python/scrape_with_praw.py
