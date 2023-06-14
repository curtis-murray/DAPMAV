import os
import re
from itertools import chain
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from Python.sbmtm import sbmtm
import graph_tool.all as gt
import warnings

DATA_DIR = "data/Topic_Model/"

def load_data(data_file):
    # Load data from CSV file
    data = pd.read_csv(data_file)
    texts = data["Content"].values.tolist()
    titles = data["Post_ID"].values.tolist()
    texts = [str(content).split() for content in texts]
    return texts, titles

def run_topic_model(texts, titles, max_iterations=10):
    # Run the topic modeling algorithm
    i = 0
    while True:
        print(f"Running model. Attempt: {i+1}")
        model = sbmtm()
        model.make_graph(texts, documents=titles)
        model.fit()
        topics = model.topics(l=0, n=10)
        if len(topics) > 1 or i >= max_iterations:
            break
        i += 1
    return model

def save_results(model, level):
    # Save the results of the topic model for a specific level
    variables = {
        "p_tw_w": model.get_groups(l=level)['p_tw_w'],
        "p_td_d": model.get_groups(l=level)['p_td_d'],
        "p_w_tw": model.get_groups(l=level)['p_w_tw'],
        "p_tw_d": model.get_groups(l=level)['p_tw_d'],
        "p_td_tw": model.group_to_group_mixture(l=level)
    }

    for var_name, var_data in variables.items():
        filename = f"{DATA_DIR}{var_name}_level_{level}.csv"
        pd.DataFrame(var_data).to_csv(filename, index=False)

def save_model_data(model):
    # Save the model's word and document data
    pd.DataFrame(model.words).to_csv(f"{DATA_DIR}words_all.csv", index=False)
    pd.DataFrame(model.documents).to_csv(f"{DATA_DIR}documents_all.csv", index=False)

def save_max_level(model, filename):
    # Save the maximum level of the topic model
    with open(filename, "w") as file:
        file.write(str(model.L))


def override_old_files(dir):
    for file in os.listdir(dir):
        if file.endswith('.csv'):
            os.remove(os.path.join(dir, file))

def main():
    # Main function to execute the topic modeling process
    data_file = "data/Scrape/clean_posts.csv"
    texts, titles = load_data(data_file)
    while True:
        with warnings.catch_warnings():
            warnings.filterwarnings("error")  # Convert warnings to exceptions
            try:
                model = run_topic_model(texts, titles)
                override_old_files(DATA_DIR)
                override_old_files(f"{DATA_DIR}Clean")    
                for level in range(0, model.L+1):
                    print(f"Saving results for level {level} of {model.L}")
                    save_results(model, level)
                save_model_data(model)
                save_max_level(model, f"{DATA_DIR}max_level.csv")
                print("Complete")
                break  
            except RuntimeWarning as warning:
                print("Caught a RuntimeWarning:", str(warning))

if __name__ == "__main__":
    main()
