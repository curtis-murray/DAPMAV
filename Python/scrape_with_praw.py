import praw
import pandas as pd
import datetime
import os

# Initialize PRAW with your Reddit App credentials
reddit = praw.Reddit(
    client_id=YOUR_CLIENT_ID,
    client_secret=YOUR_CLIENT_SECRET,
    user_agent=YOUR_USER_AGENT
)

def scrape_sub(subreddit_name):
    
    # Get the subreddit object
    subreddit = reddit.subreddit(subreddit_name)
    
    # List to store the data rows
    data_rows = []
    
    # Loop through the submissions
    i = 0
    for submission in subreddit.new(limit=None):
        i = i+1
        print(f"Scraping submission: {i}")
        created = datetime.datetime.fromtimestamp(submission.created_utc)
        
        # Extracting data
        title = submission.title
        url = submission.url
        flair = submission.link_flair_text if submission.link_flair_text else 'NA'
        author = submission.author.name if submission.author else 'NA'
        sub_id = submission.id
        score = submission.score
        selftext = submission.selftext if submission.selftext else 'NA'
        num_comms = submission.num_comments
        permalink = submission.permalink
        link_id = submission.id
        parent_id = 'NA'
        
        # Comments data
        comments_data = []
        submission.comments.replace_more(limit=None)
        j = 0
        for comment in submission.comments.list():
            j = j+1
            print(f"Scraping comment: {j} of submission {i}")
            author = comment.author.name if comment.author else 'NA'
            comment_id = comment.id
            comment_score = comment.score
            comment_body = comment.body
            comment_created = datetime.datetime.fromtimestamp(comment.created_utc)
            comment_permalink = comment.permalink
            link_id = comment.link_id
            parent_id = comment.parent_id
            
            # Append the extracted data as a dictionary to data_rows
            data_rows.append({
                'Post ID': comment_id,
                'Title': 'NA',
                'Url': 'NA',
                'Author': author,
                'Score': comment_score,
                'Publish Date': comment_created,
                'Total No. of Comments': 'NA',
                'Permalink': comment_permalink,
                'Flair': 'NA',
                'Content': comment_body,
                'link_id': link_id,
                'parent_id': parent_id
            })
        
        # Append the extracted data as a dictionary to data_rows
        data_rows.append({
            'Post ID': sub_id,
            'Title': title,
            'Url': url,
            'Author': author,
            'Score': score,
            'Publish Date': created,
            'Total No. of Comments': num_comms,
            'Permalink': permalink,
            'Flair': flair,
            'Content': selftext,
            'link_id': link_id,
            'parent_id': parent_id
        })

    # Creating a DataFrame
    df = pd.DataFrame(data_rows)
    
    # Saving data to CSV file
    location = "data/Scrape/Subs"
    f_name = os.path.join(location, f"{subreddit_name}.csv")
    df.to_csv(f_name, index=False, encoding='utf-8')

# Specify the subreddit, start date and end date
subreddit_name = 'ProstateCancer'

# Start scraping
scrape_sub(subreddit_name)
