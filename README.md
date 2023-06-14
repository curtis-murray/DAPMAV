# r/ProstateCancer Case Study

This repository contains the code used in the paper [link here].

## Dependencies

### Python Dependencies
- pandas
- graph-tool

  Detailed instructions on how to install graph-tool can be found [here](https://git.skewed.de/count0/graph-tool/-/wikis/installation-instructions#homebrew).

- praw

  PRAW is used to scrape Reddit. To use PRAW, you will need to register a Reddit App [here](https://ssl.reddit.com/prefs/apps/). Once you register, you will see on the webpage a client secret, and you will be emailed a client ID. You can use these, along with a name you choose, as the `user_agent` in the code below.

  ```python
  import praw
  
  reddit = praw.Reddit(
      client_id="my client id",
      client_secret="my client secret",
      user_agent="my user agent",
  )
  ```
  
### R Dependencies
- tidyverse
- lubridate

## Workflow
This code can be run using the makefile.
Open a terminal and enter `make` to conduct the analysis