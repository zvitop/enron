# enron

This repo contains code in R that takes as input a modified enron dataset with the following columns:

1. time -  Unix time in milliseconds

2. message identifier

3. sender

4. recipients - pipe-separated list of email recipients

5. mode - always "email"

And produces:

1. A .csv file with three columns---"person", "sent", "received"---where the
   final two columns contain the number of emails that person sent or received
   in the dataset. This file is sorted by the number of emails sent.

2. A PNG image visualizing the number of emails sent over time by some of the
   most prolific senders in (1).

3. A visualization that shows, for the people in (1), the number of unique
   people/email addresses who contacted them over the same time period. 

The code can be run by invoking the following from the commandline:
Rscript summarize-enron.R enron-event-history-all.csv
