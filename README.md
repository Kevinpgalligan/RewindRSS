### Description
Create custom RSS feeds and run them as a local server. Customisable via Common Lisp.

### Dependencies
* hunchentoot
* cl-who
* drakma
* lquery
* alexandria

### Usage
So far, just running it through the REPL:

```
(hunchentoot:start (make-instance 'hunchentoot:easy-acceptor :port 4242))
```

### TODO
* Create code to tie it all together:
   - On start-up, load files in config directory. Allows user to define new feeds. And configure
     the port. And URL?
   - Don't cache the feed itself, as that can change depending on user config.
   - Well, maybe cache it just to compare the output, then if it has changed u can go back
     and increase the ID of the feed so that feed readers reload it?
   - On receiving a request: load cached index / info (if it exists... if
     not, load from scratch); if a new post is due, parse it & cache it; return the feed.
   - Need a way to configure each of the feeds (active/inactive, which days of week, weekly or fortnightly or what have you).
   - Can it be made platform independent somehow?
* Can we somehow link the config for each feed & the code that's used to parse it?
* Allow configuring an icon.
* Error handling.
* Force rate limiting to each site so that we don't spam.
* Customise HTTP request, should maybe indicate that it's us.

### References
https://stackoverflow.com/questions/123793/design-question-how-would-you-design-a-recurring-event-system

### Design
Main loop:
* Load config (schedule, start date, how many items to keep)
* If no index, create it.
* Load in the index.
* Load any posts that are due.
