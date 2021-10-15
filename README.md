# UrlShortener
## Overview
A rather basic stack project. Can be built with `stack build` and tests can be run with `stack test`

The tests are a combo of property tests where possible and a simple spec where it got too complicated (see the note on the ServerSpec).

## Structure
![Design](Design.png)

The Server package contains everything to do with the rest server. It calls out the the Application package where the business logic can be found. 

The UrlShortener, which is the only module in the Application package currently, calls out to the Database package and does the
calculations from and to a shortened url, taking ito account potential duplicates.

The Database package deals with the Sqlite database.

## The shortening logic
For shortening I most likely could have used existing base64 packages with the row id as input.
However, I noticed that most of the tinyUrls limit themselves to uppercase/lowercase, and sometimes numbers.
Therefore, I created my own base62 implementation. 
The base64 would likely have been correct too and would have meant less code but I felt it stretched the requirements somewhat.

## Live testing
`stack run` will set up the server, runShorten.sh and runResolve.sh can be used to launch something at the server for testing. Uses curl under the hood.

## Known issues
In the current implementation, as soon as 2^64 urls have been shortened the database won't be able to cope with a new entry anymore.

With the sqlite optimistic locking approach we could hit 500s when too many people are hitting the server at the same time, or hitting the database more precisely.

We do not really check whether the url that is being abbreviated is a valid one. The main problem is that
the only way we can really do that is by making a call to that url. Lower hanging fruit could be just
validating the format of the input text but it still would not protect us against somebody making a very basic mistake such as 
entering `gogol.com` for shortening so I decided it was not really worth the effort.

