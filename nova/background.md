# Background #

## Erlang web frameworks ##

Both me and Niclas have used many different web frameworks in Erlang. We started to code Erlang around 2009 and at that time we had frameworks like Nitrogen, Erlang Web and Zotonic. Some years later Chicago Boss was introduced that tried to solve many things. But one of the things with Chicago Boss was that it didn't really follow OTP standard when it came to release handling.

In 2011 Loïc Hoguin released Cowboy, an Erlang web server. It was very fast and stable web server that got popular in the Erlang community.

Many Erlang companies started to use Cowboy and everytime me or Niclas came in contact with it we needed to start writing all the handlers, or work with a wrapper around.

## Nova ##

In a hotel room in Gratz we started to see on some of our projects we have started during all the years. Niclas had earlier started a framework called Burbweb. Some years before that we had worked on a ORM library in Erlang. We got an idea that we should try to make a product of this and really try to aim for a new good framework that people can use.

We started to look into how we can help users to get started easy and also try to make easy release builds.

So in 2019 we released Nova that we now have worked on and try to make it better.