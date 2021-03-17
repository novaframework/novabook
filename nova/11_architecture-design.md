## Architecture & Design ##

### Architecture ###

Nova is built on top of Cowboy that is a web server written in Erlang. One of the reason was that it is a well tested web server, that have great features.

In the beginning we used Cowboy routing but later we did see that we wanted to handle appliction modilarity. That is for us that you can include other Nova applications into your application.



### Design ###

One of the things that we wanted with Nova is that it should make it easier to start a new web application projet in Erlang. Without spending much time on boilerplate Cowboy for your project, or write your own wrapper around it.

The other thing is that what is core features in Nova should be small and we should not use that many dependencies. When we discuss new things we usually ask if this is something that extends the core Nova or if it is something that builds on top of Nova.

This is also one of thea reason for plugin system, that helps us to leave to other to write how they want to handle requests. Instead of building everything in Nova.

---
