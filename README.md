# DISTRESS: DISTRibutEd Secure file System #

For our graduate project in Secure Data Management at the Rochester Institute
of Technology, we proposed a possible design for a secure collaborative 
file storage system. Our system uses strict client side encryption and a 
receipt based authentication system. This lets DISTRESS lend itself to a 
diverse range of applications built on top of it, such as: an anonymous 
dead-drop tool like the New Yorker's StrongBox or a friend-to-friend backup
service like CrashPlan.

## Requirements ##

Right now we have a limited set of requirements on purpose:

* Erlang VM (version R15B01 or higher if you are building a release)
* GNUMake or CMake

However DISTRESS is headless and will need a client to connect with it. We have
a python client tested on python 2.7.3.

## Building ##

Just run `make` as long as Erlang is on the path it should work. To run the 
DISTRESS server without having to build a release, type the following:

* `./distressd`

This is helpful for debugging the distress system before building the release
for installation. 

NOTE: if you are running an older version of Erlang, then there may be errors,
you can run `make oldcompile` in the `server` directory. This means you will be
unable to build a release, but you should still be able to test it out. 

## Testing-out DISTRESS ##

After building and running the server as mentioned above, you can then use the 
client to connect to it by running:

* `cd cli`
* and then `./distress -h` to see the help menu.

The DISTRESS client works like git, in that it has a bunch of subcommands
each with their own options and parameters. Try adding a file:

* `./distress add ../README.md`

And then pulling it out again:

* `./distress get 1`

Note that we used the file ID of the file, which can be found:

* `./distress list --verbose` 

We can then verify our file by either doing a diff or checking it against the 
receipt:

* `diff README.md ../README.md`
* `./distress verify 1 README.md`


## Installing ##
Note that this is not a necessary step if you just wish to mess around with 
the DISTRESS system. But deploying DISTRESS is simple. Running `make release` 
will generate a `rel` directory which will contain everything you need to run 
DISTRESS, including a stripped down version of the Erlang VM. You can now 
zip/tar it up to distribute, however just linking to the binary should be enough 
to run on the current platform:

```
make release
ln -s $(pwd)/rel/distress/bin/distress ~/bin/distress
```

Running the release is similar to a standard init.d service (with some added
features:

```
~/bin/distress
Usage: distress {start|stop|restart|reboot|ping|console|attach}
```

Once started you can `attach` to the running service and treat it like an
Erlang shell to send commands to it.

