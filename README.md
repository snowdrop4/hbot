hbot
====

About
-----
IRC bot written in Haskell.

Features
--------

#### Admin Commands (available from the terminal):

 * **help** `[command-name]` -- if no argument given, prints list of admin commands, else prints help for specific admin command
 * **msg** `<channel/user> <message>[...]` -- sends a message to the specified destination
 * **leave** `<channel>` -- leaves the specified channel
 * **join** `<channel>` -- joins the specified channel
 * **channels** -- lists currently joined channels
 * **nick** `<new-nickname>` -- changes the bot nickname
 * **kick** `<channel> <user> [<message>[...]]` -- kicks the user from the channel, if no message is given it defaults to `bot-username`
 * **ban** `<channel> <user> <ban-mask> [<message>[...]]` -- kickbans the user from the channel, if no message is given it defaults to `bot-username`
 * **mode** `<channel/user> <flags> <args>` -- sets the mode of the thing

#### User Commands (available to all connected to the same channel as hbot): 

 * **help** `[command-name]` -- if no argument given, prints list of user commands, else prints help for specific user command
 * **echo** `<argument>[...]` -- prints all given argument(s)
 * **dictionary-list** -- lists all dictionary entries
 * **dictionary-update** `<key> <definition>[...]` -- adds or updates a word and its definition in the dictionary
 * **dictionary-remove** `<key>` -- removes a word and its definition from the dictionary

#### Other things:

 * multiple channel support
 * simple config file
 * web page title displaying

User commands are prefixed with `!`, dictionary definition requests are prefixed with `@`, and admin commands are unprefixed.

License
-------

**MIT**

See `LICENSE.md` in the root directory for more information.
