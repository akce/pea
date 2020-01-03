# Welcome to Play 'Em All

Play 'Em All (PEA) is a low-resource media player whose goals include:
- support all kinds of media including shoutcast-style radio stations
- play media and browse playlists without lag on a raspberry pi B+ (minimum)
- implementation in a higher level language that provides easy "hackability" and is reasonably efficient
- experiment with rich playlists

## Features

Client / server model.
- Any number of clients can connect to, and control the server.

Defines a new rich (and highly experimental) custom playlist format.

Reads existing playlist formats:
- pls
- m3u
- cue

Pluggable drivers with included drivers for:
- video:
  - omxplayer
  - libmpv

- audio:
  - libmpv

### Custom playlist format

The PEA custom playlist format is an s-expr based mini-DSL that allows great control over media playing.

Features include:
- start at offset (within media file)
- stop at offset (within media file)
- Complex actions
  - loop
  - repeat count
  - pause
  - queue
  - load playlist file
  - tree-like list structure
- decisions based on 
  - track tag data (artist, title, year etc)
  - number of songs played in current list / radio station
