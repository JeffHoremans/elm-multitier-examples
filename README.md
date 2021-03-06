# elm-multitier-examples
Examples showing the use of [multitier Elm](https://github.com/JeffHoremans/elm-multitier), an extension to Elm in order to make it a multi-tier language, made in the context of my Master's thesis.
Additionally, each example can be also run in debug mode with my [multitier Elm Debugger](https://github.com/JeffHoremans/elm-multitier).

### The examples

"src/" contains some small demo applications;

- ```RoomReservation/Main.elm``` very basic example that allows scheduling a conference room
- ```Chat/Main.elm``` basic hat application for that uses various features of our multi-tier framework

- ```RoomReservationDebug/Main.elm and ChatDebug/Main.elm``` definitions of the applications above in debug mode, running them will start the application with the debugger

The "src/Haste" folder contains multi-tier Elm versions of example [Haste](https://github.com/valderman/haste-compiler/tree/master/examples) applications. Haste is a similar multi-tier framework, the examples were implemented to evaluate how our framework compares to Haste in usage and lines of code. These examples can obviously be run as well:
- ```Haste/Chatbox/Main.elm``` a simple chatbox application
- ```Haste/HasteApp/Main.elm``` a "message trader"

## Try the examples

### Download [Node.js](https://nodejs.org) and [npm](https://www.npmjs.com)
npm is distributed with Node.js, so you download Node.js, you automatically get npm installed on your computer.

### Install [Elm](http://elm-lang.org/)
Install Elm by following the [installation guide](https://guide.elm-lang.org/install.html).
We used Elm version 0.18 for our implementation.

### Download the repository
    git clone --recursive https://github.com/JeffHoremans/elm-multitier-examples.git
    cd elm-multitier-examples

### Install our build tool [elm-multitier-make](https://github.com/JeffHoremans/elm-multitier-make.git)

    npm install elm-multitier-make

### Install the [websocket](https://www.npmjs.com/package/websocket) npm package
We used this package to support server-side WebSocket functionality in our multi-tier Elm framework.
To install it, run

    npm install websocket​

### Run an example
To build an example, run:

    $(npm bin)/elm-multitier-make src/[folder of example]/Main.elm

This will generate a couple of files in the current folder:

    server.js
    client.js
    state.js
    index.html

To run the example, execute the `server.js` file with Node:

    node server.js

This will start up the the server part of the multi-tier application which hosts the client part at [http://localhost:8081/](http://localhost:8081/)
