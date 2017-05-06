# elm-multitier-examples
Examples using [multitier Elm](https://github.com/JeffHoremans/elm-multitier), an extension to Elm in order to make it a multi-tier language, made in the context of my Master's thesis.

### The examples

"src/" contains some small demo applications;

- ```RoomReservation/Main.elm``` very basic example that allows scheduling a conference room
- ```Chat/Main.elm``` basic hat application for that uses various features of our multi-tier framework

- ```RoomReservationDebug/Main.elm and ChatDebug/Main.elm``` definitions of the applications above in debug mode, running them will start the application with the debugger

## Try the examples

### Download [Node.js](https://nodejs.org) and [npm](https://www.npmjs.com)
npm is distributed with Node.js, so you download Node.js, you automatically get npm installed on your computer.

### Install [Elm](http://elm-lang.org/)
Install Elm by following the [installation guide](https://guide.elm-lang.org/install.html).

### Download the repository
    git clone --recursive https://github.com/JeffHoremans/elm-multitier-examples.git
    cd elm-multitier-examples

### Run an example
To build an example, run:

    elm-multitier-make src/[folder of example]/Main.elm

This will generate a couple of files in the current folder:

    server.js
    client.js
    state.js
    index.html

To run the example, execute the `server.js` file with Node:

    node server.js

This will start up the the server part of the multi-tier application which hosts the client part at [http://localhost:8081/](http://localhost:8081/)
