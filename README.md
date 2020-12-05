purescript-abc-editor
=====================

This is an editor for musical scores written in the ABC notation. It allows you to edit ABC text and to save or reload it.  If the ABC is valid, you can see the score and play the tune, otherwise it displays an error.  You can also shift the octave, transpose or alter the tempo.

Try it [here](http://www.tradtunedb.org.uk/#/editor).

Built with Purescript 0.13.8 and Halogen 5.  The melody is generated via MIDI using purescript-abc-melody.

Building
--------

from the current directory:

    $ npm run build

Building the example
--------------------

from the current directory:

    $ npm run example   

Then navigate to example/dist/index.html.
