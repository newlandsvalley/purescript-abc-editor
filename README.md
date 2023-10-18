purescript-abc-editor
=====================

This is an editor for musical scores written in the ABC notation. It allows you to edit ABC text and to save or reload it.  If the ABC is valid, you can see the score and play the tune, otherwise it displays an error.  You can also shift the octave, transpose or alter the tempo.

Try it [here](http://www.tradtunedb.org.uk/#/editor).

The melody is generated using purescript-abc-melody.

The editor works best with VexFlow v4.0.2. Unfortunately, at the time of writing, the current VexFlow release (v4.2.3) is unusable because the score is rescaled after every keystroke!

Building
--------

from the current directory:

    $ npm run build

Building the example
--------------------

from the current directory:

    $ npm run example   

Then navigate to example/dist/index.html.
