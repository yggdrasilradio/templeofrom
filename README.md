Temple of ROM
==========

Starting from a disassembly of a Temple of ROM cart by William Astle, I've managed to build the game successfully.
I'm hoping to expand the game by adding new features:

* Expand level?
* More monsters?
* Different monsters?
* Doors and keys?
* Level editor?

I'm hoping to maybe have something available for sale eventually.

### Changes

Created project from Temple of ROM disassembly.

Cleaned up direct page.

Split into separate source files.

Deciphered maze, monster, portal and treasure data formats.

Added one more spider as a test.

Added utility to generate level data from a gif image

	* Use PSD file to alter/expand walls, add monsters and treasures
	* Walls have to be vertical/horizontal, on grid lines
	* "Objects" layer shows how to create monsters and treasures
	* White "hot spot" of each object needs to align on the grid as shown
	* Save edited PSD as map.gif, then _make custom_ to build new version
