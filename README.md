Temple of ROM II
==========

![](images/newmap.jpg)

Starting from a disassembly of a Temple of ROM cart by William Astle, I've managed to build the game successfully.
I'm hoping to expand the game by adding new features:

* Expand level -- DONE
* More rooms, monsters and treasures -- DONE
* Different monsters -- DONE
* What else?  Dunno!

I'm hoping to have something available for sale eventually, but the primary goal of this project is to have *fun*!
Please do look over my shoulder as I work on this and feel free to contact me
via the #temple-of-rom-ii channel on [Discord](https://discord.gg/4J5nHXm) .

### Changes

Created project from Temple of ROM disassembly.

Cleaned up direct page.

Split into separate source files.

Deciphered maze, monster, portal and treasure data formats.

Added generation of arbitrary number of portals, monsters, treasures, walls.

Expanded to add more rooms (the maze is 45% larger!), monsters, treasures.

CLEAR, ENTER and SHIFT change video modes.  Coco 1/2 defaults to CMP, Coco 3 to RGB.

BREAK quits the game and exits to RSDOS.

Artifacting color comes up correctly on a Coco3 without need of a reset.

RGB on a Coco3 is now supported.

If you have a Boomerang E2 board, the LED will light briefly when you fire your laser.

Added two more types of monsters:

* Ghosts (slow but invisible till you aggro them)
* Skulls (fast, nasty little buggers)

Added ability to generate level data from a gif image:

* Use PSD file to alter/expand walls, add monsters and treasures.
* Walls have to be vertical/horizontal, on grid lines.
* "Objects" layer shows how to create monsters and treasures.
* White "hot spot" of each object needs to align on the grid as shown.
* "Hot spot" of monsters must have a wall to the left, right, above and below.
* Use optional yellow walls to denote monster aggro area.
* Include all layers.
* Save edited PSD as /map/map.gif, then _make_ to build new version.

Generated levels now will build memory tables sized to the number of monsters/treasures specified.
