To continue with this, look for senders of #jmvVer2. Please forgive the comments in Spanish... Or better complain to me!

To do:

Delete #morphPosition: and morphPosition, as they use global coordinates. Replace preferably by #morphPositionInOwner and #morphPositionInOwner. If not possible, use #morphPositionInWorld and #morphPositionInWorld:, but there must be a very good reason for doing so. We don't want absolute coordinates! (BTW, these last 4 selectors don't need the #morph prefix, as there is no false polymorphism). Keep in mind that converting to local coordinates (in owner space) requires having the owner properly set, and the programmer needs to know who the owner is.

Reduce use of 'bounds' instance variables. Prefer 'position' (in owner) and 'extent'. extent is in own coordinates, not in owner. Right now this makes no difference, because there is no 'scale' ivar yet.

Other things to look for:
- senders of #revisar , #jmvVer, #jmv, #revisarM3.
- Also senders of #printStack: and methods that contain 'printStack:' or 'zzposition ' in the source code (maybe in comments)

-----------------------
Known bugs:
Duplicating a window with the halo moves the close/minimize/maximize/menu buttons. This is because of doing #privateFullMoveBy: when the owner changes... All this needs to be cleaned up when we stop using bounds, but only coordinates relative to the owner!