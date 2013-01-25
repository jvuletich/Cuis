The following is a list of github repositories with packages for the Cuis Smalltalk system.

https://github.com/jvuletich/Cuis 

website: http://www.jvuletich.org/Cuis/Index.html

Please update the list as more packages and tests become available (send a note to the Cuis mailing list).




List of packages which load fine into Cuis 4.1
-----------------------------------------------

* https://github.com/pmon/Cuis-PetitParser
 PetitParser for Cuis.
 All tests pass.
 
*  https://github.com/garduino/Cuis-CompatibilityWithOtherSmalltalks
 Needed for load other packages ported from Pharo / Squeak.
 
*  https://github.com/garduino/Cuis-RegEx
 Regular Expressions for Cuis.
 All tests pass.
 
*  https://github.com/garduino/Cuis-Artefact
 Artefact is a light and fast framework for build a PDF file.
 The failing tests are the same than in the original Cuis package.
 The original authors are doing a major rewrite of the package, estimated to release around March 2013.
 
*  https://github.com/garduino/Cuis-Cryptography
 Blowfish and System Hashing for Cuis.
 All tests pass.
 
*  https://github.com/garduino/Cuis-SimpleLogger
 Simple Logger for Cuis.

* https://github.com/CampSmalltalk/cuis-cypress 
  Monticello-style definitions for Cuis ... needed for Cuis support of common Smaltalk source import/export format.
  All tests pass.

* https://github.com/garduino/Cuis-JSON
  JSON support for Cuis (Ported from Pharo)
  All 12 tests pass. Limited Unicode support.

* https://github.com/KenDickey/Cuis-Ia-En
  An application to seek an Interlingua <-> English dictionary.
  A good example of a simple GUI.

* https://github.com/KenDickey/Cuis-NamedColors
  A LOT of named colors. Including several sets.

* https://github.com/hhzl/Cuis-SQLite
  A port of SQLite interface for Cuis.
 
Other packages practically tested
-----------------------------------------------

* https://github.com/KenDickey/Cuis-Solitaire
  Solitaire:  Klondike & FreeCell Solitaire Games
  Cuis port of the classic Squeak card games.

*  https://github.com/pbella/Cuis-Ports
 XML and XPath.
 Not all tests pass.
 
*  https://github.com/garduino/Cuis-SandstoneDB
 SandstoneDb, Simple ActiveRecord Style Persistence in Cuis.
 Still 1 test failing.

*  https://github.com/pmon/Cuis-ProfStef
   ProfStef for Cuis Smalltalk
   Does not work; 15 tests run, 6 passed, 9 errors

* https://github.com/davidgraham/Cuis-WebClient
  Port from http://www.squeaksource.com/WebClient.html
  tested by Germ‡n Arduino, far from being usable

* https://github.com/pbella/Cuis-Ports
  HTML parser; does not load properly. 
(TextFontChange is Undeclared) 
(TextIndent is Undeclared) 
(SketchMorph is Undeclared) 
(RectangleMorph is Undeclared)
(StandardFileMenu is Undeclared)
(FileUrl is Undeclared)
(MIMEDocument is Undeclared)
(GIFReadWriter is Undeclared) 
(PluggableTextMorph is Undeclared)
(StringHolder is Undeclared) 
(PopUpChoiceMorph is Undeclared)


Not yet tested / Still in development / Not adapted to latest Cuis updates
---------------------------------------------------------------------------------------

* https://github.com/garduino/Cuis-Pharo14CompatibilityLayer
 contains mainly network classes, work in progress.

* https://github.com/bpieber/Cuis-StyledTextEditor
  (works ok in Cuis 4.0, updates for Cuis 4.1 in progress)

* FFI from https://github.com/bpieber/Cuis-StyledTextEditor 

* https://github.com/garduino/Cuis-Sport (work in progress)

* https://github.com/garduino/Cuis-Swazoo (work in progress)

* https://github.com/garduino/Cuis-Zinc (not ready)

* Help System 

* SSL



Installation scripts
-----------------------------------------------

An example

    "To install Cuis-JSON assuming the repository is in a sibling directory of the Cuis directory"
    | slash |
    slash _ FileDirectory slash.

    {
         '..', slash, 'Cuis-CompatibilityWithOtherSmalltalks', slash,
      'Cuis-CompatibilityWithOtherSmalltalks.pck.st' .
         '..', slash, 'Cuis-JSON', slash,
      'Cuis-JSON.pck.st'.
    }
    do:
    [ :fileName | CodeFileBrowser installPackage:
                 (FileStream concreteStream readOnlyFileNamed: fileName)
    ] 
 