The following is a list of github repositories with packages for the Cuis Smalltalk system.

https://github.com/jvuletich/Cuis 

website: http://www.jvuletich.org/Cuis/Index.html

Please update the list as more packages and tests become available (send a note to the Cuis mailing list).




List of packages which load fine into Cuis 4.1
-----------------------------------------------

* https://github.com/bpieber/Cuis-StyledTextEditor
 Rich Text Editor based on Styles.

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

* https://github.com/hhzl/Cuis-SQLite
  A port of SQLite interface for Cuis.

* https://github.com/garduino/Cuis-Sport

* https://github.com/garduino/Cuis-Swazoo
 
* https://github.com/KenDickey/Cuis-Solitaire
  Solitaire:  Klondike & FreeCell Solitaire Games
  Cuis port of the classic Squeak card games.

* https://github.com/garduino/Cuis-Aida
Port of Aida Web (http://www.aidaweb.si) to Cuis Smalltalk 4.1
Taken from Aida6.5-interim.2 from the repo http://www.smalltalkhub.com/#!/~Aida/Aida/versions/Aida6.5-interim.2
The 80 tests are green.

* https://github.com/hhzl/Cuis-FFI
FFI port from http://source.squeak.org/FFI for Cuis Smalltalk https://github.com/jvuletich/Cuis
The port contains
    FFI-Kernel
    FFI-Pools
    FFI-Tests

* https://github.com/KenDickey/Cuis-NamedColors
  A LOT of named colors. Including several sets.


Other packages practically tested
-----------------------------------------------

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
These are various Smalltalk packages that have been ported to Cuis. Where possible, the original source that the package is based on has been noted in this document as well as in the package description field.
 Text Processing
- HTML - a tolerant HTML parser. Does a decent job of consuming the various malformed/invalid HTML out on the web. Based on HTML-sd.2.mcz found at http://squeaksource.com/@HDNjkoaXwriIV8js/Q0l6qq8Y (Some Undeclared)
- XML-Parser - Based on http://squeaksource.cdn.st/XMLSupport/XML-Parser-NorbertHartl.141.mcz
- XPath - Based on XML-Parser-AlexandreBergel.15.mcz XPath. Split out from XML-Parser
- VBRegex - A native regex implementation - no plugin required. Based on http://www.squeaksource.com/Regex/VB-Regex-damienpollet.17.mcz
Math
- 3DTransform - Based on 3DTransform-pbm.19.mcz


Not yet tested / Still in development / Not adapted to latest Cuis updates
---------------------------------------------------------------------------------------

* https://github.com/garduino/Cuis-Pharo14CompatibilityLayer
 contains mainly network classes, work in progress.

* FFI from https://github.com/bpieber/Cuis-StyledTextEditor 

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
    [ :fileName | CodePackageFile installPackageStream:
                 (FileStream concreteStream readOnlyFileNamed: fileName)
    ] 
 