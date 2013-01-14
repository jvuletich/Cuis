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

 
Other packages practically tested
-----------------------------------------------

*  https://github.com/pbella/Cuis-Ports
 XML and XPath.
 Not all tests pass.
 
*  https://github.com/garduino/Cuis-SandstoneDB
 SandstoneDb, Simple ActiveRecord Style Persistence in Cuis.
 Still 1 test failing.
 
*  https://github.com/garduino/Cuis-JSON
 JSON Support for Cuis
 Still 1 test failing.

*  https://github.com/pmon/Cuis-ProfStef
   ProfStef for Cuis Smalltalk
   Does not work; 15 tests run, 6 passed, 9 errors

* https://github.com/davidgraham/Cuis-WebClient
  Port from http://www.squeaksource.com/WebClient.html
  tested by Germán Arduino, far from being usable


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


Not yet tested
-----------------------------------------------

* https://github.com/garduino/Cuis-Pharo14CompatibilityLayer
 contains mainly network classes, work in progress.

* https://github.com/bpieber/Cuis-StyledTextEditor

* Help System 

* SSL

* FFI from https://github.com/bpieber/Cuis-StyledTextEditor 

* https://github.com/garduino/Cuis-Sport (work in progress)

* https://github.com/garduino/Cuis-Swazoo (work in progress)

* https://github.com/garduino/Cuis-Zinc (not ready)



Installation scripts
-----------------------------------------------


    "To install Cypress, assuming the package files are in 'cuis-cypress-master'
    in a subdirectory of the working directory"

    #( 
     'cuis-cypress-master\Cypress-Definitions.pck'
     'cuis-cypress-master\Cypress-Structure.pck'
     'cuis-cypress-master\Cypress-Mocks.pck'
     'cuis-cypress-master\Cypress-Tests.pck'
    )  
       do: 
        [ :fileName | CodeFileBrowser installPackage:
                          (FileStream concreteStream readOnlyFileNamed: fileName)
        ]

 
 