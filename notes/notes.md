Notes
-----



Installation
-----


"Packages:"

    SystemVersion current highestUpdate. "--> 1549"
    
    packageFileNameList do:
     [:packageNameStr | 
        FileStream fileIn: packageFileNameStr 
     ].
    
    SystemVersion current highestUpdate. "--> 1549"



"Updates:"


    SystemVersion current highestUpdate. "--> 1511"
   
    updateFileNameList := 
         (FileDirectory default directoryNamed: 
              'UpdatesSinceLastRelease') 
         fileNames.


    updateFileNameList do:
      
     [:changeFileNameStr | 
        FileStream install: 
          ('UpdatesSinceLastRelease', slash, 
           changeFileNameStr) 
      ].
   
    SystemVersion current highestUpdate. "--> 1549"





TextMorph
---------

Counting TextMorph and PluggableTextMorph hierarchies, 
Squeak seems to have 14 classes for text morphs. Cuis has only 3.
It is not documentend why Squeak needs these 14 classes. For many of them the Cuis TextModelMorph be a good replacement.

Look at TextModelMorph and TextModel in Cuis. They are rather 
easy to understand.


### Background image ###

How to set a background image

    | filename |
     filename _ 'myImage.jpg'.
     self runningWorld backgroundImageData: 
       (FileStream readOnlyFileNamed: filename) binary
        contentsOfEntireFile.


### To explore further ###



    Display.



    ProjectX ui.



    Display runningWorld



    #runningWorld




    #spawnNewMorphicProcessFor: aWorld


Method


    
#usefulExpressionsContents

contains the string for the "Useful Expressions" workspace.