RadStudioVersionInsight
=======================

RAD Studio Version Insight

This is actually a copy from:  
http://sourceforge.net/projects/radstudioverins/  
Here the master has been reset to the SourceForge's "plus" branch.

Compilation and installation instructions:
 
1. Delphi ships with it's own version insight.  Unfortunately Delphi's conflicts with the one that we are about to install here.  So you must remove the one that ships with Delphi.  The best way to do this is to search c:\ for all svn*.bpl, svn*.dcp, svn*.dcu and rename them.
  * It was found that simply disabling the packages from Delphi did not work because it caches the setting or something.
  * For example:  In XE6, it would complain about not being able to find the bpl entry point until all svn*.* files had been renamed in "C:\Program Files (x86)\Embarcadero\Studio\14.0\bin"
2. From Delphi, open the group project DelphiSvnXXX.groupproj 
  * XXX would correspond to your version of Delphi  
    * 200 = Delphi XE6
2. Choose to build all and install the design time packages (the last four in the group are design time packages)
3. For Git and Hg support, add the paths to Git.exe and Hg.exe by going to Tools -> Options -> Version Control -> [Git | Mercurial]

The installer **here** is not yet operational
