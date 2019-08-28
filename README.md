
# mplus.rstudio

Addin for Rstudio that allows to run Mplus code within RStudio (Mplus license and installed Mplus are required).

## Installation and usage

Run 

```
install.github("maksimrudnev/mplus.rstudio")

```


After this you will find two commands in your RStudio Addins button on the top panel "Run in Mplus".
Open the Mplus input file (make sure it is active, that is out your cursor inside it), click "Run in Mplus" and it will run. If it's not running, try setting up a path to the Mplus executable file, or a command that refers to it. It automatically opens the output file after run is finished. You can see the progess in the console.

Additionally, you may go to Tools -> Modify Keyboard Shortcuts, find in there "Run in Mplus" and assign a custom shortcut (for example, I use Control+R on my Mac).
