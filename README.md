
# mplus.rstudio

Addin for Rstudio that allows to run Mplus code within RStudio (Mplus license and installed Mplus are required).

## Installation and usage

Run 

```
devtools::install_github("maksimrudnev/mplus.rstudio")

```


After this you will find three commands in your RStudio Addins button under header "MPLUS.RSTUDIO" (no need to load the library).  


-**Run in Mplus** Open the Mplus input file (make sure it is active, that is, just click on the code), click "Run in Mplus" and it will run. If it's not running, try setting up a path to the Mplus executable file, or a command that refers to it. On the click "Run in Mplus" saves the input file, and after running the code, it automatically opens the output file. You can see the progess in the console.

Additionally, you may go to Tools -> Modify Keyboard Shortcuts, find in there "Run in Mplus" and assign a custom shortcut (for example, I use Control+R on my Mac).

-**Create Mplus input** This command creates a template code for the new analysis, saves a data.frame in Mplus-compatible format, and opens the code in RStudio.

-**Set Mplus path** Tell the RStudio where to find Mplus executable file. The defualt is just "mplus" and in most cases it works.
