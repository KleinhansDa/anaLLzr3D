# anaLLzr3D and LLmapR

The **anallzr3D** is an IJ plugin to analyze Z-stacks of posterior lateral line primordium in 3D.

The **LLmapR** is a web-app to analyze the outcome of the ***anallzr3D***

## anallzr3D

The latest version of the Plugin can be found under ~/anallzr3D/

![input dialogs]('anaLLzr3D/anallzr3D_macro.png')

**Install**

* simply copy it to ~/plugins/ in your IJ home directory and re-start IJ. 
* after, you may find the plugin at the very bottom of your _plugins_ section or via search function.
* if you're missing other dependent plugins you'll be notified.  

## LLmapR

The web-app is structured in two pieces
* the back-end with processing instructions under **server.R**
* the front-end with the user interface and input parameters under **.R**

You may run the app locally by opening either the server or .. file in RStudio and selecting 'run app'. or under this link.
