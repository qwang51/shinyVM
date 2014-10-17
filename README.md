Shiny
=====
Description
-----------
This will get up and running a local Shiny Server to enable viewing of 
the sample Shiny application and the creation your own Shiny app. This is 
suitable for both local development and for future deployment of a Shiny 
Server with your app.

Creating the server requires very few commands since the process is 
almost entirely automated by Fabric. The commands described below will create a virtual machine (VM) running
Ubuntu Server 14.04 and install all the necessary R and Shiny 
packages into the VM, with R Markdown enabled. Shiny app files will be 
editable using your host machine. The Shiny server will be accessed in your 
browser at *localhost:7070*. This should greatly simplify the process of 
creating a Shiny Server.
Requirements
------------
If you are using Windows the requirements and process should be similar, 
if not exactly the same. The installation of Python/Fabric is likely the 
only challenge, but both work on Windows if installed correctly.
For ease, use Mac OSX or Linux and install the following:

+ **VirtualBox:** https://www.virtualbox.org/wiki/Downloads
+ **Vagrant:** https://www.vagrantup.com/downloads
+ **Fabric:** http://www.fabfile.org/installing.html

It is likely that you already have Python, which Fabric requires. If you are 
unsure, open Terminal and execute `python`.
Installation
------------
Clone this repository into a folder on your computer. Then open your Terminal
and `cd` into the repository on your computer. Run `vagrant up` to create 
the virtual machine. Finally, run `fab vagrant setup_vagrant` to install and
set up the Shiny Server and its dependencies. That's it! The last line in 
your terminal should give you the status of Shiny Server. Open your browser
and visit *localhost:7070* to view the sample app.

Creating Apps
-------------
During the setup process, the Shiny Server on your VM was directed to look in a 
new shared folder called **project** that was created in your repository. When 
you view the sample Shiny app in your browser, the Shiny Server reads the 
files in this folder. Any modifications to files in this folder will be 
visible to the VM, so you can simply delete the sample app and develop your app
in **project** on your host machine.
