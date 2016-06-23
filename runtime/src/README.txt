More information on the Planisware wiki page : 
https://oahu.planisware.com/wiki/index.php/Emacs_runtime_environment

Planisware Emacs runtime environment installation
-------------------------------------------------

(0 - Install a Planisware runtime environments of the versions you want to use)

1 - Install emacs 24. 
* Linux: use your package manager. 
* Windows : Download and install from http://vgoulet.act.ulaval.ca/emacs/windows/

2 - Unzip the emacs-runtime-VERSION.zip to the directory you want.

3 - Add the following lines between the --- to your .emacs configuration file. 
    Replace PATHTOINSTALLATIONDIRECTORY by the directory you unzipped the files to. 
    On Windows, replace the \ in the path by /. 
Note : To open the .emacs file, do the following under emacs 

  C-x C-f 
  Type ~/.emacs
  Enter

-------------------------------------------------------------
(defvar *opx2-network-folder-work-path* (file-name-as-directory "PATHTOINSTALLATIONDIRECTORY"))
(load (format "%semacs.el" *opx2-network-folder-work-path*))
-------------------------------------------------------------

4 - Rename the file planisware-versions.conf.dist in planisware-versions.conf.

5 - Edit planisware-versions.conf and add the different paths to the versions you want as indicated in the file.

6 - Launch Emacs. If your configuration is OK, the background should be dark and you should see a "Planisware" menu at the right of the Tools menu.

How to launch an Intranet
-------------------------

Warning : You have to make sure that the installation you want to use has the sc9404 loaded !

In the Planisware menu, select "Start Planisware VERSION..." and select the intranet.ini file of the installation you want to start. The intranet port will be read from the database declaration, so make sure to update it. 
