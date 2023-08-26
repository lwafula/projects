To host RStudio on Azure;

This procedure works:

- https://www.jumpingrivers.com/blog/hosting-rstudio-server-on-azure/
-- remember when it comes to installing Rstudio step, always check the updated
   procedures by clicking on the embedded link in the steps or check here:

   https://posit.co/download/rstudio-server/

-- for creating url for your virtual machine, remember this step is on the vitual 
   machine (/home/rstudioVM)

   cd /etc/apache2/sites-available

- link to access my machine: 
       http://20.19.212.242:8787/ 

		OR
       rstudio-lmaaya.francecentral.cloudapp.azure.com:8787 
		
		OR(rstudio instead of the code)
       rstudio-lmaaya.francecentral.cloudapp.azure.com/rstudio/

- to login to my RStudio virtual machine(note there are 3 --- in the Pwd):
	Username: rstudioVM
	Pwd: Azure2023—
        Admin: lwafulae@gmail.com

- Creating and editing files on linux via CMD on windows

-- https://www.comptia.org/blog/your-nano-tutorial-create-edit-and-save-files#:~:text=Like%20Vim%2C%20nano%20uses%20the,file%20name%20by%20pressing%20Enter.&text=You%20successfully%20saved%20your%20edits.
-- check this video, from minute 22
   https://www.youtube.com/watch?v=q8NOmLD5pTU&list=PLeo1K3hjS3ut2o1ay5Dqh-r1kq6ZU8W0M&index=8&ab_channel=codebasics

   https://www.linuxfordevices.com/tutorials/linux/edit-config-files-in-linux#:~:text=To%20open%20a%20config%20file%20with%20Nano%2C%20open%20a%20Terminal,can%20make%20the%20necessary%20changes.

--- to add packages in R/RStudio
https://www.r-bloggers.com/2018/07/configuring-azure-and-rstudio-for-text-analysis/

OR simply login to the virtual machine
rstudioVM@20.19.212.242
sudo R
.libPaths()
install.packages('tidyverse')