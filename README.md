# CALREDIEAutomatedv3

1. Ensure you have Python 3.7 downloaded on your machine (note. any 3.x version should be fine, but have not been tested)
    1a. For Python development, I reccomend using Pycharm IDE(free version) but it is not necessary
    1b. If using Pycharm, start a new project and follow the steps to set up a v. 3.7 Python VirtualEnvironment
    
2. Move Main.py and CalredieAutomator.py into whichever directory you want

3. Install Dependencies
    3a. Selenium
      - python -m pip install selenium
    3b. webdrivermanager
      - pip install webdrivermanager
    3c. pandas
      - pip install pandas
      
4. Edit Paths: replace name inside single quotes with paths on your machine
    4a. download_directory = r'location-to-download-from-calredie'
        -Note: The r script must be pointing to this location as well
    4b. r_script_location = r'location-of-your-r-script'
    4c. rScript = r'location-of-Rscript.exe' (wherever R is downloaded on your machine ex)-> R\R-4.0.2\bin\Rscript
   
5. Fill in your CalRedie Username/password pair
    5a. username = "your-username"
    5b. password = "your-password"
    
6. Ensure you change file paths in your R script to point to the download directory
    
7. Run:
    7a. In cmd window enter: python3 path-to-script/main.py
    7b. Or in Pycharm, right click on main.py and select run
    
    
This script will download calRedie data sets into specified directory, and run the specified R script and save the outputs to the same directory
