macro "Save All Images and Channel 1" {
    // Ask for the save directory
    saveDir = getDirectory("Choose a Directory to Save Files");
    
    // Create a log file
    logFile = File.open(saveDir + "log_save_tiff.txt");
    
    // Get all open image titles
    titles = getList("image.titles");
    
    // Loop through all open images
    for (i = 0; i < titles.length; i++) {
        title = titles[i];
        print(logFile, "Processing image: " + title);
        
        // Select the image
        selectWindow(title);
        
        // Remove .tif extension if present
        baseName = replace(title, ".tif", "");
        
        // Save the full image as TIFF
        fullPath = saveDir + baseName + ".tif";
        saveAs("Tiff", fullPath);
        print(logFile, "Saved full image: " + fullPath);
        
        // Split channels
        run("Split Channels");
        
        // Save and close C1 channel
        c1Title = "C1-" + baseName;
        if (isOpen(c1Title)) {
            selectWindow(c1Title);
            c1Path = saveDir + c1Title + ".tif";
            saveAs("Tiff", c1Path);
            print(logFile, "Saved C1 image: " + c1Path);
            close();
        } else if (isOpen(c1Title + ".tif")) {
            selectWindow(c1Title + ".tif");
            c1Path = saveDir + c1Title + ".tif";
            saveAs("Tiff", c1Path);
            print(logFile, "Saved C1 image: " + c1Path);
            close();
        } else {
            print(logFile, "Warning: C1 channel not found for " + title);
        }
        
        // Close other channels
        for (j = 2; j <= 3; j++) {
            cjTitle = "C" + j + "-" + baseName;
            if (isOpen(cjTitle)) {
                selectWindow(cjTitle);
                close();
                print(logFile, "Closed " + cjTitle);
            } else if (isOpen(cjTitle + ".tif")) {
                selectWindow(cjTitle + ".tif");
                close();
                print(logFile, "Closed " + cjTitle + ".tif");
            }
        }
    }
    
    // Close the log file
    File.close(logFile);
    
    // Show completion message
    showMessage("Macro completed", "All images have been processed. Check the log file in the save directory for details.");
}
