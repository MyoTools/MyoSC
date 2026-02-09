/*
 * Macro template to process multiple images in a folder
 */

// Define input parameters with UI prompts
#@ File (label = "Image Directory", style = "directory") input
#@ File (label = "Auto_output Directory", style = "directory") output_120
#@ String (label = "File suffix", value = ".tif") suffix

// Start processing the input folder
processFolder(input);

/**
 * Recursively scans folders and subfolders to find files with the specified suffix
 * @param {String} input - The directory to scan
 */
function processFolder(input) {
    // Get the list of files and directories in the current input directory
    list = getFileList(input);
    // Sort the list for consistent processing order
    list = Array.sort(list);
    
    // Iterate through each item in the list
    for (i = 0; i < list.length; i++) {
        // Construct the full path of the current item
        currentPath = input + File.separator + list[i];
        
        // If the current item is a directory, recurse into it
        if (File.isDirectory(currentPath)) {
            processFolder(currentPath);
        }
        
        // If the current item ends with the specified suffix, process the file
        if (endsWith(list[i], suffix)) {
            processFile(input, output_120, list[i]);
        }
    }
}

/**
 * Processes an individual file by applying various image processing steps
 * @param {String} input - The input directory path
 * @param {String} output_auto - The output_auto directory path
 * @param {String} file - The filename to process
 */
function processFile(input, output_auto, file) {
    // Print the file being processed for debugging purposes
    print("Processing: " + input + File.separator + file);
    
    // Open the image file
    open(input + File.separator + file);
    
    // Get the title of the current image
    T = getTitle();
    
    /* 
     * Create Mask Canvases for Different Cell Types (I, IIa, IIb)
     */
    
    // Define titles for mask canvases based on the original image title
    T_I = substring(T, 0, lengthOf(T) - 4) + "_I";
    
    // Create new black images for each cell type mask
    newImage(T_I, "8-bit black", 2560, 1920, 1);
    
    // Open the ROI (Region of Interest) file corresponding to the current image
    roiManager("Open", input + File.separator + substring(T, 0, lengthOf(T) - 4) + "_rois.zip");
    
    // Show all ROIs in the ROI Manager
    //selectWindow(T);//
    //roiManager("show all");//
    
    // Get the number of ROIs
    //N_rois = roiManager("count");//
    
    /* 
     * Process I-type Cells
     */
    
    // Clear previous measurement results
    //run("Clear Results");
    
	selectWindow(T);
	run("Duplicate...", " ");
	run("8-bit");
	roiManager("deselect");
	run("Set Measurements...", "area mean min shape integrated median skewness kurtosis area_fraction display redirect=None decimal=3");
	roiManager("measure");
	N_rois = roiManager("count");
	
    for (j = N_rois - 1; j >= 0; j--) {
		selectWindow("Results");
		Int_mean = getResult("Mean", j);

		if (Int_mean<140) {

		selectWindow(T_I);
    	roiManager("select", j);
    	run("Invert");
		}
	}
    
    // Save the I-type cell mask as a PNG image
    selectWindow(T_I);
    T_temp = getTitle();
    saveAs("PNG", output_auto + File.separator + T_I + ".png");
    rename(T_temp);
    

	run("Clear Results");
	

    Name_I = substring(file, 0, lengthOf(T) - 4) + "_I.png";
    // Open and process the mask for type I cells
    open(output_auto + File.separator + Name_I);
	run("Set Measurements...", "area mean min shape integrated median skewness kurtosis area_fraction display redirect=None decimal=3");

	roiManager("reset");
	roiManager("Open", input + File.separator + substring(T, 0, lengthOf(T) - 4) + "_rois.zip"); 
	roiManager("measure");
	N_rois = roiManager("count");
		
	for (j = N_rois - 1; j >= 0; j--) {
		selectWindow("Results");
		Int_mean = getResult("Mean", j);
		
		if (Int_mean < 250) { 
			selectWindow(T_I);
			roiManager("select", j);
			roiManager("Delete");
		}
	}
	

	roiManager("Save", output_auto + File.separator + T_I + "_rois.zip");
	
	// Close all open images and clear results
    run("Close All");
    run("Clear Results");
    roiManager("reset");
}
