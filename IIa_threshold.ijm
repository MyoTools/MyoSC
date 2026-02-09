/*
 * Macro template to process multiple images in a folder
 */

// Define input parameters with UI prompts
#@ File (label = "Input directory", style = "directory") input
#@ File (label = "Output directory", style = "directory") output
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
            processFile(input, output, list[i]);
        }
    }
}

/**
 * Processes an individual file by applying various image processing steps
 * @param {String} input - The input directory path
 * @param {String} output - The output directory path
 * @param {String} file - The filename to process
 */
function processFile(input, output, file) {
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
    T_II = substring(T, 0, lengthOf(T) - 4) + "_II";
    T_IIa = substring(T, 0, lengthOf(T) - 4) + "_IIa";
    newImage(T_IIa, "8-bit black", 2560, 1920, 1);
    
    // Open the ROI (Region of Interest) file corresponding to the current image
    roiManager("Open", input + File.separator + T_II + "_rois.zip");
    
    // Show all ROIs in the ROI Manager
    //selectWindow(T);//
    //roiManager("show all");//
    
    // Get the number of ROIs
    N_rois = roiManager("count");//
    
    /* 
     * Process IIa-type Cells
     */
    
    for (j = 0; j < N_rois; j++) {
        // Select each ROI for processing
        roiManager("select", j);
        run("Fit Spline");
        
        // Get the coordinates of the current ROI
        getSelectionCoordinates(xpoints, ypoints);
        
        // Calculate the centroid of the ROI
        xc = 0;
        yc = 0;
        n = xpoints.length;
        for (i = 0; i < n; i++) {
            xc += xpoints[i];
            yc += ypoints[i];
        }
        xc /= n;
        yc /= n;
        
        // Define the inward shift distance//
        // distance = 1;//
        
        // Initialize arrays to store inward-shifted points
        xpoints1 = newArray(n);
        ypoints1 = newArray(n);
        
        // Calculate inward-shifted coordinates towards the centroid
        for (m = 0; m < n; m++) {
            dx = xc - xpoints[m];
            dy = yc - ypoints[m];
            length = sqrt(dx * dx + dy * dy);
            
            // Calculate the inward shift distance as 5% of the length
            distance = length * 0.01;
        
            if (length > 0) {
                dx /= length;
                dy /= length;
            }
            
            xpoints1[m] = xpoints[m] + dx * distance;
            ypoints1[m] = ypoints[m] + dy * distance;
        }
        
        // Create a new selection from the inward-shifted points
        selectImage(T);
        makeSelection("Polygon", xpoints1, ypoints1);
        run("Area to Line");
        
        // Calculate the average blue intensity along the profile
        list_profile = getProfile();
        sum_blue = 0;
        for (k = 0; k < list_profile.length; k++) {
            sum_blue += list_profile[k];
        }
        ave_blue = sum_blue / list_profile.length;
        run("Select None");
        
        // If average blue intensity exceeds threshold, process as IIa-type cell
        if (ave_blue > 100 && ave_blue < 170) {
            selectWindow(T_IIa);
            roiManager("reset");
            roiManager("Open", input + File.separator + T_II + "_rois.zip");
            roiManager("select", j);
            run("Invert");
        }
    }
    
    // Save the IIa-type cell mask as a JPEG image
    selectWindow(T_IIa);
    roiManager("reset");
    roiManager("Open", input + File.separator + T_II + "_rois.zip");
	
	roiManager("measure");
	N_rois = roiManager("count");
		
	for (j = N_rois - 1; j >= 0; j--) {
		selectWindow("Results");
		Int_mean = getResult("Mean", j);
		
		if (Int_mean < 250) { 
			selectWindow(T_IIa);
			roiManager("select", j);
			roiManager("Delete");
		}
	}

	roiManager("Save", output + File.separator + T_IIa + "_rois.zip");
	
	// Close all open images and clear results
	roiManager("reset");
    run("Close All");
    run("Clear Results");
}