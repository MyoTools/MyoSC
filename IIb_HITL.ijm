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
            //processFolder(currentPath);
            continue;
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
    T_IIb = substring(T, 0, lengthOf(T) - 4) + "_IIb";
    
    // Open the ROI (Region of Interest) file corresponding to the current image
    roiManager("Open", input + File.separator + T_IIb + "_rois.zip");  
    roiManager("Show None");
    roiManager("Show All with labels");
    
    while (true) {
    	waitForUser("Delete not type IIb cells");
    	userInput = getString("Enter the ROI numbers (space-separated, ranges allowed), press Enter to finish","");
    	
    	if (trim(userInput) == "") {
    		roiManager("Save", output_auto + File.separator + T_IIb + "_rois.zip");
    		print("All operations completed.");
    		break;
    		}

    cleanedArray = parseRange(userInput);

    cleanedArray = Array.sort(cleanedArray);
    cleanedArray = Array.reverse(cleanedArray);

    for (i = 0; i < cleanedArray.length; i++) {
        roiIndex = cleanedArray[i];

        roiManager("select", roiIndex);
        roiManager("delete");  
        print("ROI " + (roiIndex + 1) + " processed and deleted.");
        roiManager("Save", output_auto + File.separator + T_IIb + "_rois.zip");
    }
    print("Round completed. You can enter another set of ROI numbers.");
}
    roiManager("reset");
    run("Close All");
}

function parseRange(input) {
    parsedArray = newArray();  

    items = split(input, " ");
    
    for (i = 0; i < items.length; i++) {
        item = trim(items[i]);
        if (item != "") {
            if (indexOf(item, "-") != -1) {
                range = split(item, "-");
                start = parseInt(trim(range[0]));
                end = parseInt(trim(range[1]));
                
                for (j = start; j <= end; j++) {
                    parsedArray = Array.concat(parsedArray, j - 1); 
                }
            } else {
                parsedArray = Array.concat(parsedArray, parseInt(item) - 1);
            }
        }
    }
    
    return parsedArray;
}

    
// Close all open images and clear results
run("Close All");
run("Clear Results");
roiManager("reset");