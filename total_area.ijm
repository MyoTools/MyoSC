/*
 * Macro template to process multiple images in a folder
 */

#@ File (label = "Input directory", style = "directory") input
#@ File (label = "Output directory", style = "directory") output
#@ String (label = "File suffix", value = ".tif") suffix

// See also Process_Folder.py for a version of this code
// in the Python scripting language.

processFolder(input);

// function to scan folders/subfolders/files to find files with correct suffix
function processFolder(input) {
	list = getFileList(input);
	list = Array.sort(list);
	for (i = 0; i < list.length; i++) {
		if(File.isDirectory(input + File.separator + list[i]))
			processFolder(input + File.separator + list[i]);
		if(endsWith(list[i], suffix))
			processFile(input, output, list[i]);
	}
}

run("Set Measurements...", "area centroid center perimeter bounding fit shape feret's display nan redirect=None decimal=3");


function processFile(input, output, file) {
	// Do the processing here by adding your own code.
	 // Open the image file

	open(input + File.separator + file);
	T = getTitle();
	
	run("Set Scale...", "distance=216 known=100 unit=um");
	
	roiManager("Open", input + File.separator + substring(T, 0, lengthOf(T) - 4) + "_rois.zip");

    n = roiManager("count");
    for (j=0; j<n; j++) {
    	roiManager("Select", j);
    	run("Measure");
            }
	
	run("Close All");
	roiManager("reset");
}
selectWindow("Results");
saveAs("Results", output + File.separator + "Fiber.csv");