///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////								MACRO INSTRUCTIONS			 		        ///////////	
///////////////////////////////////////////////////////////////////////////////////////////////
/////////// To use this macro you need to open a (segmented) binary and the 		///////////
/////////// according original image data, then press Run, then follow instructions.///////////
///////////	It was designed to Analyze (shape & nuclei number) and register 		///////////
/////////// the movement of the dr pLLP in Timelapse data.							///////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////								OPTIMIZED FOR... 						    ///////////
///////////			- 40x microscopic data of the 								    ///////////
///////////			- zebrafish pLLP												///////////
/////////// 		- different labels							   					///////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////         			     David Kleinhans, 09.02.2017				    ///////////
///////////						  Kleinhansda@bio.uni-frankfurt.de				    ///////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////	
	
// ################### GET PARAMETERS & DIRECTORIES, SHOW DIALOGS ######################
	saveSettings;
	dirplug = getDirectory("plugins");
// Start up / get Screen parameters to set location of Dialog Boxes
	cleanup();
	saveSettings();
	version = 2.8;
	header = "pLLP ANALYZER 3D v"+version+" "; 
	getDateAndTime(year, month, dayOfWeek, dayOfMonth, hour, minute, second, msec);
	print("started: "+ hour + ":" + minute);
	scrH = screenHeight();
		DiaH = scrH/8;
		InfoH = scrH/8;
		LogH = scrH/8;
	scrW = screenWidth();
		DiaW = scrW/6;
		InfoW = scrW/2.5;
		LogW = scrW/1;
	selectWindow("Log");
	setLocation(LogW,LogH);
//	Opening Dialog
	guideI();
	Dialog.create(header);
	Dialog.setLocation(DiaW,DiaH);
	labelsC1 = newArray("515", "-139");
	labelsC2 = newArray("H2BRFP", "Arl13b", " ");
	ACMeasure = newArray("fit", "bounding", "area");
	Zorder = newArray("Bottom to Top", "Top to Bottom");
		Dialog.setInsets(0,10,0);
		Dialog.addMessage("~~~~~~~~~~~~~~~ DATA DIMENSIONS ~~~~~~~~~~~~~~\n~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~");
	  	  	//Dialog.addCheckbox("Deconvolved", false);
	  	  	Dialog.addCheckbox("Multi Channel", false);								// 1. multi
	  	  	Dialog.setInsets(0,20,0);
			Dialog.addChoice("C1:", labelsC1);										// 2. C1label
			Dialog.setInsets(0,20,0);
			Dialog.addChoice("C2:", labelsC2, " ");									// 3. C2label
			Dialog.setInsets(0,20,0);
			Dialog.addCheckbox("Swap Channel #", false);							// 4. inv
			Dialog.addCheckbox("Time-Series", false);								// 5. ts
		Dialog.setInsets(0,10,0);
		Dialog.addMessage("~~~~~~~~~~~~~~~ MACRO OPTIONS ~~~~~~~~~~~~~~~\n~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~");
		//Dialog.addMessage("- - - - - - - - - - - - - - DECONVOLUTION - - - - - - - - - - - - - -");
			Dialog.addCheckbox("Include deconvolution", false); 					// 6. decon
			Dialog.addSlider("Iterations", 1, 10, 5); 								// 7. iter
			//Dialog.addMessage("-------------------------- METADATA --------------------------");
			Dialog.addCheckbox("Save metadata", false); 							// 8. meta
			Dialog.setInsets(0,20,0);
			//Dialog.addMessage("-------------------------- SCAN DIRECTION --------------------------");
			Dialog.addChoice("Z order", Zorder); 									// 9. rev
			//Dialog.addMessage("-------------------------- REGISTRATION --------------------------");
			Dialog.addCheckbox("Register primordium", true); 						// 10. reg
			//Dialog.addMessage("-------------------------- RECONSTRUCTION --------------------------");
			Dialog.addCheckbox("Reconstruct Ferret lines (EXPERIMENTAL)", false); 	// 11. ferrr
			Dialog.addCheckbox("Manual segmentation", false); 						// 12. manual
		Dialog.setInsets(0,10,0);
		Dialog.addMessage("~~~~~~~~~~~~~~~~ THRESHOLDS ~~~~~~~~~~~~~~~~\n~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~");
			Dialog.setInsets(0,20,0);
			Dialog.addMessage("3D Membrane Segmentation tolerance level:"); 
			Dialog.addSlider("Tol", 0, 50, 5); 										// 13. tol
			Dialog.addMessage("Min. Volume threshold");							
			Dialog.addSlider("Min.", 0, 50, 20);									// 14. vmin
			Dialog.addMessage("Max. Volume threshold");							
			Dialog.addSlider("Max.", 250, 1000, 750);								// 15. vmax
		Dialog.setInsets(0,10,0);
		Dialog.addMessage("~~~~~~~~~~~~~~~ MEASUREMENTS ~~~~~~~~~~~~~~~\n~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~");
			Dialog.addCheckbox("Measure Rosette Constriction", false); 				// 16. rosAC
			Dialog.addSlider("µm", 1, 10, 5); 										// 17. rosum
			Dialog.addCheckbox("Measure ACI fixed delta", false); 					// 18. cellAC
			Dialog.addSlider("µm", 1, 10, 2);										// 19. cellum
			Dialog.addCheckbox("Measure ACI symmetric delta", false); 				// 20. cellACd
			Dialog.addSlider("%", 10, 40, 20); 										// 21. cell%
			Dialog.setInsets(0,20,0);
			Dialog.addChoice("AC measurement", ACMeasure);							// 22. ACM
		//Dialog.addMessage("click 'HELP' for info");
		Dialog.show();
			multi = Dialog.getCheckbox();											// 1. multi
			C1label = Dialog.getChoice();											// 2. C1
			C2label = Dialog.getChoice();											// 3. C2
			inv = Dialog.getCheckbox();												// 4. inv
			ts = Dialog.getCheckbox();												// 5. ts
			decon = Dialog.getCheckbox();											// 6. decon
			iter = Dialog.getNumber();												// 7. iter
			meta = Dialog.getCheckbox(); 											// 8. meta
			rev = Dialog.getChoice();												// 9. rev
			reg = Dialog.getCheckbox();												// 10. reg
			ferrr = Dialog.getCheckbox();											// 11. ferrr
			manual = Dialog.getCheckbox();											// 12. manual
			tol = Dialog.getNumber();												// 13. tol
			vmin = Dialog.getNumber();												// 14. vmin
			vmax = Dialog.getNumber();												// 15. vmax
			rosAC = Dialog.getCheckbox();											// 16. rosAC
			rosum = Dialog.getNumber();												// 17. rosum
			cellAC = Dialog.getCheckbox();											// 18. cellAC
			cellum = Dialog.getNumber();											// 19. cellum
			ACId = Dialog.getCheckbox();											// 20. cellACd
			if (ACId) {																// 21. cell%
				cellum = Dialog.getNumber();									
				cellum = cellum/100;
			}
			ACM = Dialog.getChoice();												// 22. ACM
	selectWindow(header);
	wait(100);
	run("Close");
//	CHOOSE DIRECTORIES
	if (decon) {
		PSF = File.openDialog("Choose PSF file");
	}
	dir = getDirectory("Choose RAW image data directory");
//	Batch Mode
	list = getFileList(dir);
//	setBackground value 0
	setBackgroundColor(0,0,0);
//	##################################################################################################################
//	#################################################### ENTER LOOP ##################################################
//	##################################################################################################################
	for (i = 0; i < list.length; i++) {
		//setBatchMode(true);
		IJ.redirectErrorMessages();
		roiManager("reset");
		//if (endsWith(list[i],".nd2")) {
		file = dir+list[i];
	// 	Import images + Split channels
		run("Bio-Formats Importer", "open=["+file+"] color_mode=Grayscale view=Hyperstack stack_order=XYCZT");
		ORG = getTitle();
		name = File.nameWithoutExtension;
		dir = File.directory;
		pardir = File.getParent(dir);
	//	Channel preparation
		if (rev == "Top to Bottom") {
			run("Reverse");
		}
		if (multi) {
			if (inv) {
				run("Arrange Channels...", "new=21");
			}
			wait(200);
			run("Split Channels");
			close(ORG);
		}
	//	Create subdirectories
			imgdir = pardir + File.separator + "02 - Processed Images" + File.separator;
				File.makeDirectory(imgdir);
			datdir = pardir + File.separator + "03 - Measurements" + File.separator;
				File.makeDirectory(datdir);
			intensdir = pardir + File.separator + "03 - Measurements" + File.separator + "Intensities" + File.separator;
				File.makeDirectory(intensdir);
			datprimdir = pardir + File.separator + "03 - Measurements" + File.separator + "3D" + File.separator;
				File.makeDirectory(datprimdir);
			if (rosAC) {
				datrosdir = pardir + File.separator + "03 - Measurements" + File.separator + "Rosettes" + File.separator;
				File.makeDirectory(datrosdir);
			}
			datcelldir = pardir + File.separator + "03 - Measurements" + File.separator + "XYZ_Ferets" + File.separator;
			File.makeDirectory(datcelldir);
			posdir = imgdir + name + File.separator;
				File.makeDirectory(posdir);
				if (multi) {
					C2dir = posdir + File.separator + "C2" + File.separator;
					File.makeDirectory(C2dir);
					C1dir = posdir + File.separator + "C1" + File.separator;
					File.makeDirectory(C1dir);
				}
			metadir = posdir + File.separator + "meta" + File.separator;
					File.makeDirectory(metadir);
			//resultsdir = pardir + File.separator + "Results" + File.separator;
					//File.makeDirectory(resultsdir);
	//	PRINT METADATA TO LOG WINDOW
		if (i==0) {
		print("Channels:");
			print("   C1: "+C1label);
			print("   C2: "+C2label);
		print("Macro options:");
			print("   pLLPANALYZER 3D version: v"+version);
			if (meta) {print("   Save metadata: true");} else {print("   Save metadata: false");}
			print("   Z Order: "+rev);
			if (decon) {print("   Deconvolution: true, "+"Algorithm: "+decon+", Iterations: "+iter);} else {print("   Deconvolution: false");}
			if (reg) {print("   Primordium Registration: true");} else {print("   Primordium Registration: false");}
			if (ferrr) {print("   Reconstruct feret lines in 3D: true");} else {print("   Reconstruct feret lines in 3D: false");}
			if (rosAC) {print("   Rosette Constriction: true, "+rosum+" µm");} else {print("   Rosette Constriction: false");}
			if (cellAC) {print("   Cell Constriction: true, "+cellum+" µm");} else {print("   Cell Constriction: false");}
			if (ACId) {print("   ACI symmetric delta: true, "+cellum*100+" %");} else {print("   Cell Constriction: false");}
			print("   Segmentation Threshold: "+tol);
		print("Directories:");
			if (multi) {
			print("   Positions directory: "+posdir);
			print("   C1 directory: "+C1dir);
			print("   C2 directory: "+C2dir);
				} else {
			print("   Positions directory: "+posdir);
			}
			if (meta) {
			print("   Meta data: "+metadir);
			}
			print("   Results: "+datdir);
		}
	//	SAVE CHANNELS
		if (multi) {
		selectWindow("C1-"+ORG);
			//saveAs("Tiff", C1dir + name + "-C1.tif");
			ORGC1 = getTitle();
		selectWindow("C2-"+ORG);
			//saveAs("Tiff", C2dir + name + "-C2.tif");
			ORGC2 = getTitle();
		}
		resetMinAndMax();
	//	Get BioFormats
		n = nSlices();
		run("Bio-Formats Macro Extensions");
		if (multi) {
			id = C1dir+ORGC1; // get ID of first element of org.filelist(ofl)
		} else {
			id = dir+list[i];}
		Ext.setId(id);
		Ext.getSeriesName(seriesName);
		Ext.getImageCreationDate(creationDate);
  		Ext.getPixelsPhysicalSizeX(sizeX);
  		Ext.getPixelsPhysicalSizeY(sizeY);
  		Ext.getPixelsPhysicalSizeZ(sizeZ);
  		Ext.getPlaneTimingDeltaT(deltaT, 2);
  		Ext.getSizeC(sizeC);
  		if (i==0) {
  			print("Bio-Formats metadata:");
  			print("   X/Y Resolution: "+sizeX+"/"+sizeY + " µm");
  			print("   Z Resolution: "+sizeZ+" µm");
  			print("   T Resolution: "+deltaT+" s");
  			setSlice(n/2);
  		}
  	  	
//	#################################################### PRE-PROCESSING ##################################################
		print("");
		print("################## Processing file "+name+" ##################");
//	######################### REGISTRATION PARAMETERS ########################
		print("Calculating registration parameters...");
		setSlice(n/2);
		resetMinAndMax();
	// 	create Z-projection to generate masks and do calculations on
		// projection=[Standard Deviation]");
		if (multi) {selectWindow(ORGC1);} else {selectWindow(ORG);}
		run("Z Project...", "projection=[Standard Deviation]");
		ZPSTD = getTitle();
		run("16-bit");
		run("Duplicate...", " ");
		ZPSTDD = getTitle();
		//projection=[Max Intensity]");
		if (multi) {selectWindow(ORGC1);} else {selectWindow(ORG);}
		run("Z Project...", "projection=[Max Intensity]");
		ZPMAX = getTitle();
	// Create Minimum Thresholded Mask
		selectWindow(ZPSTD);
		run("Gaussian Blur...", "sigma=8 scaled");
		//waitForUser("");
		setAutoThreshold("Minimum dark");
		//waitForUser("");
		run("Convert to Mask");
		getDimensions(width, height, channels, slices, frames);
		makeRectangle(1, 1, 1, height);
		getRawStatistics(nPixels, mean, min, max, std, histogram);
		if (mean > 0) {
			run("Watershed");
		}
		run("Select None");
//	############################### ANGLE ##########################
		run("Analyze Particles...", "include add");
		rmcount = roiManager("count")-1;
		if(roiManager("count")==1) {
			roiManager("select", 0);
			run("Fit Ellipse");
			List.setMeasurements;
			Angle = List.getValue("FeretAngle");
			if (Angle < 0) {Angle = Angle*(-1);}
			if (Angle > 90) {Angle = (180-Angle)*(-1);}
		} else {
			roiManager("select", 0);
			//waitForUser("");
			run("Fit Ellipse");
			//waitForUser("");
			roiManager("update");
			List.setMeasurements;
			X1Line = List.getValue("X");
			Y1Line = List.getValue("Y");
			roiManager("select", rmcount);
			List.setMeasurements;
			X2Line = List.getValue("X");
			Y2Line = List.getValue("Y");
			makeLine(X1Line, Y1Line, X2Line, Y2Line); 
			List.setMeasurements;
			Angle = List.getValue("Angle");
			if (Angle < 0) {Angle = Angle*(-1);}
			if (Angle > 90) {Angle = (180-Angle)*(-1);}
		}
		print("   Rotation angle: "+Angle);
		selectWindow(ZPSTD);
		run("Select None");
		run("Rotate... ", "angle="+Angle+" grid=1 interpolation=Bilinear");
		if (meta) {
			print("   Saving "+ name + "RC_ZPSTD_Mask.tif");
			saveAs("Tiff", metadir + name + "_RC_ZPSTD_Mask.tif");
			ZPSTD = getTitle();
		}
//	########################## CROPPING Parameters ############################## 
		roiManager("reset");
		run("Select None");
		run("Make Binary");
		run("Erode");
		selectWindow(ZPSTD);
		run("Analyze Particles...", "size=150-10000 include exclude add");
		rmcount = roiManager("count")-1;
		if(roiManager("count")==1) {
			roiManager("select", 0);
		} else {
			roiManager("select", rmcount);
		}
		List.setMeasurements;
		XRect = List.getValue("X");
		YRect = List.getValue("Y");
		selectWindow(ZPSTD);
		getDimensions(width, height, channels, slices, frames);
		Regwidth = width;
		Regheight = 400; // change height of rect here
		toUnscaled(YRect);
		YRect = YRect-(Regheight/2);
		close(ZPSTD);
		print(" Cropping parameters");
		print("   Y Location: "+YRect);
	//print("   Height: "+height);
		print(" Registering "+ORG+"...");
		roiManager("reset");
		selectWindow(ZPMAX);
//	########################## Primordium Registration ############################## 
	// Rotate
		run("Rotate... ", "angle="+Angle+" grid=1 interpolation=Bilinear stack");
	// Crop
		makeRectangle(0, YRect, Regwidth, Regheight);
		run("Crop");
		print("   Saving "+ name + "RC_ZPMAX.tif");
		saveAs("Tiff", posdir + name + "_RC_ZPMAX.tif");
		ZPMAX = getTitle();
	// Create threshold mask to clear signals outside ROI
		run("Morphological Filters", "operation=Closing element=Disk radius=40");
		if (meta) {
			saveAs("Tiff", metadir + name + "_RC_ZPMAX_Close.tif");
		}
		ZPMAXClose = getTitle();
		//close(ZPMAX);
		selectWindow(ZPMAXClose);
		run("Gaussian Blur...", "sigma=0.5 scaled");
		run("Duplicate...", " ");
		ZPMAXCloseMask = getTitle();
		//run("Enhance Contrast...", "saturated=0 equalize");
		run("Normalize Local Contrast", "block_radius_x=300 block_radius_y=20 standard_deviations=4 stretch");
		run("16-bit");
		run("Gaussian Blur...", "sigma=1 scaled");
		//run("Normalize Local Contrast", "block_radius_x=300 block_radius_y=20 standard_deviations=2 stretch");
		//run("16-bit");
		if (meta) {
			saveAs("Tiff", metadir + name + "_ZPMAX_RC_NLC.tif");
		}
		//setOption("BlackBackground", true);
		setAutoThreshold("Otsu dark");
		run("Convert to Mask");
		run("Invert");
	// save
		if (meta) {
			print("   Saving "+ name + "ZPMAX_RC_bin.tif");
			saveAs("Tiff", metadir + name + "_ZPMAX_RC_bin.tif");
		}
		ZPMAXCloseF = getTitle();
		run("Make Binary");
		roiManager("reset");
		run("Analyze Particles...", "size=50-Infinity include add");
		//	waitForUser("check");
	// Select most right Roi
		for (j=0 ; j<roiManager("count"); j++) {
			roiManager("select", j);
			run("Set Scale...", "distance=1 known=0.00005 pixel=1 unit=micron");
			List.setMeasurements;
  			//print(List.getList); // list all measurements
  			x = List.getValue("X");
    		roiManager("rename", x);
		}
		roiManager("Sort");
		run("Properties...", "channels=1 slices=1 frames=1 unit=micron pixel_width=[sizeX] pixel_height=[sizeY] voxel_depth=[sizeZ]");
		primroi = roiManager("count")-1; // since roi count starts from zero
		roiManager('select', primroi); // include selection of most right
	// Enlarge
		run("Enlarge...", "enlarge=10");
		run("Fit Ellipse");
		roiManager('update');
		roiManager('select', primroi);
		roiManager("save selected", metadir + name + ".zip");
		close(ZPMAXCloseMask);
		close(ZPMAXCloseF);
		close(ZPMAXClose);
		roiManager("reset");
//	############### Apical Constriction Detection ##############
		print("---- AC detection "+name+" ----    ");
		setBatchMode(true);
		selectWindow(ZPSTDD);
		// Rotate
			run("Rotate... ", "angle="+Angle+" grid=1 interpolation=Bilinear stack");
		// Crop Rectangle
			makeRectangle(0, YRect, Regwidth, Regheight);
			run("Crop");
			saveAs("Tiff", posdir + name + "_RC_ZPSTD.tif");
			ZPSTDD = getTitle();
		// Crop Primordium
		   	selectWindow(ZPSTDD);
		   	if (reg) {
		   		roiManager("Open", metadir + name + ".zip");
		   		roiManager("select", 0);
		   		//if (ts) {
		   			//} else {
		   			run("Crop");
		   			wait(100);
		   		run("Clear Outside");
		   	}
		   	roiManager("reset");
		   	roiManager("deselect");
		   	run("Select None");
		   	run("Rotate... ", "angle=0 grid=1 interpolation=Bilinear"); //necessary to get rid of selection outline
		// Morphological Filtering & Blurring
			//run("Morphological Filters", "operation=Closing element=Disk radius=50");
			run("Duplicate...", " ");
			ZPSTDDG = getTitle();
			run("Gaussian Blur...", "sigma=4 scaled");
		// Find Maxima
			run("8-bit");
		//	get normalised threshold value for point selection
			List.setMeasurements;
  			mean = List.getValue("Mean");
  			pointthresh = mean/2.5; // Dynamic AC threshold
  			pointthresh = round(pointthresh);
			run("Find Maxima...", "noise="+pointthresh+" output=[Point Selection]");
			getSelectionCoordinates(xpoints, ypoints);
			//Array.reverse(ypoints);
			roiManager("Add");
			roiManager("save", metadir + name + "_Rosettes.zip");
		//	save Arrays, put xpoints in right order
			Rlx = lengthOf(xpoints);
			RX = Array.sort(xpoints);
			RX = Array.invert(RX);
		//	put number of AC into array + print to log
			ACnum = newArray(list.length);
			ACnum[i] = Rlx;
			run("Clear Results");
			setResult("Embryo", 0, name);
			setResult("#AC", 0, Rlx);
			updateResults();
			saveAs("Results", datdir + name + "_ACcount" + ".txt");
			run("Clear Results");
			print("# Constricting areas: "+Rlx);
		//	Fill ypoints with mean values of all y coordinates
			Array.getStatistics(ypoints, min, max, mean, stdDev);
			meanline = mean;
			Array.fill(ypoints, meanline);
			RY = ypoints;
		// Measure Intensities along horizontal line
			selectWindow(ZPMAX);
			roiManager("reset");
			roiManager("Open", metadir + name + ".zip");
		   	roiManager("select", 0);
			run("Crop");
			run("Clear Outside");
			run("Rotate... ", "angle=0 grid=1 interpolation=Bilinear"); //necessary to get rid of selection outline
			getDimensions(width, height, channels, slices, frames);
			makeLine(0, meanline, width, meanline, 1);
			run("Clear Results");
			profile = getProfile();
			for (a=0; a<profile.length; a++) {
  				setResult("Value", a, profile[a]);
				updateResults();
			}
			saveAs("Results", intensdir + name + ".txt");
			run("Clear Results");
			if (isOpen("Results")) { 
			selectWindow("Results"); 
			run("Close");} 
			close(ZPMAX);
		// Save capture
			//roiManager("reset");
			selectWindow(ZPSTDD);
			roiManager("deselect");
			run("Select None");
			roiManager("select", 0);
			run("Rotate... ", "angle=0 grid=1 interpolation=Bilinear"); //necessary to get rid of selection outline
			run("Point Tool...", "type=Dot color=Green size=[Extra Large] label counter=0");
			//run("Capture Image");
			saveAs("Tiff", posdir + name + "_RC_ZPSTD_Rosettes.tif");
			ZPSTDD = getTitle();
			roiManager("reset");
			close(ZPSTDDG);
		close(ZPSTDD);
		// ############### C2 3D PROCESSING #################
		   	if (multi) {
		   		selectWindow(ORGC2);
		   // Rotate
		      	run("Rotate... ", "angle="+Angle+" grid=1 interpolation=Bilinear stack");
		   // Crop
		      	makeRectangle(0, YRect, Regwidth, Regheight);
		      	run("Crop");
		   // Save
		   	  	print("   Saving: "+name+"_-C2_RC.tif");
		      	saveAs("Tiff", C2dir + name + "-C2_RC.tif");
		       	ORGC2 = getTitle();
		   // label specific actions
		      	if (C2label=="H2BRFP") {
		      		run("Gaussian Blur 3D...", "x=3 y=3 z=3");
		      	}
		      	if (C2label=="Arl13b") {
		      		run("Z Project...", "projection=[Average Intensity]");
		      		AVG = getTitle();
		      		imageCalculator("Subtract stack", ORGC2, AVG);
		      		close(AVG);
		      		run("Gaussian Blur 3D...", "x=1 y=1 z=2");
		      	}
		   	  	saveAs("Tiff", C2dir + name + "-C2_RC_Pre-P.tif");
		   	  	ORGC2 = getTitle();;
		      	roiManager("select", 0);
		      	//run("Clear Outside", "stack");
		      	run("Crop");
		   	}
		// ############### C1 3D PROCESSING #################
		   // Deconvolution
		   if (decon) {
		   	if (multi) {
		   	 selectWindow(ORGC1);
		   	} else {
		   	 selectWindow(ORG);}
		   	saveAs("Tiff", posdir + name + ".tif");
		   	filedir = getDirectory("image");
		   	close();
		   	file = name + ".tif" + File.separator;
		   	filepath = filedir + file;
		   	//print("before: "+filepath);
		   	filepath = replace(filepath,"\\","/");
		   	print("filepath: "+filepath);
		   	PSF = replace(PSF,"\\","/");
		   	PSF = " -psf file "+PSF;
		   	print(PSF);
		   	image = " -image file " + filepath;
			algorithm = " -algorithm RL " + iter;
			print("algorithm: "+algorithm);
			parameters = "";
			run("DeconvolutionLab2 Run", image + PSF + algorithm + parameters);
			ORG = getTitle();
			run("16-bit");
			File.delete(filepath);
		   }
		   
		   if (multi) {
		   	selectWindow(ORGC1);
		   	} else {
		   		selectWindow(ORG);
		   		}
		   		run("Bleach Correction", "correction=[Simple Ratio] background=0");
		   		C1BC = getTitle();
		   		run("Properties...", "channels=1 slices="+n+" frames=1 unit=microns pixel_width="+sizeX+" pixel_height="+sizeY+" voxel_depth="+sizeZ);
		   		if (multi) {
		   		selectWindow(ORGC1);
		   		} else {
		   			selectWindow(ORG);}
			// Rotate
		   	selectWindow(C1BC);
		   	run("Rotate... ", "angle="+Angle+" grid=1 interpolation=Bilinear stack");
			// Crop
		   	makeRectangle(0, YRect, Regwidth, Regheight);
		   	run("Crop");
			// Save
		   	C1BC = getTitle();  // Name ORG changed by saving
		   	C1F = getTitle();
		   	//run("Select None");
		   	roiManager("reset");
		   	roiManager("Open", metadir + name + ".zip");
		   	selectWindow(C1BC);
		  // clear outside
		   	if (reg) {
		   	roiManager("select", 0);
		   		if (ts) {} else {
		   		run("Crop");
		   		}
		   	run("Clear Outside", "stack");
		   	}
		  // Save
		   	if (multi) {
		   		print("   Saving: "+name+"_-C1_RC.tif");
		   		saveAs("Tiff", C1dir + name + "-C1_RC.tif");
		   		} else {
		   		print("   Saving: "+name+"_RC.tif");
		   		saveAs("Tiff", posdir + name + "_RC.tif");
		   		}
		   	C1BC = getTitle();  // Name ORG changed by saving
		  // morphological filtering
		   	if(C1label==515) {
		   		run("Morphological Filters (3D)", "operation=Closing element=Ball x-radius=1 y-radius=1 z-radius=0.5");
		   		} else { // C1Label==-139
		   		run("Duplicate...", "duplicate");
		   		}
		    C1F = getTitle();
		    setBatchMode("exit and display");

//	###############################################################################################
//	##################################### CREATING OBJECTS ########################################
//	###############################################################################################

// 	############################### MEBRANES #####################################
// 	###################### MORPHOLOGICAL SEGMENTATIONs ###########################

			print("---- segmenting cells ----    ");
			selectWindow(C1F);
			setSlice(n/2);
			//saveAs("Tiff", posdir + name + "-C1_RC.tif");
			//run("Enhance Contrast", "saturated=0.35");
		//	3D gaussian blur
			run("Gaussian Blur 3D...", "x=2 y=2 z=0.5");
			resetMinAndMax();
		//	run segmentation (should be done using groovy)
			run("Morphological Segmentation");
			wait(2000);
			selectWindow("Morphological Segmentation");
			call("inra.ijpb.plugins.MorphologicalSegmentation.setInputImageType", "border");
			call("inra.ijpb.plugins.MorphologicalSegmentation.segment", "tolerance="+tol+"", "calculateDams=true", "connectivity=6"); //30ms exposure
			wait(100000);
			call("inra.ijpb.plugins.MorphologicalSegmentation.setDisplayFormat", "Catchment basins");
			call("inra.ijpb.plugins.MorphologicalSegmentation.createResultImage");
			run("Properties...", "channels=1 slices="+n+" frames=1 unit=microns pixel_width="+sizeX+" pixel_height="+sizeY+" voxel_depth="+sizeZ);
		// save metadata for debugging
			if (meta) {
			if (multi) {
		   	saveAs("Tiff", C1dir + name + "-C1_RC_MS.tif");
		   	} else {
		   		saveAs("Tiff", posdir + name + "_RC_MS.tif");}
		   	}
			C1CB = getTitle();
		// run gray LUT
			run("Grays");
		// close 
			selectWindow("Morphological Segmentation");
			close();
			selectWindow(C1F);
			close();
			
		// ######## ########################################################################### ########
		// ############################### CLEAN AND SAVE OBJECTS MAP ##################################
		// ######## ########################################################################### ########
		
			selectWindow(C1CB);
			run("16-bit");
			run("3D Exclude Borders", "remove");
			rename(name + "_RC_OMap.tif");
			OMap = getTitle();
			
		// ############################# Erase objects V < vmin and V > vmax ##############################
		
			print("---	Erase objects with a volume < "+vmin+"and < "+vmax+"	---");
			run("3D Manager Options", "volume surface compactness fit_ellipse 3d_moments feret centroid_(pix) centroid_(unit) distance_to_surface centre_of_mass_(unit) bounding_box radial_distance surface_contact closest exclude_objects_on_edges_xy sync distance_between_centers=10 distance_max_contact=1.80");
			run("3D Manager");
			wait(500);
			selectWindow(OMap);
			Ext.Manager3D_AddImage();
			Ext.Manager3D_Count(nb); //get number of objects
			Ext.Manager3D_MultiSelect();
			for(k=0; k<nb; k++) { // loop through all the objects and erase by filter settings
				showStatus("Processing "+k+"/"+nb);
				Ext.Manager3D_Measure3D(k,"Vol",V);
				if (V < vmin) {
				//print("volume: "+V+" Erased");
				Ext.Manager3D_Select(k);
				Ext.Manager3D_Erase();
					if (V > vmax) {
					//print("volume: "+V+" Erased");
					Ext.Manager3D_Select(k);
					Ext.Manager3D_Erase();
				}
			  }
			}
			if (manual) {
			waitForUser("Ready for manual editing \n Click 'OK' if finished");
			}
			Ext.Manager3D_Reset();
			Ext.Manager3D_Close();
			//waitForUser("Wait");
			wait(500);
			selectWindow(name + "_RC_OMap.tif");
			OMap = getTitle();
			
		// ######################### Clean blank slices from bottom and top #############################
		
			selectWindow(OMap);
			getDimensions(width, height, channels, slices, frames);
			var done = false; 
			for(l = 1; l < slices &&!done; l++) {
				setSlice(l);
				getStatistics(area, mean, min, max, std, histogram);
				if(max > 0) {
				amax = l-1;
				run("Slice Remover", "first=1 last="+amax+" increment=1");
				run("Reverse");
				getDimensions(width, height, channels, slices, frames);
				//done = true;
				for(l = 1; l < slices &&!done; l++) {
					setSlice(l);
					getStatistics(area, mean, min, max, std, histogram);
					if(max > 0) {
					bmax = l-1;
					run("Slice Remover", "first=1 last="+bmax+" increment=1");
					run("Reverse");
					done = true;
					}
				}
				}
			}
			
		// ######################### Save #############################
		
	   		if (multi) {
			saveAs("Tiff", C1dir + name + "-C1_RC_OMap.tif");
			} else {
				saveAs("Tiff", posdir + name + "_RC_OMap.tif");
			}
	   		OMap = getTitle();
			close(C1CB);
			n = nSlices();
			run("Properties...", "channels=1 slices="+n+" frames=1 unit=microns pixel_width="+sizeX+" pixel_height="+sizeY+" voxel_depth="+sizeZ);
			
		// ######## ########################################################################### #########
		// ############################### Reconstruct f0 points in 3D ##################################
		// ######## ########################################################################### #########
		
	   		getDimensions(width, height, channels, slices, frames);
			newImage("FerretOMap", "8-bit black", width, height, n);
			run("Properties...", "channels=1 slices="+n+" frames=1 unit=microns pixel_width="+sizeX+" pixel_height="+sizeY+" voxel_depth="+sizeZ);
			FerretOMap = getTitle();
			run("3D Manager Options", "volume surface compactness fit_ellipse 3d_moments feret centroid_(pix) centroid_(unit) distance_to_surface centre_of_mass_(unit) bounding_box radial_distance surface_contact closest exclude_objects_on_edges_xy sync distance_between_centers=10 distance_max_contact=1.80");
			run("3D Manager");
			wait(100);
			selectWindow(OMap);
			Ext.Manager3D_AddImage();
			Ext.Manager3D_Count(nb); //get number of objects
			fx0array = newArray(nb);
			fy0array = newArray(nb);
			fz0array = newArray(nb);
			fx1array = newArray(nb);
			fy1array = newArray(nb);
			fz1array = newArray(nb);
			objlabelArray = newArray(nb);
			selectWindow(FerretOMap);
			Ext.Manager3D_MonoSelect();
			for(k=0; k<nb; k++) {
				showStatus("Processing "+k+"/"+nb);
				Ext.Manager3D_Measure3D(k,"Feret",ferr);
				Ext.Manager3D_GetName(k,obj);
				Ext.Manager3D_Feret1(k,fx0,fy0,fz0);
				//print("Object nb:"+name+" feret1: "+fx0+" "+fy0+" "+fz0);
				toScaled(fx0);
				toScaled(fy0);
				toScaled(fz0);
				fx0array[k] = fx0;
	   		    fy0array[k] = fy0;
	   		    fz0array[k] = fz0;
				Ext.Manager3D_Feret2(k,fx1,fy1,fz1);
				//print("Object nb:"+name+" feret1: "+fx1+" "+fy1+" "+fz1);
				toScaled(fx1);
				toScaled(fy1);
				toScaled(fz1);
				fx1array[k] = fx1;
	   		    fy1array[k] = fy1;
	   		    fz1array[k] = fz1;
	   		    objlabelArray[k] = obj;
				//print("Object nb:"+name+" feret2: x="+fx1+" y="+fy1+" z="+fz1);
				run("3D Draw Shape", "size="+width+","+height+","+slices+" center="+fx0+","+fy0+","+fz0+" radius="+sizeX+","+sizeY+","+sizeZ+" vector1=1.0,0.0,0.0 vector2=0.0,1.0,0.0 res_xy="+sizeX+" res_z="+sizeZ+" unit=microns value=255 display=Overwrite ");
			}
			setSlice(n/2);
			//run("3D Draw Shape", "size="+width+","+height+","+slices+" center="+fx1+","+fy1+","+fz1+" radius=1,1,1 vector1=1,1,1 vector2=1,1,1 res_xy=1.000 res_z=1.000 unit=pix value=&val display=Overwrite ");
			run("Z Project...", "projection=[Max Intensity]");
			saveAs("Tiff", posdir + name + "_RC_OMap_Ferret0PointMax" + ".tif");
			FerretOMapMax = getTitle();
			close(FerretOMapMax);
			Ext.Manager3D_Reset();
			Ext.Manager3D_Close();
			run("Enhance Contrast...", "saturated=0.3 equalize process_all");
			// close shit
			close(FerretOMap);
			close(FerretOMapMax);
			Array.show("Results (row numbers)", objlabelArray, fx0array, fy0array, fz0array, fx1array, fy1array, fz1array);
			saveAs("results", datcelldir + name + "_ferretcoords" + ".txt");
			results();
			wait(500);
			
		// ######## ########################################################################### #########
		// ######## ############################# Define Z values ############################# #########
		// ######## ########################################################################### #########
			
			Zslice = rosum/sizeZ;
			aciza = cellum/sizeZ;
			round(Zslice);
			round(aciza);
			
		// ######## ########################################################################### #########
		// ######## #################### Analyze Cell Constriction ############################ #########
		// ######## ########################################################################### #########
		
			if (cellAC||ACId) {
			print("---- Analysis of Cell Constriction ----    ");
		//	######## Create directory ######## 
	   		celldir = imgdir + name + File.separator + "cell_volumes" + File.separator;
			File.makeDirectory(celldir);
		//	######## crop single cells and save to new dir ########
	   	//	Crop single objects and measure
	   		selectWindow(OMap);
			run("3D Manager Options", "volume surface compactness fit_ellipse 3d_moments feret centroid_(pix) centroid_(unit) distance_to_surface centre_of_mass_(unit) bounding_box radial_distance surface_contact closest exclude_objects_on_edges_xy sync distance_between_centers=10 distance_max_contact=1.80");
			run("3D Manager");
			wait(100);
			Ext.Manager3D_AddImage();
			Ext.Manager3D_Count(nb); //get number of objects
		//	create arrays to fill with measurements
			objlabelArray = newArray(nb);
			MajorAngle = newArray(nb);
			//baAngle = newArray(nb);
			if (ACM=="fit") {
			ACIMajor = newArray(nb);
			ACIMinor = newArray(nb);
			//baFMax = newArray(nb);
			//baFMin = newArray(nb);
			Dap = newArray(nb);
			}
			if (ACM=="bounding") {
			apbw = newArray(nb);
			apbh = newArray(nb);
			//babw = newArray(nb);
			//babh = newArray(nb);
			}
		//	set multiselect
			Ext.Manager3D_MultiSelect();
			for(k = 0; k < nb; k++) {
				selectWindow(OMap);
				run("Duplicate...", "duplicate");
				OMapDup = getTitle();
				if (k > 0) {Ext.Manager3D_AddImage();}
				Ext.Manager3D_GetName(k,obj);
				Ext.Manager3D_Centroid3D(k, cx, cy, cz);
				if (ACId) { // if dynamic ACI
					//Ext.Manager3D_Bounding3D(k, x0, x1, y0, y1, z0, z1);
					Ext.Manager3D_Measure3D(k,"Feret", ferr); 
					//print("zmin: "+z0+" zmax: "+z1);
					//cheight = (z1-z0)*sizeZ;
					print("  Feret:"+ferr);
					//da = cheight*cellum;
					da = ferr*cellum;
					print("ACId: "+da);
					da = round(da);
					if (da==0) {
						da = 1;
					}
					print("  proportional delta: "+da);
					Dap[k] = da;
				}
				//waitForUser("Check log for height and acid");
				toString(obj);
				objlabelArray[k] = obj;
				Ext.Manager3D_SelectAll();
				Ext.Manager3D_Select(k);
				Ext.Manager3D_Erase();
				run("Enhance Contrast...", "saturated=0.3 equalize process_all");
				run("8-bit");
				run("Crop Label", "label=255 border=5");
		//	CLEAR STACK IN Z
				getDimensions(width, height, channels, slices, frames);
				var done = false; 
				for(l = 1; l < slices &&!done; l++) { 
					setSlice(l);
					getStatistics(area, mean, min, max, std, histogram);
					if(max > 0) { // from apical
					smax = l-1;
					run("Slice Remover", "first=1 last="+smax+" increment=1");
					run("Reverse");
					getDimensions(width, height, channels, slices, frames);
					for(l = 1; l < slices &&!done; l++) { // from basal
						setSlice(l);
						getStatistics(area, mean, min, max, std, histogram);
						if(max > 0) {
						smax = l-1;
						run("Slice Remover", "first=1 last="+smax+" increment=1");
						run("Reverse");
						done = true;
						}
					}
					}
				}
				primcell = getTitle();
				naci = nSlices();
				nacimax = naci/2;
				run("Properties...", "channels=1 slices="+naci+" frames=1 unit=microns pixel_width="+sizeX+" pixel_height="+sizeY+" voxel_depth="+sizeZ);
				if (ACId) {
					aciza = da/sizeZ;
					db = naci - da;
				}
				if (aciza < (nacimax)) { 	// if fixed ACI
				acizb = naci - aciza;
		//	MEASURE APICAL
				selectWindow(primcell);
				run("Make Binary", "method=Default background=Default calculate black");
				setSlice(aciza);
				//waitForUser("right Z plane?");
				run("Set Measurements...", "area centroid bounding fit feret's redirect=None decimal=2");
				run("Analyze Particles...", "display slice");
			//	Get Angle
				MajorAngle[k] = getResult("FeretAngle", 0);
				//print("meanline: "+meanline+", CY: "+cy);
				if (MajorAngle[k] > 90) {
				//	apAngle[k] = apAngle[k]+180;
				//}
				MajorAngle[k] = 90-(MajorAngle[k]-90);
				}
			//	Get Max Feret / bounding width / area / Major
				resultsArray = newArray(nResults());
				if (ACM == "fit") {
					for(p = 0; p < nResults(); p++) { 
						resultsArray[p] = getResult("Major", p);	
  					}
  					total = 0; 
					for(p = 0; p < nResults(); p++) { 
 						total = total + resultsArray[p]; 
					}
				}
				if (ACM=="fit") {
					ACIMajor[k] = total;
				}
				if (ACM=="bounding") {
					apbw[k] = total;
				}
			//	Get Min Feret / bounding height / Minor
				resultsArray = newArray(nResults());
				if (ACM == "fit") {
					for(p = 0; p < nResults(); p++) { 
       					resultsArray[p] = getResult("Minor", p); 
  					}
  					total = 0; 
					for(p = 0; p < nResults(); p++) { 
 						total = total + resultsArray[p]; 
					}
				}
				if (ACM=="fit") {
					ACIMinor[k] = total;
				}
				if (ACM=="bounding") {
					apbh[k] = total;
				}
				run("Clear Results");
		//	MEASURE BASAL
				//selectWindow(primcell);
				//if (ACId) {
					acizb = db;
				////setSlice(acizb);
				//run("Set Measurements...", "area centroid bounding fit feret's redirect=None decimal=2");
				//run("Analyze Particles...", "display slice");
			//	Get Angle
				//baAngle[k] = getResult("FeretAngle", 0);
				//Ythresh = getResult("Y", 0);
				//print("meanline: "+meanline+", CY: "+cy);
				//if (baAngle[k] > 90) {
				//	baAngle[k] = baAngle[k]+180;
				//}
				//baAngle[k] = 90-(baAngle[k]-90);
				//}
			//	Max Feret
				//resultsArray = newArray(nResults());
				//if (ACM == "fit") {
				//	for(p = 0; p < nResults(); p++) { 
       			//		resultsArray[p] = getResult("Major", p); 
  				//	}
  				//	total = 0; 
				//	for(p = 0; p < nResults(); p++) { 
 				//		total = total + resultsArray[p]; 
				//	}
				//}
				//if (ACM=="fit") {
				//	baFMax[k] = total;
				//}
				//if (ACM=="bounding") {
				//	babw[k] = total;
				//}
			//	Min Feret
				//resultsArray = newArray(nResults());
				//if (ACM == "fit") {
				//	for(p = 0; p < nResults(); p++) { 
       			//		resultsArray[p] = getResult("Minor", p); 
  				//	}
  				//	total = 0; 
				//	for(p = 0; p < nResults(); p++) { 
 				//		total = total + resultsArray[p]; 
				//	}
				//}
				//if (ACM=="fit") {
				//	baFMin[k] = total;
				//}
				//if (ACM=="bounding") {
				//	babh[k] = total;
				//}
				//run("Clear Results");
				} else {
					MajorAngle[k] ="NaN";
				//	baAngle[k] ="NaN";
					if (ACM=="fit") {
					ACIMajor[k] = "NaN";
					ACIMinor[k] = "NaN";
				//	baFMax[k] = "NaN";
				//	baFMin[k] = "NaN";
					}
					if (ACM=="bounding") {
					apbw[k] = "NaN";
					apbh[k] = "NaN";
				//	babw[k] = "NaN";
				//	babh[k] = "NaN";
					}
				}
		//	CROP & SAVE
				nCrop = nSlices();
				run("Properties...", "channels=1 slices="+nCrop+" frames=1 unit=microns pixel_width="+sizeX+" pixel_height="+sizeY+" voxel_depth="+sizeZ);
				saveAs("Tiff", celldir + name + "_" + obj + ".tif");
				close();
				close(OMapDup);
				Ext.Manager3D_Reset();
				} // for loop cells
				Ext.Manager3D_Close();
				if (ACM=="fit") {
				Array.show("Results (row numbers)", objlabelArray, ACIMajor, ACIMinor, MajorAngle, Dap);
				}
				if (ACM=="bounding") {
				Array.show("Results (row numbers)", objlabelArray, apbw, apbh, MajorAngle, babw, babh, baAngle);
				}
				saveAs("results", datcelldir + name + "_ACI" + ".txt");
				results();
				wait(500);
			}
			
		// ######## ########################################################################### #########
		// ######## ###################### Analyze Rosette Constriction ####################### #########
		// ######## ########################################################################### #########
		
			if (rosAC) {
	   		print("---- Analysis of Rosette Constriction ----    ");
			radius=100; 
			if (Rlx > 0) {
			results(); // see functions
	   		for (j = 0; j < Rlx; j++) { //RosN = Rosette Number
	   			print("    -- Rosette #"+j+" --    ");
	   			pos = j+1;
	   		// ######## Create directory ######## 
	   			rosdir = imgdir + name + File.separator + "Ros" + pos + File.separator;
				File.makeDirectory(rosdir);
			// ######## Crop Rosette ########
	   			selectWindow(OMap);
	   			run("Duplicate...", "duplicate");
	   			OMapD = getTitle();
	   			makeOval(RX[j]-radius, RY[j]-radius, 2*radius, 2*radius); 
	   			run("Crop");
	   			run("3D Exclude Borders", " "); // Z already removed
	   			selectWindow("Objects_removed");
	   			n = nSlices();
	   			run("Properties...", "channels=1 slices="+n+" frames=1 unit=microns pixel_width="+sizeX+" pixel_height="+sizeY+" voxel_depth="+sizeZ);
	   			saveAs("Tiff", rosdir + name + "_RC_OMap" + "_R" + pos + ".tif");
	   			R = getTitle();
	   			close(OMapD);
	   		// ######## crop single cells and save to new dir ########
	   			selectWindow(R);
				run("3D Manager Options", "volume surface compactness fit_ellipse 3d_moments feret centroid_(pix) centroid_(unit) distance_to_surface centre_of_mass_(unit) bounding_box radial_distance surface_contact closest exclude_objects_on_edges_xy sync distance_between_centers=10 distance_max_contact=1.80");
				run("3D Manager");
				wait(100);
				Ext.Manager3D_AddImage();
				Ext.Manager3D_Count(nb); //get number of objects
				Ext.Manager3D_MultiSelect();
				for(k=0; k<nb; k++) {
					selectWindow(R);
					run("Duplicate...", "duplicate");
					if (k>0) {Ext.Manager3D_AddImage();}
					Ext.Manager3D_GetName(k,obj);
					Ext.Manager3D_SelectAll();
					Ext.Manager3D_Select(k);
					Ext.Manager3D_Erase();
					run("Enhance Contrast...", "saturated=0.3 equalize process_all");
					run("8-bit");
					roscell = getTitle();
					run("Crop Label", "label=255 border=20");
					nCrop = nSlices();
					run("Properties...", "channels=1 slices="+nCrop+" frames=1 unit=microns pixel_width="+sizeX+" pixel_height="+sizeY+" voxel_depth="+sizeZ);
					saveAs("Tiff", rosdir + name + "_R" + pos + "_" + obj + ".tif");
					close();
					close(roscell);
					Ext.Manager3D_Reset();
				}
				Ext.Manager3D_Close();
	   		// ### MEASURE SINGLE ROSETTE CELLS ###
	   			selectWindow(R);
	   			run("3D Manager Options", "volume surface compactness fit_ellipse 3d_moments feret centroid_(pix) centroid_(unit) distance_to_surface centre_of_mass_(unit) bounding_box radial_distance surface_contact closest exclude_objects_on_edges_xy sync distance_between_centers=10 distance_max_contact=1.80");
	   			run("3D Manager");
	   			wait(100);
	   			Ext.Manager3D_AddImage();
	  			Ext.Manager3D_DeselectAll(); // to refresh RoiManager
	   			Ext.Manager3D_Measure();
	   			Ext.Manager3D_SaveResult("M", datrosdir + name + "cells_R" + pos + ".csv");
	   			Ext.Manager3D_CloseResult("M");
	   			Ext.Manager3D_Reset();
	   			Ext.Manager3D_Close();
	   			selectWindow(R);
	   			run("Duplicate...", "duplicate");
	   			RD = getTitle();
	   			run("8-bit");
	   		// ### MEASURE WHOLE ROSETTE MERGED ###
				run("Enhance Contrast...", "saturated=0 equalize process_all");
				run("Dilate", "stack");
				run("Erode", "stack");
				run("3D Manager Options", "volume surface compactness fit_ellipse 3d_moments centroid_(pix) centroid_(unit) distance_to_surface centre_of_mass_(unit) bounding_box radial_distance surface_contact closest exclude_objects_on_edges_xy sync distance_between_centers=10 distance_max_contact=1.80");
				run("3D Manager");
				wait(100);
				Ext.Manager3D_Segment(128, 255);
				RP = getTitle();
				Ext.Manager3D_AddImage();
				Ext.Manager3D_Count(nb_obj);
				if (nb_obj > 1) {
					Ext.Manager3D_SelectAll();
					Ext.Manager3D_Merge();
				}
				Ext.Manager3D_Measure();
				Ext.Manager3D_SaveResult("M", datrosdir + name + "whole_R" + pos + ".csv");
       			Ext.Manager3D_CloseResult("M");
       			Ext.Manager3D_Reset();
				Ext.Manager3D_Close();
				close(RP);
				close(RD);
	   		// ### MEASURE APICAL CONSTRICTION ###
	   		if (rosAC) {
	   		// ######## Draw Feret lines ########
	   			selectWindow(R);
				getDimensions(width, height, channels, slices, frames);
				var done = false; 
				for(l=1; l<slices &&!done; l++) {
					setSlice(l);
					List.setMeasurements;
  					mean = List.getValue("Mean");
					if(mean > 10) {
						done = true;
						s1 = getSliceNumber();
						}
				}
				if (s1 == n) { } else {
				run("Make Substack...", "  slices=" + ""+s1+"-"+slices);
				Rreslice = getTitle();
				n = nSlices();
				run("Properties...", "channels=1 slices="+n+" frames=1 unit=microns pixel_width="+sizeX+" pixel_height="+sizeY+" voxel_depth="+sizeZ);
	   			//selectWindow(R);
	   			getDimensions(width, height, channels, slices, frames);
				newImage("Ferret", "8-bit black", width, height, slices);
				Ferret = getTitle();
				run("3D Manager Options", "volume surface compactness fit_ellipse 3d_moments feret centroid_(pix) centroid_(unit) distance_to_surface centre_of_mass_(unit) bounding_box radial_distance surface_contact closest exclude_objects_on_edges_xy sync distance_between_centers=10 distance_max_contact=1.80");
				run("3D Manager");
				wait(100);
				selectWindow(Rreslice);
				Ext.Manager3D_AddImage();
				Ext.Manager3D_Count(nb); //get number of objects
				selectWindow(Ferret);
				Ext.Manager3D_MonoSelect();
				for(k=0; k<nb; k++) {
					showStatus("Processing "+k+"/"+nb);
					Ext.Manager3D_Measure3D(k,"Feret",ferr);
					Ext.Manager3D_GetName(k,obj);
					Ext.Manager3D_Feret1(k,fx0,fy0,fz0);
					//print("Object nb:"+name+" feret1: "+fx0+" "+fy0+" "+fz0);
					Ext.Manager3D_Feret2(k,fx1,fy1,fz1);
					//print("Object nb:"+name+" feret2: x="+fx1+" y="+fy1+" z="+fz1);
					//run("3D Draw Line", "size_x=&width size_y=&height size_z=&slices x0=&fx0 y0=&fy0 z0=&fz0 x1=&fx1 y1=&fy1 z1=&fz1 thickness=1.000 value=255 display=Overwrite ");
					run("3D Draw Line", "size_x=&width size_y=&height size_z=&slices x0=&fx0 y0=&fy0 z0=&fz0 x1=&fx1 y1=&fy1 z1=&fz1 thickness=1.000 value=&ferr display=Overwrite ");
					setSlice(slices/2);
					}
				Ext.Manager3D_Reset();
				Ext.Manager3D_Close();
				selectWindow(Ferret);
				saveAs("Tiff", rosdir + name + "_RC_OMap" + "_R" + pos + "_FerretLines" + ".tif");
				FerretS = getTitle();
				run("Enhance Contrast...", "saturated=0.3 equalize process_all");
				// ######## get coordinates or feret lines @ .25% of the stack size ########
				Zslice = um/sizeZ;
				round(Zslice);
				setSlice(Zslice);
					// Duplicate and save for projection
				run("Duplicate...", " ");
				if (j == 0) {
					run("Label...", "format=Text starting=0 interval=[] x=5 y=20 font=18 text=[Z "+Zslice+"] range=1-1");
				}
				saveAs("Tiff", metadir + "R" + pos + "_" + name + "_FerrD" + ".tif");
				FerretSD = getTitle();
				//close(FerretSD);
				selectWindow(FerretS);
				// check if there are actually any points
				List.setMeasurements; 
  				mean = List.getValue("Mean");
 				if (mean > 0) {
					run("Find Maxima...", "noise=0 output=[Point Selection]");
					saveAs("XY Coordinates", datdir + "XY" + "_R" + pos + name + ".txt");
					getSelectionCoordinates(xpoints, ypoints);
					FX = xpoints;
					FY = ypoints;
					Ferrlx = lengthOf(xpoints)-1;
					selectWindow(FerretS);
					run("Select None");
					// ####### Fit circle #######
					if (Ferrlx > 2) {
						selectWindow(FerretSD);
						run("Find Maxima...", "noise=0 output=[Single Points]");
						FerretSDD = getTitle();
						run("Points from Mask");
						run("Fit Ellipse");
						//run("Set Measurements...", "area bounding fit shape nan redirect=None decimal=2");
						run("Set Measurements...", "bounding fit shape feret's nan redirect=None decimal=2");
						run("Measure");
						close(FerretSDD);
					}
					close(FerretSD);
					Dist = newArray(0); // Array will be concatenated in for loop
					for (m = 0; m < Ferrlx ; m++) {
						if (m==0) {Ferrlx = Ferrlx;} else {Ferrlx = Ferrlx-1;}
						for (n = 0; n < Ferrlx; n++) {
							if (n==0) {posi = n;} else {posi = n+1;}
							ED = sqrt((FX[Ferrlx]-FX[Ferrlx-posi])*(FX[Ferrlx]-FX[Ferrlx-posi])+(FY[Ferrlx]-FY[Ferrlx-posi])*(FY[Ferrlx]-FY[Ferrlx-posi]));
							if (ED > 0){
							Dist = Array.concat(Dist, ED);
							}
						}
					}
					Array.getStatistics(Dist, min, max, mean, stdDev);
					setResult("Rosette", j, "R"+pos);
					setResult("Min", j, min);
					setResult("Max", j, max);
					setResult("Mean", j, mean);
					setResult("StdDev", j, stdDev);
					updateResults();
  				} // if(mean>0) 
				}  //if n == nR
				close(R);
				close(Ferret);
				close(FerretS);
				close(Rreslice);
	   		} // for loop
			rn = nResults();
			for (o = 1; o < rn ; o++) {
				res = rn-o;
				rosi = getResult("Rosette", res);
				if (rosi == 0) {
				IJ.deleteRows(res,res);
				}
			}
			updateResults();
	   		saveAs("results", datdir + "ACM_" + name + ".txt");
	   		results();
	   		}
	   }
	   } // if rosAC
	   selectWindow(OMap);
	   getDimensions(width, height, channels, slices, frames);
	   
	   	// ######## ################################################################################# #########
		// ######## ###################### Measure single cells of whole prim ####################### #########
		// ######## ################################################################################# #########
	   	run("3D Manager Options", "volume surface compactness fit_ellipse 3d_moments feret centroid_(pix) centroid_(unit) distance_to_surface centre_of_mass_(unit) bounding_box radial_distance surface_contact closest exclude_objects_on_edges_xy sync distance_between_centers=10 distance_max_contact=1.80");
	   	run("3D Manager");
	   	wait(100);
	   	Ext.Manager3D_AddImage();
	   	Ext.Manager3D_DeselectAll(); // to refresh RoiManager
	   	Ext.Manager3D_Measure();
	   	Ext.Manager3D_SaveResult("M", datprimdir + name + "_cells" + ".csv");
	   	Ext.Manager3D_CloseResult("M");
	   	Ext.Manager3D_Reset();
	   	Ext.Manager3D_Close();
	   
	  	// ######## ########################################################################### #########
		// ######## ######################### Reconstruct Feret lines ######################### #########
		// ######## ########################################################################### #########
		
	   if (ferrr) {
	   		run("Clear Results");
	   		selectWindow(OMap);
	   		getDimensions(width, height, channels, slices, frames);
	   		run("3D Manager");
	   		wait(100);
			Ext.Manager3D_AddImage();
			Ext.Manager3D_Count(nb); //get number of objects
			fxarray = newArray(nb);
			fyarray = newArray(nb);
			fzarray = newArray(nb);
	   		Ext.Manager3D_MonoSelect();
	   		for(k=0; k<nb; k++) {
				showStatus("Processing "+k+"/"+nb);
				Ext.Manager3D_Measure3D(k,"Feret",ferr);
				Ext.Manager3D_GetName(k,obj);
				Ext.Manager3D_Feret1(k,fx0,fy0,fz0);
				Ext.Manager3D_Feret2(k,fx1,fy1,fz1);
				fx = fx1-fx0;
				fy = fy1-fy0;
				fz = fz1-fz0;
	   		    fxarray[k] = fx;
	   		    fyarray[k] = fy;
	   		    fzarray[k] = fz;
	   		}
	   		Ext.Manager3D_Reset();
			Ext.Manager3D_Close();
	   		Array.getStatistics(fxarray, min, max, mean, stdDev);
	   		fxmax = max*2;
	   		Array.getStatistics(fyarray, min, max, mean, stdDev);
	   		fymax = max*2;
	   		Array.getStatistics(fzarray, min, max, mean, stdDev);
	   		fzmax = max*2;
	   		//getDimensions(width, height, channels, slices, frames);
			newImage("Ferret", "8-bit black", fxmax, fymax, slices);
			Ferret = getTitle();
			run("3D Manager");
			wait(100);
			selectWindow(OMap);
			Ext.Manager3D_AddImage();
			Ext.Manager3D_Count(nb); //get number of objects
			selectWindow(Ferret);
			Ext.Manager3D_MonoSelect();
			for(k=0; k<nb; k++) {
				showStatus("Processing "+k+"/"+nb);
				Ext.Manager3D_Measure3D(k,"Feret",ferr);
				Ext.Manager3D_GetName(k,obj);
				Ext.Manager3D_Feret1(k,fx0,fy0,fz0);
				//print("Object nb:"+name+" feret1: "+fx0+" "+fy0+" "+fz0);
				Ext.Manager3D_Feret2(k,fx1,fy1,fz1);
				//print("Object nb:"+name+" feret2: x="+fx1+" y="+fy1+" z="+fz1);
				setResult("obj", k, obj);
				setResult("fx0", k, fx0);
				setResult("fy0", k, fy0);
				setResult("fz0", k, fz0);
				setResult("fx1", k, fx1);
				setResult("fy1", k, fy1);
				setResult("fz1", k, fz1);
				fx1 = (fx1-fx0)+(fxmax/2);
				fx0 = fxmax/2;
				fy1 = (fy1-fy0)+(fymax/2);
				fy0 = fymax/2;
				fz1 = (fz1-fz0)+(fzmax/2);
				fz0 = 0;
				//run("3D Draw Line", "size_x=&width size_y=&height size_z=&slices x0=&fx0 y0=&fy0 z0=&fz0 x1=&fx1 y1=&fy1 z1=&fz1 thickness=1.000 value=255 display=Overwrite ");
				//run("3D Draw Line", "size_x=&width size_y=&height size_z=&slices x0= y0=&fy0 z0=&fz0 x1=&fx1 y1=&fy1 z1=&fz1 thickness=1.000 value=&ferr display=Overwrite ");
				run("3D Draw Line", "size_x=&width size_y=&height size_z=&slices x0=&fx0 y0=&fy0 z0=0 x1=&fx1 y1=&fy1 z1=&fz1 thickness=1.000 value=255 display=Overwrite ");
				setSlice(slices/2);
				}
			Ext.Manager3D_Reset();
			Ext.Manager3D_Close();
			saveAs("results", datdir + name + "_ferretcords" + ".txt");
			selectWindow(Ferret);
			saveAs("Tiff", posdir + name + "_RC_OMap" + "_FerretLinesPrimNorm" + ".tif");
			close();
			results();
	   }
	   results();
	   wait(1000);
	   selectWindow(OMap);
	   run("8-bit");
    // CREATE COMPOSITE
       selectWindow(C1BC);
       	run("8-bit");//run("Crop");
       	run("Slice Remover", "first=1 last="+amax+" increment=1");
       	run("Reverse");
       	run("Slice Remover", "first=1 last="+bmax+" increment=1");
       	run("Reverse");
	   	//run("Merge Channels...", "c2="+OMap+" c4="+C1BC+" create keep");
	   	close(C1BC);
	   	//selectWindow("Composite");
	   	//if (multi) {
	   	//saveAs("Tiff", C1dir + name + "-C1_RC_OMap_Composite.tif");
			//} else {
			//saveAs("Tiff", posdir + name + "_RC_OMap_Composite.tif");
		//}
	   //close();
	   
	   	// ######## ################################################################################ #########
		// ######## ###################### Measure merged cells / whole Prim ####################### #########
		// ######## ################################################################################ #########

		selectWindow(OMap);
		run("Enhance Contrast...", "saturated=0 equalize process_all");
		run("Make Binary", "method=Default background=Default calculate black");
		run("Dilate", "stack");
		run("Erode", "stack");
		run("3D Manager Options", "volume surface compactness fit_ellipse 3d_moments centroid_(pix) centroid_(unit) distance_to_surface centre_of_mass_(unit) bounding_box radial_distance surface_contact closest exclude_objects_on_edges_xy sync distance_between_centers=10 distance_max_contact=1.80");
		run("3D Manager");
		wait(100);
		Ext.Manager3D_Segment(128, 255);
		OMapP = getTitle();
		Ext.Manager3D_AddImage();
		Ext.Manager3D_Count(nb_obj);
		if (nb_obj > 1) {
			Ext.Manager3D_SelectAll();
			Ext.Manager3D_Merge();
		}
		Ext.Manager3D_Measure();
		Ext.Manager3D_SaveResult("M", datprimdir + name + "_whole" + ".csv");
       	Ext.Manager3D_CloseResult("M");
       	Ext.Manager3D_Reset();
		Ext.Manager3D_Close();
		close(OMapP);
		
	   	// ######## ################################################################################## #########
		// ######## ################################## C2 clearing ################################### #########
		// ######## ################################################################################## #########
		
		if (multi) {
		setBatchMode(true);
		selectWindow(OMap);
		if (C2label=="H2BRFP") {
			sliceclear();
		}
		if (C2label=="Arl13b") {
			primseg();
			//selectWindow(indiv+"-3Dseg");
			close(); // close -3Dseg
			wait(200);
			close(OMap);
			selectWindow(name + "-C2_RC_prim-seg.tif");
			//waitForUser("HALT");
			OMap = getTitle();
			sliceclear();
		}
		selectWindow(ORGC2);
		saveAs("Tiff", C2dir + name + "-C2_RC_clear.tif");
		C2cleared = getTitle();
		run("8-bit");
		wait(200);
		selectWindow(C2cleared);
		run("8-bit");
		wait(200);
		close(OMap);
	// 	create composite
		run("Merge Channels...", "c1="+SMap+" c2="+C2cleared+" create keep");
		selectWindow("Composite");
		saveAs("Tiff", C2dir + name + "-C2_RC_prim-seg_clear_SMap_Composite.tif");
		close();
		close(SMap);
		if (C2label == "Arl13b") {
		// 	3D Maxima finder
			selectWindow(C2cleared);
			run("3D Maxima Finder", "radiusxy=5 radiusz=10 noise=30");
			selectWindow("peaks");
			saveAs("Tiff", C2dir + name + "-C2_RC_prim-seg_peaks.tif");
			close();
			selectWindow("Results");
			saveAs("results", datdir + name + "_3DMngr_3DMax_results" + ".txt");
		}
		//waitForUser("HALT");
		results(); // see functions
		setBatchMode("exit and display");
		close(C2cleared);
    	} // if (multi)
    	// CLOSE everything
	closewindows(); // see functions
	} // list.length 
//}
	getDateAndTime(year, month, dayOfWeek, dayOfMonth, hour, minute, second, msec);
	print("finished: "+ hour + ":" + minute);
	selectWindow("Log");  //select Log-window 
	saveAs("Text", dir + "Log_" + "v"+version+"_"+year+month+"0"+dayOfMonth+".txt"); 
	images();
    results();
	roiManager("reset");
	Ext.Manager3D_Close();
	setBatchMode(false);
	if(getBoolean("DONE! :).\nOpen output directory?"))
    call("bar.Utils.revealFile", posdir);
    restoreSettings();

// ==============================================================================================================================================
// ############################################################# FUNCTIONS ######################################################################
// ==============================================================================================================================================
function cleanup() {
		if (isOpen("Results")) { 
		selectWindow("Results"); 
		run("Close");} 
		if (isOpen("ROI Manager")){
		selectWindow("ROI Manager");
		run("Close");}
	run("Close All");
	}
	
function images() {
	if (nImages>0) { 
		selectImage(nImages); 
		close(); 
	}
}

function results () {
	if (isOpen("Results")) { 
    selectWindow("Results"); 
    run("Close"); 
    }
}
	
function closewindows () {
	//if (isOpen(SMap)) {  
		//selectWindow(SMap);
		//close();} 
	if (isOpen(ORG)) { 
		selectWindow(ORG);
		close(); }
	if (isOpen(OMap)) {  
		selectWindow(OMap);
		close();} 
}

function allroi () {
	array1 = newArray("0");; 
		for (i=1;i<roiManager("count");i++){ 
        	array1 = Array.concat(array1,i); 
		} 
	//print("selecting all rois");
	roiManager("select", array1); 
	}

function sliceclear() {
		for (j=1; j<=n; j++) {
		selectWindow(OMap);
		roiManager("reset");
		setSlice(j);
		run("Analyze Particles...", "include add slice");
		if (roiManager("count")==0) {
			selectWindow(ORGC2);
			setSlice(j);
			makeRectangle(0, 0, width, height);
			run("Cut");
		}
		if (roiManager("count")==1) {
			selectWindow(ORGC2);
			setSlice(j);
			allroi();
			wait(200);
			run("Clear Outside", "slice");
		}
		if (roiManager("count")>1) {
			selectWindow(ORGC2);
			setSlice(j);
			allroi();
			roiManager("Combine");
			wait(200);
			run("Clear Outside", "slice");
		}
	}
}

function primseg() {
	run("Duplicate...", "duplicate");
	indiv = getTitle();
	run("Enhance Contrast...", "saturated=0.3 equalize process_all");
	run("Options...", "iterations=3 count=1 black do=Close stack");
	run("Fill Holes", "stack");
// save
	saveAs("Tiff", C2dir + name + "-C2_RC_prim-seg.tif");
	indiv = getTitle();
// set 3D Manager Options (Feret doesn't work here)
	run("3D Manager Options", "volume surface compactness fit_ellipse 3d_moments integrated_density mean_grey_value std_dev_grey_value mode_grey_value minimum_grey_value maximum_grey_value centroid_(pix) centroid_(unit) distance_to_surface centre_of_mass_(pix) centre_of_mass_(unit) bounding_box radial_distance surface_contact closest exclude_objects_on_edges_xy exclude_objects_on_edges_z sync distance_between_centers=10 distance_max_contact=1.80");
// run the manager 3D and add image
	selectWindow(indiv);
	run("3D Manager");
	wait(100);
		Ext.Manager3D_Segment(128,255);
		Ext.Manager3D_AddImage();
		Ext.Manager3D_DeselectAll(); // to refresh RoiManager
// do some measurements, save measurements and close window
		//Ext.Manager3D_Count(nb_obj);
		//Ext.Manager3D_SelectAll();
		Ext.Manager3D_Measure();
		Ext.Manager3D_SaveResult("M", resultsdir + name + "_3DMngr_PrimMeasure.csv");
		Ext.Manager3D_CloseResult("M");
		Ext.Manager3D_Reset();
		Ext.Manager3D_Close();
}

function guideI () {
	newImage(header, "8-bit black", 400, 490, 1);
	setLocation(InfoW, InfoH);
	setColor(200, 200, 200);
  	setFont("SansSerif", 20, "antiliased bold");
  	drawString("   LEGEND\n ", 10, 35);
  	setFont("SansSerif", 18, "antiliased");
  	setColor(255, 255, 255);
  	drawString("\n \n1. DATA DIMENSIONS\n    If you want to analyze more than one \n    channel, select 'Multi Channel'.\n    If C1 in your original files is not your\n    membrane label, select 'Swap Channel #'.\n3. MACRO OPTIONS\n    If 'Register Primordium', the algorithm will \n    create an oval selection around it and \n    crop it in X and Y.\n    If 'Measure Apical Constriction', the degree \n    of AC will be measured (dimensionless). \n    For further details please look up \n    the documentation \n    \n    next: Choose directory to process", 10, 60);
}
