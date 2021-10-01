;+
; Name:
;	look
; Purpose:
;       This procedure produces a widget to view an image or series of images.
;         o You roam in a grid of 2x2 windows with menus on three sides.
;         o The plots are dynamic: horizontal and vertical cuts are updated
;           as you move the mouse over the full and zoom images.
;         o Zoom by dragging the left mouse over the full image.
;         o Select numerous options in the Options menu,
;           - blink compare (between 2 or more images)
;           - movies (all or selected images)
;           - catalogs (all or selected images)
;           - enlargements (to window and to entire screen)
;           - plots (surface, shade_surf, show3, etc.)
;           - regions of interest
;           - spatial filters, and ...
;         o Look is extensible: you can define
;           - one loading function, and
;           - procedures and functions for display and image processing, e.g.,
;               look_fourier - to filter square images
;               lego, image_cont, threed, etc. - any IDL pro using 2-D param
;               roberts, sin, alog10, etc. - any IDL function using 2-D arg
;         o Read more on operation below.  Here's a rendition:
;              -----------------------------------------------------
;              | Done  | cuts:           |                 | full  |
;              |       |   vertical ---> |      plot/      |   &   |
;              |xloadct|   horizontal    |   zoom region   | zoom  |
;              |switch |            |    |            /|\  | stats |
;              |       |------------|------------------|---|       |
;              |Options|           \|/   | cuts:       |   |Incre* |
;              | menu  |    tv/roam      |    horizontal   |Select*|
;              |       |   full image    | <--- vertical   |       |
;              |filter |                 |                 |       |
;              | slider|-----------------------------------|       |
;              | dim3  | plot min slider | plot max slider |       |
;              | ...   | =======|======= | =======|======= |       |
;              | dim7  | Autoscale/Fixed | Autoscale/Fixed |       |
;              -----------------------------------------------------
;              | instructions or comments                          |
;              -----------------------------------------------------
; Examples:
;       look                    ; 1) Test with test images
;       images = randomu(seed,10,10,100)
;       look,images             ; 2) Display series of 2-D images
;       look,/noload            ; 3) Use existing images from common
;       common look             ; 4) To access the stored images at main level
;       look,/help              ; 5) Just display the look.pro header
;       look,/assoc		; 6) Use an associate variable; prompt for
;       			;    file and definition of variable.
;       			; OR
;       file = 'existing_file'
;       definition = 'intarr(128,128)'
;       look,/assoc,file=file,definition=definition
;       			;    The associate variable can have 
;       			;    additional variables---with a tag.
;       definition = '{struct,header:bytarr(100),data:intarr(128,128)}'
;       			;    Up to v3.6, the structure has to be named!
;       look,/assoc,tag=1,file=file,definition=definition
;       			;   use tag #1 (data) in the structure
;       look,loader_name='getdata',d imensions=[3,4,5]
;       			; 7) Use getdata.pro to obtain the set of
;       			;    3x4x5 images,
;       			;    each of size returned by getdata.
;       			;    Here 3 dimension sliders will be used.
;       look,procedures='process'
;       			; 8) Add processing procedures, e.g., to use
;       			;    YOUR OWN process.pro to 
;       			;    modify the current image.
;       look,functions=['roberts','sobel']
;       			; 9) Add edge filters to Options menu
;       look,options=['db','look_fourier','tv'],types=['f','p','p']
;       			; 10)Add functions and procedures together.
;       			;    Specifying types avoids search of !path.
;       look,userscalefunc=['bytscl','myfunc']
;                               ; 11)Add custom functions to auto scaling menu
;       look,axes=['frame','index','a,b,c,d,e']
;                               ; 12) Replace 'dim 3' label with 'frame', etc.
;       look,dim5marks=['raw counts','scaled counts','temperature','radiance']
;                               ; 13) Replace 'dim 5' label with a pulldown
;                               ;     menu to select the dim5 index.
;       tmp = {mark,mark:0l,tag:''}
;       look,dim5marks=[{mark,3500,'slew start'},{mark,3650,'slew stop'}]
;                               ; 14) Same result as 13) but input is a
;                               ;     structure array.  This allows
;                               ;     quick-access marks to be placed in the
;                               ;     data, so the user can jump to them.
;                               ;     The tagsname and structure name are
;                               ;     arbitrary, but you need two tags:
;                               ;     (0)=int/long and (1)=string
;       look,units=['azimuth (200 !3m!5rad pixels)','elevation','counts']
;                               ; 15) Replace image dimensions and image value
;                               ;     names with the 3 elements of units,
;                               ;     e.g. 'y (pixels)' --> 'elevation'
;                               ;     These are stored in common look_common
;                               ;     in the 'all' structure, so they can be
;                               ;     altered by the routine to fetch images,
;                               ;     see units in Keywords section below.
; Loading Images:
;	There are four ways to load data into look:
;         1. Pass images as a parameter: look,images
;         2. Load images into common look and call: look,/noload
;         3. Access images in a file via associate variable: look,/assoc,...
;         4. Give look a function that returns images: look,loader='getdata',...
; Operation:
;       1. The basic thing is to roam around the image and look at cuts.
;          Buttons, sliders and lists control other features.
;       2. The mouse buttons control the following, on the full image.
;                  \        ______
;                   \______/      \
;                                  |   __________ 
;                     ____   ______|__/___       Middle button:
;          Left button:   \ |        /    |       Click to recenter
;           Click & drag   \|  _   _/  _  |       the zoom region.
;           on full image   \ |l| | | | | |
;           to define       |\|e| |m| |r|\|
;           the zoom        | |f| |i| |t| \
;           window.         | |t| |d| | | |\____
;           Select options. | |_| |_| |_| |     Right button:
;                           |             |      Click to do controlled move
;                           |             |      of one quantity (*'d), 
;                           |             |      e.g., the zoom center
;                           |_____________|      in the x direction.
;
;       3. The plot buttons can be selected at any time.
;          The plots appear in the zoom window (by default) or both windows.
;          Toggle the "Use Full & Zoom" button to get either
;            a. the plot of the full image in the zoom window or
;            b. the plot of the zoom image in the full window or
;            c. 2 plots: zoom and full in the 2 windows.
;       4. To zoom, click and drag the left mouse over the region of interest.
;          Then you can roam in the zoom image also, looking at cuts.
;       5. To change the center of the zoom region, click the middle mouse
;          button.  To move the zoom region in a controlled manner, 
;          use the right button.
;       6. Moving the sliders at the bottom changes the plot and tv scales.
;          You can use auto scaling or set the min & max, within the limits
;          shown.
;       7. There are guards against choosing out-of-range quantities.
;       8. If you give a series of 2-D images, the sliders at the left allow
;          you to move through the images---one slider per dimension.
;          A catalog option allows display of multiple images.
;       9. The right mouse button allows controlled increments and decrements
;          of the zoom center and width, selected by using the Increment type
;          menu.
;       10.An options menu lists available processing and image display operators.
;          The menu can contain user-supplied procedures or functions;
;          see keywords options, types, procedures, and functions.  Most of the
;          options appear in pull-down sub-menus in the followiing categories:
;            - xloadct
;            - enlargement: on the window or the entire screen
;            - catalog: all or selective
;            - plot types: contour, histogram, surface, show3, etc.
;            - region of interest
;            - filters: smooth, median, leefilt
;            - write Postscript file
;       11.There are more detailed notes below.
; Usage:
;       look[,image][,/help][,options]
; Optional Inputs:
;       image = one or more 2-D images
; Keywords:
;       /assoc = flag to say the parameter is an associate variable
;         Then the slider for dim3 becomes an index into the associated file.
;         Related keywords are "filename" "definition" "dimensions" and "offset".
;       axes = string array of names to replace dim 3, dim4, ...
;       definition = string used to define the associate variable
;       dimensions = the 3rd and higher dimensions for a series of images and
;         the /assoc or loader= options, also needed for optical disks where
;         only part of the disk has been written on
;       dim3marks = string array or structure array of names used to create
;         a pulldown menu for selecting the dim 3 index.  The pulldown menu
;         is labeled as "dim 3" or the 1st element of the axes keyword if
;         present.  The marks are values along the 3rd dimension and can be
;         selected via the pulldown menu.  The string array provides tags, each
;         with the mark set to its index in the array.  The structure array
;         consists of a named structure with two tags, an int or long and a
;         string, here called mark and tag:
;           tmp = {look_mark,mark:0l,tag:''}
;           dim3marks = [{look_mark,3600,'start'},{look_mark,3800,'end'}]
;       dim4marks = ditto for dim 4
;       dim5marks = ditto for dim 5
;       dim6marks = ditto for dim 6
;       dim7marks = ditto for dim 7
;       filename = name of the file associated with the associate variable
;       functions = synonym for options that are functions, cf. options
;       /help = flag to print the header only and return
;       interval = time in seconds between timer events (D=0.3s)
;	kernel_width = the initial width of the smooth and median filters
;       lines = # of lines in the comment/help scrollable widget (D=2)
;       loader_name = name of an IDL function to return the ith image for
;         display.  It needs one parameter, a vector of the indices for the
;         current image, and returns one parameter, the image.  The
;         important point is that allowing a function to return the image gives
;         the user freedom to grab an image in a variety of ways---not tied
;         directly to look.pro.  A prototype function, returning a 10x10 array,
;         is the following:
;         	function getdata,higherdims,comment
;         	comment = 'This is an 2-D Gaussian random image with non-zero mean.'
;         	return,randomn(seed,10,10)+higherdims(0)
;         	end
;         To use this function with 100 images, the call to look is:
;         	look,...,loader='getdata',dimensions=100
;         If the dimensions keyword is omitted, the user is asked to specify
;         the # of images.
;         Another example would be returning the magnitude, phase, real, or,
;         imaginary part of a complex array:
;         	function getcomplex,higherdims,comment
;         	c= complex(dist(10),dist(10)*higherdims(0))
;         	case higherdims(1) of	; flag is the 2nd dimension
;         	0: return,abs(c)
;         	1: return,atan(imaginary(c),float(c))*!radeg
;         	2: return,float(c)
;         	3: return,imaginary(c)
;         	endcase
;         	end
;         Then the dim4 slider would control the return value and the dim3
;         slider would control the image index.  In this example the call is:
;         	look,...,loader='getcomplex',dimensions=[...,4]
;       /noload = flag to say the input comes directly from common look
;       offset = byte offset pointing to the start of the associated data,
;         the third argument to the assoc call.
;       options = names of IDL functions or pros to process the current image for
;         display or other action, e.g., to define a region of interest or to
;         scale nonlinearly.  The option names appear in the options menu.
;         Each function accepts one parameter, the current image, and returns
;         the modified image.  You can use any idl function that
;         operates on images or make your own.  A prototype function is the
;         following:
;         	function db,image
;         	return,alog10(image)*10
;         	end
;         A procedure accepts one parameter, the current image scaled with look_scl.
;         You can use any idl procedure that operates on images or make your own.  
;         A prototype procedure is the following:
;         	pro dbscl,image
;         	tvscl,db(image)
;         	return
;         	end
;         To use this procedure, the call to look is:
;         	look,...,options='dbscl'
;	  Multiple options are easy, just make an array:
;         	look,...,options=['dbscl','lego','image_cont','slide_image']
;         look has to determine whether the option is a function or a procedure
;         and does a search for each option to find its source code.  These
;         searches are time-consuming if !path has many entries.  You can
;         shorten the search by including another keyword types; see below.
;       procedures = synonym for options that are procedures, cf. options
;       range = range of values to be displayed (D=3x range of images)
;       tag = tag # if the associate variable is a (named) structure
;         e.g., a = assoc(lun,{struct,hdr:0l,data:intarr(128,128)}).
;         Then tag=1 references the data tag; see examples above.
;       types = types of options, either 'pro' or 'function', corresponding to the
;         options keyword to obviate the need for look to determine the option
;         type, e.g., look,options=['sin','blob'],types=['function','pro']
;       units = 3-element string array to replace labels on images and plots
;         x (pixels) --> units(0)
;         y (pixels) --> units(1)
;         z (value) --> units(2)
;         common look_common contains a structure ws that has the units tag
;         and can be manipulated by the function used to fetch images, e.g.,
;           function get_an_image,indices,comment
;           common look_common
;           ws.units = ['longitude (deg)','latitude (deg)','pressure (mbar)']
;           ...
;           return,image
;           end
;       userscalefunc = array of custom scaling function names, e.g.,
;         .r
;         - function myfunc, image
;         - return, 4 * bytscl (image)
;         - end
;         look,...,userscalefunc=['bytscl','myfunc']
;       /verbose = flag to print informational messages (debug mostly)
;       viewers = array of names of procedures to call each time the display
;         is updated by a mouse event.  Procedures can do anything, e.g.,
;         display other images, calculate statistics, or keep track of what
;         has occurred in the program.  The action of each viewer can be
;         set to one of three states using the options menu:
;           off: procedure not called
;           on/all mouse: called for all mouse clicks AND movements
;           on/click only: called for mouse clicks and drags only.
;         You would choose "on/all mouse" if your procedure shows cuts but
;         "on/click only" if your procedure shows images only.  The
;         "on/all mouse" option takes more time, so there will be 
;         delayed updates on cuts.  Each viewer procedure
;         takes no parameters but can obtain information from the two
;         common blocks and should have two keywords: create and destroy.
;         Whenever the viewer is toggled on (off), the procedure will be
;         called with /create (/destroy).  Here's an example.
;           PRO my_display,create=create,destroy=destroy
;           COMMON look
;           COMMON look_common
;           COMMON look_viewer,viewer_window
;           IF keyword_set(create) THEN BEGIN
;             window,/free,xsize = 256,ysize = 256, title = 'Sobel window'
;             viewer_window = !d.window
;           ENDIF
;           IF keyword_set(destroy) THEN BEGIN
;             wdelete,viewer_window
;             return
;           ENDIF
;           wset,viewer_window
;           tvscl,sobel(thisimage) ; example
;           return
;           END
;         In addition the call to look would be:
;           look,viewers='my_display',...
; Outputs:
;       widget as rendered above
; Common blocks:
;       look = images copied from the input parameter or loaded externally,
;         declared and filled by user as one alternative to loading images:
;           common look,images,catalog,index
;           images = ...
;         Note that catalog is another variable; it is loaded by look with
;         the images selected by catalog/movie/blink.  The same is true for
;         index, which is a 2-D array of indices corresponding to the images.
;       look_common = variables that need to be passed between routines,
;         not likely to be needed by the user, except units (see above).
; Procedure:
;       There are two top-level routines: one to initialize (look) and an
;       event handler (look_event).  There are a number of lower-level
;       routines to perform specific tasks.  After creating the widgets,
;       look places the full image in the tv window and a zoom image
;       in the optional plots window.  The cut windows are left blank
;       until the mouse is moved over the image in the tv window.
;       The event loop does all the dynamic processing.
; Restrictions:
;       All the images in a series have to have the same 2-D dimensions.
;       All the sliders use whole numbers because IDL's cw_fslider lacks the
;         ability to specify an increment.
;       Slider value for the maximum slider may be incorrect, even though the
;         the proper values are being sent.  I don't understand this one.
; Future Additions:
;       Report pixel values when scanning over the cut plots?
;       Add a display of the actual values in the array.
;       Add a color bar.
;       Fourier filtering (A version for square images is look_fourier.pro.)
;       Add a comment keyword, like the 2nd parameter of the loader.
; Modification history:
;       Write, 21-26 Feb 95, FKKnight (knight@Ll.mit.edu)
;       Add roaming in the zoom image, 27 Feb-1 Mar 95, FKK
;       Fix bug if middle mouse is pressed first, 1 Mar 95, FKK
;       Add xloadct and rescale plot min and max sliders when a new image is
;         is accessed, 5 Mar 95, FKK
;       Clean up, make fixes, add manual scale limits, 11-12 Mar 95, FKK
;       Add dynamic help messages, 11 Mar 95, FKK
;       Change common to allow input of images via common.  This avoids
;         copying a large number of images to the internal common block from
;         the input parameter, 13 Mar 95, FKK
;       Add search of path for "look.pro" in !PATH + current directory
;         when the HELP button is selected, 09 Mar 95, GGA (gga@otl.sma.ch)
;       Change timer interval from 0.1 to 0.3 [s],
;         insert some code comments, 15 Mar 95, GGA.
;       Add interval keyword to override the default, 15 Mar 95, FKK
;       Add scroll increments (range/!d.n_colors) to sliders, 17 Mar 95, FKK
;       Add function to right mouse: increase/decrease selected quantities
;         by a selected increment (-10 to +10), 18 Mar 95, FKK
;       Add switch to plot zoom and full images or just full image using
;         the plot menu, 19 Mar 95, FKK
;       Add a guard against out-of-range sliders, e.g., int's with
;         imrange = (0:32767) would make range = (-32767:-2),
;         but the guard may not be sufficient, 20 Mar 95, FKK
;       Omit controlled mouse increments because there is no way to shift
;         the mouse position in software---it has to be moved, 21 Mar 95, FKK
;       Make the allowed range of increments larger, up to half the size of
;         the image, which is probably more than enough, 21 Mar 95, FKK
;       Add guards against not having images and not having Bill Thompson's
;         routines available, 21 Mar 95, FKK
;       Exchange the text on the toggle button 'Plot Zoom Only', 23 Mar 95, FKK
;       Call setflag,/noscale to avoid rescaling data, 28 Mar 95, FKK
;       Reset flags /noscale & /noexact on exit, 19 Jun 95, FKK
;       Add range keyword to preselect a slider range, 3 Jul 95, FKK
;       Add the associate variable code, 17 Jul 95, FKK
;       Add catalog of associate variable, 18-19 Jul 95, FKK
;       Add byte offset to start of assoc data, 30 Jul 95, FKK
;       Add function keyword to give the ability to grab images from outside
;         of look.pro and bring them in, e.g., from a frame grabber or an
;         associate variable, 4 Aug 95, FKK
;       Tried to fix scaling problem of imrange outside srange with another
;         guard, 4 Aug 95, FKK
;       Add an optional comment to the function return, 5 Aug 95, FKK
;       Add the enlargement button and a keyword lines to tell the
;         number of lines in the help/comment widget, 8 Aug 95, FKK
;       Add a guard against calling widget_slider with small srange (using
;         cw_fslider requires a mod to have a scroll keyword), 11 Aug 94, FKK
;       Add a keyword, option_names, to name functions to process an image.  This
;         allows user-specified functions in the process menu, 11 Aug 95, FKK
;       Revamp:
;         change keyword option_names --> options
;         change keyword last --> dimensions
;         add smooth and median filters and a slider for filter width
;         rearrange menus to group items better
;         add restore image button to undo processing effects
;         omit help button because xdisplayfile can't cope with >1000 lines
;         change help text to reflect new widget arrangement
;         13 Aug 95, FKK
;       Alter the min and max sliders, so they're easy to use, 15 Aug 95, FKK
;       Revamp:
;         change keyword options --> process_names
;         change keyword function_name --> loader_name
;         add keyword display_name to allow user-supplied display procedures
;         retool the pull_down menu code
;         20 Aug 95, FKK
;       Add selective catalog for large datasets.  Change the catalog item to
;         a pull-down menu with two options: all and selective, which,
;         if chosen, brings up a widget to select first, last, and step values
;         for the catalog display, 24 Aug 95, FKK
;       Move look_fourier to a separate file with a new name: fourier.pro,
;         24 Aug 95, FKK
;       Change menu names AGAIN to combine the two menus into one:
;         display menu --> options menu		; for procedures and functions
;         process menu --> options menu		; for procedures and functions
;       Add code to distinguish between pro and function, but also add a
;         keyword types to allow user to distinguish between pros and
;         functions and synonyms for options, procedures=... and
;         functions=..., 24 Aug 95, FKK
;       Decouple from Bill Thompson's routines, needed three procedures so I
;         mimicked them with only the options look needs:
;           plot_image --> look_image
;           exptv --> look_exptv
;           put --> look_put
;         ,26 Aug 95, FKK
;       Revamp catalog, incl. proper index, processing, 27 Aug 95, FKK
;       Allow even zoom width (odd only before), 30 Aug 95, FKK
;       Add the region of interest processing using IDL's cw_defroi.pro.  Had
;         to block the timers during the cw_defroi life; cf. look_event,
;         31 Aug 95, FKK
;       Replace cw_defroi.pro with look_defroi.pro, an augmented version,
;         including roi selection by threshold.  Keep look_defroi.pro in a
;         separate file.  3 Sep 95, FKK
;       Omit timers; add /motion_events to draw windows.  Add crosshairs.  Add
;         drag box during zoom.  Alter substitute test images, 8-10 Sep 95, FKK
;       Replace border.pro with hard-coded border, 12 Sep 95, FKK
;       Omit testimages (hassles betw IDL3.6 and IDL4.0);
;         just use calculated ones, 14 Sep 95, FKK
;       Add blink compare and fixed increments, 15-16 Sep 95, FKK, with advice
;         from Vincent Coude' de Foresto and code from David Stern
;       Add movies using xinteranimate, 16 Sep 95, FKK
;       Make cuts smoother, per Andrew Cool's suggestion, 17-19 Sep 95, FKK
;       Make press of Set Min= and Set Max= buttons work, 28 Sep 95, FKK
;       Reorder subroutines and make image updates faster by relying on
;         color table modifications where possible.  See look_span_update.
;         8 Oct 95, FKK
;       Add _extra keyword to allow non-look keywords, 13 Oct 95, FKK
;       Add optional names of axes to replace dim 3, dim 4,... 18 Oct 95, FKK
;       Add ability to input specific values for dim 3, dim 4,..., 30 Oct 95, FKK
;       Replace each dim slider with 2 buttons (+ and -) and two editable
;         text boxes for the dim value and an increment, 1 Nov 95, FKK
;       Fix bugs in catlim 1) if min and max are equal, 2) bad defaults,
;         3) out of range integers, 5-7 Dec 95, FKK
;       Improve the catalog/movie/blink options by adding a new mechanism
;         for choosing frames that allows aborting and easy frame selection,
;         11-18 Dec 95, FKK
;       Add a Laplacian filter, i.e., a high-pass filter, 24 Feb 96, FKK
;       Guard against !d.n_colors = 2^24 in look_span_update, 2 Apr 96, FKK
;       Change all !d.n_colors to (!d.n_colors<!d.table_size), 16 Apr 96, FKK
;       Add printing to file from catalog/movie/blink and a "write PS file"
;         option to the menu, 16-20 Apr 96, FKK
;       Add optional labels to the catalog/movie/blink images, 19 Apr 96, FKK
;       Add "catalog" variable to common "look" to hold the output of 
;         catalog/movie/blink.  Now the definition for common look is:
;             common look,images,catalog,index
;         where catalog gets filled, if requested by user, 22 Apr 96, FKK
;       Remove the "Set Max =" option below the slider and fix the slider 
;         entry widget so it responds to CR.  Now the two button options
;         are "Auto Max" and "FixSlide", and they seem to work naturally.
;         Ditto for min. 22 Apr 96, FKK
;       Restore help button and embellish the help widget, 22 Apr 96, FKK
;       Restore some other test images and fix some bugs, 23 Apr 96, FKK
;       Fix part of look_PSatt.pro, 24-25 Apr 96, FKK
;       Fix 3 things: initialize COMMOM colors, make helpbutton work
;         for non-Unix, scale # of help lines to screen, 6 May 96, FKK
;       Added auto-scaling options:  mean or median +/- three different user
;         specifiable sigma levels (new keywords usermean and usermedian),
;         remove top/bottom 5% or 10%, hist_equal, and custom (user-supplied
;         function - new keyword userscalefunc), 25 Apr 97, DSR
;       Added look_si2 function (improved version of look_si) to fix
;         incorrect determination of coordinate conversion factors.  This
;         prevents crosshairs from being drawn outside the plot windows; also,
;         horizontal and vertical cut windows now correspond exactly to the
;         crosshair location.  Changed look_cut to force crosshairs to be
;         placed on the center of data cells.  Axes changed to be centered on
;         the data cells.  6 May 97, DSR
;       Fixed some problems with slider adjustments which were created by the
;         recent auto-scaling changes.  Crosshairs are now erased after a
;         switch to the opposite window.  8 May 97, DSR
;       Crosscut windows are now cleared if the frame changes.  Auto scaling
;         may be done to either the full or the zoom plot, depending on the
;         full/zoom/full & zoom button setting.  9 May 97, DSR
;       Rewrote look_update.  Special plots (contour, histogram, etc.) now
;         persist if the frame number is changed.  13 May 97, DSR
;       Decoupled ws.last_cutwid and ws.cut_oplot to fix bug with erasing
;         crosshairs.  20 May 97, DSR
;       Crosscut plots no longer made for cursor moves in special plot
;         windows.  22 May 97, DSR
;       New zoom window selection resets plot type to default.  Zoom selection
;         from special plot windows is not allowed.  23 May 97, DSR
;       Crosscuts now active in contour plot windows.  Set contour plot axis
;         limits to be the same as for the corresponding image plot.
;         27 May 97, DSR
;       Added modified version of cw_fslider.pro, to allow floating point
;         values on the sliders.  30 May 97, DSR
;       Fixed draw window event handling for version 5.  Fixed on-line help
;         bug (search for look.pro was incorrect).  12 Jun 97, DSR
;       Changes to look_findbin (histogram search routine) - added check to
;         prevent illegal array index references; bin size now computed if
;         data range is small; number of bins limited to 200.  23 Jun 97, DSR
;       Miscellaneous fixes missed in recent mods for floating point sliders.
;         22 Aug 97, DSR
;       userscalefunc can now be an array of function names.  Auto scaling
;         menu now shows function names.  Crosscut plots now use scaled image,
;         rather than original image.  Catalog option now uses hist_equal or
;         custom scaling functions, if selected.  28 Aug 97, DSR
;       Fix to force image scaling routines to return images of type float.
;         29 Aug 97, DSR
;       Reset slider min/max values and scroll increment when a new image is
;         read (added ll_fslider_set_minmax).  10 Sep 97, DSR.
;       Fixed statistics display - added extra blank line.  23 Oct 97, DSR.
;       Expanded slider label box to 9 chars, added label formatting.
;         29 Oct 97, DSR.
;       Add the units keyword, 5 Dec 97, FKK
;       Add pull-down menu for dimension selection.  See dim3marks keyword
;         explanation or test with 'IDL> look', 7-9 Dec 97, FKK
;       Change the export capability in common look: catalog changed to 3-D
;         array, index array added, option to store only indices, append
;         capability, 10-12 Dec 97, FKK
;       Add a button to determine whether the increment is updated when a new
;         image is read.  Update is nice if images span widely different ranges
;         but frustrating if the images only vary slightly.  22 Dec 97, FKK
;       Fix problem in catalog where index variable didn't exist at the
;         beginnning: add extra test (n_elements(index) EQ 0), 9 Jan 98, FKK
;       Fix movie for "Use Zoom Only" case.  The ZOOM structure in LOOK_COMMON 
;         was being replaced by the zoom factor in look_catalog.  5 Mar 98, DSR
;       Add viewers keyword to allow user to add procedures to do things
;         triggered by mouse events (similar to procedures and functions
;         keywords but automatic instead of being chosen from options
;         menu), 26 Mar 98, FKK
;       Improved code for display of vertical crosscuts (histogram case).
;         22 Jul 98, FKK
;       Repair movie mode: allow zoomed movie for full and zoom images with
;         default size to fill 1/4 of smaller screen dimension and range up
;         full screen, guard against single frame sent to xinteranimate
;         (slider error).  Still can't do full & zoom movies together
;         (xinteranimate restriction). 9 Sep 98, FKK
;       look_auto_minmax changed to use fix / auto slider setting; catalog
;         now auto scales each image separately.  20 Oct 98, DSR
;       fixed catalog indexing scheme.  16 Jun 99, DSR
;       Replace stdev calls (obsolete) with calls to moment.  14 Dec 99, DSR
;       Fix to avoid calling moment with structure elements as keyword
;         arguments.  14 Mar 00, DSR
;       Added option to catalog to generate indices without displaying
;         images.  22 Jun 00, DSR
;       Changed catalog default to display images.  12 Jul 00, DSR
;-
;
; Copyright (c) 1993, Research Systems, Inc.  All rights reserved.
;	Unauthorized reproduction prohibited.
;+
; NAME:
;	look_defroi
; PURPOSE:
;   This function returns a where vector of the pixels in a region of interest,
;   selected via a number of methods from within a draw widget. 
; CATEGORY:
;   Regions of interest, graphics.
; CALLING SEQUENCE:
;   Result = look_defroi(draw[,options])
; INPUTS:
;   Draw = id of drawable to draw the region, not the window number.
; KEYWORD PARAMETERS:
;   CONTRAST = flag to select positive (/contrast, the default) or negative
;       (contrast=0) for the threshold method of definiing the roi
;   IMAGE = the real image that is displayed with the zoom factor in the
;       drawable.  Passing it in allows the threshold to be calculated exactly,
;       even for non-unity zooms.  Without it, a congrid is necessary and
;       the resulting image is only an approximation.
;   MODE = integer code for initial method of selecting pixels:
;       0=polygon, 1=point, 2=rectangle, 3=circle, 4=threshold
;   OFFSET = offset of lower left corner of image within the
;       drawable.  Default = [0,0].
;   ORDER = if set, return inverted subscripts, as if the array
;       were output from top to bottom.
;   RESTORE = Set to restore the drawable to its previous appearance
;       on exit.  Otherwise, the regions remain on the drawable.
;   SIZE = the size of the underlying array, expressed
;       as a two element vector: [columns, rows].  Default =
;       (drawable_size-offset) / zoom.
;   ZOOM = if the image array was expanded (via REBIN for example)
;       specify this two element vector containing the expansion
;       factor in X and Y.  Default = [1,1].  Must be integer.
; OUTPUTS:
;      Result = 1-D vector of subscripts of points within the region[s] defined.
;       If no region was defined, a scalar -1 is returned.
; COMMON BLOCKS:
;       None.
; SIDE EFFECTS:
;       The regions are drawn within the drawable.  Set the RESTORE
;       keyword to undo the damage.  
; RESTRICTIONS:
;   This is a MODAL widget.  No other widget applications will be
;   responsive while this widget is in use.  The draw widget should
;   have the /motion and /button.  For the threshold method, the
;   return value can be off due to the congrid employed, unless the actual
;   image is passed in.
; PROCEDURE:
;   Defining the roi is interective: you select the method(s) and use the mouse.
;   The left mouse button selects points (click or drag); the right deletes points.
;   The methods are:
;     - polygon: click on vertices
;     - point: click on single pixels
;     - rectangle: drag from lower left to upper right or vice versa
;     - circle: drag from center to point on circumference
;     - threshold: select positive or negative contrast and adjust slider.
;   When you change the method, the current points are added to the total, so
;   you can combine methods, except for the threshold method.  The event loop
;   is inside the main function, but mouse events in the drawable are processed
;   in an event procedure.  Some important points about the code are the
;   following.
;     - There are two pixel coordinate lists that are stored as uvalues.
;     - Except for the threshold method, the displayed image is not needed.
;       Instead the XOR graphics mode is used to draw and fill on top of
;       the image.
;     - For the threshold mode, the image is read using tvrd, so it is a
;       scaled copy.
;     - A structure, s, which holds the state information, is passed to the
;       subroutines:
;         look_defroi_draw: overlay outline or fill on drawable.
;         look_defroi_nmode: concatenate current points with old ones
;         look_defroi_event: process mouse events
; EXAMPLE:
;   To obtain the average of the counts of a region within a drawable:
;   Assume A = the array of interest, n columns, m rows, and that
;   it is displayed in drawable D, at offset X=20, Y=100, and zoomed
;   with a factor of 2:
;       TV, REBIN(A, M*2, N*2), 20, 100     ;Display the image
;       q = look_defroi(D, ZOOM=[2,2], OFFSET=[20,100], IMAGE_SIZE=[m,n])
;       if q(0) ne -1 then print,'Average = ', total(a(q))/n_elements(q)
;       
; MODIFICATION HISTORY:
;   DMS, RSI, December, 1993.  Written.
;   modify for incorporating into look.pro:
;     - change name from cw_defroi to look_defroi
;     - add another mode, threshold with positive or negative contrast
;     - allow floating zoom factors
;     - allow a timer to be active in the drawable by only processing events
;       with 8 tags (mouse events)  Timer events have 3 tags.
;     - add mode-specific instructions
;     31 Aug-1 Sep 95, FKKnight (knight@ll.mit.edu)
;   add the image keyword and change image_size keyword to size.  This allows
;     an exact calculation of the return value.  See notes above. 3 Sep 95, FKK
;-


;   The following code (between the triple "*" lines) is imported from a
;   modified verison of IDL's cw_fslider.pro routine.

;***************************************************************************
;***************************************************************************
;***************************************************************************
; $Id: look.pro,v 1.1.1.1 2007-02-03 17:10:49 cavagnolo Exp $
; Copyright (c) 1992-1993, Research Systems, Inc.  All rights reserved.
;	Unauthorized reproduction prohibited.
;
; NAME:
;	LLCW_FSLIDER
;
; PURPOSE:
;	The standard slider provided by the WIDGET_SLIDER() function is
;	integer only. This compound widget provides a floating point
;	slider.
;
; CATEGORY:
;	Compound widgets.
;
; CALLING SEQUENCE:
;	widget = LLCW_FSLIDER(Parent)
;
; INPUTS:
;       Parent:		The ID of the parent widget.
;
; KEYWORD PARAMETERS:
;	DRAG:		Set this keyword to zero if events should only
;			be generated when the mouse is released. If it is
;			non-zero, events will be generated continuously
;			when the slider is adjusted. Note: On slow systems,
;			/DRAG performance can be inadequate. The default
;			is DRAG=0.
;       EDIT:		Set this keyword to make the slider label be
;			editable. The default is EDIT=0.
;	FORMAT:		Provides the format in which the slider value is
;			displayed. This should be a format as accepted by
;			the STRING procedure. The default is FORMAT='(G13.6)'
;	FRAME:		Set this keyword to have a frame drawn around the
;			widget. The default is FRAME=0.
;	MAXIMUM:	The maximum value of the slider. The default is 
;			MAXIMUM=100.
;	MINIMUM:	The minimum value of the slider. The default is
;			MINIMUM=0.
;	SUPPRESS_VALUE:	If true, the current slider value is not displayed.
;			The default is SUPPRESS_VALUE=0.
;	TITLE:		The title of slider. (The default is no title.)
;	UVALUE:		The user value for the widget.
;	VALUE:		The initial value of the slider
;	VERTICAL:	If set, the slider will be oriented vertically.
;			The default is horizontal.
;	XSIZE:		For horizontal sliders, sets the length.
;	YSIZE:		For vertical sliders, sets the height.
;
; OUTPUTS:
;       The ID of the created widget is returned.
;
; SIDE EFFECTS:
;	This widget generates event structures containing a field
;	named value when its selection thumb is moved. This is a
;	floating point value.
;
; PROCEDURE:
;	WIDGET_CONTROL, id, SET_VALUE=value can be used to change the
;		current value displayed by the widget.
;
;	WIDGET_CONTROL, id, GET_VALUE=var can be used to obtain the current
;		value displayed by the widget.
;
; MODIFICATION HISTORY:
;	April 2, 1992, SMR and AB
;		Based on the RGB code from XPALETTE.PRO, but extended to
;		support color systems other than RGB.
;	5 January 1993, Mark Rivers, Brookhaven National Labs
;		Added EDIT keyword. 
;       7 April 1993, AB, Removed state caching.
;	28 July 1993, ACY, set_value: check labelid before setting text.
;       30 May 1997, Douglas S. Reynolds (MIT Lincoln Laboratory)
;               Added SCROLL keyword.  Fixed bug in case where VALUE=0,
;               and MINIMUM < VALUE < MAXIMUM.
;       25 June 1997, Douglas S. Reynolds (MIT Lincoln Laboratory)
;               Fixed bug in computation of VALUE parameter in
;               WIDGET_SLIDER call.
;       16 September 1997, Douglas S. Reynolds, MIT Lincoln Laboratory
;               Added fslider_set_minmax, to allow the slider limits
;               to be changed.
;


PRO ll_fslider_set_minmax, id, min = minvalue, max = maxvalue

;   Change the minimum and maximum values for the floating point slider.
;   Since the actual slider limits are fixed at 0 and 1000000, this is
;   actually done by changing the minimum and maximum values stored in the
;   "state" structure.  These values are used by fslider_set_value and
;   fslider_get_value to change or access the slider setting as seen by
;   the user.

  stash = WIDGET_INFO(id, /CHILD)
  WIDGET_CONTROL, stash, GET_UVALUE = state
  if n_elements (minvalue) ne 0 then state.bot = minvalue
  if n_elements (maxvalue) ne 0 then state.top = maxvalue
  WIDGET_CONTROL, stash, SET_UVALUE = state
END



PRO ll_fslider_set_value, id, value

  ; Set the value of both the slider and the label
  ON_ERROR, 2						;return to caller

  stash = WIDGET_INFO(id, /CHILD)
  WIDGET_CONTROL, stash, GET_UVALUE=state, /NO_COPY

  WIDGET_CONTROL, state.slideid, $
	SET_VALUE = 1000000. * $
		(float(value) - state.bot) / (state.top - state.bot)
  IF (state.labelid NE 0) THEN $
  	WIDGET_CONTROL, state.labelid, $
		SET_VALUE = STRING(FLOAT(value), format=state.format)

  WIDGET_CONTROL, stash, SET_UVALUE=state, /NO_COPY
END



FUNCTION ll_fslider_get_value, id

  ; Return the value of the slider
  ON_ERROR, 2						;return to caller

  stash = WIDGET_INFO(id, /CHILD)
  WIDGET_CONTROL, stash, GET_UVALUE=state, /NO_COPY

  WIDGET_CONTROL, state.slideid, GET_VALUE = tmp
  ret = ((tmp / 1000000.) * (state.top - state.bot)) + state.bot

  WIDGET_CONTROL, stash, SET_UVALUE=state, /NO_COPY 
  return, ret
END


;-----------------------------------------------------------------------------

FUNCTION ll_fslide_event, ev

  ; Retrieve the structure from the child that contains the sub ids
  parent=ev.handler
  stash = WIDGET_INFO(parent, /CHILD)
  WIDGET_CONTROL, stash, GET_UVALUE=state, /NO_COPY


  ; See which widget was adjusted, the slider or the label

  if (ev.id eq state.slideid) then begin
    ; Get the non-adjusted value
    WIDGET_CONTROL, state.slideid, GET_VALUE = nonadj
    ; Compute the floating point value
    value = ((nonadj / 1000000.) * (state.top - state.bot)) + state.bot
    drag = ev.drag
    ; Update label
    IF (state.labelid NE 0) THEN $
      WIDGET_CONTROL, state.labelid, $
           SET_VALUE=STRING(value, format=state.format)

  endif else if (ev.id eq state.labelid) then begin

    WIDGET_CONTROL, state.labelid, GET_VALUE = tmp

    value = float(tmp(0))
    value = value > state.bot
    value = value < state.top
    ;Update the slider, set new value
    WIDGET_CONTROL, state.slideid, $
	SET_VALUE = 1000000. * $
		(value - state.bot) / (state.top - state.bot)

    drag = 0
    ; Update the label so it has desired format
    WIDGET_CONTROL, state.labelid, $
           SET_VALUE=STRING(value, format=state.format)
  endif

  WIDGET_CONTROL, stash, SET_UVALUE=state, /NO_COPY
  RETURN, { ID:parent, TOP:ev.top, HANDLER:0L, VALUE:value, DRAG:drag }
END

;-----------------------------------------------------------------------------

FUNCTION llcw_fslider, parent, $
		DRAG = drag, $
                EDIT = edit, $
		FRAME = frame, $
		MAXIMUM = max, $
		MINIMUM = min, $
		SUPPRESS_VALUE = sup, $
		TITLE = title, $
		UVALUE = uval, $
		VALUE = val, $
		VERTICAL = vert, $
		XSIZE = xsize, $
		YSIZE = ysize, $
		FORMAT=format, $
                SCROLL = scrollval

  IF (N_PARAMS() EQ 0) THEN MESSAGE, 'Incorrect number of arguments'

  ON_ERROR, 2						;return to caller

  ; Defaults for keywords
  IF NOT (KEYWORD_SET(drag))  THEN drag = 0
  IF NOT (KEYWORD_SET(edit))  THEN edit = 0
  IF NOT (KEYWORD_SET(frame)) THEN frame = 0
  IF N_ELEMENTS(max) EQ 0     THEN max = 100.0
  IF N_ELEMENTS(min) EQ 0     THEN min = 0.0
  IF NOT (KEYWORD_SET(sup))   THEN sup = 0
  IF NOT (KEYWORD_SET(title)) THEN title = ""
  IF NOT (KEYWORD_SET(uval))  THEN uval = 0
;;  IF NOT (KEYWORD_SET(val))   THEN val = min
  if N_ELEMENTS(val) EQ 0 THEN val = min
  IF NOT KEYWORD_SET(format)  THEN format='(G13.6)'

;   Convert slider increment from user units to internal units (0-1000000)

  IF keyword_set (scrollval) THEN $
     scrollinc = scrollval * 1000000 / (max - min) $
  ELSE $
     scrollinc = 10000

  state = {slideid:0L, labelid:0L, top:max, bot:min, format:format }

  ; Motif 1.1 and newer sliders react differently to XSIZE and YSIZE
  ; keywords than Motif 1.0 or OpenLook. These defs are for horizontal sliders
  version = WIDGET_INFO(/version)
  newer_motif = (version.style eq 'Motif') and (version.release ne '1.0')

  ; The sizes of the parts depend on keywords and whether or not the
  ; float slider is vertical or horizontal
  ;these are display specific and known to be inherently evil
  sld_thk = 16
  chr_wid = 7
  IF (KEYWORD_SET(vert)) THEN BEGIN
    if (newer_motif) then begin
      if (not KEYWORD_SET(xsize)) then xsize = 0
    endif else begin
      title_len = STRLEN(title) * chr_wid
      xsize = (sld_thk * 1.4) + title_len	; Take label into account
    endelse
    IF NOT (KEYWORD_SET(ysize)) THEN ysize = 100
    l_yoff = ysize / 2
  ENDIF ELSE BEGIN					;horizontal slider
    vert = 0
    tmp = not keyword_set(xsize)
    if (newer_motif) then begin
      if (tmp) then xsize = 0
      IF NOT (KEYWORD_SET(ysize)) THEN ysize = 0
    endif else begin
      if (tmp) then xsize = 100
      IF (TITLE NE '') THEN sld_thk = sld_thk + 21
      ysize = sld_thk		; Make the slider not waste label space
    endelse
    l_yoff = 0
  ENDELSE

  if (vert) then begin
    mainbase = WIDGET_BASE(parent, FRAME = frame, /ROW)
    labelbase = WIDGET_BASE(mainbase)
  endif else begin
    mainbase = WIDGET_BASE(parent, FRAME = frame, /COLUMN)
    labelbase = mainbase
  endelse
  WIDGET_CONTROL, mainbase, SET_UVALUE = uval, EVENT_FUNC = 'll_fslide_event', $
	PRO_SET_VALUE='LL_FSLIDER_SET_VALUE', $
	FUNC_GET_VALUE='LL_FSLIDER_GET_VALUE'



  IF (sup EQ 0) THEN $
    ; Only build the label if suppress_value is FALSE
    state.labelid = WIDGET_TEXT(labelbase, YOFFSET = l_yoff, $
		VALUE = STRING(FLOAT(val), format=state.format), $
                edit=edit) $
  ELSE state.labelid = 0

    state.slideid = WIDGET_SLIDER(mainbase, $
		TITLE = TITLE, $
		XSIZE = xsize, $
		YSIZE = ysize, $
		/SUPPRESS_VALUE, $
		MINIMUM = 0, $
		MAXIMUM = 1000000, $
;   Fix made to VALUE computation to prevent errors resulting from lack of
;   automatic type conversion.
		VALUE = 1000000. * $
			(float(val) - state.bot) / $
;;			(state.top - state.bot), $
			(float(state.top) - state.bot), $
		VERTICAL = vert, $
		DRAG=drag, $
		SCROLL = scrollinc)
;;		SCROLL=10000)
  
  WIDGET_CONTROL, WIDGET_INFO(mainbase, /CHILD), SET_UVALUE=state, /NO_COPY
  RETURN, mainbase

END
;***************************************************************************
;***************************************************************************
;***************************************************************************

pro look_reset_sliders, image
   COMMON look_common,thisimage,zoomimage,fullimage,ws,all,zoom

   allrange = [min(image), max(image)]
   range = abs(allrange(1)-allrange(0))    ; make sure it's positive
   srange = allrange+[-1,1]*range          ; a total of 3x range of images

;;   mag = 5 - fix (alog10 (srange(1) - srange(0)))
;;   srange(0) = long (srange(0) * 10^mag) / 10^mag
;;   srange(1) = long (srange(1) * 10^mag) / 10^mag

;   Define the new scroll value
   IF ws.bothfix EQ 0 THEN BEGIN
     scroll_value = (srange(1)-srange(0))/((!d.n_colors<!d.table_size) > 1)
     widget_control, ws.spanmagid, set_value = strtrim(string(scroll_value,format = '(g12.3)'),2)
   ENDIF

;   Change the slider limits for the new image
   ll_fslider_set_minmax, ws.spansliders(0), min = srange(0), max = srange(1)
   ll_fslider_set_minmax, ws.spansliders(1), min = srange(0), max = srange(1)
   ws.srange = srange
end


;   Apply current image scaling selection (from auto scaling menu) to an
;   image
function look_apply_scalefunc, image
COMMON look_common,thisimage,zoomimage,fullimage,ws,all,zoom

;   For scaling function cases, force image to be float (some routines, like
;   hist_equal and bytscl, return images of type byte)

   case 1 of
     all.scaletype eq 10: begin                ; hist_equal
            newimage = hist_equal (image)
            newimage = float (newimage)
         end
     all.scaletype ge 11: begin                ; custom scaling functions
            newimage = call_function (all.scalefunc(all.scaletype-11), image)
            newimage = float (newimage)
         end
     else: newimage = image
   endcase

   return, newimage
end



pro look_defroi_nmode, s, new
; Set new mode... Save old roi by concatenating it with s.subs.
n = s.npts
if (s.mode ne 1) and (n le 2) then n = 0  ;must have 3 pnts for polygon
WIDGET_CONTROL, s.mode_w, SET_VALUE=0       ;Revert to add mode
s.amode = 0

if n ge 1 then begin       ;Old region to save?
  CASE 1 OF
    s.mode eq 1: begin   ;Points?
        WIDGET_CONTROL, s.xy_pnts, GET_UVALUE=xy, /NO_COPY  ;Get old ROI
        xy = xy(0,0:n-1) + s.image_size(0) * xy(1,0:n-1) ;points to subs
        xy = REFORM(xy, n_elements(xy), /OVERWRITE) ;Make linear
        END
    s.mode eq 4: WIDGET_CONTROL, s.xy_pnts, GET_UVALUE=xy, /NO_COPY      
    ELSE: begin
        look_defroi_DRAW, s, -1, /FILL
        WIDGET_CONTROL, s.xy_pnts, GET_UVALUE=xy, /NO_COPY  ;Get old ROI
        xy = polyfillv(xy(0,0:n-1),xy(1,0:n-1),s.image_size(0), s.image_size(1))
    END
ENDCASE
    WIDGET_CONTROL, s.subs, GET_UVALUE=t, /NO_COPY      ;Prev roi pnts
        ;Concatenate s and xy
    if n_elements(t) le 0 then WIDGET_CONTROL, s.subs, SET_UVALUE=xy, /NO_COPY $
    else WIDGET_CONTROL, s.subs, SET_UVALUE=[t,xy], /NO_COPY
    endif               ;Old region to save

s.mode = new
s.npts = 0
end

PRO look_defroi_DRAW, s, i, FILL = fill
; Draw the outline (or polygon if FILL is set) 
; of the region or the ith segment if i < 0.
; Use the XOR drawing mode.

n = s.npts
if n lt 1 then return
   
WSET, s.win   
DEVICE, SET_GRAPHICS=6          ;Xor drawing mode   
col = 1
while col lt !d.table_size do col = col + col
WIDGET_CONTROL, s.xy_pnts, GET_UVALUE=xy, /NO_COPY  ;Get ROI
  xsave = !x.s & ysave = !y.s       ;Set scaling to pixel coords
  p = float([!d.x_size, !d.y_size])
  f = s.offset / p
  q = s.zoom / p
  !x.s = [f(0), q(0)]
  !y.s = [f(1), q(1)]

if s.mode eq 1 then BEGIN       ;Point mode?
    if i lt 0 then begin
        i = 0 & i1 = n-1
    ENDIF else i1 = i
    for j = i, i1 do $
        polyfill, xy(0,j) + [0, .9, .9, 0], xy(1,j) + [0,0,.9,.9], COLOR=col
ENDIF ELSE BEGIN            ;Polygon/circle/rect
    if n ge 2 then begin
        if i lt 0 then plots, COLOR=col, xy(*, 0:n-1)+.5 $ ;All of it?
        else plots, COLOR=col, xy(*, i:i+1)+.5  ;One segment
        IF KEYWORD_SET(FILL) then POLYFILL, xy(*,0:n-1), COLOR=col
    ENDIF
ENDELSE

!x.s = xsave & !y.s = ysave
WIDGET_CONTROL, s.xy_pnts, SET_UVALUE=xy, /NO_COPY  ;Set ROI   
DEVICE, SET_GRAPHICS=3          ;Copy mode   
end   
   
   
PRO look_defroi_event, ev, s
; This routine is only called from the look_defroi event loop.
; ev = event structure, s = state structure.


s.button = s.button or ev.press xor ev.release  ;New button state: 1=down, 0=up  
n = s.npts
x = (ev.x - s.offset(0)) / s.zoom(0)    ;Pixel coordinates
y = (ev.y - s.offset(1)) / s.zoom(1)   

if s.order then y0 = s.image_size(1)-y-1 else y0 = y
WIDGET_CONTROL, s.pos_w, $
    SET_VALUE=string(x, y0, format='("Position: ",i,", ",i)')

if (x lt 0) or (y lt 0) or $            ;Within region?
    (x ge s.image_size(0)) or (y ge s.image_size(1)) then return
if ev.press ne 0 then s.drag = [x,y]    ;Start of drag operation
;widget_control,s.xy_pnts,set_value = string(ev,form = '(8i4)'),/append

if (s.mode eq 2) or (s.mode eq 3) then begin ;Rect or circle?
    if s.button ne 0 then begin          ;Drag
        if n gt 0 then look_defroi_draw, s, -1      ;Remove old
        t = s.drag
        if s.mode eq 2 then begin   ;Rectangle
            n = 5
            xy = [[t], [x, t(1)], [x, y], [t(0), y], [t]]
        endif else begin        ;Circle
            n = 30              ;# of points
            a = findgen(n+1) * (2 * !pi/(n-1))
            r = sqrt((float(x)-t(0))^2 + (float(y) - t(0))^2)
            xy = transpose([[t(0) + r * cos(a)], [t(1) + r * sin(a)]])
        endelse
    WIDGET_CONTROL, s.xy_pnts, SET_UVALUE=xy, /NO_COPY   ;Restore UVALUE
    s.npts = n
    look_defroi_draw, s, -1
    ENDIF                       ;DRAG
    return
ENDIF                           ;Rect or circle


if s.button eq 0 then return        ;Must be point or polygon...
tmode = s.amode                     ;Default mode
if s.button eq 4 then tmode = 1     ;Rt button to remove

if tmode then begin            ;Remove prev point?
    if (ev.press ne 0) and (n gt 0) then begin
       look_defroi_DRAW, s, -1   ;Erase old region
        WIDGET_CONTROL, s.xy_pnts, GET_UVALUE=xy, /NO_COPY ;Get ROI array
        d = float(x-xy(0,0:n-1))^2 + float(y-xy(1,0:n-1))^2  ;Dist
        t = min(d, ipnt)        ;Closest...
        if ipnt ne (n-1) then xy(0,ipnt) = xy(*,ipnt+1:*)  ;Collapse
        s.npts = n-1
        WIDGET_CONTROL, s.xy_pnts, SET_UVALUE=xy, /NO_COPY  ;Save ROI array
        if n gt 1 then look_defroi_DRAW, s, -1   ;Draw new region
        endif   
    return
    endif               ;Remove mode....   
           
; Here we add a point
WIDGET_CONTROL, s.xy_pnts, GET_UVALUE=xy, /NO_COPY  ;Get ROI array

; Add a point
if n_elements(xy) le 1 then xy = intarr(2,100)   
        ; Remove duplicates...   
if n gt 0 then if x eq xy(0,n-1) and y eq xy(1,n-1) then goto, done0
if s.mode eq 1 then for i=0, n-1 do $       ;Point mode?
    IF x eq xy(0,i) and y eq xy(1,i) then goto, done0    ;No duplicates

if (n+1) ge n_elements(xy)/2 then xy = [[xy], [intarr(2,n)]]  ;Extend array?

xy(0,n) = x     ;New point
xy(1,n) = y
n = n + 1
s.npts = n
WIDGET_CONTROL, s.xy_pnts, SET_UVALUE=xy, /NO_COPY   ;Restore UVALUE

if s.mode eq 0 then begin           ;Polygon?
    if n ge 2 then look_defroi_draw, s, n-2           ;Draw the new segment
endif else begin                    ;Point
    look_defroi_draw, s, n-1         ;Draw new point
endelse
return
   
done0: WIDGET_CONTROL, s.xy_pnts, SET_UVALUE=xy, /NO_COPY   
end   


function look_defroi, draw, ZOOM = zoom, SIZE = image_size, $   
    OFFSET = offset, RESTORE = restore, ORDER = order,contrast = contrast $
    ,mode = mode,image = image

if n_elements(contrast) eq 0 then contrast = 1
if n_elements(mode) eq 0 then mode = 4 ; threshold is the default method
if n_elements(zoom) eq 0 then zoom = [1,1]
if n_elements(zoom) eq 1 then zoom = [zoom,zoom]
if n_elements(offset) le 0 then offset = [0,0]   
if n_elements(image_size) le 0 then image_size = ([!d.x_size, !d.y_size]-offset) / zoom
WIDGET_CONTROL, draw, GET_VALUE=win
WSET, win   
p  = offset + image_size /2   
TVCRS, p(0), p(1), /DEVICE   
;
;	=====>> Read the display; create image reduced by zoom factors
;
display = tvrd(offset(0),offset(1),image_size(0)*zoom(0),image_size(1)*zoom(1))
IF n_elements(image) EQ 0 THEN image = congrid(display,image_size(0),image_size(1))
min = min(image,max = max)      ; Used for threshold slide definition
dmin = min(display,max = dmax)  ; For translating display values to image values
range = float(max-min)
drange = float(dmax-dmin)
;print,zz
;
;	=====>> Text for prompting
;
polygon_prompt = ['Add with left button: drag or click.' $   
                  ,'Remove with right button.']   
point_prompt = polygon_prompt
rectangle_prompt = ['Drag with left button from lower left' $
                    ,'to upper right or vice versa.']
circle_prompt = ['Drag with left button from center' $
                 ,'to point on circle or vice versa.']
threshold_prompt = ['Select positve (>) or negative (<) contrast.' $
                    ,'Move threshold slider to desired level.']
xsize = max([strlen(threshold_prompt),strlen(polygon_prompt),strlen(rectangle_prompt),strlen(point_prompt),strlen(circle_prompt)])

base = widget_base(title='Region of Interest', /COLUMN)   
xy_pnts = WIDGET_TEXT(base, YSIZE=2, xsize = xsize,/FRAME, UVALUE=0, $
    value=threshold_prompt,/scroll)
Options = CW_BGROUP(base, /ROW, /NO_RELEASE, /RETURN_NAME,  $
    ['Done','Clear', 'Clear All', 'New', 'Cancel'])   
tslide = widget_slider(base,title = 'Threshold, T',value = min,min = min,max = max,/drag)
row = widget_base(base,/row)
mode_map = [1,2,3,4,0]
junk = CW_BGROUP(row, /column, /EXCLUSIVE, /NO_REL, /RETURN_NAME, $
    ['Threshold, T','Polygon', 'Point', 'Rectangle', 'Circle'], SET_VALUE=mode_map(mode))
junk = CW_BGROUP(row, /ROW, /EXCLUSIVE, /NO_REL, /RETURN_NAME, $
    ['< T', '> T'], SET_VALUE=contrast)
mode_w = CW_BGROUP(base, /ROW, LABEL_LEFT = 'Mode:', /EXCLUSIVE, /NO_REL, $   
    /RETURN_NAME, ['Add', 'Remove'], SET_VALUE=0)
pos_w = WIDGET_TEXT(base, YSIZE=1, XSIZE=18, /FRAME, $
    VALUE='Position:    0,    0')

   
WINDOW, /PIXMAP, /FREE, xs = !d.x_size, ys=!d.y_size  ;Save window
backing = !d.window
DEVICE, copy = [0,0, !d.x_size, !d.y_size, 0, 0, win]  ;Save it

s = { look_defroi_STRUCT, $       ;Structure containing state
    base: base, $       ;Main base widget
    xy_pnts: xy_pnts, $ ;Current roi vertex list
    npts : 0L, $        ;# of points in current roi
    subs : pos_w, $     ;Widget holding prev subscripts
    pos_w : pos_w, $    ;Position text widget
    mode: mode, $       ;major mode
    amode: 0, $         ;0 for add, 1 for remove
    draw: draw, $       ;draw widget id
    win:  win, $        ;draw widget window #
    button: 0, $        ;button state
    image_size : long(image_size), $   ;Image array size
    mode_w: mode_w, $   ;Add/remove button widget
    backing: backing, $ ;Pixmap for backing store
    offset: fix(offset), $   ;offset of array within window
    zoom : zoom, $ ;zoom factor
    order : KEYWORD_SET(order), $  ;Image order
    drag: [0,0]}        ;Beginning of drag motion
   
WIDGET_CONTROL, base, /REALIZE
;WSHOW, win
   
WHILE 1 DO BEGIN                ;Internal event loop   
    ev = WIDGET_EVENT([base, draw])
    n = s.npts
    if ev.id eq draw then BEGIN
;   Fix for version 5 - now 9 tags, not 8
      IF n_tags(ev) GE 8 THEN look_defroi_EVENT, ev, s ; Mouse event (not timer, e.g.)
    ENDIF else BEGIN
      IF ev.id EQ tslide THEN BEGIN ; Threshold slider changed
        wset,win
        thres = ev.value
        dthres = float(thres-min)/range*drange+dmin
        IF contrast THEN w = where(display LE dthres) $
          ELSE w = where(display GT dthres)
        IF w(0) NE -1 THEN BEGIN ; Set all pixels </> threshold to min
          tmp = display
          tmp(w) = min
          tv,tmp,offset(0),offset(1)
        ENDIF
        IF contrast THEN xy = where(image GT thres,count) $
          ELSE xy = where(image LE thres,count)
        IF count GT 0 THEN BEGIN ; Catalog all image pixels >/< threshold
          s.npts = count
          WIDGET_CONTROL, s.xy_pnts, SET_UVALUE=xy, /NO_COPY   
        ENDIF
      ENDIF ELSE case ev.value of   
'Clear All':  BEGIN
    WIDGET_CONTROL, s.subs, GET_UVALUE=t, /NO_COPY  ;Clr list of subscripts
    t = 0
    WSET, win
    DEVICE, copy = [0,0, !d.x_size, !d.y_size, 0, 0, backing]  ;Restore it
    s.npts = 0
    ENDCASE
'Clear':  BEGIN   
    if (n ge 2) or (s.mode eq 1 and n ge 1) then $
        look_defroi_draw, s, -1        ;Erase roi   
    s.npts = 0   
    look_defroi_NMODE, s, s.mode
    ENDCASE
'New' : look_defroi_nmode, s, s.mode      ;Make a new region...
'Cancel':  BEGIN   
    xy = -1
    goto, all_done
    ENDCASE    

;    ['Polygon', 'Point', 'Rectangle', 'Circle'], SET_VALUE=0)
'Polygon': BEGIN
  widget_control,xy_pnts,set_value = polygon_prompt,/append
  look_defroi_nmode, s, 0
END
'Point' :   BEGIN
  widget_control,xy_pnts,set_value = point_prompt,/append
  look_defroi_nmode, s, 1
END
'Rectangle' :   BEGIN
  widget_control,xy_pnts,set_value = rectangle_prompt,/append
  look_defroi_nmode, s, 2
END
'Circle' :   BEGIN
  widget_control,xy_pnts,set_value = circle_prompt,/append
  look_defroi_nmode, s, 3
END
'> T': contrast = 1
'< T': contrast = 0
'Threshold, T':   BEGIN
  widget_control,xy_pnts,set_value = threshold_prompt
  look_defroi_nmode, s, 4
END
 
'Add':  s.amode = 0
'Remove': s.amode = 1
'Done': BEGIN   
    look_defroi_nmode, s, 0       ;Save old region
    WIDGET_CONTROL, s.subs, GET_UVALUE=t, /NO_COPY  ;List of subscripts
    xy = BYTARR(s.image_size(0), s.image_size(1))  ;Return only unique
    IF n_elements(t) GT 0 THEN IF t(0) NE -1 THEN xy(t) = 1
    if s.order then xy = reverse(xy,2)  ;Flip it?
;    print,zz
    xy = where(temporary(xy))
all_done:
    IF KEYWORD_SET(restore) then begin      ;Undo damage?
        WSET, win
        DEVICE, copy = [0,0, !d.x_size, !d.y_size, 0, 0, backing]  ;Restore it
        ENDIF
    WDELETE, backing
    WIDGET_CONTROL, base, /DESTROY   
    return, xy
  ENDCASE
ENDCASE   
ENDELSE
ENDWHILE            ;Event loop
END   
;
;       ============================
;       =====>> BASIC IMAGE SCALING FOR PLOTTING
;       ============================
;
FUNCTION look_scl,image,range
return,bytscl(image,min = range(0),max = range(1),top = (!d.n_colors<!d.table_size)-1)
END
;
;       ============================
;       =====>> Get the next image: either for catalog or as a new image
;       ============================
;
FUNCTION look_get_image,ii,ws,noscale=noscale,noslide=noslide
; ii = vector of indices for new image
; ws = structure of needed quantities
COMMON look,images,catalog,index
ws.dcat(*) = -1                 ; For use in look_put
ws.dcat(0:1) = ws.size(1:2)     ; not necessary
ws.dcat(2:2+n_elements(ii)-1) = ii ; save indices in common
CASE 1 OF                       ; Choose option
  ws.assoc: BEGIN
    IF ws.is_struct THEN BEGIN
      tmp = images(ii(0))
;;      return,tmp.(ws.tag)       ; return selected tag
      retimage = tmp.(ws.tag)       ; return selected tag
    ENDIF ELSE BEGIN
;;      return,images(ii(0))
      retimage = images(ii(0))
    ENDELSE
  END
  ws.func: BEGIN                ; user-supplied function
    text = ''
    image = call_function(ws.loader_name,ii,text)
    IF text NE '' THEN widget_control,ws.comtext,set_value = text,/append
;;    return,image
    retimage = image
  END
  ELSE: CASE ws.size(0) OF      ; when images are passed as a parameter
;;    3: return,images(*,*,ii(0))
;;    4: return,images(*,*,ii(0),ii(1))
;;    5: return,images(*,*,ii(0),ii(1),ii(2))
;;    6: return,images(*,*,ii(0),ii(1),ii(2),ii(3))
;;    7: return,images(*,*,ii(0),ii(1),ii(2),ii(3),ii(4))
;;    ELSE: return,images(*,*)
    3: retimage = images(*,*,ii(0))
    4: retimage = images(*,*,ii(0),ii(1))
    5: retimage = images(*,*,ii(0),ii(1),ii(2))
    6: retimage = images(*,*,ii(0),ii(1),ii(2),ii(3))
    7: retimage = images(*,*,ii(0),ii(1),ii(2),ii(3),ii(4))
    ELSE: retimage = images(*,*)
  ENDCASE
ENDCASE

if not keyword_set (noscale) then retimage = look_apply_scalefunc (retimage)
if not keyword_set (noslide) then look_reset_sliders, retimage

return, retimage
END
;
;       ============================
;       =====>> Add a label of the current indices to the image
;       ============================
;
FUNCTION look_addlabel,image,ii
; image = bytscl'd image ready for display but to have digits added here
; ii = set of image indices
COMMON look_common,thisimage,zoomimage,fullimage,ws,all,zoom
p = 0                           ; counter for total # of digits
szd = size(ws.digits)           ; ned digit dimensions
x = szd(1)                      ; width of digit
szi = size(image)               ; need height
y = szi(2)-1-szd(2)             ; lower left y value for digits
IF y LE 0 THEN return,image     ; not enough space in the array
w = ws.size(0)-3                ; highest index in loop, # dimensions-1
FOR i = 0,w DO BEGIN
  si = byte(strtrim(ii(i),2))-48b ; indices (0-9) into digits array.
  FOR j = 0,n_elements(si)-1 DO BEGIN ; add one digit at a time
    IF (x*p+x) GT szi(1) THEN GOTO,DONE ; End if no more space.
    image(x*p,y) = ws.digits(*,*,si(j))
    p = p+1                     ; increment counter of total digits
  ENDFOR
  IF (i EQ w) OR ((x*p+x) GT szi(1)) THEN GOTO,DONE
  image(x*p,y) = ws.digits(*,*,10) ; add a ':'
  p = p+1                       ; increment counter of total digits
ENDFOR
DONE:
return,image
END
;
;       ============================
;       =====>> Routine to "blink compare" two images 
;       ============================
;
PRO look_blink,a,b,wid
; a,b = two bytscl'd images to compare
; wid = window id of the draw region
COMMON look_common,thisimage,zoomimage,fullimage,ws,all,zoom
;
;	=====>> Prompt the user
;
widget_control,ws.comtext,/append,set_value = 'Blink control: With mouse in blink window, left=faster, middle=slower, right=stop.'
;
; Code, except event stuff, copied from flick.pro by David Stern for X or
; Windows.  I replaced flick's get_kbrd call with event loop, watching only for
; mouse events in the draw window.  Note the /nowait keyword, which enables the
; blinking to continue even if a mouse button is not pressed.
;
rate = 1.0
ichl = 0
sfact = 1.5                     ;Speed steps
cwin = !d.window
pix = intarr(2)                 ;Make 2 pixmaps
for i=0,1 do begin
  window, /FREE, /PIX, xs = !d.x_size, ys = !d.y_size
  pix(i) = !d.window
  if i eq 0 then tv,a else tv,b
endfor
wset, cwin
while 1 do begin                ; loop infinitely over each chl
  device, copy=[0,0,!d.x_size, !d.y_size, 0, 0, pix(ichl)]
  wait,1./rate                  ; This also empties the graphics buffer
  ev = widget_event(wid,/nowait)
;   Fix for version 5 - now 9 tags, not 8
;;  help,/str,ev
  IF n_tags(ev) GE 8 THEN CASE ev.press OF
    1: rate = rate*sfact       ; left --> Faster
    2: rate = rate/sfact       ; middle --> Slower
    4: goto,done1              ; right --> Done
    ELSE:
  ENDCASE
  ichl = 1 - ichl               ; Other image
ENDWHILE
done1: wdelete, pix(0), pix(1)
return
END
;
;       ============================
;       =====>> Define a widget for index selection: - value + increment.
;       ============================
;
FUNCTION look_mvpi,parent,value = value,uvalue = uvalue,menu = menu $
                  ,title = title,size = digits,scroll = scroll,all_events = all
; parent = widget id of parent
; value = starting value of index
; uvalue = prefix for uvalue, actual uvalues are uvalue+['I-','','I+']
; title = optional label underneath the widgets, if present
; scroll = optional initial value of the increment, if present
; dimid = return values, the widget ids of the 4 widgets
; menu = pull-down menu, if present, to replace the title
dimi = strtrim(uvalue,2)
doscroll = n_elements(scroll) GT 0
dimid = lonarr(3+doscroll)
col = widget_base(parent,/column)
tmp = widget_base(col,/row)
dimid(0) = widget_button(tmp,value = '-',uvalue = dimi+'I-')
IF n_elements(all) EQ 0 THEN all = 0 ; default is to generate event on CR only
dimid(1) = widget_text(tmp,value = strtrim(value,2),uvalue = dimi+'VALUE' $
                      ,/edit,xsize = digits,all_events = all)
dimid(2) = widget_button(tmp,value = '+',uvalue = dimi+'I+')
IF doscroll THEN dimid(3) = widget_text(tmp,value = strtrim(scroll,2),/edit,xsize = digits)
IF n_elements(menu) GT 0 THEN BEGIN
  junk = cw_pdmenu(col,menu,/return_name,uvalue = dimi+'pdmenu')
ENDIF ELSE BEGIN
  IF n_elements(title) GT 0 THEN IF title NE '' THEN $
    junk = widget_label(col,value = title)
ENDELSE
return,dimid
END
;
;       ============================
;       =====>> Display catalog or movie of images or blink compare
;       ============================
;
PRO look_catalog,testin,dofull,id,do_catalog = do_catalog
; testin = uvalue of the event
; dofull = flag to distinguish between full (1) or zoom (0) image
; id = widget id of the draw window
; do_catalog = flag to designate catalog so save widgets are added
COMMON look,images,catalog,index
COMMON look_common,thisimage,zoomimage,fullimage,ws,all,zoom
;
;       =====>> Setup
;
docatalog = strpos(testin,'cat') GE 0
domovie = strpos(testin,'movie') GE 0
doblink = strpos(testin,'blink') GE 0
IF dofull THEN xy = [ws.size(1),ws.size(2)] ELSE BEGIN
  xy = [zoom.xrange(1)-zoom.xrange(0)+1,zoom.yrange(1)-zoom.yrange(0)+1]
ENDELSE
IF domovie THEN BEGIN
  device,get_screen_size = scrdims ; screen size
  scrfrac = [4,2,1]             ; possible screen fractions to cover
  times = (((scrdims(0)/xy(0)) < (scrdims(1)/xy(1)))/scrfrac) > 1
ENDIF
IF domovie THEN dozoom = domovie AND (max(times) GT min(times)) ELSE dozoom = 0; movie and zoom allowed
titles = ['first','-last','/step','blink']
IF doblink THEN BEGIN
  ncol = 4
  buttons = ['DONE','GO','STOP']
ENDIF ELSE BEGIN
  ncol = 3
  buttons = ['DONE','GO','STOP']
ENDELSE
nd = long(ws.size(0)-2)         ; # of dimensions beyond 2 (3-D etc.)
IF nd LE 0 THEN return          ; guard against 2-D only
cid = lonarr(3,ncol,nd)         ; widget id's for all selection widgets
;
;       =====>> Create widget
;
CASE 1 OF
  domovie: parent = widget_base(/column,title = 'Movie')
  doblink: parent = widget_base(/column,title = 'Blink Compare')
  ELSE: parent = widget_base(/column,title = 'Catalog')
ENDCASE
dg = long(max(ceil(alog10(ws.size(3:nd+2)))))
nimages = 1l
FOR d = 0,nd-1 DO BEGIN         ; dimensions
  nimages = nimages*long(ws.size(d+3))
  row = widget_base(parent,/row)
  IF d EQ (nd-1) THEN tit = titles ELSE tit = strarr(ncol)
  uv = titles+strtrim(d,2)
  cid(0,0,d) = look_mvpi(row,val=ws.d(d+2),uv=uv(0),si=dg,ti=tit(0))
  cid(0,1,d) = look_mvpi(row,val=ws.d(d+2),uv=uv(1),si=dg,ti=tit(1))
  cid(0,2,d) = look_mvpi(row,val=1,uv=uv(2),si=dg,ti=tit(2))
  IF doblink THEN $
    cid(0,3,d) = look_mvpi(row,val=ws.d(d+2),uv=uv(3),si=dg,ti=tit(3))
  junk = widget_label(row,value = ws.dimname(d))
ENDFOR
IF dozoom THEN zid = widget_slider(parent,title = 'zoom factor',uvalue = 'ZOOM',min = times(0),max = times(2),value = times(1))
row = widget_base(parent,/row)
bid = lonarr(n_elements(buttons))
FOR n = 0,n_elements(buttons)-1 DO BEGIN
  bid(n) = widget_button(row,value = buttons(n),uvalue = buttons(n))
ENDFOR
tmp = widget_base(row,/row,/nonexclusive)
AddLabels = widget_button(tmp,value = 'Add Labels?',uvalue = 'AddLabels')
widget_control,AddLabels,set_button = ws.AddLabels
nput_pre = '# images = '
nput = 0l
nimid = widget_text(row,value = nput_pre+string(nput))
row = widget_base(parent,/row)
tmp = widget_button(row,value = 'Write PS file',uvalue = 'writePSfile')
tmp = widget_button(row,value = 'Modify PS attributes',uvalue = 'SetPSatt')
row = widget_base(parent,/row)
IF docatalog THEN BEGIN
  catalog_pre = '# catalog images = '
  sz = size(catalog)
  IF sz(0) EQ 3 THEN ncatalog = sz(3) ELSE ncatalog = 0
  ncatalogid = widget_text(row,value = catalog_pre+string(ncatalog))
  index_pre = '# index sets = '
  sz = size(index)
  IF sz(0) EQ 2 THEN nindex = sz(2) ELSE nindex = 0
  nindexid = widget_text(row,value = index_pre+string(nindex))
  col = widget_base(parent,/column,/nonexclusive)
  saveid = widget_button(col,value = 'Save images/indices in catalog/index of common look',uvalue = 'CATALOGSAVE')
  widget_control,saveid,set_button = ws.catalogsave
  indexid = widget_button(col,value = 'Only save selected indices in index of common look',uvalue = 'SAVE')
  widget_control,indexid,set_button = ws.save
  resetid = widget_button(col,value = 'Reset catalog and index at start of next save',uvalue = 'RESET')
  widget_control,resetid,set_button = ws.reset
  catshowid = widget_button(col,value = 'Display images in catalog (otherwise just generate indices)',uvalue = 'CATSHOWIMAGES')
  widget_control,catshowid,set_button = ws.catshowimages
ENDIF
instrid = widget_text(parent,value = ['Adjust indices. Enter new value with CR.','Then hit GO button. Hitting STOP aborts.'],/scroll,ysize = 2)
widget_control,parent,/realize
;
;       =====>> Processing loop: process events while watching for GO
;
go = 0                          ; Flag to start processing
newvalues = 1                   ; Flag to recalculate indices, # of images,...
LOOP:
;
;       =====>> If an index was changed, recalculate the indices, etc.
;
IF newvalues THEN BEGIN
  cidv = cid*0                  ; Calculate # of images using current data
  nput = 1l                     ; total number of images
  step = lonarr(nd)             ; step size along each dimension
  nsteps = lonarr(nd)           ; # of steps along each dimension
  FOR d = 0l,nd-1 DO BEGIN      ; loop to collect info from widgets
    FOR i = 0l,ncol-1 DO BEGIN
      widget_control,cid(1,i,d),get_value = tmp
      cidv(1,i,d) = long(tmp)
    ENDFOR
    step(d) = cidv(1,2,d) > 1
    nsteps(d) = floor((cidv(1,1,d)-cidv(1,0,d))/step(d))+1
    nput = nput*nsteps(d)
  ENDFOR
  widget_control,nimid,set_value = nput_pre+strtrim(nput,2)
  indices = lonarr(nput,nd)     ; array of indices---easiest way to keep track
  dups = [1,nsteps]             ; duplication factors
  dup = 1l                      ; duplication counter
  FOR d = 0l,nd-1 DO BEGIN      ; loop over dimensions
    dup = dups(d)*dup           ; # of times each index appears
    t = reform(cidv(1,*,d))     ; a convenience
    IF t(1) EQ t(0) THEN BEGIN  ; along this dimension, no increments
      indices(0,d) = replicate(t(0),nput)
    ENDIF ELSE BEGIN            ; along this dimension, replicate index vector
       for kk = 0, nput / dup / nsteps(d) - 1 do begin
          for ll = 0, nsteps(d) - 1 do begin
             i0 = ll * dup + kk * nsteps(d) * dup
             indices(i0:i0+dup-1,d) = t(0) + step(d) * ll
          endfor
       endfor
;      v = lindgen(nsteps(d))*step(d)+t(0)
;      tmp = lonarr(dup*nsteps(d))
;      tmp(*) = transpose((lonarr(dup)+1)#v)
;      indices(*,d) = tmp#(lonarr(nput/dup/nsteps(d))+1)
;      print,indices(*,d)
    ENDELSE
  ENDFOR
  newvalues = 0                 ; reset flag
ENDIF
;
;       =====>> If user said go, then display the next image.
;
IF go THEN BEGIN
   IF iput LT nput THEN BEGIN
      ii = reform(indices(iput,*))
;;    tmp = look_apply_scalefunc (look_get_image(ii,ws))
      if ws.catshowimages or ws.catalogsave then $
        tmp = look_get_image(ii,ws,/noslide)
      IF ws.save THEN BEGIN
         IF ws.saveoffset EQ -1 OR n_elements(index) EQ 0 THEN BEGIN ; reset catalog and index arrays?
            index = ii          ; redefine the index variable
            ws.saveoffset = 0   ; kludgey fix added 062299
         ENDIF ELSE BEGIN
            index = [[index],[ii]] ; append to index array
         ENDELSE
         IF ws.catalogsave THEN BEGIN
            ws.saveoffset = ws.saveoffset + 1 ; increment offset
            catalog(*,*,ws.saveoffset) = tmp
            widget_control,ncatalogid,set_value = catalog_pre+strtrim(ws.saveoffset+1,2)
         ENDIF
         sz = size(index)
         IF sz(0) EQ 2 THEN n = sz(2) ELSE n = 0
         widget_control,nindexid,set_value = index_pre+strtrim(n,2)
      ENDIF
      if ws.catshowimages then begin
;   Fix to use auto scaling
         look_auto_minmax, scaleto = tmp, newrange = newlim, /no_update, /no_bound
;;    tmp = look_scl(tmp,all.tvrange)
         tmp = look_scl(tmp,newlim)
         IF NOT dofull THEN $
           tmp = tmp(zoom.xrange(0):zoom.xrange(1),zoom.yrange(0):zoom.yrange(1))
         IF dozoom THEN tmp = rebin(tmp,xy(0)*zoomfact,xy(1)*zoomfact,/sample)
         IF domovie THEN BEGIN
            IF ws.AddLabels THEN tmp = look_addlabel(tmp,ii)
            xinteranimate,image = tmp,frame = iput
         ENDIF ELSE look_put,tmp,iput+1,nput
      endif
   ENDIF ELSE BEGIN
      IF domovie THEN BEGIN
         widget_control,parent,/destroy
         xinteranimate
         return
      END
      IF doblink THEN BEGIN
         blink0 = tvrd()
;;      tmp = look_scl(look_get_image(reform(cidv(1,3,*)),ws),all.tvrange)
;   Fix to use auto scaling
;;      tmp = look_scl(look_get_image(reform(cidv(1,3,*)),ws,/noscale,/noslide),all.tvrange)
         tmp = look_get_image(reform(cidv(1,3,*)),ws,/noscale,/noslide)
         look_auto_minmax, scaleto = tmp, newrange = newlim, /no_update, /no_bound
         tmp = look_scl(tmp,newlim)
         IF NOT dofull THEN $
           tmp = tmp(zoom.xrange(0):zoom.xrange(1),zoom.yrange(0):zoom.yrange(1))
         FOR n = 0,nput-1 DO look_put,tmp,n+1,nput
         blink1 = tvrd()
         widget_control,instrid,set_value = 'button control ON IMAGE: left->Faster, middle->Slower, right->Done',/append
         look_blink,blink0,blink1,id
      ENDIF
      go = 0                    ; and stop further display
   ENDELSE
   iput = iput+1                ; increment image counter
ENDIF
;
;       =====>> Get events
;
ev = widget_event(parent,/nowait)
wait,0.05                        ; to limit cpu usage during wait for event
IF ev.id EQ 0 THEN GOTO,LOOP
widget_control,ev.id,get_uvalue = uvalue,get_value = value 
IF ws.debug THEN widget_control,ws.comtext,/append $
  ,set_value = strtrim(uvalue,2) $
  +' '+strtrim(value,2)+' '+strtrim(ev.id,2)+' '+strtrim(ev.top,2) $
  +' '+strtrim(ev.handler,2)
;
;       =====>> Process events
;
test = string(uvalue)
CASE 1 OF
   test EQ 'AddLabels': ws.AddLabels = ev.select
   test EQ 'DONE': BEGIN
      widget_control,parent,/destroy
      return
   END
   (strpos(test,'I+') GE 0) OR (strpos(test,'I-') GE 0) OR (strpos(test,'VALUE') GE 0): BEGIN
      IF strpos(test,'VALUE') GE 0 THEN sign = 0 $ ; CR, so add 0 to new below
      ELSE sign = 2*(strpos(uvalue,'+') GE 0)-1 ; 1-->1 & 0-->-1
      dim = fix(strmid(uvalue,5,1))
      w = (where(titles EQ strmid(uvalue,0,5)))(0)
      IF w GE 0 THEN BEGIN
         widget_control,cid(1,w,dim),get_value = current
         new = (long(current)+sign) > 0 < long((ws.size(dim+3)-1))
         widget_control,cid(1,w,dim),set_value = strtrim(new,2)
      ENDIF
      IF (w EQ 0) THEN BEGIN    ; first modified
         widget_control,cid(1,1,dim),get_value = curlast
         widget_control,cid(1,1,dim),set_value = strtrim(new > curlast,2)
;    print,dummy
      ENDIF
      IF (w EQ 1) THEN BEGIN    ; last modified
         widget_control,cid(1,0,dim),get_value = curfirst
         widget_control,cid(1,0,dim),set_value = strtrim(curfirst < new,2)
      ENDIF
      newvalues = 1
   END
   test EQ 'GO': BEGIN
      iput = 0                  ; reset the number of images stored
      IF domovie THEN BEGIN
         IF dozoom THEN widget_control,zid,get_value = zoomfact ELSE zoomfact = 1
;    IF ws.debug THEN widget_control,ws.comtext,/append,set_value = 'zoom = '+strtrim(zoomfact,2)
         xinteranimate,set = [xy(0)*zoomfact,xy(1)*zoomfact,nput > 2],/showload
      ENDIF ELSE erase
      IF ws.save THEN BEGIN     ; Define output array?
         IF ws.reset THEN BEGIN
            ws.saveoffset = -1  ; reset pointer to first element in catalog
            ws.reset = 0        ; unset reset flag
            widget_control,resetid,set_button = 0 ; turn off reset button
            catalog = 0         ; reset catalog variable
            index = 0           ; reset index variable
         ENDIF
         IF ws.catalogsave THEN BEGIN
            sz = [3,xy,nput,ws.size(nd+1),xy(0)*xy(1)*nput]
            IF n_elements(catalog) EQ 1 THEN catalog = make_array(size = sz) $
            ELSE catalog = [[[temporary(catalog)]],[[make_array(size = sz)]]]
         ENDIF
      ENDIF
      go = 1
   END
   test EQ 'CATALOGSAVE': BEGIN
      ws.catalogsave = ev.select
      ws.save = ev.select OR ws.save ; OR in case catalog reset with index set
   END
   test EQ 'SAVE': ws.save = ev.select OR ws.catalogsave
   test EQ 'RESET': ws.reset = ev.select
   test EQ 'CATSHOWIMAGES': ws.catshowimages = ev.select
   test EQ 'SetPSatt': look_PSatt
   test EQ 'STOP': go = 0
   test EQ 'writePSfile': look_put,tvrd(),1,1,filename = ws.PSfilename
ELSE: 
ENDCASE
GOTO,LOOP
END
;
;       ============================
;       =====>> Fill the window with a plot: modeled after Bill Thompson's exptv
;       ============================
;
PRO look_exptv,image,minimum = minimum,maximum = maximum,zoom = zoom,offset = offset,image = tvimage
; image = bytscl'd 2-D image
; minimum, maximum = not used
; zoom = output, floating 2-element vector of display size to image size
; offset = output, 2-element vector of pixel offsets to lower left corner of image
x = [0,!d.x_size-1]             ; 1st and last x pixels of window
y = [0,!d.y_size-1]             ; 1st and last y pixels of window
xw = x(1)-x(0)+1                ; window x width
yw = y(1)-y(0)+1                ; window y width
aw = float(yw)/xw               ; window aspect ratio
si = size(image)
ai = float(si(2))/si(1)         ; image aspect ratio
IF aw GT ai THEN BEGIN
  y = y + [1,-1]*(yw-xw*ai)/2   ; window higher, so decrease y symmetrically
ENDIF ELSE BEGIN
  x = x + [1,-1]*(xw-yw/ai)/2   ; image higher, so decrease x symmetrically
ENDELSE
erase
tvimage = congrid(image,x(1)-x(0)+1,y(1)-y(0)+1)
tv,tvimage,x(0),y(0)
zoom = float([x(1)-x(0)+1,y(1)-y(0)+1])/si(1:2)
offset = [x(0),y(0)]
return
END
;
;       ============================
;       =====>> Catalog output plots: modeled after Bill Thompson's put
;       ============================
;
PRO look_put,image,iix,nnx,filename = filename
; image = 2-D imge, bytscl'd
; iix = # of the image: 1,...,nnx
; nnx = maximum number of plots
; filename = PS filename.  If present, then assume SINGLE plot to 'PS' device.
;
;  =====>> Special case: Assume iix=nnx=1, bytscl'd image, and 'PS' device.
;
COMMON look_common,thisimage,zoomimage,fullimage,ws,all,zoom
IF n_elements(filename) GT 0 THEN BEGIN
  IF (iix NE 1) OR (nnx NE 1) THEN BEGIN
    widget_control,ws.comtext,/append,set_value = $
      'look_put:  Only one image allowed.  No PS file written.'
    return
  ENDIF
  widget_control,/hourglass
  set_plot,'PS'                 ; Choose Postscript.
  stretch,0,2^ws.PSatt.bits-1   ; expand/dilute color table to right # of elements
  xcm = !d.x_size/!d.x_px_cm    ; window x dimension in cm
  ycm = !d.y_size/!d.y_px_cm    ; window y dimension in cm
  sz = size(image)              ; image size
  ai = float(sz(2))/sz(1)       ; image aspect ratio
  aw = float(!d.y_size)/!d.x_size ; window aspect ratio
  IF aw GT ai THEN ycm = xcm*ai ELSE xcm = ycm/ai
  device,file = ws.PSfilename,xsize = xcm,ysize = ycm
  IF ws.PSattstr NE '' THEN stat = execute('device'+ws.PSattstr)
  tv,image
  stretch                       ; restore original # of elements in color table
  device,/close                 ; close device
  set_plot,'X'
  widget_control,hourglass = 0
  return
ENDIF
;
;  =====>> Code from Bill Thompson's put.pro: arrange along the X and
;  Y axes based on the size of the image and the size of the window.
;
S = SIZE(image)
NX = NNX
NY = 1
AMAX = 0
FOR NI = 1,NNX DO BEGIN
        NJ = (NNX + NI - 1) / NI
        AX = !D.X_SIZE / (S(1)*FLOAT(NI))
        AY = !D.Y_SIZE / (S(2)*FLOAT(NJ))
        AA = AX < AY
        IF AA GT AMAX THEN BEGIN
                AMAX = AA
                NX = NI
                NY = NJ
        ENDIF
ENDFOR
;IX = ((IIX - 1) MOD NX) + 1
;IY = (IIX - 1)/NX + 1
;
;  =====>> Display the image, assumed to be scaled properly
;
xw = !d.x_size/nx
yw = !d.y_size/ny
ai = float(s(2))/s(1)
aw = float(yw)/xw
IF aw GT ai THEN yw = xw*ai ELSE xw = yw/ai
mx = max(image)
c = congrid(image,xw,yw)
IF ws.AddLabels THEN c = look_addlabel(c,ws.dcat(2:*))
c(0,*) = mx                     ; add a border all around
c(xw-1,*) = mx
c(*,0) = mx
c(*,yw-1) = mx
tv,c,iix-1
return
END
;
;       ============================
;       =====>> Plot image with axes, modeled after Bill Thompson's plot_image
;       ============================
;
PRO look_image,image,_extra = extra,origin = origin,scale = scale $
               ,minimum = minimum,mximum = maximum
; image = bytscl'd image to be tv'd
; _extra = any keyword for plot
; origin, scale, minimum, maximum = not used
IF n_elements(origin) EQ 0 THEN origin = [0,0]
plot,[0,1],_extra = extra,xstyle = 5,ystyle = 5,/nodata ; set !x.window & !y.window
x = !x.window*!d.x_size         ; x pixels in plot window
y = !y.window*!d.y_size         ; y pixels in plot window
xw = x(1)-x(0)                  ; x width in pixels
yw = y(1)-y(0)                  ; y width in pixels
si = size(image)
ai = float(si(2))/si(1)         ; aspect ratio (y:x) of the image
aw = float(yw)/xw               ; aspect ratio of the plot window
                                ; Depending on aspect, change either y or x.
IF aw GT ai THEN y = y + [1,-1]*(yw-xw*ai)/2 ELSE x = x + [1,-1]*(xw-yw/ai)/2
erase
tv,congrid(image,x(1)-x(0),y(1)-y(0)),x(0),y(0)
xpos = x/!d.x_size              ; After tv, add axes at xpos
ypos = y/!d.y_size              ; ...and ypos
; Changed xrange and yrange to make ticks centered on the corresponding data
; boxes - this is done by extending the ranges by 0.5 on each end (DSR)
plot,[0,si(2)-1],_extra = extra,xstyle = 1,ystyle = 1 $
  ,/noerase,/nodata,position = [xpos(0),ypos(0),xpos(1),ypos(1)] $
  ,xrange = [0-0.5,si(1)-0.5]+origin(0),yrange = [0-0.5,si(2)-0.5]+origin(1)
;;  ,xrange = [0,si(1)-1]+origin(0),yrange = [0,si(2)-1]+origin(1)
;print,zz
return
END
;
;       ============================
;       =====>> FIND FILE IN !PATH: like findfile but anywhere in !path
;       ============================
;
FUNCTION look_in_path,filename
path=expand_path(!PATH,/array,count=count)
file=''
i=-1
REPEAT BEGIN
  i=i+1
  file=findfile(path(i)+'/'+filename)
ENDREP UNTIL file(0) ne '' or i eq count-1
return,file
END
;
;       ============================
;       =====>> DISPLAY AN ARRAY as text in a widget
;       ============================
;
PRO look_xdisplay_event,ev
widget_control,ev.id,get_uvalue = uvalue
IF uvalue EQ 'DONE' THEN widget_control,ev.top,/destroy
return
END
PRO look_xdisplay_one,parent,buf,xsize = xsize,ysize = ysize,title = title
szb = size(buf)
junk = widget_text(parent,value = title)
CASE szb(szb(0)+1) OF
  7: junk = widget_text(parent,xsize = xsize,ysize = ysize,/scroll,value = buf)
;  1: junk = widget_text(parent,xsize = xsize,ysize = ysize,/scroll,value = string(buf(0:szb(2) < 682,*),form = '(i3)'))
  ELSE:
ENDCASE
return
END
PRO look_xdisplay,buf,buf2,xsize = xsize,ysize = ysize,titles = titles
; buf = 1D or 2D array, either strings or numbers, to display in a scrollable widget_text
; xsize = width in characters (D=80)
; ysize = height in lines (D=22)
IF n_elements(xsize) EQ 0 THEN xsize = 80
IF n_elements(ysize) EQ 0 THEN ysize = 22
parent =  widget_base(/column,title = 'look_xdisplay')
junk = widget_button(parent,value = 'DONE',uvalue = 'DONE')
look_xdisplay_one,parent,buf,xsize = xsize,ysize = ysize,title = titles(0)
IF n_elements(buf2) GT 0 THEN look_xdisplay_one,parent,buf2,xsize = xsize,ysize = ysize,title = titles(1)
widget_control,parent,/realize
xmanager,'look_xdisplay',parent,event_handler = 'look_xdisplay_event'
return
END

;
;       ============================
;       =====>> 2 COORDINATE CONVERTERS: device <--> data
;       ============================
;
; data <--> device
; given data coords, return slopes & intercepts to convert device to data
; OR
; given device coords, return slopes & intercepts to convert data to device
pro look_si,x,y,slopes,intercepts,to_data = to_data
IF keyword_set(to_data) THEN BEGIN
  dat = convert_coord([x(0),x(1)],[y(0),y(1)],/to_data,/device)
  slopes = [(x(1)-x(0))/(dat(0,1)-dat(0,0)),(y(1)-y(0))/(dat(1,1)-dat(1,0))]
  intercepts = [x(0)-dat(0,0)*slopes(0),y(0)-dat(1,0)*slopes(1)]
ENDIF ELSE BEGIN
; Note (DSR):  this is close, but not quite right for this application.  A
; better approach is used in look_si2
  dev = convert_coord([x(0),x(1)],[y(0),y(1)],/data,/to_device)
  slopes = [(x(1)-x(0))/(dev(0,1)-dev(0,0)),(y(1)-y(0))/(dev(1,1)-dev(1,0))]
  intercepts = [x(0)-dev(0,0)*slopes(0),y(0)-dev(1,0)*slopes(1)]
ENDELSE
return
end


; look_si2 is a corrected version of look_si, but *without* the to_data
; option (DSR)
pro look_si2,xrange,yrange,slopes,intercepts,to_data = to_data
IF keyword_set(to_data) THEN BEGIN
;;  dat = convert_coord([x(0),x(1)],[y(0),y(1)],/to_data,/device)
;;  slopes = [(x(1)-x(0))/(dat(0,1)-dat(0,0)),(y(1)-y(0))/(dat(1,1)-dat(1,0))]
;;  intercepts = [x(0)-dat(0,0)*slopes(0),y(0)-dat(1,0)*slopes(1)]
  message, /inform, 'look_si2 does not have the to_data option; use look_si'
ENDIF ELSE BEGIN
  slopes = [((xrange(1) - xrange(0) + 1.) / $
             (!D.X_SIZE * (!x.window(1) - !x.window(0)))), $
            ((yrange(1) - yrange(0) + 1.) / $
             (!D.Y_SIZE * (!y.window(1) - !y.window(0))))]
  intercepts = [-slopes(0) * !D.X_SIZE * !x.window(0), $
                -slopes(1) * !D.Y_SIZE * !y.window(0)]
ENDELSE
return
end


; device <--> data
; given device coords, return data coords OR vice versa
FUNCTION look_cc,coord,slope,intercept
;   Corrected:  the slope and intercept conversion factors are based on
;   the lower left corner of the data cell boundaries; they are not data
;   centered.  Therefore, the calculated coordinates should always be rounded
;   down.  Note also that the FIX function rounds positive values down, and
;   negative values up.  Therefore, 1 must be subtracted from negative values
;   in order to obtain the desired downward rounding (DSR).
zz = slope*coord+intercept
nel = n_elements (zz)
for i = 0, nel - 1 do begin
   if (zz(i) lt 0.) then zz(i) = zz(i) - 1.
endfor
return, fix (zz)
;; return,fix(slope*coord+intercept+0.5)   ; want integer; it's a pixel number.
END
;
;       ============================
;       =====>> DECIDE IF CUTS NEED UPDATING AND DO IT
;       ============================
;
;+
; NAME:
;       look_cut
; PURPOSE:
;       This procedure updates the horizontal and vertical cuts.
; INPUTS:
;       tmp = structure containing state information, either all or zoom
;       xn,yn = device coordinates of the mouse
;       xincr,yincr = increments, in data coordinates, to be added to
;         the converted values of xn,yn
; MODIFICATION HISTORY:
;       add xn,yn,xincr,yincr, 21 Mar 95, FKK
;       xincr, yincr not used anymore so remove them, 9 Sep 95, FKK
;       add crosshairs code and docrosshairs flag, 9 Sep 95, FKK
;-
pro look_cut,tmp,xn,yn,docrosshairs = docrosshairs
COMMON look,images,catalog,index
COMMON look_common,thisimage,zoomimage,fullimage,ws,all,zoom
;   Don't do cross cut plots if the cursor is moved in a window that is
;   showing a plot type other than the default (look_image) or contour
if (ws.plottype ne 0 and ws.plottype ne 1) and $
 ((ws.plotfullzoom eq 0 and tmp.id eq zoom.id) or $
  (ws.plotfullzoom eq 1 and tmp.id eq all.id) or $
   ws.plotfullzoom eq 2) then return
;
;	=====>> Convert device to data coordinates.
;
wset,tmp.id
xm = look_cc(xn,tmp.slopes(0),tmp.intercepts(0))+tmp.xrange(0)
ym = look_cc(yn,tmp.slopes(1),tmp.intercepts(1))+tmp.yrange(0)
;   Adjust cursor coordinates to force crosshairs into the middle of the
;   current data cell (DSR)
xn = fix ((xm + 0.5 - tmp.xrange(0) - tmp.intercepts(0)) / tmp.slopes(0))
yn = fix ((ym + 0.5 - tmp.yrange(0) - tmp.intercepts(1)) / tmp.slopes(1))
;  print,format = '($,4i5,a)',xm,ym,xm-tmp.datlast(0),ym-tmp.datlast(1),':'
;
;	=====>> Is the mouse inside the plot region?
;
IF ((xm<tmp.xrange(1)>tmp.xrange(0)) EQ xm) AND ((ym<tmp.yrange(1)>tmp.yrange(0)) EQ ym) THEN BEGIN

;   Changed 052097, DSR:  decoupled ws.cut_oplot and ws.last_cutwid.
;   ws.last_cutwid has the ID of the window used to make the last cut plots;
;   ws.cut_oplot indicates whether the cut plots should be updated.  In some
;   cases (following calls to look_span_update), the cut windows need to be
;   redrawn, but the crosshairs remain visible.
;;  ws.cut_oplot = ws.last_cutwid EQ tmp.id ; 1 --> oplot 2x, 0 --> oplot 1x

;   Erase crosshairs in other window, if switching windows
  if ws.last_cutwid ne -1 and ws.last_cutwid ne tmp.id and $
   keyword_set(docrosshairs) THEN BEGIN
     wset,ws.last_cutwid
     device,set_graphics = 6                ; XOR mode
     if (ws.last_cutwid eq all.id) then xl = all.devlast(0) $
        else xl = zoom.devlast(0)
     if xl ge 0 then plots,[xl,xl],[0,!d.y_size],/dev
     if (ws.last_cutwid eq all.id) then yl = all.devlast(1) $
        else yl = zoom.devlast(1)
     if yl ge 0 then plots,[0,!d.x_size],[yl,yl],/dev
     device,set_graphics = 3                ; default (overwrite) mode
  ENDIF

;   Fix added to clear (and force a redraw of) the horizontal and vertical
;   cut windows, once the cursor has switched between the full/zoom windows
  if (ws.last_cutwid ne tmp.id) then begin
     wset,tmp.vcutid              ; Activate the vertical cut window
     erase                        ; Clear the vertical cut window
     wset,tmp.hcutid              ; Activate the horizontal cut window
     erase                        ; Clear the horizontal cut window
     wset,tmp.id                  ; Reset the active window
     tmp.datlast(0) = -1          ; Invalidate the saved X coordinate
     tmp.datlast(1) = -1          ; Invalidate the saved Y coordinate
  endif
;;  ws.last_cutwid = tmp.id
  newy = (ym NE tmp.datlast(1))
  IF newy THEN BEGIN
    wset,tmp.hcutid
    xdata = lindgen(tmp.width(0))+tmp.xrange(0)
    nx = n_elements(xdata)
    ydata = thisimage(tmp.xrange(0):tmp.xrange(1),ym)
    ny = n_elements(ydata)
    IF NOT ws.cut_oplot(0) THEN erase
    plot,xdata,ydata,xstyle = 1,ystyle = 1 $
      ,position = tmp.position,yrange = tmp.tvrange,xrange = tmp.xrange $
      ,title = 'HORIZONTAL CUT',xtit = ws.units(0),ytit = ws.units(2) $
      ,/nodata,/noerase
    IF ws.cut_oplot(0) THEN $
      oplot,ws.hcutxdata(0:nx-1),ws.hcutydata(0:ny-1) $
      ,color = !p.background,psym = 10*(nx LT 100)
;    IF ws.cut_oplot(0) THEN BEGIN; this doesn't work: Does oplot set_graph?
;      device,set_graphics = 6   ; XOR mode to blank out previous cut
;      plots,ws.hcutxdata(0:nx-1,0),ws.hcutydata(0:ny-1,0) ; replot previous
;      device,set_graphics = 3   ; COPY mode to plot new cut
;    ENDIF
    oplot,xdata,ydata,psym = 10*(nx LT 100)
    ws.hcutxdata(0:nx-1) = xdata ; save it for the next call
    ws.hcutydata(0:ny-1) = ydata ; ditto
    tmp.datlast(1) = ym
    IF keyword_set(docrosshairs) THEN BEGIN
      wset,tmp.id
      device,set_graphics = 6
      plots,[0,!d.x_size],[yn,yn],/dev
      yl = tmp.devlast(1)
;   Erase the previous crosshair, unless this is the first entry in
;   the new window
      if ws.last_cutwid eq tmp.id and yl GE 0 THEN $
         plots,[0,!d.x_size],[yl,yl],/dev
      device,set_graphics = 3
      tmp.devlast(1) = yn
    ENDIF
  ENDIF
  newx = (xm NE tmp.datlast(0))
  IF newx THEN BEGIN
    wset,tmp.vcutid
    xdata = reform(thisimage(xm,tmp.yrange(0):tmp.yrange(1)))
    nx = n_elements(xdata)
    ydata = lindgen(tmp.width(1))+tmp.yrange(0)
    ny = n_elements(ydata)
    IF NOT ws.cut_oplot(1) THEN erase
    IF nx GE 2 THEN $      ; bug: need 2 pts in xrange
      plot,xdata,ydata,xstyle = 1 $
      ,ystyle = 1,position = tmp.position,title = 'VERTICAL CUT' $
      ,xrange = tmp.tvrange,yrange = tmp.yrange $
      ,xtit = ws.units(2),ytit = ws.units(1) $
      ,/nodata,/noerase
    IF ws.cut_oplot(1) THEN BEGIN
      IF nx GE 100 THEN BEGIN   ; erase the previous data---no histogram
        oplot,ws.vcutxdata(0:nx-1),ws.vcutydata(0:ny-1),color = !p.background
      ENDIF ELSE BEGIN          ; vertical histogram (can't use psym=10)
        ypsi = lindgen(ny-1)
        yps = ws.vcutydata(0:ny-1)
        ps = [1,1]#((yps(ypsi+1)+yps(ypsi))/2.)
        xps = [1,1]#ws.vcutxdata(0:nx-1)
        oplot,xps,[yps(0),ps(*),yps(ny-1)],color = !p.background
      ENDELSE
    ENDIF
    IF nx GE 100 THEN BEGIN     ; plot the current data---no histogram
      oplot,xdata,ydata
    ENDIF ELSE BEGIN            ; vertical histogram (can't use psym=10)
      ypsi = lindgen(ny-1)
      yps = ydata
      ps = [1,1]#((yps(ypsi+1)+yps(ypsi))/2.)
      xps = [1,1]#xdata
      oplot,xps,[yps(0),ps(*),yps(ny-1)]
    ENDELSE
    ws.vcutxdata(0:nx-1) = xdata ; save it for the next call
    ws.vcutydata(0:ny-1) = ydata ; ditto
    tmp.datlast(0) = xm
    IF keyword_set(docrosshairs) THEN BEGIN
      wset,tmp.id
      device,set_graphics = 6
      plots,[xn,xn],[0,!d.y_size],/dev
      xl = tmp.devlast(0)
;   Erase the previous crosshair, unless this is the first entry in
;   the new window
      if ws.last_cutwid eq tmp.id and xl GE 0 THEN $
         plots,[xl,xl],[0,!d.y_size],/dev
      device,set_graphics = 3
      tmp.devlast(0) = xn
    ENDIF
  ENDIF
  IF newx OR newy THEN BEGIN
    ws.report(1:2) = [' x='+strtrim(xm,2),' y='+strtrim(ym,2)]
    z = thisimage(xm,ym)
    IF ws.size(ws.size(0)+1) EQ 1 THEN z = long(z) ; Don't print byte as char.
    ws.report(3) = ' z='+strtrim(z,2)
    widget_control,ws.reptext,set_value = ws.star+ws.report
  ENDIF
  ws.last_cutwid = tmp.id
;   Set flag to update cut plots next time through (this may subsequently be
;   unset by look_span_update)
  ws.cut_oplot = 1
ENDIF
return
END
;
;       ============================
;       =====>> Calculate indices unique to first argument
;       ============================
;
FUNCTION look_unique,a,b
; a,b = where vectors presumably with some common elements
; This function will return all of a's elements that are not in b.
; IN this case:
; a = indices for the entire image
; b = indices of the selected source region
tmp = [a,b]                     ; David Stern's idea.
tmp = tmp(sort(tmp))            ; Sort.  Look for identical pairs, i.e.,  matches.
match = where((tmp(1:*) - tmp) LE 0,cnt)
IF cnt EQ 0 THEN return,a ELSE BEGIN
  test = [a,tmp(match)]         ; form vector of matches and one of inputs
  test = test(sort(test))       ; and sort it
  test = [test(0)-1,test,test(n_elements(test)-1)+1] ; add elements to both ends
; find each element where both neighbors are different
; Example:
;  test=[-1,0,1,2,2,3,3,5,6] ; after adding additional first and last elements
;  test-test(1:*)=[-1,-1,-1,0,-1,0,-2,-1], so ... NE 0 =   [1,1,1,0,1,0,1,1]
;  test(2:*)-test(1:*)=[-1,-1,0,-1,0,-2,-1], so ... NE 0 = [1,1,0,1,0,1,1]
;  test(...AND...+1)=[0,1,5]
;  Looks right.
  return,test(where(((test-test(1:*)) NE 0) AND ((test(2:*)-test(1:*)) NE 0))+1)
ENDELSE
END
;
;       ============================
;       =====>> Print a line containing statistics
;       ============================
;
PRO look_print_stats,sp,image,tagtext,wid
IF sp(0) eq -1 THEN text = 'No points in '+tagtext+' region.' ELSE BEGIN
  nsp = n_elements(sp)
  IF nsp GT 1 THEN begin
     res = moment (image(sp), sdev = sd)
     mean = res(0)
  endif ELSE BEGIN
    sd = 0
    mean = image(sp)
  ENDELSE
  max = max(image(sp),min = min)
  text = tagtext+': #pts='+strtrim(n_elements(sp),2) $
    +' mean='+strtrim(mean,2)+' sd='+strtrim(sd,2) $
    +' lo='+strtrim(min,2)+' hi='+strtrim(max,2)
ENDELSE
widget_control,wid,/append,set_value = text
return
END


;   Find the value of the nth percentile in an image, based on its
;   histogram.  The percentile should be specified between 0 and 100.
function look_findbin, thisimage, percentile

   bmin = min (thisimage)
   bmax = max (thisimage)
   if bmax - bmin lt 100 then begin
;   Calculate bin size to create 100 bins
      bins = (bmax - bmin) / 100.
   endif else if bmax - bmin gt 200 then begin
;   Limit number of bins to a maximum of 200
      bins = (bmax - bmin) / 200.
   endif else begin
      bins = 1
   endelse
   ihist = histogram (thisimage, binsize=bins, omin=histmin, omax=histmax)
   histdim = n_elements (ihist)

   i = 0
   cutoff = (percentile / 100.) * n_elements (thisimage)
   cuttot = ihist(0)
   while (cuttot lt cutoff and i lt histdim) do begin
      i = i + 1
      cuttot = cuttot + ihist(i)
   endwhile
   binval = i * bins + histmin
   return, binval
end


pro look_update_scale_label, which, value
   COMMON look_common,thisimage,zoomimage,fullimage,ws,all,zoom
   if which ne 0 and which ne 1 then return
   if value ge 1000 then $
      label = strtrim (string (value, format = '(i9)'), 2) $
   else $
      label = strtrim (string (value, format = '(g9.3)'), 2)

   widget_control, ws.setids(which), set_value = label
end


;   Update the sliders and the min/max value display windows
pro look_update_scales, minval, maxval
   COMMON look_common,thisimage,zoomimage,fullimage,ws,all,zoom
   widget_control,ws.spansliders(0),set_value = minval
   widget_control,ws.spansliders(1),set_value = maxval
;;   widget_control,ws.setids(0),set_value = strtrim(string(minval),2)
;;   widget_control,ws.setids(1),set_value = strtrim(string(maxval),2)
   look_update_scale_label, 0, minval
   look_update_scale_label, 1, maxval
end


;
;       ============================
;       =====>> Update the all and zoom displays.
;       ============================
;
PRO look_update,flag
   COMMON look,images,catalog,index
   COMMON look_common,thisimage,zoomimage,fullimage,ws,all,zoom
   COMMON colors, r_orig, g_orig, b_orig, r_curr, g_curr, b_curr
;
;   =====>> If no zoom region has been selected, show full image
;   =====>> in both windows.  Otherwise, add zoom and embellish full image.
;
   ws.ctrange = all.tvrange        ; make slider and color table ranges equal
   tvlct,r_orig,g_orig,b_orig      ; reset the color table
   ws.zoomshown = 1
   ws.fullshown = 1
   all.devlast = [-1,-1]           ; to omit XOR of old (now erased) crosshairs
   zoom.devlast = [-1,-1]
   ws.last_cutwid = -1             ; so cuts get updated

;   =====>> Check input according to flag
;   =====>> flag=0: no check
;   =====>> flag=1: xrange/yrange --> center/width
;   =====>> flag=2: center/width --> xrange/yrange
;
   CASE flag OF
     0:
     1: BEGIN
        zoom.center = [(zoom.xrange(1)+zoom.xrange(0))/2 $
                      ,(zoom.yrange(1)+zoom.yrange(0))/2]
        zoom.width = [(zoom.xrange(1)-zoom.xrange(0))+1 $
                     ,(zoom.yrange(1)-zoom.yrange(0))+1]
        END
     2: BEGIN
        half = -zoom.width/2
        zoom.xrange = (zoom.center(0)+[half(0),(zoom.width(0)+half(0)) > 1]) $
          > all.xrange(0) < all.xrange(1)
        zoom.yrange = (zoom.center(1)+[half(1),(zoom.width(1)+half(1)) > 1]) $
          > all.yrange(0) < all.yrange(1)
        END
   ENDCASE

;   Create a temporary array for the zoom image

   IF total(zoom.xrange-all.xrange+zoom.yrange-all.yrange) EQ 0 THEN BEGIN
      widget_control,ws.reptext,set_value = ws.star+ws.report
      zoomimage = thisimage          ; case where zoom = full image
   ENDIF else begin
      zoomimage = thisimage(zoom.xrange(0):zoom.xrange(1) $
                           ,zoom.yrange(0):zoom.yrange(1))
      zoom.imrange = [min(zoomimage),max(zoomimage)]
   ENDELSE

;   Make the plot in the "zoom" window

   wset, zoom.id                   ; activate the zoom window
   if ws.plottype ne 0 and ws.plotfullzoom ne 1 then begin

;   Special plot case.  For "Full only", a special plot based on the full
;   region is made in the zoom window.  For "Full & Zoom", a special plot is
;   made from the zoom image.

      if ws.plotfullzoom eq 0 then $
         look_update_special, thisimage, 'FULL' $
      else $
         look_update_special, zoomimage, 'ZOOM'
   endif else begin

;   For "Zoom only", a special plot is drawn in the full window, but the zoom
;   window contains the normal image.

;;      if all.scaletype eq 11 and all.scalefunc(0) ne "" then $
;;         look_image,call_function(all.scalefunc(0),zoomimage) $
;;           ,min=0,max=(!d.n_colors<!d.table_size)-1 $
;;           ,title = 'ZOOM IMAGE',xtit = ws.units(0),ytit = ws.units(1) $
;;      else if all.scaletype eq 10 then $
;;         look_image,hist_equal(zoomimage) $
;;           ,min=0,max=(!d.n_colors<!d.table_size)-1 $
;;           ,title = 'ZOOM IMAGE',xtit = ws.units(0),ytit = ws.units(1) $
;;      else $
         look_image,look_scl(zoomimage,all.tvrange) $
           ,min=0,max=(!d.n_colors<!d.table_size)-1 $
           ,title = 'ZOOM IMAGE',xtit = ws.units(0),ytit = ws.units(1)
   endelse

;   Get zoom window coordinates and conversion factors

   zoom.position = [!x.window(0),!y.window(0),!x.window(1),!y.window(1)]

;   Corrected slope / intercept calculations.  The conversion factors from
;   look_si are not quite correct; the problem becomes more apparent with
;   smaller zoom regions.  look_si2 is an improved version (DSR).
;;   look_si,[0,1],[0,1],slopes,intercepts
   look_si2,zoom.xrange,zoom.yrange,slopes,intercepts
   zoom.slopes = slopes
   zoom.intercepts = intercepts
;
;   =====>> Report the center and width of zoom region
;
   ws.report(5) = ' x='+strtrim(zoom.center(0),2)
   ws.report(6) = ' y='+strtrim(zoom.center(1),2)
   z = thisimage(zoom.center(0),zoom.center(1))
   IF ws.size(ws.size(0)+1) EQ 1 THEN z = long(z) ; Don't print a byte as a char.
   ws.report(7) = ' z='+strtrim(z,2)
   ws.report(9) = ' x='+strtrim(zoom.width(0),2)
   ws.report(10) = ' y='+strtrim(zoom.width(1),2)
   IF n_elements(zoomimage) GT 1 THEN begin
      res = moment (zoomimage, sdev = sdev)
      zoom.sd = sdev
      mean = res(0)
   endif ELSE BEGIN
      zoom.sd = 0.
      mean = 0
   ENDELSE
   zoom.mean = mean
   ws.report(15) = ' mean='+strtrim(zoom.mean,2)
   ws.report(16) = ' sd='+strtrim(zoom.sd,2)
   widget_control,ws.reptext,set_value = ws.star+ws.report
;
;   =====>> Renew the full image with the zoom region marked with
;   =====>> a 0,1,2-pixel-wide border of contrasting values---map upper
;   =====>> and lower halves of image values into each other.
;   =====>> The border width depends on the image size.
;
   wset,all.id                     ; activate the full window
   tmpimage = thisimage

   IF (all.x < all.y) GT 10 THEN indices = 0
   IF (all.x < all.y) GT 100 THEN indices = [0,1]
   IF (all.x < all.y) GT 255 THEN indices = [-1,0,1]

;   If the zoom region is active, draw a box corresponding to it on top of
;   the full image

   IF n_elements (indices) ne 0 and $
    total(zoom.xrange-all.xrange+zoom.yrange-all.yrange) NE 0 THEN begin
      xmn = zoom.xrange(0)
      xmx = zoom.xrange(1)
      ymn = zoom.yrange(0)
      ymx = zoom.yrange(1)
      midrange = 0.5*(all.tvrange(1)-all.tvrange(0))
      midway = all.tvrange(0)+midrange
      therows = [ymn+indices,ymx+indices] < all.yrange(1) > all.yrange(0)
      halfperi = tmpimage(xmn:xmx,therows)
      hilo = halfperi*0-1
      upper = where(halfperi LT midway)
      IF upper(0) GE 0 THEN hilo(upper) = 1           ; -1(1) for >(<) midway
      tmpimage(xmn:xmx,therows) = halfperi+hilo*midrange
      thecols = [xmn+indices,xmx+indices] < all.xrange(1) > all.xrange(0)
      halfperi = tmpimage(thecols,ymn:ymx)
      hilo = halfperi*0-1
      upper = where(halfperi LT midway)
      IF upper(0) GE 0 THEN hilo(upper) = 1           ; -1(1) for >(<) midway
      tmpimage(thecols,ymn:ymx) = halfperi+hilo*midrange
   ENDIF

;   Make the plot in the "full" window

   if ws.plottype ne 0 and ws.plotfullzoom ne 0 then begin

;   Special plot case.  For "Zoom only", a special plot based on the zoom
;   region is made in the full window.  For "Full & Zoom", a special plot is
;   made from the full image.

      if ws.plotfullzoom eq 1 then $
         look_update_special, zoomimage, 'ZOOM' $
      else $
         look_update_special, thisimage, 'FULL'
   endif else begin

;   For "Full only", a special plot is drawn in the zoom window, but the full
;   window contains the normal image.

;;      if all.scaletype eq 11 and all.scalefunc(0) ne "" then $
;;         look_image,call_function(all.scalefunc(0),tmpimage) $
;;           ,min=0,max=(!d.n_colors<!d.table_size)-1 $
;;           ,title = 'FULL IMAGE',xtit = ws.units(0),ytit = ws.units(1) $
;;      else if all.scaletype eq 10 then $
;;         look_image,hist_equal(tmpimage) $
;;           ,min=0,max=(!d.n_colors<!d.table_size)-1 $
;;           ,title = 'FULL IMAGE',xtit = ws.units(0),ytit = ws.units(1) $
;;      else $
         look_image,look_scl(tmpimage,all.tvrange) $
           ,min=0,max=(!d.n_colors<!d.table_size)-1 $
           ,title = 'FULL IMAGE',xtit = ws.units(0),ytit = ws.units(1)
   ENDELSE
;
;	=====>> If present and toggled on, call each viewer.
;
   FOR i = 0,n_elements(ws.viewers)-1 DO IF (ws.viewers(i) NE '') AND (ws.viewers_set(i) NE 0) THEN call_procedure,ws.viewers(i)
   return
END



;   Called by look_update to make special plots.  The desired target window
;   (full or zoom) should be active before calling this routine.
;   "title" should be either 'FULL' or 'ZOOM'
pro look_update_special, image, title
   COMMON look_common,thisimage,zoomimage,fullimage,ws,all,zoom

   case ws.plottype of
      1: begin                     ; contour plot
            si = size (image)
            contour, image, nlevels = 8, title = title + ' CONTOUR', $
                     xrange = [0-0.5,si(1)-0.5], $
                     yrange = [0-0.5,si(2)-0.5], xstyle = 1, ystyle = 1
         end
      2: begin                     ; histogram
            plot, title = title + ' HISTOGRAM', histogram(float(image), $
               binsize = (float(all.tvrange(1))-all.tvrange(0))/(all.x < 100))
         end
      3: begin                     ; surface
            surface, title = title + ' SURFACE', $
             congrid(look_scl(image,all.tvrange),all.x < ws.mxd,all.y < ws.mxd)
         end
      4: begin                     ; shade_surf
            shade_surf, title = title + ' SHADE_SURF', $
             congrid(look_scl(image,all.tvrange),all.x < ws.mxd,all.y < ws.mxd)
         end
      5: begin                     ; show3
            show3, $
             congrid(look_scl(image,all.tvrange),all.x < ws.mxd,all.y < ws.mxd)
         end
      6: begin                     ; tvscl
            erase
            tvscl, look_scl(image,all.tvrange)
         end
   endcase
end



;       ============================
;       =====>> Set PS attributes
;       ============================
;
PRO look_PSatt
COMMON look_common,thisimage,zoomimage,fullimage,ws,all,zoom
parent = widget_base(/column,title = 'Choose PS attributes')
row = widget_base(parent,/row,/nonexclusive)
tmp = widget_button(row,value = 'DONE',uvalue = 'DONE')
tags = strupcase(tag_names(ws.PSatt))
w = where(tags NE 'BITS')       ; save bits/pixel for next row of widgets
FOR i = 0,n_elements(w)-1 DO BEGIN
  tmp = widget_button(row,value = '/'+tags(w(i)),uvalue = tags(w(i)))
  widget_control,tmp,set_button = ws.PSatt.(w(i))
ENDFOR
row = widget_base(parent,/row)
bopt = ['1','2','4','8']
tmp = (where(ws.PSatt.bits EQ bopt))(0) > 0
tmp = cw_bselector(row,bopt,uvalue = 'BITS',label_left = 'Bits/pixel',set_value = tmp)
tmp = widget_button(row,value = 'New file',uvalue = 'PSFILENAME')
PSfileID = widget_text(row,/scroll,value = 'Current: '+ws.PSfilename)
widget_control,parent,/realize
;
;       =====>> Get and Process Events Using uvalue's Until Done
;
MORE:
ev = widget_event(parent,/nowait)
IF ev.id EQ 0 THEN GOTO,MORE
widget_control,ev.id,get_uvalue = uvalue
CASE 1 OF
  uvalue EQ 'BITS': ws.PSatt.bits = bopt(ev.value)
  uvalue EQ 'DONE': BEGIN
    widget_control,parent,/destroy
    ws.PSattstr = ''
    tags = tag_names(ws.PSatt)
    FOR i = 0,n_tags(ws.PSatt)-1 DO BEGIN
      ws.PSattstr = ws.PSattstr+','+tags(i)+'='+strtrim(ws.PSatt.(i),2)
    ENDFOR
    return
  END
  uvalue EQ 'PSFILENAME': BEGIN
    ws.PSfilename = pickfile(file = ws.PSfilename,filter = '*.ps',/write)
    widget_control,PSfileID,set_value = 'Current: '+ws.PSfilename
  END
  ELSE: stat = execute('ws.PSatt.'+uvalue+' = ev.select')
ENDCASE
GOTO,MORE
END
;
;       ============================
;       =====>> DO THE OPTIONAL PLOTS (called from look_event)
;       ============================
;
PRO look_optmenu,uvalue,ev
; uvalue = uvalue from event
; ev = event structure
COMMON look,images,catalog,index
COMMON look_common,thisimage,zoomimage,fullimage,ws,all,zoom
COMMON colors, r_orig, g_orig, b_orig, r_curr, g_curr, b_curr
;
;       =====>> Choose number of plots: full or zoom or both
;
IF ws.plotfullzoom EQ 2 THEN nplots = 2 ELSE nplots = 1
IF uvalue EQ 'movie' AND (nplots EQ 2) THEN BEGIN
  widget_control,ws.comtext,/append,set_value = $
    'Movie mode requires only full or zoom---not both.  Using full.'
  nplots = 1
ENDIF

;
;       =====>> Loop over the 1 or 2 plots.
;
id = [[zoom.id,all.id,all.id],[0,0,zoom.id]]
wid = [[zoom.wid,all.wid,all.wid],[0,0,zoom.wid]]
titles = [['FULL','ZOOM','FULL'],['','','ZOOM']]+' '
catalog_or_movie_or_blink = $
  (uvalue EQ 'cat to screen') OR (uvalue EQ 'cat to window') $ ; catalog
  OR (uvalue EQ 'movie') $ ; movie
  OR (uvalue EQ 'blink to screen') OR (uvalue EQ 'blink to window') ; blink
FOR n = 0,nplots-1 DO BEGIN
  wset,id(ws.plotfullzoom,n)
  title = titles(ws.plotfullzoom,n)
  dofull = strpos(title,'FULL') GE 0
  IF id(ws.plotfullzoom,n) EQ zoom.id THEN ws.zoomshown = 0
  IF id(ws.plotfullzoom,n) EQ all.id THEN ws.fullshown = 0
  IF (n EQ 1) OR (ws.plotfullzoom EQ 1) THEN image = zoomimage ELSE image = thisimage
  device,get_screen_size = scrdims
  si = size(image)
  wherecust = where (all.scalefunc eq uvalue, wherecount)
  CASE 1 OF
    uvalue EQ 'All Four Windows': BEGIN
      wset,all.hcutid           ; Get ready to read one draw window's data.
      one = tvrd()              ; Read one image; all others are same size.
      szt = size(one)           ; Make array for all four: 2 plots & 2 images
      tmp = make_array(size = [2,szt(1:2)*2,szt(3),szt(4)*4])
      x = szt(1)                ; shorthand
      y = szt(2)                ; ditto
      tmp(0:x-1,y:*) = one      ; Place the horizontal cut plot, as an image.
      wset,zoom.id              ; Move to zoom window.
      tmp(x:*,y:*) = tvrd()     ; Place it as an image in the upper right.
      wset,all.id               ; Move to full window.
      tmp(0:x-1,0:y-1) = tvrd() ; Place it as an image in the lower left.
      wset,all.vcutid           ; Move to full vcut window.
      tmp(x:*,0:y-1) = tvrd()   ; Place it as an image in the lower right.
      look_put,tmp,1,1,filename = ws.PSfilename ; Output the entire array.
    END
    catalog_or_movie_or_blink: BEGIN
      to_screen = strpos(uvalue,'to screen') GE 0
      IF to_screen THEN BEGIN
        bigbase = widget_base(title = title+'  (To exit, kill window.)' $
                              ,/row,group = ev.top)
        bigdraw = widget_draw(bigbase $
                              ,xsize = si(1)*((scrdims(0)-25)/si(1)) $
                              ,ysize = si(2)*((scrdims(1)-25)/si(2)))
        widget_control,/realize,bigbase
        widget_control,get_value = bigid,bigdraw
        wset,bigid
        tvlct, r_orig, g_orig, b_orig ; update color table
        look_catalog,uvalue,dofull,bigid
        widget_control,bigbase,/destroy
      ENDIF ELSE BEGIN
        look_catalog,uvalue,dofull,wid(ws.plotfullzoom,n)
      ENDELSE 
    END
    uvalue EQ 'contour': BEGIN
;;       contour,image,nlevels = 8,title = title+'CONTOUR'
       ws.plottype = 1
       look_update, 0
    END
    uvalue EQ 'full screen': BEGIN
      times = (scrdims(0)/si(1)) < (scrdims(1)/si(2))
      IF times EQ 0 THEN BEGIN
        slide_image,look_scl(image,all.tvrange)
      ENDIF ELSE BEGIN
        bigbase = widget_base(title = title+strtrim(times,2)+ $
                              'X enlargement  (To exit, kill window.)' $
                              ,/row,group = ev.top)
        bigdraw = widget_draw(bigbase,xsize = si(1)*times,ysize = si(2)*times)
        widget_control,/realize,bigbase
        widget_control,get_value = bigid,bigdraw
        wset,bigid
        tvlct, r_orig, g_orig, b_orig ; update via changing the color table
        look_exptv,look_scl(rebin(image,si(1)*times,si(2)*times,/sample),all.tvrange)
      ENDELSE
    END
    uvalue EQ 'full window': look_exptv,look_scl(image,all.tvrange),min=0,max=(!d.n_colors<!d.table_size)-1
    uvalue EQ 'Full Window Only': BEGIN
      wset,all.id
      look_put,tvrd(),1,1,file = ws.PSfilename
    END
    uvalue EQ 'histogram': BEGIN
;;       plot,title = title+'HISTOGRAM',histogram(float(image) $
;;         ,binsize = (float(all.tvrange(1))-all.tvrange(0))/(all.x < 100))
       ws.plottype = 2
       look_update, 0
    END
    uvalue EQ 'Laplacian': BEGIN
      IF dofull THEN thisimage = convol(thisimage,[[0,-1,0],[-1,4,-1],[0,-1,0]])
      look_update,0
    END
    uvalue EQ 'leefilt': BEGIN
      IF dofull THEN thisimage = leefilt(thisimage,ws.kernel_width)
      look_update,0
    END
    uvalue EQ 'look_image': BEGIN
;;   look_image,title = title+'IMAGE',min=0,max=(!d.n_colors<!d.table_size)-1 $
;;       ,look_scl(image,all.tvrange),xtit = ws.units(0),ytit = ws.units(1)
       ws.plottype = 0
       look_update, 0
    END

    uvalue EQ 'median': BEGIN
      IF dofull THEN thisimage = median(thisimage,ws.kernel_width)
      look_update,0
    END
    uvalue EQ 'region of interest': BEGIN
      look_exptv,look_scl(image,all.tvrange),zoom = z_xy,offset = offset
      blink0 = look_scl(image,all.tvrange)
      blink1 = blink0
      min = min(image)
      sp = look_defroi(wid(ws.plotfullzoom,n),zoom = z_xy,offset = offset $
                       ,size = (size(image))(1:2),image = blink0)
      widget_control,/hourglass
      bg = look_unique(lindgen(n_elements(image)),sp)
      look_print_stats,sp,image,'inside',ws.comtext
      look_print_stats,bg,image,'outside',ws.comtext
      IF bg(0) NE -1 THEN blink0(bg) = min
      IF sp(0) NE -1 THEN blink1(sp) = min
      look_exptv,blink0,zoom = z_xy,offset = offset,image = blink0
      look_exptv,blink1,zoom = z_xy,offset = offset,image = blink1
      look_blink,blink0,blink1,wid(ws.plotfullzoom,n)
    END
    uvalue EQ 'Set PS attributes': look_PSatt
    uvalue EQ 'shade_surf': BEGIN
;;       shade_surf,title = title+'SHADE_SURF' $
;;        ,congrid(look_scl(image,all.tvrange),all.x < ws.mxd,all.y < ws.mxd)
       ws.plottype = 4
       look_update, 0
    END
    uvalue EQ 'show3': BEGIN
;;       show3 $
;;      ,congrid(look_scl(image,all.tvrange),all.x < ws.mxd,all.y < ws.mxd)
       ws.plottype = 5
       look_update, 0
    END
    uvalue EQ 'smooth': BEGIN
      IF dofull THEN thisimage = smooth(thisimage,ws.kernel_width)
      look_update,0
    END
    uvalue EQ 'surface': BEGIN
;;       surface,title = title+'SURFACE' $
;;        ,congrid(look_scl(image,all.tvrange),all.x < ws.mxd,all.y < ws.mxd)
       ws.plottype = 3
       look_update, 0
    END
    uvalue EQ 'tvscl': BEGIN
;;       tvscl,look_scl(image,all.tvrange)
       ws.plottype = 6
       look_update, 0
    END
    uvalue EQ 'type image': look_xdisplay,image,title = title
    strpos(uvalue,'-->on/all mouse') GT 0: BEGIN
      w = ((where(ws.viewers+'-->on/all mouse' EQ uvalue)))(0)
      IF w GT -1 THEN BEGIN
        IF ws.viewers_set(w) EQ 0 THEN call_procedure,ws.viewers(w),/create
        ws.viewers_set(w) = 2   ; toggle on for all mouse events
      ENDIF
    END
    strpos(uvalue,'-->on/click only') GT 0: BEGIN
      w = ((where(ws.viewers+'-->on/click only' EQ uvalue)))(0)
      IF w GT -1 THEN BEGIN
        IF ws.viewers_set(w) EQ 0 THEN call_procedure,ws.viewers(w),/create
        ws.viewers_set(w) = 1   ; toggle on for mouse clicks only
      ENDIF
    END
    strpos(uvalue,'-->off') GT 0: BEGIN
      w = ((where(ws.viewers+'-->off' EQ uvalue)))(0)
      IF w GT -1 THEN BEGIN
        IF ws.viewers_set(w) NE 0 THEN call_procedure,ws.viewers(w),/destroy
        ws.viewers_set(w) = 0   ; toggle off
      ENDIF
    END
    uvalue EQ 'xloadct': xloadct
    uvalue EQ 'Zoom Window Only': BEGIN
      wset,zoom.id
      look_put,tvrd(),1,1,file = ws.PSfilename
    END

;   Auto-scaling cases
    uvalue EQ 'All data': begin
       all.scaletype = 1
       look_auto_minmax
       look_update, 0
    end
    uvalue EQ 'Omit top / bottom 5%': begin
       all.scaletype = 2
       look_auto_minmax
       look_update, 0
    end
    uvalue EQ 'Omit top / bottom 10%': begin
       all.scaletype = 3
       look_auto_minmax
       look_update, 0
    end
    uvalue EQ 'Mean +/- 1 sigma': begin
       all.scaletype = 4
       look_auto_minmax
       look_update, 0
    end
    uvalue EQ 'Mean +/- 2 sigma': begin
       all.scaletype = 5
       look_auto_minmax
       look_update, 0
    end
    uvalue EQ 'Mean +/- 3 sigma': begin
       all.scaletype = 6
       look_auto_minmax
       look_update, 0
    end
    uvalue EQ 'Median +/- 1 sigma': begin
       all.scaletype = 7
       look_auto_minmax
       look_update, 0
    end
    uvalue EQ 'Median +/- 2 sigma': begin
       all.scaletype = 8
       look_auto_minmax
       look_update, 0
    end
    uvalue EQ 'Median +/- 3 sigma': begin
       all.scaletype = 9
       look_auto_minmax
       look_update, 0
    end
    uvalue EQ 'hist_equal': begin
       all.scaletype = 10
       look_auto_minmax
       look_update, 0
    end
;;    uvalue EQ 'Custom': begin
    wherecount gt 0: begin
       all.scaletype = 11 + wherecust(0)
       look_auto_minmax
       look_update, 0
    end

    ELSE: BEGIN
      IF ws.ispro(where(ws.optmenu.name EQ uvalue)) THEN BEGIN
        call_procedure,uvalue,look_scl(image,all.tvrange)
      ENDIF ELSE BEGIN
        IF dofull THEN thisimage = call_function(uvalue,thisimage)
        look_update,0
      ENDELSE
    END
  ENDCASE
ENDFOR
return
END
;
;       ============================
;       =====>> Update the images after slider events or increments.
;       ============================
;
FUNCTION look_span_update,value,j
; value = new value of the tvrange(j) for all and zoom
; j = which value, minimum (j=0) or maximum (j=1) to update
COMMON look_common,thisimage,zoomimage,fullimage,ws,all,zoom
; mini = lower bounds for all and zoom
; maxi = upper bounds for all and zoom
IF j EQ 0 THEN BEGIN
  mini = [all.tvrange(1),zoom.tvrange(1)]
  maxi = [ws.srange(0),ws.srange(0)]
ENDIF ELSE BEGIN
  mini = [ws.srange(1),ws.srange(1)]
  maxi = [all.tvrange(0),zoom.tvrange(0)]
ENDELSE
save = [all.tvrange(j),zoom.tvrange(j)]
all.tvrange(j) = value < mini(0) > maxi(0)
zoom.tvrange(j) = value < mini(1) > maxi(1)
widget_control,ws.spansliders(j),set_value = all.tvrange(j)
IF ws.spanmode(j) NE 1 THEN ws.manrange(j) = all.tvrange(j)
;;widget_control,ws.setids(j),set_value = strtrim(all.tvrange(j),2)
look_update_scale_label, j, all.tvrange(j)
different = (save(0) NE all.tvrange(j)) OR (save(1) NE zoom.tvrange(j))
IF NOT different THEN return,0
;
;       =====>> If possible, alter color table to save time.
;
COMMON colors, r_orig, g_orig, b_orig, r_curr, g_curr, b_curr
ctr = ws.ctrange                ; just a shorthand
tvr = all.tvrange               ; ditto
outside = (ctr(0) GT tvr(0)) OR (ctr(1) LT tvr(1))
ctspan = float(ctr(1)-ctr(0))
IF ctspan GT 0. THEN $
  toosmall = ((tvr(1)-tvr(0))/ctspan) LT 0.1 ELSE toosmall = 1
; return whether either one of the values has changed
;IF ws.debug THEN widget_control,ws.comtext,/append $
;  ,set_value = string([ctr,tvr,outside,toosmall],form = '(4f10.2,2i3)')
IF outside OR toosmall THEN return,different
nc = !d.n_colors < !d.table_size ; was !d.n_colors(=2^24 on true color devices)
n1 = (nc*(tvr(1)-ctr(0))/ctspan) < (nc-1) ; # of colors to set to nc-1
n0 = (nc*(tvr(0)-ctr(0))/ctspan) < n1 ; # of colors to set to 0
s = 0.*r_orig                   ; array of color table indices
IF n1 GT n0 THEN s(n0) = findgen(n1-n0)*nc/(n1-n0) ; ramp spanning color table
s(n1:*) = float(nc-1)           ; indices set to top color index
r_curr = r_orig(s)
g_curr = g_orig(s)
b_curr = b_orig(s)
tvlct, r_curr, g_curr, b_curr   ; update via changing the color table
;   Changed 052097, DSR:  unset ws.cut_oplot to force update of cut windows,
;   but leave ws.last_cutwid alone so the existing crosshairs will be erased
;;ws.last_cutwid = -1		; so cuts get updated
ws.cut_oplot = 0                ; flag to update cut plots
return,0                        ; don't update images
END
;
;       ============================
;       =====>> Check for coordinates in range
;       ============================
;
FUNCTION look_in_range,t,x,y
return,(t(0) GE x(0)) AND (t(0) LE x(1)) AND (t(1) GE y(0)) AND (t(1) LE y(1))
END



;   Determine the min and max data values to map to the color range,
;   depending on the autoscale option
;   Options:
;      no_update       Don't update the scales
;      newrange        Optionally return the newly calculated extrema
;      scaleto         Image to use to determine limits (optional - if omitted,
;                      either the full or zoom image is used)
;      nobound         Don't limit min and max to slider limits
pro look_auto_minmax, newrange = imagerange, no_update = no_update, $
                      scaleto = scaleto, no_bound = no_bound
   COMMON look_common,thisimage,zoomimage,fullimage,ws,all,zoom

;   Reset displayed image in case hist_equal or custom scaling were used

   thisimage = fullimage

;   If set to "Full only" or "Full & Zoom", scale to full image; otherwise,
;   scale only to the zoom image

   if n_elements (scaleto) ne 0 then $
     useimage = scaleto $
   else if ws.plotfullzoom eq 0 or ws.plotfullzoom eq 2 then $
     useimage = thisimage $
   else $
     useimage = zoomimage

;   Find the mean and standard deviation, if needed
   if all.scaletype ge 4 and all.scaletype le 9 then $
      thismom = moment (useimage, sdev=thisdev)

;   Find the median, if needed
   if all.scaletype ge 7 and all.scaletype le 9 then $
      thismed = median (useimage)

   case all.scaletype of
      1: begin                         ; all data
            imagerange = [min (useimage), max (useimage)]
         end
      2: begin                         ; eliminate top / bottom 5%
            imin = look_findbin (useimage, 5)
            imax = look_findbin (useimage, 95)
            imagerange = [imin, imax]
         end
      3: begin                         ; eliminate top / bottom 10%
            imin = look_findbin (useimage, 10)
            imax = look_findbin (useimage, 90)
            imagerange = [imin, imax]
         end
      4: begin                         ; mean +/- 1 sigma
            imin = thismom(0) - all.meansig(0) * thisdev
            imax = thismom(0) + all.meansig(0) * thisdev
            imagerange = [imin, imax]
         end
      5: begin                         ; mean +/- 2 sigma
            imin = thismom(0) - all.meansig(1) * thisdev
            imax = thismom(0) + all.meansig(1) * thisdev
            imagerange = [imin, imax]
         end
      6: begin                         ; mean +/- 3 sigma
            imin = thismom(0) - all.meansig(2) * thisdev
            imax = thismom(0) + all.meansig(2) * thisdev
            imagerange = [imin, imax]
         end
      7: begin                         ; median +/- 1 sigma
            imin = thismed - all.mediansig(0) * thisdev
            imax = thismed + all.mediansig(0) * thisdev
            imagerange = [imin, imax]
         end
      8: begin                         ; median +/- 2 sigma
            imin = thismed - all.mediansig(1) * thisdev
            imax = thismed + all.mediansig(1) * thisdev
            imagerange = [imin, imax]
         end
      9: begin                         ; median +/- 3 sigma
            imin = thismed - all.mediansig(2) * thisdev
            imax = thismed + all.mediansig(2) * thisdev
            imagerange = [imin, imax]
         end
     else: begin                       ; hist_equal, custom scaling functions
            thisimage = look_apply_scalefunc (fullimage)
            look_reset_sliders, thisimage
            zoomimage = look_apply_scalefunc (zoomimage)
            imagerange = [min (thisimage), max (thisimage)]
         end
   endcase

;   Check to prevent exceeding the slider limits

   if not keyword_set (no_bound) then $
     imagerange = imagerange > ws.srange(0) < ws.srange(1)

;   For fixed slider cases, don't use auto value determined above

   if ws.spanmode(0) eq 0 then imagerange(0) = ws.manrange(0)   ; Min slider
   if ws.spanmode(1) eq 0 then imagerange(1) = ws.manrange(1)   ; Max slider

   if not keyword_set (no_update) then begin
      look_update_scales, imagerange(0), imagerange(1)
      i = look_span_update (imagerange(0), 0)
      i = look_span_update (imagerange(1), 1)
   endif
end


;
;       ============================
;       =====>> EVENT HANDLER
;       ============================
;
PRO look_event,ev
COMMON look,images,catalog,index
COMMON look_common,thisimage,zoomimage,fullimage,ws,all,zoom
;
;       =====>> Get value and uvalue for event.
;
IF (where(tag_names(ev) EQ 'VALUE'))(0) EQ -1 THEN BEGIN
  widget_control,ev.id,get_uvalue = uvalue,get_value = value
  value = value(0)              ; make a scalar
ENDIF ELSE BEGIN
  widget_control,ev.id,get_uvalue = uvalue
  value = ev.value
ENDELSE
IF n_elements(uvalue) EQ 0 THEN uvalue = '' ; shouldn't happen
IF ws.debug THEN widget_control,ws.comtext,/append $
  ,set_value = strtrim(uvalue,2) $
  +' '+strtrim(value,2)+' '+strtrim(ev.id,2)+' '+strtrim(ev.top,2) $
  +' '+strtrim(ev.handler,2)
;
;       =====>> Treat different ids in a variety of ways.
;       =====>> Do most of the widgets in a big case statement.
;       =====>> Choose image, cut sliders, autoscale, and draw widget events.
;
;       =====>> PROCESS THE EVENT USING A CASE STATEMENT ON THE UVALUE
;       =====>> But treat tv and zoom separately
;
CASE 1 OF
  strpos(uvalue,'apply') EQ 0: BEGIN
    widget_control,ws.spanmagid,get_value = mag
    IF strpos(uvalue,'+') GE 0 THEN sign = 1 ELSE sign = -1
    IF ws.debug THEN widget_control $
      ,ws.comtext,/append,set_value = uvalue+' '+mag(0)+string(sign)
    mag = sign*float(mag(0))
    domin = (strpos(uvalue,'min') GE 0) OR (strpos(uvalue,'both') GE 0)
    domax = (strpos(uvalue,'max') GE 0) OR (strpos(uvalue,'both') GE 0)
    IF domin THEN domin = look_span_update(all.tvrange(0)+mag,0)
    IF domax THEN domax = look_span_update(all.tvrange(1)+mag,1)
    IF domin OR domax THEN look_update,0
  END
  uvalue eq 'bothfix?': BEGIN
    text = ['Auto','Fix']
    widget_control,ws.bothfixid,get_value = now
    ws.bothfix = now NE text(1) ; toggle index in text array
    widget_control,ws.bothfixid,set_value = text(ws.bothfix)
  END
  uvalue eq 'spanmin': IF look_span_update(value,0) THEN look_update,0
  uvalue eq 'spanmax': IF look_span_update(value,1) THEN look_update,0
  strpos(uvalue,'dim') GE 0: BEGIN
    dim = fix(strmid(uvalue,3,1))-1 ; dim3->2, dim4->3, ..., dim7->6
;    widget_control,ws.comtext,/append,set_value = uvalue+' '+strtrim(dim,2)
    current = ws.d(dim)         ; save current value
    CASE strmid(uvalue,4,1) OF
      'I': BEGIN
        sign = 2*(strmid(uvalue,5,1) EQ '+')-1 ; 1-->1 & 0-->-1
        widget_control,ws.dimid(1,dim-2),get_value = curdim
        widget_control,ws.dimid(3,dim-2),get_value = curincr
        ws.d(dim) = (long(curdim)+sign*long(curincr))
      END
      'p': BEGIN                ; pull-down menu chosen
        colon = strpos(value,':') ; assume 'previous' selected if no colon
        IF colon EQ -1 THEN ws.d(dim) = ws.previous(dim) ELSE ws.d(dim) = long(strmid(value,0,colon))
;        widget_control,ws.comtext,/append,set_value = string(value)+' '+strmid(value,0,colon)
      END
      ELSE: BEGIN
        widget_control,ws.dimid(1,dim-2),get_value = curdim
        ws.d(dim) = long(curdim)
      END
    ENDCASE
    ws.previous(dim) = current  ; save the old value as previous
    ws.d(dim) = ws.d(dim) > 0 < (ws.size(dim+1)-1) ; restrict to allowed range
    widget_control,ws.dimid(1,dim-2),set_value = strtrim(ws.d(dim),2) ; set new value
  END
  uvalue eq 'donebutton': BEGIN
    widget_control,ev.top,/destroy
    return
  END
  uvalue eq 'helpbutton': BEGIN
    path = expand_path(!PATH,/array,count=count) ;GGA
    file = ''
    i = -1                      ;GGA
    REPEAT  BEGIN
      i = i+1
;   Fix made here (DSR)
      file = findfile(filepath(root_dir = path(i),'look.pro'))
    END UNTIL file(0) ne '' or i eq count-1 ;GGA
    if file(0) eq '' then BEGIN
      widget_control,ws.comtext,set_value = 'No look.pro in !path',/append
    ENDIF ELSE BEGIN
      line = ''                 ; read buffer
      openr,lun,/get_lun,file(0)
      l = 0                     ; line counter
      WHILE NOT eof(lun) DO BEGIN
        readf,lun,line
        IF strpos(line,';-') EQ 0 THEN GOTO,NEXT
        l = l+1                 ; increment counter
      ENDWHILE
      NEXT:
      device,get_scr = xyscr
      buf = strarr(l)
      point_lun,lun,0
      FOR i = 0,l-1 DO BEGIN
        readf,lun,line
        buf(i) = line
        ENDFOR
      close,lun
      free_lun,lun
;   Fix made here (DSR)
;;      look_xdisplay,ws.help.message,buf,ysize = 14.*(xyscr(1)/640.) $
      look_xdisplay,ws.help.v+': '+ws.help.message,buf,ysize = 14.*(xyscr(1)/640.) $
        ,titles = ['One-line messages about widgets','Output from doc_library']
    ENDELSE
  END
  uvalue eq 'starslide': ws.increment = value
  uvalue eq 'kslider': ws.kernel_width = value
  uvalue eq 'optpdmenu': look_optmenu,value,ev ; value is name of the button
  uvalue eq 'plotfullorzoom?': BEGIN
    ws.plotfullzoom = (ws.plotfullzoom + 1) MOD 3
    widget_control,ws.fullzoombutton,set_value = ws.fullzoom(ws.plotfullzoom)
  END
  uvalue eq 'starpdmenu': BEGIN
    ws.star = ' '                               ; reset all to blanks
    ws.star(ws.starlookup(value-1)) = '*'       ; set selection to star
    widget_control,ws.reptext,set_value = ws.star+ws.report
  END 
  ELSE: 
ENDCASE
;
;       =====>> Respond to toggle of Auto Scale/Fix Slider button
;
IF (strpos(uvalue,'auto') GE 0) OR (strpos(uvalue,'set') GE 0) THEN BEGIN
  tittype = ['Min','Max']
  j = strpos(uvalue,'max') GE 0           ; 0(1) for 'minauto'('maxauto')
  IF (strpos(uvalue,'set') GE 0) THEN ws.spanmode(j) = 0 ELSE $
    ws.spanmode(j) = (ws.spanmode(j)+1) MOD 2
  CASE ws.spanmode(j) OF
    0: BEGIN
      widget_control,ws.autoscale(j),set_value = 'FixSlide'
      widget_control,ws.setids(j),get_value = mag
      ws.manrange(j) = float(mag(0))
      all.tvrange(j) = ws.manrange(j)
      zoom.tvrange(j) = ws.manrange(j)
      widget_control,ws.spansliders(j),set_value = all.tvrange(j)
    END
    1: BEGIN
      widget_control,ws.autoscale(j),set_value = 'Auto '+tittype(j)
;   Use min or max from currently selected autoscale option
      look_auto_minmax, newrange = newlimits, /no_update
      all.tvrange(j) = newlimits(j)
;;      all.tvrange(j) = all.imrange(j)
      zoom.tvrange(j) = newlimits(j)
;;      zoom.tvrange(j) = zoom.imrange(j)
      widget_control,ws.spansliders(j),set_value = all.tvrange(j)
;;      widget_control,ws.setids(j),set_value = strtrim(all.tvrange(j),2)
      look_update_scale_label, j, all.tvrange(j)
    END
  ENDCASE
  look_update,0
ENDIF
;
;       =====>> Get new image if requested.
;
IF (uvalue EQ 'restorebutton') OR (strpos(uvalue,'dim') EQ 0) THEN BEGIN
;;  thisimage = look_get_image(ws.d(2:6),ws)
  thisimage = look_get_image(ws.d(2:6),ws)
  fullimage = thisimage
  all.tvrange = [min(thisimage),max(thisimage)] > ws.srange(0) < ws.srange(1)
  all.imrange = all.tvrange
  look_auto_minmax, newrange = newlimits, /no_update
  FOR j = 0,1 DO BEGIN
    CASE ws.spanmode(j) OF
      0: all.tvrange(j) = ws.manrange(j) ; FixSlide
      1: all.tvrange(j) = newlimits(j)   ; AutoScale (DSR)
      ELSE:
    ENDCASE
    widget_control,ws.spansliders(j),set_value = all.tvrange(j)
;;    widget_control,ws.setids(j),set_value = strtrim(all.tvrange(j),2)
    look_update_scale_label, j, all.tvrange(j)
  ENDFOR
  zoom.tvrange = all.tvrange
  zoom.imrange = zoom.tvrange
  IF n_elements(thisimage) GT 1 THEN begin
     res = moment (thisimage, sdev = sdev)
     all.sd = sdev
     mean = res(0)
  endif ELSE BEGIN
     all.sd = 0.
     mean = 0
  ENDELSE
  all.mean = mean
  ws.report(12) = ' mean='+strtrim(all.mean,2)
  ws.report(13) = ' sd='+strtrim(all.sd,2)
  look_update,0
;   Clear the horizontal and vertical cut windows (they are from the
;   previous frame)
  wset,all.vcutid              ; Activate the vertical cut window
  erase                        ; Clear the vertical cut window
  wset,all.hcutid              ; Activate the horizontal cut window
  erase                        ; Clear the horizontal cut window
  wset,all.id                  ; Reset the active window
  all.datlast(0) = -1          ; Invalidate the saved X coordinate
  all.datlast(1) = -1          ; Invalidate the saved Y coordinate
  zoom.datlast(0) = -1         ; Invalidate the saved X coordinate
  zoom.datlast(1) = -1         ; Invalidate the saved Y coordinate
ENDIF
;
;       =====>> Process the tv mouse events.
;       =====>> (8 tags from draw widget events) ;GGA
;       9 tags in version 5 (DSR)
;
IF (uvalue EQ 'alldraw') AND (n_tags(ev) GE 8) THEN BEGIN
  CASE 1 OF                     ; left mouse click/drag
    (ev.press EQ 1): BEGIN
      ws.drag(0) = 1            ; button is now down
      ws.dragstart = [ev.x,ev.y] ; device coordinates at start of drag
      ws.dragcorner = ws.dragstart ; device coordinates during drag
      t = look_cc(ws.dragstart,all.slopes,all.intercepts)
;   Addition to disallow zoom selection on special plots
      if (ws.plottype eq 0 or ws.plotfullzoom eq 0) and $
          look_in_range(t,all.xrange,all.yrange) THEN BEGIN
        all.down(*,0) = t
        wset,all.id
        device,set_graphics = 6
        yl = all.devlast(1)     ; First erase the crosshairs
        IF yl GE 0 THEN plots,[0,!d.x_size],[yl,yl],/dev
        xl = all.devlast(0)
        IF xl GE 0 THEN plots,[xl,xl],[0,!d.y_size],/dev
        device,set_graphics = 3
        all.devlast = [-1,-1]
      ENDIF
    END      
    (ev.release EQ 1): BEGIN
      ws.drag(0) = 0            ; button is now up
      ws.dragcorner = [-1,-1]   ; reset drag corner
      t = look_cc([ev.x,ev.y],all.slopes,all.intercepts)
;   Addition to disallow zoom selection on special plots
      if (ws.plottype eq 0 or ws.plotfullzoom eq 0) and $
          look_in_range(t,all.xrange,all.yrange) THEN BEGIN
        all.up(*,0) = t
        all.devlast = -1        ; to avoid generating crosshairs in next look_cut
        zoom.devlast = -1       ; to avoid generating crosshairs in next look_cut
        zoom.xrange = [(all.down(0,0)<all.up(0,0)) < (all.xrange(1)-2) $
                     ,all.down(0,0)>all.up(0,0)] > all.xrange(0)
        zoom.xrange(1) = zoom.xrange(1) > (zoom.xrange(0)+2) < all.xrange(1)
        zoom.yrange = [(all.down(1,0)<all.up(1,0)) < (all.yrange(1)-2) $
                     ,all.down(1,0)>all.up(1,0)] > all.yrange(0)
        zoom.yrange(1) = zoom.yrange(1) > (zoom.yrange(0)+2) < all.yrange(1)
;   Reset the plot type if necessary, so the zoom window will show the
;   newly selected zoom region
        if ws.plottype ne 0 then ws.plottype = 0
        look_update,1           ; xrange/yrange --> center/width
      ENDIF
    END
    (ev.press EQ 0) AND (ev.release EQ 0): BEGIN ; motion event
;   Addition to disallow zoom selection on special plots
      if (ws.plottype eq 0 or ws.plotfullzoom eq 0) and $
          ws.drag(0) THEN BEGIN
        IF ws.debug THEN widget_control,ws.comtext,/append $
          ,set_value = string(uvalue+' motion') $
          +string([ws.drag(0),ws.dragstart,ws.dragcorner],format = '(8i5)')
        q = ws.dragstart        ; start of drag
        r = ws.dragcorner       ; last drag corner
        wset,all.id
        device,set_graphics = 6
        IF total(r) GE 0 THEN $ ; to erase last drag box, if it's there.
          plots,[q(0),r(0),r(0),q(0),q(0)],[q(1),q(1),r(1),r(1),q(1)],/dev
                                ; Then add new drag box
        ws.dragcorner = [ev.x,ev.y]
        r = ws.dragcorner
        plots,[q(0),r(0),r(0),q(0),q(0)],[q(1),q(1),r(1),r(1),q(1)],/dev
        device,set_graphics = 3
      ENDIF ELSE IF ws.fullshown THEN look_cut,all,ev.x,ev.y,docrosshairs = 1-ws.drag(0)
;
;	=====>> If present and toggled on, call each viewer.
;
      FOR i = 0,n_elements(ws.viewers)-1 DO IF (ws.viewers(i) NE '') AND (ws.viewers_set(i) EQ 2) THEN call_procedure,ws.viewers(i)
    END
    (ev.press EQ 2):            ; middle mouse click
    (ev.release EQ 2): BEGIN
      ws.drag(1) = 0           ; button is now up
      t = look_cc([ev.x,ev.y],all.slopes,all.intercepts)
      IF look_in_range(t,all.xrange,all.yrange) THEN BEGIN
        zoom.center = t
        all.devlast = -1        ; to avoid generating crosshairs in next look_cut
        zoom.width = zoom.width > 2 < ((t < (all.max-t))*2)
        look_update,2           ; center/width --> xrange/yrange
      ENDIF
    END
    (ev.press EQ 4):            ; right mouse click
    (ev.release EQ 4): BEGIN
      ws.drag(2) = 0            ; button is now up
      all.devlast = -1          ; to avoid generating crosshairs in next look_cut
      del = ws.increment
      CASE (where(ws.star EQ '*'))(0) OF
        5: zoom.center(0) = (zoom.center(0)+del) > all.xrange(0) < all.xrange(1)
        6: zoom.center(1) = (zoom.center(1)+del) > all.yrange(0) < all.yrange(1)
        9: zoom.width(0) = ((zoom.width(0)+del) > 1) > all.xrange(0) < all.xrange(1)
        10: zoom.width(1) = ((zoom.width(1)+del) > 1) > all.yrange(0) < all.yrange(1)
        ELSE:
      ENDCASE
      look_update,2
    END
    ELSE:
  ENDCASE
ENDIF
;
;       =====>> Motion event in zoom window
;
IF (uvalue EQ 'zoomdraw') AND (n_tags(ev) GE 8) AND ws.zoomshown THEN $
  IF (ev.press EQ 0) AND (ev.release EQ 0) THEN look_cut,zoom,ev.x,ev.y,/docrosshairs
END                             ; of look_event
;
;       ============================
;       =====>> INITIALIZING ROUTINE
;       ============================
;
PRO look,input,help=help,verbose = verbose,noload = noload $
         ,range = inputrange,associate = associate,tag = tag $
         ,dimensions = dimens,axes = axes $
         ,filename = filename,definition = definition,offset = offset $
         ,loader_name = loader_name,lines = comlines $
         ,options = options,procedures = procedures,functions = functions $
         ,types = types,kernel_width = kernel_width,on_error = on_error $
         ,debug = debug,_extra = extra, userscalefunc = userscalefunc $
         ,usermean = usermean, usermedian = usermedian $
         ,units = units $
         ,dim3marks = dim3marks,dim4marks = dim4marks,dim5marks = dim5marks $
         ,dim6marks = dim6marks,dim7marks = dim7marks $
         ,viewers = viewers
COMMON look,images,catalog,index
COMMON look_common,thisimage,zoomimage,fullimage,ws,all,zoom
COMMON colors, r_orig, g_orig, b_orig, r_curr, g_curr, b_curr
;
;       =====>> HELP
;
IF keyword_set(help) THEN BEGIN
  doc_library,'look'
  return
ENDIF
;
;       =====>> Set Defaults: internal parameters
;
IF n_elements(on_error) EQ 0 THEN on_error,2 ELSE on_error,on_error
IF !d.name NE 'X' THEN BEGIN
  message,/inform,"Incorrect device:'+!d.name+'.  Try set_plot,'X'"
  return
ENDIF
versdate = '14 Mar 00'
maxsurfdim = 50
IF n_elements(r_orig) EQ 0 THEN loadct,0 ; in case no previous call to loadct
IF n_elements(associate) EQ 0 THEN associate = 0
IF n_elements(comlines) EQ 0 THEN comlines = 2
IF n_elements(debug) EQ 0 THEN debug = 0
IF n_elements(kernel_width) EQ 0 THEN kernel_width = 3
have_func = n_elements(loader_name) EQ 1
IF NOT have_func THEN loader_name = ''
IF n_elements(offset) EQ 0 THEN offset = 0
CASE n_elements(units) OF
  0: units = ['x (pixels)','y (pixels)','z (value)']
  1: units = [strtrim(units(0),2),'y (pixels)','z (value)']
  2: units = [strtrim(units(0),2),strtrim(units(1),2),'z (value)']
  ELSE:units = strtrim(units(0:2),2)
ENDCASE
IF n_elements(verbose) EQ 0 THEN verbose = 0
device,get_screen_size = scrdims                ; ~plot+borders
;
;       =====>> If needed, load test images.
;
needload = (n_elements(input) EQ 0) AND (NOT associate) AND (NOT have_func)
IF keyword_set(noload) THEN BEGIN               ; force load?
  IF n_elements(ws) EQ 1 THEN BEGIN
    IF ws.assoc THEN BEGIN
      associate = ws.assoc
      definition = ws.definition
      filename = ws.filename
      tag = ws.tag
      needload = 0
    ENDIF    
  ENDIF    
  IF (n_elements(images) EQ 0) THEN BEGIN
    message,/inform,'No data in common look with /noload keyword set.'
    needload = 1
  ENDIF ELSE needload = 0                       ; use images---already stored
ENDIF
IF NOT needload AND (n_elements(input) GT 0) THEN images = input 
IF needload THEN BEGIN
  message,/inform,'Loading test data...'
  d = min(scrdims)/3
  dd = dist(d)
  dr = randomn(seed,d,d)
  dg = findgen(d)*4*!pi/d
  IF !version.release GE '4.0' THEN BEGIN
    ndemo = 3
    nim = 7+ndemo
    images = fltarr(d,d,nim)
    openr,lun,/get,FILEPATH('galaxy.dat', SUBDIRECTORY = ["examples","data"])
    tmp = bytarr(256,256)
    readu,lun,tmp
    images(*,*,0) = congrid(tmp,d,d)
    close,lun
    openr,lun,/get,FILEPATH('cereb.dat', SUBDIRECTORY = ["examples","data"])
    tmp = bytarr(512,512,2)
    readu,lun,tmp
    images(*,*,1) = congrid(tmp(*,*,0),d,d)
    images(*,*,2) = roberts(congrid(tmp(*,*,1),d,d)) < 20
    close,lun
    free_lun,lun
  ENDIF ELSE BEGIN
    images = fltarr(d,d,7)
    ndemo = 0
  ENDELSE
  images(0,0,0+ndemo) = dd
  images(0,0,1+ndemo) = .5*max(dd)+dr
  images(0,0,2+ndemo) = dd+dr*sqrt(d)
  images(0,0,3+ndemo) = (sin(dg)#cos(dg))*128+128
  mul = 12
  a = beselj(findgen(d)/max(d)*5,0)*mul
  off = 80
  images(0,0,4+ndemo) = a#a+off
  images(0,0,5+ndemo) = images(*,*,4+ndemo)+dr*mul
  images(0,0,6+ndemo) = median(images(*,*,5+ndemo),5)
  dim3marks = ['barred spiral galaxy','brain','brain with Roberts filter','dist','noise','dist+noise','sin#cos','Besel','Besel+noise','smoothed Besel+noise']
  axes = 'image menu'
  units = ['x (pixels)','y (pixels)','counts (dn)']
ENDIF
;
;       =====>> Now set the parameters based on the images.  Size first.
;
CASE 1 OF
  associate: BEGIN
    IF n_elements(filename) EQ 0 THEN filename = pickfile()
    openr,lun,filename,/get_lun,err = err
    IF err NE 0 THEN BEGIN
      message,/inform,'Unable to open '+filename+'.  Aborting.'
      return
    END
    IF n_elements(definition) EQ 0 THEN REPEAT BEGIN
      definition = inquire('Enter definition of the associate variable, e.g., lonarr(100,100) or {struct,header:0,data:intarr(128,128)}',' ')
    ENDREP UNTIL execute('tmp='+definition)
    stat = execute('tmp='+definition)
    szt = size(tmp)
    is_struct = szt(szt(0)+1) EQ 8
    exestring = 'images = assoc(lun,'+definition+','+strtrim(offset,2)+')'
    IF NOT execute(exestring) THEN BEGIN
      message,/inform,'Unable to define associate variable using '+definition+' on file '+filename
      return
    ENDIF
    IF is_struct THEN BEGIN
      IF n_elements(tag) EQ 0 THEN BEGIN
        tags = tag_names(tmp)
        nt = n_elements(tags)-1
        i = 0
        REPEAT BEGIN
          si = size(tmp.(i))
          i = i+1
        ENDREP UNTIL (i EQ nt) OR (si(0) EQ 2)
        tag = inquire('Enter tag # of images',i-1)
      ENDIF
      IF verbose THEN BEGIN
        tags = tag_names(tmp)
        message,/inform,'Chosen tag name is '+tags(tag)
      ENDIF
    ENDIF ELSE tag = -1
    tmp = images(0)
    IF is_struct THEN si = size(tmp.(tag)) ELSE si = size(images)
    IF si(0) NE 2 THEN BEGIN
      message,/inform,'Associate variable is not 2-D!  Aborting.'
      return
    ENDIF
    filestats = fstat(lun)
    nb = nbytes(tmp)
    IF n_elements(dimens) EQ 0 THEN d3 = filestats.size/nb ELSE d3 = dimens
    si = [3,si(1),si(2),d3,si(3),filestats.size]
  END
  have_func: BEGIN
    images = call_function(loader_name,lonarr(5))
    si = size(images)
    IF si(0) NE 2 THEN BEGIN
      message,/inform,loader_name+' return is not 2-D!  Aborting.'
      return
    ENDIF
    x = strtrim(si(1),2) & y = strtrim(si(2),2)
    IF n_elements(dimens) EQ 0 THEN dimens = inquire('Enter the dimensions of the array of images, e.g., enter 3 4 if '+loader_name+' returns '+x+' x '+y+'images from a '+x+' x '+y+' x 3 x 4 set',1)
    n_el = si(4)
    FOR i = 0,n_elements(dimens)-1 DO n_el = n_el*dimens(i)
    filename = ''
    definition = ''
    is_struct = 0
    tag = -1
    si = [2+n_elements(dimens),si(1),si(2),dimens,si(3),n_el]
  END
  ELSE: BEGIN
    filename = ''
    definition = ''
    is_struct = 0
    tag = -1
    si = size(images)
  END
ENDCASE
IF si(si(0)+1) EQ 6 THEN BEGIN
  message,/inform,'To use look with complex arrays, you need to make a function.  Type look,/help for details.'
  return
ENDIF
IF si(si(0)+1) EQ 7 THEN BEGIN
  message,/inform,'Look doesn''t support strings.  Type look,/help for details.'
  return
ENDIF
IF verbose THEN print,'% LOOK: size=',si,form = '(a,10i8)'
;
;       =====>> Set defaults based on images
;
nimages = 1                     ; total number of images
FOR i = 3,si(0) DO nimages = nimages*si(i)
x = si(1)                       ; 4 conveniences
y = si(2)
; xgen and ygen are no longer needed, due to change to look_si2 (DSR)
;;xgen = lindgen(x)
;;ygen = lindgen(y)
xrange = [0,x-1]
yrange = [0,y-1]
dims = ([1.06,1.08]*[x,y]) < (scrdims/3) > (scrdims/4)
;
;       =====>> Compose options menu from hardwired items plus user inputs
;
tmp = {look_pdmenu,flags:0,name:''}
optmenu = [ $
            {look_pdmenu,1,'Options menu'} $
            ,{look_pdmenu,1,'blink compare'} $
            ,{look_pdmenu,0,'blink to screen'} $
            ,{look_pdmenu,2,'blink to window'} $
            ,{look_pdmenu,1,'catalog'} $
            ,{look_pdmenu,0,'cat to screen'} $
            ,{look_pdmenu,2,'cat to window'} $
            ,{look_pdmenu,1,'enlargement'} $
            ,{look_pdmenu,0,'full screen'} $
            ,{look_pdmenu,2,'full window'} $
            ,{look_pdmenu,0,'movie'} $
            ,{look_pdmenu,1,'plot types'} $
            ,{look_pdmenu,0,'contour'} $
            ,{look_pdmenu,0,'histogram'} $
            ,{look_pdmenu,0,'look_image'} $
            ,{look_pdmenu,0,'surface'} $
            ,{look_pdmenu,0,'shade_surf'} $
            ,{look_pdmenu,0,'show3'} $
            ,{look_pdmenu,2,'tvscl'} $
            ,{look_pdmenu,0,'region of interest'} $
            ,{look_pdmenu,1,'spatial filters'} $
            ,{look_pdmenu,0,'leefilt'} $
            ,{look_pdmenu,0,'median'} $
            ,{look_pdmenu,0,'smooth'} $
            ,{look_pdmenu,2,'Laplacian'} $
;            ,{look_pdmenu,0,'type image'} $
            ,{look_pdmenu,1,'Autoscale options'} $
            ,{look_pdmenu,0,'All data'} $
            ,{look_pdmenu,0,'Omit top / bottom 5%'} $
            ,{look_pdmenu,0,'Omit top / bottom 10%'} $
            ,{look_pdmenu,0,'Mean +/- 1 sigma'} $
            ,{look_pdmenu,0,'Mean +/- 2 sigma'} $
            ,{look_pdmenu,0,'Mean +/- 3 sigma'} $
            ,{look_pdmenu,0,'Median +/- 1 sigma'} $
            ,{look_pdmenu,0,'Median +/- 2 sigma'} $
            ,{look_pdmenu,0,'Median +/- 3 sigma'} ]

;   Build custom scaling function(s) into the menu

if n_elements (userscalefunc) eq 0 then optmenu = [ optmenu $
            ,{look_pdmenu,2,'hist_equal'} ] $
else begin
   optmenu = [ optmenu, {look_pdmenu,0,'hist_equal'} ]
   ncust = n_elements (userscalefunc)
   for i = 1, ncust do begin
      if i eq ncust then entry_type = 2 else entry_type = 0
      optmenu = [ optmenu, {look_pdmenu,entry_type,userscalefunc(i-1)}]
   endfor
endelse

optmenu = [ optmenu $
            ,{look_pdmenu,1,'Write PS file'} $
            ,{look_pdmenu,0,'Set PS attributes'} $
            ,{look_pdmenu,0,'Full Window Only'} $
            ,{look_pdmenu,0,'Zoom Window Only'} $
            ,{look_pdmenu,2,'All Four Windows'} $
            ,{look_pdmenu,0,'xloadct'} $
          ]
nopt = n_elements(optmenu)
n = n_elements(options)
np = n_elements(procedures)
nf = n_elements(functions)
ispro = bytarr(nopt+n+np+nf)
nt = nopt
IF np GT 0 THEN FOR i = 0,np-1 DO BEGIN         ; add procedures
  optmenu = [optmenu,{look_pdmenu,0,procedures(i)}]
  ispro(nt+i) = 1
  ENDFOR
nt = nopt+np
IF nf GT 0 THEN FOR i = 0,nf-1 DO BEGIN         ; add functions
  optmenu = [optmenu,{look_pdmenu,0,functions(i)}]
  ispro(nt+i) = 0
  ENDFOR
nt = nopt+np+nf
IF n GT 0 THEN BEGIN                            ; add options---more work!
  FOR i = 0,n-1 DO BEGIN
    optmenu = [optmenu,{look_pdmenu,0,options(i)}]
    IF n_elements(types) EQ n THEN BEGIN
      ispro(nt+i) = strpos(strupcase(types(i)),'P') GE 0
    ENDIF ELSE BEGIN
      print,'% LOOK: Checking '+options(i)+'...',form = '(a,$)'
      file = look_in_path(options(i)+'.pro')
      IF file(0) NE '' THEN BEGIN ; There's a file, so search it...
        openr,lun,/get_lun,file(0)
        line = ''
        want = byte('PRO'+strupcase(options(i))+',') ;    ... for "PRO<NAME>,"
        REPEAT BEGIN
          readf,lun,line
          bline = byte(strupcase(line)) ; Change to bytes, omit blanks.
          isapro = (where(bline(where(bline ne 32)),want))(0) NE -1
        ENDREP UNTIL eof(lun) OR isapro
        close,lun
        free_lun,lun
      ENDIF ELSE BEGIN          ; No file, so it's an IDL built-in
        tmp = intarr(2,2)
        isapro = execute(options(i)+',tmp') ; Try to execute it as a procedure.
      ENDELSE
      ispro(nt+i) = isapro
      IF isapro THEN print,'PRO' ELSE print,'FUNCTION'
    ENDELSE
  ENDFOR
ENDIF
IF n_elements(viewers) GT 0 THEN BEGIN ; add viewer on/off toggle switches
  optmenu = [optmenu,{look_pdmenu,1,'viewers'}]
  FOR i = 0,n_elements(viewers)-1 DO optmenu = [optmenu $
    ,{look_pdmenu,0,viewers(i)+'-->on/all mouse'} $
    ,{look_pdmenu,0,viewers(i)+'-->on/click only'} $
    ,{look_pdmenu,2,viewers(i)+'-->off'}]
ENDIF ELSE viewers = ''
optmenu(n_elements(optmenu)-1).flags = 2
;
;       =====>> Set the display parameters (most of these could be set earlier)
;
id = bytarr(2,2)                        ; plotting window indices
xlast = -1                              ; previous cursor coordinates
ylast = -1
report = [ $                            ; text items in the reptext widget
           'mouse pixel',' x=',' y=',' z=' $
          ,'zoom center',' x=',' y=',' z=' $
          ,'zoom width',' x=',' y=' $
          ,'full stats',' mean=',' sd=' $
          ,'zoom stats',' mean=',' sd=', ' ']  ; Extra blank is kludge
star = replicate(' ',n_elements(report))
star(5) = '*'                           ; quantity controlled by rt button
starred = [4,5,6,8,9,10]                ; those for star cw_pdmenu
starlookup = [5,5,6,9,9,10]             ; map from starred to x's & y's
down = [-1,-1]
center = down
width = down
increment = 1                   ; increment from click of rt mouse button
;
;       =====>> Get the first image
;
CASE 1 OF
  si(0) LT 2: message,'2-D or larger arrays only!  Type look,/help.'
  si(0) EQ 2: thisimage = images
  si(0) GT 2: BEGIN
    CASE 1 OF
      associate: BEGIN
        IF is_struct THEN BEGIN
          tmp = images(0)
          thisimage = tmp.(tag)
        ENDIF ELSE BEGIN
          thisimage = images(0)
        ENDELSE
      END
      have_func: thisimage = call_function(loader_name,lonarr(5))
      ELSE: thisimage = images(*,*,0)
    ENDCASE
  END
ENDCASE
fullimage = thisimage
zoomimage = thisimage           ; Use full image to start
;
;       =====>> Set the display ranges
;       inputrange = user-specified parameter to override automatic setting
;       imrange = the extremes of the first (current) image
;       allrange = the extremes of all the images
;       range = the difference in allrange values
;       srange = the slider range
;       tvrange = the extremes of the full image
;
IF n_elements(inputrange) NE 2 THEN BEGIN
  mn = min(thisimage,max = mx)
  IF mn EQ mx THEN mx = mn+1    ; take care of constant image
  imrange = [mn,mx]             ; range for first image
  IF NOT associate AND NOT have_func THEN BEGIN
    mn = min(images,max = mx)
    IF mn EQ mx THEN mx = mn+1  ; take care of constant image
  ENDIF
  allrange = [mn,mx]            ; range for all images
ENDIF ELSE BEGIN
  imrange = inputrange
  allrange = inputrange
ENDELSE
IF si(si(0)+1) LT 3 THEN imrange = long(imrange)
range = abs(allrange(1)-allrange(0))    ; make sure it's positive
srange = allrange+[-1,1]*range          ; a total of 3x range of images
IF (srange(1) LT imrange(1)) OR (srange(0) GT imrange(0)) THEN BEGIN
  message,/inform,'Overflow in slider range.  Limiting to image range.'
;  IF si(si(0)+1) EQ 2 THEN srange = [0,32767]  ELSE $ ; positive integers
    srange = allrange
ENDIF
IF (imrange(1) GT srange(1)) OR (imrange(0) LT srange(0)) THEN BEGIN
  message,/inform,'Image range outside slider range.  Limiting to slider range.'
  imrange = srange
ENDIF
IF (srange(0) GT -1) AND (srange(1) LT 1) THEN srange = [-1,1]
tvrange = float(imrange)
;
;       =====>> CREATE AND REGISTER WIDGETS: top, columns, parts of columns
;
IF xregistered("look") GT 0 THEN message,'Another look exists! Type retall & xmanager, then look,...'
looktop = widget_base(title='look: image-display widget ('+versdate+')',/column)
lookbase = widget_base(looktop,/row)
col1base = widget_base(lookbase,/column)
donebutton = widget_button(col1base,value='DONE',uvalue = 'donebutton')
helpbutton = widget_button(col1base,value='HELP',uvalue = 'helpbutton')
fullzoom = ['Use Full Only','Use Zoom Only','Use Full & Zoom']
fullzoombutton = widget_button(col1base,value = fullzoom(0),uval = 'plotfullorzoom?')
restorebutton = widget_button(col1base,value='Restore Image',uvalue = 'restorebutton')
tmp = cw_pdmenu(col1base,optmenu,/return_name,uvalue = 'optpdmenu')
kslider = widget_slider(col1base,title = 'filter width',uvalue = 'kslider',min = 1 $
              ,max = (si(1)-1) < (si(2)-1),value = kernel_width,scroll = 1)
dimid = -1
dimname = '2-D'
IF si(0) GT 2 THEN BEGIN      ; "image" has more than 2 dimensions       ;GGA
  dimid = lonarr(4,si(0)-2)
  dimname = strarr(si(0)-2)
  FOR i = 3,si(0) DO BEGIN
    IF n_elements(axes) GE (i-2) THEN stit = axes(i-3) ELSE stit = 'dim '+strtrim(i,2)
    dimname(i-3) = stit
    dimi = 'dim'+strtrim(i,2)
    digits = ceil(alog10(si(i)))
    menu = [{look_pdmenu,1,stit},{look_pdmenu,0,'previous'}] ; start of menu
;   If keyword exists, then convert to variable marks and add to menu.
    current = 'dim'+strtrim(i,2)+'marks'
    stat =  execute('nm = n_elements('+current+')')
    IF nm GT 0 THEN BEGIN
      stat = execute('marks = '+current) ; save in a global variable
      sz = size(marks)
      IF sz(sz(0)+1) NE 8 THEN BEGIN ; not a structure, so convert to structure
        tmp = replicate({mark:0l,tag:''},nm)
        FOR n = 0,nm-1 DO BEGIN
          tmp(n).mark = n
          tmp(n).tag = string(marks(n))
        ENDFOR
        marks = tmp             ; save
      ENDIF
      FOR n = 0,nm-1 DO menu = $ ; add each element of struct array to menu
        [menu,{look_pdmenu,0,strtrim(marks(n).(0),2)+': '+marks(n).(1)}]
    ENDIF
    menu = menu(0:(nm+1) < (si(i)+1)) ; limit the number of elements
    menu(n_elements(menu)-1).flags = 2 ; flag the end of the menu
    dimid(0,i-3) = look_mvpi(col1base,uvalue = dimi,title = stit $
      ,value = '0',size = digits,scroll = '1',menu = menu)
  ENDFOR
ENDIF
col2base = widget_base(lookbase,/column)
col2draw1 = widget_draw(col2base,xsize=dims(0),ysize=dims(1))
col2draw2 = widget_draw(col2base,xsize=dims(0),ysize=dims(1),uvalue='alldraw',/button,/motion)
;   Various changes here to make the scroll increment floating point
scroll_value = (srange(1)-srange(0))/((!d.n_colors<!d.table_size) > 1)
minslide = llcw_fslider(col2base,min = srange(0) $
  ,uvalue = 'spanmin',max = srange(1),value = imrange(0),/drag $
  ,scroll = scroll_value,/suppress, xsize=dims(0))
tmp = widget_base(col2base,/row)
autominbutton = widget_button(tmp,value = 'Auto Min',uval = 'minauto?')
minset = widget_text(tmp,xsize = 9,val = strtrim(imrange(0),2),/edit,uval = 'minset')
junk = widget_button(tmp,value = '-I',uvalue = 'applymin-')
junk = widget_button(tmp,value = '+I',uvalue = 'applymin+')
col3base = widget_base(lookbase,/column)
col3draw1 = widget_draw(col3base,xsize=dims(0),ysize=dims(1),uvalue='zoomdraw',/button,/motion)
col3draw2 = widget_draw(col3base,xsize=dims(0),ysize=dims(1))
maxslide = llcw_fslider(col3base,min = srange(0) $
  ,uvalue = 'spanmax',max = srange(1),value = imrange(1),/drag $
  ,scroll = scroll_value,/suppress, xsize=dims(0))
tmp = widget_base(col3base,/row)
automaxbutton = widget_button(tmp,value = 'Auto Max',uval = 'maxauto?')
maxset = widget_text(tmp,xsize = 9,val = strtrim(imrange(1),2),/edit,uval = 'maxset')
junk = widget_button(tmp,value = '-I',uvalue = 'applymax-')
junk = widget_button(tmp,value = '+I',uvalue = 'applymax+')
col4base = widget_base(lookbase,/column)
reptext = widget_text(col4base,xsize = 11,ysize = 18,value = star+report)
starslide = widget_slider(col4base,title = 'Increment for *',min = -(x < y)/2$
    ,max = (x < y)/2,value = increment,uvalue = 'starslide',scroll = 1)
np = n_elements(starred)
ppd_s = replicate({cw_pdmenu_s,flags:0,name:''},np+1)
ppd_s(0) = {cw_pdmenu_s,1,'Select *'}
tmp = report(starred)
FOR i = 1,np-1 DO ppd_s(i) = {cw_pdmenu_s,0,tmp(i-1)}
ppd_s(np) = {cw_pdmenu_s,2,tmp(np-1)}
starpdmenu = cw_pdmenu(col4base,ppd_s,/return_index,uvalue = 'starpdmenu')
tmp = widget_base(col4base,/row)
bothfixid = widget_button(tmp,value = 'Auto',uval = 'bothfix?')
junk = widget_label(tmp,value = 'I=')
spanmagid = widget_text(tmp,ysize = 1,xsize = 5,value = strtrim(string(scroll_value,format = '(g12.3)'),2),/edit,uvalue = 'spanmag')
;pm = ['+','-']
;signid = cw_bgroup(tmp,pm,button_uvalue = 'span'+pm,/no_release,/row,/frame $
;  ,/exclusive,uvalue = 'spansign',set_value = 0)
tmp = widget_base(col4base,/row)
junk = widget_label(tmp,value = 'Both')
junk = widget_button(tmp,value = '-I',uvalue = 'applyboth-')
junk = widget_button(tmp,value = '+I',uvalue = 'applyboth+')
;
;       =====>> Fill in the help structure with one-line messages
;
help = {look_aid,v:'value',uv:'uvalue',message:''}
help = {look_aid,'FULL IMAGE','alldraw',"Roam in tv image.  Click and drag left mouse to zoom.  Click middle to recenter."}
help = [help,{look_aid,'ZOOM IMAGE','zoomdraw',"Roam in zoom image to see cuts.  Default display area also."}]
help = [help,{look_aid,'HORIZONTAL CUT','',"Show horizontal cut when roaming over the full or zoom image."}]
help = [help,{look_aid,'VERTICAL CUT','',"Show vertical cut when roaming over the full or zoom image."}]
help = [help,{look_aid,'DONE','donebutton','Press this button to exit.'}]
;help = [help,{look_aid,'HELP','helpbutton',"Press this button to call xdisplayfile,'look.pro'."}]
help = [help,{look_aid,'Use Full Only','',"Select Full Only, Zoom Only, or Full & Zoom."}]
help = [help,{look_aid,'Restore Image','restorebutton',"Reread the original image for current indices: restore unfiltered image."}]
help = [help,{look_aid,'Options menu','plotpdmenu',"Pull down menu of options"}]
;help = [help,{look_aid,'Enlargement','enlargement',"Fill the screen with the current image, magnified 2X, 3X, etc.---whatever fits."}]
help = [help,{look_aid,'filter width','kslider',"Kernel size of spatial filters smooth and median."}]
help = [help,{look_aid,'dim3, dim4,...','dim3',"Move each slider to select image number in a series."}]
help = [help,{look_aid,'*','',"In the Select * pop-up menu, use the left mouse button to select the *'d quantity."}]
help = [help,{look_aid,'*','',"On the full image, click the right mouse button to change the *'d quantity by mouse increment."}]
help = [help,{look_aid,'mouse pixel','mouse',"Report the mouse pixel and image value."}]
help = [help,{look_aid,'zoom center','zoom center',"Report the zoom center in image coordinates: pixels."}]
help = [help,{look_aid,'zoom width','zoom width',"Report the zoom width in image coordinates: pixels."}]
help = [help,{look_aid,'stats','stats',"Report the mean and st.dev. for full and zoom images."}]
help = [help,{look_aid,'Increment for *','starslide',"Set the increment so a right mouse click moves the *'d item."}]
help = [help,{look_aid,'Select *','starslide',"Use the left mouse button to select the *'d quantity."}]
help = [help,{look_aid,'plot min','spanmin',"Set display minimum. Slider ranges over 3x image range."}]
help = [help,{look_aid,'Auto Scale','minauto?',"Automatically set minimum to image minimum."}]
help = [help,{look_aid,'Fix Slider','minman?',"Use the current slider value as the plot minimum."}]
help = [help,{look_aid,'plot max','spanmax',"Set display maximum. Slider ranges over 3x image range."}]
help = [help,{look_aid,'AutoMax','maxauto?',"Automatically set max to image max."}]
help = [help,{look_aid,'FixSlide','maxman?',"Use the current slider value as the plot extremum."}]
help = [help,{look_aid,'Set Min=','maxman?',"Use the text value as the plot extremum."}]
help = [help,{look_aid,'I=,-I,+I','apply',"Apply the Increment, I, to the extrema, either min, max or Both."}]
help = [help,{look_aid,'Auto Scale options','autoscale',"Map the color table to a subset of the full data range"}]
;
;       =====>> Fill in the help window and put widgets on the screen.
;
comtext = widget_text(lookTop,xsize = 50,ysize = comlines,value = help.v+': '+help.message,/scroll)
widget_control,lookbase,/realize                ; PUT THEM ON THE SCREEN
;
;       =====>> Get window indices and store in common
;
widget_control,get_value = id_c2d1,col2draw1
widget_control,get_value = id_c2d2,col2draw2
widget_control,get_value = id_c3d1,col3draw1
widget_control,get_value = id_c3d2,col3draw2
ids = [[id_c2d1,id_c2d2],[id_c3d1,id_c3d2]]
wids = [[col2draw1,col2draw2],[col3draw1,col3draw2]]
;
;       =====>> Compute small images of the digits to label images
;
wmax = 0
FOR i = 0,9 DO BEGIN
  xyouts,0,0,strtrim(i,2),width = w,/device
  wmax = w*!d.x_size > wmax
ENDFOR
xd = wmax+2                     ; add a 1-pixel border
yd = xd+2                       ; assume a height
backgrd = bytarr(xd,yd)+!d.n_colors-1 ; usually white
digits = bytarr(xd,yd,11)
FOR i = 0,9 DO BEGIN
  tv,backgrd
  xyouts,1,1,strtrim(i,2),width = w,/device,color = !p.background
  digits(*,*,i) = tvrd(0,0,xd,yd)
ENDFOR
tv,backgrd
xyouts,1,1,':',width = w,/device,color = !p.background
digits(*,*,10) = tvrd(0,0,xd,yd)
erase
;
;       =====>> Fill the image widget & calculate scale (dev->data)
;
wset,id_c2d2
look_image,look_scl(thisimage,[min(thisimage),max(thisimage)]),origin = [0,0] $
  ,scale = 1,title = 'FULL IMAGE',xtit = units(0),ytit = units(1)
position = [!x.window(0),!y.window(0),!x.window(1),!y.window(1)]
;  Corrected slope / intercept calculations.  The conversion factors from
;  look_si are not quite correct; look_si2 is an improved version (DSR).
;;look_si,xgen,ygen,slopes,intercepts
look_si2,xrange,yrange,slopes,intercepts

wset,id_c3d1
look_image,look_scl(thisimage,[min(thisimage),max(thisimage)]),origin = [0,0] $
  ,scale = 1,title = 'ZOOM IMAGE',xtit = units(0),ytit = units(1)
wset,id_c2d2
;
;       =====>> Put all the information in structures to pass to event routine
;
if n_elements (userscalefunc) eq 0 then scalefunc = '' $
else scalefunc = userscalefunc
all = { $                               ; structure for entire image
       wid:wids(1,0) $                  ; draw widget id for widget reference
       ,id:ids(1,0) $                   ; draw widget id for wset,...
       ,hcutid:ids(0,0) $               ; draw id for horizontal cut
       ,vcutid:ids(1,1) $               ; draw id for vertical cut
       ,position:position $             ; lo.left & up.right of plot region
       ,min:[xrange(0),yrange(0)] $     ; minimum x&y coordinate values: (0,0)
       ,max:[xrange(1),yrange(1)] $     ; maximum x&y coordinate values
       ,mean:0. $
       ,sd:0. $
       ,xrange:xrange $                 ; [min(0),max(0)] = x coordinate range
       ,yrange:yrange $                 ; [min(1),max(1)] = y coordinate range
       ,center:[x,y]/2 $                ; center x&y coordinates
       ,width:[x,y] $                   ; # of x&y pixels
       ,slopes:slopes $                 ; device-->data x&y slopes
       ,intercepts:intercepts $         ; device-->data x&y intercepts
       ,imrange:imrange $               ; range of pixel values
       ,tvrange:tvrange $               ; range of tv and cut plots
       ,datlast:[-1,-1] $               ; last mouse pixel; data coords
       ,devlast:[-1,-1] $               ; last mouse pixel; device coords
       ,dragcorner:[-1,-1] $            ; storage for drag box, see look_event
       ,x:x,y:y $                       ; # of x&y pixels
       ,down:lonarr(2,4)-1 $            ; data coords for mouse button presses
       ,up:lonarr(2,4)-1 $              ; data coords for mouse button releases
       ,scaletype:1 $                   ; type of image scaling
       ,scalefunc:scalefunc $           ; user-supplied image scaling function
       ,meansig:[1.,2.,3.] $            ; sigma +/- mean for image scaling
       ,mediansig:[1.,2.,3.] $          ; sigma +/- median for image scaling
      }
;;if n_elements (userscalefunc) gt 0 then all.scalefunc = userscalefunc
;   Set mean scaling to the user specified sigma levels.  Only use the first
;   three levels, if more than three were given.
nmean = n_elements (usermean)
if nmean gt 3 then nmean = 3
if nmean gt 0 then all.meansig(0:nmean-1) = usermean(0:nmean-1)
;   Set median scaling to the user specified sigma levels.  Only use the first
;   three levels, if more than three were given.
nmedian = n_elements (usermedian)
if nmedian gt 3 then nmedian = 3
if nmedian gt 0 then all.mediansig(0:nmedian-1) = usermedian(0:nmedian-1)

zoom = all                              ; same structure for zoom draw widget
zoom.id = ids(0,1)                      ; but change id for use with wset
zoom.wid = wids(0,1)                    ; but change id for widget
zoom.hcutid = ids(1,1)                  ; draw id for horizontal cut
zoom.vcutid = ids(0,0)                  ; draw id for vertical cut
xcut = thisimage(*,0)*0			; x coordinates for hor or vert cuts
ycut = reform(thisimage(0,*)*0)		; y coordinates for hor or vert cuts
ws = { $                                ; window structure
      assoc:associate $                 ; flag for associate variable
      ,filename:filename $              ; associate variable file name
      ,PSfilename:'idl.ps' $            ; PS filename for writing
      ,PSattstr:',color=1,bits=8,encap=0,pre=0,portrait=1' $ ; for device call
      ,PSatt:{look_PSatt $              ; PS attributes
              ,encap:0 $
              ,color:1 $
              ,bits:8 $
              ,portrait:1 $
              ,preview:0 $
             } $
      ,PSct:5 $                         ; PS color table number
      ,definition:definition $          ; associate variable definition
      ,is_struct:is_struct $            ; flag assoc being a structure 
      ,tag:tag $                        ; structure tag of image if is_struct
      ,func:have_func $                 ; flag to say images come from function
      ,loader_name:loader_name $        ; name of function that returns images
      ,increment:increment $            ; increment on click of rt mouse button
      ,star:star $                      ; quantity to increment with rt mouse
      ,starlookup:starlookup $          ; map from star pdmenu to x's & y's
      ,ids:ids $                        ; draw widget ids for use with wset
      ,wids:wids $                      ; draw widget ids for references
      ,last_cutwid: -1 $		; draw widget id for last cut plots
      ,cut_oplot:[0,0] $		; flags to renew hor & vert cut plots
      ,hcutxdata:long(xcut) $		; x coordinates for hor cut
      ,hcutydata:xcut $ 		; y coordinates for hor cut
      ,vcutxdata:ycut $			; x coordinates for vert cut
      ,vcutydata:long(ycut) $ 		; y coordinates for vert cut
      ,d:lonarr(7) $                    ; current dims to browse in series
      ,previous:lonarr(7) $             ; previous dims for recalling
      ,dcat:lonarr(7) $                 ; current dims for catalog/movie/blink
      ,size:si $                        ; size vector for reference
      ,kernel_width:kernel_width $      ; width in pixels of spatial filters
      ,dimid:dimid $		        ; widget ids for dimension sliders
      ,dimname:dimname $        	; names for dimensions
      ,mxd:maxsurfdim $                 ; to avoid overlapping lines
      ,help:help $                      ; help messages
      ,comtext:comtext $                ; id for comment text widget
      ,reptext:reptext $                ; id for report text widget
      ,report:report $                  ; content of report text widget
      ,spansliders:[minslide,maxslide] $ ; ids for plot min and max sliders
      ,spanmagid:spanmagid $		; id for increment text widget
      ,autoscale:[autominbutton,automaxbutton] $  ; IDs for autoscale buttons
      ,spanmode:[1,1] $             	; flags for min/max automatic scaling
      ,srange:srange $                  ; range of sliders
      ,ctrange:tvrange $                ; range of color table
      ,manrange:all.tvrange $           ; manual range of tv and cut plots
      ,fullzoombutton:fullzoombutton $  ; id for plotfullzoom button 
      ,plotfullzoom:0 $                 ; plot flag---3 options
      ,fullzoom:fullzoom $              ; the text for fullzoombutton
      ,fullshown:1 $                    ; Does the full window show full image?
      ,zoomshown:1 $                    ; Does the zoom window show zoom image?
      ,plottype:0 $                     ; Type of plot (0 = normal image)
      ,optmenu:optmenu $                ; names of optional plots
      ,ispro:ispro $                    ; Is option a procedure or a function?
      ,debug:debug $                    ; flag for debug output
      ,drag:bytarr(3) $                 ; mouse buttons' state: 0=up, 1=down
      ,dragstart:[-1,-1] $              ; device coordinates at drag start
      ,dragcorner:[-1,-1] $             ; device coordinates at drag corner
      ,setids:[minset,maxset] $		; ids for text widgets containing range
      ,AddLabels:1 $		        ; flag to add labels to catalog images
      ,digits:digits $                  ; maps of digits for image labels
      ,bothfixid:bothfixid $            ; widget id for Auto/Fix increment switch
      ,bothfix:0 $                      ; flag to automatically update increment
      ,save:0 $                         ; flag to save index data in common
      ,catalogsave:0 $                  ; flag to save catalog data in common
      ,saveoffset:0 $                   ; image offset in catalog in common
      ,reset:0 $                        ; flag to reset catalog & index in common
      ,catshowimages:1 $                ; flag to display images in catalog
      ,catlims:[0L,100 < (nimages-1),1,0] $ ; catalog limits & blink compare
      ,units:units $                    ; axes labels
      ,viewers:viewers $                ; names of procedures to execute on
                                        ;  trigger of mouse events
      ,viewers_set:intarr(n_elements(viewers)) $ ; on/off toggle switches
     }
;
;       =====>> Report Statistics
;
IF n_elements(thisimage) GT 1 THEN begin
   res = moment (thisimage, sdev = sdev)
   all.sd = sdev
   mean = res(0)
endif  ELSE BEGIN
   all.sd = 0.
   mean = 0
ENDELSE
all.mean = mean
ws.report(12) = ' mean='+strtrim(all.mean,2)
ws.report(13) = ' sd='+strtrim(all.sd,2)
widget_control,ws.reptext,set_value = ws.star+ws.report
;
;       =====>> TURN OVER CONTROL TO XMANAGER
;
xmanager,'look',lookbase,event_handler = 'look_event'
;
;       =====>> And clean up.
;
IF associate THEN BEGIN
  close,lun
  free_lun,lun
ENDIF
COMMON colors, r_orig, g_orig, b_orig, r_curr, g_curr, b_curr
tvlct, r_orig, g_orig, b_orig   ; restore original colors
end