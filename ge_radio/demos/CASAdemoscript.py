# User Interface
# --------------

# Feature: shell access            
#
# Note: the following commands only work in interactive mode
# i.e., typing at the terminal. If these were in a script they
# would look like: os.system('pwd'), etc.
#
# Note: pwd, cd, ls can be typed at the CASA prompt and will
# be interpreted as shell commands. Any other command should
# be preceded with an '!' to indicate that it is a shell 
# command.
#
pwd
ls
bookmark data # bookmark the current location, called 'data'
cd ~          # change directory to home area
cd -b data    # returns to the directory bookmarked as data
!more clean.last

# Feature: <TAB> Completion
!more ngc5921_reg<TAB> # returns the rest of the file name
                       # or will provide a list of all matching
                       # variables/files.

# Feature: autoparenthesis         
#
# Note: Each of the 'tasks' in CASA are essentially functions
#       with argument lists. These can be invoked as:
#       taskname(param1=value1, param2=value2, etc)
#       or, using the autoparenthesis feature:
#       taskname param1=value1, param2=value2
#       or, if you provide the values in order:
#       taskname value1, value2
#       or, since the parameters are globals, if they are already set:
#       taskname, e.g.:
vis='ngc5921.ms'
listobs

# Feature: command history
#
# Note: The command history is retained and can be reviewed by:
hist # Note: shell commands are parsed a bit differently and have
     # the funny _ip.magic stuff.
     # The key is the ipython.log file can be re-executed to
     # completely replicate what was done in a reduction session.


# Feature: macro
#
# Note: You can create macros for rapid execution of repeated
#        commands, e.g.:
# Note: I sometimes skip this
x=1
y=2
z=3
hist
macro temp 11-13
z=5
y=-1
z
y
temp
z
y # Note: the values are reset to those from the temp macro

# Feature: session logging
#
# Note: All commands are logged automatically into the file:
#       ipython.log
#       Logging output (information/messages from CASA during
#       a session) are in the file:
#       casapy.log (previous versions are stamped with the time/date):
#       casapy.log-2007-08-09T24:40:17
#       
!more ipython.log
!more casapy.log

# Feature: numbered input/output  	
#
# Note: All input and output is number and can be used as variables.
# Note: I often skip this!
Out[24]*3

# Feature: history/searching       
#
# Note: You can search through your command history as at the Unix
#       prompt:
<CNTRL-R> # This brings up: (reverse-i-search): 
          # type 'list' # you will see it match the last time
          # this command was typed
          # Note: <CNTRL-A> goes to the beginning of the line
                  <CNTRL-K> deletes everything on the line
	  # Note: If you want to go back further, e.g., to the
          # next occurance, hit <CNTRL-R> to go back in time.
          # Also note that you can type:
list<UP ARROW> # to complete the last matching command

# CASA Interface
#  --------------

# Feature: in-line help            
#
# Note: Several levels of help are available within CASA
#       Overview
#       Task lists/summaries
#       Task details
#       Parameter details
startup    # startup screen listing available tasks/tools/help calls
taskhelp   # one line alphabetical summary of the tasks
tasklist   # listing of the tasks by catagory (import, calibration, etc)
help clean # detailed help on a task
           #Note PAGER variable
           #export PAGER='cat' - to scroll information and keep on terminal
           #export PAGER='less' or 'more'-to show information,clear it when done
# help par.paramname (bandname,robust)
help par.bandname
help par.selectdata

# Task interface
#
# Note: inital tasks based on AIPS usage statistics (at AOC over 5 years)
#       then evolved based on feedback and use case needs
#
# Feature: All parameters are global
#
# Note: parameter names have been rationalized between tasks so that navigating
#       a reduction session takes as little typing as possible, e.g., once
#       vis is defined (in the import task), it can then be used for getting
#       information, used in the subsequent calibration tasks, etc

# Feature - Error handling  
#
# Note: Simple typos, bad options in the parameters are checked prior to 
#       execution and can be reviewed with the 'inp' command.
default clean
inp clean
vis='joe' #checks file existence
inp     # Note: joe is marked in red - does not exist
alg='hogwarts'
inp     # Note: hogwarts is in red - not an option
        # If you try to run:
clean # No further processing occurs!
# Feature: task parameter lists expand or contract
#
# Note: parameters in bold will expand/contract based on their context
#       this allows an overall succinct expression of the task parameters
#       expanding as needed by the user:
inp clean
mode='channel'
inp
# Another example as needed:
weighting='briggs'
inp 
# Summary:
#parameter=value
#         parameter
#                    black=standard parameter
#                    bold+highlight=expandable param
#                    indented green=sub parameter
#         value
#                    black=default value
#                    blue=non-default value
#                    red=wrong type,value

# Feature: task execution
#
# Note: There are two ways to execute a task:
inp listobs
listobs # call the taskname
go      # say go, after the taskname has been set (e.g., via inp)

# Feature: Save and restore of parameters
#
# Note: Every time a task is successfully executed, a taskname.last
#       file is saved, storing the parameters of that task execution.
#       To recover the last execution of a task you can:
default clean
inp 
!more clean.last # look at the saved file
run clean.last # set all of the parameters from the last execution
inp 
default clean
execfile 'clean.last' # this is an equivalent way 
vis='temp'
saveinputs 'clean','clean.demo' # you can also manually save the inputs

# Feature: Logging GUI
#
# Note: Basic features:
#       Search - type string 'MAIN' - highlights any log output with 
#                that string.
#       Filter - on Priority, Origin, Message, Time
#              - Select message and type 'TEST'
#       Insert - bottom of GUI, select a row and it will insert above 
#                this; type: JPM: Note 63 channels with RR LL
#                It will put this in the working directory.
#       Other - export to PDF, hide rows, etc (note: cut/paste difference)

#Functionality
#-------------

# Note: Look at end-to-end regression script: ngc5921_regression.py
!vi ngc5921_regression.py
#Note steps - import, flag, calibrate, split, image
execfile 'ngc5921_regression.py' # run it (note steps on terminal window)

# Note: This creates some files to look at

# Feature: Data Exploration
#
# Note: discuss: listobs, listhistory, plotxy
#
# Get a summary of your data
vis='ngc5921.ms'
listobs
listhistory # gives processing history
            # scroll through log, show tasks and parameters
            # all to help recover what has been done to the data

# Data Displays - plotxy
default plotxy
xaxis='time'
plotxy	#show basic plot
subplot=211
xaxis='uvdist'
plotxy	#show multiplotting
xaxis='u'
subplot=223
plotxy
subplot=224
xaxis='x'
plotxy 
#Note zoom, incremental and home

subplot=111
xaxis='uvdist'
plotxy	#show iterative plot
iteration='field_id'
# Use Next button to step through
#Note use locate facility to show identification of points

plotxy	#show overplotting
xaxis='time'
field='0'
plotxy
field='1'
plotcolor='red'
plotxy
field='2'
plotcolor='green'
plotxy
default plotxy
xaxis='uvdist'
field='0'
plotxy

#Note use flag facility to flag data
#Illustrate flagging on spec averages
plotxy(vis="ngc5921.ms", xaxis="channel", yaxis="amp", 
datacolumn="data", selectdata=True, field="0", subplot=211)
plotxy(vis="ngc5921.ms", xaxis="channel", yaxis="amp", 
datacolumn="data", selectdata=True, field="0", 
spw='0:0~62^5',subplot=212,average='channel')
# Now flag a five-channel average - see the flag scope 
# expand to the individual channels

#General averaging
#channel average
default('plotxy');plotxy('ngc5921.ms','channel',spw='0:5~55',antenna='1&26',field='0',subplot=311,correlation='RR');plotxy(spw='0:5~55^5',overplot=True,plotcolor='red',connect='chan')

#time average
default('plotxy');plotxy('ngc5921.ms','channel',spw='0:5~55',antenna='1&26',field='0',subplot=312,correlation='RR');plotxy(overplot=True,plotcolor='red',average='time',averagenrows=100,connect='chan')

#channel and time average
default('plotxy');plotxy('ngc5921.ms','channel',spw='0:5~55',antenna='1&26',field='0',subplot=314,correlation='RR');plotxy(overplot=True,spw='0:5~55^5',plotcolor='red',average='both',averagenrows=100,connect='chan')

# Feature: Flag manager
#
# Note: Ability to save flags at various points, recover or merge them
default plotxy
vis='ngc5921.ms'
xaxis='uvdist'
plotxy
flagmanager mode='save',versionname='current',comment='Current flags'
flagmanager mode='list' # look at logger
# Note: Initial flags are automatically saved in 'Original' upon import into CASA
# Flag some regions - big face for easy identification
flagmanager mode='save',versionname='face',comment='easily identified flag regions'
flagmanager mode='list' # look at versions
flagmanager mode='restore',versionname='current'

# Feature: Plot calibration solutions
# 
# Note: optional
inp plotcal
tablein='ngc5921.gcal'
plotcal
tablein='ngc5921.fluxscale'
plotcal

# Feature: List calibration solutions
#
# Note: Optional - formatting issues
inp listcal
listcal

# Feature: Viewer display/flagging of data
#
# Note: Usually skipped for time reasons
# Note: Similar to TVFLG

# Feature: Imaging tasks 
# 
# Note: clean (several algorithms), mosaic (several algorithms), 
#       invert (dirty beam and dirty map)

# Feature: Displaying images
# 
# Note: 
viewer 'ngc5921_task.image'
zoom
adjust panel - add channel labels
multi-panel
movie
statistics
profile support

# Feature: Moments
# 
# Note: do a help on immoments 
