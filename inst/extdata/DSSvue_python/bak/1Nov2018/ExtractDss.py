'''A script passed to HecDssVue in "batch mode" and its embedded jython 
implementation of python version 2.2.1 (April 10, 2002)

*** This file Should Not be used as a template for contemporary Python
development.  It is specific to the Neolithic Python 2.2. ***

The HecDssVue image is: /opt/physical/HEC-DSSVue/hec-dssvue201/hec-dssvue.sh
and it relies upon an embedded java (./java/bin/java), so I use a wrapper as:

> cat /usr/local/bin/hec-dssvue 
#!/bin/bash
cd /opt/physical/HEC-DSSVue/hec-dssvue201
./hec-dssvue.sh $1

To get command line parameters in sys.argv, all arguments must be
in a single string for $1 viz: dssvue "/opt/foo/bar/script.py -v -h"

The expected usage is:
   hec-dssvue "/opt/foo/bar/ExtractDss.py -i /opt/foo/bar/config.ini"
where config.ini is a version of ExtractDss.ini.
'''

# Python distribution modules
from sys     import argv, version
from os.path import isfile
from getopt  import getopt, GetoptError
import ConfigParser

# Community modules
from hec.heclib.dss import HecDss
from hec.dataTable  import HecDataTable, HecDataTableToExcel
from hec.script     import Tabulate, TableExportOptions
from hec.dssgui     import ListSelection
from java.util      import Vector
from java.lang      import Exception as JavaException

#----------------------------------------------------------------------------
#----------------------------------------------------------------------------
def ShowSysInfo():
    
    print "\nShowSysInfo(): python %s, argv [n=%d]: %s\n" % \
        ( version, len( argv ), argv )

#----------------------------------------------------------------------------
#----------------------------------------------------------------------------
class ConfigFileParameters:
    '''Data container for ReadConfigFile() parameters'''
    
    def __init__( self ):
        self.outFormat   = None
        self.tableTitle  = None
        self.dataDir     = None
        self.outDir      = None
        self.dssFile     = None
        self.outFile     = None
        self.dssDataType = None
        self.fixHeader   = None
        self.dssPathIDs  = []

    def Print( self ):
        print 'ConfigFileParameters:'
        print 'outFormat = %s  tableTitle = %s  dataDir = %s  outDir = %s' % \
            ( self.outFormat, self.tableTitle, self.dataDir, self.outDir )
        print 'fixHeader = %s dssFile = %s  outFile = %s  dssDataType = %s\n' %\
            ( self.fixHeader, self.dssFile, self.outFile, self.dssDataType )
        for dssPathID in self.dssPathIDs :
            print dssPathID,
        print

#----------------------------------------------------------------------------
#----------------------------------------------------------------------------
def ReadConfigFile( configFile, verbose ):

    if verbose:
        print "\nReadConfigFile(): configFile %s\n" % configFile

    try:
        config = ConfigParser.ConfigParser()

        config.read( configFile )

        configParams = ConfigFileParameters()
        
        configParams.outFormat  = config.get( 'DEFAULT', 'Output_Format' )
        configParams.tableTitle = config.get( 'DEFAULT', 'Table_Title'   )
        configParams.fixHeader  = config.get( 'DEFAULT', 'Fix_Header'    )

        configParams.dataDir = config.get( 'paths', 'Data_Directory'   )
        configParams.outDir  = config.get( 'paths', 'Output_Directory' )

        configParams.dssFile = config.get( 'files', 'DSS_File'    )
        configParams.outFile = config.get( 'files', 'Output_File' )

        configParams.dssDataType = config.get( 'DSS', 'Data_Type' )
        configParams.dssPathIDs  = config.get( 'DSS', 'ID' ).split()

    except Exception, err:
        print "%s %s\n" % ( "ReadConfigFile():", err )

    if verbose:
        print "\nReadConfigFile(): "
        configParams.Print()

    return configParams

#----------------------------------------------------------------------------
#----------------------------------------------------------------------------
def RefreshCatalog( configParams, verbose ):
    
    if verbose:
        print "\nRefreshCatalog(): dssFile %s\n" % configParams.dssFile
    
    # Refresh catalog and close file
    try: # In python 2.2 try: except: and try: finally: are independent
        try:
            ls = ListSelection.getMainWindow()
            ls.open( configParams.dataDir + '/' + configParams.dssFile )
            ls.refreshCatalog()
            
        except JavaException, err :
            print "%s %s\n" % ( "RefreshCatalog():", err.getMessage() )
        
    finally:
        ls.finish()

#----------------------------------------------------------------------------
#----------------------------------------------------------------------------
def ExtractDSStoCsv( configParams, verbose ):
    
    print "\nExtractDSStoCsv(): dssFile %s\n" % configParams.dssFile
    
    try:
        try:
            dss = HecDss.open( configParams.dataDir + '/' +\
                               configParams.dssFile )

            table = Tabulate.newTable()
            
            # Get catalog of paths in the dss file
            # The Desire is to use the dss.getCatalogedPathnames( pathSpec )
            # API with a pathSpec = '/*/CELL*/*/*/*/*/' or 'B=CELL*',
            # but those don't seem to work.  It may be that they require
            # a .dsc catalog file to exist?
            # So, extra work to subset the entire path list into
            # a list of what is requested...

            # Get the entire DSS path list ('/' delimited list of strings)
            pathNameList = dss.getPathnameList()

            if verbose:
                print '\nExtractDSStoCsv(): Found %d total paths\n' %\
                    len( pathNameList )
                print 'ExtractDSStoCsv(): several paths:'
                print pathNameList[0]
                print pathNameList[ int( len(pathNameList)/2 ) ]
                print pathNameList[ len(pathNameList) - 1 ]
                print

            # Subset the available dss file paths into those that
            # contain configParams.dssPathIDs and configParams.dssDataType
            # and store in dssPaths.
            # We only need one instance of the path that has the PathID
            # and DataType, not all paths with different dates for the 
            # dssPathIDs and dssDataType
            dssPaths = []
            for dssPathID in configParams.dssPathIDs :
                foundID = False
                for dssPath in pathNameList :
                    if foundID :
                        break
                    dssPathWords = dssPath.split('/')
                    if dssPathID in dssPathWords and \
                       configParams.dssDataType in dssPath:
                        dssPaths.append( dssPath )
                        foundID = True
                        break

            if verbose :
                print 'ExtractDSStoCsv(): Subset %d dssPaths' % len(dssPaths)
                    
            # Extract data and add to table
            for dssPath in dssPaths :
                # True flag ignores D (date) path and returns all dates
                data = dss.get( dssPath, True )

                if data.numberValues == 0 :
                    print "%s %s %s\n" % ( "ExtractDSStoCsv():",
                                           dssPath, 'No Data' )
                else :
                    print 'ExtractDSStoCsv(): %s numberValues = %d' %\
                        ( dssPath, data.numberValues )

                    table.addData( data )

            #--------------------------------------------------------
            # Configure table options
            tableOptions = TableExportOptions()
            tableOptions.fixedWidthCols = False
            tableOptions.delimiter      = ","
            tableOptions.title          = configParams.tableTitle

            # Export the table 
            table.export( configParams.outDir + '/' + configParams.outFile,
                          tableOptions )
            table.close()
        
        except Exception, err :
            print "%s %s\n" % ( "ExtractDSStoCsv(): Error", err )
    
        except JavaException, err :
            print "%s %s\n" % ("ExtractDSStoCsv(): Error", err.getMessage())
        
    finally:
        dss.done()

    # DSS .csv output has a glitch where the header column in line 2 has:
    # "Date / Time" rather than a columar consistent "Date, Time"
    # Try to fix that.
    if "true" in configParams.fixHeader.lower() :
        fob   = open( configParams.outDir + '/' + configParams.outFile, 'r')
        lines = fob.readlines()
        fob.close()

        # Line 2 is the offending header
        lines[1] = lines[1].replace( ' / ', ',', 1 )

        # Line3 is the '-------...------' DSS decorator
        lines.pop( 2 )

        fob = open( configParams.outDir + '/' + configParams.outFile, 'w')
        for line in lines:
            fob.write( line )
        fob.close()

#----------------------------------------------------------------------------
#----------------------------------------------------------------------------
def ExtractDSStoExcel( configParams, verbose ):
    
    print "\nExtractDSStoExcel(): dssFile %s\n" % configParams.dssFile
    
    try:
        try:
            dss = HecDss.open( configParams.dataDir + '/' +\
                               configParams.dssFile )

            datasets = Vector()
            
            pathNameList = dss.getPathnameList()

            # Subset the available dss file paths into those that
            # contain the dssPathIDs and dssDataType
            dssPaths = []
            for dssPathID in configParams.dssPathIDs :
                foundID = False
                for dssPath in pathNameList :
                    if foundID :
                        break
                    if dssPathID in dssPath and \
                       configParams.dssDataType in dssPath:
                        dssPaths.append( dssPath )
                        foundID = True
                        break

            # Extract data and add to Vector datasets
            for dssPath in dssPaths :
                data = dss.get( dssPath, True )
        
                if data.numberValues == 0 :
                    print "%s %s %s\n" % ( "ExtractDSStoExcel():",
                                           dssPath, 'No Data' )
                else :
                    print 'ExtractDSStoExcel(): %s numberValues = %d' % \
                        dssPath, data.numberValues

                    datasets.add( data )

            # Export the .xls file
            list = []
            list.append( datasets )        
            table = HecDataTableToExcel.newTable()
            table.createExcelFile( list, outFile )        
        
        except Exception, err :
            print "%s %s\n" % ( "ExtractDSStoExcel(): Python Error", err )
    
        except JavaException, err :
            print "%s %s\n" % ("ExtractDStoExcelS(): Error", err.getMessage())
        
    finally:
        dss.done()

#----------------------------------------------------------------------------
#----------------------------------------------------------------------------
def usage():
    print 'usage: hec-dssvue "/opt/foo/bar/ExtractDss.py -i ' \
          '/opt/foo/bar/config.ini"'
    print 'args: ' + argv[0] + ' -i iniFile [-v -h]'
    print '      ' + argv[0] + ' --iniFile file [--verbose --help]'
    
#----------------------------------------------------------------------------
#----------------------------------------------------------------------------
def ProcessDSS():
    
    ShowSysInfo()

    # Parse command line options with getopt (No ArgParse in Python 2.2!)
    iniFile = None
    verbose = False
    try:
        opts, args = getopt( argv[1:], "i:vh",
                             [ "iniFile=", "verbose", "help" ] )

    except GetoptError: # no valid options found
        usage()
        return

    for opt, arg in opts:
        if opt in ( "-i", "--iniFile" ):
            iniFile = arg
        if opt in ( "-v", "--verbose" ):
            verbose = True
        if opt in ( "-h", "--help" ):
            usage()

    if not iniFile :
        print "main(): -i iniFile not found in command line"
        return

    #--------------------------------------------------------
    if not isfile( iniFile ) :
        raise IOError( iniFile + " does not exist." )
    
    configParams = ReadConfigFile( iniFile, verbose )
    
    RefreshCatalog( configParams, verbose )

    if "csv" in configParams.outFormat : 
        ExtractDSStoCsv( configParams, verbose )
        
    elif "xls" in configParams.outFormat :
        ExtractDSStoExcel( configParams, verbose )

    else :
        print "main(): Error.  Unrecognized Output_Format: " + \
            configParams.outFormat
        

#----------------------------------------------------------------------------
#----------------------------------------------------------------------------
ProcessDSS()
