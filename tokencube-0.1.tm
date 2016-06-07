##################
## Module Name     --  tokencube
## Original Author --  Emmanuel Frecon - emmanuel@sics.se
## Description:
##
##      This module is a linux-only implementaton of the tokencube BLE protocol.
##      The implementation uses techniques similar to the original connector in
##      Java. For most cases, you will only need to initialise the module in
##      order to lookup the device to use for BLE communication and to pass this
##      device to ::tokencube::listen, together with a command to be called back
##      everytime the value of a sensor has changed (or been discovered).
##
##		The underlying linux commands need to be run with elevated privileges.
##
## Commands Exported:
##      All procedures starting with a lowercase are exported.  Each
##      destination will be associated to a command for tk-style API.
##      Also this creates a biotd (d as in destination) global command
##      to ease calling from the outside.
##################

namespace eval ::tokencube {
	namespace eval vars {
		variable -dev "";        # Device to use, empty for guess.
		variable -announce 600;  # Default announce frequency in milliseconds
		
		# Below are the linux commands that we depend on, most of them come from BlueZ
		variable -tool hcitool;
		variable -dump hcidump;
		variable -config hciconfig;
		variable -kill kill;
		
		variable addrPattern {[0-9a-fA-F][0-9a-fA-F]:[0-9a-fA-F][0-9a-fA-F]:[0-9a-fA-F][0-9a-fA-F]:[0-9a-fA-F][0-9a-fA-F]:[0-9a-fA-F][0-9a-fA-F]:[0-9a-fA-F][0-9a-fA-F]}
	}
	
	namespace export {[a-z]*}
	namespace ensemble create -command ::tokencube
}


# ::tokencube::devices -- List of BLE devices
#
#       Inquire the underlying system to get a list of all possible BLE devices.
#       This does not require elevated privileges to run. The implementation is
#       a wrapper around "hcitool dev"
#
# Arguments:
#       None.
#
# Results:
#       A list of local devices
#
# Side Effects:
#       None.
proc ::tokencube::devices {} {
	set devices [list]
	set tool [auto_execok ${vars::-tool}]
	set fd [open "|$tool dev"]
	while { ![eof $fd] } {
		set line [string trim [gets $fd]]
		if { [llength $line] >= 2 } {
			lassign $line dev addr
			if { [string match $vars::addrPattern $addr] } {
				lappend devices $dev
			}
		}
	}
	close $fd
	
	return $devices
}



# ::tokencube::init -- Initialise connection
#
#       Initialise the connection, this will either take the device that is
#       specified as a global in -dev or the first device in the list of
#       devices. The initialisation brings down and then up the interface,
#       meaning that elevated privileges are required.
#
# Arguments:
#       None.
#
# Results:
#       The device that was initialised and used.
#
# Side Effects:
#       None.
proc ::tokencube::init {} {
	set dev ${vars::-dev}
	if { $dev eq "" } {
		set dev [lindex [devices] 0]
	}
	
	if { $dev eq "" } {
		return -code error "Cannot find any device to communicate with"
	}
	
	set config [auto_execok ${vars::-config}]
	exec -- $config $dev down
	exec -- $config $dev up
	
	return $dev
}



# ::tokencube::listen -- Listen to tokencube events
#
#       This is the main command: it takes a device (typically returned by init,
#       above) and a number of dash-led options and their values. Recognised
#       options are:
#
#	-scanner   How often we should scan and listen (in ms)?  Set to zero or
#                  negative for manual scanning (see below)
#       -announce  Command to call on reception of advertisement frames. The
#                  mac address of the remote device and list of captured bytes
#                  for the advertisement are passed as parameters.
#       -sensor    Command to sensor data arrival. The mac address of the remote
#                  device, the type of sensors and additional arguments (sensor-
#                  dependent) are passed as parameters. The arguments are the
#                  value(s) of the sensor.
#       -onchange  Same as above, but only when value has changed.
#
# Arguments:
#	dev	   Device to listen on (as returned by init)
#       args       List of dash-led options and values, see above
#
# Results:
#       The file descriptor where we are listen to for frame data
#
# Side Effects:
#       Listen to output of hcidump, which needs proper access to the device.
proc ::tokencube::listen { dev args } {
	set dump [auto_execok ${vars::-dump}]
	if { $dump eq "" } {
		return -code error "Cannot find hcidump!"
	}

	# Open a pipe to dump --raw so we'll be able to capture raw BLE data on the
	# line 
	set fd [open "|$dump -i $dev --raw"]
	
	# Create and initialise a context
	set vname [namespace current]::$fd
	upvar \#0 $vname context	
	dict set context dir ""
	dict set context accu [list]
	
	# Setup default options and parse what was given on the command line.
	dict set context -scanner ${vars::-announce}
	dict set context -announce [list]
	dict set context -sensor [list]
	dict set context -onchange [list]
	set opts [dict keys $context -*]
	foreach {k v} $args {
		set opt -[string trimleft $k -]
		if { $opt ni $opts } {
			close $fd
			return -code error "$opt is an unknown option, should be one of [join $opts ,\ ]"
		} else {
			dict set context $opt $v
		}
	}
	
	# Start consuming low-level BLE data
	fileevent $fd readable [namespace code [list Reader $fd]]
	
	# Automatically start scanning.
	if { [dict get $context -scanner] >= 0 } {
		dict set context scanner [scanner $dev [dict get $context -scanner]]
	}
	
	return $fd
}


# ::tokencube::scanner -- Periodic scan
#
#       Start periodically scanning, thus triggering answer and data provision
#       from the tokencube sensors.
#
# Arguments:
#	dev	   Device to listen on (as returned by init)
#       ms         Number of millseconds, negative for default from -announce
#
# Results:
#       None.
#
# Side Effects:
#       Will periodically initiate a BLE scan so we can capture data from nearby
#       sensors.
proc ::tokencube::scanner { dev { ms -1 } } {
	if { $ms <= 0 } {
		set ms ${vars::-announce}
	}
	
	set tool [auto_execok ${vars::-tool}]
	set fd [open "|$tool -i $dev lescan 2>@1"]
	fileevent $fd readable [namespace code [list ScanReader $fd]]
	after $ms [namespace code [list ScanAbort $fd $dev $ms]]
}



#####################################
##
## Below are all internal procedures, change only if you want to help
##
#####################################

# ::tokencube::ScanReader -- Read lines from scanning
#
#       Read lines output from the scanning process and close file descriptor on
#       eof.
#
# Arguments:
#	fd	   Filedescriptor to scanner output
#
# Results:
#       None.
#
# Side Effects:
#       Will close the file descriptor when eof has been reached
proc ::tokencube::ScanReader { fd } {
	# Close whenever EOF reached (process died)
	if { [eof $fd] } {
		fileevent $fd readable [list]
		catch {close $fd}
		return;  #What to do??
	}

	set line [gets $fd]
}


# ::tokencube::ScanAbort -- Abort scanning
#
#       Abort scanning, which will actively kill the scanning process and reset
#       the BLE device, since otherwise next scanning would fail. (re)schedule
#       new scan in the scanning period.
#
# Arguments:
#	fd	   Filedescriptor to scanner output
#	dev	   BLE device we are scanning on
#	ms	   Scanning period
#
# Results:
#       None.
#
# Side Effects:
#       Kill process and reset device!
proc ::tokencube::ScanAbort { fd dev ms } {
	# Kill current scanning process
	set kill [auto_execok ${vars::-kill}]
	if { [catch {pid $fd} pid] == 0 } {
		exec $kill $pid
		catch {close $fd}
	}
	
	# Reset BLE device
	set config [auto_execok ${vars::-config}]
	exec $config $dev reset
	
	# Scan again in a while
	scanner $dev $ms
}


# ::tokencube::ConvertInt8 -- Convert to Int8
#
#       Convert list of bytes (1!) to an Int8. The bytes are joined and put
#       together in a big binary block before we extract the (signed) value from
#       the set of bytes.
#
# Arguments:
#	l	   List of bytes
#
# Results:
#       Value represented by the bytes.
#
# Side Effects:
#       None
proc ::tokencube::ConvertInt8 { l } {
    set bytes [binary format H* [join $l ""]]
    binary scan $bytes c res
    return $res
}


# ::tokencube::ConvertInt16 -- Convert to Int16
#
#       Convert list of bytes (2!) to an Int16. The bytes are joined and put
#       together in a big binary block before we extract the (signed) value from
#       the set of bytes.
#
# Arguments:
#	l	   List of bytes
#	dvd	   Divider to divide value before returning
#
# Results:
#       Value represented by the bytes.
#
# Side Effects:
#       None
proc ::tokencube::ConvertInt16 { l { dvd 1 } } {
    set bytes [binary format H* [join $l ""]]
    binary scan $bytes S res
    return [expr {double($res)/$dvd}]
}


# ::tokencube::ConvertInt32 -- Convert to Int32
#
#       Convert list of bytes (4!) to an Int32. The bytes are joined and put
#       together in a big binary block before we extract the (signed) value from
#       the set of bytes.
#
# Arguments:
#	l	   List of bytes
#	dvd	   Divider to divide value before returning
#
# Results:
#       Value represented by the bytes.
#
# Side Effects:
#       None
proc ::tokencube::ConvertInt32 { l { dvd 1 } } {
    set bytes [binary format H* [join $l ""]]
    binary scan $bytes I res
    return [expr {double($res)/$dvd}]
}


# ::tokencube::Callback -- Callback with sensor values
#
#       Callback the various relevant procedure with sensor values. This will
#       callback the generic -sensor command, but also detect whenever changes
#       to the values have occurred and call the -onchange command as
#       appropriate.
#
# Arguments:
#	fd	   File descriptor where we are listening
#	mac	   MAC address of remote device
#	type	   Type of sensor
#	args	   Values of the sensor
#
# Results:
#       None
#
# Side Effects:
#       None
proc ::tokencube::Callback { fd mac type args } {
	set vname [namespace current]::$fd
	upvar \#0 $vname context

	# Callback generic command
	if { [llength [dict get $context -sensor]] } {
		{*}[dict get $context -sensor] $mac $type {*}$args
	}
	
	# Detect changes and callback -onchange whenever first time/modified
	# data has occured for sensor. The implementation supposes that there is
	# only one type of sensor per mac address.
	if { [llength [dict get $context -onchange]] } {
		set sname [namespace current]::$mac
		upvar \#0 $sname sensor
		if { [info exists sensor] && [dict exists $sensor $type] } {
			if { [dict get $sensor $type] ne $args } {
				{*}[dict get $context -onchange] $mac $type {*}$args
			}
			dict set sensor $type $args
		} else {
			{*}[dict get $context -onchange] $mac $type {*}$args
			dict set sensor $type $args
		}
	}
}


# ::tokencube::Decompose -- Decodes data frames
#
#       Decode the content of an advertisement frame sent by a tokencube sensor
#       into the different sensor values that it contains and performs adequate
#       callbacks.
#
# Arguments:
#	fd	   File descriptor where we are listening
#	mac	   MAC address of remote device
#	sensors	   List of bytes for advertisement.
#
# Results:
#       None
#
# Side Effects:
#       None
proc ::tokencube::Decompose { fd mac sensors } {
	set j 0
	set len [llength $sensors]
	while { $j < $len } {
		set sensorid [lindex $sensors $j]
		incr j
		switch -- $sensorid {
			"81" -
			"01" {
				set temp [ConvertInt16 [lrange $sensors $j [expr {$j+1}]] 100]
				incr j 2
				Callback $fd $mac TEMPERATURE $temp
			}
			"84" -
			"04" {
				set hum [ConvertInt16 [lrange $sensors $j [expr {$j+1}]] 100]
				incr j 2
				Callback $fd $mac HUMIDITY $hum
			}
			"85" -
			"05" {
				set press [ConvertInt32 [lrange $sensors $j [expr {$j+3}]] 100]
				incr j 4
				Callback $fd $mac PRESSURE $press
			}
			"86" -
			"06" {
				set x [ConvertInt16 [lrange $sensors $j [expr {$j+1}]] 4096]
				incr j 2
				set y [ConvertInt16 [lrange $sensors $j [expr {$j+1}]] 4096]
				incr j 2
				set z [ConvertInt16 [lrange $sensors $j [expr {$j+1}]] 4096]
				incr j 2
				Callback $fd $mac ORIENTATION $x $y $z
			}
			"87" -
			"07" {
				set pir [ConvertInt8 [lindex $sensors $j]]
				incr j
				Callback $fd $mac PRESENCE $pir
			}
			"88" -
			"08" {
				set inmotion [ConvertInt8 [lindex $sensors $j]]
				incr j
				Callback $fd $mac MOTION $inmotion
			}
			"89" -
			"09" {
				set x [ConvertInt8 [lindex $sensors $j]]
				incr j
				set y [ConvertInt8 [lindex $sensors $j]]
				incr j
				set z [ConvertInt8 [lindex $sensors $j]]
				incr j
				Callback $fd $mac SHOCK $x $y $z
			}
			"8A" -
			"0A" {
				set batt [ConvertInt8 [lindex $sensors $j]]
				incr j
				Callback $fd $mac BATTERY $batt
			}
		}
	}
}


# ::tokencube::Frame -- Parse tokencube data frames
#
#       Decode the content of a data frame and extract sensor data from
#       advertisements, whenver available.
#
# Arguments:
#	fd	   File descriptor where we are listening
#	direction	   > or < to indicate sent or received (respectively)
#	bytes	   List of bytes for frame.
#
# Results:
#       None
#
# Side Effects:
#       None
proc ::tokencube::Frame { fd direction bytes } {
	set vname [namespace current]::$fd
	upvar \#0 $vname context

    #puts stdout "FRAME: $direction -- $bytes"
    if { $direction ne ">" } {
		return
    }
    
    if { [lindex $bytes 0] ne "04" || [lindex $bytes 1] ne "3E" } {
		return
    }
    
    set len [scan [lindex $bytes 2] %x]
    if { [llength $bytes] != $len+3 } {
		return -code error "Wrong frame: expected length: $len"
    }

    set type [lindex $bytes 5]
	switch -- $type {
		"00" {
			set mac [join [lreverse [lrange $bytes 7 12]] :]
			set plen [scan [lindex $bytes 13] %x]
			set adv_payload [lrange $bytes 14 [expr {$plen+13}]]
			set i 0
			while { $i < $plen } {
				set ad_len [scan [lindex $adv_payload $i] %x]
				incr i
				set ad_type [lindex $adv_payload $i]
				incr i
				set ad_data [lrange $adv_payload $i [expr {$i+$ad_len-2}]]
				incr i [expr {$ad_len-1}]
				#puts "$ad_type: $ad_len --> $ad_data"
				switch -- $ad_type {
					"08" {
						# Get the short name from $ad_data??
					}
					"01" {
						# What is it?
					}
					"FF" {
						# Check manufacturer ID.
						if { [lindex $ad_data 0] eq "EE" && [lindex $ad_data 1] eq "FF" } {
							set hw_id [lindex $ad_data 2]
							set fw_version [lindex $ad_data 3]
							set page_info [lindex $ad_data 4]
							set adv [lrange $ad_data 5 end]
							if { $hw_id eq "04" && $fw_version eq "01" } {
								if { [llength [dict get $context -announce]] } {
									{*}[dict get $context -announce] $mac $adv
								}
								Decompose $fd $mac $adv
							}
						}
					}
				}
			}
		}
	}
}


# ::tokencube::Reader -- Line reader for hcidump output
#
#       Read the content of hcidump on a line-by-line basis and reconstruct the
#       frames (and directions). Part of the algorithm is based on the knowledge
#       that output is 20 bytes long.
#
# Arguments:
#	fd	   File descriptor where we are listening
#
# Results:
#       None
#
# Side Effects:
#       None

proc ::tokencube::Reader { fd } {
	set vname [namespace current]::$fd
	upvar \#0 $vname context

	if { [eof $fd] } {
		return;  #What to do??
	}

	set line [gets $fd]
	if { $line ne "" } {
		set first [string index $line 0]
		set bytes [split [string trim [string range $line 2 end]]]
		switch -- $first {
			">" -
			"<" {
				# Output previous frame
				if { [llength [dict get $context accu]] > 0 } {
					Frame $fd [dict get $context dir] [dict get $context accu]
					dict set context dir ""
					dict set context accu [list]
				}
				
				# Output frame or accumulate in preparation for next line
				if { [llength $bytes] < 20 } {
					Frame $fd $first $bytes
					dict set context dir ""
					dict set context accu [list]
				} else {
					dict set context dir $first
					dict set context accu $bytes
				}
			}
			" " {
				# Accumulate with previous
				dict set context accu [concat [dict get $context accu] $bytes]
				
				# Send
				if { [llength $bytes] < 20 } {
					Frame $fd [dict get $context dir] [dict get $context accu]
					dict set context dir ""
					dict set context accu [list]
				}
			}
		}
	}
}



#####################################
##
## Simple test whenever this file is sourced directly. This performs
## initialisation and registers a procedure that will be called whenever data
## for any nearby sensor has changed. The procedure outputs the type of the
## value, its type and unit.
##
#####################################

if { [info exists argv0] && [info script] eq $argv0 } {
	proc dump { mac type args } {
		set units {
			TEMPERATURE		"C"
			HUMIDITY		"%RH"
			PRESSURE		"mBar"
			ORIENTATION		"g"
			PRESENCE		"bool"
			MOTION		"bool"
			SHOCK		"bool"
			BATTERY		"%"
		}
		puts "$mac -- $type: $args [dict get $units $type]"
	}
	set dev [::tokencube::init]
	::tokencube::listen $dev -onchange dump

	vwait forever
}
