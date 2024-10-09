# tcltestex - improved tcltest
#
# Copyright (C) 2024 Konstantin Kushnir <chpock@gmail.com>
#
# See the file "license.terms" for information on usage and redistribution of
# this file, and for a DISCLAIMER OF ALL WARRANTIES.

package require tcltest

namespace eval ::tcltest {

    namespace export makeBinFile viewBinFile assert*

    variable ex_memdebug [info exists ::env(MEMDEBUG)]
    variable ex_testname
    variable ex_testname_nested
    variable ex_info

    Option -colors -1 {
        Whether to use ANSI colors
    } AcceptInteger

    variable colors4msg [dict create]
    variable ansi_colors [list \
        \$black  "\033\[0;30m" \$white  "\033\[1;37m" \
        \$gray   "\033\[1;30m" \$Gray   "\033\[0;37m" \
        \$red    "\033\[0;31m" \$Red    "\033\[1;31m" \
        \$green  "\033\[0;32m" \$Green  "\033\[1;32m" \
        \$yellow "\033\[0;33m" \$Yellow "\033\[1;33m" \
        \$blue   "\033\[0;34m" \$Blue   "\033\[1;34m" \
        \$purple "\033\[0;35m" \$Purple "\033\[1;35m" \
        \$cyan   "\033\[0;36m" \$Cyan   "\033\[1;36m" \
        \$reset  "\033\[0m" \
    ]

    foreach { _match _replace } [list {*}{
        {^(Tests began at )(.+)$}                               {$purple\1$white\2}
        {^(Tests ended at )(.+)$}                               {$purple\1$white\2}
        {^(Test files exiting with errors)(:)(.*)$}             {$red\1$gray\2$reset\3}
        {^(Files with failing tests)(:)(.*)$}                   {$red\1$gray\2$reset\3}
        {^(Test file error)(:)(.*)$}                            {$red\1$gray\2$reset\3}
        {^(Error)(:)(.*)$}                                      {$red\1$gray\2$reset\3}
        {^(==== )(\S+?)(-)?([\d\.]+)?( .*?)( FAILED)$}          {$gray\1$reset\2\3$cyan\4$white\5$Red\6}
        {^(==== )(Contents of test case)(:)$}                   {$gray\1$red\2$gray\3}
        {^(---- )(Test setup failed)(:)(.*)$}                   {$gray\1$red\2$gray\3$reset\4}
        {^(---- )(errorInfo\(setup\))(:)(.*)$}                  {$gray\1$red\2$gray\3$reset\4}
        {^(---- )(errorCode\(setup\))(:)(.*)$}                  {$gray\1$red\2$gray\3$reset\4}
        {^(---- )(Error testing result)(:)(.*)$}                {$gray\1$red\2$gray\3$reset\4}
        {^(\n?---- )(Result was)(:)(.*)$}                       {$gray\1$red\2$gray\3$reset\4}
        {^(---- )(Result should have been \(\S+ matching\))(:)(.*)$}              {$gray\1$red\2$gray\3$reset\4}
        {^(---- )(Error code was)(:)(.*)$}                      {$gray\1$red\2$gray\3$reset\4}
        {^(---- )(Error code should have been)(:)(.*)$}         {$gray\1$red\2$gray\3$reset\4}
        {^(---- )([^;]+; Return code was)(:)(.+)$}              {$gray\1$red\2$gray\3$reset\4}
        {^(---- )(Return code should have been one of)(:)(.+)$} {$gray\1$red\2$gray\3$reset\4}
        {^(---- )(errorInfo)(:)([^\n]+)(.*)$}                   {$gray\1$red\2$gray\3$Red\4$reset\5}
        {^(---- )(errorCode)(:)(.*)$}                           {$gray\1$red\2$gray\3$Red\4}
        {^(---- )(Error testing output)(:)(.*)$}                {$gray\1$red\2$gray\3$Red\4}
        {^(---- )(Output was)(:)(.*)$}                          {$gray\1$red\2$gray\3$Red\4}
        {^(---- )(Output should have been \(\S+ matching\))(:)(.*)$}              {$gray\1$red\2$gray\3$Red\4}
        {^(---- )(Error testing errorOutput)(:)(.*)$}           {$gray\1$red\2$gray\3$Red\4}
        {^(---- )(Error output was)(:)(.*)$}                    {$gray\1$red\2$gray\3$Red\4}
        {^(---- )(Error output should have been \(\S+ matching\))(:)(.*)$}        {$gray\1$red\2$gray\3$Red\4}
        {^(---- )(Test cleanup failed)(:)(.*)$}                 {$gray\1$red\2$gray\3$reset\4}
        {^(---- )(errorInfo\(cleanup\))(:)(.*)$}                {$gray\1$red\2$gray\3$reset\4}
        {^(---- )(errorCode\(cleanup\))(:)(.*)$}                {$gray\1$red\2$gray\3$reset\4}
        {^(---- )(Core file produced while running test!)(.*)$} {$gray\1$red\2$reset\3}
        {^(==== )(Assertion failed on line \d+)(:)$}            {$gray\1$red\2$gray\3}
        {^(---- )(Value from test was)(:)$}                     {$gray\1$red\2$gray\3}
        {^(---- )(Expected value was)(:)$}                      {$gray\1$red\2$gray\3}
        {^(---- )(Should have been equal to this value)(:)$}    {$gray\1$red\2$gray\3}
        {^(---- )(Not a number value in assertion statement)(:)$}                 {$gray\1$red\2$gray\3}
        {^(---- )(Value from test is not a number)(:)$}         {$gray\1$red\2$gray\3}
        {^(---- )(Should have been less than this value)(:)(.*)$}                 {$gray\1$red\2$gray\3$reset\4}
        {^(---- )(Should have been less than or equal to this value)(:)(.*)$}     {$gray\1$red\2$gray\3$reset\4}
        {^(---- )(Should have been greater than this value)(:)(.*)$}              {$gray\1$red\2$gray\3$reset\4}
        {^(---- )(Should have been greater than or equal to this value)(:)(.*)$}  {$gray\1$red\2$gray\3$reset\4}
        {^(---- )(Not a number value in the 1st base of assertion statement)(:)$} {$gray\1$red\2$gray\3}
        {^(---- )(Not a number value in the 2nd base of assertion statement)(:)$} {$gray\1$red\2$gray\3}
        {^(---- )(Should have been less than or equal to )(\d+)(, end greater than or equal to )(\d+)$} {$gray\1$red\2$reset\3$red\4$reset\5}
        {^(---- )(Specified code did not return an error)(:)$}  {$gray\1$red\2$gray\3}
        {^(---- )(Returned message)(:)$}                        {$gray\1$red\2$gray\3}
        {^(---- )(Returned code)(:)(.*)$}                       {$gray\1$red\2$gray\3$reset\4}
        {^(---- )(Specified code did not return the expected error message)(:)$}  {$gray\1$red\2$gray\3}
        {^(---- )(Error message from test was)(:)$}             {$gray\1$red\2$gray\3}
        {^(---- )(Expected error message was)(:)$}              {$gray\1$red\2$gray\3}
        {^(---- )(Should have matched \(case-sensitive\) glob pattern)(:)$}       {$gray\1$red\2$gray\3}
        {^(---- )(or should have matched \(case-sensitive\) glob pattern)(:)$}    {$gray\1$red\2$gray\3}
        {^(---- )(Should NOT have matched \(case-sensitive\) glob pattern)(:)$}   {$gray\1$red\2$gray\3}
        {^(---- )(Should have matched \(case-insensitive\) glob pattern)(:)$}     {$gray\1$red\2$gray\3}
        {^(---- )(Should NOT have matched \(case-insensitive\) glob pattern)(:)$} {$gray\1$red\2$gray\3}
        {^(---- )(Wrong regexp in assertion statement)(:)$}                       {$gray\1$red\2$gray\3}
        {^(---- )(Regexp compile error)(:)$}                                      {$gray\1$red\2$gray\3}
        {^(---- )(Should have been matched regexp pattern)(:)$}                   {$gray\1$red\2$gray\3}
        {^(---- )(Should NOT have been matched regexp pattern)(:)$}               {$gray\1$red\2$gray\3}
        {^(---- )(Should have contained this substring)(:)$}                      {$gray\1$red\2$gray\3}
        {^(---- )(Should NOT have contained this substring)(:)$}                  {$gray\1$red\2$gray\3}
        {^(---- )(Memory leaks)(:)(.*)$}                        {$gray\1$red\2$gray\3$reset\4}
        {^(---- )(Changes)(:)(.*)$}                             {$gray\1$red\2$gray\3$reset\4}
        {^(==== )(\S+?)(-)?([\d\.]+)?( FAILED)$}                {$gray\1$reset\2\3$cyan\4$Red\5}
        # { test start }
        {^(---- )(.+?)(-)?([\d\.]+)?( start)$}                  {$gray\1$reset\2\3$cyan\4$reset\5}
        # { Found new/changed var/namespace/interp: .... }
        {^(Found (?:new|changed) [^\s:]+)(:)(.*)$}              {$yellow\1$gray\2$reset\3}
        {^(WARNING)(:)(.*)$}                                    {$yellow\1$gray\2$reset\3}
        # { test file name }
        {^[^\s:*+/\\]+\.test$}                                  {$blue\0}
        # { other messages }
        {^(---- )(.+)$}                                         {$gray\1$reset\2}
    }] {
        if { $_match eq "#" } continue
        append _replace {$reset}
        if { [string range $_match 0 7] eq {^(---- )} } {
            set _prefix {---- }
        } elseif { [string range $_match 0 7] eq {^(==== )} } {
            set _prefix {==== }
        } else {
            set _prefix {}
        }
        dict lappend colors4msg $_prefix $_match [string map $ansi_colors $_replace]
    }
    unset _match _replace

    if { $ex_memdebug && ![llength [info commands memory]] } {
        return -code error "memory leak detection mode has been set by the MEMDEBUG\
            environment variable, but the current Tcl interpreter is built without\
            the memory debugging feature"
    }

}

if { ![llength [info commands ::tcltest::test_original]] } {
    rename ::tcltest::test ::tcltest::test_original
}

proc ::tcltest::makeBinFile { contents name {directory ""} } {
    variable filesMade

    FillFilesExisted

    if {[llength [info level 0]] == 3} {
        set directory [temporaryDirectory]
    }

    set fullName [file join $directory $name]

    DebugPuts 3 "[lindex [info level 0] 0]: putting ``$contents'' into $fullName"

    set fd [open $fullName wb]
    if { [string length $contents] } {
        puts -nonewline $fd $contents
    }
    close $fd

    if { $fullName ni $filesMade } {
        lappend filesMade $fullName
    }
    return $fullName
}

proc ::tcltest::viewBinFile { name {directory ""} } {
    FillFilesExisted
    if {[llength [info level 0]] == 2} {
        set directory [temporaryDirectory]
    }
    set fullName [file join $directory $name]
    set f [open $fullName rb]
    set data [read $f]
    close $f
    return $data
}

proc ::tcltest::istty { } {
    if { $::tcl_platform(platform) ne "unix" } {
        return 0
    }
    if { [catch { exec /usr/bin/test -t 1 >@stdout } err] } {
        ::puts stderr "test err: $err"
        return 0
    }
    return 1
}

proc ::tcltest::SetupTest { code } {

    variable ex_memdebug
    variable ex_testname
    variable ex_info
    variable testLevel

    if { $ex_memdebug && $testLevel == 1 } {
        dict set ex_info $ex_testname interp_state [interp_state::create]
        set vars_before [uplevel 1 [list info vars]]
    }

    catch {
        uplevel 1 $code
    } res opts

    if { $ex_memdebug && $testLevel == 1 } {
        # See comment in tcltest::EvalTest about this hack with 'string range'.
        set opts [string range " $opts" 1 end]
        set res [string range " $res" 1 end]
        set varsMade [list]
        foreach varname [uplevel 1 [list info vars]] {
            if { [lsearch -exact $vars_before $varname] == -1 } {
                lappend varsMade $varname
            }
        }
        dict set ex_info $ex_testname varsMade $varsMade
    }

    return -options $opts $res

}

proc ::tcltest::EvalTest { code } {

    variable ex_memdebug
    variable testLevel
    variable ex_testname
    variable ex_info

    # This is a hack for counting empty lines at the beginning of test code.
    # It cuts out all space characters at the beginning of the code, and then
    # counts the newline characters in the cut part.
    dict set ex_info $ex_testname skip_new_line_count \
        [expr { [llength [split [lindex [regexp -inline {^\s+} "\n$code"] 0] \n]] - 2 }]

    catch {
        uplevel 1 $code
    } res opts

    if { $ex_memdebug && $testLevel == 1 } {

        # Do not mark objects created after the test has been executed as
        # related to the test.
        memory tag ""

        # After running the test, it is possible that the test result contains
        # objects that were created in the extension. We want to identify
        # all objects that were created in the extension to judge possible
        # memory leaks. However, these objects in interp's result are expected
        # and should not be considered as memory leaks.
        #
        # To avoid detecting such objects, we convert the interp's result to
        # a new string object created from scratch and not associated with
        # the extension. We will do this with the 'string range' command.
        set opts [string range " $opts" 1 end]
        set res [string range " $res" 1 end]

        # Make sure that this object is not assotiated with memory area from test
        set tmp [string range " [dict get $ex_info $ex_testname skip_new_line_count]" 1 end]
        dict unset ex_info $ex_testname skip_new_line_count
        dict set ex_info $ex_testname skip_new_line_count $tmp

    }

    return -options $opts $res

}

proc ::tcltest::CleanupTest { code } {

    variable ex_memdebug
    variable ex_info
    variable ex_testname
    variable filesMade
    variable testLevel

    set code [catch [list uplevel 1 $code] message]

    # If we got non ok/return from test's cleanup script, return as is
    # because that means there is something is wrong with test itself.
    if { $code ni {0 2} } {
        return -code $code $message
    }

    # We want to remove created files/directories after each test,
    # but not only at the end of all tests.
    foreach file $filesMade {
        if { [file exists $file] } {
            DebugDo 1 {Warn "cleanupTests deleting $file..."}
            if { [catch { file delete -force -- $file } errmsg] } {
                puts [outputChannel] "WARNING: could not delete file/dir '$file': $errmsg"
            }
            unset errmsg
        }
    }
    unset -nocomplain file
    set filesMade [list]

    if { [dict exists $ex_info $ex_testname varsMade] } {
        uplevel 1 [list unset -nocomplain {*}[dict get $ex_info $ex_testname varsMade]]
    }

    set code 0
    set message [list]
    set details [list]

    if { [dict exists $ex_info $ex_testname failedAsserts] } {

        lappend message "failed asserts: [llength [dict get $ex_info $ex_testname failedAsserts]]"

        lappend details {*}[dict get $ex_info $ex_testname failedAsserts]

        dict unset ex_info $ex_testname failedAsserts

    }

    if { $ex_memdebug && $testLevel == 1 } {

        # Commands enclosed in "catch" in cleanup code for a test can leave
        # references to objects created by the extension. I am not sure where they
        # are stored. Unsetting ::errorInfo and ::errorCode does not work,
        # and these references still remain in the interpreter.
        # It looks like the only stable option to remove all possible
        # references is to throw an error in script enclosed in
        # "catch" command.
        catch { unset not_existing_variable_here }

        # This code was here before the code to delete all files/directories
        # created during the test was added to this procedure.
        # Nevertheless, let this commented code be here to remind us that
        # the paths in the $filesMade variable should be cleared if we have
        # something there and we have a mode with memory leak tracking.
        #
        # # tcltest::makeFile stores the names of the created files in
        # # the filesMade variable. The filename may have been generated by
        # # the extension or may contain linked objects (such as a normalized path
        # # or internal file system representation) generated by the extension.
        # # To clear all references to objects created by the extension, we convert
        # # the filenames in this variable to normal strings.
        # if { [info exists filesMade] } {
        #     set result [list]
        #     foreach fn $filesMade {
        #         lappend result [string range " $fn" 1 end]
        #     }
        #     set filesMade $result
        #     # fn variable also contains a file name! release it now.
        #     unset -nocomplain fn
        # }

        set leaks [list]
        foreach msg [interp_state::compare [dict get $ex_info $ex_testname interp_state] [interp_state::create]] {
            lappend leaks $msg
        }

        if { [llength $leaks] } {
            lappend message "changes in the interpreter(s): [llength $leaks]"
            lappend details "---- Changes:\n\n[join $leaks \n]"
        }

        set leaks [list]
        set memdump [file join [temporaryDirectory] memdump]
        catch { file delete -force $memdump }
        memory active $memdump
        set fi [open $memdump r]
        set len [expr { [string length $ex_testname] + 1 }]
        while { [gets $fi line] != -1 } {
            if { [string match "* $ex_testname" $line] } {
                if { [string match "*/generic/tcl*.c * $ex_testname" $line] } continue
                if { [string match "*/unix/tcl*.c * $ex_testname" $line] } continue
                if { [string match "*/win/tcl*.c * $ex_testname" $line] } continue
                if { [string match "*/generic/regcomp.c * $ex_testname" $line] } continue
                if { [string match "*/generic/regc_*.c * $ex_testname" $line] } continue
                lappend leaks $line
            }
        }
        close $fi
        file delete $memdump

        if { [llength $leaks] } {
            lappend message "memory leaks: [llength $leaks]"
            lappend details "---- Memory leaks:\n\n[join $leaks \n]"
        }

    }

    if { [llength $message] } {
        set code 2
        set message "\nFound: [join $message {, }]\n"
        append message "\n" [join $details "\n\n"] "\n"
    }

    return -code $code $message

}

if { ![info exists ::tcltest::ex_cleanupTests_patched] } {

    set body {
        set ::tcltest::cleanupDone 1
    }

    append body "\n" [info body ::tcltest::cleanupTests]

    proc ::tcltest::cleanupTests { {calledFromAllFile 0} } $body
    unset body

    set ::tcltest::ex_cleanupTests_patched 1

}

if { ![info exists ::tcltest::ex_runAllTests_patched] } {

    set body [info body ::tcltest::runAllTests]

    if { [regsub -all {incr numTestFiles} $body {\0; unset -nocomplain ::tcltest::cleanupDone} body] != 2 } {
        return -code error "Failed to patch ::tcltest::runAllTests"
    }

    if { [regsub -all "\\\} msg\\\]\\\}" $body {
        if { ![info exists ::tcltest::cleanupDone] } {
            incr numTestFiles -1
            return -code error "cleanupTests was not executed in the test file"
        }
    \0} body] != 2 } {
        return -code error "Failed to patch ::tcltest::runAllTests"
    }

    if { [regsub -all "foreach index \\\{Total Passed Skipped Failed\\\} \\\{" $body {
        set ::tcltest::cleanupDone 1
    \0} body] != 1 } {
        return -code error "Failed to patch ::tcltest::runAllTests"
    }

    if { [regsub -all "cleanupTests 1" $body {
        if { !$numTests(Passed) } {
            puts [outputChannel] "Error: no tests have been passed"
            set failFilesSet 1
        }
    \0} body] != 1 } {
        return -code error "Failed to patch ::tcltest::runAllTests"
    }

    proc ::tcltest::runAllTests { {shell ""} } $body
    unset body

    set ::tcltest::ex_runAllTests_patched 1

}

proc ::tcltest::test { name description args } {

    variable ex_testname
    variable ex_testname_nested
    variable ex_info

    if { [info exists ex_testname_nested] && [lsearch -exact $ex_testname_nested $name] != -1 } {
        return -code error "Nested test with the same name '$name'"
    }

    # We rewrite arguments only for the new test format. Here we check
    # whether the 3rd argument starts with dash. The old test format can't
    # have the 3rd argument that starts with '-'.
    if { [string match "-*" [lindex $args 0]] } {
        set args_processed [list]
        set args_length [llength $args]
        for { set idx 0 } { $idx < $args_length } { incr idx } {
            set arg [lindex $args $idx]
            switch -exact -- $arg {
                -setup - -cleanup - -body - -result - -returnCodes - \
                -errorCode - -match - -output - -errorOutput - -constraints {
                    if { $idx == $args_length } {
                        return -code error "no value is specified for the $arg option"
                    }
                    lappend args_processed $arg [lindex $args [incr idx]]
                }
                -ok {
                    lappend args_processed -returnCodes {0 2} -match glob -result *
                }
                -error {
                    if { $idx == $args_length } {
                        return -code error "no value is specified for the $arg option"
                    }
                    lappend args_processed -returnCodes 1 -result [lindex $args [incr idx]]
                }
                default {
                    return -code error "unknown option '$arg'"
                }
            }
        }
        set args $args_processed
    }

    set ex_testname $name
    lappend ex_testname_nested $name
    dict set ex_info $name [dict create]

    catch [list uplevel 1 [list ::tcltest::test_original $name $description {*}$args]] res opts

    if { [llength $ex_testname_nested] == 1 } {
        unset ex_testname
        unset ex_testname_nested
    } else {
        set ex_testname_nested [lreplace $ex_testname_nested end end]
        set ex_testname [lindex $ex_testname_nested end]
    }

    dict unset ex_info $name

    return -options $opts $res

}

# If the "-singleproc true" parameter is not set, tcltest executes each test,
# spawning another interpreter process. It is done by the "open" command.
# However, when the interpreter is spawned by the open command, any message
# from a child process on stderr is considered an error. That's not what we
# want. Any test or process under test can throw something into stderr,
# and it should not necessarily be considered an error. For example,
# these could be debugging messages.
#
# To avoid this behavior, we will rewrite the open command in the ::tcltest
# namespace so that it is used by tcltest procedures. In the rewritten version,
# we add 2>@stderr to the open argument so that stderr from the child process
# is redirected to stderr from the parent process, and will not cause
# an unexpected error when processing the test file.

proc ::tcltest::open { args } {
    set arg [lindex $args 0]
    if { [string index $arg 0] eq "|" } {
        append arg " 2>@stderr"
        set args [lreplace $args 0 0 $arg]
    }
    tailcall ::open {*}$args
}


proc ::tcltest::puts_line { msg } {

    variable colors4msg

    set found 0

    dict for { prefix mappings } $colors4msg {

        if { [set prefix_len [string length $prefix]] } {
            if { [string range $msg 0 $prefix_len-1] ne $prefix } {
                continue
            }
        }

        foreach { match replace } $mappings {
            if { [regsub $match $msg $replace msg] } {
                set found 1
                break
            }
        }

        if { $found } break

    }

    tailcall ::puts stdout $msg

}

proc ::tcltest::puts { args } {

    variable Option
    variable puts_code_now
    variable puts_buffer
    variable ansi_colors
    variable ex_testname
    variable ex_info

    if { $Option(-colors) == -1 } {
        set Option(-colors) [istty]
    }

    if { [llength $args] == 3 && [lindex $args 0] eq "-nonewline" && [lindex $args 1] eq "stdout" } {
        append puts_buffer [lindex $args 2]
        return
    }

    if { [llength $args] != 2 || [lindex $args 0] ne "stdout" } {
        tailcall ::puts {*}$args
    }

    if { [info exists puts_buffer] } {
        set msg $puts_buffer
        unset puts_buffer
    }
    append msg [lindex $args 1]

    # Special handling for test result line:
    #
    #     all.tcl:        Total   96      Passed  5       Skipped 91      Failed  0
    #
    # But we should be careful with that line. If we output it from child
    # test process, then we should not add any colors to it as it will be
    # parsed by parent test process.
    #
    if { $Option(-colors) && [lindex [info level -1] 0] eq "cleanupTests" } {
        # Handle the test result only from the cleanupTests proc
        if { ![catch [list uplevel 1 [list set calledFromAllFile]] calledFromAllFile] } {
            if { $calledFromAllFile } {
                if { [regexp \
                    {^([^:]+):\tTotal\t([0-9]+)\tPassed\t([0-9]+)\tSkipped\t([0-9]+)\tFailed\t([0-9]+)$} \
                    $msg -> file total passed skipped failed \
                ] } {
                    if { $total == 0 } {
                        set format_total "\t\$redTotal\t%s\$reset"
                    } else {
                        set format_total "\tTotal\t%s"
                    }
                    set total [format [string map $ansi_colors $format_total] $total]
                    if { $failed == 0 } {
                        if { $passed == 0 } {
                            set format_passed "\t\$redPassed\t%s\$reset"
                            set format_failed "\tFailed\t%s"
                        } else {
                            set format_passed "\t\$greenPassed\t%s\$reset"
                            set format_failed "\t\$greenFailed\t%s\$reset"
                        }
                    } else {
                        set format_passed "\tPassed\t%s"
                        set format_failed "\t\$redFailed\t%s\$reset"
                    }
                    set passed [format [string map $ansi_colors $format_passed] $passed]
                    set failed [format [string map $ansi_colors $format_failed] $failed]
                    set format "\tSkipped\t%s"
                    set skipped [format [string map $ansi_colors $format] $skipped]
                    set format "\$blue%s\$gray:\$reset%s%s%s%s"
                    set msg [format [string map $ansi_colors $format] $file $total $passed $skipped $failed]
                    tailcall puts_line $msg
                }
            }
        }
    }

    if { [info exists puts_code_now] } {

        # Add line numbers to test code

        set line_number 0
        set processed [list]

        foreach line [split $msg \n] {

            if { !$line_number && [string trim $line] ne "" } {
                set line_number 1
            }

            if { $line_number } {
                if { [string trim $line] ne "" } {
                    if { [string index [string trimleft $line] 0] eq "#" } {
                        set format {$cyan%2d $gray%s$reset}
                    } elseif {
                        [dict exists $ex_info $ex_testname failedAssertLines] &&
                        [lsearch -exact [dict get $ex_info $ex_testname failedAssertLines] $line_number] != -1
                    } {
                        set format {$cyan%2d$Red>$red%s$reset}
                    } else {
                        set format {$cyan%2d $reset%s}
                    }
                    set format [string map $ansi_colors $format]
                    set line [format $format $line_number $line]
                }
                incr line_number
            }

            lappend processed $line

        }

        set msg [join $processed \n]

        unset puts_code_now

    } elseif { $msg eq "==== Contents of test case:" } {
        set puts_code_now 1
    }

    if { !$Option(-colors) || $msg eq "" } {
        tailcall ::puts stdout $msg
    }

    foreach msg [split $msg \n] {
        puts_line $msg
    }

}

proc ::tcltest::GenerateAssertMessage { args } {

    variable ex_testname
    variable ex_info

    set frame [info frame -2]

    set line [dict get $frame line]
    set line [expr { $line - [dict get $ex_info $ex_testname skip_new_line_count] }]

    if { [dict exists $ex_info $ex_testname failedAssertLines] } {
        dict set ex_info $ex_testname failedAssertLines \
            [concat [dict get $ex_info $ex_testname failedAssertLines] $line]
    } else {
        dict set ex_info $ex_testname failedAssertLines $line
    }

    set msg [list]
    lappend msg "==== Assertion failed on line $line:"
    lappend msg ""
    lappend msg "    [dict get $frame cmd]"
    lappend msg ""
    foreach { head body } $args {
        if { [string index $head end] eq "&" } {
            set head [string range $head 0 end-1]
            if { $body ne "" } {
                append head " " $body
            }
            lappend msg "---- $head"
        } else {
            lappend msg "---- $head" $body
        }
    }
    lappend msg "----  "

    set msg [list [join $msg \n]]

    if { [dict exists $ex_info $ex_testname failedAsserts] } {
        set msg [concat [dict get $ex_info $ex_testname failedAsserts] $msg]
    }
    dict set ex_info $ex_testname failedAsserts $msg

    return $msg

}

proc ::tcltest::IsNumber { data } {
    return [expr { ![catch { expr { $data > 0 } }] }]
}

proc ::tcltest::MayBeNumber { data } {
    if { [catch { expr [string map [list "\\" "\\\\" "\[" "\\\[" "\]" "\\\]"] $data] } result] } {
        return -code error $data
    }
    return $result
}

proc ::tcltest::BinaryToPrint { data } {
    set result ""
    set count 0
    foreach c [split [encoding convertto iso8859-1 $data] ""] {
        if { $count } {
            if { $count % 32 == 0 } {
                append result "\n"
            } elseif { $count % 16 == 0 } {
                append result "   "
            } elseif { $count % 8 == 0 } {
                append result " "
            }
        }
        append result [format %02X [scan $c %c]]
        incr count
    }
    return $result
}

proc ::tcltest::assertEq { actual expected { description {} } } {
    if { $actual eq $expected } {
        return $actual
    }
    GenerateAssertMessage \
        "Value from test was:" $actual \
        "Expected value was:" $expected
}

proc ::tcltest::assertNe { actual expected { description {} } } {
    if { $actual ne $expected } {
        return $actual
    }
    GenerateAssertMessage \
        "Value from test was:" $actual \
        "Should have been equal to this value:" $expected
}

proc ::tcltest::assertBinEq { actual expected { description {} } } {
    if { $actual eq $expected } {
        return $actual
    }
    GenerateAssertMessage \
        "Value from test was:" [BinaryToPrint $actual] \
        "Expected value was:" [BinaryToPrint $expected]
}

proc ::tcltest::assertBinNe { actual expected { description {} } } {
    if { $actual ne $expected } {
        return $actual
    }
    GenerateAssertMessage \
        "Value from test was:" [BinaryToPrint $actual] \
        "Should have been equal to this value:" [BinaryToPrint $expected]
}

proc ::tcltest::assertLt { actual base { description {} } } {
    if { [catch { MayBeNumber $base } base] } {
        GenerateAssertMessage \
            "Not a number value in assertion statement:" $base
    } elseif { ![IsNumber $actual] } {
        GenerateAssertMessage \
            "Value from test is not a number:" $base
    } elseif { [expr { $actual < $base }] } {
        return $actual
    } else {
        GenerateAssertMessage \
            "Value from test was:" $actual \
            "Should have been less than this value:&" $base
    }
}

proc ::tcltest::assertLe { actual base { description {} } } {
    if { [catch { MayBeNumber $base } base] } {
        GenerateAssertMessage \
            "Not a number value in assertion statement:" $base
    } elseif { ![IsNumber $actual] } {
        GenerateAssertMessage \
            "Value from test is not a number:" $base
    } elseif { [expr { $actual <= $base }] } {
        return $actual
    } else {
        GenerateAssertMessage \
            "Value from test was:" $actual \
            "Should have been less than or equal to this value:&" $base
    }
}

proc ::tcltest::assertGt { actual base { description {} } } {
    if { [catch { MayBeNumber $base } base] } {
        GenerateAssertMessage \
            "Not a number value in assertion statement:" $base
    } elseif { ![IsNumber $actual] } {
        GenerateAssertMessage \
            "Value from test is not a number:" $base
    } elseif { [expr { $actual > $base }] } {
        return $actual
    } else {
        GenerateAssertMessage \
            "Value from test was:" $actual \
            "Should have been greater than this value:&" $base
    }
}

proc ::tcltest::assertGe { actual base { description {} } } {
    if { [catch { MayBeNumber $base } base] } {
        GenerateAssertMessage \
            "Not a number value in assertion statement:" $base
    } elseif { ![IsNumber $actual] } {
        GenerateAssertMessage \
            "Value from test is not a number:" $base
    } elseif { [expr { $actual >= $base }] } {
        return $actual
    } else {
        GenerateAssertMessage \
            "Value from test was:" $actual \
            "Should have been greater than or equal to this value:&" $base
    }
}

proc ::tcltest::assertBetween { actual base1 base2 { description {} } } {
    if { [catch { MayBeNumber $base1 } base1] } {
        GenerateAssertMessage \
            "Not a number value in the 1st base of assertion statement:" $base1
    } elseif { [catch { MayBeNumber $base2 } base2] } {
        GenerateAssertMessage \
            "Not a number value in the 2nd base of assertion statement:" $base2
    } elseif { ![IsNumber $actual] } {
        GenerateAssertMessage \
            "Value from test is not a number:" $base
    } elseif { [expr { $actual >= $base1 && $actual <= $base2 }] } {
        return $actual
    } else {
        GenerateAssertMessage \
            "Value from test was:" $actual \
            "Should have been greater than or equal to $base1, and less than or equal to $base2&" ""
    }
}

proc ::tcltest::assertTrue { actual { description {} } } {
    if { [string is true -strict $actual] } {
        return $actual
    }
    GenerateAssertMessage \
        "Value from test was:" $actual
}

proc ::tcltest::assertFalse { actual { description {} } } {
    if { [string is false -strict $actual] } {
        return $actual
    }
    GenerateAssertMessage \
        "Value from test was:" $actual
}

proc ::tcltest::assertErr { script { description {} } } {
    set code [catch [list uplevel 1 $script] err]
    if { $code == 1 } {
        return 1
    }
    GenerateAssertMessage \
        "Specified code did not return an error:" $script \
        "Returned code:&" $code \
        "Returned message:" $err
}

proc ::tcltest::assertErrMsg { script message { description {} } } {
    set code [catch [list uplevel 1 $script] err]
    if { $code != 1 } {
        GenerateAssertMessage \
            "Specified code did not return an error:" $script \
            "Returned code:&" $code \
            "Returned message:" $err
    } elseif { $err ne $message } {
        GenerateAssertMessage \
            "Specified code did not return the expected error message:" $script \
            "Error message from test was:" $err \
            "Expected error message was:" $message
    } else {
        return 1
    }
}

proc ::tcltest::assertErrMsgMatch { script pattern { description {} } } {
    set code [catch [list uplevel 1 $script] err]
    if { $code != 1 } {
        GenerateAssertMessage \
            "Specified code did not return an error:" $script \
            "Returned code:&" $code \
            "Returned message:" $err
    } elseif { ![string match $pattern $err] } {
        GenerateAssertMessage \
            "Specified code did not return the expected error message:" $script \
            "Error message from test was:" $err \
            "Should have matched (case-sensitive) glob pattern:" $pattern
    } else {
        return 1
    }
}

proc ::tcltest::assertMatch { actual pattern { description {} } } {
    if { [string match $pattern $actual] } {
        return $actual
    }
    GenerateAssertMessage \
        "Value from test was:" $actual \
        "Should have matched (case-sensitive) glob pattern:" $pattern
}

proc ::tcltest::assertMatchAny { actual patterns { description {} } } {
    foreach pattern $patterns {
        if { [string match $pattern $actual] } {
            return $actual
        }
    }
    GenerateAssertMessage \
        "Value from test was:" $actual \
        "Should have matched (case-sensitive) glob pattern:" [lindex $patterns 0] \
        {*}[join [lmap x [lrange $patterns 1 end] {
            list "or should have matched (case-sensitive) glob pattern:" $x
        }]]
}

proc ::tcltest::assertNotMatch { actual pattern { description {} } } {
    if { ![string match $pattern $actual] } {
        return $actual
    }
    GenerateAssertMessage \
        "Value from test was:" $actual \
        "Should NOT have matched (case-sensitive) glob pattern:" $pattern
}

proc ::tcltest::assertIMatch { actual pattern { description {} } } {
    if { [string match -nocase $pattern $actual] } {
        return $actual
    }
    GenerateAssertMessage \
        "Value from test was:" $actual \
        "Should have matched (case-insensitive) glob pattern:" $pattern
}

proc ::tcltest::assertNotIMatch { actual pattern { description {} } } {
    if { ![string match -nocase $pattern $actual] } {
        return $actual
    }
    GenerateAssertMessage \
        "Value from test was:" $actual \
        "Should NOT have matched (case-insensitive) glob pattern:" $pattern
}

proc ::tcltest::assertRegexp { actual pattern { description {} } } {
    if { [catch { regexp -about $pattern } err] } {
        GenerateAssertMessage \
            "Wrong regexp in assertion statement:" $pattern \
            "Regexp compile error:" $err
    } elseif { [regexp $pattern $actual] } {
        return $actual
    } else {
        GenerateAssertMessage \
            "Value from test was:" $actual \
            "Should have been matched regexp pattern:" $pattern
    }
}

proc ::tcltest::assertNotRegexp { actual pattern { description {} } } {
    if { [catch { regexp -about $pattern } err] } {
        GenerateAssertMessage \
            "Wrong regexp in assertion statement:" $pattern \
            "Regexp compile error:" $err
    } elseif { ![regexp $pattern $actual] } {
        return $actual
    } else {
        GenerateAssertMessage \
            "Value from test was:" $actual \
            "Should NOT have been matched regexp pattern:" $pattern
    }
}

proc ::tcltest::assertContain { haystack needle { description {} } } {
    if { [string first $needle $haystack] != -1 } {
        return $haystack
    }
    GenerateAssertMessage \
        "Value from test was:" $actual \
        "Should have contained this substring:" $needle
}

proc ::tcltest::assertNotContain { haystack needle { description {} } } {
    if { [string first $needle $haystack] == -1 } {
        return $haystack
    }
    GenerateAssertMessage \
        "Value from test was:" $actual \
        "Should NOT have contained this substring:" $needle
}

namespace eval ::tcltest::interp_state {

    proc snapshot_vars { interp ns } {
        set result [dict create]
        foreach var [interp eval $interp [list info vars "${ns}::*"]] {
            if { $var in { ::errorCode ::errorInfo ::tcl::auto_oldpath ::auto_index } } continue
            if { [interp eval $interp [list array exists $var]] } {
                dict set result $var [interp eval $interp [list array get $var]]
            } else {
                if { [catch { interp eval $interp [list set $var] } data] } {
                    # This is the case where a variable is defined in the namespace
                    # but does not yet exist.
                    continue
                }
                dict set result $var $data
            }
        }
        return $result
    }

    proc snapshot_procs { interp ns } {
        return [interp eval $interp [list info commands "${ns}::*"]]
    }

    proc snapshot_chans { interp } {
        return [interp eval $interp [list chan names]]
    }

    proc snapshot_namespace { interp ns } {
        set result [dict create]
        dict set result vars [snapshot_vars $interp $ns]
        dict set result procs [snapshot_procs $interp $ns]
        return $result
    }

    proc get_namespaces { interp parent } {
        set result [list $parent]
        foreach ns [interp eval $interp [list uplevel #0 [list namespace children $parent]]] {
            lappend result {*}[get_namespaces $interp $ns]
        }
        return $result
    }

    proc snapshot_interp { interp } {
        set result [dict create]
        dict set result chans [snapshot_chans $interp]
        foreach ns [get_namespaces $interp ""] {
            if { $ns eq "::tcltest" } {
                continue
            }
            dict set result namespaces $ns [snapshot_namespace $interp $ns]
        }
        return $result
    }

    proc create {} {
        set result [dict create]
        foreach interp [list {} {*}[interp slaves]] {
            dict set result $interp [snapshot_interp $interp]
        }
        return $result
    }

    proc compare { old new } {

        set result [list]

        dict for { interp data } $new {
            if { $interp eq "" } {
                set iprefix ""
            } else {
                set iprefix "\[interp $interp\] "
            }
            if { ![dict exists $old $interp] } {
                lappend result "Found new interp: \"$interp\""
                continue
            }
            foreach chan [dict get $data chans] {
                if { [lsearch -exact [dict get $old $interp chans] $chan] == -1 } {
                    lappend result "Found new channel: $iprefix\"$chan\""
                }
            }
            dict for { ns data } [dict get $data namespaces] {
                if { ![dict exists $old $interp namespaces $ns] } {
                    lappend result "Found new namespace: $iprefix\"$ns\""
                    continue
                }
                foreach proc [dict get $data procs] {
                    if { [lsearch -exact [dict get $old $interp namespaces $ns procs] $proc] == -1 } {
                        lappend result "Found new proc: $iprefix\"$proc\""
                    }
                }
                dict for { var data } [dict get $data vars] {
                    if { ![dict exists $old $interp namespaces $ns vars $var] } {
                        lappend result "Found new var: $iprefix\"$var\""
                    } elseif { [dict get $old $interp namespaces $ns vars $var] ne $data } {
                        lappend result "Found changed var: $iprefix\"$var\""
                    }
                }
            }
        }

        return $result

    }

}

package provide tcltestex 1.0.0

