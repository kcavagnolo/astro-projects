proc get_xspec_home {args} {
    global env
    if {[lsearch [array names env] XSPEC_HOME] != -1} {
	set home $env(XSPEC_HOME)
    } else {
	set home "$env(HOME)/.xspec"
    }
    return $home
}
