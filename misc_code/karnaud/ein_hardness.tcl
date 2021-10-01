proc ein_hardness {args} {
# Computes Einstein hardness for current spectrum assuming that the rsp file
# is ipc_90jun07_16ch.rsp
# A. Ptak, 7/98
    global xs_return_result

    set hold_xs_ret_res $xs_return_result

    not 1-16
    ig 1-2 6-16
    set s [show rate]
    not 6-11
    ig 3-5
    set h [show rate]
    not 1-16
    set xs_return_result $hold_xs_ret_res

    return [expr ($h - $s)/($h + $s)]
}
