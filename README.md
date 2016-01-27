# tompebar
An enhancement for the bspwm desktop environment. Divides the desktop list into workspaces for better multitasking.

In bspwm, a monitor is associated with a list of desktops, each one containing windows.
This tool provides an ability to group desktops into workspaces and simplifies the navigation between them.

The utility, similarly to bspwm, uses two input sources:
* output of "bspc control --subscribe"
* a dedicated socket for user commands.

# Usage
`$ bspc control --subscribe | tompebar "bar>" | lemonbar`

Read the bspc output from stdin and start printing the environment state to stdout (to lemonbar), prefixed with "bar>".

# Control
**WARNING:** The current version will crash if you try to manage (add/rename/delete) desktops via direct `bspc` calls during the work of the tool. Support for multiple monitors is not tested.

The utility called `tbctl` is provided to manipulate the state of workspaces. It is called as follows:
```
$ tbctl --add-desktop <name>
$ tbctl --add-workspace <name>
$ tbctl --switch-desktop [prev|next|<number>]
$ tbctl --switch-workspace [prev|next|<number>]
$ tbctl --rename-desktop <name>                 # Renames the current desktop
$ tbctl --rename-workspace <name>
$ tbctl --remove-desktop                        # (and the workspace, if it becomes empty)
```
