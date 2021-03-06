[![Build Status](https://travis-ci.org/agrover/froyo.svg?branch=master)](https://travis-ci.org/agrover/froyo)

# Froyo

Semi-automated bulk storage management

You give Froyo block devices (hard drives) and it gives you a single
redundant filesystem. You may add any size drive to expand it. When a
drive goes bad, your data is automatically reshaped in the background
across the remaining drives (if space allows). You can replace older,
smaller drives with bigger new ones while the filesystem remains online.

Drobo + Free + You = Froyo!

### Dev Resources

Use GitHub for issue reporting and Pull Requests.

IRC: `#froyo` on irc.freenode.org

Mailing list: `froyo@lists.fedorahosted.org`

## Trying it out (Caution highly recommended, may eat data!)

1. Compile Froyo
1. In one terminal, run `froyo -d dev dbus_server`.
1. In another terminal, use other commands, such as `froyo create`, `froyo list`,
   `froyo status <froyodevname>`, `froyo add <newblockdev>` and `froyo remove
   <existingblockdev>`. To mount and use froyodevs, mount block devices in `/dev/froyo`.

### Things that work

1. Create a froyodev
1. Destroy a froyodev
1. Deactivate a froyodev
1. Rename a froyodev
1. Add another drive to an existing froyodev
1. Remove a drive from an existing froyodev
1. Extending thin device when threshold is hit
1. List froyodevs and get status on a froyodev

### Things that are implemented but don't work

1. Reshape smaller to re-establish redundancy after a disk is removed

### Things that don't yet work

1. Reshape bigger to use new disks
1. Extending filesystem when it nears capacity
1. Slowing writes to avoid running out of thin data blocks
1. Automatically starting Froyo service
1. Automatically checking layers for errors when setting up
