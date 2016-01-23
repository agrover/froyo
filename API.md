## Froyo API

Froyo has a DBus API. The Froyo command-line program uses it, and
other clients can also use it, both to get information and to
configure new or existing Froyo devices.

The Froyo service registers the name `org.freedesktop.Froyo1` on the
system's DBus System bus.

### Root Object path

`/org/freedesktop/froyo`

##### Method: `Create`

In Args: `Name`(string), `Blockdevs`(array(string)), `Force`(bool)

Out Args: `FroyodevPath`(string)

Create a Froyodev from the given blockdevs. Froyo will refuse to
create the device if it thinks data is present on any of the block
devices, unless Force is true. (Force will not override other
creation errors.)
Clients should not set `Force` to `true` without getting a secondary
confirmation of intent that Froyo can overwrite the contents of the
block device.

Returns the object path of the newly created Froyodev.

### Froyodev paths

`/org/freedesktop/froyo/<uuid>`

Each Froyodev present on the system will have an object here based on
its uuid. These can be enumerated using the DBus `ObjectManager` API.
Property changes will cause `PropertiesChanged` signals except where
noted.

##### RO Property: `Name` (string)

The friendly name of the Froyodev.

##### Method: `SetName`

In Args: `NewName`(string)

Change the friendly name of the Froyo device.

##### RO Property: `RemainingSectors` (u64)
##### RO Property: `TotalSectors` (u64)

The remaining and total 512-byte sectors that the
Froyodev currently has available for user data. Both of these numbers
may change, as more or less data is stored on the Froyodev, and also
increase or decrease if block devices are added or removed from the
Froyodev.

Due to frequently changing, these properties do not emit
`PropertyChanged` signals. Clients looking to track almost-full
conditions should track signals from the Status property and look for
write-throttling, as shown by bit 11 of `RunningStatus`.

##### RO Property: `Status` (u32)
##### RO Property: `RunningStatus` (u32)

`Status` of 0 indicates the Froyodev is successfully online. Bits
set in Status indicate the Froyodev has failed to start due to an
issue with a particular area:

| Bit | Description
|-----|----------------
|0-7  |Block device-level failure. Too few valid block devices that make up the Froyodev are present. This `u8` indicates how many more devices are needed to start the Froyodev in non-redundant condition.
|8    |Raid Failure. The redundancy layer has failed.
|9    |ThinPool failure - Meta. Something is wrong with the metadata device or its data.
|10   |ThinPool failure - Data. Something is wrong with the thinpool data device.
|11   |Thin volume failure. An error occurred when creating the thin volume.
|12   |Filesystem failure. The filesystem has experienced a failure.
|13-31|Reserved or unenumerated failure.

`RunningStatus` is also returned, but will only be valid if
`Status` is 0 -- that is, if the Froyodev is started. If
`RunningStatus` is 0, the Froyodev's operation is normal. Bits set
in `RunningStatus` indicate the device is running non-optimally in
some way:

| Bit | Description
|-----|----------------
|0-7  |Missing Block devices. This `u8` indicates how many more devices are needed for full operation.
|8    |Non-redundant. This will likely be set if the above field is nonzero, but may not be if the Froyodev had a redundancy of 2 or greater to start.
|9    |Cannot reshape. The Froyodev is non-redundant and does not have enough free space to re-establish redundancy without additional resources. See the `Reshape` command.
|10   |Reshaping. The Froyodev is currently reshaping. Read and write performance may be affected.
|11   |Throttled. The Froyodev's write speed has been throttled to avoid running out of space.
|12-31|Reserved or unenumerated issue that does not prevent operation.

##### RO Property: `BlockDevices`

`Array(string, u32)`

The paths of the block devices making up the Froyodev. the `u32` indicates
each block device's status:

| Value | Description
|-------|----------------
|0      | Good, in use
|1      | Good, not in use
|2      | Bad
|3      | Not present

##### Method: `AddBlockDevice`

In Args: `BlockDevicePath`(string), `Force`(bool)

Adds the given block device to the Froyodev. The `Force` parameter's
behavior is similar to its use in the `Create` method.

##### Method: `RemoveBlockDevice`

In Args: `BlockDevicePath`(string)

Removes the given block device from the Froyodev. If the block device
was in-use, this will cause the Froyodev to enter either non-redundant
or a failed mode, depending on whether the Froyodev was previously
already non-redundant or not.

##### Method: `Reshape`

No In or Out arguments

The Froyodev will reconfigure itself in the background to operate
redundantly with the block devices it currently has available. This
will likely fail if bit 9 (`Cannot Reshape`) in the `Status`
property's `RunningStatus` field is set. Reshape operation will
begin immediately and will impact the performance of other I/O
operations to the Froyodev.

After reshape, all bad or not present block devices are no longer
tracked as part of the Froyodev.
