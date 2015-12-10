## On-disk format for Froyo

Froyo takes 1MiB (2048 512-byte sectors) from the start and end of each
base block device for its configuration.

The zones are identical (lengths in 512-byte sectors):

| offset | length | description
|--------|--------|-------
|0       |1       |signature block
|1       |7       |unused
|8       |1020    |metadata area A (MDAA)
|1028    |1020    |metadata area B (MDAB)


The format of the signature block is (lengths in bytes):

All values little-endian.

|offset  |length  |description
|--------|--------|-----------
|0       |4       |CRC32 of signature block (bytes at offset 4 len 508)
|4       |16      |Froyo signature ```!IamFroy0\x86\xffGO\x02^\x41```
|20      |8       |Device size in 512-byte sectors (u64)
|28      |4       |flags
|32      |32      |Hex UUID for the block device
|64      |8       |MDAA timestamp (u64)
|72      |4       |MDAA serial (u32)
|76      |4       |MDAA used length in bytes (u32)
|80      |4       |MDAA CRC32
|84      |12      |unused
|96      |8       |MDAB timestamp (u64)
|104     |4       |MDAB serial (u32)
|108     |4       |MDAB used length in bytes (u32)
|112     |4       |MDAB CRC32
|116     |12      |unused
|128     |32      |Hex UUID for the associated Froyodev
|160     |352     |unused

All "flags" or "unused" ranges are zeroed.

Data within the metadata areas is stored in JSON format.

Metadata updates write to the older of the MDAA and MDAB areas. This
is determined by lowest timestamp, and then lowest serial if
timestamps are equal.

First, the JSON metadata is written to either MDAA or MDAB, as
determined above, in both zones.

Second, the chosen MDA's timestamp, serial, length, and CRC fields are
updated: timestamp gets the UNIX timestamp. If the other MDA has the
same timestamp, serial is one more than the other MDA's
serial. Otherwise serial is zero.  Length is set to the length of the
metadata, and the CRC32 is calculated on the data up to that length.

Third, the sig block's CRC32 is calculated.

Finally, the updated sig block is written to the start and end zones,
in that order. Now would be a good time for a flush/FUA.

### JSON Metadata

Froyo is implemented using layers of devicemapper devices:

|Layer  |Description
|-------|-----------
|0      | Block devices given to Froyo to use
|1      | linear targets that divide blockdev into one or more pairs of raid meta and data devices
|2      | raid5 targets that build redundant storage on top of layer 1
|3      | two linear targets for thin-meta and thin-data, and the thin-pool that uses them (actually two layers)
|4      | Thin volumes allocated out of the thin pool

Additional target layers may be used, but info on these layers is sufficient to configure the Froyodev for use.

##### example json

```json
{
"name": "foo",
"l0_devs" : [
  {"id":"asdasd",
	    "major":322,
	    "minor":12},
	{"id":"3de56",
	    "major":322,
	    "minor":45},
	{"id":"f00f1234",
	    "major":4,
	    "minor":22}],
"l1_devs" : [
  {"name": "qweqwe",
   "from": "asdasd",
	 "raid_meta":[{"offset":213,
	 "length":16}],
	 "raid_data":[{"offset":400,
	 "length":9000}]}],
"l2_devs": [{"name":"ertert",
	"region_size": 8192,
	"stripe_size": 2048,
	"type": "raid5_ls",
	"from":["qweqwe", "2nd disk", "etc"]}],
"l3_dev": {
  "meta":[
     {"from":"ertert",
	    "offset":0,
	    "len":200}],
	"data":[
	   {"from":"ertert",
	    "offset":200,
	    "len":1000}],
	 "block_size":8192},
"l4_devs": [
    {"name":"5d3e2",
	  "thin_id":0,
	  "fs":"xfs",
	  "size":123123141241241}]
}

```
