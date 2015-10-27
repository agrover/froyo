On-disk format for Froyo

Froyo takes 1MiB (2048 512-byte sectors) from the start and end of each
base block device for its configuration.

The zones are identical (lengths in 512-byte sectors):

offset  length  description
0       1       signature block
1       7       unused
8       1020    metadata area A (MDAA)
1028    1020    metadata area B (MDAB)


The format of the signature block is (lengths in bytes):

All values little-endian.

offset  length  description
0       4       CRC32 of signature block (bytes at offset 4 len 508)
4       16      Froyo signature "!IamFroy0\x86\xffGO\x02^\x41"
20      8       Device size in 512-byte sectors (u64)
28      4       flags
32      32      Hex UUID for the block device
64      8       MDAA timestamp (u64)
72      4       MDAA serial (u32)
76      4       MDAA used length in bytes (u32)
80      4       MDAA CRC32
84      12      unused
96      8       MDAB timestamp (u64)
104     4       MDAB serial (u32)
108     4       MDAB used length in bytes (u32)
112     4       MDAB CRC32
116     12      unused
128     384     unused

All "flags" or "unused" ranges are zeroed.

Data within the metadata areas is stored in JSON format.

Froyo uses a hybrid of the models used by LVM and ZFS
metadata. Metadata updates write to the older of the MDAA and MDAB
areas. This is determined by lowest timestamp, and then lowest serial
if timestamps are equal.

First, the JSON metadata is written to either MDAA or MDAB, as
determined above, in both zones.

Second, the chosen MDA's timestamp, serial, length, and CRC fields are
updated: timestamp gets the UNIX timestamp, serial is incremented,
length is set to the length of the metadata, and the CRC32 is
calculated on the data up to that length.

Third, the sig block's CRC32 is calculated.

Finally, the updated sig block is written to the start and end zones,
in that order. Now would be a good time for a flush/FUA.
