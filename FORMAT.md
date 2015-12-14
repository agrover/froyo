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
|64      |8       |MDAA UNIX timestamp (seconds since Jan 1 1970) (u64)
|72      |4       |MDAA nanoseconds (u32)
|76      |4       |MDAA used length in bytes (u32)
|80      |4       |MDAA CRC32
|84      |12      |unused
|96      |8       |MDAB UNIX timestamp (seconds since Jan 1 1970) (u64)
|104     |4       |MDAB nanoseconds (u32)
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

Second, the chosen MDA's timestamp, nanosecond, length, and CRC fields
are updated: timestamp gets the UNIX timestamp. Nanoseconds is the
current nanoseconds within the second. Length is set to the length of
the metadata, and the CRC32 is calculated on the data up to that
length.

(Note: The same timestamp, nanosecond, length, and CRC MDA values must
be used when writing the same metadata to all blockdevs in a
froyodev. Other fields within the sig block may vary, as may which
MDA slot is being updated, if not all blockdevs were added at the same
time.)

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

Plus additional temporary devices as needed

##### Example JSON

```json
{
  "name": "froyodev-1",
  "id": "3e0e1e09061e4cae8248c24576e4d931",
  "block_devs": {
    "3f85352dea0346fbb2ea846abba66f0f": {
      "path": "/dev/vdb",
      "sectors": 16777216
    },
    "968287b70dca479682b63c4dfe5a5948": {
      "path": "/dev/vdc",
      "sectors": 16777216
    },
    "c667fab02599442e9a0de7bc0309530b": {
      "path": "/dev/vde",
      "sectors": 25165824
    },
    "e07d78df365c4590bb5c790c487a24a1": {
      "path": "/dev/vdd",
      "sectors": 23068672
    }
  },
  "raid_devs": [
    {
      "id": "1400b1e499e64e3fac1206dbb45fc76b",
      "stripe_sectors": 2048,
      "region_sectors": 8192,
      "length": 50313216,
      "members": [
        {
          "id": "bc00f415fc3d483090c6c4e75ff8aa5f",
          "meta_segments": [
            {
              "start": 2048,
              "length": 32
            }
          ],
          "data_segments": [
            {
              "start": 2080,
              "length": 16771072
            }
          ],
          "parent": "3f85352dea0346fbb2ea846abba66f0f"
        },
        {
          "id": "4f811dc3bb3b4755b0c54514ecbff790",
          "meta_segments": [
            {
              "start": 2048,
              "length": 32
            }
          ],
          "data_segments": [
            {
              "start": 2080,
              "length": 16771072
            }
          ],
          "parent": "968287b70dca479682b63c4dfe5a5948"
        },
        {
          "id": "505c1d7af6434f968ec5a6881c48a252",
          "meta_segments": [
            {
              "start": 2048,
              "length": 32
            }
          ],
          "data_segments": [
            {
              "start": 2080,
              "length": 16771072
            }
          ],
          "parent": "c667fab02599442e9a0de7bc0309530b"
        },
        {
          "id": "bb8db73b7f044e7ab54dfe1fc6dc6642",
          "meta_segments": [
            {
              "start": 2048,
              "length": 32
            }
          ],
          "data_segments": [
            {
              "start": 2080,
              "length": 16771072
            }
          ],
          "parent": "e07d78df365c4590bb5c790c487a24a1"
        }
      ]
    },
    {
      "id": "980e1a685e744a73b36ece9a7f5f6f32",
      "stripe_sectors": 2048,
      "region_sectors": 8192,
      "length": 6291456,
      "members": [
        {
          "id": "728d7c81c8a948fdb36ddbb7910af7f0",
          "meta_segments": [
            {
              "start": 16773152,
              "length": 32
            }
          ],
          "data_segments": [
            {
              "start": 16773184,
              "length": 6291456
            }
          ],
          "parent": "c667fab02599442e9a0de7bc0309530b"
        },
        {
          "id": "4d12c3dc27fa4a7b99a49b8e5f037404",
          "meta_segments": [
            {
              "start": 16773152,
              "length": 32
            }
          ],
          "data_segments": [
            {
              "start": 16773184,
              "length": 6291456
            }
          ],
          "parent": "e07d78df365c4590bb5c790c487a24a1"
        }
      ]
    }
  ],
  "thin_pool_dev": {
    "meta_dev": {
      "id": "05ec2e691bc0482e9f76be36c0d8acf5",
      "segments": [
        {
          "start": 0,
          "length": 8192,
          "parent": "1400b1e499e64e3fac1206dbb45fc76b"
        }
      ]
    },
    "data_dev": {
      "id": "2d27514c21f843f3a24635de771bec36",
      "segments": [
        {
          "start": 0,
          "length": 1048576,
          "parent": "980e1a685e744a73b36ece9a7f5f6f32"
        }
      ]
    }
  },
  "thin_devs": [
    {
     "thin_number": 0,
      "fs": "xfs",
      "size": 2147483648
    }
  ]
}
```
