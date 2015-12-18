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
  "id": "4a8390f9b22a4c8ba6d38f0de894e8da",
  "block_devs": {
    "53b754ec804142ca8a6b8752b3a94049": {
      "path": "/dev/vdc",
      "sectors": 16777216
    },
    "8e8d1998f2ad469fbad00038a0843477": {
      "path": "/dev/vdb",
      "sectors": 16777216
    },
    "9bc53cef46a2486abbd99fb92e6ae89e": {
      "path": "/dev/vde",
      "sectors": 25165824
    },
    "df85a23bff4146dd844b45deae37d480": {
      "path": "/dev/vdd",
      "sectors": 23068672
    }
  },
  "raid_devs": {
    "2a498c1b3ad346c2a0b588b09726bb09": {
      "stripe_sectors": 2048,
      "region_sectors": 8192,
      "length": 50313216,
      "members": [
        {
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
          "parent": "53b754ec804142ca8a6b8752b3a94049"
        },
        {
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
          "parent": "8e8d1998f2ad469fbad00038a0843477"
        },
        {
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
          "parent": "9bc53cef46a2486abbd99fb92e6ae89e"
        },
        {
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
          "parent": "df85a23bff4146dd844b45deae37d480"
        }
      ]
    },
    "5ea3c67e31a74ca9b22250e8732fc6eb": {
      "stripe_sectors": 2048,
      "region_sectors": 8192,
      "length": 6291456,
      "members": [
        {
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
          "parent": "9bc53cef46a2486abbd99fb92e6ae89e"
        },
        {
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
          "parent": "df85a23bff4146dd844b45deae37d480"
        }
      ]
    }
  },
  "thin_pool_dev": {
    "data_block_size": 2048,
    "low_water_blocks": 512,
    "meta_dev": {
      "id": "cfab24ed3ca4469ca6b506d25910b077",
      "segments": [
        {
          "start": 0,
          "length": 8192,
          "parent": "2a498c1b3ad346c2a0b588b09726bb09"
        }
      ]
    },
    "data_dev": {
      "id": "f948ad65a5164d36a03c35f7a59f1efb",
      "segments": [
        {
          "start": 8192,
          "length": 1048576,
          "parent": "2a498c1b3ad346c2a0b588b09726bb09"
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
