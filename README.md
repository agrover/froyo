# Froyo

Semi-automated bulk storage management

You give Froyo block devices (hard drives) and it gives you a single
redundant filesystem. You may add any size drive to expand it. When a
drive goes bad, your data is automatically reshaped in the background
across the remaining drives (if space allows). You can replace older,
smaller drives with bigger new ones while the filesystem remains online.

Drobo + Free + You = Froyo!

## Development notes

Currently requires using [Rust nightly compiler](https://doc.rust-lang.org/book/nightly-rust.html).

The last verified working nightly snapshot was 2016-05-15. If using
Multirust, run `multirust override nightly-2016-05-15` in the `froyo`
directory to use this snapshot for Froyo. If using Rustup, see its
docs for how to accomplish the same thing.

### Dev Resources

IRC: `#froyo` on irc.freenode.org

Mailing list: `froyo@lists.fedorahosted.org`
