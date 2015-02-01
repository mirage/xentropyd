xentropyd: entropy for Xen VMs
==============================

This is a daemon which watches for new domains appearing on a
Xen host and connects to them, offering them entropy from dom0.

To start the daemon:

```sudo xentropy --daemon```

The entropy data is currently read from `/dev/urandom` and sent
to the domain on a specially named "console" (also called a 
[channel](http://xenbits.xen.org/gitweb/?p=xen.git;a=blob;f=docs/misc/channel.txt;h=9fc701a64a03f1afdb52c65ac44b27caf1a600da;hb=HEAD)).
`xentropyd` applies a simple rate-limiter to each connection to
prevent any domain stealing all of dom0's entropy.

`xentropyd` is similar in purpose to 
[virtio-rng](http://log.amitshah.net/2013/01/about-random-numbers-and-virtual-machines/), except that
`xentropyd` targets Xen and works with both PV and HVM domains.

Version 1 of the [entropy transport protocol](doc/protocol.md) is
defined here.

Installing
----------

First install [opam](https://opam.ocaml.org/doc/Install.html).

Second install necessary headers e.g.
```
apt-get install libxen-dev
```

Third build the binary
```
opam install xentropyd
```

