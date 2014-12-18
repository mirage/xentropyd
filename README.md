xentropyd: entropy for Xen VMs
==============================

This is a daemon which watches for new domains appearing on a
Xen host and connects to them, offering them entropy from dom0.

To start the daemon:

```sudo xentropy --daemon```

The entropy data is currently read from `/dev/random` and sent
to the domain on a specially named "console" (also called a "channel").
`xentropyd` applies a simple rate-limiter to each connection to
prevent any domain stealing all of dom0's entropy.

`xentropyd` is similar in purpose to `virtio-rng`, except that
`xentropyd` works for both PV and HVM domains.
