The entropy transport protocol
==============================

The daemon will create "channel" connections to every domain on a host,
(except domain 0) where a "channel" is a
[PV console with a well-known name](http://xenbits.xen.org/gitweb/?p=xen.git;a=blob;f=docs/misc/channel.txt;h=9fc701a64a03f1afdb52c65ac44b27caf1a600da;hb=HEAD).

We use the well-known name:

```
  org.openmirage.entropy.1
```

To avoid the guest connecting to the wrong console and misinterpreting other
data as being entropy, we use a simple handshake. First the guest writes
(in one single block):

```
  "Hello, may I have some entropy?\r\n"
```

Second the server writes:

```
  "You may treat everything following this message as entropy.\r\n"
```

After this the server writes entropy at the configured rate. Any data written
by the guest on the ring is discarded.
