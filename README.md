# TokenCube Receiver

This Tcl module implements a generic receiver for the excellent
[Tokencube](http://www.tokencube.com/bluetooth-sensor.html) sensors. The
implementation of the module is inspired by the official
[connector.jar](http://www.tokencube.com/util/connector/connector.jar) and an
analysis of its output. This is still beta software but has been tested against
a small numbers of sensors.

The module is self-testing and running it directly will automatically start
scanning at regular intervals and dump out the values received from nearby
devices at regular intervals. For integration into your own project, you should
simply be able to require it using `package require`.

## Raspberry Pi 3

This software has been tested on the
[latest](https://www.raspberrypi.org/products/raspberry-pi-3-model-b/) Raspberry
Pi running [Raspbian](https://www.raspbian.org/). You will have to install Tcl,
BlueZ and make sure to also install `hcidump`.

To test this simply run the following command (note that you need to run this
through `sudo` to make sure you will be able to capture low-level BLE traffic
and reset the interface)

    sudo tclsh tokencube-0.1.tm