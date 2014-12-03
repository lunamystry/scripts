#! /bin/bash

sudo ip link set wlan0 down
sudo iwconfig wlan0 mode ad-hoc
sudo iwconfig wlan0 channel 4
sudo iwconfig wlan0 essid 'gytha'
sudo ip link set wlan0 up
sudo ifconfig wlan0 10.0.0.1 up
sudo ip addr add 10.0.0.1 dev wlan0
