#! /usr/bin/python2

"""
volume_notification: Displays the volume using dbus

author: Leonard Mandla Mbuli <mandla.mbuli@live.co.za>
creation date: 08 August 2011

"""

import dbus
import subprocess
import shlex

def dbus_notify(volume, bar):
    item = ('org.freedesktop.Notifications')
    path = ('/org/freedesktop/Notifications')
    interface = ('org.freedesktop.Notifications') 
    icon = 'speaker'
    array = ''
    hint = ''
    time = 10000
    app_name = ('Sound control')
    title = ('Sound Check : Master             ')
    body = (' <b><i>'+volume+'\n</i>'+bar+'</b>')
    bus = dbus.SessionBus()
    notif = bus.get_object(item, path)
    notify = dbus.Interface(notif, interface)
    notify.Notify(app_name, 1, icon, title, body, array, hint, time)

def bar(volume):
    """ Calculates how long the length of the volume bar is to be"""
    barstr = ''
    steps = 3
    for x in range(0, volume, steps):
        barstr = barstr+'|'
    for x in range(volume, 100, steps):
        barstr = barstr+'-'
    return barstr

def getvolume():
    mixerstr = ['amixer', 'get', 'Master']
    p = subprocess.check_output(mixerstr)
    args = shlex.split(p)
    x = args[-3].replace('[', '')
    x = x.replace(']', '')
    volume = x.replace('%', '')
    volume = int(volume)
    return volume

def main():
    """ Does the argument parsing"""
    des = "This is a script to allow me to show the volume on a system with \
            dbus but no volume notification (xmonad for example)"
    volume = getvolume()
    dbus_notify(str(volume)+'%', bar(volume))

if __name__ == '__main__':
    main()
