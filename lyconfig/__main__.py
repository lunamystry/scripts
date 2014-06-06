import kivy
kivy.require('1.8.0')

from kivy.app import App
from kivy.uix.button import Button
from kivy.uix.anchorlayout import AnchorLayout
from kivy.uix.boxlayout import BoxLayout
from kivy.uix.stacklayout import StackLayout
from kivy.uix.listview import ListItemButton
from kivy.properties import ObjectProperty
from kivy.properties import StringProperty
from kivy.properties import ListProperty
import subprocess
import fileinput
import sys
import os
import re


class EditableLabel(BoxLayout):
    field = ObjectProperty()

    def __init__(self, **kwargs):
        super(EditableLabel, self).__init__(**kwargs)
        self.show(None)

    def edit(self, obj):
        if obj is not None:
            self.remove_widget(obj)
        self.field = Button(text="done", on_press=self.show)
        self.add_widget(self.field)

    def show(self, obj):
        if obj is not None:
            self.remove_widget(obj)
        self.field = Button(text="edit", on_press=self.edit)
        self.add_widget(self.field)


class FileListItem(BoxLayout, ListItemButton):
    directory = StringProperty()
    filename = StringProperty()


class FileList(StackLayout):
    label = ObjectProperty()
    directory = StringProperty()
    file_list = ObjectProperty()
    files = ListProperty(os.listdir("/home/leny/Images/wallpapers"))

    def filename_converter(self, index, filename):
        result = {
            "filename": filename,
            "directory": self.directory
        }
        return result

    def update(self):
        if os.path.isdir(self.directory):
            self.files = os.listdir(self.directory)


class MainView(AnchorLayout):
    pass


class Lyconfig(App):
    def build(self):
        main_view = MainView()
        return main_view

    def set_wallpaper(self, filepath):
        command = "feh --bg-scale " + filepath
        process = subprocess.Popen(command,
                                   shell=True,
                                   stdout=subprocess.PIPE,
                                   stderr=subprocess.PIPE)
        homepath = os.path.expanduser("~")
        self.change_in_file(homepath+"/.xprofile", filepath)
        process.communicate()

    def change_in_file(self, filename, new_wallpaper):
        wallpaper_command = "feh --bg-scale "
        if os.path.isfile(filename):
            for line in fileinput.input(filename, inplace=True):
                if re.match(r''+wallpaper_command, line) is not None:
                    line = wallpaper_command + new_wallpaper + "\n"
                    sys.stdout.write(line)
                else:
                    sys.stdout.write(line)

if __name__ == "__main__":
    Lyconfig().run()
