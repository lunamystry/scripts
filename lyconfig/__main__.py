import kivy
kivy.require('1.8.0')

from kivy.app import App
from kivy.uix.anchorlayout import AnchorLayout
from kivy.uix.boxlayout import BoxLayout
from kivy.uix.stacklayout import StackLayout
from kivy.uix.listview import ListItemButton
from kivy.properties import ObjectProperty
from kivy.properties import StringProperty
from kivy.properties import ListProperty
import subprocess
import os


class FileListItem(BoxLayout, ListItemButton):
    directory = StringProperty()
    filename = StringProperty()


class FileList(StackLayout):
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
        process.communicate()


if __name__ == "__main__":
    Lyconfig().run()
