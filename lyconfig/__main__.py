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
from kivy.adapters.listadapter import ListAdapter
from kivy.uix.label import Label
import os


class FileListItem(BoxLayout, ListItemButton):
    filename = StringProperty()


class FileList(StackLayout):
    directory = StringProperty()
    file_list = ObjectProperty()
    files = ListProperty(os.listdir("/home/leny/Images/wallpapers"))

    def filename_converter(self, index, filename):
        result = {
            "filename": filename,
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


if __name__ == "__main__":
    Lyconfig().run()
