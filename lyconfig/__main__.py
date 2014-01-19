import kivy
kivy.require('1.7.2')

from kivy.app import App
from kivy.uix.anchorlayout import AnchorLayout
from kivy.uix.boxlayout import BoxLayout
from kivy.uix.listview import ListItemButton
from kivy.properties import ObjectProperty
from kivy.properties import StringProperty
import os


class FileListItem(BoxLayout, ListItemButton):
    filename = StringProperty()

class FileList(BoxLayout):
    list_view = ObjectProperty()

    def __init__(self, **kwargs):
        super(FileList, self).__init__()
        self.list_view.adapter.data = os.listdir("/home/leny/System/Wallpapers")

    def args_converter(self, index, id):
        return {}


class MainView(AnchorLayout):
    Wallpaper_dir = StringProperty()


class Lyconfig(App):
    def build(self):
        main_view = MainView()
        return main_view


if __name__ == "__main__":
    Lyconfig().run()
