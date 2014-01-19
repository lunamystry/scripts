import kivy
kivy.require('1.7.2')

from kivy.app import App
from kivy.uix.anchorlayout import AnchorLayout
from kivy.uix.boxlayout import BoxLayout
from kivy.uix.stacklayout import StackLayout
from kivy.uix.listview import ListItemButton
from kivy.properties import ObjectProperty
from kivy.properties import StringProperty
import os


class FileListItem(BoxLayout, ListItemButton):
    filename = StringProperty()

class FileList(StackLayout):
    wallpaper_dir = StringProperty()
    list_view = ObjectProperty()

    def update(self, dir):
        if os.path.isdir(dir):
            self.list_view.adapter.data = os.listdir(dir)


class MainView(AnchorLayout):
    pass


class Lyconfig(App):
    def build(self):
        main_view = MainView()
        return main_view


if __name__ == "__main__":
    Lyconfig().run()
