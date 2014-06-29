import kivy
kivy.require('1.8.0')

from kivy.app import App
from kivy.uix.button import Button
from kivy.uix.label import Label
from kivy.uix.textinput import TextInput
from kivy.uix.anchorlayout import AnchorLayout
from kivy.uix.boxlayout import BoxLayout
from kivy.uix.stacklayout import StackLayout
from kivy.uix.settings import SettingsWithSidebar
from kivy.uix.listview import ListItemButton
from kivy.properties import ObjectProperty
from kivy.properties import StringProperty
from kivy.properties import ListProperty

from settingsjson import settings_json

import subprocess
import fileinput
import sys
import os
import re


class EditableLabel(BoxLayout, Label):
    field = ObjectProperty()

    def __init__(self, **kwargs):
        super(EditableLabel, self).__init__(**kwargs)
        self.show(None)

    def edit(self, label):
        if label is not None:
            self.remove_widget(self.field)
        self.field = TextInput(text=self.text,
                               on_text_validate=self.show,
                               focus=True,
                               multiline=False)
        self.field.bind(focus=self.on_focus)
        self.add_widget(self.field)

    def on_focus(self, instance, value):
        if not value:
            self.show(instance)

    def show(self, textinput):
        if textinput is not None:
            self.remove_widget(self.field)
            self.text = textinput.text
        self.field = Button(text=self.text,
                            background_color=[1, 0, 0, 0],
                            on_press=self.edit)
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
        self.settings_cls = SettingsWithSidebar
        main_view = MainView()
        return main_view

    def build_config(self, config):
        config.setdefaults('example', {
            'boolexample': True,
            'wallpaper_options': 'scale'})

    def build_settings(self, settings):
        settings.add_json_panel('Panel Name',
                                self.config,
                                data=settings_json)

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

    def rename(self, directory, filename, new_filename):
        old_path = os.path.join(directory, filename)
        new_path = os.path.join(directory, new_filename)
        if os.path.isfile(old_path):
            os.rename(old_path, new_path)

if __name__ == "__main__":
    Lyconfig().run()
