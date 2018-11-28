import json

settings_json = json.dumps([
    {'type': 'title',
        'title': 'example title'},
    {'type': 'bool',
     'title': 'A boolean setting',
     'desc': 'Boolean description text',
     'section': 'example',
     'key': 'boolexample'},
    {'type': 'options',
     'title': 'wallpaper layout',
     'desc': 'should the wallpaper be stretches, tile scaled etc...',
     'key': 'wallpaper_options',
     'section': 'example',
     'options': ['center', 'fill', 'max', 'scale', 'tile']}
    ])
