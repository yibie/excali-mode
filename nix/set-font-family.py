"""Name a TTF's family FAMILY, style Regular: set-font-family.py FILE FAMILY.

Upstream's WOFF2 subsets carry names that differ from the CSS family
the web app gives them (Nunito's say "Nunito ExtraLight"), and
fontconfig matches the names in the file.
"""
import sys

from fontTools.ttLib import TTFont

path, family = sys.argv[1:]
font = TTFont(path)
names = font["name"]
for name_id in (1, 2, 4, 16, 17):
    names.removeNames(nameID=name_id)
for platform in ((3, 1, 0x409), (1, 0, 0)):
    names.setName(family, 1, *platform)
    names.setName("Regular", 2, *platform)
    names.setName(f"{family} Regular", 4, *platform)
font.save(path)
