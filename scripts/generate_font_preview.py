# -*- coding: utf-8 -*-

import os
import sys
import Image
import ImageDraw
import ImageFont

DEFAULT_TEXT = u"掌优RootTools"


def print_help():
    print("usage: generate_font_preview.py <FONT FILE PATH> <OUTPUT PATH>")


def do_generate_preview(filename, output):
    img = Image.new("RGB", (160, 80), (255, 255, 255))
    draw = ImageDraw.Draw(img)
    font = ImageFont.truetype(filename=filename, size=14)
    w, h = draw.textsize(DEFAULT_TEXT, font=font)
    x = (160 - w) / 2
    y = (80 - h) / 2
    draw.text((x, y), DEFAULT_TEXT, fill=(0, 0, 0), font=font)
    output = os.path.join(output, os.path.basename(filename) + ".png")
    img.save(output, "png")
    img.show()


if __name__ == "__main__":
    if len(sys.argv) != 3:
        print_help()
        exit()
    filename = sys.argv[1]
    output = sys.argv[2]
    do_generate_preview(filename, output)