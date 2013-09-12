package com.rarnu.devlib.component.tools;

import android.graphics.Bitmap;

public class GifFrame {

	public GifFrame(Bitmap im, int del) {
		image = im;
		delay = del;
	}

	public GifFrame(String name, int del) {
		imageName = name;
		delay = del;
	}

	public Bitmap image;

	public int delay;

	public String imageName = null;

	public GifFrame nextFrame = null;
}
