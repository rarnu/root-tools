package com.rarnu.command.emu.renderer;

import android.graphics.Canvas;

public abstract class TextRenderer {
	
	protected int[] mForePaint = { 0xff000000, // Black
			0xffff0000, // Red
			0xff00ff00, // green
			0xffffff00, // yellow
			0xff0000ff, // blue
			0xffff00ff, // magenta
			0xff00ffff, // cyan
			0xffffffff // white -- is overridden by constructor
	};
	protected int[] mBackPaint = { 0xff000000, // Black -- is overridden by
												// constructor
			0xffcc0000, // Red
			0xff00cc00, // green
			0xffcccc00, // yellow
			0xff0000cc, // blue
			0xffff00cc, // magenta
			0xff00cccc, // cyan
			0xffffffff // white
	};
	protected final static int mCursorPaint = 0xff808080;

	public TextRenderer(int forePaintColor, int backPaintColor) {
		mForePaint[7] = forePaintColor;
		mBackPaint[0] = backPaintColor;
	}
	
	public abstract int getCharacterWidth();

	public abstract int getCharacterHeight();

	public abstract void drawTextRun(Canvas canvas, float x, float y, int lineOffset,
			char[] text, int index, int count, boolean cursor, int foreColor,
			int backColor);
}