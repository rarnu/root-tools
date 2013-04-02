package com.rarnu.command.emu;

interface Screen {

	void setLineWrap(int row);

	void set(int x, int y, byte b, int foreColor, int backColor);

	void scroll(int topMargin, int bottomMargin, int foreColor, int backColor);

	void blockCopy(int sx, int sy, int w, int h, int dx, int dy);

	void blockSet(int sx, int sy, int w, int h, int val, int foreColor,
			int backColor);

	String getTranscriptText();

	void resize(int columns, int rows, int foreColor, int backColor);
}