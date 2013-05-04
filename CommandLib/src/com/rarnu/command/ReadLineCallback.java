package com.rarnu.command;

public interface ReadLineCallback {
	void onReadLine(String line);
	void onReadError(String line);
}
